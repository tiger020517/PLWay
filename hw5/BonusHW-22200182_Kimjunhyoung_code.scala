import $ivy.`org.scala-lang.modules::scala-parser-combinators:2.3.0`
import scala.util.parsing.combinator.RegexParsers


class SimpleException(message: String) extends RuntimeException(message) {
  override def fillInStackTrace(): Throwable = this
  override def printStackTrace(): Unit = println(message)
}

case class Id(name: String)


trait Expr
case class Num(n: Int) extends Expr
case class Add(lhs: Expr, rhs: Expr) extends Expr
case class Sub(lhs: Expr, rhs: Expr) extends Expr
case class Fun(param: Id, body: Expr) extends Expr
case class Identifier(name: Id) extends Expr
case class App(ftn: Expr, arg: Expr) extends Expr
case class If0(testExpr: Expr, thenExpr: Expr, elseExpr: Expr) extends Expr
case class Rec(fname: Id, namedExpr: Expr, fstCall: Expr) extends Expr


trait ExprValue
case class NumV(n: Int) extends ExprValue
case class ClosureV(param: Id, body: Expr, ds: DefrdSub) extends ExprValue
case class Thunk(expr: Expr, var ds: DefrdSub) {
  var cache: Option[ExprValue] = None
}


trait DefrdSub
case object MtSub extends DefrdSub
case class ASub(name: Id, value: Thunk, saved: DefrdSub) extends DefrdSub
case class ARecSub(name: Id, value: Thunk, saved: DefrdSub) extends DefrdSub


//before one
def parse(input: String): Expr = {
  object Parser extends RegexParsers {
    def int: Parser[Int] = """-?\d+""".r ^^ { _.toInt } 
    def symbol: Parser[Id] = """[a-zA-Z_][a-zA-Z0-9_]*""".r ^^ { s => Id(s) }

    def wrap[T](parser: Parser[T]): Parser[T] = "{" ~> parser <~ "}"

    lazy val expr: Parser[Expr] =
      int ^^ { n => Num(n) } |
      symbol ^^ { id => Identifier(id) } |
      wrap("+" ~> expr ~ expr) ^^ { case l ~ r => Add(l, r) } |
      wrap("-" ~> expr ~ expr) ^^ { case l ~ r => Sub(l, r) } |
      wrap("with" ~> wrap(symbol ~ expr) ~ expr) ^^ {
        case (Id(name) ~ value) ~ body => App(Fun(Id(name), body), value)
      } |
      wrap("fun" ~> wrap(symbol) ~ expr) ^^ {
        case Id(param) ~ body => Fun(Id(param), body)
      } |
      wrap("if0" ~> expr ~ expr ~ expr) ^^ {
        case test ~ thenE ~ elseE => If0(test, thenE, elseE)
      } |
      wrap("rec" ~> wrap(symbol ~ expr) ~ expr) ^^ {
        case (Id(name) ~ named) ~ body => Rec(Id(name), named, body)
      } |
      wrap(expr ~ expr) ^^ { case ftn ~ arg => App(ftn, arg) }

    def parseAllExpr(str: String): Expr =
      parseAll(expr, str).getOrElse(throw new SimpleException(s"bad syntax: $str"))
  }
  Parser.parseAllExpr(input)
}

//before one
def lookup(name: Id, ds: DefrdSub): Thunk = ds match {
  case MtSub => throw new SimpleException(s"Free Identifier: ${name.name}")
  case ASub(i, v, saved) =>
    if (i == name) v else lookup(name, saved)
  case ARecSub(i, v, saved) =>
    if (i == name) v else lookup(name, saved)
}


/*
[solve myself]: N, Gemini + class video
[Time Taken]: 14m
[contract]: strict: Thunk -> ExprValue
[purpose]: Evalutate the Thunk
[tests]:
	strict(Thunk(Num(10), MtSub)) -> NumV(10)
    strict(Thunk(Add(Num(3), Num(5)), MtSub)) -> NumV(8)
*/
def strict(t: Thunk): ExprValue = {
  if (t.cache.isDefined) {
    t.cache.get
  } else {
    val v = interp(t.expr, t.ds)
    t.cache = Some(v)
    v
  }
}

//before one
def numOperator(op: (Int, Int) => Int): (ExprValue, ExprValue) => ExprValue = {
  case (NumV(x), NumV(y)) => NumV(op(x, y))
  case _ => throw new SimpleException("Expected numerical expr")
}

//before one
val numAdd: (ExprValue, ExprValue) => ExprValue = numOperator(_ + _)
val numSub: (ExprValue, ExprValue) => ExprValue = numOperator(_ - _)



//before one
def numZero(v: ExprValue): Boolean = v match {
  case NumV(0) => true
  case NumV(_) => false
  case _ => throw new SimpleException("Expected numerical expr for if0 test")
}

/*
[solve myself]: n, Gemini + lecture video
[Time Taken]: 12m
[contract]: interp: Expr, DefrdSub -> ExprValue
[purpose]: 주어진 ds에서 LRFAE 식을 해석하여 결과값을 반환 (재귀, 조건문, lazy 지원)
[tests]:
	interp(parse("{{fun {x} 10} {+ 1 {fun {y} y}}}"), MtSub) -> NumV(10)    interp(Rec(Id("f"), Fun(Id("x"), Identifier(Id("x"))), App(Identifier(Id("f")), Num(10))), MtSub) -> NumV(10)
	interp(parse("{rec {f {fun {n} {if0 n 0 {- {f {- n 1}} 1}}}} {f 10}}"), MtSub) -> NumV(-10)
*/
def interp(expr: Expr, ds: DefrdSub): ExprValue = expr match {
  case Num(n) => NumV(n)

  case Add(l, r) => numAdd(interp(l, ds), interp(r, ds))
  case Sub(l, r) => numSub(interp(l, ds), interp(r, ds))

  case Identifier(id) => strict(lookup(id, ds)) 

  case Fun(p, b) => ClosureV(p, b, ds)

  case App(ftn, arg) =>
    val fVal = interp(ftn, ds)
    val aVal = Thunk(arg, ds)
    fVal match {
      case ClosureV(param, body, closure_ds) => interp(body, ASub(param, aVal, closure_ds))
      case _ => throw new SimpleException("Expected a function")
    }

  case If0(test, thenE, elseE) =>
    if (numZero(interp(test, ds))) interp(thenE, ds)
    else interp(elseE, ds)

  case Rec(fname, namedExpr, fstCall) =>
    val recThunk = Thunk(namedExpr, ds)
    val newDs = ARecSub(fname, recThunk, ds)
    recThunk.ds = newDs
    interp(fstCall, newDs)
}
 


println("\n=== Running LRFAE Tests ===\n")

def run(name: String, code: String, expected: ExprValue): Unit = {
  try {
    val result = interp(parse(code), MtSub)
    if (result == expected) 
      println(s"✅ [$name] Passed: $result")
    else 
      println(s"❌ [$name] Failed: Expected $expected but got $result")
  } catch {
    case e: Exception => println(s"❌ [$name] Error: ${e.getMessage}")
  }
}

// Test Case 1: 기본 연산
run("Basic Add", "{+ 10 20}", NumV(30))

// Test Case 2: Lazy Evaluation (Type Error 회피)
// 설명: {+ 1 {fun {y} y}}는 에러지만, lazy하므로 실행되지 않아 10이 나와야 함
run("Lazy Check", "{{fun {f} {f 1}} {fun {x} {+ x 1}}}", NumV(2))

// Test Case 3: Recursion (Factorial/Sum)
// 설명: 10에서 0까지 10번 재귀 호출하여 10번 뺄셈 수행 -> -10
run("Recursion Check", "{rec {f {fun {n} {if0 n 0 {- {f {- n 1}} 1}}}} {f 10}}", NumV(-10))

println("\n=== Done ===")
