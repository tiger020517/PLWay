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


trait DefrdSub
case object MtSub extends DefrdSub
case class ASub(name: Id, value: ExprValue, saved: DefrdSub) extends DefrdSub
case class ARecSub(name: Id, var valueBox: ExprValue, saved: DefrdSub) extends DefrdSub


/*
[solve myself]: N
[Time Taken]: 28
[contract]: parse: String -> Expr
[purpose]: RCFAE 언어의 문자열을 받아 AST(Abstract Syntax Tree)를 반환함
[tests]:
    parse("10") -> Num(10)
    parse("{rec {x 1} x}") -> Rec(Id("x"), Num(1), Identifier(Id("x")))
    parse("{if0 0 1 2}") -> If0(Num(0), Num(1), Num(2))
*/
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

/*
[solve myself]: N
[Time Taken]: 37m
[contract]: lookup: Id, DefrdSub -> ExprValue
[purpose]: 환경(ds)에서 식별자(name)에 바인딩된 값을 찾음 (ARecSub의 경우 가변 필드 참조)
[tests]:
    lookup(Id("x"), ASub(Id("x"), NumV(10), MtSub)) -> NumV(10)
    lookup(Id("f"), ARecSub(Id("f"), NumV(42), MtSub)) -> NumV(42)
*/
def lookup(name: Id, ds: DefrdSub): ExprValue = ds match {
  case MtSub => throw new SimpleException(s"Free Identifier: ${name.name}")
  case ASub(i, v, saved) =>
    if (i == name) v
    else lookup(name, saved)
  case ARecSub(i, vBox, saved) =>
    if (i == name) vBox
    else lookup(name, saved)
}

//before one
def numOperator(op: (Int, Int) => Int): (ExprValue, ExprValue) => ExprValue = {
  case (NumV(x), NumV(y)) => NumV(op(x, y))
  case _ => throw new SimpleException("Expected numerical expr")
}

//before one
val numAdd: (ExprValue, ExprValue) => ExprValue = numOperator(_ + _)
val numSub: (ExprValue, ExprValue) => ExprValue = numOperator(_ - _)


/*
[solve myself]: Y
[Time Taken]: 7m
[contract]: numZero: ExprValue -> Boolean
[purpose]: 0인지 확인합니다
[tests]:
	numZero(NumV(0)) -> true
    numZero(NumV(5)) -> false
*/
def numZero(v: ExprValue): Boolean = v match {
  case NumV(0) => true
  case NumV(_) => false
  case _ => throw new SimpleException("Expected numerical expr for if0 test")
}

/*
[solve myself]: Y
[Time Taken]: 67m
[contract]: interp: Expr, DefrdSub -> ExprValue
[purpose]: 주어진 ds에서 RCFAE 식을 해석하여 결과값을 반환 (재귀, 조건문 지원)
[tests]:
    interp(If0(Num(0), Num(1), Num(2)), MtSub) -> NumV(1)
    interp(Rec(Id("f"), Fun(Id("x"), Identifier(Id("x"))), App(Identifier(Id("f")), Num(10))), MtSub) -> NumV(10)
*/
def interp(expr: Expr, ds: DefrdSub): ExprValue = expr match {
  case Num(n) => NumV(n)

  case Add(l, r) => numAdd(interp(l, ds), interp(r, ds))
  case Sub(l, r) => numSub(interp(l, ds), interp(r, ds))

  case Identifier(id) => lookup(id, ds) 

  case Fun(p, b) => ClosureV(p, b, ds)

  case App(ftn, arg) =>
    val f_val = interp(ftn, ds)
    val a_val = interp(arg, ds)
    f_val match {
      case ClosureV(param, body, closure_ds) =>
        interp(body, ASub(param, a_val, closure_ds))
      case _ => throw new SimpleException("Expected a function")
    }

  case If0(test, thenE, elseE) =>
    if (numZero(interp(test, ds))) interp(thenE, ds)
    else interp(elseE, ds)

  case Rec(fname, namedExpr, fstCall) =>
    val valueHolder = NumV(42)
    val newDs = ARecSub(fname, valueHolder, ds)
    val resolvedValue = interp(namedExpr, newDs)
    newDs.valueBox = resolvedValue
    interp(fstCall, newDs)
}
