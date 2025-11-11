import scala.util.parsing.combinator.RegexParsers


class SimpleException(message: String) extends RuntimeException(message) 
{
	override def fillInStackTrace(): Throwable = this
	override def printStackTrace(): Unit = println(message)
}

case class Id(name: String)


trait Expr
case class Num(n: Int) extends Expr
case class Add(lhs: Expr, rhs: Expr) extends Expr
case class Sub(lhs: Expr, rhs: Expr) extends Expr
case class Fun(Param: Id, body: Expr) extends Expr
case class Identifier(name: Id) extends Expr
case class App(ftn: Expr, arg: Expr) extends Expr


trait ExprValue
case class NumV(n: Int)extends ExprValue
case class ClosureV(param: Id, body: Expr, ds: DefrdSub) extends ExprValue


trait DefrdSub
case object MtSub extends DefrdSub 
case class ASub(name: Id, value: ExprValue, saved: DefrdSub) extends DefrdSub

/*
[solve myself]: N (Slide 04~08, 12~14 + gemini)
[Time Taken]: 27m
[contract]: parse: String -> Expr
[purpose]: FAE 언어의 문자열을 받아 AST 반환
[tests]:
	parse("10") -> Num(10)
	parse("{+ 1 2}") -> Add(Num(1), Num(2))
*/

def parse(input: String): Expr = {
	object Parser extends RegexParsers {
		def int: Parser[Int] = """\d+""".r ^^ { _.toInt }

		def symbol: Parser[Id] = """[a-zA-Z_]\w*""".r ^^ { s => Id(s)}

		def wrap[T](parser: Parser[T]): Parser[T] = "{" ~> parser <~ "}"

		lazy val expr: Parser[Expr] =
			int ^^ { case n => Num(n) } |
			symbol ^^ { id => Identifier(id) } |
			wrap("+" ~> expr ~ expr) ^^ { case l ~ r => Add(l, r) } |
			wrap("-" ~> expr ~ expr) ^^ { case l ~ r => Sub(l, r) } |
			wrap("with" ~> wrap(symbol ~ expr) ~ expr) ^^ {
                case (Id(name) ~ value) ~ body => App(Fun(Id(name), body), value) } |
            wrap("fun" ~> wrap(symbol) ~ expr) ^^ {
                case Id(param) ~ body => Fun(Id(param), body)} |
            wrap(expr ~ expr) ^^ { case ftn ~ arg => App(ftn, arg) }

		def parseAllExpr(str: String): Expr =
			parseAll(expr, str).getOrElse(throw new SimpleException(s"bad syntax: $str"))
	}	
	Parser.parseAllExpr(input)
}

/*
[solve myself]: N (Slide 11 + gemini)
[Time Taken]: 18m
[contract]: lookup: Id, DefrdSub -> ExprValue
[purpose]: ds에서 name에 바인딩된 값을 찾습니다.
[tests]:
	val testEnv = ASub(Id("x"), NumV(10), ASub(Id("y"), NumV(20), MtSub))
	lookup(Id("x"), testEnv) -> NumV(10)
*/

def lookup(name: Id, ds: DefrdSub): ExprValue = ds match{
case MtSub => throw new SimpleException(s"Free Identifier: $name")
case ASub(i, v, saved) =>
	if (i == name) v
	else lookup(name, saved)
}



/*
[solve myself]: N (Slide 12~14 + gemini)
[Time Taken]: 24m
[contract]: numOperator: ((Int, Int) => Int) -> ((ExprValue) => ExprValue)
[purpose]: op함수를 FAE 값 함수로 반환함
[tests]:
	val addOp = numOperato(_ + _)
	addOp(NumV(3). NumV(4)) -> NumV(7)
*/

def numOperator(op: (Int, Int) => Int): (ExprValue, ExprValue) => ExprValue = {
	case (NumV(x), NumV(y)) => NumV(op(x, y))
	case _ => throw new SimpleException("Expected numerical expr")
}


/*
[solve myself]: N (Slide 04, 05 + gemini)
[Time Taken]: 12m
[contract]: numAdd: ExprValue, ExprValue -> ExprValue
[purpose]: numOperator를 사용해 정의한 두 NumV의 합
[tests]:
	numAdd(NumV(5), NumV(10)) -> NumV(15)
	numAdd(NumV(3), NumV(8)) -> NumV(11)
*/
val numAdd: (ExprValue, ExprValue) => ExprValue = numOperator(_ + _)

/*
[solve myself]: Y
[Time Taken]: 3m
[contract]: numSub: ExprValue, ExprValue -> ExprValue
[purpose]: numOperator를 사용해 정의한 두 NumV의 차
[tests]:
	numAdd(NumV(50), NumV(10)) -> NumV(40)
	numAdd(NumV(8), NumV(3)) -> NumV(5)
*/
val numSub: (ExprValue, ExprValue) => ExprValue = numOperator(_ - _)

/* 
[solve myself]: N (Slide 12~14 + gemini)
[Time Taken]: 48m
[contract]: interp: Expr, DefrsSub -> ExprValue
[purpose]: 주어진 ds에서 FAE를 해석하여 FAE값 반환함
[tests]:
	interp(Num(10), MtSub) -> NumV(10)
	interp(Add(Num(3), Num(4)), MtSub) -> NumV(7)
*/

def interp(expr: Expr, ds: DefrdSub): ExprValue = expr match {
    case Num(n) => NumV(n) 
    
    case Add(l, r) => numAdd(interp(l, ds), interp(r, ds)) 
    case Sub(l, r) => numSub(interp(l, ds), interp(r, ds)) 
    
    case Identifier(s) => lookup(Id(s), ds) 
    
    case Fun(p, b) => ClosureV(p, b, ds)
    
    case App(ftn, arg) =>
        val f_val = interp(ftn, ds)
        val a_val = interp(arg, ds)
        
        f_val match {
            case ClosureV(param, body, closure_ds) =>
                interp(body, ASub(param, a_val, closure_ds))
            
            case _ => throw new SimpleException("Expected a function")
        }
}
