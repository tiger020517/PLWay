import $ivy.`org.scala-lang.modules::scala-parser-combinators:2.3.0`

import scala.util.parsing.combinator._

trait Expr
case class Num(num: Int) extends Expr
case class Add(left: Expr, right: Expr) extends Expr
case class Sub(left: Expr, right: Expr) extends Expr

class SimpleException(message: String) extends RuntimeException(message) {
	override def fillInStackTrace(): Throwable = this
	override def printStackTrace(): Unit = println(message)
}

def parse(input: String): Expr = {
	object Parser extends RegexParsers {
		def int: Parser[Int] = """\d+""".r^^{_.toInt}
		def wrap[T](Parser: Parser[T]):Parser[T] = "{"~>Parser<~"}"

		lazy val expr:Parser[Expr]=
		int^^{case n => Num(n)}|
		wrap("+"~>expr~expr)^^{case l~r => Add(l,r)}|
		wrap("-"~>expr~expr)^^{case l~r => Sub(l,r)}
		
		def parseAllExpr(input: String): Expr=
			parseAll(expr, input).getOrElse(throw new SimpleException(s"bad syntax: $input"))
	}
	Parser.parseAllExpr(input)
}

def interp(expr: Expr): Int = expr match{
	case Num(n) => n
	case Add(l, r) => interp(l) + interp(r)
	case Sub(l, r) => interp(l) - interp(r)
}

@main def run(): Unit = {
	val cdeInConcreteSyntax= "{+3 {-8 2}}"
	val abstracSytaxOfCode = parse(cdeInConcreteSyntax)
	println(interp(abstracSytaxOfCode))
}