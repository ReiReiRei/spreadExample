import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

object MathExpParser extends Parsers {
  override type Elem = Expr

  class MathExpTokenReader(tokens: Seq[Expr]) extends Reader[Expr] {
    override def first: Expr = tokens.head
    override def atEnd: Boolean = tokens.isEmpty
    override def pos: Position = NoPosition
    override def rest: Reader[Expr] = new MathExpTokenReader(tokens.tail)
  }

  def constant: Parser[Expr] =
    accept("constant", {
      case x: Literal => Literal(x.v)
    })

  def variable: Parser[Expr] =
    accept("value", {
      case Ref(x) => Ref(x)
    })

  def shortFactor: Parser[Expr] =
    constant | variable | LeftParenthesis ~> expression <~ RightParenthesis ^^ {
      case x => x
    }

  def term: Parser[Expr] =
    shortFactor ~ rep((Multiply | Divide) ~ shortFactor) ^^ {
      case x ~ ls =>
        ls.foldLeft[Expr](x) {
          case (d1, Multiply ~ d2) => Multiply(d1, d2)
          case (d1, Divide ~ d2)   => Divide(d1, d2)
        }
    }

  def expression: Parser[Expr] = term ~ rep((Add | Sub) ~ term) ^^ {
    case x ~ ls =>
      ls.foldLeft[Expr](x) {
        case (d1, Add ~ d2) => Add(d1, d2)
        case (d1, Sub ~ d2) => Sub(d1, d2)
      }
  }

  def program: Parser[Expr] = phrase(expression)

  def apply(tokens: Seq[Expr]): Either[_, Expr] = {
    val reader = new MathExpTokenReader(tokens)
    program(reader) match {
      case Success(result, _) => Right(result)
      case NoSuccess(msg, _)  => Left(msg)
    }
  }
}

import scala.util.parsing.combinator.JavaTokenParsers

object MathExpScanner extends JavaTokenParsers {
  def add: Parser[Expr] = "+" ^^ (_ => Add)
  def minus: Parser[Expr] = "-" ^^ (_ => Sub)
  def multiply: Parser[Expr] = "*" ^^ (_ => Multiply)
  def divide: Parser[Expr] = "/" ^^ (_ => Divide)
  def leftParenthesis: Parser[Delimiter] = "(" ^^ (_ => LeftParenthesis)
  def rightParenthesis: Parser[Delimiter] = ")" ^^ (_ => RightParenthesis)

  def number: Parser[Literal] =
    floatingPointNumber ^^ (x => Literal(x.toDouble))

  def variable: Parser[Ref] = "" ~ ident ^^ {
    case _ ~ n => Ref(n)
  }

  def tokens: Parser[List[Expr]] = {
    phrase(
      rep1(
        add | minus | multiply | divide | leftParenthesis | rightParenthesis |
          number |
          variable))
  }

  def apply(expression: String): Either[_, List[Expr]] =
    parse(tokens, expression) match {
      case NoSuccess(msg, _)  => Left((msg))
      case Success(result, _) => Right(result)
    }
}
object MathExpCompiler {
  def apply(code: String): Either[_, Expr] =
    for {
      tokens <- MathExpScanner(code).right
      ast <- MathExpParser(tokens).right
    } yield ast
}
