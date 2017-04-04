import atto._
import Atto._
import compat.scalaz._

import scala.io.Source
import scalaz.{-\/, \/-}

object Calc {
  type Number = Int
  type Statement = String
  type Statements = List[Statement]


  sealed trait StatementType
  case class nakedExpression(e: Expression) extends StatementType
  case class VariableAssignment(name: String, e: Expression) extends StatementType
  case class Print(e: Expression) extends StatementType

  sealed trait Expression
  case class Constant(n: Number) extends Expression
  case class Add(n1: Expression, n2: Expression) extends Expression
  case class Subtract(n1: Expression, n2: Expression) extends Expression
  case class Multiply(n1: Expression, n2: Expression) extends Expression
  case class Divide(n1: Expression, n2: Expression) extends Expression
  case class VariableLookup(name: String) extends Expression


  def evaluate(e: Expression, environment: Map[String, Expression]): Number = {
    e match {
      case Constant(n) => n
      case Add(n1,n2) => evaluate(n1, environment) + evaluate(n2, environment)
      case Subtract(n1, n2) => evaluate(n1, environment) - evaluate(n2, environment)
      case Multiply(n1, n2) => evaluate(n1, environment) * evaluate(n2, environment)
      case Divide(n1, n2) => evaluate(n1, environment) / evaluate(n2, environment)
      case VariableLookup(name) => evaluate(environment.get(name).get, environment)
    }
  }

  def arithmeticExpressionHelper(sign: Char) = {
    for {
      _ <- char('(')
      n1 <- expressionCombinatorP
      _ <- spaceChar
      _ <- char(sign)
      _ <- spaceChar
      n2 <- expressionCombinatorP
      _ <- char(')')
    } yield (n1, n2)
  }

  def variableAssignmentP: Parser[StatementType] = {
    for {
      n <- many1(letter).map(l => (l.list.toList.mkString))
      _ <- spaceChar
      _ <- char('=')
      _ <- spaceChar
      v <- expressionCombinatorP
    } yield VariableAssignment(n, v)
  }

  val constantP: Parser[Expression] = int.map(Constant)
  val addP: Parser[Expression] = arithmeticExpressionHelper('+').map(v => Add(v._1, v._2))
  val subtractP: Parser[Expression] = arithmeticExpressionHelper('-').map(v => Subtract(v._1, v._2))
  val multiplyP: Parser[Expression] = arithmeticExpressionHelper('*').map(v => Multiply(v._1, v._2))
  val divideP: Parser[Expression] = arithmeticExpressionHelper('/').map(v => Divide(v._1, v._2))
  val variableLookupP: Parser[Expression] = many1(letter).map(l => VariableLookup(l.list.toList.mkString))
  val expressionCombinatorP: Parser[Expression] = constantP | variableLookupP | addP | subtractP | multiplyP | divideP

  def main(args: Array[String]): Unit = {
    val test = expressionCombinatorP.parseOnly("(1 + 2)").done
    println(test)
  }
}
