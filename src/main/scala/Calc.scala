import atto._
import Atto._
import compat.scalaz._

import scala.io.Source
import scalaz.{-\/, \/-}

object Calc {
  type CalcValue = Int
  type CalcVariable = String
  type CalcProgram = List[Statement]

  sealed trait Expression
  case class Constant(v: CalcValue) extends Expression
  case class VariableLookup(v: CalcVariable) extends Expression
  case class Add(e1: Expression, e2: Expression) extends Expression
  case class Subtract(e1: Expression, e2: Expression) extends Expression
  case class Multiply(e1: Expression, e2: Expression) extends Expression
  case class Divide(e1: Expression, e2: Expression) extends Expression
  case class Negate(e: Expression) extends Expression

  sealed trait Statement
  case class Print(e: Expression) extends Statement
  case class Assignment(v: CalcVariable, e: Expression) extends Statement
  case class NakedExpression(e: Expression) extends Statement

  def evaluate(expression: Expression, environment: Map[CalcVariable, CalcValue]): CalcValue = {
    expression match {
      case Constant(v) => v
      case VariableLookup(v) => environment.get(v).get
      case Add(e1, e2) => evaluate(e1, environment) + evaluate(e2, environment)
      case Subtract(e1, e2) => evaluate(e1, environment) - evaluate(e2, environment)
      case Multiply(e1, e2) => evaluate(e1, environment) * evaluate(e2, environment)
      case Divide(e1, e2) => evaluate(e1, environment) / evaluate(e2, environment)
      case Negate(e) => - evaluate(e, environment)
    }
  }

  def run(program: CalcProgram, environment: Map[CalcVariable, CalcValue] = Map.empty): Unit = {
    program match {
      case Nil => ()
      case statement :: restOfStatements => statement match {
        case Print(e) => {
          println(evaluate(e, environment))
          run(restOfStatements, environment)
        }
        case Assignment(v, e) => {
          val newEnvironment = environment + (v -> evaluate(e, environment))
          run(restOfStatements, newEnvironment)
        }
        case NakedExpression(e) => {
          evaluate(e, environment)
          run(restOfStatements, environment)
        }
      }
    }
  }

  def expressionHelperFunction(sign: Parser[Char]) = {
    for {
      _ <- char('(')
      e1 <- expressionP
      _ <- many(spaceChar)
      _ <- sign
      _ <- many(spaceChar)
      e2 <- expressionP
      _ <- char(')')
    } yield (e1, e2)
  }

  val constantP: Parser[Expression] = int.map(Constant)
  val variableLookupP: Parser[Expression] = many1(letter).map(letters => VariableLookup(letters.list.toList.mkString))
  val addP: Parser[Expression] = expressionHelperFunction(char('+')).map(res => Add(res._1, res._2))
  val subtractP: Parser[Expression] = expressionHelperFunction(char('-')).map(res => Subtract(res._1, res._2))
  val multiplyP: Parser[Expression] = expressionHelperFunction(char('*')).map(res => Multiply(res._1, res._2))
  val divideP: Parser[Expression] = expressionHelperFunction(char('/')).map(res => Divide(res._1, res._2))
  val expressionP: Parser[Expression] = constantP | variableLookupP| addP | subtractP | multiplyP | divideP

  val printP: Parser[Statement] = {
    for {
      _ <- string("print")
      _ <- many(spaceChar)
      _ <- char('(')
      _ <- many(spaceChar)
      e <- expressionP
      _ <- many(spaceChar)
      _ <- char(')')
    } yield Print(e)
  }

  val assignmentP: Parser[Statement] = {
    for {
      v <- many1(letter).map(letters => letters.list.toList.mkString)
      _ <- many(spaceChar)
      _ <- char('=')
      _ <- many(spaceChar)
      e <- expressionP
    } yield Assignment(v, e)
  }

  val nakedExpressionP: Parser[Statement] = {
    expressionP.map(NakedExpression)
  }
  val statementP: Parser[Statement] = printP | assignmentP | nakedExpressionP
  val programP: Parser[CalcProgram] = sepBy(statementP, many(char('\n')))

  def main(args: Array[String]): Unit = {
    val stringToParse: String = Source.fromFile("/Users/bass/Code/scala/calc-scala/src/main/resources/example.calc").mkString
    programP.parseOnly(stringToParse).done.either match {
      case -\/(error) => println(error)
      case \/-(program) => run(program)
    }
  }
}
