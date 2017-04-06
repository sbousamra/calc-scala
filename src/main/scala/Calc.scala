import atto._
import Atto._
import compat.scalaz._
import scala.io.Source
import scalaz.{-\/, \/-}

object Calc {
  type Number = Int
  type Environment = Map[String, Expression]
  type Statements = List[StatementType]


  sealed trait StatementType
  case class NakedExpression(e: Expression) extends StatementType
  case class VariableAssignment(name: String, e: Expression) extends StatementType
  case class Print(e: Expression) extends StatementType

  sealed trait Expression
  case class Constant(n: Number) extends Expression
  case class Add(n1: Expression, n2: Expression) extends Expression
  case class Subtract(n1: Expression, n2: Expression) extends Expression
  case class Multiply(n1: Expression, n2: Expression) extends Expression
  case class Divide(n1: Expression, n2: Expression) extends Expression
  case class VariableLookup(name: String) extends Expression


  def evaluate(e: Expression, environment: Environment): Number = {
    e match {
      case Constant(n) => n
      case Add(n1,n2) => evaluate(n1, environment) + evaluate(n2, environment)
      case Subtract(n1, n2) => evaluate(n1, environment) - evaluate(n2, environment)
      case Multiply(n1, n2) => evaluate(n1, environment) * evaluate(n2, environment)
      case Divide(n1, n2) => evaluate(n1, environment) / evaluate(n2, environment)
      case VariableLookup(name) => evaluate(environment.get(name).get, environment)
    }
  }

  def runStatements(statements: Statements, environment: Environment = Map.empty): Unit = {
    statements match {
      case Nil => ()
      case statement => statement.head match {
        case NakedExpression(e) => {
          evaluate(e, environment)
          runStatements(statements.drop(1), environment)
        }
        case VariableAssignment(name, e) => {
          val newEnvironment = environment + (name -> e)
          runStatements(statements.drop(1), newEnvironment)
        }
        case Print(e) => {
          println(evaluate(e, environment))
          runStatements(statements.drop(1), environment)
        }
      }
    }
  }

  def arithmeticExpressionHelper(sign: Char): Parser[(Expression, Expression)] = {
    for {
      _ <- char('(')
      n1 <- expressionCombinatorP
      _ <- many(spaceChar)
      _ <- char(sign)
      _ <- many(spaceChar)
      n2 <- expressionCombinatorP
      _ <- char(')')
    } yield (n1, n2)
  }

  def printP: Parser[StatementType] = {
    for {
      _ <- string("print")
      _ <- many(spaceChar)
      _ <- char('(')
      _ <- many(spaceChar)
      e <- expressionCombinatorP
      _ <- many(spaceChar)
      _ <- char(')')
    } yield Print(e)
  }

  def variableAssignmentP: Parser[StatementType] = {
    for {
      n <- many1(letter).map(l => (l.list.toList.mkString))
      _ <- many(spaceChar)
      _ <- char('=')
      _ <- many(spaceChar)
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
  val nakedExpressionP: Parser[StatementType] = expressionCombinatorP.map(x => NakedExpression(x))
  val statementTypeP: Parser[StatementType] = printP | variableAssignmentP | nakedExpressionP
  val statementsP: Parser[Statements] = sepBy(statementTypeP, many(char('\n')))

  def main(args: Array[String]): Unit = {
    val statements: String = Source.fromFile("/Users/bass/Code/scala/calc-scala/src/main/resources/example.calc").mkString
    val statementsParsed: ParseResult[Statements] = statementsP.parseOnly(statements).done
    statementsParsed.either match {
      case -\/(error) => println("error")
      case \/-(result) => runStatements(result)
    }
  }
}
