import atto._, Atto._, compat.scalaz._

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
      _ <- parenLeftP
      e1 <- expressionP
      _ <- many(spaceChar)
      _ <- sign
      _ <- many(spaceChar)
      e2 <- expressionP
      _ <- parenRightP
    } yield (e1, e2)
  }

  val newlineP: Parser[Char] = char('\n')
  val parenLeftP: Parser[Char] = char('(')
  val parenRightP: Parser[Char] = char(')')
  val intP: Parser[Int] = int
  val constantP: Parser[Expression] = intP.map(x => Constant(x))
  val addSignP: Parser[Char] = char('+')
  val subtractSignP: Parser[Char] = char('-')
  val multiplySignP: Parser[Char] = char('*')
  val divideSignP: Parser[Char] = char('/')
//  val negateSignP: Parser[Char] = ???

  val addP: Parser[Expression] = expressionHelperFunction(addSignP).map(res => Add(res._1, res._2))
  val subtractP: Parser[Expression] = expressionHelperFunction(subtractSignP).map(res => Subtract(res._1, res._2))
  val multiplyP: Parser[Expression] = expressionHelperFunction(multiplySignP).map(res => Multiply(res._1, res._2))
  val divideP: Parser[Expression] = expressionHelperFunction(divideSignP).map(res => Divide(res._1, res._2))
//  val negateP: Parser[Expression] = expressionHelperFunction(negateSignP).map(res => Negate(res._1))

  val expressionP: Parser[Expression] = constantP | addP | subtractP | multiplyP | divideP

  def main(args: Array[String]): Unit = {
    val testProgram = expressionP.parseOnly("(1 * 23)").done.option
    println(testProgram)
  }
}
