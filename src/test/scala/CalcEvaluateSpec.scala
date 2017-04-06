import Calc._
import org.scalatest.{FunSpec, Matchers}
import atto.Atto._
import atto.ParseResult._

class CalcEvaluateSpec extends FunSpec with Matchers{

  describe("Expressions evaluated") {

    it("should return a Number from the Constant") {
      evaluate(Constant(50), Map.empty) should be (50)
    }
    it("should add two Number together to equal one Number") {
      evaluate(Add(Constant(1),Constant(2)), Map.empty) should be (3)
    }

    it("should subtract two Number together to equal one Number") {
      evaluate(Subtract(Constant(5),Constant(3)), Map.empty) should be (2)
    }

    it("should multiply two Number together to equal one Number") {
      evaluate(Multiply(Constant(10), Constant(10)), Map.empty) should be (100)
    }

    it("should divide two Number together to equal one Number") {
      evaluate(Divide(Constant(60), Constant(6)), Map.empty) should be (10)
    }

    it("should lookup variable name from environment") {
      evaluate(VariableLookup("y"), Map("y" -> Constant(10))) should be (10)
    }
  }

  describe("Expression parsers") {

    it("should take in an input char and yield two expressions") {
      val sample = arithmeticExpressionHelper('#').map(res => Add(res._1, res._2))
      sample.parseOnly("(1 # 5)").done should be (Done("", Add(Constant(1), Constant(5))))
    }
    it("should parse two numbers into Add") {
      addP.parseOnly("(5 + 10)").done should be (Done("",Add(Constant(5),Constant(10))))
    }

    it("should parse two numbers into Subtract") {
      subtractP.parseOnly("(5 - 10)").done should be (Done("",Subtract(Constant(5),Constant(10))))
    }

    it("should parse two numbers into Multiply") {
      multiplyP.parseOnly("(5 * 10)").done should be (Done("",Multiply(Constant(5),Constant(10))))
    }

    it("should parse two numbers into Divide") {
      divideP.parseOnly("(5 / 10)").done should be (Done("",Divide(Constant(5),Constant(10))))
    }

    it("should lookup variable name from environment") {
      variableLookupP.parseOnly("x").done should be (Done("", VariableLookup("x")))
    }
  }

//  describe("run method") {
//    it("should run a NakedExpression to return an evaluate of Expression") {
//      val nakedExpression = NakedExpression(Subtract(Constant(20), Constant(5)))
//      runStatements(nakedExpression) should be (15)
//    }
//    it("should run a VariableAssignment to add a variable and its Expression to the environment") {
//      val variableAssignment = VariableAssignment("x", Add(Constant(10), Constant(20)))
//      runStatements(variableAssignment) should be (Map("x" -> Add(Constant(10), Constant(20))))
//    }
//    it("should run a Print returning a println of the Expression") {
//      val print = Print(Divide(Constant(50), Constant(5)))
//      runStatements(print) should be (10)
//    }
//  }
}
