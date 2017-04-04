import Calc._
import org.scalatest.{FunSpec, Matchers}
import atto.Atto._
import atto.ParseResult._

class CalcParserSpec extends FunSpec with Matchers{

  describe("Expressions evaluated") {
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
  }

  describe("Expression parsers") {
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

    it("should lookup variablename from environemnt") {
      variableLookupP.parseOnly("x").done should be (Done("", VariableLookup("x")))
    }
  }
}
