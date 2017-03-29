import atto._, Atto._, compat.scalaz._

case class NameAndNumber(name: String, number: Int)

object Experiment {
  val nameP: Parser[String] = many1(lower).map(char => char.list.toList.mkString)
  val dashP: Parser[Char] = char('-')
  val numberP: Parser[Int] = int
  val newlineP: Parser[Char] = char('\n')
  val nameAndNumberP: Parser[NameAndNumber] = for {
    name <- nameP
    _ <- dashP
    number <- numberP
  } yield NameAndNumber(name, number)
  val listOfNameAndNumberP: Parser[List[NameAndNumber]] = sepBy(nameAndNumberP, newlineP)

  def main(args: Array[String]): Unit = {
    val string = "hello-123\nhello-456\ndom-666"
    val result = listOfNameAndNumberP.parseOnly(string).done
    println(result)
  }
}