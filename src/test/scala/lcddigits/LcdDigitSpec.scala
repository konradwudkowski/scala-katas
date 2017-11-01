package lcddigits

import cats.Show
import org.scalatest.{FlatSpec, Matchers}

class LcdDigitSpec extends FlatSpec with Matchers {

  import LcdDigit._

  behavior of "LCD digits"

  it should "create sequence of lcd digits" in {

    val expected: Map[Int, LcdDigit] = Map(
      321 -> LcdDigit(
        " _   _     ",
        " _|  _|   |",
        " _| |_    |"
      ),
      12 -> LcdDigit(
        "     _ ",
        "  |  _|",
        "  | |_ "
      )
    )

    expected.foreach {
      case (d, expected) =>
        val lcdDigit = LcdDigit.parse(d)
        lcdDigit shouldBe expected
        println(Show[LcdDigit].show(lcdDigit))
    }

  }

}
