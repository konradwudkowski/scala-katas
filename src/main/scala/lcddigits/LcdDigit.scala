package lcddigits

import cats.{Semigroup, Show}

final case class LcdDigit(
  t: String,
  m: String,
  b: String
)

object LcdDigit {

  implicit val show: Show[LcdDigit] =
    (d: LcdDigit) => List(d.t, d.m, d.b).mkString("\n")

  implicit val monoid: Semigroup[LcdDigit] =
    new Semigroup[LcdDigit] {
      def combine(x: LcdDigit, y: LcdDigit): LcdDigit =
        LcdDigit(
          t = s"${x.t} ${y.t}",
          m = s"${x.m} ${y.m}",
          b = s"${x.b} ${y.b}"
        )
    }

  def parse(d: Int): LcdDigit = {
    val lcdDigits =
      d.toString.map { c: Char =>
        parseSingleDigit(c.toString.toInt)
      }
    Semigroup.combineAllOption(lcdDigits).getOrElse(Empty)
  }

  private[lcddigits] def parseSingleDigit(d: Int): LcdDigit =
    d match {
      case 0 => `0`
      case 1 => `1`
      case 2 => `2`
      case 3 => `3`
    }

  val Empty = LcdDigit("", "", "")

  val `0` =
    LcdDigit(
      t = " _ ",
      m = "| |",
      b = "|_|"
    )

  val `1` =
    LcdDigit(
      t = "   ",
      m = "  |",
      b = "  |"
    )

  val `2` =
    LcdDigit(
      t = " _ ",
      m = " _|",
      b = "|_ "
    )

  val `3` =
    LcdDigit(
      t = " _ ",
      m = " _|",
      b = " _|"
    )
}
