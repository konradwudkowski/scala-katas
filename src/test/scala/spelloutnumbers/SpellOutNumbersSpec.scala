package spelloutnumbers

import org.scalatest.{Matchers, WordSpec}

class SpellOutNumbersSpec extends WordSpec with Matchers {
  "Spelling out a number" should {
    "work for single digits" in {
      val expectations =
        Map(
          0 -> "zero",
          1 -> "one",
          2 -> "two",
          3 -> "three",
          4 -> "four",
          5 -> "five",
          6 -> "six",
          7 -> "seven",
          8 -> "eight",
          9 -> "nine"
        )

      expectations.foreach {
        case (d, expectedTextRepr) =>
          SpellOutNumbers.print(d) shouldBe expectedTextRepr
      }
    }

    "work for the number 10 & teen numbers" in {
      val expectations =
        Map(
          10 -> "ten",
          11 -> "eleven",
          12 -> "twelve",
          13 -> "thirteen",
          14 -> "fourteen",
          15 -> "fifteen",
          16 -> "sixteen",
          17 -> "seventeen",
          18 -> "eighteen",
          19 -> "nineteen"
        )

      expectations.foreach {
        case (d, expectedTextRepr) =>
          SpellOutNumbers.print(d) shouldBe expectedTextRepr
      }
    }

    "work for tens between 20 & 99" in {
      val expectations =
        Map(
          20 -> "twenty",
          21 -> "twenty one",
          32 -> "thirty two",
          42 -> "forty two",
          53 -> "fifty three",
          64 -> "sixty four",
          75 -> "seventy five",
          86 -> "eighty six",
          99 -> "ninety nine"
        )

      expectations.foreach {
        case (d, expectedTextRepr) =>
          SpellOutNumbers.print(d) shouldBe expectedTextRepr
      }
    }

    "work for hundreds" in {
      val expectations =
        Map(
          100 -> "one hundred",
          205 -> "two hundred five",
          311 -> "three hundred eleven",
          420 -> "four hundred twenty",
          531 -> "five hundred thirty one",
          600 -> "six hundred",
          799 -> "seven hundred ninety nine",
          888 -> "eight hundred eighty eight",
          999 -> "nine hundred ninety nine"
        )

      expectations.foreach {
        case (d, expectedTextRepr) =>
          SpellOutNumbers.print(d) shouldBe expectedTextRepr
      }
    }

    "work for thousands" in {
      val expectations =
        Map(
          1000  -> "one thousand",
          1001  -> "one thousand one",
          2000  -> "two thousands",
          2002  -> "two thousands two",
          3019  -> "three thousands nineteen",
          4040  -> "four thousands forty",
          10451 -> "ten thousands 4 hundred fifty one"
        )

      expectations.foreach {
        case (d, expectedTextRepr) =>
          SpellOutNumbers.print(d) shouldBe expectedTextRepr
      }
    }
  }

}
