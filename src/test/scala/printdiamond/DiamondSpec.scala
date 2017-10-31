package printdiamond

import org.scalatest.{FlatSpec, Matchers}

/*
  Given a letter print a diamond starting with 'A'
  with the supplied letter at the widest point.

  For example: print-diamond 'E' prints

      A
     B B
    C   C
   D     D
  E       E
   D     D
    C   C
     B B
      A

  For example: print-diamond 'C' prints

    A
   B B
  C   C
   B B
    A

 */

class DiamondSpec extends FlatSpec with Matchers {
  behavior of "Printing a diamond"

  it should "print expected shape" in {
    val expected =
      Map(
        List(
          "A"
        ) -> 'A',
        List(
          " A ",
          "B B",
          " A "
        ) -> 'B',
        List(
          "  A  ",
          " B B ",
          "C   C",
          " B B ",
          "  A  "
        ) -> 'C',
        List(
          "    A    ",
          "   B B   ",
          "  C   C  ",
          " D     D ",
          "E       E",
          " D     D ",
          "  C   C  ",
          "   B B   ",
          "    A    "
        ) -> 'E'
      )

    expected.foreach {
      case (expectedResult, letter) =>
        Diamond(letter).rows shouldBe expectedResult
    }
  }

}
