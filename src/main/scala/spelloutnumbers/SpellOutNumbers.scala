package spelloutnumbers

object SpellOutNumbers {
  def print(d: Long): String = {

    val groupedByThreeDigits =
      d.toString
        .foldRight(List("")) {
          case (char, acc) =>
            if (acc.head.length < 3) {
              s"$char${acc.head}" :: acc.tail
            } else {
              char.toString :: acc
            }
        }
        .map(_.toLong)
        .reverse

    val labels           = List("", "thousand", "million", "billion")
    val numbersAndLabels = groupedByThreeDigits.zip(labels)

    def superfluousZero(n: Long, label: String) = {
      val largerLabelExists = {
        val currentLabelPosition = numbersAndLabels.map(_._2).indexOf(label)
        val maxLabelPosition     = numbersAndLabels.size - 1
        currentLabelPosition < maxLabelPosition
      }
      n == 0 && largerLabelExists
    }

    numbersAndLabels.foldLeft("") {
      case (acc, (number, label)) =>
        if (superfluousZero(number, label)) {
          acc
        } else {
          List(upToThreeDigits(number), label, acc).filter(_.nonEmpty).mkString(" ")
        }
    }
  }

  private def upToThreeDigits(d: Long): String =
    if (d < 10) {
      singleDigit(d)
    } else if (d < 100) {
      tens(d)
    } else {
      hundreds(d)
    }

  private def singleDigit(d: Long): String =
    d match {
      case 0 => "zero"
      case 1 => "one"
      case 2 => "two"
      case 3 => "three"
      case 4 => "four"
      case 5 => "five"
      case 6 => "six"
      case 7 => "seven"
      case 8 => "eight"
      case 9 => "nine"
    }

  private def tens(d: Long): String =
    if (d < 20) {
      teens(d)
    } else {
      val tensDigit  = d / 10
      val unitsDigit = d % 10

      if (unitsDigit == 0) {
        tensPrefix(tensDigit)
      } else {
        s"${tensPrefix(tensDigit)} ${singleDigit(unitsDigit)}"
      }
    }

  private def teens(d: Long): String =
    d match {
      case 10 => "ten"
      case 11 => "eleven"
      case 12 => "twelve"
      case 13 => "thirteen"
      case 14 => "fourteen"
      case 15 => "fifteen"
      case 16 => "sixteen"
      case 17 => "seventeen"
      case 18 => "eighteen"
      case 19 => "nineteen"
    }

  private def tensPrefix(tensDigit: Long): String =
    tensDigit match {
      case 2 => "twenty"
      case 3 => "thirty"
      case 4 => "forty"
      case 5 => "fifty"
      case 6 => "sixty"
      case 7 => "seventy"
      case 8 => "eighty"
      case 9 => "ninety"
    }

  private def hundreds(d: Long): String = {
    val hundredsDigit = d / 100
    val tensNumber    = d % 100

    if (tensNumber == 0) {
      s"${singleDigit(hundredsDigit)} hundred"
    } else if (tensNumber < 10) {
      s"${singleDigit(hundredsDigit)} hundred ${singleDigit(tensNumber)}"
    } else {
      s"${singleDigit(hundredsDigit)} hundred ${tens(tensNumber)}"
    }
  }

}
