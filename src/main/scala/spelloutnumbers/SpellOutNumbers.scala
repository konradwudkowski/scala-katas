package spelloutnumbers

object SpellOutNumbers {
  def print(d: Int): String =
    if (d < 10) {
      singleDigit(d)
    } else if (d < 100) {
      tens(d)
    } else if (d < 1000) {
      hundreds(d)
    } else {
      thousands(d)
    }

  private def singleDigit(d: Int): String =
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

  private def tens(d: Int): String =
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

  private def teens(d: Int): String =
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

  private def tensPrefix(tensDigit: Int): String =
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

  private def hundreds(d: Int): String = {
    val hundredsDigit = d / 100
    val tensNumber    = d % 100

    if (tensNumber == 0) {
      s"${singleDigit(hundredsDigit)} hundred"
    } else {
      s"${singleDigit(hundredsDigit)} hundred ${print(tensNumber)}"
    }
  }

  private def thousands(d: Int): String = {
    val thousandsNumber = d / 1000
    val hundredsNumber  = d % 1000

    if (thousandsNumber == 1) {
      s"one thousand ${hundreds(hundredsNumber)}"
    } else if (hundredsNumber == 0) {
      s"${print(thousandsNumber)} thousands"
    } else {
      s"${print(thousandsNumber)} thousands ${print(hundredsNumber)}"
    }
  }

}
