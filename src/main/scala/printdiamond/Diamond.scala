package printdiamond

final case class Diamond(c: Char) {
  def stringify: String = rows.mkString("\n")

  private[printdiamond] val width: Int = 1 + (('A' to c).size - 1) * 2

  private[printdiamond] def rows: List[String] = {
    val verticalChars = ('A' to c).toList ::: ('A' to (c - 1).toChar).toList.reverse
    verticalChars.map(row)
  }

  private[printdiamond] def row(char: Char): String = {
    val mid    = width / 2 + 1
    val offset = ('A' to char).size - 1
    val left   = mid - offset
    val right  = mid + offset

    (1 to width).toList.map { n =>
      if (n == left || n == right) char.toString else " "
    }.mkString
  }
}
