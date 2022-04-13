object SpiralAscension extends App {
  printSpiral(createSpiral(5))

  def printSpiral(spiral: Seq[Seq[Int]]): Unit = {
    val maxLength = (spiral.size * spiral.size).toString.length
    spiral.foreach(row => {
      val rowString = row.map(slot => s"%1$$${maxLength}d".format(slot)).mkString(" ")
      println(rowString)
    })
  }

  def createSpiral(size: Int): List[List[Int]] = {
    def spiralHelper(numberRows: Int, lineLength: Int, start: Int): List[List[Int]] = {
      if (numberRows == 0 || lineLength == 0) Nil else {
        (start until start + lineLength).toList ::
        spiralHelper(lineLength, numberRows - 1, start + lineLength).reverse.transpose
      }
    }

    spiralHelper(size, size, 1)
  }
}

