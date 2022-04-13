import scala.collection.mutable.ArrayBuffer

object SpiralAscension extends App {
  printSpiral(createSpiral(3))

  def printSpiral(spiral: Seq[Seq[Int]]): Unit = {
    val maxLength = (spiral.size * spiral.size).toString.length
    spiral.foreach(row => {
      val rowString = row.map(slot => s"%1$$${maxLength}d".format(slot)).mkString(" ")
      println(rowString)
    })
  }

  def createSpiral(size: Int): List[List[Int]] = {
    def spiralHelper(numberLines: Int, lineLength: Int, start: Int): List[List[Int]] = {
      if (numberLines == 0) Nil else {
        (start until start + lineLength).toList ::
        spiralHelper(lineLength, numberLines - 1, start + lineLength).reverse.transpose
      }
    }

    spiralHelper(size, size, 1)
  }
}

