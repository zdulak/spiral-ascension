import scala.collection.mutable.ArrayBuffer

object SpiralAscension extends App {
  printSpiral(createSpiral(10))

  def printSpiral(spiral: Seq[Seq[Int]]): Unit = {
    val maxLength = (spiral.size * spiral.size).toString.length
    spiral.foreach(row => {
      val rowString = row.map(slot => s"%1$$${maxLength}d".format(slot)).mkString(" ")
      println(rowString)
    })
  }

  def createSpiral(n: Int): Seq[Seq[Int]] = {
    val startX = if (n % 2 == 0) n/ 2 else (1 - n) / 2
    val startY = if (n % 2 == 0) (1 - n) / 2 else n / 2
    val endX = if (n % 2 == 0) (1 - n) / 2 - 1 else n / 2 + 1
    val endY = if (n % 2 == 0) n/ 2 + 1 else (1 - n) / 2 - 1
    val dx = if (n % 2 == 0) -1 else 1
    val dy = if (n % 2 == 0) 1 else -1
    val reversedMatrix =
      for (y <- startY until endY by dy) yield
        for (x <- startX until endX by dx) yield
          n * n - spiralIndex(x, y)
    reversedMatrix.map(_.reverse).reverse
  }

  def  spiralIndex(x: Int, y: Int): Int = {
    if (y * y >= x * x) {
      val index = 4 * y * y - y - x
      if (y < x) index - 2 * (y - x) else index
    }
    else {
      val index = 4 * x * x - y - x
      if (y < x) index + 2 * (y - x) else index
    }
  }


}
