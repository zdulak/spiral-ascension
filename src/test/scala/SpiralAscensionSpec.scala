import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class SpiralAscensionSpec extends AnyFlatSpec with should.Matchers {
  behavior of "The createSpiral method"

  it should "return list with three-line spiral for size 3" in {
    SpiralAscension.createSpiral(3) shouldBe
      List(
        List(1, 2, 3),
        List(8, 9, 4),
        List(7, 6, 5)
      )
  }

  it should "return list with two-line spiral for size 2" in {
    SpiralAscension.createSpiral(2) shouldBe
      List(
        List(1, 2),
        List(4, 3)
      )
  }
}
