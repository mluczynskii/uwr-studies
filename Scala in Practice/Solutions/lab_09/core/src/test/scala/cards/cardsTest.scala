package cards

import org.scalatest.flatspec.AnyFlatSpec

class CardSpec extends AnyFlatSpec {
  "Numerical" should "throw IllegalArgumentException if wrong number is passed" in {
    assertThrows[IllegalArgumentException] {
      val card = Numerical(11)
    }
  }

  "A Card" should "format properly" in {
    val card = Card(Hearts, Jack)
    assert(card.toString == "Jack of Hearts")
  }
}
