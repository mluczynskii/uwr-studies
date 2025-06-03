package deck

import org.scalatest.flatspec.AnyFlatSpec
import cards._

class DeckSpec extends AnyFlatSpec {
  "A Deck" should "throw IllegalArgumentException if an empty deck is pulled" in {
    val deck = Deck(Nil)
    assertThrows[IllegalArgumentException] {
      deck.pull
    }
  }

  it should "return the tail when calling pull()" in {
    val deck = Deck()
    assert(deck.pull.cards == deck.cards.tail)
  }

  it should "return the top card when calling top()" in {
    val cards = List(Card(Spades, Ace), Card(Hearts, 10))
    val deck = Deck(cards)
    assert(deck.top == Card(Spades, Ace))
    assert(deck.pull.top == Card(Hearts, 10))
  }

  it should "return true when calling isStandard() on a standard deck" in {
    val deck = Deck()
    assert(deck.isStandard)
  }

  it should "properly detect card duplicates" in {
    val cards = List(Card(Hearts, Jack), Card(Spades, Jack), Card(Hearts, Jack), Card(Spades, Jack))
    val deck = Deck(cards)
    assert(deck.duplicatesOfCard(Card(Hearts, Jack)) == 1)
    assert(deck.duplicatesOfCard(Card(Spades, Jack)) == 1)
    assert(deck.duplicatesOfCard(Card(Clubs, Ace)) == 0)
  }

  it should "properly count amount of face cards in a standard deck" in {
    val deck = Deck()
    assert(deck.amountWithFace == 12)
    assert(deck.amountOfFace(Jack) == 4)
  }

  it should "properly count amount of numerical cards in a standard deck" in {
    val deck = Deck()
    assert(deck.amountWithNumerical == 36)
    assert(deck.amountOfNumerical(3) == 4)
  }

  it should "properly count the amount of colored cards in a standard deck" in {
    val deck = Deck()
    assert(deck.amountOfColor(Spades) == 13)
    assert(deck.amountOfColor(Clubs) == 13)
    assert(deck.amountOfColor(Diamonds) == 13)
  }
}