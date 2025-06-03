package deck

import cards._
import scala.util.Random

class Deck(val cards: List[Card]) {

  /* new deck without top card */
  def pull(): Deck = {
    Predef.require(!cards.isEmpty, "Deck cannot be empty!")
    Deck(cards.tail)
  }

  /* get the top card */
  def top(): Card = {
    Predef.require(!cards.isEmpty, "Deck is empty - there is no top card!")
    cards.head
  }

  /* new deck with added card on top */
  def push(c: Card): Deck = Deck(c :: cards)

  def push(suit: Suit, rank: Rank): Deck = {
    val card = Card(suit, rank)
    Deck(card :: cards)
  }

  def push(suit: Suit, n: Int): Deck = {
    val card = Card(suit, n)
    Deck(card :: cards)
  }

  /* check if deck is a standard playing deck */
  val isStandard: Boolean =
    Deck.standard.forall(this.duplicatesOfCard(_) == 0) && cards.length == 52

  /* number of duplicates of the given card inside the deck */
  def duplicatesOfCard(card: Card): Int =
    Math.max(cards.count(_ == card) - 1, 0)

  /* amount of cards in the deck with the given color */
  def amountOfColor(suit: Suit): Int =
    cards.count(_ match {
      case Card(`suit`, _) => true
      case _               => false
    })

  /* amount of numerical cards with value n in the deck */
  def amountOfNumerical(n: Int): Int =
    cards.count(_ match {
      case Card(_, Numerical(`n`)) => true
      case _                       => false
    })

  /* amount of ALL numerical cards in the deck */
  val amountWithNumerical: Int =
    (2 to 10).toList.map(this.amountOfNumerical(_)).sum

  /* amount of a face card in the deck */
  def amountOfFace(face: Face): Int =
    cards.count(_ match {
      case Card(_, `face`) => true
      case _               => false
    })

  val amountWithFace: Int =
    List(Jack, Queen, King).map(this.amountOfFace(_)).sum

}

object Deck {

  val standard: List[Card] = {
    val suits = List(Hearts, Spades, Clubs, Diamonds)
    val numerical = (2 to 10).toList.flatMap(n => suits.map(s => Card(s, n)))
    val other = List(Ace, Jack, Queen, King).flatMap(rank =>
      suits.map(s => Card(s, rank))
    )
    numerical ::: other
  }

  def apply(cards: List[Card]): Deck =
    new Deck(cards)

  def apply(): Deck =
    Deck(Random.shuffle(Deck.standard))

}
