package cards

sealed trait Suit
case object Hearts extends Suit
case object Spades extends Suit
case object Clubs extends Suit
case object Diamonds extends Suit

sealed trait Rank
sealed trait Face extends Rank
case object Jack extends Face
case object Queen extends Face
case object King extends Face
case object Ace extends Rank

case class Numerical(n: Int) extends Rank {
  Predef.require(n >= 2 && n <= 10)
  override def toString(): String = n.toString
}

case class Card(val suit: Suit, val rank: Rank) {

  override def toString(): String =
    s"$rank of $suit"

}

case object Card {

  def apply(suit: Suit, n: Int): Card =
    Card(suit, Numerical(n))

}
