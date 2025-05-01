package com.mluczynski.cards {
  sealed trait Suit
  case object Hearts extends Suit 
  case object Spades extends Suit 
  case object Clubs extends Suit 
  case object Diamonds extends Suit 

  sealed trait Rank
  case object Ace extends Rank 
  case class Numerical(n: Int) extends Rank {
    Predef.require(n >= 2 && n <= 10, s"$n is not a valid numerical card!")
    override def toString(): String = n.toString()
  }

  sealed trait Face extends Rank
  case object Jack extends Face 
  case object Queen extends Face 
  case object King extends Face 

  case class Card(suit: Suit, rank: Rank) {
    override def toString(): String = 
      s"$rank of $suit"
  }
  object Card {
    def apply(suit: Suit, n: Int): Card = {
      val numeral = Numerical(n)
      new Card(suit, numeral)
    }
  }
}

package com.mluczynski.deck {
  import com.mluczynski.cards._
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
        case _ => false
      })

    /* amount of numerical cards with value n in the deck */
    def amountOfNumerical(n: Int): Int =
      cards.count(_ match {
        case Card(_, Numerical(`n`)) => true 
        case _ => false 
      })

    /* amount of ALL numerical cards in the deck */
    val amountWithNumerical: Int = 
      (2 to 10).toList.map(this.amountOfNumerical(_)).sum

    /* amount of a face card in the deck */
    def amountOfFace(face: Face): Int =
      cards.count(_ match {
        case Card(_, `face`) => true 
        case _ => false
      })

    val amountWithFace: Int =
      List(Jack, Queen, King).map(this.amountOfFace(_)).sum 
  }
  object Deck {
    val standard: List[Card] = {
      val suits = List(Hearts, Spades, Clubs, Diamonds)
      val numerical = (2 to 10).toList.flatMap(n => suits.map(s => Card(s, n)))
      val other = List(Ace, Jack, Queen, King).flatMap(rank => suits.map(s => Card(s, rank)))
      numerical ::: other
    }
    def apply(cards: List[Card]): Deck =
      new Deck(cards)
    def apply(): Deck = 
      Deck(Random.shuffle(Deck.standard))
  }
}

package com.mluczynski.games {
  import com.mluczynski.deck._
  import com.mluczynski.cards._
  import scala.util.Random

  object Blackjack {
    def score(card: Card): List[Int] = {
      card.rank match {
        case Ace => List(1, 11)
        case _: Face => List(10)
        case Numerical(n) => List(n)
      } 
    }

    def apply(n: Int): Blackjack = {
      val cards = (1 to n).toList.flatMap(_ => Deck.standard)
      val deck = Deck(Random.shuffle(cards))
      new Blackjack(deck)
    }

    def apply(deck: Deck): Blackjack = new Blackjack(deck)

    def update_score(score: List[Int], card: Card): List[Int] =
      score.flatMap(s => Blackjack.score(card).map(_ + s))

    @scala.annotation.tailrec def subseq(xs: List[Card], acc: List[List[Card]]): List[List[Card]] = {
      xs match {
        case Nil => acc 
        case x :: xs => subseq(xs, acc ::: acc.map(ys => ys ::: List(x)))
      }
    }
  }

  /* scuffed blackjack */
  class Blackjack(deck: Deck) {
    def play(n: Int): Unit = {
      Predef.require(n > 0, "n must be a positive integer!")
      var temp: Deck = deck
      var score: List[Int] = List(0)
      println("You drew:")
      for (_ <- 1 to n) {
        val card: Card = temp.top()
        println(card)
        temp = temp.pull()
        score = Blackjack.update_score(score, card)
      }
      val diff = score.map(_ - 21).sortWith((a, b) => a.abs < b.abs)
      val final_score = 21 + diff.head 
      if (final_score > 21) println(s"Final score: $final_score...")
      else println(s"Final score: $final_score!")
    }

    lazy val all21: List[List[Card]] = {
      Blackjack.subseq(deck.cards, List(Nil)).filter{seq => 
        val possible = seq.foldLeft(List(0))(Blackjack.update_score(_, _))
        possible.exists(_ == 21)
      }
    }

    def first21(): Unit = {
      val first = this.all21
      assert(first.nonEmpty, "There is no sequence of cards with score equal to 21")
      println("First sequence of cards with score equal to 21 is:")
      first.head.foreach(println)
    }
  }
}

object Main {
  import com.mluczynski.deck._
  import com.mluczynski.cards._
  import com.mluczynski.games._
  def main(args: Array[String]): Unit = {
    val standard = Deck()
    assert(standard.isStandard, "isStandard: invalid result")
    assert(standard.amountWithFace == 12, "amountWithFace: invalid result")
    assert(standard.amountWithNumerical == 36, "amountWithNumerical: invalid result")
    assert(standard.amountOfColor(Hearts) == 13, "amountOfColor: invalid result")
    assert(standard.amountOfNumerical(4) == 4, "amountOfNumerical: invalid result")
    val game = Blackjack(5)
    game.play(3)
    val simple_deck = Deck(List(Card(Hearts, 10), Card(Spades, 2), Card(Clubs, Ace)))
    val simple_game = Blackjack(simple_deck)
    simple_game.first21()
    println("Every test passed! 🎇")
  }
}