package games

import cards._
import deck._
import scala.util.Random

object Blackjack {

  def score(card: Card): List[Int] = {
    card.rank match {
      case Ace          => List(1, 11)
      case _: Face      => List(10)
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

  @scala.annotation.tailrec
  def subseq(xs: List[Card], acc: List[List[Card]]): List[List[Card]] = {
    xs match {
      case Nil     => acc
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
    Blackjack.subseq(deck.cards, List(Nil)).filter { seq =>
      val possible = seq.foldLeft(List(0))(Blackjack.update_score(_, _))
      possible.exists(_ == 21)
    }
  }

  def first21(): Unit = {
    val first = this.all21
    assert(
      first.nonEmpty,
      "There is no sequence of cards with score equal to 21"
    )
    println("First sequence of cards with score equal to 21 is:")
    first.head.foreach(println)
  }

}
