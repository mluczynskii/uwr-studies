package object money {
  /* https://www.xe.com/currencyconverter/ */
  val conversion: Map[(Currency, Currency), BigDecimal] = Map(
    (EUR, PLN) -> BigDecimal("4.2295357"),
    (PLN, USD) -> BigDecimal("0.26604383"),
    (USD, EUR) -> BigDecimal("0.88869781")
  )

  val defaultConverter: CurrencyConverter = 
    CurrencyConverter(conversion)

  implicit class AsMoney(amount: Double) {
    def apply(currency: Currency): Money = 
      Money(BigDecimal(amount), currency)(defaultConverter)
  }

  val `€`: Currency = EUR
  val $: Currency = USD 
  val zl: Currency = PLN 

  implicit val moneyOrdering: Ordering[Money] = new Ordering[Money] {
    override def compare(a: Money, b: Money): Int = {
      if (a.currency == b.currency) Ordering[BigDecimal].compare(a.amount, b.amount)
      else compare(a, b `as` a.currency)
    }
  }
}

package money {
  sealed trait Currency
  case object EUR extends Currency { override def toString(): String = "€" }
  case object USD extends Currency { override def toString(): String = "$" }
  case object PLN extends Currency { override def toString(): String = "zł" }

  case class CurrencyConverter(conversion: Map[(Currency, Currency), BigDecimal]) {
    def convert(from: Currency, to: Currency): BigDecimal = {
      if (from == to) BigDecimal("1.0")
      else conversion.get(from -> to) match {
        case None => {
          val rate = conversion.getOrElse(to -> from, throw new Exception(s"unknown conversion: $from -> $to"))
          BigDecimal("1.0") / rate 
        }
        case Some(value) => value
      }
    }
  }

  case class Money(val amount: BigDecimal, val currency: Currency)
    (implicit currencyConverter: CurrencyConverter) {
    override def toString(): String = {
      val amountRounded = amount.setScale(2, BigDecimal.RoundingMode.HALF_UP)
      s"$amountRounded$currency"
    }

    def +(other: Money): Money = {
      val rate = currencyConverter.convert(other.currency, currency)
      Money(amount + rate * other.amount, currency)
    }
    def -(other: Money): Money = 
      this + Money(-other.amount, other.currency)
    def *(n: Int): Money = 
      Money(amount * n, currency)
    def as(currency: Currency): Money = {
      val rate = currencyConverter.convert(this.currency, currency)
      Money(amount * rate, currency)
    }
  }
}

object Main {
  import money._
  import scala.math.Ordered.orderingToOrdered

  def main(args: Array[String]): Unit = {
    /* Addition */
    val sum1: Money = 100.01(USD) + 200(EUR)
    val sum2: Money = 100.01(zl) + 200($)
    val sum3: Money = 5(zl) + 3(PLN) + 20.5(USD)
    println(s"1) $sum1 2) $sum2 3) $sum3")

    /* Subtraction */
    val sub: Money = 300.01(USD) - 200(EUR)
    println(s"1) $sub")

    /* Multiplication */
    val mult1: Money = 30(zl) * 20
    val mult2: Money = 20($) * 11
    println(s"1) $mult1 2) $mult2")

    /* Conversion */
    val conv1: Money = 150.01(USD) `as` PLN 
    val conv2: Money = 120.01(USD) `as` `€`
    println(s"1) $conv1 2) $conv2")

    /* Comparison */
    val compare1: Boolean = 300.30(USD) > 200(`€`)
    val compare2: Boolean = 300.30($) < 200(EUR)
    println(s"1) $compare1 2) $compare2")
  }
}
