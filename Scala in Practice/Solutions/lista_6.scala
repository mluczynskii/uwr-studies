package com.mluczynski.pizzeria {
  import com.mluczynski.pizzeria.pizzeria._
  sealed trait Priced

  sealed trait Flavor extends Priced
  case object Margarita extends Flavor
  case object Pepperoni extends Flavor 
  case object Funghi extends Flavor 

  sealed trait Size 
  case object Large extends Size 
  case object Regular extends Size 
  case object Small extends Size 

  sealed trait Crust
  case object Thick extends Crust 
  case object Thin extends Crust

  sealed trait Topping extends Priced
  case object Ketchup extends Topping 
  case object Garlic extends Topping

  sealed trait Meat extends Priced
  case object Salami extends Meat

  sealed trait Drink extends Priced 
  case object Lemonade extends Drink

  case class Pizza(
    flavor: Flavor,
    size: Size,
    crust: Crust,
    extraMeat: Option[Meat],
    extraTopping: Option[Topping]
  ) {
    override def toString(): String = {
      val meat = extraMeat.map("extra " + _).getOrElse("no extra meat")
      val topping = extraTopping.map("extra " + _).getOrElse("no extra topping")
      s"$size $flavor on $crust crust with $meat and $topping"
    }

    val price: Double = {
      val base = priceModifier(size) * itemPrice(Some(flavor))
      val extra = itemPrice(extraMeat) + itemPrice(extraTopping)
      base + extra
    }
  }

  package object pizzeria {
    def itemPrice(item: Option[Priced]): Double = {
      item match {
        case Some(value) => value match {
          case Margarita => 5.0
          case Pepperoni => 6.5
          case Funghi => 7.0
          case Ketchup => 0.5
          case Garlic => 0.5
          case Salami => 1.0
          case Lemonade => 2.0
        }
        case None => 0.0
      }
    }

    def priceModifier(mod: Size): Double = {
      mod match {
        case Large => 1.5
        case Regular => 1.0
        case Small => 0.9
      }
    }
  }
}

package com.mluczynski.orders {
  import com.mluczynski.pizzeria._
  import com.mluczynski.pizzeria.pizzeria._
  import com.mluczynski.orders.orders._
  import scala.util.matching.Regex

  sealed trait Discount
  case object Student extends Discount 
  case object Senior extends Discount 

  package object orders {
    def discountModifier(mod: Option[Discount]): (Double, Double) = {
      mod match {
        case Some(value) => value match {
          case Student => (0.95, 1.0)
          case Senior => (0.93, 0.93)
        }
        case None => (1.0, 1.0)
      }
    }
  }

  class Order(
    name: String,
    address: String,
    phone: String,
    pizzas: List[Pizza],
    drinks: List[Drink],
    discount: Option[Discount],
    specialInfo: Option[String]
  ) {
    private val phoneRegex: Regex = "\\+[0-9]{2} [0-9]{9}".r
    Predef.require(phoneRegex.matches(phone), "Invalid phone number")

    override def toString(): String = {
      val pizzasFormatted = pizzas.map(pizza => s"${pizza.toString} - ${pizza.price}$$")
      val drinksFormatted = drinks.map(drink => s"$drink - ${itemPrice(Some(drink))}$$")
      s"""Order for $name
         |Phone number: $phone 
         |Address: $address 
         |------------------
         |${pizzasFormatted.mkString("\n")}
         |${drinksFormatted.mkString("\n")}
         |
         |Total = $price$$
         |------------------
         |Discount: ${discount.getOrElse("None")}
         |Special information: ${specialInfo.getOrElse("None")}""".stripMargin
    }

    def extraMeatPrice: Option[Double] = {
      val sum = pizzas.foldLeft(0.0){(acc, pizza) => 
        itemPrice(pizza.extraMeat) + acc
      }
      Option.when(sum > 0)(sum)
    }
    def pizzasPrice: Option[Double] = {
      val sum = pizzas.foldLeft(0.0){_ + _.price}
      Option.when(sum > 0)(sum)
    }
    def drinksPrice: Option[Double] = {
      val sum = drinks.foldLeft(0.0){(acc, drink) => 
        itemPrice(Some(drink)) + acc  
      }
      Option.when(sum > 0)(sum)
    }
    def priceByFlavor(flavor: Flavor): Option[Double] = {
      val sum = pizzas.filter{_.flavor == flavor}.foldLeft(0.0){_ + _.price}
      Option.when(sum > 0)(sum)
    }
    val price: Double = {
      val (pizzaDiscount, drinkDiscount) = discountModifier(discount)
      val pizzaPrice = pizzasPrice.getOrElse(0.0) * pizzaDiscount
      val drinkPrice = drinksPrice.getOrElse(0.0) * drinkDiscount
      pizzaPrice + drinkPrice
    }
  }
}

object Main {
  import com.mluczynski.orders._
  import com.mluczynski.pizzeria._

  def main(args: Array[String]): Unit = {
    val p1 = Pizza(Margarita, Regular, Thick, None, Some(Ketchup))
    val p2 = Pizza(Funghi, Large, Thin, Some(Salami), None)
    val p3 = Pizza(Pepperoni, Small, Thick, Some(Salami), Some(Garlic))

    val drinks = List(Lemonade, Lemonade)
    val pizzas = List(p1, p2, p3)

    val order = new Order(
      "Hymel Jadwiga",
      "ul. Łączna 43 Lipinki Łużyckie",
      "+48 420213769",
      pizzas,
      drinks,
      Some(Senior),
      Some("Styrta is burning")
    )

    println(order.toString)
  }
}