package com.mluczynski.numbers {
  class Rational(n: Int, d: Int = 1) {
    Predef.require(d > 0, "Denominator must be greater than 0")

    private val div = Rational.gcd(n.abs, d)
    val numer: Int = n / div 
    val denom: Int = d / div

    override def toString = 
      s"$numer/$denom"
    def +(other: Rational): Rational = 
      Rational(numer*other.denom + other.numer*denom, denom*other.denom)
    def unary_- = 
      Rational(-numer, denom)
    def -(other: Rational): Rational = 
      this + (- other)
    def *(other: Rational): Rational =
      Rational(numer*other.numer, denom*other.denom)
    def /(other: Rational): Rational = 
      this * Rational.invert(other)
    def ==(other: Rational): Boolean =
      numer == other.numer && denom == other.denom
    def >(other: Rational): Boolean =
      numer * other.denom > other.numer * denom
  }

  object Rational {
    def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

    val zero: Rational = new Rational(0, 1)
    val one: Rational = new Rational(1, 1)

    def apply(n: Int, d: Int) = new Rational(n, d)
    def apply(n: Int) = new Rational(n, 1)

    def invert(x: Rational): Rational = {
      Predef.require(x.numer != 0)
      new Rational(x.denom, x.numer)
    }

    def abs(x: Rational): Rational =
      new Rational(Math.abs(x.numer), x.denom)
  }
}

package com.mluczynski.figures {
  import com.mluczynski.numbers._
  class Point(val x: Rational, val y: Rational)

  abstract class Shape {
    def area: Rational
    val description: String
    override def toString = s"$description, area: $area"
  }

  class Triangle(a: Point, b: Point, c: Point) extends Shape {
    def area: Rational =
      Rational.abs(a.x*(b.y-c.y) + b.x*(c.y-a.y) + c.x*(a.y-b.y)) / Rational(2)
    val description: String = "Triangle"
  }

  class Rectangle(a: Point, b: Point, c: Point, d: Point) extends Shape {
    Predef.require(Rectangle.checkProper(a, b, c, d), "Points do not form a rectangle!")
    def area: Rational = 
      Triangle(a, b, c).area + Triangle(a, c, d).area
    val description: String = "Rectangle"
  }

  class Square(a: Point, b: Point, c: Point, d: Point) extends Rectangle(a, b, c, d) {
    Predef.assert(Square.checkProper(a, b, c, d), "Points do not form a square!")
    override val description: String = "Square"
  }

  object Point {
    def apply(a: Integer, b: Integer, c: Integer, d: Integer) = 
      new Point(Rational(a, b), Rational(c, d))
    def apply(x: Integer, y: Integer) = new Point(Rational(x), Rational(y))
    def apply(x: Rational, y: Rational) = new Point(x, y)
  }

  object Triangle {
    def apply(a: Point, b: Point, c: Point) = 
      new Triangle(a, b, c)
  }

  object Rectangle {
    /* checks if all points are equally distant from their center of mass */
    def checkProper(a: Point, b: Point, c: Point, d: Point): Boolean = {
      val cx = (a.x + b.x + c.x + d.x) / Rational(4)
      val cy = (a.y + b.y + c.y + d.y) / Rational(4)
      val ds = List(a, b, c, d).map{p => (cx-p.x) * (cx-p.x) + ((cy-p.y) * (cy-p.y))}
      ds forall {_ == ds.head}
    }

    def apply(a: Point, b: Point, c: Point, d: Point) =
      new Rectangle(a, b, c, d)
  }

  object Square {
    /* checks if sides are of equal length */
    def checkProper(a: Point, b: Point, c: Point, d: Point): Boolean = {
      /* clockwise point ordering */
      val List(aa, bb, dd, cc) = List(a, b, c, d).sortWith{(p, q) => 
        if (p.y > q.y) true 
        else p.x > q.x
      }
      val d1 = (bb.x-aa.x) * (bb.x-aa.x) + ((bb.y-aa.y) * (bb.y-aa.y))
      val d2 = (bb.x-cc.x) * (bb.x-cc.x) + ((bb.y-cc.y) * (bb.y-cc.y))
      d1 == d2
    }
    def apply(a: Point, b: Point, c: Point, d: Point) = new Square(a, b, c, d)
  }
}

object Main {
  import com.mluczynski.numbers._ 
  import com.mluczynski.figures._

  def areaSum(xs: List[Shape]): Rational =
    xs.foldLeft(Rational.zero){(acc, fig) => acc + fig.area}
  def printAll(xs: List[Shape]): Unit =
    xs foreach println

  def main(args: Array[String]): Unit = {
    /* numbers.Rational tests */
    val x = Rational(10, 5)
    println(x)
    println(Rational.invert(x))
    println(-x)
    println(x * x)
    println(x / x)
    println(x - x)

    /* figures.Shape tests */
    val zero = Rational.zero 
    val one = Rational.one
    val y = Triangle(Point(zero, zero), Point(one, zero), Point(zero, one))
    println(y)

    val z = Rectangle(Point(zero, zero), Point(one, zero), Point(zero, one), Point(one, one))
    println(z)

    try {
      val error = Rectangle(Point(zero, zero), Point(one, zero), Point(zero, one), Point(420, 2137, 666, 42))
      println(error)
    } catch {
      case _: Throwable => println("Not a rectangle!")
    }

    val s = Square(Point(0, 0), Point(1, 1), Point(0, 2), Point(-1, 1))
    println(s)

    try {
      val error2 = Square(Point(0, 0), Point(2, 0), Point(2, 1), Point(0, 1))
      println(error2)
    } catch {
      case _: Throwable => println("Not a square!")
    }

    println("\nareaSum:")
    println(areaSum(List(y, z, s)))
    println("\nprintAll:")
    printAll(List(y, z, s))
  }
}