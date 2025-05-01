object Utils {
  /* checks if xs is sorted according to ordering */
  def isSorted(xs: List[Int], ordering: (Int, Int) => Boolean): Boolean = 
    xs match {
      case x :: y :: xs => ordering(x, y) && isSorted(y :: xs, ordering)
      case _ => true 
    }
  def isAscSorted(xs: List[Int]): Boolean = isSorted(xs, _ < _)
  def isDescSorted(xs: List[Int]): Boolean = isSorted(xs, _ > _)

  /* applies f to a start value and all elements of xs, going left to right. */
  def foldLeft[A, B](xs: List[A], acc: B)(f: (B, A) => B): B =
    xs match {
      case x :: xs => foldLeft(xs, f(acc, x))(f)
      case _ => acc
    }
  def sum(xs: List[Int]): Int = foldLeft(xs, 0)(_ + _)
  def length(xs: List[Int]): Int = foldLeft(xs, 0)((x, _) => x + 1)

  /* composes two unary functions f and g */
  def compose[A, B, C](f: B => C, g: A => B): A => C = 
    a => f(g(a))
  def repeated(f: Int => Int, n: Int): Int =
    foldLeft((1 to n).toList, Predef.identity[Int] _)((g, x) => compose(f, g))(n)

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    a => (b => f(a, b))
  def uncurry[A, B, C](f: A => (B => C)): (A, B) => C =
    (a, b) => f(a)(b)

  def unSafe[T](ex: Exception)(m: => T) =
    try m
    catch { case e: Exception => println(e.getMessage); throw ex }
}

object Main {
  def main(args: Array[String]): Unit = {
    val ord: Ordering[Int] = Ordering.Int 
    val xs = (1 to 20).toList

    /* isSorted tests */
    Predef.assert(Utils.isSorted(xs, ord.lt), "isSorted: invalid result")
    Predef.assert(Utils.isAscSorted(xs), "isAscSorted: invalid result")
    Predef.assert(!Utils.isDescSorted(xs), "isDescSorted: invalid result")

    /* foldLeft tests */
    Predef.assert(Utils.sum(xs) == xs.foldLeft(0)(_ + _), "sum: invalid result")
    Predef.assert(Utils.length(xs) == xs.length, "length: invalid result")

    /* compose and repeated tests */
    val f: Int => Int = x => x + 1
    val g: Int => Int = x => x * 2 
    Predef.assert(Utils.compose(f, g)(3) == 7, "compose: invalid result")
    Predef.assert(Utils.repeated(f, 5) == 10, "repeated: invalid result")

    /* curry/uncurry tests */
    val op: (Int, Int) => Int = _ + _
    val op_c: Int => (Int => Int) = x => y => x * y
    Predef.assert(Utils.curry(op)(1)(2) == op(1, 2), "curry: invalid result")
    Predef.assert(Utils.uncurry(op_c)(2, 10) == op_c(2)(10), "uncurry: invalid result")

    Utils.unSafe(new Exception("All tests passed!"))(1 / 0)
  }
}