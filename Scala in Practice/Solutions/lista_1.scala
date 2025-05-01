//scalar product of two vectors xs and ys
def scalarUgly(xs: List[Int], ys: List[Int]): Int = {
  var result = 0
  for (p <- xs zip ys) result = result + p._1 * p._2 
  result  
}
def scalar(xs: List[Int], ys: List[Int]): Int = 
  (xs zip ys).foldLeft(0){(acc, p) => acc + p._1 * p._2}  
  
//quicksort algorithm
def sortUgly(xs: List[Int]): List[Int] = ???
def sort(xs: List[Int]): List[Int] = xs match {
  case x :: xs => 
    val (pref, suf) = xs partition {n => n < x}
    sort(pref) ::: x :: sort(suf)
  case _ => Nil
}

//checks if n is prime
def isPrimeUgly(n: Int): Boolean = {
  if (n < 2) false 
  else {
    var flag = true
    for (m <- 2 to math.sqrt(n).toInt) flag = flag && (n % m != 0)
    flag
  }
}
def isPrime(n: Int): Boolean = 
  n >= 2 && ((2 to math.sqrt(n).toInt) forall {m => n % m != 0})

//for given positive integer n, find all pairs of integers i and j, where 1 ≤ j < i < n such that i + j is prime
def primePairsUgly(n : Int): List[(Int, Int)] = {
  var result: List[(Int, Int)] = Nil 
  for (j <- 1 until n; i <- (j+1) until n)
    if (isPrime(i + j)) result = (i, j) :: result else ()
  result
}
def primePairs(n : Int): List[(Int, Int)] = for {
  j <- (1 until n).toList
  i <- (j + 1) until n 
  if isPrime(i + j)
} yield (i, j)

//create a list with all lines from given file
val filesHere = new java.io.File(".").listFiles
def fileLinesUgly(file: java.io.File): List[String] = {
  val scanner = new java.util.Scanner(file)
  var result: List[String] = Nil 
  do {
    result = scanner.nextLine() :: result
  } while (scanner.hasNextLine)
  scanner.close()
  result.reverse
}
def fileLines(file: java.io.File): List[String] = {
  val source = scala.io.Source.fromFile(file)
  try source.getLines().toList
  finally source.close()
}

//print names of all .scala files which are in filesHere & are non empty
def printNonEmptyUgly(pattern: String): Unit = {
  for (f <- filesHere) {
    val name = f.getName
    if (f.isFile && !fileLines(f).isEmpty && name.endsWith(pattern)) println(name) else ()
  }
}
def printNonEmpty(pattern: String): Unit =
  filesHere.filter{f => f.isFile && !fileLines(f).isEmpty}
           .map{_.getName}
           .filter{_.endsWith(pattern)}
           .foreach{println(_)}

/* val xs = List(1, 2, 3)
println(scalar(xs, xs) == scalarUgly(xs, xs))
println((2 to 100) filter isPrime)
println(primePairs(10))
printNonEmpty(".scala") */
