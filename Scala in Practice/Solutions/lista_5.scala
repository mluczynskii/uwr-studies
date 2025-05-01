package com.mluczynski.plugins {
  trait Pluginable { 
    def plugin(s: String): String = s /* base plugin in the chain is just the id function */
  }
  trait Reverting extends Pluginable { 
    override def plugin(s: String): String = 
      super.plugin(s.reverse) /* super.plugin calls the next plugin in the chain */
  }
  trait LowerCasing extends Pluginable {
    override def plugin(s: String): String = {
      val ns = s.map(_.toLower)
      super.plugin(ns)
    }
  }
  trait SingleSpacing extends Pluginable {
    override def plugin(s: String): String = {
      val ns = s.split("\\s+").mkString(" ")
      super.plugin(ns)
    }
  }
  trait NoSpacing extends Pluginable {
    override def plugin(s: String): String = {
      val ns = s.filter(_ != ' ')
      super.plugin(ns)
    }
  }
  trait DuplicateRemoval extends Pluginable {
    override def plugin(s: String): String = {
      val counts = s.groupBy(Predef.identity[Char])
                    .view.mapValues(_.length)
                    .toMap.withDefaultValue(0)
      val ns = s.filter(counts(_) < 2)
      super.plugin(ns)
    }
  }
  trait Rotating extends Pluginable {
    override def plugin(s: String): String = {
      if (s.isEmpty) s 
      else super.plugin(s"${s.last}${s.init}")
    }
  }
  trait Doubling extends Pluginable {
    override def plugin(s: String): String = {
      val ns = s.zipWithIndex.map{case (c, idx) => 
        if (idx % 2 == 1) s"$c$c"
        else s"$c"
      }.mkString
      super.plugin(ns)
    }
  }
  trait Shortening extends Pluginable {
    override def plugin(s: String): String = {
      val ns = s.zipWithIndex.filter{case (_, idx) => idx % 2 == 0}
                .map(_._1).mkString
      super.plugin(ns)
    }
  }
}

object Actions {
  import com.mluczynski.plugins._
  val actionA: Pluginable = new Pluginable with Shortening with Doubling with SingleSpacing
  val actionB: Pluginable = new Pluginable with Doubling with Shortening with NoSpacing
  val actionC: Pluginable = new Pluginable with Doubling with LowerCasing
  val actionD: Pluginable = new Pluginable with Rotating with DuplicateRemoval
  val actionE: Pluginable = new Pluginable with Reverting with Doubling with Shortening with NoSpacing
  val actionF: Pluginable = new Pluginable {
    private final val rotations: Int = 5
    private def aux(s: String, n: Int): String = {
      if (n == rotations) s 
      else aux(new Rotating {}.plugin(s), n+1)
    } 
    override def plugin(s: String): String =
      super.plugin(aux(s, 0))
  }
  val actionG: Pluginable = new Pluginable {
    override def plugin(s: String): String =
      super.plugin(actionB.plugin(actionA.plugin(s)))
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    assert(Actions.actionA.plugin("ala ma  kota") == "al makot", "actionA doesn't work")
    assert(Actions.actionB.plugin("  ala ma  kota") == "aaaaooa", "actionB doesn't work")
    assert(Actions.actionC.plugin("AlA  MA kotA") == "alla   mma  kootaa", "actionC doesn't work")
    assert(Actions.actionD.plugin("ala ma kota") == "tlmko", "actionD doesn't work")
    assert(Actions.actionE.plugin("  ala ma  kota") == "aooaaaa", "actionE doesn't work")
    assert(Actions.actionF.plugin("abcdefgh") == "defghabc", "actionF doesn't work")
    assert(Actions.actionG.plugin("ala ma  kota") == "ammktt", "actionG doesn't work")
    println("All tests passed 🎇")
  }
}