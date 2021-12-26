package example2

import better.files.Resource

object Hello extends App {
  val startPolymer = "PSVVKKCNBPNBBHNSFKBO"
  def parseData(input: String): Map[String, Char] = {
    input.split("\n").toList.map(x => x.split(" -> ").toList match {
      case x :: y :: Nil => (x, y.head)
      case _ => throw new IllegalStateException()
    }).toMap
  }
  val rules = parseData(Resource.getAsString("input.txt"))
  val result = Polymer.fromString(startPolymer, rules).manyStep(40).charCount.values
  println(Polymer.fromString(startPolymer, rules).manyStep(40))
  println(result.max - result.min)
}

final case class Polymer(polymer: Map[String, Long], duplicate: Map[Char, Long], rules: Map[String, Char]) { self =>
  private[this] implicit class LongValueMapSyntax[K](value: Map[K, Long]) {
    def addToKey(k: K, v: Long): Map[K, Long] =
      value + (k -> (value.getOrElse(k, 0L) + v))
  }

  def step: Polymer = {
    val (np, nd) = polymer.toList.foldLeft[(Map[String, Long], Map[Char, Long])]((Map.empty, duplicate)){ case ((np, nd), (pair, count)) =>
      rules.get(pair).fold((np.addToKey(pair, count), nd)) { c =>
        val nnd = nd.addToKey(c, count)
        val newPairs = pair.patch(1, c.toString(), 0).sliding(2)
        val nnp = newPairs.foldLeft(np)((acc, pair) => acc.addToKey(pair, count))
        (nnp, nnd)
      }
    }
    Polymer(np, nd, rules)
  }

  def charCount: Map[Char, Long] = {
    polymer
      .toList.flatMap { case (str, i) => str.toList.map((_, i)) }
      .groupBy(_._1).view.map { case (c, v) => (c, v.map(_._2).sum - duplicate.getOrElse(c, 0L)) }
      .toMap
  }

  def manyStep(n: Int): Polymer = {
    if (n > 0) step.manyStep(n-1) else self
  }

}
object Polymer {
  def fromString(input: String, rules: Map[String, Char]): Polymer = {
    val pairs = input.sliding(2).toList
    val polymers = pairs.groupBy(identity).view.mapValues(_.size.toLong).toMap
    val duplicates = pairs.tail.map(_.head).groupBy(identity).view.mapValues(_.size.toLong).toMap
    Polymer(polymers, duplicates, rules)
  }
}
