package example

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
  val result = Polymer(startPolymer, rules).manyStep(10).polymer.groupBy(identity).mapValues(_.length().toLong).values

  println(result.max- result.min)
}

final case class Polymer(polymer: String, rules: Map[String, Char]) { self =>
  def step: Polymer = {
    val inserted = polymer.sliding(2).foldLeft(polymer.head.toString)((acc, str)=> rules.get(str).fold(acc++str.tail)(y => acc + y ++ str.tail))
      Polymer(inserted, rules)
  }
  def manyStep(n: Int): Polymer = {
    if (n > 0) step.manyStep(n-1) else self
  }

}
