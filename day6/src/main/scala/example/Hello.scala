package example

import better.files.Resource

object Hello extends App {
  def countFish(input: List[Fish]): Long ={
    def makeFishes(days: Int, fishes: Fishes): Fishes= {
      if (days <= 0) fishes else makeFishes(days-1, fishes.step)
    }
      makeFishes(256, Fishes.fromFish(input)).numberOfFishes
  }
  def parseData(input: String): List[Fish] = {
    input.split("\n").head.split(",").toList.map(str => Fish(str.toInt))
  }
  println(countFish(parseData(Resource.getAsString("input.txt"))))
}

final case class Fish(counter: Int) extends AnyVal {
  def step: List[Fish] = {
    if (counter <= 0) List(Fish(6), Fish.fresh) else List(Fish(counter-1))
  }
}
object Fish {
  val fresh: Fish = Fish(8)
}

final case class Fishes(values: Map[Fish, Long]){
  def step: Fishes = {
    val newFish: List[List[(Fish, Long)]] = values.toList.map { case (f, i) => f.step.map((_, i)) }
    val newValue = newFish.foldLeft(Map.empty[Fish, Long])((m, l) => l.foldLeft(m){ case (m, (f, i)) => m + (f -> (m.getOrElse(f, 0L) + i)) })
    Fishes(newValue)
  }
  def numberOfFishes: Long ={
    values.values.sum
  }
}

object Fishes{
  def fromFish(fishes: List[Fish]): Fishes ={
    Fishes(fishes.groupBy(identity).view.mapValues(_.length.toLong).toMap)
  }
}
