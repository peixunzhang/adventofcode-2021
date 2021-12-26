package example
import better.files.Resource

object Hello extends App {
  def median(crabs: Vector[Long]): Long = {
    val sorted = crabs.sorted
    if (sorted.length % 2 != 0) sorted((sorted.length+1)/2)
    else (sorted(sorted.length/2) + sorted(sorted.length/2+1))/2
  }

  def parseData(input: String): Vector[Long] ={
    input.split("\n").head.split(",").toVector.map(_.toLong)
  }

  def exhaustiveSearch(start: Long, end: Long, cost: Long => Long): (Long, Long) =
    (start to end).map(i => (i, cost(i))).minBy(_._2)

  def distance(a: Long, b: Long): Long = {
    Math.abs(b - a)
  }

  def fancyDistance(a: Long, b: Long): Long = {
    def sumDown(n: Long, acc: Long = 0): Long =
      if (n <= 0) acc else sumDown(n - 1, acc + n)
    sumDown(Math.abs(a - b))
  }

  def fuel(target: Long, crabs: Vector[Long], distance: (Long, Long) => Long): Long = {
    crabs.foldLeft(0L)((sum, c) => distance(target, c) + sum)
  }

  val crabs = parseData(Resource.getAsString("input.txt"))
  println(s"part1: ${exhaustiveSearch(crabs.min, crabs.max, fuel(_, crabs, distance))}")
  println(s"part2: ${exhaustiveSearch(crabs.min, crabs.max, fuel(_, crabs, fancyDistance))}")
}
