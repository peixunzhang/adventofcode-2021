package example

import better.files.Resource

object Hello extends App {

  def processMeasurements(ar: List[Int]): Int =
    ar.foldLeft[(Int, Option[Int])]((0, None)) { case ((counts, pre), line) =>
      pre.fold((counts, Some(line))) { i =>
        if (line > i) (counts + 1, Some(line)) else (counts, Some(line))
      }
    }._1

  def sumOfWindows(ar: List[Int]): List[Int] = {
    ar.sliding(3).map(_.sum).toList
  }

  def parseData(input: String): List[Int] =
    input.split("\n").toList.map(_.toInt)

  val data = parseData(Resource.getAsString("input.txt"))
  println(s"part1: ${processMeasurements(data)}")
  println(s"part2: ${processMeasurements(sumOfWindows(data))}")
}
