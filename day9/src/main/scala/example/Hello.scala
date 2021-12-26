package example

import better.files.Resource

object Hello extends App {
  def parseData(input: String): Map = {
    Map(input.split("\n").toVector.map(_.toVector.map(_.asDigit)))
  }
  val data = parseData(Resource.getAsString("input.txt"))
  println(s"part1: ${data.riskSum}")
  println(s"part2: ${data.threeLargestBasinsSize}")
}

final case class Map(map: Vector[Vector[Int]]) {
  def get(row: Int, col: Int): Option[Int] =
    if (map.isDefinedAt(row) && map(row).isDefinedAt(col)) Some(map(row)(col))
    else None

  def getLowPointOld(col: Int, row: Int): Option[Int] = {
    val target: Option[Int] = get(row, col)
    val up: Option[Int] = get(row - 1, col)
    val down: Option[Int] = get(row + 1, col)
    val right: Option[Int] = get(row, col + 1)
    val left: Option[Int] = get(row, col - 1)

    if (
      up.zip(target).fold(true) { case (u, t) => t < u } &&
      down.zip(target).fold(true) { case (d, t) => t < d } &&
      right.zip(target).fold(true) { case (r, t) => t < r } &&
      left.zip(target).fold(true) { case (l, t) => t < l }
    ) target
    else None
  }

  def getLowPoint(row: Int, col: Int): Option[LowPoint] = {
    get(row, col).flatMap { t =>
      if (getNeighbours(row, col).forall(x => x.value > t))
        Some(LowPoint(Point(t, row, col)))
      else None
    }
  }

  def getRisk(row: Int, col: Int): Int = {
    getLowPoint(row, col).fold(0)(_.value.value + 1)
  }

  def getNeighbours(row: Int, col: Int): List[Point] = {
    val up = get(row - 1, col).map(Point(_, row - 1, col))
    val down = get(row + 1, col).map(Point(_, row + 1, col))
    val right = get(row, col + 1).map(Point(_, row, col + 1))
    val left = get(row, col - 1).map(Point(_, row, col - 1))
    List(up, down, right, left).flatten
  }

  def getBasinSize(lowPoint: LowPoint): Int = {
    def go(neighbour: Point): Set[Point] = {
      Set(neighbour) ++ getNeighbours(neighbour.row, neighbour.col)
        .filter(x => x.value > neighbour.value && x.value != 9)
        .toSet
        .flatMap(go(_))
    }
    go(lowPoint.value).size
  }

  def threeLargestBasinsSize: Int = {
    val allBasins = map.zipWithIndex.flatMap { case (cs, r) =>
      cs.indices.flatMap { c => getLowPoint(r, c).map(getBasinSize) }
    }
    allBasins.sortBy(-_).take(3).product
  }

  def riskSum: Int = map.zipWithIndex.foldLeft(0) { case (n, (cs, r)) =>
    cs.indices.foldLeft(n)((n, c) => getRisk(r, c) + n)
  }
}

final case class Point(value: Int, row: Int, col: Int)
final case class LowPoint(value: Point)
