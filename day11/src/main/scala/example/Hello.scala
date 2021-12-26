package example

import better.files.Resource

object Hello extends App {
  def play(grid: Grid, step: Int, acc: Int = 0): Int = {
    if (step <= 0) acc
    else {
      val touched = grid.touchAll
      play(touched.resetFlashes, step - 1, acc + touched.countFlashed)
    }
  }

  def play2(grid: Grid, acc: Int = 0): Int = {
    if (!grid.allFlashed) play2(grid.resetFlashes.touchAll, acc + 1) else acc
  }

  def parseData(input: String): Grid = {
    Grid(
      input
        .split("\n")
        .toVector
        .map(_.toVector.map(x => Octopus(x.asDigit, false)))
    )
  }

  val data = parseData(Resource.getAsString("input.txt"))

  println(s"part1: ${play(data, 100)}")
  println(s"part2: ${play2(data)}")
}

final case class Octopus(value: Int, flashed: Boolean)

object Octopus {
  val freshlyFlashed = Octopus(0, true)
}

final case class OctopusWithPosition(row: Int, col: Int, octopus: Octopus)

final case class Grid(values: Vector[Vector[Octopus]]) { self =>
  def resetFlashes: Grid = Grid(values.map(_.map(_.copy(flashed = false))))

  def get(row: Int, col: Int): Option[Octopus] = {
    if (values.isDefinedAt(row) && values(row).isDefinedAt(col))
      Some(values(row)(col))
    else None
  }

  def getWithPosition(row: Int, col: Int): Option[OctopusWithPosition] = {
    get(row, col).map(OctopusWithPosition(row, col, _))
  }

  def neighbours(row: Int, col: Int): List[OctopusWithPosition] = {
    val upRight = getWithPosition(row - 1, col - 1)
    val up = getWithPosition(row - 1, col)
    val upLeft = getWithPosition(row - 1, col + 1)
    val right = getWithPosition(row, col + 1)
    val left = getWithPosition(row, col - 1)
    val downRight = getWithPosition(row + 1, col + 1)
    val down = getWithPosition(row + 1, col)
    val downLeft = getWithPosition(row + 1, col - 1)
    List(upRight, up, upLeft, right, left, downRight, down, downLeft).flatten
  }

  def touchOctopus(row: Int, col: Int): Grid = {
    get(row, col) match {
      case Some(Octopus(_, true)) => self
      case Some(Octopus(9, false)) =>
        neighbours(row, col)
          .filterNot(_.octopus.flashed)
          .foldLeft(updated(row, col, Octopus.freshlyFlashed)) {
            case (acc, oct) =>
              acc.touchOctopus(oct.row, oct.col)
          }
      case Some(oct @ Octopus(n, false)) =>
        updated(row, col, oct.copy(value = n + 1))
      case None => self
    }
  }

  def touchAll: Grid =
    values.zipWithIndex.foldLeft(self) { case (acc, (cols, row)) =>
      cols.indices.foldLeft(acc) { case (acc, col) =>
        acc.touchOctopus(row, col)
      }
    }

  def countFlashed: Int = values.flatten.count(_.flashed)

  def updated(row: Int, col: Int, value: Octopus): Grid = {
    Grid(values.updated(row, values(row).updated(col, value)))
  }
  def allFlashed: Boolean = {
    values.forall(_.forall(_.flashed))
  }
}
