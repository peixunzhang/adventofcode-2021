package example

import better.files.Resource

object Hello extends App {
  def parseData(input: String): Board = {
    val entries = input
      .split("\n")
      .map { str =>
        str.split(",").toList match {
          case rs :: cs :: Nil => (rs.toInt, cs.toInt)
          case _               => throw new IllegalStateException()
        }
      }
      .toSet
    val initial = Board.empty(1311, 895)
    initial.updateAll((r, c) => entries.contains((r, c)))
  }

  val data = parseData(Resource.getAsString("input.txt"))

  val part1 = data.foldX.countMark

  val part2 =
    data.foldX.foldY.foldX.foldY.foldX.foldY.foldX.foldY.foldX.foldY.foldY.foldY

  println(s"part1: $part1")
  println(s"part2:\n$part2")
}

final case class Board(value: Vector[Vector[Boolean]], rows: Int, cols: Int) {

  def getValue(row: Int, col: Int): Boolean = {
    value(row)(col)
  }

  def foldX: Board = {
    println(s"dims: ($rows, $cols)")
    println(s"foldX: ${rows / 2}")
    Board
      .empty(rows / 2, cols)
      .updateAll((r, c) => getValue(r, c) || getValue((rows - 1) - r, c))
  }

  def foldY: Board = {
    println(s"dims: ($rows, $cols)")
    println(s"foldY: ${cols / 2}")
    Board
      .empty(rows, cols / 2)
      .updateAll((r, c) => getValue(r, c) || getValue(r, (cols - 1) - c))
  }

  def updateAll(f: (Int, Int) => Boolean): Board = {
    Board(
      value.zipWithIndex.map { case (row, rowindex) =>
        row.zipWithIndex.map { case (_, colindex) =>
          f(rowindex, colindex)
        }
      },
      rows,
      cols
    )
  }

  def countMark: Int = {
    value.flatten.count(identity)
  }

  override def toString =
    value
      .map(_.map(if (_) 'x' else '.').reverse.mkString(""))
      .mkString("\n")
}

object Board {
  def empty(rows: Int, cols: Int): Board =
    Board(Vector.fill(rows, cols)(false), rows, cols)
}

// get, takes a row, col and turn boolean
// update row, col, value = board
// object board empty board x * y to conculate the size of board
// make a new board conculate new size create new board then update location
//   0 1 * 3 4 5 6 7 8 9
//
//         1 0            ((fold - 1) - index)
//         3 4 5 6 7 8 9  ((fold + 1) + index)
//         0 1 2 3 4 5 6
//
//   0 1 2 3 * 5 6 7 8
//
//   0 1 2 3 4 5 6        (index)
//             9 8        (2 * fold - index)
//   0 1 2 3 4 5 6
//

//  0 a b * d e f g h i j
// row 1 col 10 foldCon 2
