package example

import better.files.Resource
import cats.Eval
import cats.syntax.traverse._
import cats.syntax.foldable._
import cats.instances.list._
import scala.collection.mutable

object Hello extends App {
  def parseData(input: String): Vector[Vector[Int]] = {
    input.split("\n").toVector.map(_.toVector.map(_.asDigit))
  }

  val data = parseData(Resource.getAsString("input.txt"))

  val smallCaves = Caves(data, data.size, data.head.size)

  val bigCaves = {
    def wrap(value: Int) = ((value - 1) % 9) + 1

    val fullWidth = data.map(Vector.fill(5)(_).zipWithIndex.flatMap { case (original, i) => original.map(x => wrap(x + i)) } )
    val full = Vector.fill(5)(fullWidth).zipWithIndex.flatMap { case (board, i) => board.map(_.map(x => wrap(x + i)))  }
    Caves(full, full.size, full.head.size)
  }

  println(s"part1: ${smallCaves.cheapestPath(0, 0, smallCaves.rows - 1, smallCaves.cols - 1)}")
  println(s"part2: ${bigCaves.cheapestPath(0, 0, bigCaves.rows - 1, bigCaves.cols - 1)}")

  // def cheapestPathHelperOriginal(
  //   start: Field,
  //   board: Caves,
  //   went: Set[Field] = Set.empty,
  //   acc: Int = 0,
  //   cheapestSoFar: Option[Int] = None
  // ): Eval[Option[Int]] = {
  //   val lowerBoundRemainingCost = board.rows - 1 - start.row + board.cols - 1 - start.col
  //   if (cheapestSoFar.fold(false)(_ <= acc + lowerBoundRemainingCost))
  //     Eval.now(cheapestSoFar)
  //   else if (start.row == board.rows - 1 && start.col == board.cols - 1) {
  //     println(acc)
  //     Eval.now(Some(acc))
  //   }
  //   else
  //     Eval.defer {
  //       board
  //         .neighnour(start.row, start.col)
  //         .filterNot(went)
  //         .foldLeftM(cheapestSoFar) { (cheapestSoFar, next) =>
  //           cheapestPathHelperOriginal(next, board, went + start, acc + next.value, cheapestSoFar)
  //             .map(_.orElse(cheapestSoFar))
  //         }
  //     }
  // }

  // def cheapestPathHelper(
  //   start: Field,
  //   board: Caves,
  //   went: Set[Field] = Set.empty,
  //   acc: Int = 0,
  //   cheapestSoFar: Option[Int] = None,
  //   costs: Map[Field, Int] = Map.empty
  // ): Eval[(Option[Int], Map[Field, Int])] = {
  //   costs.get(start).map(cost => Eval.now((Some(cost + acc), costs))) getOrElse {
  //     val lowerBoundRemainingCost = board.rows - 1 - start.row + board.cols - 1 - start.col
  //     if (cheapestSoFar.fold(false)(_ <= acc + lowerBoundRemainingCost))
  //       Eval.now((None, costs))
  //     else if (start.row == board.rows - 1 && start.col == board.cols - 1) {
  //       Eval.now((Some(acc), costs))
  //     }
  //     else
  //       Eval.defer {
  //         board
  //           .neighnour(start.row, start.col)
  //           .filterNot(went)
  //           .foldLeftM((cheapestSoFar, costs)) { case ((cheapestSoFar, costs), next) =>
  //             cheapestPathHelper(next, board, went + start, acc + next.value, cheapestSoFar, costs)
  //               .map { case (cheaperCost, costs) =>
  //                 (cheaperCost.orElse(cheapestSoFar), cheaperCost.fold(costs)(c => costs + (next -> (c - acc))))
  //               }
  //           }
  //       }
  //     }
  // }
}

final case class Caves(board: Vector[Vector[Int]], rows: Int, cols: Int) {

  def cheapestPath(
    fromRow: Int, fromCol: Int,
    toRow: Int, toCol: Int
  ): Option[Int] = {
    final case class Vertex(row: Int, col: Int, distance: Int)

    object Vertex {
      implicit val ordering: Ordering[Vertex] =
        Ordering.by(_.distance)
    }

    val vertices = mutable.PriorityQueue.empty[Vertex].reverse
    val visited = mutable.Set.empty[(Int, Int)]

    vertices.enqueue(Vertex(fromRow, fromCol, 0))

    var result = Option.empty[Int]

    while (result.isEmpty && !vertices.isEmpty) {
      val next = vertices.dequeue()
      if (next.row == toRow && next.col == toCol)
        result = Some(next.distance)
      else if (visited.add((next.row, next.col))) {
        neighnour(next.row, next.col)
          .filterNot(x => visited.contains((x.row, x.col)))
          .foreach { neighbour =>
            val calculatedDistance = next.distance + neighbour.value
            vertices.enqueue(Vertex(neighbour.row, neighbour.col, calculatedDistance))
          }
      }
    }
    result
  }

  def neighnour(row: Int, col: Int): List[Field] = {
    val up = get(row-1, col)
    val down = get(row+1, col)
    val right = get(row, col+1)
    val left = get(row, col-1)
    List(down, right, up, left).flatten
  }
  def get(row: Int, col: Int): Option[Field] = {
    if (board.isDefinedAt(row) && board(row).isDefinedAt(col)) Some(Field(row, col , board(row)(col)))
    else None
  }
}

final case class Field(row: Int, col: Int, value: Int)
