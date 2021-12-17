package example

import better.files.Resource
import cats.Eval
import cats.syntax.traverse._
import cats.syntax.foldable._
import cats.instances.list._

object Hello extends App {
  def parseData(input: String): Caves = {
    val board = input.split("\n").toVector.map(_.toVector.map(_.asDigit))
    Caves(board, board.size, board.head.size)
  }
  def cheapestPath(start: Field, board: Caves): Option[Int] = {
    cheapestPathHelperOriginal(start, board).value
  }

  def cheapestPathHelperOriginal(
    start: Field,
    board: Caves,
    went: Set[Field] = Set.empty,
    acc: Int = 0,
    cheapestSoFar: Option[Int] = None
  ): Eval[Option[Int]] = {
    val lowerBoundRemainingCost = board.rows - 1 - start.row + board.cols - 1 - start.col
    if (cheapestSoFar.fold(false)(_ <= acc + lowerBoundRemainingCost)) 
      Eval.now(cheapestSoFar)
    else if (start.row == board.rows - 1 && start.col == board.cols - 1) {
      println(acc)
      Eval.now(Some(acc))
    }
    else 
      Eval.defer {
        board
          .neighnour(start.row, start.col)
          .filterNot(went)
          .foldLeftM(cheapestSoFar) { (cheapestSoFar, next) =>
            cheapestPathHelperOriginal(next, board, went + start, acc + next.value, cheapestSoFar)
              .map(_.orElse(cheapestSoFar))
          }
      }
  }

  def cheapestPathHelper(
    start: Field,
    board: Caves,
    went: Set[Field] = Set.empty,
    acc: Int = 0,
    cheapestSoFar: Option[Int] = None,
    costs: Map[Field, Int] = Map.empty
  ): Eval[(Option[Int], Map[Field, Int])] = {
    costs.get(start).map(cost => Eval.now((Some(cost + acc), costs))) getOrElse {
      val lowerBoundRemainingCost = board.rows - 1 - start.row + board.cols - 1 - start.col
      if (cheapestSoFar.fold(false)(_ <= acc + lowerBoundRemainingCost)) 
        Eval.now((None, costs))
      else if (start.row == board.rows - 1 && start.col == board.cols - 1) {
        Eval.now((Some(acc), costs))
      }
      else 
        Eval.defer {
          board
            .neighnour(start.row, start.col)
            .filterNot(went)
            .foldLeftM((cheapestSoFar, costs)) { case ((cheapestSoFar, costs), next) =>
              cheapestPathHelper(next, board, went + start, acc + next.value, cheapestSoFar, costs)
                .map { case (cheaperCost, costs) =>
                  (cheaperCost.orElse(cheapestSoFar), cheaperCost.fold(costs)(c => costs + (next -> (c - acc))))  
                }
            }
        }
      }
  }

  val board = parseData(Resource.getAsString("input.txt"))
  println(cheapestPath(board.get(0, 0).get, board))
}

final case class Caves(board: Vector[Vector[Int]], rows: Int, cols: Int) {

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
