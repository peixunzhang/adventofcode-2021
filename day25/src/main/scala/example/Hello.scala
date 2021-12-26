package example

import better.files.Resource

object Hello extends App {
  def parseCucumber(c: Char): Option[Cucumber] =
    c match {
      case '>' => Some(RightCucumber)
      case 'v' => Some(DownCucumber)
      case _   => None
    }
  def parseData(input: String): Field = {
    val rows = input.split("\n").map(_.toVector.map(parseCucumber(_))).toVector
    Field(rows, rows.size, rows.head.size)
  }
  println(parseData(Resource.getAsString("input.txt")).convergesAfter)
}

sealed trait Cucumber { self =>
  override def toString(): String =
    self match {
      case DownCucumber  => "v"
      case RightCucumber => ">"
    }
}
case object RightCucumber extends Cucumber
case object DownCucumber extends Cucumber

final case class Field(
    rows: Vector[Vector[Option[Cucumber]]],
    nRows: Int,
    nCols: Int
) { self =>
  def get(row: Int, col: Int): Option[Cucumber] =
    rows(Math.floorMod(row, nRows))(Math.floorMod(col, nCols))

  def moveAll: Field =
    moveRight.moveDown

  def moveRight: Field =
    Field.make(nRows, nCols) { case (row, col) =>
      get(row, col) match {
        case Some(RightCucumber) =>
          get(row, col + 1).map(_ => RightCucumber)
        case old @ Some(DownCucumber) => old
        case None =>
          get(row, col - 1) match {
            case Some(RightCucumber) => Some(RightCucumber)
            case _                   => None
          }
      }
    }

  def moveDown: Field =
    Field.make(nRows, nCols) { case (row, col) =>
      get(row, col) match {
        case Some(DownCucumber) =>
          get(row + 1, col).map(_ => DownCucumber)
        case old @ Some(RightCucumber) => old
        case None =>
          get(row - 1, col) match {
            case Some(DownCucumber) => Some(DownCucumber)
            case _                  => None
          }
      }
    }

  def convergesAfter: Int = {
    val next = moveAll
    if (self == next) 1
    else next.convergesAfter + 1
  }

  override def toString(): String =
    rows.map(_.map(_.fold(".")(_.toString())).mkString).mkString("\n")
}

object Field {
  def make(nRows: Int, nCols: Int)(f: (Int, Int) => Option[Cucumber]): Field =
    Field(Vector.tabulate(nRows, nCols)(f), nRows, nCols)
}
