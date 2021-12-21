package example

import better.files.Resource

object Hello extends App {
  def parseData(str: String): (EnhancementString, Grid) = {
    val lines = str.split("\n").toVector

    val enhancementString = EnhancementString(lines.head.map(_ == '#').toVector)
    val gridData = lines.drop(2).map(_.toVector.map(_  == '#') )
    val nRows = gridData.size
    val nCols = gridData.head.size
    val grid = Grid(gridData, nRows, nCols, false)
    (enhancementString, grid)
  }

  val (enhancementString, grid) = parseData(Resource.getAsString("input.txt"))
  println(grid.enhanceMany(50, enhancementString).countPixel)
}

final case class Grid(rows: Vector[Vector[Boolean]], nRows: Int, nCols: Int, background: Boolean) { self =>
  def window(row: Int, col: Int): Int = {
    val values = List(
      get(row-1, col-1), get(row-1, col), get(row-1, col+1), 
      get(row, col-1), get(row, col), get(row, col+1), 
      get(row+1, col-1), get(row+1, col), get(row+1, col+1)
    )
    val bitString = values.map(if (_) 1 else 0).mkString
    Integer.parseInt(bitString, 2)
  }
  
  def get(row: Int, col: Int): Boolean = 
    if (row >= 0 && row < nRows && col >= 0 && col < nCols) rows(row)(col) else background
  
  def computeAll(f: (Int, Int) => Boolean): Grid = {
    Grid(
      rows.zipWithIndex.map { case (cols, row) => cols.indices.map(col => f(row, col)).toVector },
      nRows,
      nCols,
      background
    )
  }

  def setBackground(value: Boolean): Grid =
    copy(background = value)

  def applyEnhancement(enhancement: EnhancementString): Grid = {
    Grid.make(nRows+2, nCols+2) { (row, col) =>
      enhancement.values(self.window(row - 1, col - 1))
    }.setBackground(if (self.background) enhancement.values(511) else enhancement.values(0))
  }

  def countPixel: Option[Int] =
    if (background) None else Some(rows.flatten.count(identity))

  def enhanceMany(times: Int, enhancement: EnhancementString): Grid = {
    if (times > 0) 
      self.applyEnhancement(enhancement).enhanceMany(times-1, enhancement)
    else
      self
  }
   
  override def toString(): String =
    rows.map(_.map(if (_) "#" else ".").mkString).mkString("\n")
}

object Grid{
  def empty(rows: Int, cols: Int): Grid = Grid(Vector.fill(rows, cols)(false), rows, cols, false)
  def make(rows: Int, cols: Int)(computeValue: (Int, Int) => Boolean): Grid =
    empty(rows, cols).computeAll(computeValue(_, _))
}

final case class EnhancementString(values: Vector[Boolean])
