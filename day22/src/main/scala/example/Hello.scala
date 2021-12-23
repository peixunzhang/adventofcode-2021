package example
import scala.util.parsing.combinator._
import better.files.Resource

object InputParser extends RegexParsers with JavaTokenParsers {
  // on x=-44..7,y=-48..-4,z=-28..22
  def line: Parser[(Boolean, Cube)] = {
    boolean ~ cube ^^ { case b ~ c =>
      (b, c)
    }
  }

  def on: Parser[Boolean] = {
    literal("on") ^^ { _ => true }
  }
  def off: Parser[Boolean] = {
    literal("off") ^^ { _ => false }
  }
  def boolean: Parser[Boolean] = {
    on | off
  }

  // x=-44..7,y=-48..-4,z=-28..22
  def cube: Parser[Cube] = {
    literal("x=") ~ num ~ literal("..") ~ num ~ literal(",y=") ~ num ~ literal(
      ".."
    ) ~ num ~ literal(",z=") ~ num ~ literal("..") ~ num ^^ {
      case _ ~ xs ~ _ ~ xe ~ _ ~ ys ~ _ ~ ye ~ _ ~ zs ~ _ ~ ze =>
        Cube(xs, xe, ys, ye, zs, ze)
    }
  }

  def num: Parser[Int] = wholeNumber ^^ { str => str.toInt }
}
object Hello extends App {
  def parseData(input: String): List[(Boolean, Cube)] = {
    input
      .split("\n")
      .toList
      .map(str => InputParser.parse(InputParser.line, str).get)
  }
  val data = parseData(Resource.getAsString("input.txt"))
  val result = data.foldLeft(FancyGrid.empty) {
    case (grid, (false, cube)) => grid.turnOff(cube)
    case (grid, (true, cube))  => grid.turnOn(cube)
  }
  println(result.countCube)
}

object Grid {
  val start = Grid(Set.empty)
}

final case class Grid(value: Set[(Int, Int, Int)]) {
  def get(x: Int, y: Int, z: Int): Boolean = value.contains((x, y, z))
  def turnOn(c: Cube): Grid = {
    val shouldOn = for {
      x <- c.xStart to c.xEnd
      y <- c.yStart to c.yEnd
      z <- c.zStart to c.zEnd
    } yield (x, y, z)
    Grid(value ++ shouldOn)
  }
  def turnOff(c: Cube): Grid = {
    val shouldOff = for {
      x <- c.xStart to c.xEnd
      y <- c.yStart to c.yEnd
      z <- c.zStart to c.zEnd
    } yield (x, y, z)
    Grid(value -- shouldOff)
  }

  def countCube: Int = {
    value.size
  }

}
final case class FancyGrid(deltas: List[Delta]) {
  def countCube: Long = deltas.foldLeft(0L) {
    case (acc, Add(cube))    => acc + cube.volume
    case (acc, Remove(cube)) => acc - cube.volume
  }
  def turnOn(cube: Cube): FancyGrid = {
    val compensations = deltas.flatMap {
      case Add(that)    => cube.overlap(that).map(Remove(_))
      case Remove(that) => cube.overlap(that).map(Add(_))
    }
    FancyGrid(deltas ++ List(Add(cube)) ++ compensations)
  }
  def turnOff(cube: Cube): FancyGrid = {
    val compensations = deltas.flatMap {
      case Add(that)    => cube.overlap(that).map(Remove(_))
      case Remove(that) => cube.overlap(that).map(Add(_))
    }
    FancyGrid(deltas ++ compensations)
  }
}
object FancyGrid {
  val empty: FancyGrid = FancyGrid(Nil)
}

final case class Cube(
    xStart: Int,
    xEnd: Int,
    yStart: Int,
    yEnd: Int,
    zStart: Int,
    zEnd: Int
) { self =>
  def volume: Long =
    (xEnd - xStart + 1).toLong * (yEnd - yStart + 1).toLong * (zEnd - zStart + 1).toLong

  def overlap(that: Cube): Option[Cube] =
    if (overlaps(that))
      Some(
        Cube(
          self.xStart max that.xStart,
          self.xEnd min that.xEnd,
          self.yStart max that.yStart,
          self.yEnd min that.yEnd,
          self.zStart max that.zStart,
          self.zEnd min that.zEnd
        )
      )
    else
      None

  def overlaps(that: Cube): Boolean =
    self.xEnd > that.xStart && self.xStart < that.xEnd &&
      self.yEnd > that.yStart && self.yStart < that.yEnd &&
      self.zEnd > that.zStart && self.zStart < that.zEnd
}

sealed trait Delta
final case class Add(cube: Cube) extends Delta
final case class Remove(cube: Cube) extends Delta
