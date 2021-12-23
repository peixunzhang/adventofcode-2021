package example
import scala.util.parsing.combinator._
import better.files.Resource

object InputParser extends RegexParsers with JavaTokenParsers {
  def line: Parser[(Boolean, Cube)] = {
    //on x=-44..7,y=-48..-4,z=-28..22
    boolean ~ cube ^^ {
      case b ~ c => (b, c)
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
    literal("x=") ~ num ~ literal("..") ~ num ~ literal(",y=") ~ num ~ literal("..") ~ num ~ literal(",z=") ~ num ~ literal("..") ~ num ^^ {
      case _ ~ xs ~ _ ~ xe ~ _ ~ ys ~ _ ~ ye ~ _ ~ zs ~ _ ~ ze => Cube(xs, xe, ys, ye, zs, ze) 
    }
  }

  def num: Parser[Int] = wholeNumber ^^ { str => str.toInt }
}
object Hello extends App {
  def parseData(input: String): List[(Boolean, Cube)] = {
    input.split("\n").toList.map(str => InputParser.parse(InputParser.line, str).get)
  }
  val data = parseData(Resource.getAsString("input.txt"))
  val result = data.foldLeft(Grid.start){
    case (grid, (false, cube)) => grid.turnOff(cube)
    case (grid, (true, cube)) => grid.turnOn(cube)
  }
  println(result.countCube)
}


object Grid {
  val start = Grid(Set.empty)
}

final case class Grid(value: Set[(Int, Int, Int)]) {
  def get(x: Int, y: Int, z:Int): Boolean = value.contains((x ,y, z))
  def turnOn(c: Cube): Grid = {
    val shouldOn = for {
      x <- c.xStart to c.xEnd
      y <- c.yStart to c.yEnd
      z <- c.zStart to c.zEnd
    } yield (x, y, z)
    Grid(value++shouldOn)
  }
  def turnOff(c: Cube): Grid = {
    val shouldOff = for {
      x <- c.xStart to c.xEnd
      y <- c.yStart to c.yEnd
      z <- c.zStart to c.zEnd
    } yield (x, y, z)
    Grid(value--shouldOff)
  }
  
  def countCube: Int = {
    value.size
  }
  
}
final case class Cube(xStart: Int, xEnd: Int, yStart: Int, yEnd: Int, zStart: Int, zEnd: Int)
