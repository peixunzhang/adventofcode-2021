package example
import better.files.Resource

object Hello extends App {
  def parseData(input: String): List[Instruction] =
    input.split("\n").toList.map { str =>
      str.split(" ").toList match {
        case dirStr :: stepStr :: Nil =>
          Instruction(Direction.parse(dirStr), stepStr.toInt)
        case _ => throw new IllegalArgumentException()
      }
    }

  def move1(input: List[Instruction]): Int = {
    val (x, y) = input.foldLeft((0, 0)) {
      case ((x, y), instruction) =>
        instruction.direction match {
          case Direction.Forward =>
            (x + instruction.step, y)
          case Direction.Down => (x, y + instruction.step)
          case Direction.Up   => (x, y - instruction.step)
        }
    }
    x * y
  }

  def move2(input: List[Instruction]): Int = {
    val (x, y, _) = input.foldLeft((0, 0, 0)) {
      case ((x, y, aim), instruction) =>
        instruction.direction match {
          case Direction.Forward =>
            (x + instruction.step, y + (instruction.step * aim), aim)
          case Direction.Down => (x, y, aim + instruction.step)
          case Direction.Up   => (x, y, aim - instruction.step)
        }
    }
    x * y
  }
  val data = parseData(Resource.getAsString("input.txt"))
  println(s"part1: ${move1(data)}")
  println(s"part2: ${move2(data)}")
}

final case class Instruction(direction: Direction, step: Int)

sealed trait Direction
object Direction {
  case object Forward extends Direction
  case object Down extends Direction
  case object Up extends Direction
  def parse(str: String): Direction = str match {
    case "forward" => Forward
    case "down"    => Down
    case "up"      => Up
  }
}
