package example

import scala.util.parsing.combinator._
import better.files.Resource

object FishParser extends RegexParsers {
  def fish: Parser[Fish] =
    fishLiteral | fishTuple
  def fishLiteral: Parser[Fish] =
    "\\d+".r ^^ { s => Literal(s.toInt) }
  def fishTuple: Parser[Fish] =
    literal("[") ~ fish ~ literal(",") ~ fish ~ literal("]") ^^ {
      case _ ~ left ~ _ ~ right ~ _ => FishTuple(left, right)
    }
}

object Hello extends App {
  def parseFish(str: String): Fish = {
    FishParser.parse(FishParser.fish, str).get
  }
  def parseData(str: String): List[Fish] = {
    str.split("\n").toList.map(parseFish(_))
  }
  val fish = parseData(Resource.getAsString("input.txt"))

  val combination = for {
    left <- fish
    right <- fish.filterNot(_ == left)
  } yield (left, right)

  val biggest = combination.map { case (left, right) =>
    left.add(right).magnitude
  }.maxOption
  println(s"part1: ${fish.reduceLeft(_.add(_)).magnitude}")
  println(s"part2: ${biggest}")
}

sealed trait Fish { self =>
  private def explodeHelper(depth: Int): ExplodeResult = self match {
    case ft @ FishTuple(Literal(left), Literal(right)) =>
      if (depth >= 4)
        JustExploded(Literal(0), left, right)
      else
        NotExploded(ft)
    case FishTuple(left, right) =>
      left.explodeHelper(depth + 1) match {
        case Exploded(value) => Exploded(FishTuple(value, right))
        case JustExploded(value, leftAdd, rightAdd) =>
          LeftExploded(FishTuple(value, right.addLeft(rightAdd)), leftAdd)
        case LeftExploded(value, add) =>
          LeftExploded(FishTuple(value, right), add)
        case RightExploded(value, add) =>
          Exploded(FishTuple(value, right.addLeft(add)))
        case NotExploded(leftValue) =>
          right.explodeHelper(depth + 1) match {
            case NotExploded(value) => NotExploded(FishTuple(leftValue, value))
            case Exploded(value)    => Exploded(FishTuple(leftValue, value))
            case JustExploded(value, leftAdd, rightAdd) =>
              RightExploded(
                FishTuple(leftValue.addRight(leftAdd), value),
                rightAdd
              )
            case LeftExploded(value, add) =>
              Exploded(FishTuple(leftValue.addRight(add), value))
            case RightExploded(value, add) =>
              RightExploded(FishTuple(leftValue, value), add)
          }
      }

    case lit @ Literal(_) => NotExploded(lit)
  }

  def explode: ExplodeResult = {
    explodeHelper(0)
  }

  def split: SplitResult = self match {
    case FishTuple(left, right) =>
      left.split match {
        case Split(value) => Split(FishTuple(value, right))
        case NotSplit(leftValue) =>
          right.split match {
            case Split(value)    => Split(FishTuple(leftValue, value))
            case NotSplit(value) => NotSplit(FishTuple(leftValue, value))
          }
      }
    case Literal(number) if number >= 10 =>
      val half = number.toDouble / 2
      Split(
        FishTuple(
          Literal(Math.floor(half).toInt),
          Literal(Math.ceil(half).toInt)
        )
      )
    case lit @ Literal(_) => NotSplit(lit)
  }

  private def addLeft(value: Int): Fish =
    if (value == 0) self
    else {
      self match {
        case FishTuple(left, right) => FishTuple(left.addLeft(value), right)
        case Literal(number)        => Literal(number + value)
      }
    }

  private def addRight(value: Int): Fish =
    if (value == 0) self
    else {
      self match {
        case FishTuple(left, right) => FishTuple(left, right.addRight(value))
        case Literal(number)        => Literal(number + value)
      }
    }

  def show: String = self match {
    case FishTuple(left, right) => s"[${left.show},${right.show}]"
    case Literal(number)        => number.toString()
  }

  override def toString(): String = show

  def reduce: Fish = {
    self.explode match {
      case NotExploded(value) =>
        value.split match {
          case Split(value)    => value.reduce
          case NotSplit(value) => value
        }
      case exploded => exploded.value.reduce
    }
  }

  def add(that: Fish): Fish = {
    FishTuple(self, that).reduce
  }
  def magnitude: Long = self match {
    case Literal(number)        => number
    case FishTuple(left, right) => left.magnitude * 3 + right.magnitude * 2
  }

}
final case class Literal(number: Int) extends Fish
final case class FishTuple(left: Fish, right: Fish) extends Fish

sealed trait ExplodeResult {
  def value: Fish
}

final case class NotExploded(value: Fish) extends ExplodeResult
final case class Exploded(value: Fish) extends ExplodeResult
final case class JustExploded(value: Fish, leftAdd: Int, rightAdd: Int)
    extends ExplodeResult
final case class LeftExploded(value: Fish, add: Int) extends ExplodeResult
final case class RightExploded(value: Fish, add: Int) extends ExplodeResult

sealed trait SplitResult {
  def value: Fish
}

final case class Split(value: Fish) extends SplitResult
final case class NotSplit(value: Fish) extends SplitResult
