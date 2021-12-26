package example
import better.files.Resource

object Hello extends App {
  def makeCount(input: List[List[Int]]): RowCounts = {
    input.foldLeft(RowCounts.empty)(_.addRow(_))
  }
  def parseData(input: String): List[List[Int]] = {
    input.split("\n").toList.map{
      row => row.toList.map(_.asDigit)
    }
  }

  def getNum(input: List[List[Int]], col: Int, f: Count => Int): Int = {
    input match {
      case List(result) => Integer.parseInt(result.mkString, 2)
      case Nil => throw new IllegalStateException()
      case _ =>
        val rowCount = makeCount(input)
        val num = f(rowCount.values(col))
        val newInput = input.filter(_(col) == num)
        getNum(newInput, col+1, f)
    }
  }
  val data = parseData(Resource.getAsString("input.txt"))

  val part1 = makeCount(data).finalNum
  val part2 = getNum(data, 0, _.mostCount) * getNum(data, 0, _.leastCount)

  println(s"part1: $part1")
  println(s"part2: $part2")
}

final case class Count (ones: Int, zeros: Int) { self =>
  def ++(other: Count): Count = {
    Count(ones+other.ones, zeros+other.zeros)
  }

  def add(num: Int): Count = num match {
      case 1 => Count(ones+1, zeros)
      case 0 => Count(ones, zeros+1)
      case _ => self
  }

  def mostCount: Int = {
    if (ones >= zeros) 1 else 0
  }

  def leastCount: Int = {
    if (ones >= zeros) 0 else 1
  }
}
object Count {
  val empty: Count = Count(0, 0)
}

final case class RowCounts(values: List[Count]) {
  def addRow(other: List[Int]): RowCounts =
    RowCounts {
      values.zipAll(other, Count.empty, 2).map{
        case (count, num) => count.add(num)
      }
    }
  def finalNum: Int = {
    val g = Integer.parseInt(values.map(_.mostCount).mkString, 2)
    val e = Integer.parseInt(values.map(_.leastCount).mkString, 2)
    g*e
  }
  }

  object RowCounts{
    val empty: RowCounts = RowCounts(Nil)
  }

