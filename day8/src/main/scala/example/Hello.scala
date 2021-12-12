package example

import Hello.Digit
import better.files.Resource

object Hello extends App {
  type Digit = Set[Char]

  def parseData(input: String): List[(List[Digit], List[Digit])] ={
    input.split("\n").toList.map(str => str.split(" \\| ").toList match {
      case x :: y :: Nil => (x.split(" ").toList.map(_.toSet), y.split(" ").toList.map(_.toSet))
      case other => throw new IllegalStateException()
    })
  }
  def getNumber(parsed: List[(List[Digit], List[Digit])]): Int = {
    val decoded = parsed.map {
      case (input, output) => 
        val display = Display.fromDigits(input)
        display.decodeMany(output)
    }
    decoded.sum
  }
 println(getNumber(parseData(Resource.getAsString("input.txt"))))
}

final case class Display(value: Map[Digit, Int]) {
  def decode(digit: Digit): Option[Int] = {
    value.get(digit)
  }
  def decodeMany(digits: List[Digit]): Int = {
    digits.reverse.zipWithIndex.map { case (d, i) =>
      Math.pow(10, i).toInt * value(d)
    }.sum
  }
}
object Display{
  def fromDigits(c: List[Digit]): Display = {
    val code1 = c.find(_.size == 2).head
    val code4 = c.find(_.size == 4).head
    val code7 = c.find(_.size == 3).head
    val code8 = c.find(_.size == 7).head
    val segmentA = (code7 -- code1).head
    val code9 = c.find(x => code4.forall(x.contains) && x.contains(segmentA) && (x -- code4 - segmentA).size == 1).head
    val segmentG = (code9 -- code4 - segmentA).head
    val segmentE = (code8 -- code9).head
    val code0 = c.find(x => x != code9 && (code8 -- x).size == 1 && (x & code1).size == 2).head
    val code6 = c.find(x => x != code9 && x != code0 && (code8 -- x).size == 1).head
    val code5 = code6 - segmentE
    val segmentB = (code0 -- code7 - segmentE - segmentG).head
    val segmentD = (code8 -- code0).head
    val code3 = code7 + segmentG + segmentD
    val code2 = c.find(x => x != code3 && x != code5 && x.size == 5).head
    Display(Map(
      code0 -> 0,
      code1 -> 1,
      code2 -> 2,
      code3 -> 3,
      code4 -> 4,
      code5 -> 5,
      code6 -> 6,
      code7 -> 7,
      code8 -> 8,
      code9 -> 9
    ))
  }
}
