package example
import better.files.Resource

object Hello extends App {
  def parseData(input: String): (List[Int], List[Board]) ={
    val lines = input.split("\n")
    val nums = lines.head.split(",").map(_.toInt).toList
    val boards = lines.tail.filter(_.nonEmpty).grouped(5).map {
      group => Board(group.toList.map(_.trim.split(" +").map(s => (s.toInt, false)).toList))
    }.toList
    (nums, boards)
  }
  def play(nums: List[Int], boards: List[Board], lastWinner: Option[Int]): Option[Int] = {
    (nums, boards) match {
      case (Nil, _) | (_, Nil) => lastWinner
      case (x :: xs, _) => 
        val marked = boards.map(_.mark(x))
        val (won, notWonYet) = marked.partition(_.hasWon)
        val currentWinner = won.map(_.score * x).maxOption.orElse(lastWinner)
        play(xs, notWonYet, currentWinner)
    }
  }
  val (nums, boards) = parseData(Resource.getAsString("input.txt"))
  println(play(nums, boards, None))
}

final case class Board (value: List[List[(Int, Boolean)]]) {
  def hasWon: Boolean = {
    value.exists(_.forall(_._2)) || value.transpose.exists(_.forall(_._2))
  }
  def score: Int ={
    value.flatten.collect {
      case (num, false) => num
    }.sum
  }
  def mark(num: Int): Board = {
    Board(value.map(_.map{
      case (`num`, false) => (num, true)
      case other => other
    }))
  }
  override def toString(): String =
    value.map(_.mkString(" ")).mkString("\n")
}
  
