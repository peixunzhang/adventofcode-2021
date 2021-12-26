package example

object Hello extends App {
  def play(
      active: Player,
      passive: Player,
      dice: DetDice = DetDice.initial
  ): Int = {
    val (d, p) = active.makeTurn(dice)
    if (p.score >= 1000) passive.score * d.total else play(passive, p, d)
  }

  def playPart2(active: Player, passive: Player): Outcomes = {
    active.allTurns
      .map { case (p, times) =>
        if (p.score >= 21) Outcomes(times, 0)
        else playPart2(passive, p).invert * times
      }
      .reduce(_ + _)
  }

  val p1 = Player(Space(8))
  val p2 = Player(Space(9))

  println(s"part1: ${play(p1, p2)}")
  println(s"part2: ${playPart2(p1, p2)}")
}

final case class Outcomes(wins: Long, loses: Long) {
  def +(that: Outcomes): Outcomes = {
    Outcomes(this.wins + that.wins, this.loses + that.loses)
  }
  def *(time: Int): Outcomes = {
    Outcomes(wins * time, loses * time)
  }
  def invert: Outcomes = {
    Outcomes(loses, wins)
  }
}

final case class Player(space: Space, score: Int = 0) {

  def makeTurn(dice: DetDice): (DetDice, Player) = {
    val (rolled, step) = dice.rollN(3)
    val nextSpace = space.move(step)
    (rolled, Player(nextSpace, score + nextSpace.value))
  }

  def allTurns: List[(Player, Int)] = {
    val rolls = Dice.roll3
    rolls.map { case (step, times) =>
      val nextSpace = space.move(step)
      (Player(nextSpace, nextSpace.value + score), times)
    }
  }
}

final case class Space(value: Int) {
  def move(step: Int): Space = Space((value + step - 1) % 10 + 1)
}

final case class DetDice(value: Int, total: Int) { self =>
  def roll: (DetDice, Int) = {
    if (value < 100) (DetDice(value + 1, total + 1), value)
    else (DetDice(1, total + 1), value)
  }
  def rollN(n: Int, accStep: Int = 0): (DetDice, Int) = {
    if (n <= 0) (self, accStep)
    else {
      val (rolled, stepCount) = self.roll
      rolled.rollN(n - 1, accStep + stepCount)
    }
  }
}
object DetDice {
  val initial = DetDice(1, 0)
}

object Dice { self =>
  def roll: List[Int] = {
    List(1, 2, 3)
  }
  def rollN(n: Int, acc: Int = 0): List[Int] = {
    if (n <= 0) List(acc)
    else
      self.roll.flatMap { i => rollN(n - 1, acc + i) }
  }
  val roll3: List[(Int, Int)] =
    rollN(3).groupBy(identity).view.mapValues(_.size).toList
}
