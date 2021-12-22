package example

object Hello extends App {
  def play(active: Player, passive: Player, dice: Dice): Int = {
    val (d, p) = active.makeTurn(dice)
    if (p.haswon) passive.score * d.total else play(passive, p, d)
  }
  println(play(Player(Space(8)), Player(Space(9)), Dice.initial))
}

final case class Player(space: Space, score: Int = 0) {
  def haswon: Boolean = score >= 1000
  def makeTurn(dice: Dice): (Dice, Player) = {
    val (rolled, step) = dice.rollN(3)
    val nextSpace = space.move(step)
    (rolled, Player(nextSpace, score + nextSpace.value))
  }
}

final case class Dice(value: Int, total: Int) { self =>
  def roll: (Dice, Int) = {
    if (value < 100) (Dice(value + 1, total + 1), value)
    else (Dice(1, total + 1), value)
  }
  def rollN(n: Int, accStep: Int = 0): (Dice, Int) = {
    if (n <= 0) (self, accStep)
    else {
      val (rolled, stepCount) = self.roll
      rolled.rollN(n - 1, accStep + stepCount)
    }
  }
}
object Dice {
  val initial = Dice(1, 0)
}

final case class Space(value: Int) {
  def move(step: Int): Space = Space((value + step - 1) % 10 + 1)
}
