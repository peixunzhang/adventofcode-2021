package example

object Hello extends App {
  def launchAll(target: Target): List[Int] = {
    val vxMin = 0
    val vxMax = target.xRight
    val vyMin = target.yDown
    val vyMax = -target.yDown
    val probes = for {
      vx <- vxMin to vxMax
      vy <- vyMin to vyMax
    } yield Probe(0, 0, vx, vy)
    probes.flatMap(_.launch(target)).toList
  }

  println(launchAll(Target(96, 125, -144, -98)).size)
}

final case class Probe(x: Int, y: Int, vx: Int, vy: Int) { self =>
  def step: Probe = {
    val newVx = if (vx > 0) vx - 1 else if (vx < 0) vx + 1 else vx
    Probe(x + vx, y + vy, newVx, vy - 1)
  }
  def launch(target: Target, accY: Option[Int] = None): Option[Int] = {
    if (x > target.xRight || y < target.yDown) None
    else if (target.isHit(self)) accY.fold(Some(y))(old => Some(old.max(y)))
    else step.launch(target, accY.fold(Some(y))(old => Some(old.max(y))))
  }

}

final case class Target(xLeft: Int, xRight: Int, yDown: Int, yUp: Int) {
  def isHit(probe: Probe): Boolean =
    probe.x >= xLeft && probe.x <= xRight && probe.y >= yDown && probe.y <= yUp

}
