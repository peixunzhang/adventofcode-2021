package example
import better.files.Resource

object Hello extends App {
  def makeSpace(vents: List[Vent]): Space = {
    val dimX = (vents.map(_.startX) ++ vents.map(_.endX)).max+1
    val dimY = (vents.map(_.startY) ++ vents.map(_.endY)).max+1
    val space = Space.make(dimX, dimY)
    vents.foldLeft(space)(_.addVent(_))
  }
  def parseData(input: String): List[Vent] = {
    val lines = input.split("\n")
    lines
      .map(_.split(" -> |,").toList match {
        case startX :: startY :: endX :: endY :: Nil =>
          Vent(startX.toInt, startY.toInt, endX.toInt, endY.toInt)
        case _ => throw new IllegalStateException()
      })
      .toList
  }

  println(makeSpace(parseData(Resource.getAsString("input.txt"))).result)
}

final case class Space(locations: Vector[Vector[Int]]) { self =>
  def addVent(vent: Vent): Space =
    if (vent.startX == vent.endX) {
      val x = vent.startX
      val loc = (vent.startY.min(vent.endY) to vent.startY.max(vent.endY)).foldLeft(locations)((l, y) =>
        l.updated(x, l(x).updated(y, l(x)(y) + 1))
      )
      Space(loc)
    } else if (vent.startY == vent.endY) {
      val y = vent.startY
      val loc = (vent.startX.min(vent.endX) to vent.startX.max(vent.endX)).foldLeft(locations)((l, x) =>
        l.updated(x, l(x).updated(y, l(x)(y) + 1))
      )
      Space(loc)
    } else if (Math.abs(vent.endX - vent.startX) == Math.abs(vent.endY - vent.startY)) {
      val xRange = if (vent.startX < vent.endX) (vent.startX to vent.endX) else (vent.startX to vent.endX by -1)
      val yRange = if (vent.startY < vent.endY) (vent.startY to vent.endY) else (vent.startY to vent.endY by -1)
      val loc = xRange.zip(yRange).foldLeft(locations) { case (l, (x, y)) =>
        l.updated(x, l(x).updated(y, l(x)(y) + 1))
      }
      Space(loc)
    }
    else
      self

  def result: Int = locations.flatten.count(_ >= 2)

  override def toString() =
    locations.map(_.map("%04d".format(_)).mkString(" ")).mkString("\n")
}
object Space {
  def make(dimX: Int, dimY: Int): Space = Space(Vector.fill(dimX, dimY)(0))
}
final case class Vent(startX: Int, startY: Int, endX: Int, endY: Int)
