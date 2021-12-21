package example

import better.files.Resource

object Hello extends App {
  def parseData(input: String): List[Scanner] = {
    def parseScanner(ls: List[String]): (Scanner, List[String]) = {
      val scanner = Scanner {
        ls.tail.takeWhile(s => !s.startsWith("---")).filter(_.nonEmpty).map(_.split(",").toList match {
          case x :: y :: z :: Nil => V3(x.toInt, y.toInt, z.toInt)
          case _ => ???
        }).toSet
      }
      (scanner, ls.tail.dropWhile(l => !l.startsWith("---")))
    }
    val lines = input.split("\n").toList
    List.unfold(lines)(ls => if (ls.count(_.nonEmpty) == 0) None else Some(parseScanner(ls)))
  }

  val scanners = parseData(Resource.getAsString("input.txt"))
  println(scanners.head.mergeAll(scanners.tail).beacons.size)
}

final case class V3(x: Int, y: Int, z: Int) { self =>
  def rotations: List[V3] = List(
    V3(x, y, z), V3(y, z, x), V3(z, x, y),
    V3(-x, z, y), V3(z, y, -x), V3(y, -x, z),
    V3(x, z, -y), V3(z, -y, x), V3(-y, x, z),
    V3(x, -z, y), V3(-z, y, x), V3(y, x, -z),
    V3(-x, -y, z), V3(-y, z, -x), V3(z, -x, -y),
    V3(-x, y, -z), V3(y, -z, -x), V3(-z, -x, y),
    V3(x, -y, -z), V3(-y, -z, x), V3(-z, x, -y),
    V3(-x, -z, -y), V3(-z, -y, -x), V3(-y, -x, -z),
  )

  def -(that: V3): V3 =
    V3(x - that.x, y - that.y, z - that.z)

  def +(that: V3): V3 =
    V3(x + that.x, y + that.y, z + that.z)
}

final case class Scanner(beacons: Set[V3]) { self =>
  lazy val rotations: List[Scanner] =
    beacons.toList.map(_.rotations).transpose.map(rotated => Scanner(rotated.toSet))

  lazy val beaconVectors: Set[V3] = {
    for {
      first <- beacons
      second <- beacons.filter(_ != first)
    } yield first - second
  }.toSet

  def translate(value: V3): Scanner =
    Scanner(beacons.map(_ + value))

  def merge(that: Scanner): Option[Scanner] =
    that.alignTo(self).fold[Option[Scanner]](None)(alligned => Some(Scanner((self.beacons ++ alligned.beacons))))

  def mergeAll(scanners: List[Scanner]): Scanner =
    mergeAllHelper(scanners, Nil)

  private def mergeAllHelper(scanners: List[Scanner], attempted: List[Scanner]): Scanner =
    scanners match {
      case x :: xs => self.merge(x).fold(mergeAllHelper(xs, x :: attempted))(_.mergeAllHelper(xs, attempted))
      case Nil =>
        println(attempted.size)
        if (attempted.nonEmpty) mergeAllHelper(attempted, Nil) else self
    }

  def alignTo(that: Scanner): Option[Scanner] = {
    rotations
      .filter(_.beaconVectors.intersect(that.beaconVectors).size >= 132)
      .flatMap { rotation =>
        rotation.beacons.flatMap { beacon =>
          that.beacons.flatMap { thatBeacon =>
            val corrected = rotation.translate(thatBeacon - beacon)
            if (corrected.beacons.intersect(that.beacons).size >= 12) Some(corrected) else None
          }
        }
      }.headOption
  }
}
object Scanner {
  def apply(beacons: V3*): Scanner =
    Scanner(beacons.toSet)
}

