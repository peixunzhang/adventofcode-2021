package example

import better.files.Resource

object Hello extends App {
  def parseData(input: String): List[Scanner] = {
    def parseScanner(ls: List[String]): (Scanner, List[String]) = {
      val scanner = Scanner {
        ls.tail.takeWhile(s => !s.startsWith("---")).filter(_.nonEmpty).map(_.split(",").toList match {
          case x :: y :: z :: Nil => Beacon(x.toInt, y.toInt, z.toInt)
          case _ => ???
        })
      }
      (scanner, ls.tail.dropWhile(l => !l.startsWith("---")))
    }
    val lines = input.split("\n").toList
    List.unfold(lines)(ls => if (ls.count(_.nonEmpty) == 0) None else Some(parseScanner(ls)))
  }

  val scanners = parseData(Resource.getAsString("input.txt"))
  println(scanners.head.mergeAll(scanners.tail).beacons.size)
}

final case class Beacon(x: Int, y: Int, z: Int) { self =>
  def rotations: List[Beacon] = List(
    Beacon(x, y, z), Beacon(y, z, x), Beacon(z, x, y),
    Beacon(-x, z, y), Beacon(z, y, -x), Beacon(y, -x, z),
    Beacon(x, z, -y), Beacon(z, -y, x), Beacon(-y, x, z),
    Beacon(x, -z, y), Beacon(-z, y, x), Beacon(y, x, -z),
    Beacon(-x, -y, z), Beacon(-y, z, -x), Beacon(z, -x, -y),
    Beacon(-x, y, -z), Beacon(y, -z, -x), Beacon(-z, -x, y),
    Beacon(x, -y, -z), Beacon(-y, -z, x), Beacon(-z, x, -y),
    Beacon(-x, -z, -y), Beacon(-z, -y, -x), Beacon(-y, -x, -z),
  )
  def -(that: Beacon): Beacon =
    Beacon(x - that.x, y - that.y, z - that.z)

  def +(that: Beacon): Beacon =
    Beacon(x + that.x, y + that.y, z + that.z)

  def reverse: Beacon =
    Beacon(-x, -y, -z)

  def isSameOrReverse(that: Beacon) = self == that || self.reverse == that
}

final case class Scanner(beacons: List[Beacon]) { self =>
  lazy val rotations: List[Scanner] =
    beacons.map(_.rotations).transpose.map(Scanner(_))

  lazy val beaconVectors: List[Beacon] =
    for {
      first <- beacons
      second <- beacons.filter(_ != first)
    } yield first - second

  def applyCorrection(correction: Beacon): Scanner =
    Scanner(beacons.map(_ + correction))

  def merge(that: Scanner): Option[Scanner] =
    that.alignTo(self).fold[Option[Scanner]](None)(alligned => Some(Scanner((self.beacons ++ alligned.beacons).distinct)))

  def mergeAll(scanners: List[Scanner]): Scanner =
    mergeAllHelper(scanners, Nil)

  private def mergeAllHelper(scanners: List[Scanner], attempted: List[Scanner]): Scanner =
    scanners match {
      case x :: xs => merge(x).fold(mergeAllHelper(xs, x :: attempted))(_.mergeAllHelper(xs, attempted))
      case Nil =>
        println(attempted.size)
        if (attempted.nonEmpty) mergeAllHelper(attempted, Nil) else self
    }

  def alignTo(that: Scanner): Option[Scanner] = {
    rotations
      .find(_.beaconVectors.count(beacon => that.beaconVectors.exists(_.isSameOrReverse(beacon))) >= 132)
      .flatMap { rotation =>
        rotation.beacons.flatMap { beacon =>
          that.beacons.flatMap { thatBeacon =>
            val corrected = rotation.applyCorrection(thatBeacon - beacon)
            if (corrected.beacons.intersect(that.beacons).size >= 12) Some(corrected) else None
          }
        }.headOption
      }
  }
}
object Scanner {
  def apply(beacons: Beacon*): Scanner =
    Scanner(beacons.toList)
}

