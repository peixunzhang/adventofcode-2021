package example

import better.files.Resource

object Hello extends App {
  def parseData(input: String): List[ScannerGroup] = {
    def parseScanner(ls: List[String]): (ScannerGroup, List[String]) = {
      val scanner = ScannerGroup(
        ls.tail.takeWhile(s => !s.startsWith("---")).filter(_.nonEmpty).map(_.split(",").toList match {
          case x :: y :: z :: Nil => V3(x.toInt, y.toInt, z.toInt)
          case _ => ???
        }).toSet,
        Set(V3(0, 0, 0))
      )
      (scanner, ls.tail.dropWhile(l => !l.startsWith("---")))
    }
    val lines = input.split("\n").toList
    List.unfold(lines)(ls => if (ls.count(_.nonEmpty) == 0) None else Some(parseScanner(ls)))
  }

  val scanners = parseData(Resource.getAsString("input.txt"))
  val alligned = scanners.head.mergeAll(scanners.tail).scanners.toList
  val distances = for {
    first <- alligned
    second <- alligned.filter(_ != first)
  } yield (first - second).manhattanDistance

  println(distances.max)
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

  def manhattanDistance: Int =
    Math.abs(x) + Math.abs(y) + Math.abs(z)
}


final case class ScannerGroup(beacons: Set[V3], scanners: Set[V3]) { self =>
  lazy val rotations: List[ScannerGroup] =
    beacons.toList.map(_.rotations).transpose.zip(scanners.toList.map(_.rotations).transpose).map { case (rotatedBeacons, rotatedScanners) =>
      ScannerGroup(rotatedBeacons.toSet, rotatedScanners.toSet)
    }

  lazy val beaconVectors: Set[V3] = {
    for {
      first <- beacons
      second <- beacons.filter(_ != first)
    } yield first - second
  }.toSet

  def translate(value: V3): ScannerGroup =
    ScannerGroup(beacons.map(_ + value), scanners.map(_ + value))

  def merge(that: ScannerGroup): Option[ScannerGroup] =
    that.alignTo(self).fold[Option[ScannerGroup]](None) { alligned =>
      Some(ScannerGroup(self.beacons ++ alligned.beacons, self.scanners ++ alligned.scanners))
    }

  def mergeAll(scanners: List[ScannerGroup]): ScannerGroup =
    mergeAllHelper(scanners, Nil)

  private def mergeAllHelper(scanners: List[ScannerGroup], attempted: List[ScannerGroup]): ScannerGroup =
    scanners match {
      case x :: xs => self.merge(x).fold(mergeAllHelper(xs, x :: attempted))(_.mergeAllHelper(xs, attempted))
      case Nil => if (attempted.nonEmpty) mergeAllHelper(attempted, Nil) else self
    }

  def alignTo(that: ScannerGroup): Option[ScannerGroup] = {
    rotations
      .filter(_.beaconVectors.intersect(that.beaconVectors).size >= 132)
      .flatMap { candidate =>
        candidate.beacons.flatMap { beacon =>
          that.beacons.flatMap { thatBeacon =>
            val translated = candidate.translate(thatBeacon - beacon)
            if (translated.beacons.intersect(that.beacons).size >= 12) Some(translated) else None
          }
        }
      }.headOption
  }
}
object Scanner {
  def apply(beacons: V3*): ScannerGroup =
    ScannerGroup(beacons.toSet, Set(V3(0, 0, 0)))
}

