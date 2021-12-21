package example

object Hello extends App {
 
}

final case class Beacon(x: Int, y: Int, z: Int) {
  def transform(transform: Transform): Beacon = {
    ???
  }
}
final case class Transform()
final case class Scanner(x: Int, y: Int, z: Int, rx: Int, ry: Int, rz: Int, beacons: List[Beacon]) {
  def transformTo(that: Scanner): Transform = ???
  def overlap(that: Scanner): Int = ???
  def rotations: List[Scanner] = ???
  def rotate(): Scanner = {

  }
}
