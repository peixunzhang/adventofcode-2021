package example

import scala.collection.mutable

object Hello extends App {
  val start = Board(
    Vector(
      Hallway(None),
      Hallway(None),
      SideRoom(List(D, D, D, B), 4),
      Hallway(None),
      SideRoom(List(B, C, B, D), 4),
      Hallway(None),
      SideRoom(List(A, B, A, A), 4),
      Hallway(None),
      SideRoom(List(C, A, C, C), 4),
      Hallway(None),
      Hallway(None)
    ),
    Map(
      A -> 2,
      B -> 4,
      C -> 6,
      D -> 8
    )
  )
  val end = Board(
    Vector(
      Hallway(None),
      Hallway(None),
      SideRoom(List(A, A, A, A), 4),
      Hallway(None),
      SideRoom(List(B, B, B, B), 4),
      Hallway(None),
      SideRoom(List(C, C, C, C), 4),
      Hallway(None),
      SideRoom(List(D, D, D, D), 4),
      Hallway(None),
      Hallway(None)
    ),
    Map(
      A -> 2,
      B -> 4,
      C -> 6,
      D -> 8
    )
  )
  println(Dijkstra.cheapestPath[Board](start, end, _.validMoves).map(_._2))
}
sealed trait Shrimp { self =>
  val muliplier: Int =
    self match {
      case A => 1
      case B => 10
      case C => 100
      case D => 1000
    }
}
case object A extends Shrimp {
  override def toString(): String = "A"
}
case object B extends Shrimp {
  override def toString(): String = "B"
}
case object C extends Shrimp {
  override def toString(): String = "C"
}
case object D extends Shrimp {
  override def toString(): String = "D"
}

sealed trait Room { self =>
  def canPass: Boolean =
    self match {
      case Hallway(space) => space.isEmpty
      case SideRoom(_, _) => true
    }
  def take: Option[(Shrimp, Int, Room)] =
    self match {
      case Hallway(Some(shrimp)) => 
        Some((shrimp, 0, Hallway(None)))
      case SideRoom(x :: xs, depth) => 
        Some((x, (depth - xs.size) * x.muliplier, SideRoom(xs, depth)))
      case _ => None
    }
  def isHallway: Boolean =
    self match {
      case Hallway(_) => true
      case SideRoom(_, _) => false
    }
}

final case class Hallway(space: Option[Shrimp]) extends Room {
  override def toString(): String =
    space.fold(".")(_.toString())
}

final case class SideRoom(values: List[Shrimp], depth: Int) extends Room {
  def hasSpace: Boolean = values.size < depth
  def add(shrimp: Shrimp): (Room, Int) =
    (SideRoom(shrimp :: values, depth), (depth - values.size) * shrimp.muliplier)
  override def toString(): String =
    values.map(Some(_)).reverse.padTo(depth, None).reverse.map(_.fold(".")(_.toString())).mkString
}

final case class Board(rooms: Vector[Room], shrimpRooms: Map[Shrimp, Int]) { self =>
  def validMoves: List[(Board, Int)] =
    (0 until rooms.size).flatMap(moveFromRoom(_)).toList

  def moveFromRoom(startId: Int): List[(Board, Int)] =
    rooms(startId).take.fold(List.empty[(Board, Int)]) {
      case (shrimp, startCost, startRoom) =>
        reachableRooms(startId).flatMap { targetId =>
          val moveCost = Math.abs(targetId - startId) * shrimp.muliplier
          rooms(targetId) match {
            case Hallway(None) =>
              if (!startRoom.isHallway) {
                val newBoard = self
                  .updated(startId, startRoom)
                  .updated(targetId, Hallway(Some(shrimp)))
                List((newBoard, startCost + moveCost))
              } else Nil
            case Hallway(Some(_)) => Nil
            case sr @ SideRoom(_, _) =>
              val canMoveIn = 
                sr.hasSpace && 
                shrimpRooms(shrimp) == targetId && 
                sr.values.forall(_ == shrimp)

              if (canMoveIn) {
                val (targetRoom, targetCost) = sr.add(shrimp)
                val newBoard = self
                  .updated(startId, startRoom)
                  .updated(targetId, targetRoom)
                List((newBoard, startCost + moveCost + targetCost))
              } else Nil
          }
        }
    }
  def reachableRooms(from: Int): List[Int] = {
    def go(i: Int, step: Int, acc: List[Int] = Nil): List[Int] = {
      if (i >= 0 && i < rooms.size && rooms(i).canPass) go(i+step, step, i :: acc)
      else acc
    }
    go(from - 1, -1) ++ go(from + 1, 1).reverse
  }
  def updated(roomId: Int, room: Room): Board =
    copy(rooms.updated(roomId, room))
  override def toString(): String =
    rooms.mkString("\n")
}


object Dijkstra {
  def cheapestPath[A](start: A, end: A, paths: A => List[(A, Int)]): Option[(List[A], Int)] = {
    
    final case class Vertex(value: A, path: List[A], distance: Int)

    object Vertex {
      implicit val ordering: Ordering[Vertex] =
        Ordering.by(_.distance)
    }

    val vertices = mutable.PriorityQueue.empty[Vertex].reverse
    val visited = mutable.Set.empty[A]

    vertices.enqueue(Vertex(start, List(start), 0))

    var result = Option.empty[(List[A], Int)]

    while (result.isEmpty && !vertices.isEmpty) {
      val next = vertices.dequeue()
      if (next.value == end)
        result = Some((next.path.reverse, next.distance))
      else if (visited.add(next.value)) {
        paths(next.value)
          .filterNot { case (a, _) => visited.contains(a) }
          .foreach { case (neighbour, neighbourDistance) =>
            val calculatedDistance = next.distance + neighbourDistance
            vertices.enqueue(Vertex(neighbour, neighbour :: next.path, calculatedDistance))
          }
      }
    }
    result
  }
}
