package example

import scala.collection.mutable

object Hello extends App {
  val start = Board(
    Hallway(None),
    Hallway(None),
    SideRoom(4, A, List(D, D, D, B)),
    Hallway(None),
    SideRoom(4, B, List(B, C, B, D)),
    Hallway(None),
    SideRoom(4, C, List(A, B, A, A)),
    Hallway(None),
    SideRoom(4, D, List(C, A, C, C)),
    Hallway(None),
    Hallway(None)
  )
  println(Dijkstra.cheapestPath[Board](start, _.isDone, _.validMoves).map(_._2))
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
  def isDone: Boolean =
    self match {
      case Hallway(space) =>
        space.isEmpty
      case SideRoom(depth, shrimpType, values) =>
        values.size >= depth && values.forall(_ == shrimpType)
    }
  def canPass: Boolean =
    self match {
      case Hallway(space)    => space.isEmpty
      case SideRoom(_, _, _) => true
    }
  def take: Option[(Shrimp, Int, Room)] =
    self match {
      case Hallway(Some(shrimp)) =>
        Some((shrimp, 0, Hallway(None)))
      case sr @ SideRoom(depth, shrimpType, x :: xs) =>
        Some((x, (depth - xs.size) * x.muliplier, sr.copy(values = xs)))
      case _ => None
    }
  def isHallway: Boolean =
    self match {
      case Hallway(_)        => true
      case SideRoom(_, _, _) => false
    }
}

final case class Hallway(space: Option[Shrimp]) extends Room {
  override def toString(): String =
    space.fold(".")(_.toString())
}

final case class SideRoom(depth: Int, shrimpType: Shrimp, values: List[Shrimp])
    extends Room {
  def hasSpace: Boolean = values.size < depth
  def add(shrimp: Shrimp): Option[(Room, Int)] = {
    val canMoveIn =
      hasSpace && shrimpType == shrimp && values.forall(_ == shrimpType)
    if (canMoveIn)
      Some(
        (
          SideRoom(depth, shrimpType, shrimp :: values),
          (depth - values.size) * shrimp.muliplier
        )
      )
    else
      None
  }
  override def toString(): String =
    values
      .map(Some(_))
      .reverse
      .padTo(depth, None)
      .reverse
      .map(_.fold(".")(_.toString()))
      .mkString
}

final case class Board(rooms: Vector[Room]) { self =>
  def isDone: Boolean =
    rooms.forall(_.isDone)

  def validMoves: List[(Board, Int)] =
    (0 until rooms.size).flatMap(movesFromRoom(_)).toList

  def movesFromRoom(startId: Int): List[(Board, Int)] =
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
            case sr @ SideRoom(_, _, _) =>
              sr.add(shrimp).fold(List.empty[(Board, Int)]) {
                case (targetRoom, targetCost) =>
                  val newBoard = self
                    .updated(startId, startRoom)
                    .updated(targetId, targetRoom)
                  List((newBoard, startCost + moveCost + targetCost))
              }
          }
        }
    }
  def reachableRooms(startId: Int): List[Int] = {
    def go(i: Int, step: Int, acc: List[Int] = Nil): List[Int] = {
      if (i >= 0 && i < rooms.size && rooms(i).canPass)
        go(i + step, step, i :: acc)
      else acc
    }
    go(startId - 1, -1) ++ go(startId + 1, 1).reverse
  }
  def updated(roomId: Int, room: Room): Board =
    copy(rooms.updated(roomId, room))
  override def toString(): String =
    rooms.mkString("\n")
}

object Board {
  def apply(rooms: Room*): Board =
    Board(rooms.toVector)
}

object Dijkstra {
  def cheapestPath[A](
      start: A,
      isTarget: A => Boolean,
      paths: A => List[(A, Int)]
  ): Option[(List[(A, Int)], Int)] = {

    final case class Vertex(value: A, path: List[(A, Int)], distance: Int)

    object Vertex {
      implicit val ordering: Ordering[Vertex] =
        Ordering.by(_.distance)
    }

    val vertices = mutable.PriorityQueue.empty[Vertex].reverse
    val visited = mutable.Set.empty[A]

    vertices.enqueue(Vertex(start, List((start, 0)), 0))

    var result = Option.empty[(List[(A, Int)], Int)]

    while (result.isEmpty && !vertices.isEmpty) {
      val next = vertices.dequeue()
      if (isTarget(next.value))
        result = Some((next.path.reverse, next.distance))
      else if (visited.add(next.value)) {
        paths(next.value)
          .filterNot { case (a, _) => visited.contains(a) }
          .foreach { case (neighbour, neighbourDistance) =>
            val calculatedDistance = next.distance + neighbourDistance
            vertices.enqueue(
              Vertex(
                neighbour,
                (neighbour, calculatedDistance) :: next.path,
                calculatedDistance
              )
            )
          }
      }
    }
    result
  }
}
