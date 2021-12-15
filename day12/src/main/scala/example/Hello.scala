package example
import Hello.Path
import better.files.Resource
object Hello extends App {
  type Path = List[Cave]
  def parseCave(str: String): Cave = str match {
    case "start" => Cave.Start
    case "end"               => Cave.End
    case s if s.head.isLower => Cave.Small(s)
    case s                   => Cave.Big(s)
  }
  def parseData(input: String): List[(Cave, Cave)] = {
    input
      .split("\n")
      .toList
      .map { str =>
        str.split("-").toList match {
          case start :: end :: Nil =>
            val startCave = parseCave(start)
            val endCave = parseCave(end)
            (startCave, endCave)
          case _ =>
            throw new IllegalArgumentException()
        }
      }
      .toList
  }
  def makeMap(connections: List[(Cave, Cave)]): CaveMap = {

    connections.foldLeft(CaveMap.empty)((a, b) => a.addConnection(b._1, b._2))
  }
  println(
    makeMap(parseData(Resource.getAsString("input.txt")))
      .findPaths(Cave.Start)
      .length
  )
}

final case class CaveMap(connections: Map[Cave, Set[Cave]]) {
  def findPaths(
      startPoint: Cave,
      visitedCaves: Set[Cave] = Set.empty,
      specialCave: Boolean = false
  ): List[Path] = startPoint match {
    case Cave.End => 
      List(List(Cave.End))
    case _ =>
      val newVisited = startPoint match {
        case Cave.Small(_) => visitedCaves + startPoint
        case _             => visitedCaves
      }
      connections(startPoint).toList
        .flatMap {
          case Cave.Start => 
            Nil
          case c @ Cave.Small(_) =>
            val visited = newVisited.contains(c)
            if (!visited) 
              findPaths(c, newVisited, specialCave)
            else if (visited && !specialCave)
              findPaths(c, newVisited, true)
            else
              Nil
          case c =>
            findPaths(c, newVisited, specialCave)
        }
        .map(startPoint :: _)
  }
  def addConnection(start: Cave, end: Cave): CaveMap = {
    CaveMap(
      connections
        + ((start, connections.getOrElse(start, Set.empty) + end))
        + ((end, connections.getOrElse(end, Set.empty) + start))
    )
  }
}
object CaveMap {
  val empty = CaveMap(Map.empty)
}
sealed trait Cave
object Cave {
  final case class Big(name: String) extends Cave
  final case class Small(name: String) extends Cave
  case object Start extends Cave
  case object End extends Cave
}
