package net.jcazevedo.adventofcode

object Day12 extends App with AdventOfCode {
  val neighbors: Map[Int, List[Int]] = loadFile(12).map { l =>
    val Array(from, to) = l.split("<->")
    from.trim.toInt -> to.split(",").map(_.trim.toInt).toList
  }.toMap

  def groupOf(curr: Int, neighbors: Map[Int, List[Int]], visited: Set[Int] = Set()): Set[Int] =
    if (visited.contains(curr))
      visited
    else {
      val nVisited = visited + curr
      neighbors(curr).foldLeft(nVisited) {
        case (currVisited, neigh) =>
          currVisited ++ groupOf(neigh, neighbors, currVisited)
      }
    }

  val (_, nGroups) = neighbors.foldLeft((Set.empty[Int], 0)) {
    case ((visited, nGroups), (node, _)) =>
      if (!visited.contains(node)) {
        val nextVisited = visited ++ groupOf(node, neighbors)
        (nextVisited, nGroups + 1)
      } else {
        (visited, nGroups)
      }
  }

  println(groupOf(0, neighbors).size)
  println(nGroups)
}
