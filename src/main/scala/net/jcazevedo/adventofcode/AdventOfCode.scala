package net.jcazevedo.adventofcode

import scala.collection.mutable
import scala.language.existentials

trait AdventOfCode {
  def loadFile(day: Int): List[String] =
    io.Source.fromFile(f"$day%02d.input").getLines.toList

  def aStar[T](
    from:      T,
    to:        T,
    heuristic: T => Double,
    neighbors: T => List[(Double, T)]): Option[List[T]] = {
    implicit val ord: Ordering[(Double, List[T])] =
      Ordering.by[(Double, List[T]), Double](_._1).reverse
    val pq = mutable.PriorityQueue[(Double, List[T])]()
    val cost = mutable.Map[T, Double]()
    cost(from) = 0.0
    pq.enqueue((heuristic(from), List(from)))
    while (pq.nonEmpty) {
      val (_, path @ (current :: _)) = pq.dequeue()
      if (current == to)
        return Some(path.reverse)
      val currentCost = cost(current)
      neighbors(current).foreach {
        case (nextCost, next) =>
          if (!cost.contains(next) || cost(next) >= currentCost + nextCost) {
            cost(next) = currentCost + nextCost
            pq.enqueue((currentCost + nextCost + heuristic(next), next :: path))
          }
      }
    }
    None
  }
}
