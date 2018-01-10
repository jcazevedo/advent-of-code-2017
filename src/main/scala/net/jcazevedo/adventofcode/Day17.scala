package net.jcazevedo.adventofcode

import scala.collection.mutable

object Day17 extends App with AdventOfCode {
  case class Node(value: Int, var next: Node)
  def startNode(value: Int): Node = {
    val res = Node(value, null)
    res.next = res
    res
  }

  def rotate(l: List[Int], times: Int): List[Int] =
    if (times % l.size == 0)
      l
    else {
      val t = times % l.size
      l.drop(t) ++ l.take(t)
    }

  def insert(maxValue: Int, steps: Int): Iterator[Int] = {
    val l = (1 to maxValue).foldLeft(List[Int](0)) {
      case (l, v) =>
        rotate(l, steps) :+ v
    }
    Iterator.continually(l).flatten
  }

  val steps = 316

  val v1 = insert(2017, steps).dropWhile(_ != 2017).drop(1).next
  println(v1)

  var l = 1
  var i = 0
  var res = 0
  (1 to 50000000).foreach { next =>
    i = (i + steps) % l
    i += 1
    if (i == 1)
      res = next
    l += 1
  }

  println(res)
}
