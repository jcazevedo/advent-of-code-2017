package net.jcazevedo.adventofcode

import scala.annotation.tailrec

object Day06 extends App with AdventOfCode {
  val lines = loadFile(6)
  val blocks = lines.head.split("\\s+").map(_.toInt).toVector
  @tailrec
  def redistribute(v: Int, i: Int, curr: Vector[Int]): Vector[Int] = {
    if (v == 0)
      curr
    else if (i >= curr.length)
      redistribute(v, 0, curr)
    else
      redistribute(v - 1, i + 1, curr.updated(i, curr(i) + 1))
  }

  @tailrec
  def steps1(b: Vector[Int], v: Set[Vector[Int]] = Set(), n: Int = 0): Int = {
    if (v.contains(b))
      n
    else {
      val mi = b.zipWithIndex.maxBy(_._1)._2
      steps1(redistribute(b(mi), mi + 1, b.updated(mi, 0)), v + b, n + 1)
    }
  }

  @tailrec
  def steps2(b: Vector[Int], v: Map[Vector[Int], Int] = Map(), n: Int = 0): Int = {
    if (v.contains(b))
      n - v(b)
    else {
      val mi = b.zipWithIndex.maxBy(_._1)._2
      steps2(redistribute(b(mi), mi + 1, b.updated(mi, 0)), v + (b -> n), n + 1)
    }
  }

  println(s"Part One: ${steps1(blocks)}")
  println(s"Part Two: ${steps2(blocks)}")
}
