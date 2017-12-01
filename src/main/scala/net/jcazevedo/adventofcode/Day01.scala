package net.jcazevedo.adventofcode

object Day01 extends App with AdventOfCode {
  val lines = loadFile(1)
  val sequence = lines.head.map(_ - '0')
  def sumIfEqual(current: Int, pair: (Int, Int)) = pair match {
    case (i, j) if i == j => current + i
    case _ => current
  }
  val res1 = sequence.zip(sequence.tail :+ sequence.head).foldLeft(0)(sumIfEqual)
  println(s"Part One: $res1")
  val res2 = sequence.zip(sequence.drop(sequence.length / 2) ++ sequence.take(sequence.length / 2)).foldLeft(0)(sumIfEqual)
  println(s"Part Two: $res2")
}
