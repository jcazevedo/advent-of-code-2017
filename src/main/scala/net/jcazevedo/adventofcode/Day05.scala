package net.jcazevedo.adventofcode

object Day05 extends App with AdventOfCode {
  val lines = loadFile(5)
  val l = Array(lines.map(_.toInt): _*)
  val L = l.length
  var i = 0
  var steps = 0
  while (i >= 0 && i < L) {
    val pi = l(i)
    l(i) += 1
    i += pi
    steps += 1
  }
  println(s"Part One: $steps")

  val nl = Array(lines.map(_.toInt): _*)
  i = 0
  steps = 0
  while (i >= 0 && i < L) {
    val pi = nl(i)
    if (pi >= 3)
      nl(i) -= 1
    else
      nl(i) += 1
    i += pi
    steps += 1
  }
  println(s"Part Two: $steps")
}
