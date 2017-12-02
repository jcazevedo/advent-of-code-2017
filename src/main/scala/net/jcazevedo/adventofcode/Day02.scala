package net.jcazevedo.adventofcode

object Day02 extends App with AdventOfCode {
  val lines = loadFile(2)
  val ns = lines.map(_.split("\\s+").map(_.toInt))
  val res1 = ns.map { n =>
    val ss = n.sorted
    ss.last - ss.head
  }.sum
  println(s"Part One: $res1")
  val res2 = ns.map { n =>
    var l = 0
    n.zipWithIndex.foreach {
      case (n1, i) =>
        n.zipWithIndex.foreach {
          case (n2, j) =>
            if (i != j && n1 % n2 == 0)
              l = n1 / n2
        }
    }
    l
  }.sum
  println(s"Part Two: $res2")
}
