package net.jcazevedo.adventofcode

object Day09 extends App with AdventOfCode {
  val lines = loadFile(9)

  def run(groups: String): (Int, Int) = {
    var totalScore = 0
    var nonCancellable = 0
    var inGroup = 0
    var garbage = false
    var ignore = false
    groups.foreach { ch =>
      if (!ignore) {
        if (ch == '!') {
          ignore = true
        } else if (garbage) {
          if (ch == '>') {
            garbage = false
          } else {
            nonCancellable += 1
          }
        } else if (ch == '<') {
          garbage = true
        } else if (ch == '{') {
          inGroup += 1
        } else if (ch == '}') {
          totalScore += inGroup
          inGroup -= 1
        }
      } else {
        ignore = false
      }
    }
    (totalScore, nonCancellable)
  }

  println(run(lines.head))
}
