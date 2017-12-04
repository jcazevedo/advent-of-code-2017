package net.jcazevedo.adventofcode

object Day04 extends App with AdventOfCode {
  val lines = loadFile(4)
  def valid(line: String) = {
    val words = line.split("\\s+").toList
    words.length == words.toSet.size
  }

  println(s"Part One: ${lines.count(valid)}")

  def anagram(word1: String, word2: String): Boolean =
    word1.sorted == word2.sorted

  def valid1(line: String) = {
    def aux(curr: String, other: List[String]): Boolean =
      if (other.isEmpty)
        true
      else
        other.forall(!anagram(_, curr)) && aux(other.head, other.tail)
    val words = line.split("\\s+").toList
    aux(words.head, words.tail)
  }

  println(s"Part Two: ${lines.count(valid1)}")
}
