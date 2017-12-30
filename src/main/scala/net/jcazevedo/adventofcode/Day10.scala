package net.jcazevedo.adventofcode

object Day10 extends App with AdventOfCode {
  val line = loadFile(10)

  def knotHash(input: String): String = {
    def reverse(ns: IndexedSeq[Int], i: Int, j: Int): IndexedSeq[Int] =
      if (i > j)
        ns
      else {
        val ni = i % ns.length
        val nj = j % ns.length
        reverse(ns.updated(nj, ns(ni)).updated(ni, ns(nj)), i + 1, j - 1)
      }

    def go(ns: IndexedSeq[Int], ls: Seq[Int], position: Int = 0, skipSize: Int = 0): (IndexedSeq[Int], Int, Int) =
      if (ls.isEmpty)
        (ns, position, skipSize)
      else
        go(reverse(ns, position, position + ls.head - 1), ls.tail, position + ls.head + skipSize, skipSize + 1)

    def xorAll(l: Seq[Int]): Int =
      if (l.size == 1) l.head
      else l.head ^ xorAll(l.tail)

    val numbers = (0 to 255)
    val lengths = input.map(_.toInt) ++ Seq(17, 31, 73, 47, 23)

    val (sparseHash, _, _) =
      (0 until 64).foldLeft[(IndexedSeq[Int], Int, Int)]((numbers, 0, 0)) {
        case ((currSeq, currPos, currSkip), _) =>
          go(currSeq, lengths, currPos, currSkip)
      }

    sparseHash.sliding(16, 16).map(xorAll).map(_.toHexString).mkString
  }

  println(knotHash(line.head))
}
