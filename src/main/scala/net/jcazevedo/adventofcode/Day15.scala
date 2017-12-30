package net.jcazevedo.adventofcode

object Day15 extends App with AdventOfCode {
  def generator(firstValue: Long, factor: Long, valid: Long => Boolean = _ => true, remainder: Long = 2147483647l): Iterator[Long] = {
    def aux(value: Long) = (value * factor) % remainder
    Iterator.iterate(aux(firstValue))(aux).filter(valid)
  }

  def countPairs(generatorA: Iterator[Long], generatorB: Iterator[Long], nPairs: Int): Long =
    generatorA.zip(generatorB).take(nPairs).count {
      case (v1, v2) =>
        v1 % 65536 == v2 % 65536
    }

  println(countPairs(generator(277, 16807), generator(349, 48271), 40000000))
  println(countPairs(generator(277, 16807, _ % 4 == 0), generator(349, 48271, _ % 8 == 0), 5000000))
}
