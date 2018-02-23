package net.jcazevedo.adventofcode

import scala.collection.mutable

object Day22 extends App with AdventOfCode {
  val directions = Vector((-1, 0), (0, 1), (1, 0), (0, -1))
  case class Carrier(i: Int, j: Int, d: Int) {
    def next(currentNode: Char): (Char, Carrier) = currentNode match {
      case '.' =>
        val nd = (d + directions.length - 1) % directions.length
        ('#', this.copy(i = this.i + directions(nd)._1, j = this.j + directions(nd)._2, d = nd))
      case '#' =>
        val nd = (d + 1) % directions.length
        ('.', this.copy(i = this.i + directions(nd)._1, j = this.j + directions(nd)._2, d = nd))
      case o =>
        println(s"Unexpected node $o")
        (o, this)
    }

    def nextNew(currentNode: Char): (Char, Carrier) = currentNode match {
      case '.' =>
        val nd = (d + directions.length - 1) % directions.length
        ('W', this.copy(i = this.i + directions(nd)._1, j = this.j + directions(nd)._2, d = nd))
      case 'W' =>
        val nd = d
        ('#', this.copy(i = this.i + directions(nd)._1, j = this.j + directions(nd)._2, d = nd))
      case '#' =>
        val nd = (d + 1) % directions.length
        ('F', this.copy(i = this.i + directions(nd)._1, j = this.j + directions(nd)._2, d = nd))
      case 'F' =>
        val nd = (d + 2) % directions.length
        ('.', this.copy(i = this.i + directions(nd)._1, j = this.j + directions(nd)._2, d = nd))
      case o =>
        println(s"Unexpected node $o")
        (o, this)
    }
  }

  val m1 = mutable.Map[(Int, Int), Char]()
  val m2 = mutable.Map[(Int, Int), Char]()
  val initial = loadFile(22).zipWithIndex.foreach {
    case (s, i) =>
      s.zipWithIndex.foreach {
        case (n, j) =>
          m1((i, j)) = n
          m2((i, j)) = n
      }
  }

  def run1(carrier: Carrier, runs: Int, infections: Int = 0): Int = runs match {
    case 0 => infections
    case n =>
      val (nextNode, nextCarrier) = carrier.next(m1.getOrElseUpdate((carrier.i, carrier.j), '.'))
      m1((carrier.i, carrier.j)) = nextNode
      run1(nextCarrier, n - 1, infections + (if (nextNode == '#') 1 else 0))
  }

  def run2(carrier: Carrier, runs: Int, infections: Int = 0): Int = runs match {
    case 0 => infections
    case n =>
      val (nextNode, nextCarrier) = carrier.nextNew(m2.getOrElseUpdate((carrier.i, carrier.j), '.'))
      m2((carrier.i, carrier.j)) = nextNode
      run2(nextCarrier, n - 1, infections + (if (nextNode == '#') 1 else 0))
  }

  println(run1(Carrier((m1.keySet.map(_._1).max + 1) / 2, (m1.keySet.map(_._2).max + 1) / 2, 0), 10000))
  println(run2(Carrier((m2.keySet.map(_._1).max + 1) / 2, (m2.keySet.map(_._2).max + 1) / 2, 0), 10000000))
}
