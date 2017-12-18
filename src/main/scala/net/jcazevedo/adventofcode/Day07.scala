package net.jcazevedo.adventofcode

import scala.collection.mutable

object Day07 extends App with AdventOfCode {
  val lines = loadFile(7)

  val weight = mutable.Map[String, Int]()
  val parent = mutable.Map[String, String]()
  val nodes = mutable.Set[String]()
  val children = mutable.Map[String, List[String]]()

  lines.foreach { line =>
    val ss = line.split("\\s+")
    nodes += ss(0)
    weight(ss(0)) = ss(1).drop(1).dropRight(1).toInt
    if (ss.length > 2) {
      (3 until ss.length).foreach { idx =>
        val n = if (ss(idx).endsWith(",")) ss(idx).dropRight(1) else ss(idx)
        nodes += n
        parent(n) = ss(0)
        children(ss(0)) = n :: children.getOrElse(ss(0), Nil)
      }
    }
  }

  var base = nodes.find(n => !parent.contains(n))

  base match {
    case Some(node) =>
      println(s"Part One: $node")
    case None =>
      println("Not found")
  }

  val weightCache = mutable.Map[String, Int]()

  def programWeight(program: String): Int =
    if (weightCache.contains(program))
      weightCache(program)
    else {
      val w = weight(program) + children.getOrElse(program, Nil).map(programWeight).sum
      weightCache(program) = w
      w
    }

  def balanced(program: String): Boolean = {
    val weights = children.getOrElse(program, Nil).map(programWeight)
    weights.zip(weights.drop(1)).forall { case (v1, v2) => v1 == v2 }
  }

  def treeIterator(program: String): Iterator[String] = {
    Iterator(program) ++ children.getOrElse(program, Nil).flatMap(treeIterator)
  }

  treeIterator(base.get).foreach { program =>
    if (!balanced(program)) {
      println(s"Program '$program' not balanced!")
      println("Weights of children:")
      children.getOrElse(program, Nil).foreach { children =>
        println(s"$children -> ${programWeight(children)}")
      }
      println("---")
    }
  }
}
