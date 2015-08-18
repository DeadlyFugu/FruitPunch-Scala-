package net.sekien.fruitpunch

import scala.collection.mutable
import scala.util.parsing.input.Position

case class FruityException(message: String,
    trace: mutable.Stack[Position] = new mutable.Stack[Position])
    extends Exception(message) {
  def addPos(pos: Position) = trace.push(pos)
  def shortString = message + "\n" + ste(trace.last)
  def longString = message + "\n" + (trace.reverse map ste).mkString("\n")
  def ste(pos: Position) = {
    rew((pos.longString.split("\n") filter {s => s != ""} map { n: String =>
      "  " + n
    }).mkString("\n")).updated(0,'@')
  }
  def rew(s: String) = {
    val c = s.length
    var i = 0
    while (i < c && s(i) <= 32) {i+=1}
    (s.split("\n") map {_.substring(i-2)}).mkString("\n")
  }
}
