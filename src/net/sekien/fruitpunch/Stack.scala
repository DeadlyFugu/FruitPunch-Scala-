package net.sekien.fruitpunch

import java.util.NoSuchElementException

import scala.collection.mutable

class Stack(prev: Stack) {

  val ls = new mutable.Stack[Type]

  def push(value: Type): Unit = ls.push(value)

  // todo remove parens from pop and peek
  def pop(): Type = try {
    ls.pop()
  } catch {
    case e: NoSuchElementException => throw new FruityException("stack underflow")
  }

  // util methods for DragonScales
  def popAsNum: Double = popAs[DoubleValue].value
  def popAsStr: String = popAs[StringValue].value
  def popAs[T](implicit ev: Manifest[T]): T = pop() match {
    case v: T => v
    case n => throw new FruityException("expected " + Type.getTypeName(ev.runtimeClass) + " got " + Type.getTypeName(n.getClass))
  }

  def peek(): Type = ls.top

  def dot(n: Int): Unit = if (prev != null) {
    ls.pushAll(prev.top(n))
  } else {
    throw new FruityException("dot on root stack")
  }

  def top(n: Int) = {
    try {
      val r = ls.slice(0, n).reverse
      for (i <- 1 to n) ls.pop()
      r
    } catch {
      case e: NoSuchElementException => throw new FruityException("stack underflow")
    }
  }

  def merge(): Unit = prev.ls.pushAll(ls.reverse)

  def toList = ls.reverse.toList
  def toListReversed = ls.toList

  def length = ls.length

  def clear(): Unit = ls.clear()

  override def toString = s"(${ls.length}) ["+
    toList.map({o: Type => PrettyPrinter(o)}).mkString(", ")+"]"
}