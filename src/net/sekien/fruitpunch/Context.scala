package net.sekien.fruitpunch

import scala.collection.mutable

class Context(var parent: Context) extends Type {
  val map = new mutable.HashMap[String, Type]()

  def getv(key: String): Type = map.get(key) match {
    case Some(value) => value
    case None => parent.getv(key)
  }

  def touchv(key: String): Context = map.getOrElseUpdate(key, new Context(this)).asInstanceOf[Context]

  def setv(key: String, value: Type): Unit = {
    if (map.contains(key)) {
      map.put(key, value)
    } else {
      parent.setv(key, value)
    }
  }

  def bindv(key: String, value: Type): Unit = {
    map.put(key, value)
  }

  def lsv() = map.keySet

  override def toString = ":{" + map.keySet.map(s => s"$s:${Type.getTypeName(map(s).getClass)}").mkString(",") + "}"
}

class RootContext(name: String) extends Context(null) {
  val lazies: mutable.Map[String, () => Unit] = mutable.Map()

  def addLazy(name: String, generator: () => Unit) = {
    lazies.put(name, generator)
  }

  override def getv(key: String): Type = map.get(key) match {
    case Some(value) => value
    case None => {
      if (lazies.contains(key)) {
        if (FruitPunch.cfig.verbose) println("lazy load " + key)
        lazies.get(key).orNull.apply()
        lazies.remove(key)
        this.getv(key)
      } else {
        throw new FruityException(s"$key is unbound")
      }
    }
  }

  override def setv(key: String, value: Type): Unit = {
    if (map.contains(key)) {
      map.put(key, value)
    } else {
      throw new FruityException(s"$key is unbound")
    }
  }

  override def toString = s"<$name>"
}