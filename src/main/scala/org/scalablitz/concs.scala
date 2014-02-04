package org.scalablitz






sealed abstract class Conc[+T] {
  def level: Int
  def size: Int
  def normalize = this
}


case object Empty extends Conc[Nothing] {
  def level = 0
  def size = 0
}


sealed abstract class Leaf[T] extends Conc[T]


case class Single[T](x: T) extends Leaf[T] {
  def level = 0
  def size = 1
}


case class Chunk[T](array: Array[T], size: Int) {
  def level = 0
}


case class <>[T](left: Conc[T], right: Conc[T]) {
  val level = 1 + math.max(left.level, right.level)
  val size = left.size + right.size
}


object Conc {

  def concat[T](self: Conc[T], that: Conc[T]) = ???

}
