package org.scalablitz



import scala.annotation.unchecked
import scala.reflect.ClassTag



sealed abstract class Conc[+T] {
  def level: Int
  def size: Int
  def left: Conc[T]
  def right: Conc[T]
  def normalize = this
}


case class <>[T](left: Conc[T], right: Conc[T]) extends Conc[T] {
  val level = 1 + math.max(left.level, right.level)
  val size = left.size + right.size
}


object Conc {

  /* data types */

  sealed abstract class Leaf[T] extends Conc[T] {
    def left = throw new UnsupportedOperationException
    def right = throw new UnsupportedOperationException
  }
  
  case object Empty extends Leaf[Nothing] {
    def level = 0
    def size = 0
  }
  
  case class Single[T](x: T) extends Leaf[T] {
    def level = 0
    def size = 1
  }
  
  case class Chunk[T](array: Array[T], size: Int, k: Int) extends Leaf[T] {
    def level = 0
  }
  
  /* operations */

  def foreach[T](xs: Conc[T], f: T => Unit): Unit = (xs: @unchecked) match {
    case left <> right =>
      foreach(left, f)
      foreach(right, f)
    case Single(x) =>
      f(x)
    case Chunk(a, sz, _) =>
      var i = 0
      while (i < sz) {
        f(a(i))
        i += 1
      }
    case Empty =>
  }

  def apply[T](xs: Conc[T], i: Int): T = (xs: @unchecked) match {
    case left <> _ if i < left.size =>
      apply(left, i)
    case left <> right =>
      apply(right, i - left.size)
    case Single(x) => x
    case Chunk(a, _, _) => a(i)
  }

  private def updatedArray[T: ClassTag](a: Array[T], i: Int, y: T, sz: Int) = {
    val na = new Array[T](a.length)
    System.arraycopy(a, 0, na, 0, sz)
    na(i) = y
    na
  }

  def update[T: ClassTag](xs: Conc[T], i: Int, y: T): Conc[T] = (xs: @unchecked) match {
    case left <> right if i < left.size =>
      new <>(update(left, i, y), right)
    case left <> right =>
      val ni = i - left.size
      new <>(left, update(right, ni, y))
    case Single(x) =>
      Single(y)
    case Chunk(a: Array[T], sz, k) =>
      Chunk(updatedArray(a, i, y, sz), sz, k)
  }

  def concatTop[T](xs: Conc[T], ys: Conc[T]) = {
    if (xs == Empty) ys
    else if (ys == Empty) xs
    else concat(xs, ys)
  }

  def concat[T](xs: Conc[T], ys: Conc[T]): Conc[T] = {
    val diff = ys.level - xs.level
    if (diff >= -1 && diff <= 1) new <>(xs, ys)
    else if (diff < -1) {
      if (xs.left.level >= xs.right.level) {
        val nr = concat(xs.right, ys)
        new <>(xs.left, nr)
      } else {
        val nl = new <>(xs.left, xs.right.left)
        val nr = concat(xs.right.right, ys)
        new <>(nl, nr)
      }
    } else {
      if (ys.right.level >= ys.left.level) {
        val nl = concat(xs, ys.left)
        new <>(nl, ys.right)
      } else {
        val nl = concat(xs, ys.left.left)
        val nr = new <>(ys.left.right, ys.right)
        new <>(nl, nr)
      }
    }
  }

  private def insertedArray[T: ClassTag](a: Array[T], from: Int, i: Int, y: T, sz: Int) = {
    val na = new Array[T](sz + 1)
    System.arraycopy(a, from, na, 0, i)
    na(i) = y
    System.arraycopy(a, from + i, na, i + 1, sz - i)
    na
  }

  private def copiedArray[T: ClassTag](a: Array[T], from: Int, sz: Int) = {
    val na = new Array[T](sz)
    System.arraycopy(a, from, na, 0, sz)
    na
  }

  def insert[T: ClassTag](xs: Conc[T], i: Int, y: T): Conc[T] = (xs: @unchecked) match {
    case left <> right if i < left.size =>
      insert(left, i, y) <> right
    case left <> right =>
      left <> insert(right, i - left.size, y)
    case Single(x) =>
      if (i == 0) new <>(Single(y), xs)
      else new <>(xs, Single(y))
    case Chunk(a: Array[T], sz, k) if sz == k =>
      if (i < k / 2) {
        val la = insertedArray(a, 0, i, y, k / 2)
        val ra = copiedArray(a, k / 2, k - k / 2)
        new <>(Chunk(la, k / 2 + 1, k), Chunk(ra, k - k / 2, k))
      } else {
        val la = copiedArray(a, 0, k / 2)
        val ra = insertedArray(a, k / 2, i - k / 2, y, k - k / 2 + 1)
        new <>(Chunk(la, k / 2, k), Chunk(ra, k - k / 2 + 1, k))
      }
    case Chunk(a: Array[T], sz, k) =>
      Chunk(insertedArray(a, 0, i, y, sz), sz + 1, k)
  }

}



