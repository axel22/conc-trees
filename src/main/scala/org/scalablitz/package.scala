package org



import scala.reflect.ClassTag



package object scalablitz {

  def invalid(msg: String) = throw new IllegalStateException(msg)

  def unsupported(msg: String) = throw new UnsupportedOperationException(msg)

  implicit class ConcApi[T](val self: Conc[T]) extends AnyVal {
    def apply(i: Int) = {
      require(i >= 0 && i < self.size)
      ConcOps.apply(self, i)
    }
    def foreach[U](f: T => U) = ConcOps.foreach(self, f)
    def <>(that: Conc[T]) = ConcOps.concatTop(self, that)
  }

  implicit class ConcModificationApi[T: ClassTag](val self: Conc[T]) {
    def update(i: Int, y: T) = {
      require(i >= 0 && i < self.size)
      ConcOps.update(self, i, y)
    }
    def insert(i: Int, y: T) = {
      require(i >= 0 && i <= self.size)
      ConcOps.insert(self, i, y)
    }
    def :+(y: T) = ConcOps.appendTop(self, new Conc.Single(y))
  }
}
