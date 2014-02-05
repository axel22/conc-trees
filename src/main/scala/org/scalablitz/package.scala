package org



import scala.reflect.ClassTag



package object scalablitz {

  implicit class ConcOps[T](val self: Conc[T]) extends AnyVal {
    def apply(i: Int) = {
      require(i >= 0 && i < self.size)
      Conc.apply(self, i)
    }
    def foreach(f: T => Unit) = Conc.foreach(self, f)
    def <>(that: Conc[T]) = Conc.concatTop(self, that)
  }

  implicit class ConcModificationOps[T: ClassTag](val self: Conc[T]) {
    def update(i: Int, y: T) = {
      require(i >= 0 && i < self.size)
      Conc.update(self, i, y)
    }
    def insert(i: Int, y: T) = {
      require(i >= 0 && i <= self.size)
      Conc.insert(self, i, y)
    }
  }
}
