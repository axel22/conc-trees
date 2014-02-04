package org






package object scalablitz {

  implicit class ConcOps[T](val self: Conc[T]) extends AnyVal {
    def <>[T](that: Conc[T]) = Conc.concat(self, that)
  }

}
