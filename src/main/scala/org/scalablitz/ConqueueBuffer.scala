package org.scalablitz



import scala.reflect.ClassTag



class ConqueueBuffer[@specialized(Byte, Char, Int, Long, Float, Double) T: ClassTag](isLazy: Boolean = true) {
  import Conc._
  import Conqueue._

  private var conqueue: Conqueue[T] = if (isLazy) Lazy(Nil, Conqueue.empty, Nil) else Conqueue.empty

  def size = conqueue.size

  def isEmpty = ConcOps.isEmptyConqueue(conqueue)

  def nonEmpty = !isEmpty

  def head = ConcOps.head(conqueue).asInstanceOf[Single[T]].x

  def last = ConcOps.last(conqueue).asInstanceOf[Single[T]].x

  def pushHead(elem: T): this.type = {
    conqueue = ConcOps.pushHeadTop(conqueue, new Single(elem))
    this
  }

  def popHead(): T = {
    val head = ConcOps.head(conqueue)
    conqueue = ConcOps.popHeadTop(conqueue)
    head.asInstanceOf[Single[T]].x
  }

  def pushLast(elem: T): this.type = {
    conqueue = ConcOps.pushLastTop(conqueue, new Single(elem))
    this
  }

  def popLast(): T = {
    val last = ConcOps.last(conqueue)
    conqueue = ConcOps.popLastTop(conqueue)
    last.asInstanceOf[Single[T]].x
  }

  def compressSmall() {
    if (size < 4) {
      var nconqueue = Conqueue.empty[T]
      while (this.nonEmpty) {
        val last = ConcOps.last(conqueue)
        nconqueue = ConcOps.pushHead(nconqueue, last)
        conqueue = ConcOps.popLast(conqueue)
      }
      conqueue = nconqueue
    }
  }

  def toConqueue = conqueue

}









