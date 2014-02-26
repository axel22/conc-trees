package org.scalablitz



import scala.annotation.unchecked
import scala.annotation.tailrec
import scala.annotation.switch
import scala.reflect.ClassTag



sealed trait Conc[@specialized(Byte, Char, Int, Long, Float, Double) +T] {
  def level: Int
  def size: Int
  def left: Conc[T]
  def right: Conc[T]
  def normalized = this
}


case class <>[+T](left: Conc[T], right: Conc[T]) extends Conc[T] {
  val level = 1 + math.max(left.level, right.level)
  val size = left.size + right.size
}


object Conc {

  sealed trait Leaf[T] extends Conc[T] {
    def left = unsupported("Leaves do not have children.")
    def right = unsupported("Leaves do not have children.")
  }
  
  case object Empty extends Leaf[Nothing] {
    def level = 0
    def size = 0
  }
  
  class Single[@specialized(Byte, Char, Int, Long, Float, Double) T](val x: T) extends Leaf[T] {
    def level = 0
    def size = 1
    override def toString = s"Single($x)"
  }
  
  class Chunk[@specialized(Byte, Char, Int, Long, Float, Double) T](val array: Array[T], val size: Int, val k: Int) extends Leaf[T] {
    def level = 0
    override def toString = s"Chunk(${array.take(5).mkString(", ")}, size, k)"
  }

}


sealed abstract class ConcRope[+T] extends Conc[T]


object ConcRope {

  case class Append[+T](left: Conc[T], right: Conc[T]) extends ConcRope[T] {
    val level = 1 + math.max(left.level, right.level)
    val size = left.size + right.size
    override def normalized = ConcOps.wrap(this, Conc.Empty)
  }
  
}


sealed abstract class Conqueue[+T] extends Conc[T] {
  def evaluated: Boolean
  def tail: Conqueue[T]
  def addIfUnevaluated[U >: T](stack: List[Conqueue.Spine[U]]): List[Conqueue.Spine[U]] = stack
}


object Conqueue {

  def empty = Tip(Zero)

  case class Lazy[+T](lstack: List[Spine[T]], queue: Conqueue[T], rstack: List[Spine[T]]) extends Conqueue[T] {
    def left = queue.left
    def right = queue.right
    def level = queue.level
    def size = queue.size
    def evaluated = unsupported("Undefined for lazy conqueue.")
    def tail = unsupported("Undefined for lazy conqueue.")
    override def normalized = queue.normalized
  }

  class Spine[+T](val lwing: Num[T], val rwing: Num[T], @volatile var evaluateTail: AnyRef) extends Conqueue[T] {
    lazy val tail: Conqueue[T] = {
      val t = (evaluateTail: @unchecked) match {
        case eager: Conqueue[T] => eager
        case suspension: Function0[_] => suspension().asInstanceOf[Conqueue[T]]
      }
      evaluateTail = null
      t
    }
    def evaluated = evaluateTail == null
    override def addIfUnevaluated[U >: T](stack: List[Conqueue.Spine[U]]) = if (!evaluated) this :: stack else stack
    def left = lwing
    def right = new <>(tail, rwing)
    lazy val level: Int = 1 + math.max(lwing.level, math.max(tail.level, rwing.level))
    lazy val size: Int = lwing.size + tail.size + rwing.size
    override def normalized = ConcOps.normalizeLeftWingsAndTip(this, Conc.Empty) <> ConcOps.normalizeRightWings(this, Conc.Empty)
  }

  object Spine {
    def withSameTail[T](s: Spine[T], lwing: Num[T], rwing: Num[T]): Spine[T] = {
      var tail = s.evaluateTail
      if (tail eq null) tail = s.tail
      new Spine(lwing, rwing, tail)
    }
  }

  case class Tip[+T](tip: Num[T]) extends Conqueue[T] {
    def left = tip.left
    def right = tip.right
    def level = tip.level
    def size = tip.size
    def evaluated = true
    def tail = unsupported("Undefined for the tip.")
    override def normalized = tip.normalized
  }

  sealed abstract class Num[+T] extends Conc[T] {
    def leftmost: Conc[T]
    def rightmost: Conc[T]
    def index: Int
  }

  case object Zero extends Num[Nothing] {
    def left = unsupported("Zero does not have children.")
    def right = unsupported("Zero does not have children.")
    def leftmost = unsupported("empty")
    def rightmost = unsupported("empty")
    def level: Int = 0
    def size: Int = 0
    def index = 0
    override def normalized = Conc.Empty
  }

  case class One[T](_1: Conc[T]) extends Num[T] {
    def left = _1
    def right = Conc.Empty
    def leftmost = _1
    def rightmost = _1
    def level: Int = 1 + _1.level
    def size: Int = _1.size
    def index = 1
    override def normalized = _1
  }

  case class Two[T](_1: Conc[T], _2: Conc[T]) extends Num[T] {
    def left = _1
    def right = _2
    def leftmost = _1
    def rightmost = _2
    def level: Int = 1 + math.max(_1.level, _2.level)
    def size: Int = _1.size + _2.size
    def index = 2
    override def normalized = _1 <> _2
  }

  case class Three[T](_1: Conc[T], _2: Conc[T], _3: Conc[T]) extends Num[T] {
    def left = _1
    def right = new <>(_2, _3)
    def leftmost = _1
    def rightmost = _3
    def level: Int = 1 + math.max(math.max(_1.level, _2.level), _3.level)
    def size: Int = _1.size + _2.size + _3.size
    def index = 3
    override def normalized = _1 <> _2 <> _3
  }

  case class Four[T](_1: Conc[T], _2: Conc[T], _3: Conc[T], _4: Conc[T]) extends Num[T] {
    def left = new <>(_1, _2)
    def right = new <>(_3, _4)
    def leftmost = _1
    def rightmost = _4
    def level: Int = 1 + math.max(math.max(_1.level, _2.level), math.max(_3.level, _4.level))
    def size: Int = _1.size + _2.size + _3.size + _4.size
    def index = 4
    override def normalized = _1 <> _2 <> _3 <> _4
  }
}


object ConcOps {

  import Conc._
  import ConcRope._
  import Conqueue._

  private def str[T](num: Num[T]): String = num match {
    case Zero => "Zero"
    case One(_1) => s"One(${_1.level})"
    case Two(_1, _2) => s"Two(${_1.level}, ${_2.level})"
    case Three(_1, _2, _3) => s"Three(${_1.level}, ${_2.level}, ${_3.level})"
    case Four(_1, _2, _3, _4) => s"Four(${_1.level}, ${_2.level}, ${_3.level}, ${_4.level})"
  }

  def queueString[T](conq: Conqueue[T], showNum: Num[T] => String = str _): String = {
    val buffer = new StringBuffer

    def traverse(rank: Int, indent: Int, conq: Conqueue[T]): Unit = (conq: @unchecked) match {
      case s: Spine[T] =>
        val lefts = showNum(s.lwing)
        val rights = showNum(s.rwing)
        val spines = "Spine(+)"
        buffer.append(" " * (indent - lefts.length) + lefts + " " + spines + " " + rights)
        buffer.append("\n")
        traverse(rank + 1, indent, s.tail)
      case Tip(tip) =>
        val tips = s"Tip(${showNum(tip)})"
        buffer.append(" " * (indent) + tips)
    }

    traverse(0, 40, conq)
    buffer.toString
  }

  def wrap[T](xs: Conc[T], ys: Conc[T]): Conc[T] = (xs: @unchecked) match {
    case Append(ws, zs) => wrap(ws, zs <> ys)
    case xs => xs <> ys
  }

  def foreach[@specialized(Byte, Char, Int, Long, Float, Double) T, @specialized(Byte, Char, Int, Long, Float, Double) U](xs: Conc[T], f: T => U): Unit = (xs: @unchecked) match {
    case left <> right =>
      foreach(left, f)
      foreach(right, f)
    case s: Single[T] =>
      f(s.x)
    case c: Chunk[T] =>
      val a = c.array
      val sz = c.size
      var i = 0
      while (i < sz) {
        f(a(i))
        i += 1
      }
    case Empty =>
    case Append(left, right) =>
      foreach(left, f)
      foreach(right, f)
    case Zero =>
    case Tip(Zero) =>
    case conc: Conc[T] =>
      // TODO make more efficient
      foreach(conc.left, f)
      foreach(conc.right, f)
  }

  def apply[@specialized(Byte, Char, Int, Long, Float, Double) T](xs: Conc[T], i: Int): T = (xs: @unchecked) match {
    case left <> _ if i < left.size =>
      apply(left, i)
    case left <> right =>
      apply(right, i - left.size)
    case s: Single[T] => s.x
    case c: Chunk[T] => c.array(i)
    case Append(left, _) if i < left.size =>
      apply(left, i)
    case Append(left, right) =>
      apply(right, i - left.size)
  }

  private def updatedArray[T: ClassTag](a: Array[T], i: Int, y: T, sz: Int): Array[T] = {
    val na = new Array[T](a.length)
    System.arraycopy(a, 0, na, 0, sz)
    na(i) = y
    na
  }

  def update[@specialized(Byte, Char, Int, Long, Float, Double) T: ClassTag](xs: Conc[T], i: Int, y: T): Conc[T] = (xs.normalized: @unchecked) match {
    case left <> right if i < left.size =>
      new <>(update(left, i, y), right)
    case left <> right =>
      val ni = i - left.size
      new <>(left, update(right, ni, y))
    case s: Single[T] =>
      new Single(y)
    case c: Chunk[T] =>
      new Chunk(updatedArray(c.array, i, y, c.size), c.size, c.k)
  }

  def concatTop[T](xs: Conc[T], ys: Conc[T]) = {
    if (xs == Empty) ys
    else if (ys == Empty) xs
    else concat(xs, ys)
  }

  private def concat[T](xs: Conc[T], ys: Conc[T]): Conc[T] = {
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

  private def insertedArray[T: ClassTag](a: Array[T], from: Int, i: Int, y: T, sz: Int): Array[T] = {
    val na = new Array[T](sz + 1)
    System.arraycopy(a, from, na, 0, i)
    na(i) = y
    System.arraycopy(a, from + i, na, i + 1, sz - i)
    na
  }

  private def copiedArray[T: ClassTag](a: Array[T], from: Int, sz: Int): Array[T] = {
    val na = new Array[T](sz)
    System.arraycopy(a, from, na, 0, sz)
    na
  }

  def insert[@specialized(Byte, Char, Int, Long, Float, Double) T: ClassTag](xs: Conc[T], i: Int, y: T): Conc[T] = (xs.normalized: @unchecked) match {
    case left <> right if i < left.size =>
      insert(left, i, y) <> right
    case left <> right =>
      left <> insert(right, i - left.size, y)
    case s: Single[T] =>
      if (i == 0) new <>(new Single(y), xs)
      else new <>(xs, new Single(y))
    case c: Chunk[T] if c.size == c.k =>
      val a = c.array
      val sz = c.size
      val k = c.k
      if (i < k / 2) {
        val la = insertedArray(a, 0, i, y, k / 2)
        val ra = copiedArray(a, k / 2, k - k / 2)
        new <>(new Chunk(la, k / 2 + 1, k), new Chunk(ra, k - k / 2, k))
      } else {
        val la = copiedArray(a, 0, k / 2)
        val ra = insertedArray(a, k / 2, i - k / 2, y, k - k / 2 + 1)
        new <>(new Chunk(la, k / 2, k), new Chunk(ra, k - k / 2 + 1, k))
      }
    case c: Chunk[T] =>
      val a = c.array
      val sz = c.size
      val k = c.k
      new Chunk(insertedArray(a, 0, i, y, sz), sz + 1, k)
    case Empty =>
      new Single(y)
  }

  def appendTop[T](xs: Conc[T], ys: Leaf[T]): Conc[T] = (xs: @unchecked) match {
    case xs: Append[T] => append(xs, ys)
    case _ <> _ => new Append(xs, ys)
    case Empty => ys
    case xs: Leaf[T] => new <>(xs, ys)
  }

  @tailrec private def append[T](xs: Append[T], ys: Conc[T]): Conc[T] = {
    if (xs.right.level > ys.level) new Append(xs, ys)
    else {
      val zs = new <>(xs.right, ys)
      xs.left match {
        case ws @ Append(_, _) => append(ws, zs)
        case ws if ws.level <= zs.level => ws <> zs
        case ws => new Append(ws, zs)
      }
    }
  }

  def shakeLeft[T](xs: Conc[T]): Conc[T] = {
    if (xs.level <= 1) {
      //
      //       1       
      //    +--+--+    
      //    0     0    
      //
      xs
    } else if (xs.left.level >= xs.right.level) {
      //
      //                 n             
      //           +-----+-----+       
      //         n - 1       n - 1     
      //       +---+---+    (n - 2)    
      //     n - 2   n - 2             
      //    (n - 3) (n - 2)            
      //    (n - 2) (n - 3)            
      //
      xs
    } else if (xs.right.right.level >= xs.right.left.level) {
      //
      //            n                              n         
      //      +-----+-----+                  +-----+-----+   
      //    n - 2       n - 1      =>      n - 1       n - 2 
      //              +---+---+          +---+---+    (n - 2)
      //            n - 2   n - 2      n - 2   n - 2         
      //           (n - 3) (n - 2)            (n - 3)        
      //
      val nl = new <>(xs.left, xs.right.left)
      val nr = xs.right.right
      new <>(nl, nr)
    } else if (xs.left.left.level >= xs.left.right.level) {
      //
      //                    n                                      n                
      //          +---------+---------+                  +---------+---------+      
      //        n - 2               n - 1      =>      n - 1               n - 2    
      //      +---+---+           +---+---+          +---+---+           +---+---+  
      //    n - 3   n - 3       n - 2   n - 3      n - 3   n - 2       n - 3   n - 3
      //                      +---+---+                  +---+---+    (n - 4)       
      //                    n - 3   n - 3              n - 3   n - 3  (n - 3)       
      //                   (n - 3) (n - 4)                    (n - 3)               
      //                   (n - 4) (n - 3)                    (n - 4)               
      //
      //  OR:
      //
      //                    n                                      n                
      //          +---------+---------+                  +---------+---------+      
      //        n - 2               n - 1      =>      n - 1               n - 2    
      //      +---+---+           +---+---+          +---+---+           +---+---+  
      //    n - 3   n - 4       n - 2   n - 3      n - 3   n - 2       n - 3   n - 3
      //                      +---+---+                  +---+---+    (n - 4)       
      //                    n - 3   n - 3              n - 4   n - 3                
      //                   (n - 3) (n - 4)                    (n - 3)               
      //
      //  OR:
      //
      //                    n                                    n - 1              
      //          +---------+---------+                  +---------+---------+      
      //        n - 2               n - 1      =>      n - 2               n - 2    
      //      +---+---+           +---+---+          +---+---+           +---+---+  
      //    n - 3   n - 4       n - 2   n - 3      n - 3   n - 3       n - 3   n - 3
      //                      +---+---+                  +---+---+                  
      //                    n - 4   n - 3              n - 4   n - 4                
      //
      val nll = xs.left.left
      val nlr = new <>(xs.left.right, xs.right.left.left)
      val nl = new <>(nll, nlr)
      val nr = new <>(xs.right.left.right, xs.right.right)
      new <>(nl, nr)
    } else if (xs.right.left.left.level >= xs.right.left.right.level) {
      //
      //                    n                                             n                
      //          +---------+---------+                         +---------+---------+      
      //        n - 2               n - 1      =>             n - 1               n - 2    
      //      +---+---+           +---+---+              +------+------+        +---+---+  
      //    n - 4   n - 3       n - 2   n - 3          n - 2         n - 3    n - 3   n - 3
      //                      +---+---+              +---+---+      (n - 3)  (n - 4)       
      //                    n - 3   n - 3          n - 4   n - 3                           
      //                   (n - 3) (n - 4)                                                 
      //
      val nl = new <>(xs.left, xs.right.left.left)
      val nr = new <>(xs.right.left.right, xs.right.right)
      new <>(nl, nr)
    } else {
      //
      //                       n                                                    n - 1                 
      //          +------------+------------+                            +------------+------------+      
      //        n - 2                     n - 1      =>                n - 2                     n - 2    
      //      +---+---+                 +---+---+              +---------+---------+           +---+---+  
      //    n - 4   n - 3             n - 2   n - 3          n - 3               n - 3       n - 3   n - 3
      //          +---+---+         +---+---+              +---+---+           +---+---+                  
      //        n - 4   n - 4     n - 4   n - 3          n - 4   n - 4       n - 4   n - 4                
      //       (n - 4) (n - 5)                                  (n - 4)     (n - 5)                       
      //       (n - 5) (n - 4)                                  (n - 5)     (n - 4)                       
      //
      val nll = new <>(xs.left.left, xs.left.right.left)
      val nlr = new <>(xs.left.right.right, xs.right.left.left)
      val nl = new <>(nll, nlr)
      val nr = new <>(xs.right.left.right, xs.right.right)
      new <>(nl, nr)
    }
  }

  def shakeRight[T](xs: Conc[T]): Conc[T] = {
    if (xs.level <= 1) {
      //
      //       1       
      //    +--+--+    
      //    0     0    
      //
      xs
    } else if (xs.left.level <= xs.right.level) {
      //
      //             n                 
      //       +-----+-----+           
      //     n - 1       n - 1         
      //    (n - 2)    +---+---+       
      //             n - 2   n - 2     
      //            (n - 3) (n - 2)    
      //            (n - 2) (n - 3)    
      //
      xs
    } else if (xs.left.left.level >= xs.left.right.level) {
      //
      //                 n                      n            
      //           +-----+-----+          +-----+-----+      
      //         n - 1       n - 2  =>  n - 2       n - 1    
      //       +---+---+               (n - 2)    +---+---+  
      //     n - 2   n - 2                      n - 2   n - 2
      //    (n - 2) (n - 3)                    (n - 3)       
      //
      val nl = xs.left.left
      val nr = new <>(xs.left.right, xs.right)
      new <>(nl, nr)
    } else if (xs.right.right.level >= xs.right.left.level) {
      //
      //                    n                                      n                
      //          +---------+---------+                  +---------+---------+      
      //        n - 1               n - 2      =>      n - 2               n - 1    
      //      +---+---+           +---+---+          +---+---+           +---+---+  
      //    n - 3   n - 2       n - 3   n - 3      n - 3   n - 3       n - 2   n - 3
      //          +---+---+                               (n - 4)    +---+---+      
      //        n - 3   n - 3                             (n - 3)  n - 3   n - 3    
      //       (n - 4) (n - 3)                                    (n - 3)           
      //       (n - 3) (n - 4)                                    (n - 4)           
      //
      //  OR:
      //
      //                    n                                      n                
      //          +---------+---------+                  +---------+---------+      
      //        n - 1               n - 2      =>      n - 2               n - 1    
      //      +---+---+           +---+---+          +---+---+           +---+---+  
      //    n - 3   n - 2       n - 4   n - 3      n - 3   n - 3       n - 2   n - 3
      //          +---+---+                               (n - 4)    +---+---+      
      //        n - 3   n - 3                                      n - 3   n - 4    
      //       (n - 4) (n - 3)                                    (n - 3)           
      //
      //  OR:
      //
      //                    n                                    n - 1              
      //          +---------+---------+                  +---------+---------+      
      //        n - 1               n - 2      =>      n - 2               n - 2    
      //      +---+---+           +---+---+          +---+---+           +---+---+  
      //    n - 3   n - 2       n - 4   n - 3      n - 3   n - 3       n - 3   n - 3
      //          +---+---+                                          +---+---+      
      //        n - 3   n - 4                                      n - 4   n - 4    
      //
      val nl = new <>(xs.left.left, xs.left.right.left)
      val nrl = new <>(xs.left.right.right, xs.right.left)
      val nrr = xs.right.right
      val nr = new <>(nrl, nrr)
      new <>(nl, nr)
    } else if (xs.left.right.right.level >= xs.left.right.left.level) {
      //
      //                    n                                      n                       
      //          +---------+---------+                  +---------+---------+             
      //        n - 1               n - 2      =>      n - 2               n - 1           
      //      +---+---+           +---+---+          +---+---+        +------+------+      
      //    n - 3   n - 2       n - 3   n - 4      n - 3   n - 3    n - 3         n - 2    
      //          +---+---+                               (n - 4)  (n - 3)      +---+---+  
      //        n - 3   n - 3                                                 n - 3   n - 4
      //       (n - 4) (n - 3)                                                             
      //
      val nl = new <>(xs.left.left, xs.left.right.left)
      val nr = new <>(xs.left.right.right, xs.right)
      new <>(nl, nr)
    } else {
      //
      //                       n                                          n - 1                           
      //          +------------+------------+                  +------------+------------+                
      //        n - 1                     n - 2      =>      n - 2                     n - 2              
      //      +---+---+                 +---+---+          +---+---+           +---------+---------+      
      //    n - 3   n - 2             n - 3   n - 4      n - 3   n - 3       n - 3               n - 3    
      //          +---+---+         +---+---+                              +---+---+           +---+---+  
      //        n - 3   n - 4     n - 4   n - 4                          n - 4   n - 4       n - 4   n - 4
      //                         (n - 5) (n - 4)                                (n - 5)     (n - 4)       
      //                         (n - 4) (n - 5)                                (n - 4)     (n - 5)       
      //
      val nl = new <>(xs.left.left, xs.left.right.left)
      val nrl = new <>(xs.left.right.right, xs.right.left.left)
      val nrr = new <>(xs.right.left.right, xs.right.right)
      val nr = new <>(nrl, nrr)
      new <>(nl, nr)
    }
  }

  def pay[T](work: List[Spine[T]]): List[Spine[T]] = work match {
    case head :: rest =>
      // do 2 units of work
      val tail = head.tail
      if (tail.evaluated) pay(rest)
      else {
        val tailtail = tail.tail
        tailtail.addIfUnevaluated(rest)
      }
    case Nil =>
      // hoorah - nothing to do
      Nil
  }

  val doNothing = () => {}

  def noCarryPushHead[T](num: Num[T], c: Conc[T]): Num[T] = (num.index: @switch) match {
    case 0 =>
      One(c)
    case 1 =>
      val One(_1) = num
      Two(c, _1)
    case 2 =>
      val Two(_1, _2) = num
      Three(c, _1, _2)
    case _ =>
      invalid("Causes a carry.")
  }

  def noCarryPushLast[T](num: Num[T], c: Conc[T]): Num[T] = (num.index: @switch) match {
    case 0 =>
      One(c)
    case 1 =>
      val One(_1) = num
      Two(_1, c)
    case 2 =>
      val Two(_1, _2) = num
      Three(_1, _2, c)
    case _ =>
      invalid("Causes a carry.")
  }

  def noBorrowPopHead[T](num: Num[T]): Num[T] = (num.index: @switch) match {
    case 0 =>
      unsupported("empty")
    case 1 =>
      Zero
    case 2 =>
      val Two(_1, _2) = num
      One(_2)
    case 3 =>
      val Three(_1, _2, _3) = num
      Two(_2, _3)
    case 4 =>
      invalid("Four should never happen.")
  }

  def noBorrowPopLast[T](num: Num[T]): Num[T] = (num.index: @switch) match {
    case 0 =>
      unsupported("empty")
    case 1 =>
      Zero
    case 2 =>
      val Two(_1, _2) = num
      One(_1)
    case 3 =>
      val Three(_1, _2, _3) = num
      Two(_1, _2)
    case 4 =>
      invalid("Four should never happen.")
  }

  def pushHead[T](conq: Conqueue[T], c: Conc[T], onPush: () => Unit): Conqueue[T] = {
    onPush()

    (conq: @unchecked) match {
      case s: Spine[T] =>
        if (s.lwing.index < 3) {
          Spine.withSameTail(s, noCarryPushHead(s.lwing, c), s.rwing)
        } else {
          val Three(_1, _2, _3) = s.lwing
          val nlwing = Two(c, _1)
          val carry = _2 <> _3
          val ntail = (s.tail: @unchecked) match {
            case st: Spine[T] if st.lwing.index == 3 =>
              () => pushHead(s.tail, carry, onPush)
            case _ =>
              pushHead(s.tail, carry, onPush)
          }
          new Spine(nlwing, s.rwing, ntail)
        }
      case Tip(tip) =>
        if (tip.index < 3) {
          Tip(noCarryPushHead(tip, c))
        } else {
          val Three(_1, _2, _3) = tip
          new Spine(Two(c, _1), Two(_2, _3), Tip(Zero))
        }
    }
  }

  def pushHeadTop[T](conq: Conqueue[T], leaf: Leaf[T], onPush: () => Unit = doNothing): Conqueue[T] = conq match {
    case Conqueue.Lazy(lstack, queue, rstack) =>
      val nqueue = pushHead(queue, leaf, onPush)
      val nlstack = pay(nqueue.addIfUnevaluated(lstack))
      val nrstack = pay(rstack)
      Conqueue.Lazy(nlstack, nqueue, nrstack)
    case _ =>
      pushHead(conq, leaf, onPush)
  }

  def popHead[T](conq: Conqueue[T], onFix: () => Unit = doNothing): Conqueue[T] = {
    def fix(s: Spine[T]): Spine[T] = {
      onFix()

      def spreadBorrow(b: Conc[T], otail: Spine[T], nttail: Conqueue[T], continue: Boolean): Spine[T] = {
        val bshaken = shakeRight(b)
        if (bshaken.level == b.level) {
          if (bshaken.left.level == b.level - 1) {
            // regular Two in position n - 1
            val ntlwing = Two(bshaken.left, bshaken.right)
            val ntspine = new Spine(ntlwing, otail.rwing, nttail)
            val ntail = if (continue) ntspine else () => fix(ntspine)
            new Spine(s.lwing, s.rwing, ntail)
          } else {
            // regular One in position n - 1, regular One in position n - 2
            val ntlwing = One(bshaken.right)
            val ntspine = new Spine(ntlwing, otail.rwing, nttail)
            val ntail = if (continue) ntspine else () => fix(ntspine)
            val nlwing = noCarryPushLast(s.lwing, bshaken.left)
            new Spine(nlwing, s.rwing, ntail)
          }
        } else {
          // excited One in position n - 1
          val ntlwing = One(bshaken)
          val ntspine = new Spine(ntlwing, otail.rwing, nttail)
          val ntail = if (continue) ntspine else () => fix(ntspine)
          new Spine(s.lwing, s.rwing, ntail)
        }
      }

      (s.tail: @unchecked) match {
        case st: Spine[T] if st.lwing.index == 0 =>
          (st.tail: @unchecked) match {
            case stt: Spine[T] =>
              val nttlwing = noBorrowPopHead(stt.lwing)
              val nttail = Spine.withSameTail(stt, nttlwing, stt.rwing)
              spreadBorrow(stt.lwing.leftmost, st, nttail, nttlwing.index > 0)
            case Tip(Zero) =>
              new Spine(s.lwing, s.rwing, Tip(st.rwing))
            case Tip(tip) =>
              spreadBorrow(tip.leftmost, st, Tip(noBorrowPopHead(tip)), false)
          }
        case _ =>
          s
      }
    }

    (conq: @unchecked) match {
      case s: Spine[T] =>
        if (s.lwing.index > 1) {
          Spine.withSameTail(s, noBorrowPopHead(s.lwing), s.rwing)
        } else {
          (s.tail: @unchecked) match {
            case st: Spine[T] => // note: s is at rank 0
              val tleftmost = st.lwing.leftmost
              val nlwing = Two(tleftmost.left, tleftmost.right)
              val ntlwing = noBorrowPopHead(st.lwing)
              val ntail = Spine.withSameTail(st, ntlwing, st.rwing)
              val nspine = new Spine(nlwing, s.rwing, ntail)
              if (ntlwing.index > 0) nspine else fix(nspine)
            case Tip(Zero) =>
              Tip(s.rwing)
            case Tip(tip) =>
              val leftmost = tip.leftmost
              val nlwing = Two(leftmost.left, leftmost.right)
              val ntip = Tip(noBorrowPopHead(tip))
              new Spine(nlwing, s.rwing, ntip)
          }
        }
      case Tip(tip) =>
        Tip(noBorrowPopHead(tip))
    }
  }

  def popHeadTop[T](conq: Conqueue[T], onFix: () => Unit = doNothing): Conqueue[T] = conq match {
    case Conqueue.Lazy(lstack, queue, rstack) =>
      val nqueue = popHead(queue, onFix)
      val nlstack = pay(nqueue.addIfUnevaluated(lstack))
      val nrstack = pay(rstack)
      Conqueue.Lazy(nlstack, nqueue, nrstack)
    case _ =>
      popHead(conq, onFix)
  }

  def head[T](conq: Conqueue[T]): Leaf[T] = {
    @tailrec def leftmost(c: Conc[T]): Leaf[T] = c match {
      case Empty => unsupported("empty")
      case l: Leaf[T] => l
      case _ <> _ => leftmost(c.left)
      case _ => invalid("Invalid conqueue state.")
    }

    (conq: @unchecked) match {
      case s: Spine[T] =>
        leftmost(s.lwing.leftmost)
      case Tip(tip) =>
        leftmost(tip.leftmost)
      case Lazy(_, queue, _) =>
        head(queue)
    }
  }

  def pushLast[T](conq: Conqueue[T], c: Conc[T], onPush: () => Unit = doNothing): Conqueue[T] = {
    onPush()

    (conq: @unchecked) match {
      case s: Spine[T] =>
        if (s.rwing.index < 3) {
          Spine.withSameTail(s, s.lwing, noCarryPushLast(s.rwing, c))
        } else {
          val Three(_1, _2, _3) = s.rwing
          val nrwing = Two(_3, c)
          val carry = _1 <> _2
          val ntail = (s.tail: @unchecked) match {
            case st: Spine[T] =>
              () => pushLast(s.tail, carry, onPush)
            case Tip(_) =>
              pushLast(s.tail, carry, onPush)
          }
          new Spine(s.lwing, nrwing, ntail)
        }
      case Tip(tip) =>
        if (tip.index < 3) {
          Tip(noCarryPushLast(tip, c))
        } else {
          val Three(_1, _2, _3) = tip
          new Spine(Two(_1, _2), Two(_3, c), Tip(Zero))
        }
    }
  }

  def pushLastTop[T](conq: Conqueue[T], leaf: Leaf[T], onPush: () => Unit = doNothing): Conqueue[T] = conq match {
    case Conqueue.Lazy(lstack, queue, rstack) =>
      val nqueue = pushLast(queue, leaf, onPush)
      val nlstack = pay(lstack)
      val nrstack = pay(nqueue.addIfUnevaluated(rstack))
      Conqueue.Lazy(nlstack, nqueue, nrstack)
    case _ =>
      pushLast(conq, leaf, onPush)
  }

  def popLast[T](conq: Conqueue[T], onFix: () => Unit = doNothing): Conqueue[T] = {
    def fix(s: Spine[T]): Spine[T] = {
      onFix()

      def spreadBorrow(b: Conc[T], otail: Spine[T], nttail: Conqueue[T], continued: Boolean): Spine[T] = {
        val bshaken = shakeLeft(b)
        if (bshaken.level == b.level) {
          if (bshaken.right.level == b.level - 1) {
            // regular Two in position n - 1
            val ntrwing = Two(bshaken.left, bshaken.right)
            val ntspine = new Spine(otail.lwing, ntrwing, nttail)
            val ntail = if (continued) ntspine else () => fix(ntspine)
            new Spine(s.lwing, s.rwing, ntail)
          } else {
            // regular One in position n - 1, regular One in position n - 2
            val ntrwing = One(bshaken.left)
            val ntspine = new Spine(otail.lwing, ntrwing, nttail)
            val ntail = if (continued) ntspine else () => fix(ntspine)
            val nrwing = noCarryPushHead(s.rwing, bshaken.right)
            new Spine(s.lwing, nrwing, ntail)
          }
        } else {
          // excited One in position n - 1
          val ntrwing = One(bshaken)
          val ntspine = new Spine(otail.lwing, ntrwing, nttail)
          val ntail = if (continued) ntspine else () => fix(ntspine)
          new Spine(s.lwing, s.rwing, ntail)
        }
      }

      (s.tail: @unchecked) match {
        case st: Spine[T] if st.rwing.index == 0 =>
          (st.tail: @unchecked) match {
            case stt: Spine[T] =>
              val nttrwing = noBorrowPopLast(stt.rwing)
              val nttail = Spine.withSameTail(stt, stt.lwing, nttrwing)
              spreadBorrow(stt.rwing.rightmost, st, nttail, nttrwing.index > 0)
            case Tip(Zero) =>
              new Spine(s.lwing, s.rwing, Tip(st.lwing))
            case Tip(tip) =>
              spreadBorrow(tip.rightmost, st, Tip(noBorrowPopLast(tip)), false)
          }
        case _ =>
          s
      }
    }

    (conq: @unchecked) match {
      case s: Spine[T] =>
        if (s.rwing.index > 1) {
          Spine.withSameTail(s, s.lwing, noBorrowPopLast(s.rwing))
        } else {
          (s.tail: @unchecked) match {
            case st: Spine[T] => // note: s is at rank 0
              val trightmost = st.rwing.rightmost
              val nrwing = Two(trightmost.left, trightmost.right)
              val ntrwing = noBorrowPopLast(st.rwing)
              val ntail = Spine.withSameTail(st, st.lwing, ntrwing)
              val nspine = new Spine(s.lwing, nrwing, ntail)
              if (ntrwing.index > 0) nspine else fix(nspine)
            case Tip(Zero) =>
              Tip(s.lwing)
            case Tip(tip) =>
              val rightmost = tip.rightmost
              val nrwing = Two(rightmost.left, rightmost.right)
              val ntip = Tip(noBorrowPopLast(tip))
              new Spine(s.lwing, nrwing, ntip)
          }
        }
      case Tip(tip) =>
        Tip(noBorrowPopLast(tip))
    }
  }

  def popLastTop[T](conq: Conqueue[T], onFix: () => Unit = doNothing): Conqueue[T] = conq match {
    case Conqueue.Lazy(lstack, queue, rstack) =>
      val nqueue = popLast(queue, onFix)
      val nlstack = pay(lstack)
      val nrstack = pay(nqueue.addIfUnevaluated(rstack))
      Conqueue.Lazy(nlstack, nqueue, nrstack)
    case _ =>
      popLast(conq, onFix)
  }

  def last[T](conq: Conqueue[T]): Leaf[T] = {
    @tailrec def rightmost(c: Conc[T]): Leaf[T] = c match {
      case Empty => unsupported("empty")
      case l: Leaf[T] => l
      case _ <> _ => rightmost(c.right)
      case _ => invalid("Invalid conqueue state: " + c.getClass.getSimpleName)
    }

    (conq: @unchecked) match {
      case s: Spine[T] =>
        rightmost(s.rwing.rightmost)
      case Tip(tip) =>
        rightmost(tip.rightmost)
      case Lazy(_, queue, _) =>
        last(queue)
    }
  }

  @tailrec def normalizeLeftWingsAndTip[T](conq: Conqueue[T], front: Conc[T]): Conc[T] = {
    @tailrec def wrapUntil(s: Spine[T], wrapped: Conc[T], level: Int): (Conc[T], Conqueue[T]) = {
      if (wrapped.level >= level) (wrapped, s)
      else {
        val nwrapped = wrapped <> s.lwing.normalized
        (s.tail: @unchecked) match {
          case st: Spine[T] => wrapUntil(st, nwrapped, level)
          case Tip(tip) => (nwrapped, s.tail)
        }
      }
    }

    (conq: @unchecked) match {
      case s: Spine[T] =>
        val (wrapped, remaining) = wrapUntil(s, Conc.Empty, math.max(1, front.level))
        normalizeLeftWingsAndTip(remaining, front <> wrapped)
      case Tip(tip) =>
        front <> tip.normalized
    }
  }

  @tailrec def normalizeRightWings[T](conq: Conqueue[T], back: Conc[T]): Conc[T] = {
    @tailrec def wrapUntil(s: Spine[T], wrapped: Conc[T], level: Int): (Conc[T], Conqueue[T]) = {
      if (wrapped.level >= level) (wrapped, s)
      else {
        val nwrapped = s.rwing.normalized <> wrapped
        (s.tail: @unchecked) match {
          case st: Spine[T] => wrapUntil(st, nwrapped, level)
          case Tip(tip) => (nwrapped, Tip(Zero))
        }
      }
    }

    (conq: @unchecked) match {
      case s: Spine[T] =>
        val (wrapped, remaining) = wrapUntil(s, Conc.Empty, math.max(1, back.level))
        normalizeRightWings(remaining, wrapped <> back)
      case Tip(tip) =>
        back
    }
  }

}









