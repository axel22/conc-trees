package org.scalablitz



import scala.annotation.unchecked
import scala.reflect.ClassTag



sealed abstract class Conc[+T] {
  def level: Int
  def size: Int
  def left: Conc[T]
  def right: Conc[T]
  def normalized = this
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

  case class Append[+T](left: Conc[T], right: Conc[T]) extends Conc[T] {
    val level = 1 + math.max(left.level, right.level)
    val size = left.size + right.size
    override def normalized = wrap(this, Empty)
  }
  
  /* operations */

  def foreach[T, U](xs: Conc[T], f: T => U): Unit = (xs: @unchecked) match {
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
    case Append(left, right) =>
      foreach(left, f)
      foreach(right, f)
  }

  def apply[T](xs: Conc[T], i: Int): T = (xs: @unchecked) match {
    case left <> _ if i < left.size =>
      apply(left, i)
    case left <> right =>
      apply(right, i - left.size)
    case Single(x) => x
    case Chunk(a, _, _) => a(i)
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

  def update[T: ClassTag](xs: Conc[T], i: Int, y: T): Conc[T] = (xs.normalized: @unchecked) match {
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

  def insert[T: ClassTag](xs: Conc[T], i: Int, y: T): Conc[T] = (xs.normalized: @unchecked) match {
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
    case Empty =>
      Single(y)
  }

  def appendTop[T](xs: Conc[T], ys: Leaf[T]): Conc[T] = (xs: @unchecked) match {
    case Empty => ys
    case xs: Leaf[T] => new <>(xs, ys)
    case _ <> _ => new Append(xs, ys)
    case xs: Append[T] => append(xs, ys)
  }

  private def append[T](xs: Append[T], ys: Conc[T]): Conc[T] = {
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

  def wrap[T](xs: Conc[T], ys: Conc[T]): Conc[T] = (xs: @unchecked) match {
    case Append(ws, zs) => wrap(ws, zs <> ys)
    case xs => xs <> ys
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

}






