package org.scalablitz



import scala.collection._
import Conc._



trait ConcListSnippets {

  def concList[T](elems: Seq[T]): Conc[T] = {
    var xs: Conc[T] = Empty
    for (x <- elems) {
      xs <>= Single(x)
    }
    xs
  }

  def toSeq[T](xs: Conc[T]): Seq[T] = {
    val buffer = mutable.Buffer[T]()
    for (x <- xs) {
      buffer += x
    }
    buffer
  }

  def testConcatCorrectness(n: Int) = {
    var xs: Conc[Int] = concList(0 until n)

    toSeq(xs) == (0 until n)
  }

  def testConcatBalance(n: Int) = {
    var xs: Conc[Int] = concList(0 until n)

    def check(xs: Conc[Int]): Boolean = xs match {
      case left <> right =>
        math.abs(left.level - right.level) <= 1 && check(left) && check(right)
      case _ =>
        true
    }

    check(xs)
  }

  def testApply(n: Int) = {
    var xs: Conc[Int] = concList(0 until n)
    val checks = for (i <- 0 until n) yield i == xs(i)

    checks.forall(_ == true)
  }

  def testUpdate(n: Int) = {
    var xs: Conc[Int] = concList(0 until n)

    for (i <- 0 until n) xs = xs.update(i, -i)

    val checks = for (i <- 0 until n) yield -i == xs(i)

    checks.forall(_ == true)
  }

  def compare(sample: Int, xs: Conc[Int], ys: Seq[Int]) {
    val xsb = mutable.Buffer[Int]()
    for (x <- xs) xsb += x
    println("----------")
    println(sample)
    println(ys)
    println(xsb)
  }

  def testInsert(size: Int, samples: Int, seed: Int) = {
    var xs: Conc[Int] = concList(0 until size)
    val buffer = mutable.Buffer[Int]() ++ (0 until size)

    for (i <- 0 until samples) {
      val sample = (seed + i * 179) % (xs.size + 1)
      xs = xs.insert(sample, -sample - 1)
      buffer.insert(sample, -sample - 1)
    }

    buffer == toSeq(xs)
  }

}
