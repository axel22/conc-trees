package org.scalablitz



import scala.collection._
import org.scalacheck._
import org.scalacheck.Prop._
import org.scalacheck.Gen._
import Conc._
import ConcRope._
import Conqueue._



object ConcChecks extends Properties("Conc") with ConcSnippets {

  /* conc tree */

  val genLeaf = for (n <- choose(0, 500)) yield new Conc.Single(n)

  def genTree(level: Int): Gen[Conc[Int]] = if (level <= 0) genLeaf else for {
    tp <- oneOf(0, 1, 2)
    left <- if (tp == 0) genTree(level - 2) else genTree(level - 1)
    right <- if (tp == 2) genTree(level - 2) else genTree(level - 1)
  } yield new <>(left, right)

  def trees(maxlevel: Int) = for {
    level <- choose(0, maxlevel + 1)
    tree <- genTree(level)
  } yield tree

  property("<> correctness") = forAll(choose(0, 500), choose(0, 500)) {
    testConcatCorrectness
  }

  property("<> balance") = forAll(choose(0, 500), choose(0, 500)) {
    testConcatBalance
  }

  property("apply correctness") = forAll(choose(1, 500)) {
    testApply
  }

  property("update correctness") = forAll(choose(1, 500)) {
    testUpdate
  }

  property("insert correctness") = forAll(choose(0, 500), choose(0, 20), choose(0, 500)) {
    testInsert
  }

  property("generated trees") = forAll(trees(10)) { tree =>
    s"invariants: $tree" |: checkInvs(tree)
  }

  property("left shake") = forAll(trees(10)) { tree =>
    val shaken = ConcOps.shakeLeft(tree)
    all(
      s"invariants: $shaken" |: checkInvs(shaken),
      s"leaning left: $shaken" |: (shaken.level <= 1 || shaken.level < tree.level || shaken.left.level >= shaken.right.level)
    )
  }

  property("right shake") = forAll(trees(10)) { tree =>
    val shaken = ConcOps.shakeRight(tree)
    all(
      s"invariants: $shaken" |: checkInvs(shaken),
      s"leaning right: $shaken" |: (shaken.level <= 1 || shaken.level < tree.level || shaken.left.level <= shaken.right.level)
    )
  }

  /* conc rope */

  property("append correctness") = forAll(choose(1, 1000), choose(1, 5000)) {
    testAppendCorrectness
  }

  property("append balance") = forAll(choose(1, 1000), choose(1, 5000)) {
    testAppendBalance
  }

  /* conqueue */

  def genSequence[T](length: Int, g: Gen[T]): Gen[Seq[T]] = for {
    head <- g
    tail <- if (length <= 1) oneOf(Nil, Nil) else genSequence(length - 1, g)
  } yield head +: tail

  def genNum(num: Int, rank: Int) = for {
    xs <- genSequence(num, genTree(rank))
  } yield xs.length  match {
    case 0 => Zero
    case 1 => One(xs(0))
    case 2 => Two(xs(0), xs(1))
    case 3 => Three(xs(0), xs(1), xs(2))
    case 4 => Four(xs(0), xs(1), xs(2), xs(3))
  }

  def genTip(rank: Int) = for {
    num <- oneOf(2, 3)
    xs <- genNum(num, rank)
  } yield Tip(xs)

  def genSpine(rank: Int, maxRank: Int): Gen[Spine[Int]] = for {
    leftNum <- oneOf(2, 3)
    rightNum <- oneOf(2, 3)
    leftWing <- genNum(leftNum, rank)
    rightWing <- genNum(rightNum, rank)
    tail <- genConqueue(rank + 1, maxRank)
  } yield new Spine(leftWing, rightWing, () => tail)

  def genConqueue(rank: Int, maxRank: Int) = for {
    conqueue <- if (rank == maxRank) genTip(rank) else genSpine(rank, maxRank)
  } yield conqueue

  def queues(rankLimit: Int) = for {
    maxRank <- choose(0, rankLimit)
    conqueue <- genConqueue(0, maxRank)
  } yield conqueue

  def lazyQueues(rankLimit: Int) = for {
    queue <- queues(rankLimit)
  } yield Conqueue.Lazy(Nil, queue, Nil)

  property("conqueue invariants") = forAll(queues(5)) { conq =>
    checkConqueueInvs(conq, 0)
  }

  property("head correctness") = forAll(queues(5)) { conq =>
    val buffer = mutable.Buffer[Int]()
    for (x <- conq) buffer += x
    buffer.head == ConcOps.head(conq).asInstanceOf[Single[Int]].x
  }

  property("last correctness") = forAll(queues(5)) { conq =>
    val buffer = mutable.Buffer[Int]()
    for (x <- conq) buffer += x
    s"${ConcOps.queueString(conq)}\n: ${buffer.last} vs ${ConcOps.last(conq)}" |: buffer.last == ConcOps.last(conq).asInstanceOf[Single[Int]].x
  }

  property("conqueue pushHeadTop") = forAll(queues(9)) { conq =>
    val pushed = ConcOps.pushHeadTop(conq, new Single(-1))
    //println(ConcOps.queueString(conq))
    //println("after:")
    //println(ConcOps.queueString(pushed))
    //println("--------------")
    all(
      s"Head is the value just pushed." |: ConcOps.head(pushed).asInstanceOf[Single[Int]].x == -1,
      s"Invariants are met." |: checkConqueueInvs(pushed, 0),
      s"Correctly prepended." |: toSeq(pushed) == (-1 +: toSeq(conq))
    )
  }

  property("conqueue pushHeadTop many times") = forAll(queues(9), choose(1, 10000)) { (conq, n) =>
    var pushed = conq
    for (i <- 0 until n) {
      var units = 0
      pushed = ConcOps.pushHeadTop(pushed, new Single(-i), () => units += 1)
      //println("Work done: " + units)
    }
    //println("n = " + n)
    //println(ConcOps.queueString(conq))
    //println("after:")
    //println(ConcOps.queueString(pushed))
    //println("--------------")
    all(
      s"Invariants are met." |: checkConqueueInvs(pushed, 0),
      s"Correctly prepended." |: toSeq(pushed) == ((0 until n).map(-_).reverse ++ toSeq(conq))
    )
  }

  property("conqueue pushLastTop") = forAll(queues(9)) { conq =>
    val pushed = ConcOps.pushLastTop(conq, new Single(-1))
    //println(ConcOps.queueString(conq))
    //println("after:")
    //println(ConcOps.queueString(pushed))
    //println("--------------")
    all(
      s"Last is the value just pushed." |: ConcOps.last(pushed).asInstanceOf[Single[Int]].x == -1,
      s"Invariants are met." |: checkConqueueInvs(pushed, 0),
      s"Correctly appended." |: toSeq(pushed) == (toSeq(conq) :+ -1)
    )
  }

  property("conqueue pushLastTop many times") = forAll(queues(9), choose(1, 10000)) { (conq, n) =>
    var pushed = conq
    for (i <- 0 until n) {
      var units = 0
      pushed = ConcOps.pushLastTop(pushed, new Single(-i), () => units += 1)
      //println("Work done: " + units)
    }
    //println("n = " + n)
    //println(ConcOps.queueString(conq))
    //println("after:")
    //println(ConcOps.queueString(pushed))
    //println("--------------")
    all(
      s"Invariants are met." |: checkConqueueInvs(pushed, 0),
      s"Correctly appended." |: toSeq(pushed) == (toSeq(conq) ++ (0 until n).map(-_))
    )
  }

  property("lazy conqueue pushHeadTop constant work") = forAll(lazyQueues(9), choose(1, 10000)) { (lazyq, n) =>
    var pushed: Conqueue[Int] = lazyq
    val workHistory = for (i <- 0 until n) yield {
      var units = 0
      pushed = ConcOps.pushHeadTop(pushed, new Single(-i), () => units += 1)
      units
    }
    val mostWork = workHistory.max
    all(
      s"Most work ever done <= 4: $mostWork" |: mostWork <= 4,
      s"Invariants are met." |: checkConqueueInvs(pushed, 0),
      s"Correctly prepended." |: toSeq(pushed) == ((0 until n).map(-_).reverse ++ toSeq(lazyq))
    )
  }

  property("lazy conqueue pushLastTop constant work") = forAll(lazyQueues(9), choose(1, 10000)) { (lazyq, n) =>
    var pushed: Conqueue[Int] = lazyq
    val workHistory = for (i <- 0 until n) yield {
      var units = 0
      pushed = ConcOps.pushLastTop(pushed, new Single(-i), () => units += 1)
      units
    }
    val mostWork = workHistory.max
    all(
      s"Most work ever done <= 4: $mostWork" |: mostWork <= 4,
      s"Invariants are met." |: checkConqueueInvs(pushed, 0),
      s"Correctly appended." |: toSeq(pushed) == (toSeq(lazyq) ++ (0 until n).map(-_))
    )
  }

  property("lazy conqueue alternating pushHeadTop/pushLastTop constant work") = forAll(lazyQueues(9), choose(1, 10000), choose(1, 1000)) { (lazyq, n, seed) =>
    val random = new scala.util.Random(seed)
    var buffer = toSeq(lazyq)
    var pushed: Conqueue[Int] = lazyq
    val workHistory = for (i <- 0 until n) yield {
      var units = 0
      if (random.nextBoolean()) {
        pushed = ConcOps.pushHeadTop(pushed, new Single(-i), () => units += 1)
        buffer = -i +: buffer
      } else {
        pushed = ConcOps.pushLastTop(pushed, new Single(-i), () => units += 1)
        buffer = buffer :+ -i
      }
      units
    }
    val mostWork = workHistory.max
    all(
      s"Most work ever done <= 4: $mostWork" |: mostWork <= 4,
      s"Invariants are met." |: checkConqueueInvs(pushed, 0),
      s"Correctly appended." |: toSeq(pushed) == buffer
    )
  }

  property("conqueue popHeadTop") = forAll(queues(9)) { conq =>
    var popped = conq
    var list: List[Int] = toSeq(conq).toList
    val buffer = mutable.Buffer[Int]()
    while (list.nonEmpty) {
      list = list.tail
      //println(ConcOps.queueString(popped))
      //println("-------------------------")
      buffer += ConcOps.head(popped).asInstanceOf[Single[Int]].x
      popped = ConcOps.popHeadTop(popped)
      checkConqueueInvs(popped, 0)
    }
    //println(ConcOps.queueString(popped))
    all(
      s"Invariants are met." |: checkConqueueInvs(popped, 0),
      s"Correctly popped." |: toSeq(conq) == buffer,
      s"Conqueue is empty." |: popped == Tip(Zero)
    )
  }

  property("lazy conqueue popHeadTop constant work") = forAll(lazyQueues(12)) { conq =>
    var popped: Conqueue[Int] = conq
    var list: List[Int] = toSeq(conq).toList
    val buffer = mutable.Buffer[Int]()
    val workHistory = mutable.Buffer[Int]()
    while (list.nonEmpty) {
      var units = 0
      list = list.tail
      buffer += ConcOps.head(popped).asInstanceOf[Single[Int]].x
      popped = ConcOps.popHeadTop(popped, () => units += 1)
      workHistory += units
    }
    val mostWork = workHistory.max
    all(
      s"Invariants are met." |: checkConqueueInvs(popped, 0),
      s"Correctly popped." |: toSeq(conq) == buffer,
      s"Conqueue is empty." |: popped == Lazy(Nil, Tip(Zero), Nil),
      s"Most work ever done <= 4: $mostWork in $workHistory" |: mostWork <= 4
    )
  }

  property("conqueue popLastTop") = forAll(queues(9)) { conq =>
    var popped = conq
    var list: List[Int] = toSeq(conq).toList
    val buffer = mutable.Buffer[Int]()
    while (list.nonEmpty) {
      list = list.tail
      //println(ConcOps.queueString(popped))
      //println("-------------------------")
      buffer += ConcOps.last(popped).asInstanceOf[Single[Int]].x
      popped = ConcOps.popLastTop(popped)
      checkConqueueInvs(popped, 0)
    }
    //println(ConcOps.queueString(popped))
    all(
      s"Invariants are met." |: checkConqueueInvs(popped, 0),
      s"Correctly popped." |: toSeq(conq).reverse == buffer,
      s"Conqueue is empty." |: popped == Tip(Zero)
    )
  }

  property("lazy conqueue popHeadTop constant work") = forAll(lazyQueues(12)) { conq =>
    var popped: Conqueue[Int] = conq
    var list: List[Int] = toSeq(conq).toList
    val buffer = mutable.Buffer[Int]()
    val workHistory = mutable.Buffer[Int]()
    while (list.nonEmpty) {
      var units = 0
      list = list.tail
      buffer += ConcOps.last(popped).asInstanceOf[Single[Int]].x
      popped = ConcOps.popLastTop(popped, () => units += 1)
      workHistory += units
    }
    val mostWork = workHistory.max
    all(
      s"Invariants are met." |: checkConqueueInvs(popped, 0),
      s"Correctly popped." |: toSeq(conq) == buffer.reverse,
      s"Conqueue is empty." |: popped == Lazy(Nil, Tip(Zero), Nil),
      s"Most work ever done <= 4: $mostWork in $workHistory" |: mostWork <= 4
    )
  }

}






