package org.scalablitz



import scala.collection._
import org.scalacheck._
import org.scalacheck.Prop._
import org.scalacheck.Gen._
import Conc._



object ConcListChecks extends Properties("ConcList") with ConcListSnippets {

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

  property("test generated trees") = forAll(trees(10)) { tree =>
    s"invariants: $tree" |: checkInvs(tree)
  }

  property("test left shake") = forAll(trees(10)) { tree =>
    val shaken = Conc.shakeLeft(tree)
    all(
      s"invariants: $shaken" |: checkInvs(shaken),
      s"leaning left: $shaken" |: (shaken.level <= 1 || shaken.level < tree.level || shaken.left.level >= shaken.right.level)
    )
  }

}

