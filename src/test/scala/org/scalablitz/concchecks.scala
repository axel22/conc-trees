package org.scalablitz



import scala.collection._
import org.scalacheck._
import org.scalacheck.Prop._
import org.scalacheck.Gen._
import Conc._



object ConcListChecks extends Properties("ConcList") with ConcListSnippets {

  property("<> correctness") = forAll(choose(0, 500)) {
    testConcatCorrectness
  }

  property("<> balance") = forAll(choose(0, 500)) {
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

}

