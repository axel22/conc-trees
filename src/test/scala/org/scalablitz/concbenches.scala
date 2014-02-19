package org.scalablitz



import scala.collection._
import Conc._
import org.scalameter.api._



class ConcBenches extends PerformanceTest.OfflineReport {

  override def historian = org.scalameter.reporting.RegressionReporter.Historian.Complete()

  def sizes(from: Int, until: Int) = Gen.range("size")(from, until, (until - from) / 4)

  def concs(from: Int, until: Int) = for {
    size <- sizes(from, until)
  } yield {
    var xs: Conc[Int] = Empty
    for (x <- 0 until size) xs <>= new Single(x)
    xs
  }

  def ropes(k: Int, from: Int, until: Int) = for {
    size <- sizes(from, until)
  } yield {
    val xs = new Conc.Buffer[Int](k)
    for (x <- 0 until size) xs += x
    xs.extractConc()
  }

  def lists(from: Int, until: Int) = for {
    size <- sizes(from, until)
  } yield (0 until size).toList

  def vectors(from: Int, until: Int) = for {
    size <- sizes(from, until)
  } yield (0 until size).toVector

  val opts = Context(
    exec.minWarmupRuns -> 60,
    exec.maxWarmupRuns -> 120,
    exec.independentSamples -> 3
  )

  performance of "foreach" config(opts) in {
    using(concs(30000, 150000)) curve("ConcList") in { conc =>
      conc.foreach(x => {})
    }

    using(lists(30000, 150000)) curve("List") in { list =>
      list.foreach(x => {})
    }

    using(vectors(300000, 1500000)) curve("Vector") in { vector =>
      vector.foreach(x => {})
    }

    using(ropes(128, 300000, 1500000)) curve("Conc.Buffer(128)") in { rope =>
      rope.foreach(x => {})
    }
  }

  performance of "append" config(opts) in {
    using(sizes(30000, 150000)) curve("ConcList") in { sz =>
      var xs: Conc[String] = Empty
      var i = 0
      while (i < sz) {
        xs = xs :+ ""
        i += 1
      }
      xs
    }

    using(sizes(30000, 150000)) curve("List") in { sz =>
      var xs: List[String] = Nil
      var i = 0
      while (i < sz) {
        xs = "" :: xs
        i += 1
      }
      xs
    }

    using(sizes(30000, 150000)) curve("Vector") in { sz =>
      var xs: Vector[String] = Vector()
      var i = 0
      while (i < sz) {
        xs = xs :+ ""
        i += 1
      }
      xs
    }

    using(sizes(300000, 1500000)) curve("Conc.Buffer(128)") in { sz =>
      val xs = new Conc.Buffer[Int](128)
      var i = 0
      while (i < sz) {
        xs += i
        i += 1
      }
      xs
    }

    using(sizes(300000, 1500000)) curve("VectorBuilder") in { sz =>
      val xs = new collection.immutable.VectorBuilder[String]()
      var i = 0
      while (i < sz) {
        xs += ""
        i += 1
      }
      xs
    }

  }

  performance of "concat" config(opts) in {
    using(concs(300000, 1500000) zip concs(3000, 30000).rename("size" -> "thatSize")) curve("ConcList") in { case (conc, thatConc) =>
      var i = 0
      while (i < 10000) {
        conc <> thatConc
        i += 1
      }
    }

    using(ropes(128, 300000, 1500000) zip concs(3000, 30000).rename("size" -> "thatSize")) curve("Conc.Buffer(128)") in { case (rope, thatRope) =>
      var i = 0
      while (i < 10000) {
        rope <> thatRope
        i += 1
      }
    }

    using(vectors(300, 1500) zip vectors(30, 150).rename("size" -> "thatSize")) curve("Vector") config(
      exec.minWarmupRuns -> 1,
      exec.maxWarmupRuns -> 1,
      exec.benchRuns -> 2,
      exec.independentSamples -> 1
    ) in { case (vector, thatVector) =>
      var i = 0
      while (i < 10000) {
        vector ++ thatVector
        i += 1
      }
    }
  }

}




