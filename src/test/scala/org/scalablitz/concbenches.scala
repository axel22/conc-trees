package org.scalablitz



import scala.collection._
import Conc._
import org.scalameter.api._



class ConcBenches extends PerformanceTest.Regression with Serializable {

  def persistor = Persistor.None

  def sizes(from: Int, until: Int) = Gen.range("size")(from, until, (until - from) / 5)

  def concs(from: Int, until: Int) = for {
    size <- sizes(from, until)
  } yield {
    var xs: Conc[Int] = Empty
    for (x <- 0 until size) xs <>= Single(x)
    xs
  }

  def ropes(k: Int, from: Int, until: Int) = for {
    size <- sizes(from, until)
  } yield {
    val xs = new Conc.Buffer[Int](k)
    for (x <- 0 until size) xs += x
    xs.toConc
  }

  def lists(from: Int, until: Int) = for {
    size <- sizes(from, until)
  } yield (0 until size).toList

  def vectors(from: Int, until: Int) = for {
    size <- sizes(from, until)
  } yield (0 until size).toVector

  val opts = Context(
    exec.independentSamples -> 1
  )

  performance of "foreach" config(opts) in {
    using(concs(30000, 150000)) curve("ConcList") in { conc =>
      conc.foreach(x => {})
    }

    using(ropes(50, 30000, 150000)) curve("ConcRope") in { rope =>
      rope.foreach(x => {})
    }

    using(lists(30000, 150000)) curve("List") in { list =>
      list.foreach(x => {})
    }

    using(vectors(30000, 150000)) curve("Vector") in { vector =>
      vector.foreach(x => {})
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
  }

}
