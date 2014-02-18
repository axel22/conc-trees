package org.scalablitz



import scala.collection._
import Conc._
import org.scalameter.api._



class ConcBenches extends PerformanceTest.Regression with ConcSnippets with Serializable {

  def persistor = Persistor.None

  val concs = for {
    size <- Gen.range("size")(30000, 150000, 30000)
  } yield concList(0 until size)

  performance of "foreach" in {
    using(concs) curve("Conc") in { c =>
      c.foreach(x => {})
    }
  }

}
