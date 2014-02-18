package org.scalablitz



import org.scalatest._
import Conc._



class ConcSuite extends FunSuite with ConcSnippets {

  test("ConcList.apply") {
    var xs: Conc[Int] = Empty

    intercept[IllegalArgumentException] {
      xs(0)
    }
  }

}
