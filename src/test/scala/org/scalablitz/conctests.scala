package org.scalablitz



import org.scalatest._
import Conc._



class ConcListSuite extends FunSuite with ConcListSnippets {

  test("ConcList.apply") {
    var xs: Conc[Int] = Empty

    intercept[IllegalArgumentException] {
      xs(0)
    }
  }

}
