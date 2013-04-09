package org.scalatest.examples.funsuite.ignore

import org.scalatest.FunSuite

class SetSuite extends FunSuite {

  ignore("An empty Set should have size 0") {
    assert(Set.empty.size === 0)
  }

  test("Invoking head on an empty Set should produce NoSuchElementException") {
    intercept[NoSuchElementException] {
      Set.empty.head
    }
  }
}
