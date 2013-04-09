package org.scalatest.examples.funsuite.pending

import org.scalatest._

class SetSuite extends FunSuite {

  test("An empty Set should have size 0") (pending)

  test("Invoking head on an empty Set should produce NoSuchElementException") {
    intercept[NoSuchElementException] {
      Set.empty.head
    }
  }
}
