package org.scalatest.examples.funsuite.ignoreall

import org.scalatest.FunSuite
import org.scalatest.Ignore

@Ignore
class SetSuite extends FunSuite {

  test("An empty Set should have size 0") {
    assert(Set.empty.size === 0)
  }

  test("Invoking head on an empty Set should produce NoSuchElementException") {
    intercept[NoSuchElementException] {
      Set.empty.head
    }
  }
}
