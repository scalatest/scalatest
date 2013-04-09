package org.scalatest.examples.suite.ignore

import org.scalatest._

class SetSuite extends Suite {

  @Ignore def `test: an empty Set should have size 0` {
    assert(Set.empty.size === 0)
  }

  def `test: invoking head on an empty Set should produce NoSuchElementException` {
    intercept[NoSuchElementException] {
      Set.empty.head
    }
  }
}
