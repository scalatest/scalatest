package org.scalatest.examples.suite.ignoreall

import org.scalatest._

@Ignore 
class SetSuite extends Suite {

  def `test: an empty Set should have size 0` {
    assert(Set.empty.size === 0)
  }

  def `test: invoking head on an empty Set should produce NoSuchElementException` {
    intercept[NoSuchElementException] {
      Set.empty.head
    }
  }
}
