package org.scalatest.examples.suite.pending

import org.scalatest._

class SetSuite extends Suite {

  def `test: an empty Set should have size 0` { pending }

  def `test: invoking head on an empty Set should produce NoSuchElementException` {
    intercept[NoSuchElementException] {
      Set.empty.head
    }
  }
}
