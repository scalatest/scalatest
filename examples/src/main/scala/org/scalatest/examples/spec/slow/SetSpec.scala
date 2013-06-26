package org.scalatest.examples.spec.slow

import org.scalatest._
import tags.Slow

class SetSpec extends Spec {

  @Slow def `an empty Set should have size 0` {
    assert(Set.empty.size === 0)
  }

  def `invoking head on an empty Set should produce NoSuchElementException` {
    intercept[NoSuchElementException] {
      Set.empty.head
    }
  }
}
