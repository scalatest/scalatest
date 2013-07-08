package org.scalatest.examples.spec.cpu

import org.scalatest._
import tags.CPU

class SetSpec extends Spec {

  @CPU def `an empty Set should have size 0` {
    assert(Set.empty.size === 0)
  }

  def `invoking head on an empty Set should produce NoSuchElementException` {
    intercept[NoSuchElementException] {
      Set.empty.head
    }
  }
}
