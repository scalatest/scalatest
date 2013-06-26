package org.scalatest.examples.spec.network

import org.scalatest._
import tags.Network

class SetSpec extends Spec {

  @Network def `an empty Set should have size 0` {
    assert(Set.empty.size === 0)
  }

  def `invoking head on an empty Set should produce NoSuchElementException` {
    intercept[NoSuchElementException] {
      Set.empty.head
    }
  }
}
