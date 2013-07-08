package org.scalatest.examples.spec.disk

import org.scalatest._
import tags.Disk

class SetSpec extends Spec {

  @Disk def `an empty Set should have size 0` {
    assert(Set.empty.size === 0)
  }

  def `invoking head on an empty Set should produce NoSuchElementException` {
    intercept[NoSuchElementException] {
      Set.empty.head
    }
  }
}
