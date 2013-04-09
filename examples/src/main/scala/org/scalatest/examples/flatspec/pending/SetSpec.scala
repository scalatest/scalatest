package org.scalatest.examples.flatspec.pending

import org.scalatest._

class SetSpec extends FlatSpec {

  "An empty Set" should "have size 0" in (pending)
    
  it should "produce NoSuchElementException when head is invoked" in {
    intercept[NoSuchElementException] {
      Set.empty.head
    }
  }
}