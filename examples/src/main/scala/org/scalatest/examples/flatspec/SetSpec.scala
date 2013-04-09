package org.scalatest.examples.flatspec

import org.scalatest.FlatSpec

class SetSpec extends FlatSpec {
  
  "An empty Set" should "have size 0" in {
    assert(Set.empty.size === 0)
  }
    
  it should "produce NoSuchElementException when head is invoked" in {
    intercept[NoSuchElementException] {
      Set.empty.head
    }
  }
}