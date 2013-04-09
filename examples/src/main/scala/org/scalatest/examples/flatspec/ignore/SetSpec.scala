package org.scalatest.examples.flatspec.ignore

import org.scalatest.FlatSpec

class SetSpec extends FlatSpec {
  
  "An empty Set" should "have size 0" in {
    assert(Set.empty.size === 0)
  }
    
  ignore should "produce NoSuchElementException when head is invoked" in {
    intercept[NoSuchElementException] {
      Set.empty.head
    }
  }
}
