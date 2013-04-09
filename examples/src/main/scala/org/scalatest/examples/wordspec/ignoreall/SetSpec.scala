package org.scalatest.examples.wordspec.ignoreall

import org.scalatest.WordSpec
import org.scalatest.Ignore

@Ignore 
class SetSpec extends WordSpec {
  
  "A Set" when {
    "empty" should {
      "have size 0" in {
        assert(Set.empty.size === 0)
      }
      
      "produce NoSuchElementException when head is invoked" in {
        intercept[NoSuchElementException] {
          Set.empty.head
        }
      }
    }
  }
}
