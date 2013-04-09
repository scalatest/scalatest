package org.scalatest.examples.wordspec.pending

import org.scalatest._

class SetSpec extends WordSpec {

  "A Set" when {
    "empty" should {
      "have size 0" in (pending)
      
      "produce NoSuchElementException when head is invoked" in {
        intercept[NoSuchElementException] {
          Set.empty.head
        }
      }
    }
  }
}
