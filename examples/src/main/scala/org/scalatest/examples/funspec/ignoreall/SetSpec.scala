package org.scalatest.examples.funspec.ignoreall

import org.scalatest.FunSpec
import org.scalatest.Ignore

@Ignore
class SetSpec extends FunSpec {
  
  describe("A Set") {
    describe("when empty") {
      it("should have size 0") {
        assert(Set.empty.size === 0)
      }
      
      it("should produce NoSuchElementException when head is invoked") {
        intercept[NoSuchElementException] {
          Set.empty.head
        }
      }
    }
  }
}
