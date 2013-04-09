package org.scalatest.examples.spec.ignore

import org.scalatest.Spec
import org.scalatest.Ignore

class SetSpec extends Spec {
  
  object `A Set` {
    object `when empty` {
      @Ignore def `should have size 0` {
        assert(Set.empty.size === 0)
      }
      
      def `should produce NoSuchElementException when head is invoked` {
        intercept[NoSuchElementException] {
          Set.empty.head
        }
      }
    }
  }
}