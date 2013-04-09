package org.scalatest.examples.spec.ignoreall

import org.scalatest.Spec
import org.scalatest.Ignore

@Ignore
class SetSpec extends Spec {
 
  object `A Set` {
    object `when empty` {
      def `should have size 0` {
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
