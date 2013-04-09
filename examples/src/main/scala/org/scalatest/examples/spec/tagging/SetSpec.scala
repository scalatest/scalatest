package org.scalatest.examples.spec.tagging

import org.scalatest._

class SetSpec extends Spec {

  object `A Set` {
    object `when empty` {

      @SlowTest
      def `should have size 0` {
        assert(Set.empty.size === 0)
      }
      
      @SlowTest @DbTest
      def `should produce NoSuchElementException when head is invoked` {
        intercept[NoSuchElementException] {
          Set.empty.head
        }
      }
    }
  }
}