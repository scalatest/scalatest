package org.scalatest.examples.spec.pending

import org.scalatest._

class SetSpec extends Spec {

  object `A Set` {
    object `when empty` {
      def `should have size 0` { pending }
      
      def `should produce NoSuchElementException when head is invoked` {
        intercept[NoSuchElementException] {
          Set.empty.head
        }
      }
    }
  }
}