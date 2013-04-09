package org.scalatest.examples.funspec.tagging

import org.scalatest.Tag

object SlowTest extends Tag("com.mycompany.tags.SlowTest")
object DbTest extends Tag("com.mycompany.tags.DbTest")

import org.scalatest.FunSpec

class SetSpec extends FunSpec {

  describe("A Set") {
    describe("when empty") {
      it("should have size 0", SlowTest) {
        assert(Set.empty.size === 0)
      }
      
      it("should produce NoSuchElementException when head is invoked", SlowTest, DbTest) {
        intercept[NoSuchElementException] {
          Set.empty.head
        }
      }
    }
  }
}
