package org.scalatest.examples.freespec.tagging

import org.scalatest.Tag

object SlowTest extends Tag("com.mycompany.tags.SlowTest")
object DbTest extends Tag("com.mycompany.tags.DbTest")

import org.scalatest.FreeSpec

class SetSpec extends FreeSpec {

  "A Set" - {
    "when empty" - {
      "should have size 0" taggedAs(SlowTest) in {
        assert(Set.empty.size === 0)
      }
      
      "should produce NoSuchElementException when head is invoked" taggedAs(SlowTest, DbTest) in {
        intercept[NoSuchElementException] {
          Set.empty.head
        }
      }
    }
  }
}
