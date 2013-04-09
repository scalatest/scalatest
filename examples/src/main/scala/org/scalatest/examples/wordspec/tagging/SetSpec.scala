package org.scalatest.examples.wordspec.tagging

import org.scalatest.Tag

object SlowTest extends Tag("com.mycompany.tags.SlowTest")
object DbTest extends Tag("com.mycompany.tags.DbTest")

import org.scalatest.WordSpec

class SetSpec extends WordSpec {

  "A Set" when {
    "empty" should {
      "have size 0" taggedAs(SlowTest) in {
        assert(Set.empty.size === 0)
      }
      
      "produce NoSuchElementException when head is invoked" taggedAs(SlowTest, DbTest) in {
        intercept[NoSuchElementException] {
          Set.empty.head
        }
      }
    }
  }
}
