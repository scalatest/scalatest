package org.scalatest.examples.funsuite.tagging

import org.scalatest.Tag

object SlowTest extends Tag("com.mycompany.tags.SlowTest")
object DbTest extends Tag("com.mycompany.tags.DbTest")

import org.scalatest.FunSuite

class SetSuite extends FunSuite {

  test("An empty Set should have size 0", SlowTest) {
    assert(Set.empty.size === 0)
  }

  test("Invoking head on an empty Set should produce NoSuchElementException",
       SlowTest, DbTest)
  {
    intercept[NoSuchElementException] {
      Set.empty.head
    }
  }
}
