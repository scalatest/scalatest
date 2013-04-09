package org.scalatest.examples.flatspec.tagging

import org.scalatest.Tag

object SlowTest extends Tag("com.mycompany.tags.SlowTest")
object DbTest extends Tag("com.mycompany.tags.DbTest")

import org.scalatest.FlatSpec

class SetSpec extends FlatSpec {

  behavior of "An empty Set"
  
  it should "have size 0" taggedAs(SlowTest) in {
    assert(Set.empty.size === 0)
  }
    
  it should "produce NoSuchElementException when head is invoked" taggedAs(SlowTest, DbTest) in {
    intercept[NoSuchElementException] {
      Set.empty.head
    }
  }
}
