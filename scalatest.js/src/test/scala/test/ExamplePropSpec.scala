package test

import org.scalatest.PropSpec

class ExampleSpec extends PropSpec {

  property("an empty Set should have size 0") {
    assert(Set.empty[Int].size == 0)
  }

}