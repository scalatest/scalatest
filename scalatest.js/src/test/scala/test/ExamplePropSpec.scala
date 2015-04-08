package test

import org.scalatest.PropSpec

class ExamplePropSpec extends PropSpec {

  property("an empty Set should have size 0") {
    assert(Set.empty[Int].size == 0)
  }

}