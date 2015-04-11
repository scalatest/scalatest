package test.fixture

import org.scalatest._

class ExamplePropSpec extends fixture.PropSpec {

  type FixtureParam = String
  def withFixture(test: OneArgTest): Outcome = {
    test("hi")
  }

  property("an empty Set should have size 0") { f =>
    assert(Set.empty[Int].size == 0)
  }

}