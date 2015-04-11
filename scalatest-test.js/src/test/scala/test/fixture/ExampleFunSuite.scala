package test.fixture

import org.scalatest._

class ExampleFunSuite extends fixture.FunSuite {

  type FixtureParam = String
  def withFixture(test: OneArgTest): Outcome = {
    test("hi")
  }

  test("An empty Set should have size 0") { f =>
    assert(Set.empty.size == 0)
  }

  test("Invoking head on an empty Set should produce NoSuchElementException") { f =>
    intercept[NoSuchElementException] {
      Set.empty.head
    }
  }
}
