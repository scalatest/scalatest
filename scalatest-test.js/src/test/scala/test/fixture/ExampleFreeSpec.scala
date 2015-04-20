package test.fixture

import org.scalatest._

class ExampleFreeSpec extends fixture.FreeSpec {

  type FixtureParam = String
  def withFixture(test: OneArgTest): Outcome = {
    test("hi")
  }

  "A Set" - {
    "when empty" - {
      "should have size 0" in { f =>
        assert(Set.empty.size == 0)
      }

      "should produce NoSuchElementException when head is invoked" in { f =>
        intercept[NoSuchElementException] {
          Set.empty.head
        }
      }
    }
  }
}