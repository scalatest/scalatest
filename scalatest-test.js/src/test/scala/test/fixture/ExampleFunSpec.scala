package test.fixture

import org.scalatest._

class ExampleFunSpec extends fixture.FunSpec {

  type FixtureParam = String
  def withFixture(test: OneArgTest): Outcome = {
    test("hi")
  }

  describe("A Set") {
    describe("when empty") {
      it("should have size 0") { f =>
        assert(Set.empty.size == 0)
      }

      it("should produce NoSuchElementException when head is invoked") { f =>
        intercept[NoSuchElementException] {
          Set.empty.head
        }
      }
    }
  }
}