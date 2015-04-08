package test.fixture

import org.scalatest._

class SetSpec extends fixture.FlatSpec {

  type FixtureParam = String
  def withFixture(test: OneArgTest): Outcome = {
    test("hi")
  }

  "An empty Set" should "have size 0" in { f =>
    assert(Set.empty.size == 0)
  }

  it should "produce NoSuchElementException when head is invoked" in { f =>
    intercept[NoSuchElementException] {
      Set.empty.head
    }
  }
}