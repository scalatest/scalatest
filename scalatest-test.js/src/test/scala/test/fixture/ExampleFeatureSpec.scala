package test.fixture

import org.scalatest._

class ExampleFeatureSpec extends fixture.FeatureSpec {

  type FixtureParam = String
  def withFixture(test: OneArgTest): Outcome = {
    test("hi")
  }

  class TVSet {
    private var on: Boolean = false
    def isOn: Boolean = on
    def pressPowerButton() {
      on = !on
    }
  }

  info("As a TV set owner")
  info("I want to be able to turn the TV on and off")
  info("So I can watch TV when I want")
  info("And save energy when I'm not watching TV")

  feature("TV power button") {
    scenario("User presses power button when TV is off") { f =>

      val tv = new TVSet
      assert(!tv.isOn)

      tv.pressPowerButton()

      assert(tv.isOn)
    }

    scenario("User presses power button when TV is on") { f =>

      val tv = new TVSet
      tv.pressPowerButton()
      assert(tv.isOn)

      tv.pressPowerButton()

      assert(!tv.isOn)
    }
  }
}