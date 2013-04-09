package org.scalatest.examples.featurespec.tagging

import org.scalatest.Tag

object SlowTest extends Tag("com.mycompany.tags.SlowTest")
object DbTest extends Tag("com.mycompany.tags.DbTest")

import org.scalatest.FeatureSpec

class TVSet {
  private var on: Boolean = false
  def isOn: Boolean = on
  def pressPowerButton() {
    on = !on
  }
}

class TVSetSpec extends FeatureSpec {

  feature("TV power button") {
    scenario("User presses power button when TV is off", SlowTest) {
      val tv = new TVSet
      assert(!tv.isOn)
      tv.pressPowerButton()
      assert(tv.isOn)
    }

    scenario("User presses power button when TV is on", SlowTest, DbTest) {
      val tv = new TVSet
      tv.pressPowerButton()
      assert(tv.isOn)
      tv.pressPowerButton()
      assert(!tv.isOn)
    }
  }
}
