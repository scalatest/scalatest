/*
 * Copyright 2001-2024 Artima, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.scalatest.examples.featurespec.pending

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

    scenario("User presses power button when TV is off") (pending)

    scenario("User presses power button when TV is on") {
      val tv = new TVSet
      tv.pressPowerButton()
      assert(tv.isOn)
      tv.pressPowerButton()
      assert(!tv.isOn)
    }
  }
}
