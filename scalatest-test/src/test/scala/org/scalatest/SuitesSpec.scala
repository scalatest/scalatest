/*
 * Copyright 2001-2013 Artima, Inc.
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
package org.scalatest

import SharedHelpers._
import Suite.CHOSEN_STYLES
import org.scalatest.exceptions.NotAllowedException

class SuitesSpec extends FunSpec {

  val a = new Suite {}
  val b = new FunSuite {}
  val c = new FunSpec {}
  val d = new WordSpec {}
  val e = new FeatureSpec {}

  describe("Suites") {
    it("should return the passed suites from nestedSuites") {
      val f = new Suites(a, b, c, d, e)
      assert(f.nestedSuites == List(a, b, c, d, e))
      val g = new Suites(Array(a, b, c, d, e): _*)
      assert(g.nestedSuites == List(a, b, c, d, e))
      intercept[NullPointerException] {
        new Suites(a, b, null, d, e)
      }
      intercept[NullPointerException] {
        val aNull: Array[Suite] = null
        new Suites(aNull: _*)
      }
    }
    it("should not care about chosenStyles if it contains no tests directly and only contains nested suites with no tests") {
      val f = new Suites(a, b, c, d, e)
      f.run(None, Args(SilentReporter))
      f.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap(CHOSEN_STYLES -> Set("FunSuite")), None, new Tracker, Set.empty))
    }

    it("should care about chosenStyles if it contains tests directly") {

      class SuitesWithSpecStyleTests(suitesToNest: Suite*) extends Suites(suitesToNest.toList: _*) with SpecLike {
        def `test method 1` = {}
        def `test method 2` = {}
      }

      val g = new SuitesWithSpecStyleTests(a, b, c, d, e)
      // OK if no chosen styles specified
      g.run(None, Args(SilentReporter))
      // OK if chosen styles is Suite, because that's the style of *tests* written in this Suites
      g.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap(CHOSEN_STYLES -> Set("org.scalatest.Spec")), None, new Tracker, Set.empty))
      intercept[NotAllowedException] {
        // Should not allow if chosen styles is FunSuite, because Suite is the style of *tests* written in this Suites
        g.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap(CHOSEN_STYLES -> Set("org.scalatest.FunSuite")), None, new Tracker, Set.empty))
      }
    }
  }
}

