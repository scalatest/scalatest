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
package org.scalatest.matchers

import org.scalatest._
import Inside._
import org.scalautils.PrettyMethods

class MatcherProducersSpec extends Spec with Matchers {

  object `A Matcher (without MatcherProducers)` {
    def `can be composed via compose ... andThen ... compose` {
      val f = be > (_: Int)
      val g = (_: String).toInt
      // (f compose g)(x) === f(g(x))
      val beAsIntsGreaterThan = (f compose g) andThen (_ compose g)
      "8" should beAsIntsGreaterThan ("7")
      val tfe = the [TestFailedException] thrownBy {
        "7" should beAsIntsGreaterThan ("8")
      }
      tfe.message should be (Some(Resources("wasNotGreaterThan", "7", "8")))
    }
  }
  object `The MatcherProducers trait` {
    import MatcherProducers._
    def `should enable compose ... andThen ... compose behavior  via composeTwice` {
      val f = be > (_: Int)
      val g = (_: String).toInt
      // (f compose g)(x) === f(g(x))
      val beAsIntsGreaterThan = f composeTwice g
      "8" should beAsIntsGreaterThan ("7")
      val tfe = the [TestFailedException] thrownBy {
        "7" should beAsIntsGreaterThan ("8")
      }
      tfe.message should be (Some(Resources("wasNotGreaterThan", "7", "8")))
    }
  }
}

