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
package org.scalatest.fixture

import org.scalatest._
import SharedHelpers._

class UnitFixtureSpec extends org.scalatest.Spec {
  object `A UnitFixture` {
    def `should pass the unit value to each test` {
      class MySuite extends fixture.FunSuite with UnitFixture {
        var unitPassed = false
        test("something") { (un: Unit) =>
          if (un == ())
            unitPassed = true // Now this is a silly test
        }
      }
      val suite = new MySuite
      suite.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(suite.unitPassed)
    }
  }
}
