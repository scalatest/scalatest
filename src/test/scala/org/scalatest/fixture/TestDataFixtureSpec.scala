/*
 * Copyright 2001-2009 Artima, Inc.
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

class TestDataFixtureSpec extends org.scalatest.FunSpec with SharedHelpers {
  describe("A TestDataFixture") {
    it("should pass the test data to each test") {
      val myConfigMap = ConfigMap("hello" -> "world", "salt" -> "pepper")
      class MySuite extends fixture.FunSuite with TestDataFixture {
        var testDataPassed = false
        test("something") { (td: TestData) =>
          if (td.configMap == myConfigMap && td.name == "something")
            testDataPassed = true
        }
      }
      val suite = new MySuite
      suite.run(None, Args(SilentReporter, Stopper.default, Filter(), myConfigMap, None, new Tracker, Set.empty))
      assert(suite.testDataPassed)
    }
  }
}
