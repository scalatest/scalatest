/*
 * Copyright 2001-2016 Artima, Inc.
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
import org.scalatest
import org.scalatest.funsuite

class AsyncConfigMapFixtureSpec extends scalatest.funspec.AnyFunSpec {
  describe("A AsyncConfigMapFixture") {
    it("should pass the config map to each test") {
      val myConfigMap = ConfigMap("hello" -> "world", "salt" -> "pepper")
      class MySpec extends funsuite.FixtureAsyncFunSuite with AsyncConfigMapFixture {
        var configMapPassed = false
        test("test something") { configMap =>
          if (configMap == myConfigMap)
            configMapPassed = true
          succeed
        }
      }
      val spec = new MySpec
      spec.run(None, Args(SilentReporter, Stopper.default, Filter(), myConfigMap, None, new Tracker))
      assert(spec.configMapPassed)
    }
  }
}
