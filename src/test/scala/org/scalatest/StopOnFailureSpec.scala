/*
 * Copyright 2001-2011 Artima, Inc.
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

class StopOnFailureSpec extends FlatSpec with SharedHelpers {

  "StopOnFailure" should "not invoke stop on the stopper if a test succeeds" in {
    class MySuite extends FunSuite with StopOnFailure {
      test("this test succeeds") {
        assert(1 + 1 === 2)
      }
    }
    val stopper = Stopper.default
    // (new MySuite).run(None, RunArgs(SilentReporter, stopper, Filter(), ConfigMap.empty, None, new Tracker(), Set.empty))
    (new MySuite).run(None, Args(SilentReporter, stopper))
    assert(!stopper.stopRequested)
  }

  it should "invoke stop on the stopper if a test fails" in { pending
    class MySuite extends FunSuite with StopOnFailure {
      test("this test fails") {
        assert(1 + 1 === 3)
      }
    }
    val stopper = Stopper.default
    //(new MySuite).run(None, RunArgs(SilentReporter, stopper, Filter(), ConfigMap.empty, None, new Tracker(), Set.empty))
    (new MySuite).run(None, Args(SilentReporter, stopper))
    assert(stopper.stopRequested)
  }
}
