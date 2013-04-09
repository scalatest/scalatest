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

class CancelAfterFailureSpec extends FlatSpec with SharedHelpers {

  "CancelAfterFailure" should "not interfere if no tests fail" in {
    class MySuite extends FunSuite with CancelAfterFailure {
      test("this test succeeds") {
        assert(1 + 1 === 2)
      }
      test("this test does not fail") {
        assert(1 + 1 === 2)
      }
      test("this test also succeeds") {
        assert(1 + 1 === 2)
      }
      test("and this test succeeds too") {
        assert(1 + 1 === 2)
      }
    }
    val rep = new EventRecordingReporter
    (new MySuite).run(None, Args(rep))
    assert(rep.testSucceededEventsReceived.size === 4)
    assert(rep.testFailedEventsReceived.size === 0)
    assert(rep.testCanceledEventsReceived.size === 0)
  }

  it should "cancel remaining tests if a test fails" in {
    class MySuite extends FunSuite with CancelAfterFailure {
      test("this test succeeds") {
        assert(1 + 1 === 2)
      }
      test("this test fails") {
        assert(1 + 1 === 3)
      }
      test("this test also succeeds") {
        assert(1 + 1 === 2)
      }
      test("and this test succeeds too") {
        assert(1 + 1 === 2)
      }
    }
    val rep = new EventRecordingReporter
    // (new MySuite).run(None, Args(rep))
    (new MySuite).run(None, Args(rep))
    assert(rep.testSucceededEventsReceived.size === 1)
    assert(rep.testFailedEventsReceived.size === 1)
    assert(rep.testCanceledEventsReceived.size === 2)
  }
  // it should not compile if you attempt to mix CancelAfterFailure in along with OneInstancePerTest
  // class MySuite extends Suite with CancelAfterFailure with OneInstancePerTest
  // class MySuite extends Suite with OneInstancePerTest with CancelAfterFailure
}

