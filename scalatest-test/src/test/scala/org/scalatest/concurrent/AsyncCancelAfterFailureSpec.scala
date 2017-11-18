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
package org.scalatest.concurrent

import org.scalatest.SharedHelpers._
import org.scalatest.{Args, AsyncFlatSpec, AsyncFunSuite}
import scala.concurrent.{Promise, Future}

class AsyncCancelAfterFailureSpec extends AsyncFlatSpec {

  "AsyncCancelAfterFailure" should "not interfere if no tests fail" in {
    class MySuite extends AsyncFunSuite with AsyncCancelAfterFailure {
      test("this test succeeds") {
        Future {
          assert(1 + 1 === 2)
        }
      }
      test("this test does not fail") {
        Future {
          assert(1 + 1 === 2)
        }
      }
      test("this test also succeeds") {
        Future {
          assert(1 + 1 === 2)
        }
      }
      test("and this test succeeds too") {
        Future {
          assert(1 + 1 === 2)
        }
      }
    }
    val rep = new EventRecordingReporter
    val status = (new MySuite).run(None, Args(rep))
    val promise = Promise[EventRecordingReporter]
    status whenCompleted { _ => promise.success(rep) }
    promise.future.map { rep =>
      assert(rep.testSucceededEventsReceived.size === 4)
      assert(rep.testFailedEventsReceived.size === 0)
      assert(rep.testCanceledEventsReceived.size === 0)
    }
  }

  it should "cancel remaining tests if a test fails" in {
    class MySuite extends AsyncFunSuite with AsyncCancelAfterFailure {
      test("this test succeeds") {
        Future {
          assert(1 + 1 === 2)
        }
      }
      test("this test fails") {
        Future {
          assert(1 + 1 === 3)
        }
      }
      test("this test also succeeds") {
        Future {
          assert(1 + 1 === 2)
        }
      }
      test("and this test succeeds too") {
        Future {
          assert(1 + 1 === 2)
        }
      }
    }
    val rep = new EventRecordingReporter
    val status = (new MySuite).run(None, Args(rep))
    val promise = Promise[EventRecordingReporter]
    status whenCompleted { _ => promise.success(rep) }
    promise.future.map { rep =>
      assert(rep.testSucceededEventsReceived.size === 1)
      assert(rep.testFailedEventsReceived.size === 1)
      assert(rep.testCanceledEventsReceived.size === 2)
    }
  }
  // it should not compile if you attempt to mix CancelAfterFailure in along with OneInstancePerTest
  // class MySuite extends Suite with CancelAfterFailure with OneInstancePerTest
  // class MySuite extends Suite with OneInstancePerTest with CancelAfterFailure
}

