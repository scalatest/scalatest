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
import SharedHelpers.EventRecordingReporter
import scala.concurrent.Future

class AsyncPropSpecLikeSpec extends org.scalatest.FunSpec {

  describe("AsyncPropSpecLike") {

    it("can be used for tests that return Future") {

      class ExampleSpec extends AsyncPropSpecLike {

        // SKIP-SCALATESTJS-START
        implicit val executionContext = scala.concurrent.ExecutionContext.Implicits.global
        // SKIP-SCALATESTJS-END
        //SCALATESTJS-ONLY implicit val executionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow

        type FixtureParam = String
        def withAsyncFixture(test: OneArgAsyncTest): Future[Outcome] =
          test("testing")

        val a = 1

        property("test 1") { fixture =>
          Future {
            assert(a == 1)
          }
        }

        property("test 2") { fixture =>
          Future {
            assert(a == 2)
          }
        }

        property("test 3") { fixture =>
          Future {
            pending
          }
        }

        property("test 4") { fixture =>
          Future {
            cancel
          }
        }

        ignore("test 5") { fixture =>
          Future {
            cancel
          }
        }

        override def newInstance = new ExampleSpec
      }

      val rep = new EventRecordingReporter
      val spec = new ExampleSpec
      val status = spec.run(None, Args(reporter = rep))
      // SKIP-SCALATESTJS-START
      status.waitUntilCompleted()
      // SKIP-SCALATESTJS-END
      assert(rep.testStartingEventsReceived.length == 4)
      assert(rep.testSucceededEventsReceived.length == 1)
      assert(rep.testSucceededEventsReceived(0).testName == "test 1")
      assert(rep.testFailedEventsReceived.length == 1)
      assert(rep.testFailedEventsReceived(0).testName == "test 2")
      assert(rep.testPendingEventsReceived.length == 1)
      assert(rep.testPendingEventsReceived(0).testName == "test 3")
      assert(rep.testCanceledEventsReceived.length == 1)
      assert(rep.testCanceledEventsReceived(0).testName == "test 4")
      assert(rep.testIgnoredEventsReceived.length == 1)
      assert(rep.testIgnoredEventsReceived(0).testName == "test 5")
    }

    it("can be used for tests that did not return Future") {

      class ExampleSpec extends AsyncPropSpecLike {

        // SKIP-SCALATESTJS-START
        implicit val executionContext = scala.concurrent.ExecutionContext.Implicits.global
        // SKIP-SCALATESTJS-END
        //SCALATESTJS-ONLY implicit val executionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow

        type FixtureParam = String
        def withAsyncFixture(test: OneArgAsyncTest): Future[Outcome] =
          test("testing")

        val a = 1

        property("test 1") { fixture =>
          assert(a == 1)
        }

        property("test 2") { fixture =>
          assert(a == 2)
        }

        property("test 3") { fixture =>
          pending
        }

        property("test 4") { fixture =>
          cancel
        }

        ignore("test 5") { fixture =>
          cancel
        }

        override def newInstance = new ExampleSpec
      }

      val rep = new EventRecordingReporter
      val spec = new ExampleSpec
      val status = spec.run(None, Args(reporter = rep))
      // SKIP-SCALATESTJS-START
      status.waitUntilCompleted()
      // SKIP-SCALATESTJS-END
      assert(rep.testStartingEventsReceived.length == 4)
      assert(rep.testSucceededEventsReceived.length == 1)
      assert(rep.testSucceededEventsReceived(0).testName == "test 1")
      assert(rep.testFailedEventsReceived.length == 1)
      assert(rep.testFailedEventsReceived(0).testName == "test 2")
      assert(rep.testPendingEventsReceived.length == 1)
      assert(rep.testPendingEventsReceived(0).testName == "test 3")
      assert(rep.testCanceledEventsReceived.length == 1)
      assert(rep.testCanceledEventsReceived(0).testName == "test 4")
      assert(rep.testIgnoredEventsReceived.length == 1)
      assert(rep.testIgnoredEventsReceived(0).testName == "test 5")
    }

  }

}