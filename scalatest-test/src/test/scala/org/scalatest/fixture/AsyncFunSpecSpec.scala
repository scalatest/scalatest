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

import scala.concurrent.Future
import org.scalatest._
import SharedHelpers.EventRecordingReporter
import org.scalatest.concurrent.SleepHelper

class AsyncFunSpecSpec extends org.scalatest.FunSpec {

  describe("AsyncFunSpec") {

    it("can be used for tests that return Future under parallel async test execution") {

      class ExampleSpec extends AsyncFunSpec with ParallelTestExecution {

        type FixtureParam = String
        def withAsyncFixture(test: OneArgAsyncTest): Future[Outcome] =
          test("testing")

        val a = 1

        it("test 1") { fixture =>
          Future {
            assert(a == 1)
          }
        }

        it("test 2") { fixture =>
          Future {
            assert(a == 2)
          }
        }

        it("test 3") { fixture =>
          Future {
            pending
          }
        }

        it("test 4") { fixture =>
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

    it("can be used for tests that did not return Future under parallel async test execution") {

      class ExampleSpec extends AsyncFunSpec with ParallelTestExecution {

        type FixtureParam = String
        def withAsyncFixture(test: OneArgAsyncTest): Future[Outcome] =
          test("testing")

        val a = 1

        it("test 1") { fixture =>
          assert(a == 1)
        }

        it("test 2") { fixture =>
          assert(a == 2)
        }

        it("test 3") { fixture =>
          pending
        }

        it("test 4") { fixture =>
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

    it("should run tests that return Future in serial by default") {

      @volatile var count = 0

      class ExampleSpec extends AsyncFunSpec {

        type FixtureParam = String
        def withAsyncFixture(test: OneArgAsyncTest): Future[Outcome] =
          test("testing")

        it("test 1") { fixture =>
          Future {
            SleepHelper.sleep(30)
            assert(count == 0)
            count = 1
            succeed
          }
        }

        it("test 2") { fixture =>
          Future {
            assert(count == 1)
            SleepHelper.sleep(50)
            count = 2
            succeed
          }
        }

        it("test 3") { fixture =>
          Future {
            assert(count == 2)
          }
        }

      }

      val rep = new EventRecordingReporter
      val suite = new ExampleSpec
      val status = suite.run(None, Args(reporter = rep))
      // SKIP-SCALATESTJS-START
      status.waitUntilCompleted()
      // SKIP-SCALATESTJS-END

      assert(rep.testStartingEventsReceived.length == 3)
      assert(rep.testSucceededEventsReceived.length == 3)

    }

    it("should run tests that does not return Future in serial by default") {

      @volatile var count = 0

      class ExampleSpec extends AsyncFunSpec {

        type FixtureParam = String
        def withAsyncFixture(test: OneArgAsyncTest): Future[Outcome] =
          test("testing")

        it("test 1") { fixture =>
          SleepHelper.sleep(30)
          assert(count == 0)
          count = 1
          succeed
        }

        it("test 2") { fixture =>
          assert(count == 1)
          SleepHelper.sleep(50)
          count = 2
          succeed
        }

        it("test 3") { fixture =>
          assert(count == 2)
        }

      }

      val rep = new EventRecordingReporter
      val suite = new ExampleSpec
      val status = suite.run(None, Args(reporter = rep))
      // SKIP-SCALATESTJS-START
      status.waitUntilCompleted()
      // SKIP-SCALATESTJS-END

      assert(rep.testStartingEventsReceived.length == 3)
      assert(rep.testSucceededEventsReceived.length == 3)

    }

  }

}
