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

import scala.concurrent.{Promise, Future}
import org.scalatest._
import SharedHelpers.EventRecordingReporter
import org.scalatest.concurrent.SleepHelper

class AsyncFunSuiteLikeSpec2 extends org.scalatest.AsyncFunSpec {

  // SKIP-SCALATESTJS-START
  implicit val executionContext = scala.concurrent.ExecutionContext.Implicits.global
  // SKIP-SCALATESTJS-END
  //SCALATESTJS-ONLY implicit val executionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

  describe("AsyncFunSuiteLike") {

    it("can be used for tests that return Future under parallel async test execution") {

      class ExampleSuite extends AsyncFunSuiteLike with ParallelAsyncTestExecution {

        // SKIP-SCALATESTJS-START
        implicit val executionContext = scala.concurrent.ExecutionContext.Implicits.global
        // SKIP-SCALATESTJS-END
        //SCALATESTJS-ONLY implicit val executionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

        type FixtureParam = String
        def withAsyncFixture(test: OneArgAsyncTest): Future[Outcome] =
          test("testing")

        val a = 1

        test("test 1") { fixture =>
          Future {
            assert(a == 1)
          }
        }

        test("test 2") { fixture =>
          Future {
            assert(a == 2)
          }
        }

        test("test 3") { fixture =>
          Future {
            pending
          }
        }

        test("test 4") { fixture =>
          Future {
            cancel
          }
        }

        ignore("test 5") { fixture =>
          Future {
            cancel
          }
        }

        override def newInstance = new ExampleSuite
      }

      val rep = new EventRecordingReporter
      val suite = new ExampleSuite
      val status = suite.run(None, Args(reporter = rep))
      val promise = Promise[EventRecordingReporter]
      status whenCompleted { _ => promise.success(rep) }
      promise.future.map { repo =>
        assert(repo.testStartingEventsReceived.length == 4)
        assert(repo.testSucceededEventsReceived.length == 1)
        assert(repo.testSucceededEventsReceived(0).testName == "test 1")
        assert(repo.testFailedEventsReceived.length == 1)
        assert(repo.testFailedEventsReceived(0).testName == "test 2")
        assert(repo.testPendingEventsReceived.length == 1)
        assert(repo.testPendingEventsReceived(0).testName == "test 3")
        assert(repo.testCanceledEventsReceived.length == 1)
        assert(repo.testCanceledEventsReceived(0).testName == "test 4")
        assert(repo.testIgnoredEventsReceived.length == 1)
        assert(repo.testIgnoredEventsReceived(0).testName == "test 5")
      }
    }

    it("can be used for tests that did not return Future under parallel async test execution") {

      class ExampleSuite extends AsyncFunSuiteLike with ParallelAsyncTestExecution {

        // SKIP-SCALATESTJS-START
        implicit val executionContext = scala.concurrent.ExecutionContext.Implicits.global
        // SKIP-SCALATESTJS-END
        //SCALATESTJS-ONLY implicit val executionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

        type FixtureParam = String
        def withAsyncFixture(test: OneArgAsyncTest): Future[Outcome] =
          test("testing")

        val a = 1

        test("test 1") { fixture =>
          assert(a == 1)
        }

        test("test 2") { fixture =>
          assert(a == 2)
        }

        test("test 3") { fixture =>
          pending
        }

        test("test 4") { fixture =>
          cancel
        }

        ignore("test 5") { fixture =>
          cancel
        }

        override def newInstance = new ExampleSuite
      }

      val rep = new EventRecordingReporter
      val suite = new ExampleSuite
      val status = suite.run(None, Args(reporter = rep))
      val promise = Promise[EventRecordingReporter]
      status whenCompleted { _ => promise.success(rep) }
      promise.future.map { repo =>
        assert(repo.testStartingEventsReceived.length == 4)
        assert(repo.testSucceededEventsReceived.length == 1)
        assert(repo.testSucceededEventsReceived(0).testName == "test 1")
        assert(repo.testFailedEventsReceived.length == 1)
        assert(repo.testFailedEventsReceived(0).testName == "test 2")
        assert(repo.testPendingEventsReceived.length == 1)
        assert(repo.testPendingEventsReceived(0).testName == "test 3")
        assert(repo.testCanceledEventsReceived.length == 1)
        assert(repo.testCanceledEventsReceived(0).testName == "test 4")
        assert(repo.testIgnoredEventsReceived.length == 1)
        assert(repo.testIgnoredEventsReceived(0).testName == "test 5")
      }
    }

    it("should run tests that return Future in serial by default") {

      @volatile var count = 0

      class ExampleSuite extends AsyncFunSuiteLike {

        // SKIP-SCALATESTJS-START
        implicit val executionContext = scala.concurrent.ExecutionContext.Implicits.global
        // SKIP-SCALATESTJS-END
        //SCALATESTJS-ONLY implicit val executionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

        type FixtureParam = String
        def withAsyncFixture(test: OneArgAsyncTest): Future[Outcome] =
          test("testing")

        test("test 1") { fixture =>
          Future {
            SleepHelper.sleep(30)
            assert(count == 0)
            count = 1
            Succeeded
          }
        }

        test("test 2") { fixture =>
          Future {
            assert(count == 1)
            SleepHelper.sleep(50)
            count = 2
            Succeeded
          }
        }

        test("test 3") { fixture =>
          Future {
            assert(count == 2)
          }
        }

      }

      val rep = new EventRecordingReporter
      val suite = new ExampleSuite
      val status = suite.run(None, Args(reporter = rep))
      val promise = Promise[EventRecordingReporter]
      status whenCompleted { _ => promise.success(rep) }
      promise.future.map { repo =>
        assert(repo.testStartingEventsReceived.length == 3)
        assert(repo.testSucceededEventsReceived.length == 3)
      }
    }

    it("should run tests that does not return Future in serial by default") {

      @volatile var count = 0

      class ExampleSuite extends AsyncFunSuiteLike {

        // SKIP-SCALATESTJS-START
        implicit val executionContext = scala.concurrent.ExecutionContext.Implicits.global
        // SKIP-SCALATESTJS-END
        //SCALATESTJS-ONLY implicit val executionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

        type FixtureParam = String
        def withAsyncFixture(test: OneArgAsyncTest): Future[Outcome] =
          test("testing")

        test("test 1") { fixture =>
          SleepHelper.sleep(30)
          assert(count == 0)
          count = 1
          Succeeded
        }

        test("test 2") { fixture =>
          assert(count == 1)
          SleepHelper.sleep(50)
          count = 2
          Succeeded
        }

        test("test 3") { fixture =>
          assert(count == 2)
        }

      }

      val rep = new EventRecordingReporter
      val suite = new ExampleSuite
      val status = suite.run(None, Args(reporter = rep))
      val promise = Promise[EventRecordingReporter]
      status whenCompleted { _ => promise.success(rep) }
      promise.future.map { repo =>
        assert(repo.testStartingEventsReceived.length == 3)
        assert(repo.testSucceededEventsReceived.length == 3)
      }
    }

  }

}
