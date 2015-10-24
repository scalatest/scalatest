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
package org.scalatest

import SharedHelpers.EventRecordingReporter
import scala.concurrent.{Promise, Future}
import org.scalatest.concurrent.SleepHelper

class AsyncFreeSpecSpec2 extends AsyncFunSpec {

  // SKIP-SCALATESTJS-START
  implicit val executionContext = scala.concurrent.ExecutionContext.Implicits.global
  // SKIP-SCALATESTJS-END
  //SCALATESTJS-ONLY implicit val executionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

  describe("AsyncFreeSpec") {

    it("can be used for tests that return Future under parallel async test execution") {

      class ExampleSpec extends AsyncFreeSpec with ParallelTestExecution {

        // SKIP-SCALATESTJS-START
        implicit val executionContext = scala.concurrent.ExecutionContext.Implicits.global
        // SKIP-SCALATESTJS-END
        //SCALATESTJS-ONLY implicit val executionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

        val a = 1

        "test 1" in {
          Future {
            assert(a == 1)
          }
        }

        "test 2" in {
          Future {
            assert(a == 2)
          }
        }

        "test 3" in {
          Future {
            pending
          }
        }

        "test 4" in {
          Future {
            cancel
          }
        }

        "test 5" ignore {
          Future {
            cancel
          }
        }

        override def newInstance = new ExampleSpec
      }

      val rep = new EventRecordingReporter
      val spec = new ExampleSpec
      val status = spec.run(None, Args(reporter = rep))
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

      class ExampleSpec extends AsyncFreeSpec with ParallelTestExecution {

        // SKIP-SCALATESTJS-START
        implicit val executionContext = scala.concurrent.ExecutionContext.Implicits.global
        // SKIP-SCALATESTJS-END
        //SCALATESTJS-ONLY implicit val executionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

        val a = 1

        "test 1" in {
          assert(a == 1)
        }

        "test 2" in {
          assert(a == 2)
        }

        "test 3" in {
          pending
        }

        "test 4" in {
          cancel
        }

        "test 5" ignore {
          cancel
        }

        override def newInstance = new ExampleSpec
      }

      val rep = new EventRecordingReporter
      val spec = new ExampleSpec
      val status = spec.run(None, Args(reporter = rep))
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

      class ExampleSpec extends AsyncFreeSpec {

        // SKIP-SCALATESTJS-START
        implicit val executionContext = scala.concurrent.ExecutionContext.Implicits.global
        // SKIP-SCALATESTJS-END
        //SCALATESTJS-ONLY implicit val executionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

        "test 1" in {
          Future {
            SleepHelper.sleep(30)
            assert(count == 0)
            count = 1
            Succeeded
          }
        }

        "test 2" in {
          Future {
            assert(count == 1)
            SleepHelper.sleep(50)
            count = 2
            Succeeded
          }
        }

        "test 3" in {
          Future {
            assert(count == 2)
          }
        }

      }

      val rep = new EventRecordingReporter
      val suite = new ExampleSpec
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

      class ExampleSpec extends AsyncFreeSpec {

        // SKIP-SCALATESTJS-START
        implicit val executionContext = scala.concurrent.ExecutionContext.Implicits.global
        // SKIP-SCALATESTJS-END
        //SCALATESTJS-ONLY implicit val executionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

        "test 1" in {
          SleepHelper.sleep(30)
          assert(count == 0)
          count = 1
          Succeeded
        }

        "test 2" in {
          assert(count == 1)
          SleepHelper.sleep(50)
          count = 2
          Succeeded
        }

        "test 3" in {
          assert(count == 2)
        }

      }

      val rep = new EventRecordingReporter
      val suite = new ExampleSpec
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
