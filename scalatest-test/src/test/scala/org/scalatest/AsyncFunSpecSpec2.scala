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

class AsyncFunSpecSpec2 extends AsyncFunSpec {

  // SKIP-SCALATESTJS-START
  implicit val executionContext = scala.concurrent.ExecutionContext.Implicits.global
  // SKIP-SCALATESTJS-END
  //SCALATESTJS-ONLY implicit val executionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

  override def newInstance = new AsyncFunSpecSpec2

  def toFuture(status: Status): Future[Boolean] = {
    val promise = Promise[Boolean]
    status.whenCompleted { s => promise.success(s) }
    promise.future
  }

  describe("AsyncFunSpec") {

    it("can be used for tests that return a Future") {

      class ExampleSpec extends AsyncFunSpec with Expectations {

        // SKIP-SCALATESTJS-START
        implicit val executionContext = scala.concurrent.ExecutionContext.Implicits.global
        // SKIP-SCALATESTJS-END
        //SCALATESTJS-ONLY implicit val executionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

        val a = 1

        it("test 1") {
          Future {
            assert(a == 1)
          }
        }

        it("test 2") {
          Future {
            assert(a == 2)
          }
        }

        it("test 3") {
          Future {
            pending
          }
        }

        it("test 4") {
          Future {
            cancel
          }
        }

        ignore("test 5") {
          Future {
            cancel
          }
        }

        it("test 6") {
          Future {
            assert(a == 1)
          }
        }

        it("test 7") {
          Future {
            assert(a == 22)
          }
        }

        override def newInstance = new ExampleSpec
      }

      val rep = new EventRecordingReporter
      val spec = new ExampleSpec
      val status = spec.run(None, Args(reporter = rep))
      toFuture(status).map { s =>
        assert(rep.testStartingEventsReceived.length == 6)
        assert(rep.testSucceededEventsReceived.length == 2)
        assert(rep.testSucceededEventsReceived(0).testName == "test 1")
        assert(rep.testSucceededEventsReceived(1).testName == "test 6")
        assert(rep.testFailedEventsReceived.length == 2)
        assert {
          val zero = rep.testFailedEventsReceived(0).testName
          val one = rep.testFailedEventsReceived(1).testName
          (zero == "test 2" && one == "test 7") || (zero == "test 7" && one == "test 2")
        }
        assert(rep.testPendingEventsReceived.length == 1)
        assert(rep.testPendingEventsReceived(0).testName == "test 3")
        assert(rep.testCanceledEventsReceived.length == 1)
        assert(rep.testCanceledEventsReceived(0).testName == "test 4")
        assert(rep.testIgnoredEventsReceived.length == 1)
        assert(rep.testIgnoredEventsReceived(0).testName == "test 5")
      }
    }

    it("can be used for tests that did not return Future") {

      class ExampleSpec extends AsyncFunSpec with Expectations {

        // SKIP-SCALATESTJS-START
        implicit val executionContext = scala.concurrent.ExecutionContext.Implicits.global
        // SKIP-SCALATESTJS-END
        //SCALATESTJS-ONLY implicit val executionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

        val a = 1

        it("test 1") {
          assert(a == 1)
        }

        it("test 2") {
          assert(a == 2)
        }

        it("test 3") {
          pending
        }

        it("test 4") {
          cancel
        }

        ignore("test 5") {
          cancel
        }

        it("test 6") {
          expect(a == 1)
        }

        it("test 7") {
          val result = expect(a == 22)
          result
        }

        override def newInstance = new ExampleSpec
      }

      val rep = new EventRecordingReporter
      val spec = new ExampleSpec
      val status = spec.run(None, Args(reporter = rep))
      toFuture(status).map { s =>
        assert(rep.testStartingEventsReceived.length == 6)
        assert(rep.testSucceededEventsReceived.length == 2)
        assert(rep.testSucceededEventsReceived(0).testName == "test 1")
        assert(rep.testSucceededEventsReceived(1).testName == "test 6")
        assert(rep.testFailedEventsReceived.length == 2)
        assert {
          val zero = rep.testFailedEventsReceived(0).testName
          val one = rep.testFailedEventsReceived(1).testName
          (zero == "test 2" && one == "test 7") || (zero == "test 7" && one == "test 2")
        }
        assert(rep.testPendingEventsReceived.length == 1)
        assert(rep.testPendingEventsReceived(0).testName == "test 3")
        assert(rep.testCanceledEventsReceived.length == 1)
        assert(rep.testCanceledEventsReceived(0).testName == "test 4")
        assert(rep.testIgnoredEventsReceived.length == 1)
        assert(rep.testIgnoredEventsReceived(0).testName == "test 5")
      }
    }

    it("should run tests that return Future in serial when oneAfterAnotherAsync is set to true") {

      @volatile var count = 0

      class ExampleSpec extends AsyncFunSpec {

        override protected val oneAfterAnotherAsync: Boolean = true

        // SKIP-SCALATESTJS-START
        implicit val executionContext = scala.concurrent.ExecutionContext.Implicits.global
        // SKIP-SCALATESTJS-END
        //SCALATESTJS-ONLY implicit val executionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

        it("test 1") {
          Future {
            SleepHelper.sleep(30)
            assert(count == 0)
            count = 1
            Succeeded
          }
        }

        it("test 2") {
          Future {
            assert(count == 1)
            SleepHelper.sleep(50)
            count = 2
            Succeeded
          }
        }

        it("test 3") {
          Future {
            assert(count == 2)
          }
        }

        override def newInstance = new ExampleSpec

      }

      val rep = new EventRecordingReporter
      val suite = new ExampleSpec
      val status = suite.run(None, Args(reporter = rep))
      toFuture(status).map { s =>
        assert(rep.testStartingEventsReceived.length == 3)
        assert(rep.testSucceededEventsReceived.length == 3)
      }
    }

    it("should run tests that does not return Future in serial when oneAfterAnotherAsync is set to true") {

      @volatile var count = 0

      class ExampleSpec extends AsyncFunSpec {

        override protected val oneAfterAnotherAsync: Boolean = true

        // SKIP-SCALATESTJS-START
        implicit val executionContext = scala.concurrent.ExecutionContext.Implicits.global
        // SKIP-SCALATESTJS-END
        //SCALATESTJS-ONLY implicit val executionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

        it("test 1") {
          SleepHelper.sleep(30)
          assert(count == 0)
          count = 1
          Succeeded
        }

        it("test 2") {
          assert(count == 1)
          SleepHelper.sleep(50)
          count = 2
          Succeeded
        }

        it("test 3") {
          assert(count == 2)
        }

        override def newInstance = new ExampleSpec

      }

      val rep = new EventRecordingReporter
      val suite = new ExampleSpec
      val status = suite.run(None, Args(reporter = rep))
      toFuture(status).map { s =>
        assert(rep.testStartingEventsReceived.length == 3)
        assert(rep.testSucceededEventsReceived.length == 3)
      }
    }
  }
}
