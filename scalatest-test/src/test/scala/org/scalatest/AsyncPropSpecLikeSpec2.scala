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

class AsyncPropSpecLikeSpec2 extends AsyncFunSpec {

  // SKIP-SCALATESTJS-START
  implicit val executionContext = scala.concurrent.ExecutionContext.Implicits.global
  // SKIP-SCALATESTJS-END
  //SCALATESTJS-ONLY implicit val executionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

  override def newInstance = new AsyncPropSpecLikeSpec2

  describe("AsyncPropSpecLike") {

    it("can be used for tests that return Future") {

      class ExampleSpec extends AsyncPropSpecLike {

        // SKIP-SCALATESTJS-START
        implicit val executionContext = scala.concurrent.ExecutionContext.Implicits.global
        // SKIP-SCALATESTJS-END
        //SCALATESTJS-ONLY implicit val executionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

        val a = 1

        property("test 1") {
          Future {
            assert(a == 1)
          }
        }

        property("test 2") {
          Future {
            assert(a == 2)
          }
        }

        property("test 3") {
          Future {
            pending
          }
        }

        property("test 4") {
          Future {
            cancel
          }
        }

        ignore("test 5") {
          Future {
            cancel
          }
        }

        override def newInstance = new ExampleSpec
      }

      val rep = new EventRecordingReporter
      val spec = new ExampleSpec
      val status = spec.run(None, Args(reporter = rep))
      status.toFuture.map { s =>
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

    it("can be used for tests that did not return Future") {

      class ExampleSpec extends AsyncPropSpecLike {

        // SKIP-SCALATESTJS-START
        implicit val executionContext = scala.concurrent.ExecutionContext.Implicits.global
        // SKIP-SCALATESTJS-END
        //SCALATESTJS-ONLY implicit val executionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

        val a = 1

        property("test 1") {
          assert(a == 1)
        }

        property("test 2") {
          assert(a == 2)
        }

        property("test 3") {
          pending
        }

        property("test 4") {
          cancel
        }

        ignore("test 5") {
          cancel
        }

        override def newInstance = new ExampleSpec
      }

      val rep = new EventRecordingReporter
      val spec = new ExampleSpec
      val status = spec.run(None, Args(reporter = rep))
      status.toFuture.map { s =>
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

    it("should run tests that return Future in serial when oneAfterAnotherAsync is set to true") {

      @volatile var count = 0

      class ExampleSpec extends AsyncPropSpecLike {

        override protected val oneAfterAnotherAsync: Boolean = true

        // SKIP-SCALATESTJS-START
        implicit val executionContext = scala.concurrent.ExecutionContext.Implicits.global
        // SKIP-SCALATESTJS-END
        //SCALATESTJS-ONLY implicit val executionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

        property("test 1") {
          Future {
            SleepHelper.sleep(30)
            assert(count == 0)
            count = 1
            Succeeded
          }
        }

        property("test 2") {
          Future {
            assert(count == 1)
            SleepHelper.sleep(50)
            count = 2
            Succeeded
          }
        }

        property("test 3") {
          Future {
            assert(count == 2)
          }
        }

        override def newInstance = new ExampleSpec

      }

      val rep = new EventRecordingReporter
      val suite = new ExampleSpec
      val status = suite.run(None, Args(reporter = rep))
      status.toFuture.map { s =>
        assert(rep.testStartingEventsReceived.length == 3)
        assert(rep.testSucceededEventsReceived.length == 3)
      }
    }

    it("should run tests that does not return Future in serial when oneAfterAnotherAsync is set to true") {

      @volatile var count = 0

      class ExampleSpec extends AsyncPropSpecLike {

        override protected val oneAfterAnotherAsync: Boolean = true

        // SKIP-SCALATESTJS-START
        implicit val executionContext = scala.concurrent.ExecutionContext.Implicits.global
        // SKIP-SCALATESTJS-END
        //SCALATESTJS-ONLY implicit val executionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

        property("test 1") {
          SleepHelper.sleep(30)
          assert(count == 0)
          count = 1
          Succeeded
        }

        property("test 2") {
          assert(count == 1)
          SleepHelper.sleep(50)
          count = 2
          Succeeded
        }

        property("test 3") {
          assert(count == 2)
        }

        override def newInstance = new ExampleSpec

      }

      val rep = new EventRecordingReporter
      val suite = new ExampleSpec
      val status = suite.run(None, Args(reporter = rep))
      status.toFuture.map { s =>
        assert(rep.testStartingEventsReceived.length == 3)
        assert(rep.testSucceededEventsReceived.length == 3)
      }
    }

  }

}
