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
import scala.concurrent.Future
import org.scalatest.concurrent.SleepHelper

class AsyncFeatureSpecLikeSpec extends FunSpec {

  describe("AsyncFeatureSpecLike") {

    it("can be used for tests that return Future under parallel async test execution") {

      class ExampleSpec extends AsyncFeatureSpecLike with ParallelTestExecution {

        //SCALATESTJS-ONLY implicit override val executionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow

        val a = 1

        scenario("test 1") {
          Future {
            assert(a == 1)
          }
        }

        scenario("test 2") {
          Future {
            assert(a == 2)
          }
        }

        scenario("test 3") {
          Future {
            pending
          }
        }

        scenario("test 4") {
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
      // SKIP-SCALATESTJS-START
      status.waitUntilCompleted()
      // SKIP-SCALATESTJS-END
      //Thread.sleep(3000)
      assert(rep.testStartingEventsReceived.length == 4)
      assert(rep.testSucceededEventsReceived.length == 1)
      assert(rep.testSucceededEventsReceived(0).testName == "Scenario: test 1")
      assert(rep.testFailedEventsReceived.length == 1)
      assert(rep.testFailedEventsReceived(0).testName == "Scenario: test 2")
      assert(rep.testPendingEventsReceived.length == 1)
      assert(rep.testPendingEventsReceived(0).testName == "Scenario: test 3")
      assert(rep.testCanceledEventsReceived.length == 1)
      assert(rep.testCanceledEventsReceived(0).testName == "Scenario: test 4")
      assert(rep.testIgnoredEventsReceived.length == 1)
      assert(rep.testIgnoredEventsReceived(0).testName == "Scenario: test 5")
    }

    it("can be used for tests that did not return Future under parallel async test execution") {

      class ExampleSpec extends AsyncFeatureSpecLike with ParallelTestExecution {

        //SCALATESTJS-ONLY implicit override val executionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow

        val a = 1

        scenario("test 1") {
          assert(a == 1)
        }

        scenario("test 2") {
          assert(a == 2)
        }

        scenario("test 3") {
          pending
        }

        scenario("test 4") {
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
      // SKIP-SCALATESTJS-START
      status.waitUntilCompleted()
      // SKIP-SCALATESTJS-END
      assert(rep.testStartingEventsReceived.length == 4)
      assert(rep.testSucceededEventsReceived.length == 1)
      assert(rep.testSucceededEventsReceived(0).testName == "Scenario: test 1")
      assert(rep.testFailedEventsReceived.length == 1)
      assert(rep.testFailedEventsReceived(0).testName == "Scenario: test 2")
      assert(rep.testPendingEventsReceived.length == 1)
      assert(rep.testPendingEventsReceived(0).testName == "Scenario: test 3")
      assert(rep.testCanceledEventsReceived.length == 1)
      assert(rep.testCanceledEventsReceived(0).testName == "Scenario: test 4")
      assert(rep.testIgnoredEventsReceived.length == 1)
      assert(rep.testIgnoredEventsReceived(0).testName == "Scenario: test 5")
    }

    it("should run tests that return Future in serial by default") {

      @volatile var count = 0

      class ExampleSpec extends AsyncFeatureSpecLike {

        //SCALATESTJS-ONLY implicit override val executionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow

        scenario("test 1") {
          Future {
            SleepHelper.sleep(30)
            assert(count == 0)
            count = 1
            succeed
          }
        }

        scenario("test 2") {
          Future {
            assert(count == 1)
            SleepHelper.sleep(50)
            count = 2
            succeed
          }
        }

        scenario("test 3") {
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

      class ExampleSpec extends AsyncFeatureSpecLike {

        //SCALATESTJS-ONLY implicit override val executionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow

        scenario("test 1") {
          SleepHelper.sleep(30)
          assert(count == 0)
          count = 1
          succeed
        }

        scenario("test 2") {
          assert(count == 1)
          SleepHelper.sleep(50)
          count = 2
          succeed
        }

        scenario("test 3") {
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

    // SKIP-SCALATESTJS-START
    it("should run tests and its future in same main thread") {

      var mainThread = Thread.currentThread
      var test1Thread: Option[Thread] = None
      var test2Thread: Option[Thread] = None

      class ExampleSpec extends AsyncFeatureSpecLike {

        scenario("test 1") {
          Future {
            test1Thread = Some(Thread.currentThread)
            succeed
          }
        }

        scenario("test 2") {
          Future {
            test2Thread = Some(Thread.currentThread)
            succeed
          }
        }

      }

      val rep = new EventRecordingReporter
      val suite = new ExampleSpec
      val status = suite.run(None, Args(reporter = rep))
      status.waitUntilCompleted()

      assert(test1Thread.isDefined)
      assert(test1Thread.get == mainThread)
      assert(test2Thread.isDefined)
      assert(test2Thread.get == mainThread)
    }
    // SKIP-SCALATESTJS-END

  }

}
