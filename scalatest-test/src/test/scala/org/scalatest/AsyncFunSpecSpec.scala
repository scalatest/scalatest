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

class AsyncFunSpecSpec extends FunSpec {

  describe("AsyncFunSpec") {

    it("can be used for tests that return a Future under parallel async test execution") {

      class ExampleSpec extends AsyncFunSpec with ParallelAsyncTestExecution with Expectations {

        // SKIP-SCALATESTJS-START
        implicit val executionContext = scala.concurrent.ExecutionContext.Implicits.global
        // SKIP-SCALATESTJS-END
        //SCALATESTJS-ONLY implicit val executionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow

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
            expect(a == 1)
          }
        }

        it("test 7") {
          Future {
            expect(a == 22)
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

    it("can be used for tests that did not return Future under parallel async test execution") {

      class ExampleSpec extends AsyncFunSpec with ParallelAsyncTestExecution with Expectations {

        // SKIP-SCALATESTJS-START
        implicit val executionContext = scala.concurrent.ExecutionContext.Implicits.global
        // SKIP-SCALATESTJS-END
        //SCALATESTJS-ONLY implicit val executionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow

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
      // SKIP-SCALATESTJS-START
      status.waitUntilCompleted()
      // SKIP-SCALATESTJS-END
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

    it("should run tests that return Future in serial by default") {

      @volatile var count = 0

      class ExampleSpec extends AsyncFunSpec {

        // SKIP-SCALATESTJS-START
        implicit val executionContext = scala.concurrent.ExecutionContext.Implicits.global
        // SKIP-SCALATESTJS-END
        //SCALATESTJS-ONLY implicit val executionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow

        it("test 1") {
          Future {
            SleepHelper.sleep(30)
            assert(count == 0)
            count = 1
          }
        }

        it("test 2") {
          Future {
            assert(count == 1)
            SleepHelper.sleep(50)
            count = 2
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
      // SKIP-SCALATESTJS-START
      status.waitUntilCompleted()
      // SKIP-SCALATESTJS-END

      assert(rep.testStartingEventsReceived.length == 3)
      assert(rep.testSucceededEventsReceived.length == 3)

    }

    it("should run tests that does not return Future in serial by default") {

      @volatile var count = 0

      class ExampleSpec extends AsyncFunSpec {

        // SKIP-SCALATESTJS-START
        implicit val executionContext = scala.concurrent.ExecutionContext.Implicits.global
        // SKIP-SCALATESTJS-END
        //SCALATESTJS-ONLY implicit val executionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow

        it("test 1") {
          SleepHelper.sleep(30)
          assert(count == 0)
          count = 1
        }

        it("test 2") {
          assert(count == 1)
          SleepHelper.sleep(50)
          count = 2
        }

        it("test 3") {
          assert(count == 2)
        }

        override def newInstance = new ExampleSpec

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
