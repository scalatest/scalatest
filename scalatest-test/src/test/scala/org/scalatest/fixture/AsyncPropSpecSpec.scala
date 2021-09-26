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
/*package org.scalatest.fixture

import org.scalatest._
import SharedHelpers.EventRecordingReporter
import scala.concurrent.{Promise, ExecutionContext, Future}
import org.scalatest.concurrent.SleepHelper

import scala.util.Success

class AsyncPropSpecSpec extends org.scalatest.FunSpec {

  describe("AsyncPropSpec") {

    it("can be used for tests that return Future under parallel async test execution") {

      class ExampleSpec extends AsyncPropSpec with ParallelTestExecution {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        type FixtureParam = String
        def withFixture(test: OneArgAsyncTest): FutureOutcome =
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
      // SKIP-SCALATESTJS,NATIVE-START
      status.waitUntilCompleted()
      // SKIP-SCALATESTJS,NATIVE-END
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

      class ExampleSpec extends AsyncPropSpec with ParallelTestExecution {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        type FixtureParam = String
        def withFixture(test: OneArgAsyncTest): FutureOutcome =
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
      // SKIP-SCALATESTJS,NATIVE-START
      status.waitUntilCompleted()
      // SKIP-SCALATESTJS,NATIVE-END
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

      class ExampleSpec extends AsyncPropSpec {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        type FixtureParam = String
        def withFixture(test: OneArgAsyncTest): FutureOutcome =
          test("testing")

        property("test 1") { fixture =>
          Future {
            SleepHelper.sleep(30)
            assert(count == 0)
            count = 1
            succeed
          }
        }

        property("test 2") { fixture =>
          Future {
            assert(count == 1)
            SleepHelper.sleep(50)
            count = 2
            succeed
          }
        }

        property("test 3") { fixture =>
          Future {
            assert(count == 2)
          }
        }

      }

      val rep = new EventRecordingReporter
      val suite = new ExampleSpec
      val status = suite.run(None, Args(reporter = rep))
      // SKIP-SCALATESTJS,NATIVE-START
      status.waitUntilCompleted()
      // SKIP-SCALATESTJS,NATIVE-END

      assert(rep.testStartingEventsReceived.length == 3)
      assert(rep.testSucceededEventsReceived.length == 3)

    }

    it("should run tests that does not return Future in serial by default") {

      @volatile var count = 0

      class ExampleSpec extends AsyncPropSpec {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        type FixtureParam = String
        def withFixture(test: OneArgAsyncTest): FutureOutcome =
          test("testing")

        property("test 1") { fixture =>
          SleepHelper.sleep(30)
          assert(count == 0)
          count = 1
          succeed
        }

        property("test 2") { fixture =>
          assert(count == 1)
          SleepHelper.sleep(50)
          count = 2
          succeed
        }

        property("test 3") { fixture =>
          assert(count == 2)
        }

      }

      val rep = new EventRecordingReporter
      val suite = new ExampleSpec
      val status = suite.run(None, Args(reporter = rep))
      // SKIP-SCALATESTJS,NATIVE-START
      status.waitUntilCompleted()
      // SKIP-SCALATESTJS,NATIVE-END

      assert(rep.testStartingEventsReceived.length == 3)
      assert(rep.testSucceededEventsReceived.length == 3)

    }

    // SKIP-SCALATESTJS,NATIVE-START
    it("should run tests and its future in same main thread when use SerialExecutionContext") {

      var mainThread = Thread.currentThread
      var test1Thread: Option[Thread] = None
      var test2Thread: Option[Thread] = None
      var onCompleteThread: Option[Thread] = None

      class ExampleSpec extends AsyncPropSpec {

        type FixtureParam = String
        def withFixture(test: OneArgAsyncTest): FutureOutcome =
          test("testing")

        property("test 1") { fixture =>
          Future {
            test1Thread = Some(Thread.currentThread)
            succeed
          }
        }

        property("test 2") { fixture =>
          Future {
            test2Thread = Some(Thread.currentThread)
            succeed
          }
        }

      }

      val rep = new EventRecordingReporter
      val suite = new ExampleSpec
      val status = suite.run(None, Args(reporter = rep))
      status.whenCompleted { s =>
        onCompleteThread = Some(Thread.currentThread)
      }
      status.waitUntilCompleted()

      assert(test1Thread.isDefined)
      assert(test1Thread.get == mainThread)
      assert(test2Thread.isDefined)
      assert(test2Thread.get == mainThread)
      assert(onCompleteThread.isDefined)
      assert(onCompleteThread.get == mainThread)
    }

    it("should run tests and its true async future in the same thread when use SerialExecutionContext") {
      var mainThread = Thread.currentThread
      @volatile var test1Thread: Option[Thread] = None
      @volatile var test2Thread: Option[Thread] = None
      var onCompleteThread: Option[Thread] = None

      class ExampleSpec extends AsyncPropSpec {

        type FixtureParam = String
        def withFixture(test: OneArgAsyncTest): FutureOutcome =
          test("testing")

        property("test 1") { fixture =>
          val promise = Promise[Assertion]
          val timer = new java.util.Timer
          timer.schedule(
            new java.util.TimerTask {
              def run(): Unit = {
                promise.complete(Success(succeed))
              }
            },
            1000
          )
          promise.future.map { s =>
            test1Thread = Some(Thread.currentThread)
            s
          }
        }

        property("test 2") { fixture =>
          val promise = Promise[Assertion]
          val timer = new java.util.Timer
          timer.schedule(
            new java.util.TimerTask {
              def run(): Unit = {
                promise.complete(Success(succeed))
              }
            },
            500
          )
          promise.future.map { s =>
            test2Thread = Some(Thread.currentThread)
            s
          }
        }

      }

      val rep = new EventRecordingReporter
      val suite = new ExampleSpec
      val status = suite.run(None, Args(reporter = rep))
      status.whenCompleted { s =>
        onCompleteThread = Some(Thread.currentThread)
      }
      status.waitUntilCompleted()

      assert(test1Thread.isDefined)
      assert(test1Thread.get == mainThread)
      assert(test2Thread.isDefined)
      assert(test2Thread.get == mainThread)
      assert(onCompleteThread.isDefined)
      assert(onCompleteThread.get == mainThread)
    }

    it("should not run out of stack space with nested futures when using SerialExecutionContext") {

      class ExampleSpec extends AsyncPropSpec {

        // Note we get a StackOverflowError with the following execution
        // context.
        // override implicit def executionContext: ExecutionContext = new ExecutionContext { def execute(runnable: Runnable) = runnable.run; def reportFailure(cause: Throwable) = () }

        type FixtureParam = String
        def withFixture(test: OneArgAsyncTest): FutureOutcome =
          test("testing")

        def sum(xs: List[Int]): Future[Int] =
          xs match {
            case Nil => Future.successful(0)
            case x :: xs => Future(x).flatMap(xx => sum(xs).map(xxx => xx + xxx))
          }

        property("test 1") { fixture =>
          val fut: Future[Int] = sum((1 to 50000).toList)
          fut.map(total => assert(total == 1250025000))
        }
      }

      val rep = new EventRecordingReporter
      val suite = new ExampleSpec
      val status = suite.run(None, Args(reporter = rep))
      status.waitUntilCompleted()
      assert(!rep.testSucceededEventsReceived.isEmpty)
    }
    // SKIP-SCALATESTJS,NATIVE-END

    it("should run tests that returns Future and report their result in serial") {

      class ExampleSpec extends AsyncPropSpec {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        type FixtureParam = String
        def withFixture(test: OneArgAsyncTest): FutureOutcome =
          test("testing")

        property("test 1") { fixture =>
          Future {
            SleepHelper.sleep(60)
            succeed
          }
        }

        property("test 2") { fixture =>
          Future {
            SleepHelper.sleep(30)
            succeed
          }
        }

        property("test 3") { fixture =>
          Future {
            succeed
          }
        }

      }

      val rep = new EventRecordingReporter
      val suite = new ExampleSpec
      val status = suite.run(None, Args(reporter = rep))
      // SKIP-SCALATESTJS,NATIVE-START
      status.waitUntilCompleted()
      // SKIP-SCALATESTJS,NATIVE-END

      assert(rep.testStartingEventsReceived.length == 3)
      assert(rep.testStartingEventsReceived(0).testName == "test 1")
      assert(rep.testStartingEventsReceived(1).testName == "test 2")
      assert(rep.testStartingEventsReceived(2).testName == "test 3")
      assert(rep.testSucceededEventsReceived.length == 3)
      assert(rep.testSucceededEventsReceived(0).testName == "test 1")
      assert(rep.testSucceededEventsReceived(1).testName == "test 2")
      assert(rep.testSucceededEventsReceived(2).testName == "test 3")
    }

    it("should run tests that does not return Future and report their result in serial") {

      class ExampleSpec extends AsyncPropSpec {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        type FixtureParam = String
        def withFixture(test: OneArgAsyncTest): FutureOutcome =
          test("testing")

        property("test 1") { fixture =>
          SleepHelper.sleep(60)
          succeed
        }

        property("test 2") { fixture =>
          SleepHelper.sleep(30)
          succeed
        }

        property("test 3") { fixture =>
          succeed
        }

      }

      val rep = new EventRecordingReporter
      val suite = new ExampleSpec
      val status = suite.run(None, Args(reporter = rep))
      // SKIP-SCALATESTJS,NATIVE-START
      status.waitUntilCompleted()
      // SKIP-SCALATESTJS,NATIVE-END

      assert(rep.testStartingEventsReceived.length == 3)
      assert(rep.testStartingEventsReceived(0).testName == "test 1")
      assert(rep.testStartingEventsReceived(1).testName == "test 2")
      assert(rep.testStartingEventsReceived(2).testName == "test 3")
      assert(rep.testSucceededEventsReceived.length == 3)
      assert(rep.testSucceededEventsReceived(0).testName == "test 1")
      assert(rep.testSucceededEventsReceived(1).testName == "test 2")
      assert(rep.testSucceededEventsReceived(2).testName == "test 3")
    }

  }

}*/
