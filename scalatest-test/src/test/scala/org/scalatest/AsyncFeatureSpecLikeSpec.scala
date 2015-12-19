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
import scala.concurrent.{Promise, ExecutionContext, Future}
import org.scalatest.concurrent.SleepHelper

import scala.util.Success

class AsyncFeatureSpecLikeSpec extends FunSpec {

  describe("AsyncFeatureSpecLike") {

    it("can be used for tests that return Future under parallel async test execution") {

      class ExampleSpec extends AsyncFeatureSpecLike with ParallelTestExecution {

        //SCALATESTJS-ONLY implicit override def executionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow

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

        //SCALATESTJS-ONLY implicit override def executionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow

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

        //SCALATESTJS-ONLY implicit override def executionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow

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

        //SCALATESTJS-ONLY implicit override def executionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow

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
    it("should run tests and its future in same main thread when use SerialExecutionContext") {

      var mainThread = Thread.currentThread
      var test1Thread: Option[Thread] = None
      var test2Thread: Option[Thread] = None
      var onCompleteThread: Option[Thread] = None

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

      class ExampleSpec extends AsyncFeatureSpecLike {

        scenario("test 1") {
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

        scenario("test 2") {
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

    it("should not hang if a Future completes with nothing on the queue") {
      // Despite not having a notify registered as a callback on the Future
      // passed to SerialExecutionContext.runNow, it is not possible to get
      // a test to hang, because there's always one more transformation after
      // the Future[Assertion] completes, and that's the one that transforms
      // the Future[Assertion] to a Future[Outcome]. That will cause a
      // job to be enqueued via SerialExecutionContext.execute, and that
      // will do the final notify. After running that job, the Future passed
      // to runNow will be complete, and runNow will return.
      class ExampleSpec extends AsyncFeatureSpecLike {

        scenario("test 1") {
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
          promise.future
        }
      }

      val rep = new EventRecordingReporter
      val suite = new ExampleSpec
      val status = suite.run(None, Args(reporter = rep))
      status.waitUntilCompleted()
      assert(!rep.testSucceededEventsReceived.isEmpty)
    }

    it("should not run out of stack space with nested futures when using SerialExecutionContext") {

      class ExampleSpec extends AsyncFeatureSpecLike {

        // Note we get a StackOverflowError with the following execution
        // context.
        // override implicit def executionContext: ExecutionContext = new ExecutionContext { def execute(runnable: Runnable) = runnable.run; def reportFailure(cause: Throwable) = () }

        def sum(xs: List[Int]): Future[Int] = 
          xs match {
            case Nil => Future.successful(0)
            case x :: xs => Future(x).flatMap(xx => sum(xs).map(xxx => xx + xxx))
          }

        scenario("test 1") {
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
    // SKIP-SCALATESTJS-END

    it("should run tests that returns Future and report their result in serial") {

      class ExampleSpec extends AsyncFeatureSpecLike {

        //SCALATESTJS-ONLY implicit override def executionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow

        scenario("test 1") {
          Future {
            SleepHelper.sleep(60)
            succeed
          }
        }

        scenario("test 2") {
          Future {
            SleepHelper.sleep(30)
            succeed
          }
        }

        scenario("test 3") {
          Future {
            succeed
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
      assert(rep.testStartingEventsReceived(0).testName == "Scenario: test 1")
      assert(rep.testStartingEventsReceived(1).testName == "Scenario: test 2")
      assert(rep.testStartingEventsReceived(2).testName == "Scenario: test 3")
      assert(rep.testSucceededEventsReceived.length == 3)
      assert(rep.testSucceededEventsReceived(0).testName == "Scenario: test 1")
      assert(rep.testSucceededEventsReceived(1).testName == "Scenario: test 2")
      assert(rep.testSucceededEventsReceived(2).testName == "Scenario: test 3")
    }

    it("should run tests that does not return Future and report their result in serial") {

      class ExampleSpec extends AsyncFeatureSpecLike {

        //SCALATESTJS-ONLY implicit override def executionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow

        scenario("test 1") {
          SleepHelper.sleep(60)
          succeed
        }

        scenario("test 2") {
          SleepHelper.sleep(30)
          succeed
        }

        scenario("test 3") {
          succeed
        }

      }

      val rep = new EventRecordingReporter
      val suite = new ExampleSpec
      val status = suite.run(None, Args(reporter = rep))
      // SKIP-SCALATESTJS-START
      status.waitUntilCompleted()
      // SKIP-SCALATESTJS-END

      assert(rep.testStartingEventsReceived.length == 3)
      assert(rep.testStartingEventsReceived(0).testName == "Scenario: test 1")
      assert(rep.testStartingEventsReceived(1).testName == "Scenario: test 2")
      assert(rep.testStartingEventsReceived(2).testName == "Scenario: test 3")
      assert(rep.testSucceededEventsReceived.length == 3)
      assert(rep.testSucceededEventsReceived(0).testName == "Scenario: test 1")
      assert(rep.testSucceededEventsReceived(1).testName == "Scenario: test 2")
      assert(rep.testSucceededEventsReceived(2).testName == "Scenario: test 3")
    }

    it("should report as failed test when non-fatal exception is thrown from scenario body") {

      class ExampleSpec extends AsyncFeatureSpecLike {

        //SCALATESTJS-ONLY implicit override def executionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow

        scenario("test 1") {
          succeed
        }

        scenario("test 2") {
          throw new IllegalStateException("oops!")
        }

        scenario("test 3") {
          succeed
        }

      }

      val rep = new EventRecordingReporter
      val suite = new ExampleSpec
      val status = suite.run(None, Args(reporter = rep))
      // SKIP-SCALATESTJS-START
      status.waitUntilCompleted()
      // SKIP-SCALATESTJS-END

      assert(rep.testStartingEventsReceived.length == 3)
      assert(rep.testStartingEventsReceived(0).testName == "Scenario: test 1")
      assert(rep.testStartingEventsReceived(1).testName == "Scenario: test 2")
      assert(rep.testStartingEventsReceived(2).testName == "Scenario: test 3")
      assert(rep.testSucceededEventsReceived.length == 2)
      assert(rep.testSucceededEventsReceived(0).testName == "Scenario: test 1")
      assert(rep.testSucceededEventsReceived(1).testName == "Scenario: test 3")
      assert(rep.testFailedEventsReceived.length == 1)
      assert(rep.testFailedEventsReceived(0).testName == "Scenario: test 2")

    }

    it("should report as failed test when non-fatal exception is thrown from Future returned by scenario body") {

      class ExampleSpec extends AsyncFeatureSpecLike {

        //SCALATESTJS-ONLY implicit override def executionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow

        scenario("test 1") {
          succeed
        }

        scenario("test 2") {
          Future {
            throw new IllegalStateException("oops!")
          }
        }

        scenario("test 3") {
          succeed
        }

      }

      val rep = new EventRecordingReporter
      val suite = new ExampleSpec
      val status = suite.run(None, Args(reporter = rep))
      // SKIP-SCALATESTJS-START
      status.waitUntilCompleted()
      // SKIP-SCALATESTJS-END

      assert(rep.testStartingEventsReceived.length == 3)
      assert(rep.testStartingEventsReceived(0).testName == "Scenario: test 1")
      assert(rep.testStartingEventsReceived(1).testName == "Scenario: test 2")
      assert(rep.testStartingEventsReceived(2).testName == "Scenario: test 3")
      assert(rep.testSucceededEventsReceived.length == 2)
      assert(rep.testSucceededEventsReceived(0).testName == "Scenario: test 1")
      assert(rep.testSucceededEventsReceived(1).testName == "Scenario: test 3")
      assert(rep.testFailedEventsReceived.length == 1)
      assert(rep.testFailedEventsReceived(0).testName == "Scenario: test 2")

    }

    // SKIP-SCALATESTJS-START
    it("should propagate fatal exception when thrown from scenario body") {

      class ExampleSpec extends AsyncFeatureSpecLike {

        //SCALATESTJS-ONLY implicit override def executionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow

        scenario("test 1") {
          succeed
        }

        scenario("test 2") {
          throw new VirtualMachineError("oops!") {}
        }

        scenario("test 3") {
          succeed
        }

      }

      val rep = new EventRecordingReporter
      assertThrows[VirtualMachineError] {
        val suite = new ExampleSpec
        suite.run(None, Args(reporter = rep))
      }
    }

    it("should propagate fatal exception when thrown from Future returned by scenario body") {

      class ExampleSpec extends AsyncFeatureSpecLike {

        //SCALATESTJS-ONLY implicit override def executionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow

        scenario("test 1") {
          succeed
        }

        scenario("test 2") {
          Future {
            throw new VirtualMachineError("oops!") {}
          }
        }

        scenario("test 3") {
          succeed
        }

      }

      val rep = new EventRecordingReporter
      assertThrows[VirtualMachineError] {
        val suite = new ExampleSpec
        suite.run(None, Args(reporter = rep))
      }
    }
    // SKIP-SCALATESTJS-END

  }

}
