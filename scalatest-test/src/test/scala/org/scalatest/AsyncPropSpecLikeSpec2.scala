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
import scala.concurrent.{ExecutionContext, Promise, Future}
import org.scalatest.concurrent.SleepHelper

import scala.util.Success

class AsyncPropSpecLikeSpec2 extends AsyncFunSpec {

  describe("AsyncPropSpecLike") {

    it("can be used for tests that return Future under parallel async test execution") {

      class ExampleSpec extends AsyncPropSpecLike with ParallelTestExecution {

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

      class ExampleSpec extends AsyncPropSpecLike with ParallelTestExecution {

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

      class ExampleSpec extends AsyncPropSpecLike {

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

      class ExampleSpec extends AsyncPropSpecLike {

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

    // SKIP-SCALATESTJS-START
    it("should run tests and its future in same main thread when use SerialExecutionContext") {

      var mainThread = Thread.currentThread
      var test1Thread: Option[Thread] = None
      var test2Thread: Option[Thread] = None
      var onCompleteThread: Option[Thread] = None

      class ExampleSpec extends AsyncPropSpecLike {

        override implicit val executionContext: ExecutionContext = new concurrent.SerialExecutionContext

        property("test 1") {
          Future {
            test1Thread = Some(Thread.currentThread)
            succeed
          }
        }

        property("test 2") {
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

      val promise = Promise[EventRecordingReporter]
      status whenCompleted { _ => promise.success(rep) }
      promise.future.map { repo =>
        assert(test1Thread.isDefined)
        assert(test1Thread.get == mainThread)
        assert(test2Thread.isDefined)
        assert(test2Thread.get == mainThread)
        assert(onCompleteThread.isDefined)
        assert(onCompleteThread.get == mainThread)
      }
    }

    it("should run tests and its true async future in the same thread when use SerialExecutionContext") {
      var mainThread = Thread.currentThread
      @volatile var test1Thread: Option[Thread] = None
      @volatile var test2Thread: Option[Thread] = None
      var onCompleteThread: Option[Thread] = None

      class ExampleSpec extends AsyncPropSpecLike {

        override implicit val executionContext: ExecutionContext = new concurrent.SerialExecutionContext

        property("test 1") {
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

        property("test 2") {
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

      val promise = Promise[EventRecordingReporter]
      status whenCompleted { _ => promise.success(rep) }
      promise.future.map { repo =>
        assert(test1Thread.isDefined)
        assert(test1Thread.get == mainThread)
        assert(test2Thread.isDefined)
        assert(test2Thread.get == mainThread)
        assert(onCompleteThread.isDefined)
        assert(onCompleteThread.get == mainThread)
      }
    }

    it("should not run out of stack space with nested futures when using SerialExecutionContext") {

      class ExampleSpec extends AsyncPropSpecLike {

        // Note we get a StackOverflowError with the following execution
        // context.
        // override implicit val executionContext: ExecutionContext = new ExecutionContext { def execute(runnable: Runnable) = runnable.run; def reportFailure(cause: Throwable) = () }
        override implicit val executionContext: ExecutionContext = new concurrent.SerialExecutionContext

        def sum(xs: List[Int]): Future[Int] =
          xs match {
            case Nil => Future.successful(0)
            case x :: xs => Future(x).flatMap(xx => sum(xs).map(xxx => xx + xxx))
          }

        property("test 1") {
          val fut: Future[Int] = sum((1 to 50000).toList)
          fut.map(total => assert(total == 1250025000))
        }
      }

      val rep = new EventRecordingReporter
      val suite = new ExampleSpec
      val status = suite.run(None, Args(reporter = rep))

      val promise = Promise[EventRecordingReporter]
      status whenCompleted { _ => promise.success(rep) }
      promise.future.map { repo =>
        assert(!rep.testSucceededEventsReceived.isEmpty)
      }
    }
    // SKIP-SCALATESTJS-END

    it("should run tests that returns Future and report their result in serial") {

      class ExampleSpec extends AsyncPropSpecLike {

        property("test 1") {
          Future {
            SleepHelper.sleep(60)
            succeed
          }
        }

        property("test 2") {
          Future {
            SleepHelper.sleep(30)
            succeed
          }
        }

        property("test 3") {
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

      val promise = Promise[EventRecordingReporter]
      status whenCompleted { _ => promise.success(rep) }
      promise.future.map { repo =>
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

    it("should run tests that does not return Future and report their result in serial") {

      class ExampleSpec extends AsyncPropSpecLike {

        property("test 1") {
          SleepHelper.sleep(60)
          succeed
        }

        property("test 2") {
          SleepHelper.sleep(30)
          succeed
        }

        property("test 3") {
          succeed
        }

      }

      val rep = new EventRecordingReporter
      val suite = new ExampleSpec
      val status = suite.run(None, Args(reporter = rep))
      // SKIP-SCALATESTJS-START
      status.waitUntilCompleted()
      // SKIP-SCALATESTJS-END

      val promise = Promise[EventRecordingReporter]
      status whenCompleted { _ => promise.success(rep) }
      promise.future.map { repo =>
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

  }

}
