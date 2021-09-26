/*
 * Copyright 2001-2014 Artima, Inc.
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

import org.scalatest.SharedHelpers.{EventRecordingReporter, thisLineNumber}
import scala.concurrent.{Promise, ExecutionContext, Future}
import org.scalatest.concurrent.SleepHelper
import org.scalatest.events.{InfoProvided, MarkupProvided}
import org.scalatest.exceptions.DuplicateTestNameException

import scala.util.Success

class AsyncFlatSpecSpec extends FunSpec {

  describe("AsyncFlatSpec") {

    it("can be used for tests that return Future under parallel async test execution") {

      class ExampleSpec extends AsyncFlatSpec with ParallelTestExecution {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        val a = 1

        it should "test 1" in {
          Future {
            assert(a == 1)
          }
        }

        it should "test 2" in {
          Future {
            assert(a == 2)
          }
        }

        it should "test 3" in {
          Future {
            pending
          }
        }

        it should "test 4" in {
          Future {
            cancel
          }
        }

        it should "test 5" ignore {
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
      assert(rep.testSucceededEventsReceived(0).testName == "should test 1")
      assert(rep.testFailedEventsReceived.length == 1)
      assert(rep.testFailedEventsReceived(0).testName == "should test 2")
      assert(rep.testPendingEventsReceived.length == 1)
      assert(rep.testPendingEventsReceived(0).testName == "should test 3")
      assert(rep.testCanceledEventsReceived.length == 1)
      assert(rep.testCanceledEventsReceived(0).testName == "should test 4")
      assert(rep.testIgnoredEventsReceived.length == 1)
      assert(rep.testIgnoredEventsReceived(0).testName == "should test 5")
    }

    it("can be used for tests that did not return Future under parallel async test execution") {

      class ExampleSpec extends AsyncFlatSpec with ParallelTestExecution {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        val a = 1

        it should "test 1" in {
          assert(a == 1)
        }

        it should "test 2" in {
          assert(a == 2)
        }

        it should "test 3" in {
          pending
        }

        it should "test 4" in {
          cancel
        }

        it should "test 5" ignore {
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
      assert(rep.testSucceededEventsReceived(0).testName == "should test 1")
      assert(rep.testFailedEventsReceived.length == 1)
      assert(rep.testFailedEventsReceived(0).testName == "should test 2")
      assert(rep.testPendingEventsReceived.length == 1)
      assert(rep.testPendingEventsReceived(0).testName == "should test 3")
      assert(rep.testCanceledEventsReceived.length == 1)
      assert(rep.testCanceledEventsReceived(0).testName == "should test 4")
      assert(rep.testIgnoredEventsReceived.length == 1)
      assert(rep.testIgnoredEventsReceived(0).testName == "should test 5")
    }

    it("can be used with is for pending tests that don't return a Future") {

      class ExampleSpec extends AsyncFlatSpec {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        val a = 1

        it should "test 1" is {
          pending
        }

        it should "test 2" ignore {
          pending
        }
      }

      val rep = new EventRecordingReporter
      val spec = new ExampleSpec
      val status = spec.run(None, Args(reporter = rep))
      // SKIP-SCALATESTJS,NATIVE-START
      status.waitUntilCompleted()
      // SKIP-SCALATESTJS,NATIVE-END
      assert(rep.testStartingEventsReceived.length == 1)
      assert(rep.testSucceededEventsReceived.length == 0)
      assert(rep.testFailedEventsReceived.length == 0)
      assert(rep.testPendingEventsReceived.length == 1)
      assert(rep.testPendingEventsReceived(0).testName == "should test 1")
      assert(rep.testCanceledEventsReceived.length == 0)
      assert(rep.testIgnoredEventsReceived.length == 1)
      assert(rep.testIgnoredEventsReceived(0).testName == "should test 2")
    }

    it("should run tests that return Future in serial by default") {

      @volatile var count = 0

      class ExampleSpec extends AsyncFlatSpec {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        it should "test 1" in {
          Future {
            SleepHelper.sleep(30)
            assert(count == 0)
            count = 1
            succeed
          }
        }

        it should "test 2" in {
          Future {
            assert(count == 1)
            SleepHelper.sleep(50)
            count = 2
            succeed
          }
        }

        it should "test 3" in {
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

      class ExampleSpec extends AsyncFlatSpec {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        it should "test 1" in {
          SleepHelper.sleep(30)
          assert(count == 0)
          count = 1
          succeed
        }

        it should "test 2" in {
          assert(count == 1)
          SleepHelper.sleep(50)
          count = 2
          succeed
        }

        it should "test 3" in {
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

      class ExampleSpec extends AsyncFlatSpec {

        it should "test 1" in {
          Future {
            test1Thread = Some(Thread.currentThread)
            succeed
          }
        }

        it should "test 2" in {
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

      class ExampleSpec extends AsyncFlatSpec {

        it should "test 1" in {
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

        it should "test 2" in {
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

      class ExampleSpec extends AsyncFlatSpec {

        // Note we get a StackOverflowError with the following execution
        // context.
        // override implicit def executionContext: ExecutionContext = new ExecutionContext { def execute(runnable: Runnable) = runnable.run; def reportFailure(cause: Throwable) = () }

        def sum(xs: List[Int]): Future[Int] =
          xs match {
            case Nil => Future.successful(0)
            case x :: xs => Future(x).flatMap(xx => sum(xs).map(xxx => xx + xxx))
          }

        it should "test 1" in {
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

      class ExampleSpec extends AsyncFlatSpec {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        it should "test 1" in {
          Future {
            SleepHelper.sleep(60)
            succeed
          }
        }

        it should "test 2" in {
          Future {
            SleepHelper.sleep(30)
            succeed
          }
        }

        it should "test 3" in {
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
      assert(rep.testStartingEventsReceived(0).testName == "should test 1")
      assert(rep.testStartingEventsReceived(1).testName == "should test 2")
      assert(rep.testStartingEventsReceived(2).testName == "should test 3")
      assert(rep.testSucceededEventsReceived.length == 3)
      assert(rep.testSucceededEventsReceived(0).testName == "should test 1")
      assert(rep.testSucceededEventsReceived(1).testName == "should test 2")
      assert(rep.testSucceededEventsReceived(2).testName == "should test 3")
    }

    it("should run tests that does not return Future and report their result in serial") {

      class ExampleSpec extends AsyncFlatSpec {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        it should "test 1" in {
          SleepHelper.sleep(60)
          succeed
        }

        it should "test 2" in {
          SleepHelper.sleep(30)
          succeed
        }

        it should "test 3" in {
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
      assert(rep.testStartingEventsReceived(0).testName == "should test 1")
      assert(rep.testStartingEventsReceived(1).testName == "should test 2")
      assert(rep.testStartingEventsReceived(2).testName == "should test 3")
      assert(rep.testSucceededEventsReceived.length == 3)
      assert(rep.testSucceededEventsReceived(0).testName == "should test 1")
      assert(rep.testSucceededEventsReceived(1).testName == "should test 2")
      assert(rep.testSucceededEventsReceived(2).testName == "should test 3")
    }

    it("should send an InfoProvided event for an info in main spec body") {
      class MySuite extends AsyncFlatSpec  {
        info(
          "hi there"
        )
      }
      val suite = new MySuite
      val reporter = new EventRecordingReporter
      val status = suite.run(None, Args(reporter))
      // SKIP-SCALATESTJS,NATIVE-START
      status.waitUntilCompleted()
      // SKIP-SCALATESTJS,NATIVE-END

      val infoList = reporter.infoProvidedEventsReceived

      assert(infoList.size == 1)
      assert(infoList(0).message == "hi there")
    }

    it("should send an InfoProvided event for an info in test body") {
      class MySuite extends AsyncFlatSpec  {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        "test feature" should "test 1" in {
          info("hi there")
          succeed
        }
      }
      val suite = new MySuite
      val reporter = new EventRecordingReporter
      val status = suite.run(None, Args(reporter))
      // SKIP-SCALATESTJS,NATIVE-START
      status.waitUntilCompleted()
      // SKIP-SCALATESTJS,NATIVE-END

      val infoList = reporter.infoProvidedEventsReceived
      assert(infoList.size == 0)

      val testSucceededList = reporter.testSucceededEventsReceived
      assert(testSucceededList.size == 1)
      assert(testSucceededList(0).recordedEvents.size == 1)
      val recordedEvent = testSucceededList(0).recordedEvents(0)
      assert(recordedEvent.isInstanceOf[InfoProvided])
      val infoProvided = recordedEvent.asInstanceOf[InfoProvided]
      assert(infoProvided.message == "hi there")
    }

    it("should send an InfoProvided event for an info in Future returned by scenario body") {
      class MySuite extends AsyncFlatSpec  {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        "test feature" should "test 1" in {
          Future {
            info("hi there")
            succeed
          }
        }
      }
      val suite = new MySuite
      val reporter = new EventRecordingReporter
      val status = suite.run(None, Args(reporter))
      // SKIP-SCALATESTJS,NATIVE-START
      status.waitUntilCompleted()
      // SKIP-SCALATESTJS,NATIVE-END

      val infoList = reporter.infoProvidedEventsReceived
      assert(infoList.size == 0)

      val testSucceededList = reporter.testSucceededEventsReceived
      assert(testSucceededList.size == 1)
      assert(testSucceededList(0).recordedEvents.size == 1)
      val recordedEvent = testSucceededList(0).recordedEvents(0)
      assert(recordedEvent.isInstanceOf[InfoProvided])
      val infoProvided = recordedEvent.asInstanceOf[InfoProvided]
      assert(infoProvided.message == "hi there")
    }

    it("should send a NoteProvided event for a note in main spec body") {
      class MySuite extends AsyncFlatSpec  {
        note(
          "hi there"
        )
      }
      val suite = new MySuite
      val reporter = new EventRecordingReporter
      val status = suite.run(None, Args(reporter))
      // SKIP-SCALATESTJS,NATIVE-START
      status.waitUntilCompleted()
      // SKIP-SCALATESTJS,NATIVE-END

      val noteList = reporter.noteProvidedEventsReceived

      assert(noteList.size == 1)
      assert(noteList(0).message == "hi there")
    }

    it("should send a NoteProvided event for a note in test body") {
      class MySuite extends AsyncFlatSpec  {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        "test feature" should "test 1" in {
          note("hi there")
          succeed
        }
      }
      val suite = new MySuite
      val reporter = new EventRecordingReporter
      val status = suite.run(None, Args(reporter))
      // SKIP-SCALATESTJS,NATIVE-START
      status.waitUntilCompleted()
      // SKIP-SCALATESTJS,NATIVE-END

      val noteList = reporter.noteProvidedEventsReceived
      assert(noteList.size == 1)
      assert(noteList(0).message == "hi there")
    }

    it("should send a NoteProvided event for a note in Future returned by test body") {
      class MySuite extends AsyncFlatSpec  {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        "test feature" should "test 1" in {
          Future {
            note("hi there")
            succeed
          }
        }
      }
      val suite = new MySuite
      val reporter = new EventRecordingReporter
      val status = suite.run(None, Args(reporter))
      // SKIP-SCALATESTJS,NATIVE-START
      status.waitUntilCompleted()
      // SKIP-SCALATESTJS,NATIVE-END

      val noteList = reporter.noteProvidedEventsReceived
      assert(noteList.size == 1)
      assert(noteList(0).message == "hi there")
    }

    it("should send an AlertProvided event for an alert in main spec body") {
      class MySuite extends AsyncFlatSpec  {
        alert(
          "hi there"
        )
      }
      val suite = new MySuite
      val reporter = new EventRecordingReporter
      val status = suite.run(None, Args(reporter))
      // SKIP-SCALATESTJS,NATIVE-START
      status.waitUntilCompleted()
      // SKIP-SCALATESTJS,NATIVE-END

      val alertList = reporter.alertProvidedEventsReceived

      assert(alertList.size == 1)
      assert(alertList(0).message == "hi there")
    }

    it("should send an AlertProvided event for an alert in test body") {
      class MySuite extends AsyncFlatSpec  {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        "test feature" should "test 1" in {
          alert("hi there")
          succeed
        }
      }
      val suite = new MySuite
      val reporter = new EventRecordingReporter
      val status = suite.run(None, Args(reporter))
      // SKIP-SCALATESTJS,NATIVE-START
      status.waitUntilCompleted()
      // SKIP-SCALATESTJS,NATIVE-END

      val alertList = reporter.alertProvidedEventsReceived
      assert(alertList.size == 1)
      assert(alertList(0).message == "hi there")
    }

    it("should send an AlertProvided event for an alert in Future returned by test body") {
      class MySuite extends AsyncFlatSpec  {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        "test feature" should "test 1" in {
          Future {
            alert("hi there")
            succeed
          }
        }
      }
      val suite = new MySuite
      val reporter = new EventRecordingReporter
      val status = suite.run(None, Args(reporter))
      // SKIP-SCALATESTJS,NATIVE-START
      status.waitUntilCompleted()
      // SKIP-SCALATESTJS,NATIVE-END

      val alertList = reporter.alertProvidedEventsReceived
      assert(alertList.size == 1)
      assert(alertList(0).message == "hi there")
    }

    it("should send a MarkupProvided event for a markup in main spec body") {
      class MySuite extends AsyncFlatSpec  {
        markup(
          "hi there"
        )
      }
      val suite = new MySuite
      val reporter = new EventRecordingReporter
      val status = suite.run(None, Args(reporter))
      // SKIP-SCALATESTJS,NATIVE-START
      status.waitUntilCompleted()
      // SKIP-SCALATESTJS,NATIVE-END

      val markupList = reporter.markupProvidedEventsReceived

      assert(markupList.size == 1)
      assert(markupList(0).text == "hi there")
    }

    it("should send a MarkupProvided event for a markup in test body") {
      class MySuite extends AsyncFlatSpec  {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        "test feature" should "test 1" in {
          markup("hi there")
          succeed
        }
      }
      val suite = new MySuite
      val reporter = new EventRecordingReporter
      val status = suite.run(None, Args(reporter))
      // SKIP-SCALATESTJS,NATIVE-START
      status.waitUntilCompleted()
      // SKIP-SCALATESTJS,NATIVE-END

      val markupList = reporter.markupProvidedEventsReceived
      assert(markupList.size == 0)

      val testSucceededList = reporter.testSucceededEventsReceived
      assert(testSucceededList.size == 1)
      assert(testSucceededList(0).recordedEvents.size == 1)
      val recordedEvent = testSucceededList(0).recordedEvents(0)
      assert(recordedEvent.isInstanceOf[MarkupProvided])
      val markupProvided = recordedEvent.asInstanceOf[MarkupProvided]
      assert(markupProvided.text == "hi there")
    }

    it("should send a MarkupProvided event for a markup in Future returned by scenario body") {
      class MySuite extends AsyncFlatSpec  {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        "test feature" should "test 1" in {
          Future {
            markup("hi there")
            succeed
          }
        }
      }
      val suite = new MySuite
      val reporter = new EventRecordingReporter
      val status = suite.run(None, Args(reporter))
      // SKIP-SCALATESTJS,NATIVE-START
      status.waitUntilCompleted()
      // SKIP-SCALATESTJS,NATIVE-END

      val markupList = reporter.markupProvidedEventsReceived
      assert(markupList.size == 0)

      val testSucceededList = reporter.testSucceededEventsReceived
      assert(testSucceededList.size == 1)
      assert(testSucceededList(0).recordedEvents.size == 1)
      val recordedEvent = testSucceededList(0).recordedEvents(0)
      assert(recordedEvent.isInstanceOf[MarkupProvided])
      val markupProvided = recordedEvent.asInstanceOf[MarkupProvided]
      assert(markupProvided.text == "hi there")
    }

    it("should generate a DuplicateTestNameException is detected") {
      class TestSpec extends AsyncFlatSpec {
        behavior of "a feature"
        it should "test 1" in { succeed }
        it should "test 1" in { succeed }
      }
      val e = intercept[DuplicateTestNameException] {
        new TestSpec
      }
      assert("AsyncFlatSpecSpec.scala" == e.failedCodeFileName.get)
      assert(e.failedCodeLineNumber.get == thisLineNumber - 6)
      assert(!e.cause.isDefined)
    }

    it("should allow other execution context to be used") {
      class TestSpec extends AsyncFlatSpec {
        // SKIP-SCALATESTJS,NATIVE-START
        override implicit val executionContext = scala.concurrent.ExecutionContext.Implicits.global
        // SKIP-SCALATESTJS,NATIVE-END
        // SCALATESTJS-ONLY override implicit val executionContext = scala.scalajs.concurrent.JSExecutionContext.runNow
        val a = 1
        "feature 1" should "test A" in {
          Future { assert(a == 1) }
        }
        "feature 2" should "test B" in {
          Future { assert(a == 1) }
        }
        "feature 3" should "test C" in {
          Future { assert(a == 1) }
        }
      }
      val suite = new TestSpec
      val reporter = new EventRecordingReporter
      val status = suite.run(None, Args(reporter))

      // SKIP-SCALATESTJS,NATIVE-START
      status.waitUntilCompleted()
      // SKIP-SCALATESTJS,NATIVE-END
      assert(reporter.scopeOpenedEventsReceived.length == 3)
      assert(reporter.scopeClosedEventsReceived.length == 3)
      assert(reporter.testStartingEventsReceived.length == 3)
      assert(reporter.testSucceededEventsReceived.length == 3)
    }

  }

}
