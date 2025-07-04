/*
 * Copyright 2001-2024 Artima, Inc.
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
package org.scalatest.funsuite

import scala.concurrent.{Promise, ExecutionContext, Future}
import org.scalatest._
import SharedHelpers.{EventRecordingReporter, thisLineNumber}
import org.scalatest.concurrent.SleepHelper
import org.scalatest.events.{InfoProvided, MarkupProvided}
import org.scalatest.exceptions.DuplicateTestNameException

import scala.util.Success
import org.scalatest

class FixtureAsyncFunSuiteSpec extends scalatest.funspec.AnyFunSpec {

  describe("AsyncFunSuite") {

    it("can be used for tests that return Future under parallel async test execution") {

      class ExampleSuite extends funsuite.FixtureAsyncFunSuite with ParallelTestExecution {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        type FixtureParam = String
        def withFixture(test: OneArgAsyncTest): FutureOutcome =
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
            cancel()
          }
        }

        ignore("test 5") { fixture =>
          Future {
            cancel()
          }
        }

        override def newInstance = new ExampleSuite
      }

      val rep = new EventRecordingReporter
      val suite = new ExampleSuite
      val status = suite.run(None, Args(reporter = rep))
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

      class ExampleSuite extends funsuite.FixtureAsyncFunSuite with ParallelTestExecution {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        type FixtureParam = String
        def withFixture(test: OneArgAsyncTest): FutureOutcome =
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
          cancel()
        }

        ignore("test 5") { fixture =>
          cancel()
        }

        override def newInstance = new ExampleSuite
      }

      val rep = new EventRecordingReporter
      val suite = new ExampleSuite
      val status = suite.run(None, Args(reporter = rep))
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

      class ExampleSuite extends funsuite.FixtureAsyncFunSuite {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        type FixtureParam = String
        def withFixture(test: OneArgAsyncTest): FutureOutcome =
          test("testing")

        test("test 1") { fixture =>
          Future {
            SleepHelper.sleep(30)
            assert(count == 0)
            count = 1
            succeed
          }
        }

        test("test 2") { fixture =>
          Future {
            assert(count == 1)
            SleepHelper.sleep(50)
            count = 2
            succeed
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
      // SKIP-SCALATESTJS,NATIVE-START
      status.waitUntilCompleted()
      // SKIP-SCALATESTJS,NATIVE-END

      assert(rep.testStartingEventsReceived.length == 3)
      assert(rep.testSucceededEventsReceived.length == 3)

    }

    it("should run tests that does not return Future in serial by default") {

      @volatile var count = 0

      class ExampleSuite extends funsuite.FixtureAsyncFunSuite {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        type FixtureParam = String
        def withFixture(test: OneArgAsyncTest): FutureOutcome =
          test("testing")

        test("test 1") { fixture =>
          SleepHelper.sleep(30)
          assert(count == 0)
          count = 1
          succeed
        }

        test("test 2") { fixture =>
          assert(count == 1)
          SleepHelper.sleep(50)
          count = 2
          succeed
        }

        test("test 3") { fixture =>
          assert(count == 2)
        }

      }

      val rep = new EventRecordingReporter
      val suite = new ExampleSuite
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

      class ExampleSpec extends funsuite.FixtureAsyncFunSuite {

        type FixtureParam = String
        def withFixture(test: OneArgAsyncTest): FutureOutcome =
          test("testing")

        test("test 1") { fixture =>
          Future {
            test1Thread = Some(Thread.currentThread)
            succeed
          }
        }

        test("test 2") { fixture =>
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

      class ExampleSpec extends funsuite.FixtureAsyncFunSuite {

        type FixtureParam = String
        def withFixture(test: OneArgAsyncTest): FutureOutcome =
          test("testing")

        test("test 1") { fixture =>
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

        test("test 2") { fixture =>
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

      class ExampleSpec extends funsuite.FixtureAsyncFunSuite {

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

        test("test 1") { fixture =>
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

      class ExampleSpec extends funsuite.FixtureAsyncFunSuite {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        type FixtureParam = String
        def withFixture(test: OneArgAsyncTest): FutureOutcome =
          test("testing")

        test("test 1") { fixture =>
          Future {
            SleepHelper.sleep(60)
            succeed
          }
        }

        test("test 2") { fixture =>
          Future {
            SleepHelper.sleep(30)
            succeed
          }
        }

        test("test 3") { fixture =>
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

      class ExampleSpec extends funsuite.FixtureAsyncFunSuite {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        type FixtureParam = String
        def withFixture(test: OneArgAsyncTest): FutureOutcome =
          test("testing")

        test("test 1") { fixture =>
          SleepHelper.sleep(60)
          succeed
        }

        test("test 2") { fixture =>
          SleepHelper.sleep(30)
          succeed
        }

        test("test 3") { fixture =>
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

    it("should send an InfoProvided event for an info in main spec body") {

      class MySuite extends funsuite.FixtureAsyncFunSuite  {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        type FixtureParam = String
        def withFixture(test: OneArgAsyncTest): FutureOutcome =
          test("testing")
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
      class MySuite extends funsuite.FixtureAsyncFunSuite  {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        type FixtureParam = String
        def withFixture(test: OneArgAsyncTest): FutureOutcome =
          test("testing")
        test("test 1") { fixture =>
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

    it("should send an InfoProvided event for an info in Future returned by test body") {
      class MySuite extends funsuite.FixtureAsyncFunSuite  {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        type FixtureParam = String
        def withFixture(test: OneArgAsyncTest): FutureOutcome =
          test("testing")
        test("test 1") { fixture =>
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
      class MySuite extends funsuite.FixtureAsyncFunSuite  {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        type FixtureParam = String
        def withFixture(test: OneArgAsyncTest): FutureOutcome =
          test("testing")
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
      class MySuite extends funsuite.FixtureAsyncFunSuite  {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        type FixtureParam = String
        def withFixture(test: OneArgAsyncTest): FutureOutcome =
          test("testing")
        test("test 1") { fixture =>
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
      class MySuite extends funsuite.FixtureAsyncFunSuite  {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        type FixtureParam = String
        def withFixture(test: OneArgAsyncTest): FutureOutcome =
          test("testing")
        test("test 1") { fixture =>
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
      class MySuite extends funsuite.FixtureAsyncFunSuite  {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        type FixtureParam = String
        def withFixture(test: OneArgAsyncTest): FutureOutcome =
          test("testing")
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
      class MySuite extends funsuite.FixtureAsyncFunSuite  {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        type FixtureParam = String
        def withFixture(test: OneArgAsyncTest): FutureOutcome =
          test("testing")
        test("test 1") { fixture =>
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
      class MySuite extends funsuite.FixtureAsyncFunSuite  {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        type FixtureParam = String
        def withFixture(test: OneArgAsyncTest): FutureOutcome =
          test("testing")
        test("test 1") { fixture =>
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
      class MySuite extends funsuite.FixtureAsyncFunSuite  {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        type FixtureParam = String
        def withFixture(test: OneArgAsyncTest): FutureOutcome =
          test("testing")
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
      class MySuite extends funsuite.FixtureAsyncFunSuite  {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        type FixtureParam = String
        def withFixture(test: OneArgAsyncTest): FutureOutcome =
          test("testing")
        test("test 1") { fixture =>
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

    it("should send a MarkupProvided event for a markup in Future returned by test body") {
      class MySuite extends funsuite.FixtureAsyncFunSuite  {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        type FixtureParam = String
        def withFixture(test: OneArgAsyncTest): FutureOutcome =
          test("testing")
        test("test 1") { fixture =>
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

    it("should generate a DuplicateTestNameException when duplicate test name is detected") {
      class TestSpec extends funsuite.FixtureAsyncFunSuite {
        type FixtureParam = String
        def withFixture(test: OneArgAsyncTest): FutureOutcome = test("testing")
        test("test 1") { fixture => succeed }
        test("test 1") { fixture => succeed }
      }
      val e = intercept[DuplicateTestNameException] {
        new TestSpec
      }
      assert("FixtureAsyncFunSuiteSpec.scala" == e.failedCodeFileName.get)
      assert(e.failedCodeLineNumber.get == thisLineNumber - 6)
      assert(!e.cause.isDefined)
    }

    it("should generate a DuplicateTestNameException when duplicate test name is detected using ignore") {
      class TestSpec extends funsuite.FixtureAsyncFunSuite {
        type FixtureParam = String
        def withFixture(test: OneArgAsyncTest): FutureOutcome = test("testing")
        test("test 1") { fixture => succeed }
        ignore("test 1") { fixture => succeed }
      }
      val e = intercept[DuplicateTestNameException] {
        new TestSpec
      }
      assert("FixtureAsyncFunSuiteSpec.scala" == e.failedCodeFileName.get)
      assert(e.failedCodeLineNumber.get == thisLineNumber - 6)
      assert(!e.cause.isDefined)
    }

    it("should allow other execution context to be used") {
      //SCALATESTJS-ONLY var changeMe = false

      //SCALATESTJS-ONLY object CustomTestExecutionContext extends scala.concurrent.ExecutionContextExecutor {
      //SCALATESTJS-ONLY   override def execute(runnable: Runnable): Unit = {
      //SCALATESTJS-ONLY     changeMe = true
      //SCALATESTJS-ONLY     try {
      //SCALATESTJS-ONLY       runnable.run()
      //SCALATESTJS-ONLY     } catch {
      //SCALATESTJS-ONLY       case t: Throwable => reportFailure(t)
      //SCALATESTJS-ONLY     }
      //SCALATESTJS-ONLY   }
      //SCALATESTJS-ONLY   def reportFailure(t: Throwable): Unit =
      //SCALATESTJS-ONLY     t.printStackTrace()
      //SCALATESTJS-ONLY }
      class TestSpec extends funsuite.FixtureAsyncFunSuite {
        // SKIP-SCALATESTJS,NATIVE-START
        override implicit val executionContext = scala.concurrent.ExecutionContext.Implicits.global
        // SKIP-SCALATESTJS,NATIVE-END
        //SCALATESTJS-ONLY override implicit val executionContext: ExecutionContext = CustomTestExecutionContext

        type FixtureParam = String
        def withFixture(test: OneArgAsyncTest): FutureOutcome =
          test("testing")

        val a = 1
        test("test A") { fixture =>
          Future { assert(a == 1) }
        }
        test("test B") { fixture =>
          Future { assert(a == 1) }
        }
        test("test C") { fixture =>
          Future { assert(a == 1) }
        }
      }
      val suite = new TestSpec
      val reporter = new EventRecordingReporter
      val status = suite.run(None, Args(reporter))

      // SKIP-SCALATESTJS,NATIVE-START
      status.waitUntilCompleted()
      // SKIP-SCALATESTJS,NATIVE-END
      assert(reporter.testStartingEventsReceived.length == 3)
      assert(reporter.testSucceededEventsReceived.length == 3)
      //SCALATESTJS-ONLY assert(changeMe)
    }

  }

}
