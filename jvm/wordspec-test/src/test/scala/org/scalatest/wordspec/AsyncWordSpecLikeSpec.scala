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
package org.scalatest.wordspec

import org.scalatest._
import org.scalatest.SharedHelpers.{EventRecordingReporter, thisLineNumber}
import scala.concurrent.{Promise, ExecutionContext, Future}
import org.scalatest.concurrent.SleepHelper
import org.scalatest.events.{InfoProvided, MarkupProvided}
import org.scalatest.exceptions.{DuplicateTestNameException, NotAllowedException}
import org.scalactic.Prettifier

import scala.util.Success
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.wordspec.{ AsyncWordSpec, AsyncWordSpecLike }

class AsyncWordSpecLikeSpec extends AnyFunSpec {

  private val prettifier = Prettifier.default

  describe("AsyncWordSpecLike") {

    it("can be used for tests that return Future under parallel async test execution") {

      class ExampleSpec extends AsyncWordSpecLike with ParallelTestExecution {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

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
            cancel()
          }
        }

        "test 5" ignore {
          Future {
            cancel()
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

      class ExampleSpec extends AsyncWordSpecLike with ParallelTestExecution {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

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
          cancel()
        }

        "test 5" ignore {
          cancel()
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

    it("can be used with is for pending tests that don't return a Future") {

      class ExampleSpec extends AsyncWordSpec {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        val a = 1

        "test 1" is {
          pending
        }

        "test 2" ignore {
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
      assert(rep.testPendingEventsReceived(0).testName == "test 1")
      assert(rep.testCanceledEventsReceived.length == 0)
      assert(rep.testIgnoredEventsReceived.length == 1)
      assert(rep.testIgnoredEventsReceived(0).testName == "test 2")
    }

    it("should run tests that return Future in serial by default") {

      @volatile var count = 0

      class ExampleSpec extends AsyncWordSpecLike {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        "test 1" in {
          Future {
            SleepHelper.sleep(30)
            assert(count == 0)
            count = 1
            succeed
          }
        }

        "test 2" in {
          Future {
            assert(count == 1)
            SleepHelper.sleep(50)
            count = 2
            succeed
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
      // SKIP-SCALATESTJS,NATIVE-START
      status.waitUntilCompleted()
      // SKIP-SCALATESTJS,NATIVE-END

      assert(rep.testStartingEventsReceived.length == 3)
      assert(rep.testSucceededEventsReceived.length == 3)

    }

    it("should run tests that does not return Future in serial by default") {

      @volatile var count = 0

      class ExampleSpec extends AsyncWordSpecLike {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        "test 1" in {
          SleepHelper.sleep(30)
          assert(count == 0)
          count = 1
          succeed
        }

        "test 2" in {
          assert(count == 1)
          SleepHelper.sleep(50)
          count = 2
          succeed
        }

        "test 3" in {
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

      class ExampleSpec extends AsyncWordSpecLike {

        "test 1" in {
          Future {
            test1Thread = Some(Thread.currentThread)
            succeed
          }
        }

        "test 2" in {
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

      class ExampleSpec extends AsyncWordSpecLike {

        "test 1" in {
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

        "test 2" in {
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

      class ExampleSpec extends AsyncWordSpecLike {

        // Note we get a StackOverflowError with the following execution
        // context.
        // override implicit def executionContext: ExecutionContext = new ExecutionContext { def execute(runnable: Runnable) = runnable.run; def reportFailure(cause: Throwable) = () }

        def sum(xs: List[Int]): Future[Int] =
          xs match {
            case Nil => Future.successful(0)
            case x :: xs => Future(x).flatMap(xx => sum(xs).map(xxx => xx + xxx))
          }

        "test 1" in {
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

      class ExampleSpec extends AsyncWordSpecLike {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        "test 1" in {
          Future {
            SleepHelper.sleep(60)
            succeed
          }
        }

        "test 2" in {
          Future {
            SleepHelper.sleep(30)
            succeed
          }
        }

        "test 3" in {
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

      class ExampleSpec extends AsyncWordSpecLike {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        "test 1" in {
          SleepHelper.sleep(60)
          succeed
        }

        "test 2" in {
          SleepHelper.sleep(30)
          succeed
        }

        "test 3" in {
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
      class MySuite extends AsyncWordSpecLike  {
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

    it("should send an InfoProvided event for an info in scope body") {
      class MySuite extends AsyncWordSpecLike  {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        "test feature" should {
          info(
            "hi there"
          )

          "test 1" in { succeed }
        }
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
      class MySuite extends AsyncWordSpecLike  {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        "test feature" should {
          "test 1" in {
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

    it("should send an InfoProvided event for an info in Future returned by test body") {
      class MySuite extends AsyncWordSpecLike  {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        "test feature" should {
          "test 1" in {
            Future {
              info("hi there")
              succeed
            }
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
      class MySuite extends AsyncWordSpecLike  {
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

    it("should send a NoteProvided event for a note in scope body") {
      class MySuite extends AsyncWordSpecLike  {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        "test feature" should {
          note(
            "hi there"
          )

          "test 1" in { succeed }
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

    it("should send a NoteProvided event for a note in test body") {
      class MySuite extends AsyncWordSpecLike  {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        "test feature" should {
          "test 1" in {
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

    it("should send a NoteProvided event for a note in Future returned by test body") {
      class MySuite extends AsyncWordSpecLike  {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        "test feature" should {
          "test 1" in {
            Future {
              note("hi there")
              succeed
            }
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
      class MySuite extends AsyncWordSpecLike  {
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

    it("should send an AlertProvided event for an alert in scope body") {
      class MySuite extends AsyncWordSpecLike  {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        "test feature" should {
          alert(
            "hi there"
          )

          "test 1" in { succeed }
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

    it("should send an AlertProvided event for an alert in test body") {
      class MySuite extends AsyncWordSpecLike  {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        "test feature" should {
          "test 1" in {
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

    it("should send an AlertProvided event for an alert in Future returned by test body") {
      class MySuite extends AsyncWordSpecLike  {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        "test feature" should {
          "test 1" in {
            Future {
              alert("hi there")
              succeed
            }
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
      class MySuite extends AsyncWordSpecLike  {
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

    it("should send a MarkupProvided event for a markup in scope body") {
      class MySuite extends AsyncWordSpecLike  {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        "test feature" should {
          markup(
            "hi there"
          )

          "test 1" in { succeed }
        }
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
      class MySuite extends AsyncWordSpecLike  {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        "test feature" should {
          "test 1" in {
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

    it("should send a MarkupProvided event for a markup in Future returned by test body") {
      class MySuite extends AsyncWordSpecLike  {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        "test feature" should {
          "test 1" in {
            Future {
              markup("hi there")
              succeed
            }
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

    it("should throw NotAllowedException wrapping a DuplicateTestNameException when duplicate test name is detected inside when") {
      class TestSpec extends AsyncWordSpecLike {
        "a feature" when {
          "test 1" in { succeed }
          "test 1" in { succeed }
        }
      }
      val e = intercept[NotAllowedException] {
            new TestSpec
          }
      assert("AsyncWordSpecLikeSpec.scala" == e.failedCodeFileName.get)
      assert(e.failedCodeLineNumber.get == thisLineNumber - 7)
      assert(e.cause.isDefined)
      val causeThrowable = e.cause.get
      assert(e.message == Some(FailureMessages.exceptionWasThrownInWhenClause(prettifier, UnquotedString(causeThrowable.getClass.getName), "a feature", FailureMessages.duplicateTestName(prettifier, UnquotedString("a feature when test 1")))))

      assert(causeThrowable.isInstanceOf[DuplicateTestNameException])
      val cause = causeThrowable.asInstanceOf[DuplicateTestNameException]
      assert(cause.getMessage == FailureMessages.duplicateTestName(prettifier, UnquotedString("a feature when test 1")))
    }

    it("should throw NotAllowedException wrapping a DuplicateTestNameException when duplicate test name is detected inside shorthand when") {
      class TestSpec extends AsyncWordSpecLike {
        "a feature" when {
          //DOTTY-ONLY ()
        }
        it when {
          "test 1" in { succeed }
          "test 1" in { succeed }
        }
      }
      val e = intercept[NotAllowedException] {
            new TestSpec
          }
      assert("AsyncWordSpecLikeSpec.scala" == e.failedCodeFileName.get)
      assert(e.failedCodeLineNumber.get == thisLineNumber - 7)
      assert(e.cause.isDefined)
      val causeThrowable = e.cause.get
      assert(e.message == Some(FailureMessages.exceptionWasThrownInWhenClause(prettifier, UnquotedString(causeThrowable.getClass.getName), "a feature", FailureMessages.duplicateTestName(prettifier, UnquotedString("a feature when test 1")))))

      assert(causeThrowable.isInstanceOf[DuplicateTestNameException])
      val cause = causeThrowable.asInstanceOf[DuplicateTestNameException]
      assert(cause.getMessage == FailureMessages.duplicateTestName(prettifier, UnquotedString("a feature when test 1")))
    }

    it("should throw NotAllowedException wrapping a DuplicateTestNameException when duplicate test name is detected inside should") {
      class TestSpec extends AsyncWordSpecLike {
        "a feature" should {
          "test 1" in { succeed }
          "test 1" in { succeed }
        }
      }
      val e = intercept[NotAllowedException] {
            new TestSpec
          }
      assert("AsyncWordSpecLikeSpec.scala" == e.failedCodeFileName.get)
      assert(e.failedCodeLineNumber.get == thisLineNumber - 7)
      assert(e.cause.isDefined)
      val causeThrowable = e.cause.get
      assert(e.message == Some(FailureMessages.exceptionWasThrownInShouldClause(prettifier, UnquotedString(causeThrowable.getClass.getName), "a feature", FailureMessages.duplicateTestName(prettifier, UnquotedString("a feature should test 1")))))

      assert(causeThrowable.isInstanceOf[DuplicateTestNameException])
      val cause = causeThrowable.asInstanceOf[DuplicateTestNameException]
      assert(cause.getMessage == FailureMessages.duplicateTestName(prettifier, UnquotedString("a feature should test 1")))
    }

    it("should throw NotAllowedException wrapping a DuplicateTestNameException when duplicate test name is detected inside shorthand should") {
      class TestSpec extends AsyncWordSpecLike {
        "a feature" should {
          //DOTTY-ONLY ()
        }
        it should {
          "test 1" in { succeed }
          "test 1" in { succeed }
        }
      }
      val e = intercept[NotAllowedException] {
        new TestSpec
      }
      assert("AsyncWordSpecLikeSpec.scala" == e.failedCodeFileName.get)
      assert(e.failedCodeLineNumber.get == thisLineNumber - 7)
      assert(e.cause.isDefined)
      val causeThrowable = e.cause.get
      assert(e.message == Some(FailureMessages.exceptionWasThrownInShouldClause(prettifier, UnquotedString(causeThrowable.getClass.getName), "a feature", FailureMessages.duplicateTestName(prettifier, UnquotedString("a feature should test 1")))))

      assert(causeThrowable.isInstanceOf[DuplicateTestNameException])
      val cause = causeThrowable.asInstanceOf[DuplicateTestNameException]
      assert(cause.getMessage == FailureMessages.duplicateTestName(prettifier, UnquotedString("a feature should test 1")))
    }

    it("should throw NotAllowedException wrapping a DuplicateTestNameException when duplicate test name is detected inside must") {
      class TestSpec extends AsyncWordSpecLike {
        "a feature" must {
          "test 1" in { succeed }
          "test 1" in { succeed }
        }
      }
      val e = intercept[NotAllowedException] {
            new TestSpec
          }
      assert("AsyncWordSpecLikeSpec.scala" == e.failedCodeFileName.get)
      assert(e.failedCodeLineNumber.get == thisLineNumber - 7)
      assert(e.cause.isDefined)
      val causeThrowable = e.cause.get
      assert(e.message == Some(FailureMessages.exceptionWasThrownInMustClause(prettifier, UnquotedString(causeThrowable.getClass.getName), "a feature", FailureMessages.duplicateTestName(prettifier, UnquotedString("a feature must test 1")))))

      assert(causeThrowable.isInstanceOf[DuplicateTestNameException])
      val cause = causeThrowable.asInstanceOf[DuplicateTestNameException]
      assert(cause.getMessage == FailureMessages.duplicateTestName(prettifier, UnquotedString("a feature must test 1")))
    }

    it("should throw NotAllowedException wrapping a DuplicateTestNameException when duplicate test name is detected inside shorthand must") {
      class TestSpec extends AsyncWordSpecLike {
        "a feature" must {
          //DOTTY-ONLY ()
        }
        it must {
          "test 1" in { succeed }
          "test 1" in { succeed }
        }
      }
      val e = intercept[NotAllowedException] {
            new TestSpec
          }
      assert("AsyncWordSpecLikeSpec.scala" == e.failedCodeFileName.get)
      assert(e.failedCodeLineNumber.get == thisLineNumber - 7)
      assert(e.cause.isDefined)
      val causeThrowable = e.cause.get
      assert(e.message == Some(FailureMessages.exceptionWasThrownInMustClause(prettifier, UnquotedString(causeThrowable.getClass.getName), "a feature", FailureMessages.duplicateTestName(prettifier, UnquotedString("a feature must test 1")))))

      assert(causeThrowable.isInstanceOf[DuplicateTestNameException])
      val cause = causeThrowable.asInstanceOf[DuplicateTestNameException]
      assert(cause.getMessage == FailureMessages.duplicateTestName(prettifier, UnquotedString("a feature must test 1")))
    }

    it("should throw NotAllowedException wrapping a DuplicateTestNameException when duplicate test name is detected inside that") {
      class TestSpec extends AsyncWordSpecLike {
        "a feature" that {
          "test 1" in { succeed }
          "test 1" in { succeed }
        }
      }
      val e = intercept[NotAllowedException] {
            new TestSpec
          }
      assert("AsyncWordSpecLikeSpec.scala" == e.failedCodeFileName.get)
      assert(e.failedCodeLineNumber.get == thisLineNumber - 7)
      assert(e.cause.isDefined)
      val causeThrowable = e.cause.get
      assert(e.message == Some(FailureMessages.exceptionWasThrownInThatClause(prettifier, UnquotedString(causeThrowable.getClass.getName), "a feature that", FailureMessages.duplicateTestName(prettifier, UnquotedString("a feature that test 1")))))

      assert(causeThrowable.isInstanceOf[DuplicateTestNameException])
      val cause = causeThrowable.asInstanceOf[DuplicateTestNameException]
      assert(cause.getMessage == FailureMessages.duplicateTestName(prettifier, UnquotedString("a feature that test 1")))
    }

    it("should throw NotAllowedException wrapping a DuplicateTestNameException when duplicate test name is detected inside which") {
      class TestSpec extends AsyncWordSpecLike {
        "a feature" which {
          "test 1" in { succeed }
          "test 1" in { succeed }
        }
      }
      val e = intercept[NotAllowedException] {
            new TestSpec
          }
      assert("AsyncWordSpecLikeSpec.scala" == e.failedCodeFileName.get)
      assert(e.failedCodeLineNumber.get == thisLineNumber - 7)
      assert(e.cause.isDefined)
      val causeThrowable = e.cause.get
      assert(e.message == Some(FailureMessages.exceptionWasThrownInWhichClause(prettifier, UnquotedString(causeThrowable.getClass.getName), "a feature which", FailureMessages.duplicateTestName(prettifier, UnquotedString("a feature which test 1")))))

      assert(causeThrowable.isInstanceOf[DuplicateTestNameException])
      val cause = causeThrowable.asInstanceOf[DuplicateTestNameException]
      assert(cause.getMessage == FailureMessages.duplicateTestName(prettifier, UnquotedString("a feature which test 1")))
    }

    it("should throw NotAllowedException wrapping a DuplicateTestNameException when duplicate test name is detected inside can") {
      class TestSpec extends AsyncWordSpecLike {
        "a feature" can {
          "test 1" in { succeed }
          "test 1" in { succeed }
        }
      }
      val e = intercept[NotAllowedException] {
            new TestSpec
          }
      assert("AsyncWordSpecLikeSpec.scala" == e.failedCodeFileName.get)
      assert(e.failedCodeLineNumber.get == thisLineNumber - 7)
      assert(e.cause.isDefined)
      val causeThrowable = e.cause.get
      assert(e.message == Some(FailureMessages.exceptionWasThrownInCanClause(prettifier, UnquotedString(causeThrowable.getClass.getName), "a feature", FailureMessages.duplicateTestName(prettifier, UnquotedString("a feature can test 1")))))

      assert(causeThrowable.isInstanceOf[DuplicateTestNameException])
      val cause = causeThrowable.asInstanceOf[DuplicateTestNameException]
      assert(cause.getMessage == FailureMessages.duplicateTestName(prettifier, UnquotedString("a feature can test 1")))
    }

    it("should throw NotAllowedException wrapping a DuplicateTestNameException when duplicate test name is detected inside shorthand can") {
      class TestSpec extends AsyncWordSpecLike {
        "a feature" can {
          //DOTTY-ONLY ()
        }
        it can {
          "test 1" in { succeed }
          "test 1" in { succeed }
        }
      }
      val e = intercept[NotAllowedException] {
            new TestSpec
          }
      assert("AsyncWordSpecLikeSpec.scala" == e.failedCodeFileName.get)
      assert(e.failedCodeLineNumber.get == thisLineNumber - 7)
      assert(e.cause.isDefined)
      val causeThrowable = e.cause.get
      assert(e.message == Some(FailureMessages.exceptionWasThrownInCanClause(prettifier, UnquotedString(causeThrowable.getClass.getName), "a feature", FailureMessages.duplicateTestName(prettifier, UnquotedString("a feature can test 1")))))

      assert(causeThrowable.isInstanceOf[DuplicateTestNameException])
      val cause = causeThrowable.asInstanceOf[DuplicateTestNameException]
      assert(cause.getMessage == FailureMessages.duplicateTestName(prettifier, UnquotedString("a feature can test 1")))
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
      class TestSpec extends AsyncWordSpecLike {
        // SKIP-SCALATESTJS,NATIVE-START
        override implicit val executionContext = scala.concurrent.ExecutionContext.Implicits.global
        // SKIP-SCALATESTJS,NATIVE-END
        //SCALATESTJS-ONLY override implicit val executionContext: ExecutionContext = CustomTestExecutionContext
        val a = 1
        "feature 1" should {
          "test A" in {
            Future { assert(a == 1) }
          }
        }
        "feature 2" should {
          "test B" in {
            Future { assert(a == 1) }
          }
        }
        "feature 3" should {
          "test C" in {
            Future { assert(a == 1) }
          }
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

    it("should throw DuplicateTestNameException when duplicate test name is detected inside in a forAll") {
      class TestSpec extends AsyncWordSpecLike {

        // SKIP-SCALATESTJS,NATIVE-START
        override implicit val executionContext: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global
        // SKIP-SCALATESTJS,NATIVE-END
        //SCALATESTJS-ONLY override implicit val executionContext: ExecutionContext = org.scalajs.macrotaskexecutor.MacrotaskExecutor.Implicits.global

        import org.scalatest.prop.TableDrivenPropertyChecks._
        "trying something" should {
          val scenarios = Table("value", "first", "first")
          
          forAll(scenarios) { value =>
            s"work as expected for $value " in {
              succeed
            }
          } 
        }
      }
      val e = intercept[DuplicateTestNameException] {
        new TestSpec
      }
      assert("AsyncWordSpecLikeSpec.scala" == e.failedCodeFileName.get)
      assert(e.failedCodeLineNumber.get == thisLineNumber - 10)
      assert(e.message == Some(FailureMessages.duplicateTestName(prettifier, UnquotedString("trying something should work as expected for first"))))
    }

  }

}
