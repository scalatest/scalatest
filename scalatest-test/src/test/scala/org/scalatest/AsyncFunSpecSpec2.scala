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
import org.scalatest.events.{InfoProvided, MarkupProvided}

import scala.util.Success

class AsyncFunSpecSpec2 extends AsyncFunSpec {

  describe("AsyncFunSpec") {

    it("can be used for tests that return a Future under parallel async test execution") {

      class ExampleSpec extends AsyncFunSpec with ParallelTestExecution /* with expectations.Expectations */ { // Can resurrect expect later

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
      val promise = Promise[EventRecordingReporter]
      status whenCompleted { _ => promise.success(rep) }
      promise.future.map { repo =>
        assert(repo.testStartingEventsReceived.length == 6)
        assert(repo.testSucceededEventsReceived.length == 2)
        assert(repo.testSucceededEventsReceived(0).testName == "test 1")
        assert(repo.testSucceededEventsReceived(1).testName == "test 6")
        assert(repo.testFailedEventsReceived.length == 2)
        assert {
          val zero = rep.testFailedEventsReceived(0).testName
          val one = rep.testFailedEventsReceived(1).testName
          (zero == "test 2" && one == "test 7") || (zero == "test 7" && one == "test 2")
        }
        assert(repo.testPendingEventsReceived.length == 1)
        assert(repo.testPendingEventsReceived(0).testName == "test 3")
        assert(repo.testCanceledEventsReceived.length == 1)
        assert(repo.testCanceledEventsReceived(0).testName == "test 4")
        assert(repo.testIgnoredEventsReceived.length == 1)
        assert(repo.testIgnoredEventsReceived(0).testName == "test 5")
      }
    }

    it("can be used for tests that did not return Future under parallel async test execution") {

      class ExampleSpec extends AsyncFunSpec with ParallelTestExecution /* with expectations.Expectations */ {

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
          // expect(a == 1)
          assert(a == 1)
        }

        it("test 7") {
          // val result = expect(a == 22)
          // result
          assert(a == 22)
        }

        override def newInstance = new ExampleSpec
      }

      val rep = new EventRecordingReporter
      val spec = new ExampleSpec
      val status = spec.run(None, Args(reporter = rep))
      val promise = Promise[EventRecordingReporter]
      status whenCompleted { _ => promise.success(rep) }
      promise.future.map { repo =>
        assert(repo.testStartingEventsReceived.length == 6)
        assert(repo.testSucceededEventsReceived.length == 2)
        assert(repo.testSucceededEventsReceived(0).testName == "test 1")
        assert(repo.testSucceededEventsReceived(1).testName == "test 6")
        assert(repo.testFailedEventsReceived.length == 2)
        assert {
          val zero = rep.testFailedEventsReceived(0).testName
          val one = rep.testFailedEventsReceived(1).testName
          (zero == "test 2" && one == "test 7") || (zero == "test 7" && one == "test 2")
        }
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

      class ExampleSpec extends AsyncFunSpec {

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

      class ExampleSpec extends AsyncFunSpec {

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

    // SKIP-SCALATESTJS,NATIVE-START
    it("should run tests and its future in same main thread when use SerialExecutionContext") {

      var mainThread = Thread.currentThread
      var test1Thread: Option[Thread] = None
      var test2Thread: Option[Thread] = None
      var onCompleteThread: Option[Thread] = None

      class ExampleSpec extends AsyncFunSpec {

        it("test 1") {
          Future {
            test1Thread = Some(Thread.currentThread)
            succeed
          }
        }

        it("test 2") {
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

      class ExampleSpec extends AsyncFunSpec {

        it("test 1") {
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

        it("test 2") {
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

      class ExampleSpec extends AsyncFunSpec {

        // Note we get a StackOverflowError with the following execution
        // context.
        // override implicit def executionContext: ExecutionContext = new ExecutionContext { def execute(runnable: Runnable) = runnable.run; def reportFailure(cause: Throwable) = () }

        def sum(xs: List[Int]): Future[Int] =
          xs match {
            case Nil => Future.successful(0)
            case x :: xs => Future(x).flatMap(xx => sum(xs).map(xxx => xx + xxx))
          }

        it("test 1") {
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
    // SKIP-SCALATESTJS,NATIVE-END

    it("should run tests that returns Future and report their result in serial") {

      class ExampleSpec extends AsyncFunSpec {

        it("test 1") {
          Future {
            SleepHelper.sleep(60)
            succeed
          }
        }

        it("test 2") {
          Future {
            SleepHelper.sleep(30)
            succeed
          }
        }

        it("test 3") {
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

      class ExampleSpec extends AsyncFunSpec {

        it("test 1") {
          SleepHelper.sleep(60)
          succeed
        }

        it("test 2") {
          SleepHelper.sleep(30)
          succeed
        }

        it("test 3") {
          succeed
        }

      }

      val rep = new EventRecordingReporter
      val suite = new ExampleSpec
      val status = suite.run(None, Args(reporter = rep))
      // SKIP-SCALATESTJS,NATIVE-START
      status.waitUntilCompleted()
      // SKIP-SCALATESTJS,NATIVE-END

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

    it("should send an InfoProvided event for an info in main spec body") {
      class MySuite extends AsyncFunSpecLike  {
        info(
          "hi there"
        )
      }
      val suite = new MySuite
      val reporter = new EventRecordingReporter
      val status = suite.run(None, Args(reporter))

      val promise = Promise[EventRecordingReporter]
      status whenCompleted { _ => promise.success(reporter) }
      promise.future.map { repo =>
        val infoList = reporter.infoProvidedEventsReceived

        assert(infoList.size == 1)
        assert(infoList(0).message == "hi there")
      }
    }

    it("should send an InfoProvided event for an info in scope body") {
      class MySuite extends AsyncFunSpecLike  {

        describe("test feature") {
          info(
            "hi there"
          )

          it("test 1") { succeed }
        }
      }
      val suite = new MySuite
      val reporter = new EventRecordingReporter
      val status = suite.run(None, Args(reporter))

      val promise = Promise[EventRecordingReporter]
      status whenCompleted { _ => promise.success(reporter) }
      promise.future.map { repo =>
        val infoList = reporter.infoProvidedEventsReceived

        assert(infoList.size == 1)
        assert(infoList(0).message == "hi there")
      }
    }

    it("should send an InfoProvided event for an info in test body") {
      class MySuite extends AsyncFunSpecLike  {

        describe("test feature") {
          it("test 1") {
            info("hi there")
            succeed
          }
        }
      }
      val suite = new MySuite
      val reporter = new EventRecordingReporter
      val status = suite.run(None, Args(reporter))

      val promise = Promise[EventRecordingReporter]
      status whenCompleted { _ => promise.success(reporter) }
      promise.future.map { repo =>
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
    }

    it("should send an InfoProvided event for an info in Future returned by test body") {
      class MySuite extends AsyncFunSpecLike  {

        describe("test feature") {
          it("test 1") {
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

      val promise = Promise[EventRecordingReporter]
      status whenCompleted { _ => promise.success(reporter) }
      promise.future.map { repo =>
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
    }

    it("should send a NoteProvided event for a note in main spec body") {
      class MySuite extends AsyncFunSpecLike  {
        note(
          "hi there"
        )
      }
      val suite = new MySuite
      val reporter = new EventRecordingReporter
      val status = suite.run(None, Args(reporter))

      val promise = Promise[EventRecordingReporter]
      status whenCompleted { _ => promise.success(reporter) }
      promise.future.map { repo =>
        val noteList = reporter.noteProvidedEventsReceived

        assert(noteList.size == 1)
        assert(noteList(0).message == "hi there")
      }
    }

    it("should send a NoteProvided event for a note in scope body") {
      class MySuite extends AsyncFunSpecLike  {

        describe("test feature") {
          note(
            "hi there"
          )

          it("test 1") { succeed }
        }
      }
      val suite = new MySuite
      val reporter = new EventRecordingReporter
      val status = suite.run(None, Args(reporter))

      val promise = Promise[EventRecordingReporter]
      status whenCompleted { _ => promise.success(reporter) }
      promise.future.map { repo =>
        val noteList = reporter.noteProvidedEventsReceived

        assert(noteList.size == 1)
        assert(noteList(0).message == "hi there")
      }
    }

    it("should send a NoteProvided event for a note in test body") {
      class MySuite extends AsyncFunSpecLike  {

        describe("test feature") {
          it("test 1") {
            note("hi there")
            succeed
          }
        }
      }
      val suite = new MySuite
      val reporter = new EventRecordingReporter
      val status = suite.run(None, Args(reporter))

      val promise = Promise[EventRecordingReporter]
      status whenCompleted { _ => promise.success(reporter) }
      promise.future.map { repo =>
        val noteList = reporter.noteProvidedEventsReceived
        assert(noteList.size == 1)
        assert(noteList(0).message == "hi there")
      }
    }

    it("should send a NoteProvided event for a note in Future returned by test body") {
      class MySuite extends AsyncFunSpecLike  {

        describe("test feature") {
          it("test 1") {
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

      val promise = Promise[EventRecordingReporter]
      status whenCompleted { _ => promise.success(reporter) }
      promise.future.map { repo =>
        val noteList = reporter.noteProvidedEventsReceived
        assert(noteList.size == 1)
        assert(noteList(0).message == "hi there")
      }
    }

    it("should send an AlertProvided event for an alert in main spec body") {
      class MySuite extends AsyncFunSpecLike  {
        alert(
          "hi there"
        )
      }
      val suite = new MySuite
      val reporter = new EventRecordingReporter
      val status = suite.run(None, Args(reporter))

      val promise = Promise[EventRecordingReporter]
      status whenCompleted { _ => promise.success(reporter) }
      promise.future.map { repo =>
        val alertList = reporter.alertProvidedEventsReceived

        assert(alertList.size == 1)
        assert(alertList(0).message == "hi there")
      }
    }

    it("should send an AlertProvided event for an alert in scope body") {
      class MySuite extends AsyncFunSpecLike  {

        describe("test feature") {
          alert(
            "hi there"
          )

          it("test 1") { succeed }
        }
      }
      val suite = new MySuite
      val reporter = new EventRecordingReporter
      val status = suite.run(None, Args(reporter))

      val promise = Promise[EventRecordingReporter]
      status whenCompleted { _ => promise.success(reporter) }
      promise.future.map { repo =>
        val alertList = reporter.alertProvidedEventsReceived

        assert(alertList.size == 1)
        assert(alertList(0).message == "hi there")
      }
    }

    it("should send an AlertProvided event for an alert in test body") {
      class MySuite extends AsyncFunSpecLike  {

        describe("test feature") {
          it("test 1") {
            alert("hi there")
            succeed
          }
        }
      }
      val suite = new MySuite
      val reporter = new EventRecordingReporter
      val status = suite.run(None, Args(reporter))

      val promise = Promise[EventRecordingReporter]
      status whenCompleted { _ => promise.success(reporter) }
      promise.future.map { repo =>
        val alertList = reporter.alertProvidedEventsReceived
        assert(alertList.size == 1)
        assert(alertList(0).message == "hi there")
      }
    }

    it("should send an AlertProvided event for an alert in Future returned by test body") {
      class MySuite extends AsyncFunSpecLike  {

        describe("test feature") {
          it("test 1") {
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

      val promise = Promise[EventRecordingReporter]
      status whenCompleted { _ => promise.success(reporter) }
      promise.future.map { repo =>
        val alertList = reporter.alertProvidedEventsReceived
        assert(alertList.size == 1)
        assert(alertList(0).message == "hi there")
      }
    }

    it("should send a MarkupProvided event for a markup in main spec body") {
      class MySuite extends AsyncFunSpecLike  {
        markup(
          "hi there"
        )
      }
      val suite = new MySuite
      val reporter = new EventRecordingReporter
      val status = suite.run(None, Args(reporter))

      val promise = Promise[EventRecordingReporter]
      status whenCompleted { _ => promise.success(reporter) }
      promise.future.map { repo =>
        val markupList = reporter.markupProvidedEventsReceived

        assert(markupList.size == 1)
        assert(markupList(0).text == "hi there")
      }
    }

    it("should send a MarkupProvided event for a markup in scope body") {
      class MySuite extends AsyncFunSpecLike  {

        describe("test feature") {
          markup(
            "hi there"
          )

          it("test 1") { succeed }
        }
      }
      val suite = new MySuite
      val reporter = new EventRecordingReporter
      val status = suite.run(None, Args(reporter))

      val promise = Promise[EventRecordingReporter]
      status whenCompleted { _ => promise.success(reporter) }
      promise.future.map { repo =>
        val markupList = reporter.markupProvidedEventsReceived

        assert(markupList.size == 1)
        assert(markupList(0).text == "hi there")
      }
    }

    it("should send a MarkupProvided event for a markup in test body") {
      class MySuite extends AsyncFunSpecLike  {

        describe("test feature") {
          it("test 1") {
            markup("hi there")
            succeed
          }
        }
      }
      val suite = new MySuite
      val reporter = new EventRecordingReporter
      val status = suite.run(None, Args(reporter))

      val promise = Promise[EventRecordingReporter]
      status whenCompleted { _ => promise.success(reporter) }
      promise.future.map { repo =>
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
    }

    it("should send a MarkupProvided event for a markup in Future returned by test body") {
      class MySuite extends AsyncFunSpecLike  {

        describe("test feature") {
          it("test 1") {
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

      val promise = Promise[EventRecordingReporter]
      status whenCompleted { _ => promise.success(reporter) }
      promise.future.map { repo =>
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
    }

    it("should allow other execution context to be used") {
      class TestSpec extends AsyncFunSpec {
        // SKIP-SCALATESTJS,NATIVE-START
        override implicit val executionContext = scala.concurrent.ExecutionContext.Implicits.global
        // SKIP-SCALATESTJS,NATIVE-END
        // SCALATESTJS-ONLY override implicit val executionContext = scala.scalajs.concurrent.JSExecutionContext.runNow
        val a = 1
        describe("feature 1") {
          it("test A") {
            Future { assert(a == 1) }
          }
        }
        describe("feature 2") {
          it("test B") {
            Future { assert(a == 1) }
          }
        }
        describe("group3") {
          it("test C") {
            Future { assert(a == 1) }
          }
        }
      }
      val suite = new TestSpec
      val reporter = new EventRecordingReporter
      val status = suite.run(None, Args(reporter))
      val promise = Promise[EventRecordingReporter]
      status whenCompleted { _ => promise.success(reporter) }
      promise.future.map { r =>
        assert(reporter.scopeOpenedEventsReceived.length == 3)
        assert(reporter.scopeClosedEventsReceived.length == 3)
        assert(reporter.testStartingEventsReceived.length == 3)
        assert(reporter.testSucceededEventsReceived.length == 3)
      }
    }

  }
}
