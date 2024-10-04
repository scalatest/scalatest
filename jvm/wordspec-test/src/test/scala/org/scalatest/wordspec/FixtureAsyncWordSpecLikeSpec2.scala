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
import SharedHelpers.EventRecordingReporter
import scala.concurrent.{ExecutionContext, Promise, Future}
import org.scalatest.concurrent.SleepHelper
import org.scalatest.events.{InfoProvided, MarkupProvided}

import scala.util.Success
import org.scalatest
import org.scalatest.wordspec

class FixtureAsyncWordSpecLikeSpec2 extends scalatest.funspec.AsyncFunSpec {

  //SCALATESTJS-ONLY override implicit val executionContext: ExecutionContext = org.scalajs.macrotaskexecutor.MacrotaskExecutor.Implicits.global

  describe("AsyncWordSpecLike") {

    it("can be used for tests that return Future under parallel async test execution") {

      class ExampleSpec extends wordspec.FixtureAsyncWordSpecLike with ParallelTestExecution {

        type FixtureParam = String
        def withFixture(test: OneArgAsyncTest): FutureOutcome =
          test("testing")

        val a = 1

        "test 1" in { fixture =>
          Future {
            assert(a == 1)
          }
        }

        "test 2" in { fixture =>
          Future {
            assert(a == 2)
          }
        }

        "test 3" in { fixture =>
          Future {
            pending
          }
        }

        "test 4" in { fixture =>
          Future {
            cancel()
          }
        }

        "test 5" ignore { fixture =>
          Future {
            cancel()
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

      class ExampleSpec extends wordspec.FixtureAsyncWordSpecLike with ParallelTestExecution {

        type FixtureParam = String
        def withFixture(test: OneArgAsyncTest): FutureOutcome =
          test("testing")

        val a = 1

        "test 1" in { fixture =>
          assert(a == 1)
        }

        "test 2" in { fixture =>
          assert(a == 2)
        }

        "test 3" in { fixture =>
          pending
        }

        "test 4" in { fixture =>
          cancel()
        }

        "test 5" ignore { fixture =>
          cancel()
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

      class ExampleSpec extends wordspec.FixtureAsyncWordSpecLike {

        type FixtureParam = String
        def withFixture(test: OneArgAsyncTest): FutureOutcome =
          test("testing")

        "test 1" in { fixture =>
          Future {
            SleepHelper.sleep(30)
            assert(count == 0)
            count = 1
            Succeeded
          }
        }

        "test 2" in { fixture =>
          Future {
            assert(count == 1)
            SleepHelper.sleep(50)
            count = 2
            Succeeded
          }
        }

        "test 3" in { fixture =>
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

      class ExampleSpec extends wordspec.FixtureAsyncWordSpecLike {

        type FixtureParam = String
        def withFixture(test: OneArgAsyncTest): FutureOutcome =
          test("testing")

        "test 1" in { fixture =>
          SleepHelper.sleep(30)
          assert(count == 0)
          count = 1
          Succeeded
        }

        "test 2" in { fixture =>
          assert(count == 1)
          SleepHelper.sleep(50)
          count = 2
          Succeeded
        }

        "test 3" in { fixture =>
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

      class ExampleSpec extends wordspec.FixtureAsyncWordSpecLike {

        type FixtureParam = String
        def withFixture(test: OneArgAsyncTest): FutureOutcome =
          test("testing")

        "test 1" in { fixture =>
          Future {
            test1Thread = Some(Thread.currentThread)
            succeed
          }
        }

        "test 2" in { fixture =>
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

      class ExampleSpec extends wordspec.FixtureAsyncWordSpecLike {

        type FixtureParam = String
        def withFixture(test: OneArgAsyncTest): FutureOutcome =
          test("testing")

        "test 1" in { fixture =>
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

        "test 2" in { fixture =>
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

      class ExampleSpec extends wordspec.FixtureAsyncWordSpecLike {

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

        "test 1" in { fixture =>
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

      class ExampleSpec extends wordspec.FixtureAsyncWordSpecLike {

        type FixtureParam = String
        def withFixture(test: OneArgAsyncTest): FutureOutcome =
          test("testing")

        "test 1" in { fixture =>
          Future {
            SleepHelper.sleep(60)
            succeed
          }
        }

        "test 2" in { fixture =>
          Future {
            SleepHelper.sleep(30)
            succeed
          }
        }

        "test 3" in { fixture =>
          Future {
            succeed
          }
        }

      }

      val rep = new EventRecordingReporter
      val suite = new ExampleSpec
      val status = suite.run(None, Args(reporter = rep))

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

      class ExampleSpec extends wordspec.FixtureAsyncWordSpecLike {

        type FixtureParam = String
        def withFixture(test: OneArgAsyncTest): FutureOutcome =
          test("testing")

        "test 1" in { fixture =>
          SleepHelper.sleep(60)
          succeed
        }

        "test 2" in { fixture =>
          SleepHelper.sleep(30)
          succeed
        }

        "test 3" in { fixture =>
          succeed
        }

      }

      val rep = new EventRecordingReporter
      val suite = new ExampleSpec
      val status = suite.run(None, Args(reporter = rep))

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
      class MySuite extends wordspec.FixtureAsyncWordSpecLike  {
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

      val promise = Promise[EventRecordingReporter]
      status whenCompleted { _ => promise.success(reporter) }
      promise.future.map { repo =>
        val infoList = reporter.infoProvidedEventsReceived

        assert(infoList.size == 1)
        assert(infoList(0).message == "hi there")
      }
    }

    it("should send an InfoProvided event for an info in scope body") {
      class MySuite extends wordspec.FixtureAsyncWordSpecLike  {
        type FixtureParam = String
        def withFixture(test: OneArgAsyncTest): FutureOutcome =
          test("testing")
        "test feature" should {
          info(
            "hi there"
          )

          "test 1" in { fixture => succeed }
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
      class MySuite extends wordspec.FixtureAsyncWordSpecLike  {
        type FixtureParam = String
        def withFixture(test: OneArgAsyncTest): FutureOutcome =
          test("testing")
        "test feature" should {
          "test 1" in { fixture =>
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
      class MySuite extends wordspec.FixtureAsyncWordSpecLike  {
        type FixtureParam = String
        def withFixture(test: OneArgAsyncTest): FutureOutcome =
          test("testing")
        "test feature" should {
          "test 1" in { fixture =>
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
      class MySuite extends wordspec.FixtureAsyncWordSpecLike  {
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

      val promise = Promise[EventRecordingReporter]
      status whenCompleted { _ => promise.success(reporter) }
      promise.future.map { repo =>
        val noteList = reporter.noteProvidedEventsReceived

        assert(noteList.size == 1)
        assert(noteList(0).message == "hi there")
      }
    }

    it("should send a NoteProvided event for a note in scope body") {
      class MySuite extends wordspec.FixtureAsyncWordSpecLike  {
        type FixtureParam = String
        def withFixture(test: OneArgAsyncTest): FutureOutcome =
          test("testing")
        "test feature" should {
          note(
            "hi there"
          )

          "test 1" in { fixture => succeed }
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
      class MySuite extends wordspec.FixtureAsyncWordSpecLike  {
        type FixtureParam = String
        def withFixture(test: OneArgAsyncTest): FutureOutcome =
          test("testing")
        "test feature" should {
          "test 1" in { fixture =>
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
      class MySuite extends wordspec.FixtureAsyncWordSpecLike  {
        type FixtureParam = String
        def withFixture(test: OneArgAsyncTest): FutureOutcome =
          test("testing")
        "test feature" should {
          "test 1" in { fixture =>
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
      class MySuite extends wordspec.FixtureAsyncWordSpecLike  {
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

      val promise = Promise[EventRecordingReporter]
      status whenCompleted { _ => promise.success(reporter) }
      promise.future.map { repo =>
        val alertList = reporter.alertProvidedEventsReceived

        assert(alertList.size == 1)
        assert(alertList(0).message == "hi there")
      }
    }

    it("should send an AlertProvided event for an alert in scope body") {
      class MySuite extends wordspec.FixtureAsyncWordSpecLike  {
        type FixtureParam = String
        def withFixture(test: OneArgAsyncTest): FutureOutcome =
          test("testing")
        "test feature" should {
          alert(
            "hi there"
          )

          "test 1" in { fixture => succeed }
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
      class MySuite extends wordspec.FixtureAsyncWordSpecLike  {
        type FixtureParam = String
        def withFixture(test: OneArgAsyncTest): FutureOutcome =
          test("testing")
        "test feature" should {
          "test 1" in { fixture =>
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
      class MySuite extends wordspec.FixtureAsyncWordSpecLike  {
        type FixtureParam = String
        def withFixture(test: OneArgAsyncTest): FutureOutcome =
          test("testing")
        "test feature" should {
          "test 1" in { fixture =>
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
      class MySuite extends wordspec.FixtureAsyncWordSpecLike  {
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

      val promise = Promise[EventRecordingReporter]
      status whenCompleted { _ => promise.success(reporter) }
      promise.future.map { repo =>
        val markupList = reporter.markupProvidedEventsReceived

        assert(markupList.size == 1)
        assert(markupList(0).text == "hi there")
      }
    }

    it("should send a MarkupProvided event for a markup in scope body") {
      class MySuite extends wordspec.FixtureAsyncWordSpecLike  {
        type FixtureParam = String
        def withFixture(test: OneArgAsyncTest): FutureOutcome =
          test("testing")
        "test feature" should {
          markup(
            "hi there"
          )

          "test 1" in { fixture => succeed }
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
      class MySuite extends wordspec.FixtureAsyncWordSpecLike  {
        type FixtureParam = String
        def withFixture(test: OneArgAsyncTest): FutureOutcome =
          test("testing")
        "test feature" should {
          "test 1" in { fixture =>
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
      class MySuite extends wordspec.FixtureAsyncWordSpecLike  {
        type FixtureParam = String
        def withFixture(test: OneArgAsyncTest): FutureOutcome =
          test("testing")
        "test feature" should {
          "test 1" in { fixture =>
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
      class TestSpec extends wordspec.FixtureAsyncWordSpecLike {
        // SKIP-SCALATESTJS,NATIVE-START
        override implicit val executionContext = scala.concurrent.ExecutionContext.Implicits.global
        // SKIP-SCALATESTJS,NATIVE-END
        //SCALATESTJS-ONLY override implicit val executionContext: ExecutionContext = org.scalajs.macrotaskexecutor.MacrotaskExecutor.Implicits.global

        type FixtureParam = String
        def withFixture(test: OneArgAsyncTest): FutureOutcome = { test("hi") }

        val a = 1
        "feature 1" should {
          "test A" in { fixture =>
            Future { assert(a == 1) }
          }
        }
        "feature 2" should {
          "test B" in { fixture =>
            Future { assert(a == 1) }
          }
        }
        "feature 3" should {
          "test C" in { fixture =>
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
