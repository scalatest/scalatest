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
package org.scalatest.featurespec

import org.scalatest.SharedHelpers.{EventRecordingReporter, thisLineNumber}
import org.scalatest.events.{InfoProvided, MarkupProvided}
import scala.concurrent.{Promise, ExecutionContext, Future}
import org.scalatest.concurrent.SleepHelper
import org.scalatest.exceptions.{DuplicateTestNameException, NotAllowedException}
import org.scalactic.Prettifier

import scala.util.Success
import org.scalatest.featurespec.{ AsyncFeatureSpec, AsyncFeatureSpecLike }
import org.scalatest.funspec
import org.scalatest.ParallelTestExecution
import org.scalatest.Args
import org.scalatest.Assertion
import org.scalatest.Succeeded
import org.scalatest.FailureMessages
import org.scalatest.UnquotedString

class AsyncFeatureSpecLikeSpec extends funspec.AnyFunSpec {

  private val prettifier = Prettifier.default

  describe("AsyncFeatureSpecLike") {

    // ParallelTestExecution not working yet.
    it("can be used for tests that return Future under parallel async test execution") {

      class ExampleSpec extends AsyncFeatureSpecLike with ParallelTestExecution {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        val a = 1

        Scenario("test 1") {
          Future {
            assert(a == 1)
          }
        }

        Scenario("test 2") {
          Future {
            assert(a == 2)
          }
        }

        Scenario("test 3") {
          Future {
            pending
          }
        }

        Scenario("test 4") {
          Future {
            cancel()
          }
        }

        ignore("test 5") {
          Future {
            cancel()
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

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        val a = 1

        Scenario("test 1") {
          assert(a == 1)
        }

        Scenario("test 2") {
          assert(a == 2)
        }

        Scenario("test 3") {
          pending
        }

        Scenario("test 4") {
          cancel()
        }

        ignore("test 5") {
          cancel()
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

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        Scenario("test 1") {
          Future {
            SleepHelper.sleep(30)
            assert(count == 0)
            count = 1
            succeed
          }
        }

        Scenario("test 2") {
          Future {
            assert(count == 1)
            SleepHelper.sleep(50)
            count = 2
            succeed
          }
        }

        Scenario("test 3") {
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

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        Scenario("test 1") {
          SleepHelper.sleep(30)
          assert(count == 0)
          count = 1
          succeed
        }

        Scenario("test 2") {
          assert(count == 1)
          SleepHelper.sleep(50)
          count = 2
          succeed
        }

        Scenario("test 3") {
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

    // SKIP-SCALATESTJS,NATIVE-START
    it("should run tests and its future in same main thread when use SerialExecutionContext") {

      var mainThread = Thread.currentThread
      var test1Thread: Option[Thread] = None
      var test2Thread: Option[Thread] = None
      var onCompleteThread: Option[Thread] = None

      class ExampleSpec extends AsyncFeatureSpecLike {

        Scenario("test 1") {
          Future {
            test1Thread = Some(Thread.currentThread)
            succeed
          }
        }

        Scenario("test 2") {
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

        Scenario("test 1") {
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

        Scenario("test 2") {
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

        Scenario("test 1") {
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

        Scenario("test 1") {
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

      class ExampleSpec extends AsyncFeatureSpecLike {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        Scenario("test 1") {
          Future {
            SleepHelper.sleep(60)
            succeed
          }
        }

        Scenario("test 2") {
          Future {
            SleepHelper.sleep(30)
            succeed
          }
        }

        Scenario("test 3") {
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

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        Scenario("test 1") {
          SleepHelper.sleep(60)
          succeed
        }

        Scenario("test 2") {
          SleepHelper.sleep(30)
          succeed
        }

        Scenario("test 3") {
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

    it("should send an InfoProvided event for an info in main spec body") {
      class MySuite extends AsyncFeatureSpecLike  {
        info(
          "hi there"
        )
      }
      val suite = new MySuite
      val reporter = new EventRecordingReporter
      val status = suite.run(None, Args(reporter))
      // SKIP-SCALATESTJS-START
      status.waitUntilCompleted()
      // SKIP-SCALATESTJS-END

      val infoList = reporter.infoProvidedEventsReceived

      assert(infoList.size == 1)
      assert(infoList(0).message == "hi there")
    }

    it("should send an InfoProvided event for an info in feature body") {
      class MySuite extends AsyncFeatureSpecLike  {

        Feature("test feature") {
          info(
            "hi there"
          )

          Scenario("test 1") { succeed }
        }
      }
      val suite = new MySuite
      val reporter = new EventRecordingReporter
      val status = suite.run(None, Args(reporter))
      // SKIP-SCALATESTJS-START
      status.waitUntilCompleted()
      // SKIP-SCALATESTJS-END

      val infoList = reporter.infoProvidedEventsReceived

      assert(infoList.size == 1)
      assert(infoList(0).message == "hi there")
    }

    it("should send an InfoProvided event for an info in scenario body") {
      class MySuite extends AsyncFeatureSpecLike  {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        Feature("test feature") {
          Scenario("test 1") {
            info("hi there")
            succeed
          }
        }
      }
      val suite = new MySuite
      val reporter = new EventRecordingReporter
      val status = suite.run(None, Args(reporter))
      // SKIP-SCALATESTJS-START
      status.waitUntilCompleted()
      // SKIP-SCALATESTJS-END

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
      class MySuite extends AsyncFeatureSpecLike  {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        Feature("test feature") {
          Scenario("test 1") {
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
      // SKIP-SCALATESTJS-START
      status.waitUntilCompleted()
      // SKIP-SCALATESTJS-END

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
      class MySuite extends AsyncFeatureSpecLike  {
        note(
          "hi there"
        )
      }
      val suite = new MySuite
      val reporter = new EventRecordingReporter
      val status = suite.run(None, Args(reporter))
      // SKIP-SCALATESTJS-START
      status.waitUntilCompleted()
      // SKIP-SCALATESTJS-END

      val noteList = reporter.noteProvidedEventsReceived

      assert(noteList.size == 1)
      assert(noteList(0).message == "hi there")
    }

    it("should send a NoteProvided event for a note in feature body") {
      class MySuite extends AsyncFeatureSpecLike {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        Feature("test feature") {
          note(
            "hi there"
          )

          Scenario("test 1") {
            succeed
          }
        }
      }
      val suite = new MySuite
      val reporter = new EventRecordingReporter
      val status = suite.run(None, Args(reporter))

      // SKIP-SCALATESTJS-START
      status.waitUntilCompleted()
      // SKIP-SCALATESTJS-END

      val noteList = reporter.noteProvidedEventsReceived

      assert(noteList.size == 1)
      assert(noteList(0).message == "hi there")
    }

    it("should report as failed test when non-fatal exception is thrown from scenario body") {

      class ExampleSpec extends AsyncFeatureSpecLike {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        Scenario("test 1") {
          succeed
        }

        Scenario("test 2") {
          throw new IllegalStateException("oops!")
        }

        Scenario("test 3") {
          succeed
        }

      }

      val rep = new EventRecordingReporter
      val suite = new ExampleSpec
      val status = suite.run(None, Args(reporter = rep))

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

    it("should send a NoteProvided event for a note in scenario body") {
      class MySuite extends AsyncFeatureSpecLike  {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        Feature("test feature") {
          Scenario("test 1") {
            note("hi there")
            succeed
          }
        }
      }
      val suite = new MySuite
      val reporter = new EventRecordingReporter
      val status = suite.run(None, Args(reporter))
      // SKIP-SCALATESTJS-START
      status.waitUntilCompleted()
      // SKIP-SCALATESTJS-END

      val noteList = reporter.noteProvidedEventsReceived
      assert(noteList.size == 1)
      assert(noteList(0).message == "hi there")
    }

    it("should send a NoteProvided event for a note in Future returned by scenario body") {
      class MySuite extends AsyncFeatureSpecLike  {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        Feature("test feature") {
          Scenario("test 1") {
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
      // SKIP-SCALATESTJS-START
      status.waitUntilCompleted()
      // SKIP-SCALATESTJS-END

      val noteList = reporter.noteProvidedEventsReceived
      assert(noteList.size == 1)
      assert(noteList(0).message == "hi there")
    }

    it("should send an AlertProvided event for an alert in main spec body") {
      class MySuite extends AsyncFeatureSpecLike  {
        alert(
          "hi there"
        )
      }
      val suite = new MySuite
      val reporter = new EventRecordingReporter
      val status = suite.run(None, Args(reporter))
      // SKIP-SCALATESTJS-START
      status.waitUntilCompleted()
      // SKIP-SCALATESTJS-END

      val alertList = reporter.alertProvidedEventsReceived

      assert(alertList.size == 1)
      assert(alertList(0).message == "hi there")
    }

    it("should send an AlertProvided event for an alert in feature body") {
      class MySuite extends AsyncFeatureSpecLike  {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        Feature("test feature") {
          alert(
            "hi there"
          )

          Scenario("test 1") { succeed }
        }
      }
      val suite = new MySuite
      val reporter = new EventRecordingReporter
      val status = suite.run(None, Args(reporter))

      // SKIP-SCALATESTJS-START
      status.waitUntilCompleted()
      // SKIP-SCALATESTJS-END

      val alertList = reporter.alertProvidedEventsReceived

      assert(alertList.size == 1)
      assert(alertList(0).message == "hi there")
    }

    it("should report as failed test when non-fatal exception is thrown from Future returned by scenario body") {

      class ExampleSpec extends AsyncFeatureSpecLike {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        Scenario("test 1") {
          succeed
        }

        Scenario("test 2") {
          Future {
            throw new IllegalStateException("oops!")
          }
        }

        Scenario("test 3") {
          succeed
        }

      }

      val rep = new EventRecordingReporter
      val suite = new ExampleSpec
      val status = suite.run(None, Args(reporter = rep))

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

    it("should send an AlertProvided event for an alert in scenario body") {
      class MySuite extends AsyncFeatureSpecLike  {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        Feature("test feature") {
          Scenario("test 1") {
            alert("hi there")
            succeed
          }
        }
      }
      val suite = new MySuite
      val reporter = new EventRecordingReporter
      val status = suite.run(None, Args(reporter))
      // SKIP-SCALATESTJS-START
      status.waitUntilCompleted()
      // SKIP-SCALATESTJS-END

      val alertList = reporter.alertProvidedEventsReceived
      assert(alertList.size == 1)
      assert(alertList(0).message == "hi there")
    }

    it("should send an AlertProvided event for an alert in Future returned by scenario body") {
      class MySuite extends AsyncFeatureSpecLike  {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        Feature("test feature") {
          Scenario("test 1") {
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
      // SKIP-SCALATESTJS-START
      status.waitUntilCompleted()
      // SKIP-SCALATESTJS-END

      val alertList = reporter.alertProvidedEventsReceived
      assert(alertList.size == 1)
      assert(alertList(0).message == "hi there")
    }

    it("should send a MarkupProvided event for a markup in main spec body") {
      class MySuite extends AsyncFeatureSpecLike  {
        markup(
          "hi there"
        )
      }
      val suite = new MySuite
      val reporter = new EventRecordingReporter
      val status = suite.run(None, Args(reporter))
      // SKIP-SCALATESTJS-START
      status.waitUntilCompleted()
      // SKIP-SCALATESTJS-END

      val markupList = reporter.markupProvidedEventsReceived

      assert(markupList.size == 1)
      assert(markupList(0).text == "hi there")
    }

    it("should send a MarkupProvided event for a markup in feature body") {
      class MySuite extends AsyncFeatureSpecLike  {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        Feature("test feature") {
          markup(
            "hi there"
          )

          Scenario("test 1") { succeed }
        }
      }
      val suite = new MySuite
      val reporter = new EventRecordingReporter
      val status = suite.run(None, Args(reporter))
      // SKIP-SCALATESTJS-START
      status.waitUntilCompleted()
      // SKIP-SCALATESTJS-END

      val markupList = reporter.markupProvidedEventsReceived

      assert(markupList.size == 1)
      assert(markupList(0).text == "hi there")
    }

    it("should send a MarkupProvided event for a markup in scenario body") {
      class MySuite extends AsyncFeatureSpecLike  {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        Feature("test feature") {
          Scenario("test 1") {
            markup("hi there")
            succeed
          }
        }
      }
      val suite = new MySuite
      val reporter = new EventRecordingReporter
      val status = suite.run(None, Args(reporter))
      // SKIP-SCALATESTJS-START
      status.waitUntilCompleted()
      // SKIP-SCALATESTJS-END

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
      class MySuite extends AsyncFeatureSpecLike  {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        Feature("test feature") {
          Scenario("test 1") {
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
      // SKIP-SCALATESTJS-START
      status.waitUntilCompleted()
      // SKIP-SCALATESTJS-END

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



    // SKIP-SCALATESTJS,NATIVE-START
    it("should propagate fatal exception when thrown from scenario body") {

      class ExampleSpec extends AsyncFeatureSpecLike {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        Scenario("test 1") {
          succeed
        }

        Scenario("test 2") {
          throw new VirtualMachineError("oops!") {}
        }

        Scenario("test 3") {
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

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow

        Scenario("test 1") {
          succeed
        }

        Scenario("test 2") {
          Future {
            throw new VirtualMachineError("oops!") {}
          }
        }

        Scenario("test 3") {
          succeed
        }

      }

      val rep = new EventRecordingReporter
      assertThrows[VirtualMachineError] {
        val suite = new ExampleSpec
        suite.run(None, Args(reporter = rep))
      }
    }
    // SKIP-SCALATESTJS,NATIVE-END

    it("should generate NotAllowedException wrapping a DuplicateTestNameException is thrown inside scope") {
      class TestSpec extends AsyncFeatureSpec {
        Feature("a feature") {
          Scenario("test 1") { succeed }
          Scenario("test 1") { succeed }
        }
      }
      val e = intercept[NotAllowedException] {
          new TestSpec
        }
      assert("AsyncFeatureSpecLikeSpec.scala" == e.failedCodeFileName.get)
      assert(e.failedCodeLineNumber.get == thisLineNumber - 7)
      assert(e.cause.isDefined)
      val causeThrowable = e.cause.get
      assert(e.message == Some(FailureMessages.exceptionWasThrownInFeatureClause(prettifier, UnquotedString(causeThrowable.getClass.getName), "a feature", FailureMessages.duplicateTestName(prettifier, UnquotedString("Feature: a feature Scenario: test 1")))))

      assert(causeThrowable.isInstanceOf[DuplicateTestNameException])
      val cause = causeThrowable.asInstanceOf[DuplicateTestNameException]
      assert(cause.getMessage == FailureMessages.duplicateTestName(prettifier, UnquotedString("Feature: a feature Scenario: test 1")))
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

      class TestSpec extends AsyncFeatureSpecLike {
        // SKIP-SCALATESTJS-START
        override implicit val executionContext: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global
        // SKIP-SCALATESTJS-END
        //SCALATESTJS-ONLY override implicit val executionContext: ExecutionContext = CustomTestExecutionContext
        val a = 1
        Feature("feature 1") {
          Scenario("scenario A") {
            Future { assert(a == 1) }
          }
        }
        Feature("feature 2")  {
          Scenario("scenario B") {
            Future { assert(a == 1) }
          }
        }
        Feature("group3") {
          Scenario("test C") {
            Future { assert(a == 1) }
          }
        }
      }
      val suite = new TestSpec
      val reporter = new EventRecordingReporter
      val status = suite.run(None, Args(reporter))

      // SKIP-SCALATESTJS-START
      status.waitUntilCompleted()
      // SKIP-SCALATESTJS-END
      assert(reporter.scopeOpenedEventsReceived.length == 3)
      assert(reporter.scopeClosedEventsReceived.length == 3)
      assert(reporter.testStartingEventsReceived.length == 3)
      assert(reporter.testSucceededEventsReceived.length == 3)
      //SCALATESTJS-ONLY assert(changeMe)
    }

  }

}
