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
package org.scalatest.tools

import org.scalatest.Suite._
import org.scalatest.events.{TestFailed,
                             ExceptionalEvent,
                             Event,
                             SuiteStarting,
                             TopOfClass,
                             SuiteCompleted,
                             SuiteAborted,
                             SeeStackDepthException}
import org.scalatest.tools.StringReporter._
import sbt.testing._
import scala.scalajs.reflect.Reflect
import org.scalatest._
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.Promise
import scala.concurrent.Future
import scala.concurrent.Await
import scala.util.Success

import scala.compat.Platform
import scala.concurrent.duration.Duration

final class TaskRunner(task: TaskDef,
                       cl: ClassLoader,
                       tracker: Tracker,
                       tagsToInclude: Set[String],
                       tagsToExclude: Set[String],
                       selectors: Array[Selector],
                       explicitlySpecified: Boolean,
                       presentAllDurations: Boolean,
                       presentInColor: Boolean,
                       presentShortStackTraces: Boolean,
                       presentFullStackTraces: Boolean,
                       presentUnformatted: Boolean,
                       presentReminder: Boolean,
                       presentReminderWithShortStackTraces: Boolean,
                       presentReminderWithFullStackTraces: Boolean,
                       presentReminderWithoutCanceledTests: Boolean,
                       presentFilePathname: Boolean,
                       presentJson: Boolean,
                       notifyServer: Option[String => Unit]) extends Task {
  def tags(): Array[String] = Array.empty
  def taskDef(): TaskDef = task

  def execute(eventHandler: EventHandler, loggers: Array[Logger], continuation: (Array[Task]) => Unit): Unit = {
    import org.scalajs.macrotaskexecutor.MacrotaskExecutor.Implicits._
    val future = executionFuture(eventHandler, loggers)
    future.recover { case t =>
      loggers.foreach(_.trace(t))
    }.onComplete{ _ =>
      continuation(Array.empty)
    }
  }

  def execute(eventHandler: EventHandler, loggers: Array[Logger]): Array[Task] = {
    Await.result(executionFuture(eventHandler, loggers), Duration.Inf)
    Array.empty
  }

  def executionFuture(eventHandler: EventHandler, loggers: Array[Logger]): Future[Unit] = {
    val suiteStartTime = Platform.currentTime
    val suite = Reflect.lookupInstantiatableClass(task.fullyQualifiedName).getOrElse(throw new RuntimeException("Cannot load suite class: " + task.fullyQualifiedName)).newInstance().asInstanceOf[Suite]
    val sbtLogInfoReporter = new SbtLogInfoReporter(
      loggers,
      presentAllDurations,
      presentInColor,
      presentShortStackTraces,
      presentFullStackTraces,
      presentUnformatted,
      presentReminder,
      presentReminderWithShortStackTraces,
      presentReminderWithFullStackTraces,
      presentReminderWithoutCanceledTests,
      presentFilePathname,
      presentJson,
      notifyServer
    )

    val filter =
      if ((selectors.length == 1 && selectors(0).isInstanceOf[SuiteSelector] && !explicitlySpecified))  // selectors will always at least have one SuiteSelector, according to javadoc of TaskDef
        Filter(if (tagsToInclude.isEmpty) None else Some(tagsToInclude), tagsToExclude)
      else {
        var suiteTags = Map[String, Set[String]]()
        var testTags = Map[String, Map[String, Set[String]]]()
        var hasTest = false
        var hasNested = false

        selectors.foreach { selector =>
          selector match {
            case suiteSelector: SuiteSelector =>
              suiteTags = mergeMap[String, Set[String]](List(suiteTags, Map(suite.suiteId -> Set(SELECTED_TAG)))) { _ ++ _ }
            case testSelector: TestSelector =>
              testTags = mergeMap[String, Map[String, Set[String]]](List(testTags, Map(suite.suiteId -> Map(testSelector.testName -> Set(SELECTED_TAG))))) { (testMap1, testMap2) =>
                mergeMap[String, Set[String]](List(testMap1, testMap2)) { _ ++ _}
              }
              hasTest = true
            case testWildcardSelector: TestWildcardSelector =>
              val filteredTestNames = suite.testNames.filter(_.contains(testWildcardSelector.testWildcard))
              val selectorTestTags = Map.empty ++ filteredTestNames.map(_ -> Set(SELECTED_TAG))
              testTags = mergeMap[String, Map[String, Set[String]]](List(testTags, Map(suite.suiteId -> selectorTestTags))) { (testMap1, testMap2) =>
                mergeMap[String, Set[String]](List(testMap1, testMap2)) { _ ++ _}
              }
              hasTest = true
            case nestedSuiteSelector: NestedSuiteSelector =>
              suiteTags = mergeMap[String, Set[String]](List(suiteTags, Map(nestedSuiteSelector.suiteId -> Set(SELECTED_TAG)))) { _ ++ _ }
              hasNested = true
            case nestedTestSelector: NestedTestSelector =>
              testTags = mergeMap[String, Map[String, Set[String]]](List(testTags, Map(nestedTestSelector.suiteId -> Map(nestedTestSelector.testName -> Set(SELECTED_TAG))))) { (testMap1, testMap2) =>
                mergeMap[String, Set[String]](List(testMap1, testMap2)) { _ ++ _}
              }
              hasNested = true
          }
        }

        // Only exclude nested suites when using -s XXX -t XXXX, same behaviour with Runner.
        val excludeNestedSuites = hasTest && !hasNested
        // For suiteTags, we need to remove them if there's entry in testTags already, because testTags is more specific.
        Filter(if (tagsToInclude.isEmpty) Some(Set(SELECTED_TAG)) else Some(tagsToInclude + SELECTED_TAG), tagsToExclude, false, new DynaTags(suiteTags.filter(s => !testTags.contains(s._1)).toMap, testTags.toMap))
      }

    val formatter = Suite.formatterForSuiteStarting(suite)
    val suiteClassName = Suite.getSuiteClassName(suite)

    val reporter = new SbtReporter(suite.suiteId, task.fullyQualifiedName, task.fingerprint, eventHandler, sbtLogInfoReporter)

    if (!suite.isInstanceOf[DistributedTestRunnerSuite])
      reporter(SuiteStarting(tracker.nextOrdinal(), suite.suiteName, suite.suiteId, Some(suiteClassName), formatter, Some(TopOfClass(suiteClassName))))

    val args = Args(reporter, Stopper.default, filter, ConfigMap.empty, None, tracker)

    val future: Future[Unit] =
      try {
        val status = suite.run(None, args)
        val promise = Promise[Unit]
        status.whenCompleted { _ =>
          val formatter = Suite.formatterForSuiteCompleted(suite)
          val duration = Platform.currentTime
          status.unreportedException match {
            case Some(ue) =>
              reporter(SuiteAborted(tracker.nextOrdinal(), ue.getMessage, suite.suiteName, suite.suiteId, Some(suiteClassName), Some(ue), Some(duration), formatter, Some(SeeStackDepthException)))
              promise.complete(scala.util.Failure(ue))

            case None =>
              reporter(SuiteCompleted(tracker.nextOrdinal(), suite.suiteName, suite.suiteId, Some(suiteClassName), Some(duration), formatter, Some(TopOfClass(suiteClassName))))
              promise.complete(Success(()))
          }
        }
        promise.future
      } catch {
          case e: Throwable =>
            val rawString = "Exception encountered when attempting to run a suite with class name: " + suiteClassName
            val formatter = Suite.formatterForSuiteAborted(suite, rawString)
  
            val duration = Platform.currentTime - suiteStartTime
            // Do fire SuiteAborted even if a DistributedTestRunnerSuite, consistent with SuiteRunner behavior
            reporter(SuiteAborted(tracker.nextOrdinal(), rawString, suite.suiteName, suite.suiteId, Some(suiteClassName), Some(e), Some(duration), formatter, Some(SeeStackDepthException)))
        Future.failed(e)
      }

    future
  }

  private class SbtLogInfoReporter(
                                    loggers: Array[Logger],
                                    presentAllDurations: Boolean,
                                    presentInColor: Boolean,
                                    presentShortStackTraces: Boolean,
                                    presentFullStackTraces: Boolean,
                                    presentUnformatted: Boolean,
                                    presentReminder: Boolean,
                                    presentReminderWithShortStackTraces: Boolean,
                                    presentReminderWithFullStackTraces: Boolean,
                                    presentReminderWithoutCanceledTests: Boolean,
                                    presentFilePathname: Boolean,
                                    presentJson: Boolean,
                                    notifyServer: Option[String => Unit]
                                    ) extends StringReporter(
    presentAllDurations,
    presentInColor,
    presentShortStackTraces,
    presentFullStackTraces,
    presentUnformatted,
    presentReminder,
    presentReminderWithShortStackTraces,
    presentReminderWithFullStackTraces,
    presentReminderWithoutCanceledTests,
    presentFilePathname,
    presentJson
  ) {

    protected def printPossiblyInColor(fragment: Fragment): Unit = {
      loggers.foreach { logger =>
        logger.info(fragment.toPossiblyColoredText(logger.ansiCodesSupported && presentInColor))
      }
    }

    protected def printNoColor(text: String): Unit = {
      loggers.foreach { logger =>
        logger.info(text)
      }
    }

    override def apply(event: Event): Unit = {
      /*event match {
        case ee: ExceptionalEvent if presentReminder =>
          if (!presentReminderWithoutCanceledTests || event.isInstanceOf[TestFailed]) {
            summaryCounter.recordReminderEvents(ee)
          }
        case _ =>
      }*/
      notifyServer.foreach(send => send(event.getClass.getName))

      if (presentJson)
        printNoColor(event.toJson)
      else
        fragmentsForEvent(
          event,
          presentUnformatted,
          presentAllDurations,
          presentShortStackTraces,
          presentFullStackTraces,
          presentReminder,
          presentReminderWithShortStackTraces,
          presentReminderWithFullStackTraces,
          presentReminderWithoutCanceledTests,
          presentFilePathname,
          reminderEventsBuf
        ) foreach printPossiblyInColor
    }

    def dispose() = ()
  }
}
