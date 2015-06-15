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
import org.scalajs.testinterface.TestUtils
import org.scalatest._
import java.util.concurrent.atomic.AtomicInteger

import scala.compat.Platform

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
                       notifyServer: Option[String => Unit]) extends Task {
  def tags(): Array[String] = Array.empty
  def taskDef(): TaskDef = task

  def execute(eventHandler: EventHandler, loggers: Array[Logger], continuation: (Array[Task]) => Unit): Unit = {
    continuation(execute(eventHandler, loggers))
  }

  def execute(eventHandler: EventHandler, loggers: Array[Logger]): Array[Task] = {
    val suiteStartTime = Platform.currentTime
    val suite = TestUtils.newInstance(task.fullyQualifiedName, cl)(Seq.empty).asInstanceOf[Suite]
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
    val suiteClass = suite.getClass

    val reporter = new SbtReporter(suite.suiteId, task.fullyQualifiedName, task.fingerprint, eventHandler, sbtLogInfoReporter)

    if (!suite.isInstanceOf[DistributedTestRunnerSuite])
      reporter(SuiteStarting(tracker.nextOrdinal(), suite.suiteName, suite.suiteId, Some(suiteClass.getName), formatter, Some(TopOfClass(suiteClass.getName))))

    val args = Args(reporter, Stopper.default, filter, ConfigMap.empty, None, tracker, Set.empty)

    try {
      suite.run(None, args)
      val formatter = Suite.formatterForSuiteCompleted(suite)
      val duration = Platform.currentTime
      if (!suite.isInstanceOf[DistributedTestRunnerSuite])
        reporter(SuiteCompleted(tracker.nextOrdinal(), suite.suiteName, suite.suiteId, Some(suiteClass.getName), Some(duration), formatter, Some(TopOfClass(suiteClass.getName))))
    } catch {
        case e: Throwable =>
          val rawString = "Exception encountered when attempting to run a suite with class name: " + suiteClass.getName
          val formatter = Suite.formatterForSuiteAborted(suite, rawString)

          val duration = Platform.currentTime - suiteStartTime
          // Do fire SuiteAborted even if a DistributedTestRunnerSuite, consistent with SuiteRunner behavior
          reporter(SuiteAborted(tracker.nextOrdinal(), rawString, suite.suiteName, suite.suiteId, Some(suiteClass.getName), Some(e), Some(duration), formatter, Some(SeeStackDepthException)))
    }


    Array.empty
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
    presentReminderWithoutCanceledTests
  ) {

    protected def printPossiblyInColor(fragment: Fragment) {
      loggers.foreach { logger =>
        logger.info(fragment.toPossiblyColoredText(logger.ansiCodesSupported && presentInColor))
      }
    }

    override def apply(event: Event) {
      /*event match {
        case ee: ExceptionalEvent if presentReminder =>
          if (!presentReminderWithoutCanceledTests || event.isInstanceOf[TestFailed]) {
            summaryCounter.recordReminderEvents(ee)
          }
        case _ =>
      }*/
      notifyServer.foreach(send => send(event.getClass.getName))

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
        reminderEventsBuf
      ) foreach printPossiblyInColor
    }

    def dispose() = ()
  }
}
