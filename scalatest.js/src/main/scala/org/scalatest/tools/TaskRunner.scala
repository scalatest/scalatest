package org.scalatest.tools

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
import org.scalatest.{Suite, Args, Tracker}
import java.util.concurrent.atomic.AtomicInteger

import scala.compat.Platform

final class TaskRunner(task: TaskDef, cl: ClassLoader, tracker: Tracker, summaryCounter: SummaryCounter) extends Task {
  def tags(): Array[String] = Array.empty
  def taskDef(): TaskDef = task

  def execute(eventHandler: EventHandler, loggers: Array[Logger], continuation: (Array[Task]) => Unit): Unit = {
    continuation(execute(eventHandler, loggers))
  }

  def execute(eventHandler: EventHandler, loggers: Array[Logger]): Array[Task] = {
    val suiteStartTime = Platform.currentTime
    val suite = TestUtils.newInstance(task.fullyQualifiedName, cl)(Seq.empty).asInstanceOf[Suite]
    val summaryCounter = new SummaryCounter
    val sbtLogInfoReporter = new SbtLogInfoReporter(
      loggers,
      true,
      true,
      true,
      true, // If they say both S and F, F overrules
      false,
      false,
      false,
      false,
      false,
      summaryCounter
    )

    val formatter = Suite.formatterForSuiteStarting(suite)
    val suiteClass = suite.getClass

    val reporter = new SbtReporter(suite.suiteId, task.fullyQualifiedName, task.fingerprint, eventHandler, sbtLogInfoReporter, summaryCounter)

    if (!suite.isInstanceOf[DistributedTestRunnerSuite])
      reporter(SuiteStarting(tracker.nextOrdinal(), suite.suiteName, suite.suiteId, Some(suiteClass.getName), formatter, Some(TopOfClass(suiteClass.getName))))

    try {
      suite.run(None, Args(reporter))
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
                                    summaryCounter: SummaryCounter
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
      event match {
        case ee: ExceptionalEvent if presentReminder =>
          if (!presentReminderWithoutCanceledTests || event.isInstanceOf[TestFailed]) {
            summaryCounter.recordReminderEvents(ee)
          }
        case _ =>
      }
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
