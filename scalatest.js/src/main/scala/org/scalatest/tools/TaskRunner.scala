package org.scalatest.tools

import org.scalatest.events.{TestFailed, ExceptionalEvent, Event}
import org.scalatest.tools.StringReporter._
import sbt.testing._
import org.scalajs.testinterface.TestUtils
import org.scalatest.{Suite, Args}
import java.util.concurrent.atomic.AtomicInteger

final class TaskRunner(task: TaskDef, cl: ClassLoader) extends Task {
  def tags(): Array[String] = Array.empty
  def taskDef(): TaskDef = task

  def execute(eventHandler: EventHandler, loggers: Array[Logger], continuation: (Array[Task]) => Unit): Unit = {
    continuation(execute(eventHandler, loggers))
  }

  def execute(eventHandler: EventHandler, loggers: Array[Logger]): Array[Task] = {
    val suite = TestUtils.newInstance(task.fullyQualifiedName, cl)(Seq.empty).asInstanceOf[Suite]
    val summaryCounter = new SummaryCounter
    val reporter = new SbtLogInfoReporter(
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
    suite.run(None, Args(reporter))
    Array.empty
  }

  private[tools] class SummaryCounter {
    val testsSucceededCount, testsFailedCount, testsIgnoredCount, testsPendingCount, testsCanceledCount, suitesCompletedCount, suitesAbortedCount, scopesPendingCount = new AtomicInteger
    val reminderEventsQueue = new scala.collection.mutable.ListBuffer[ExceptionalEvent]

    def incrementTestsSucceededCount() {
      testsSucceededCount.incrementAndGet()
    }

    def incrementTestsFailedCount() {
      testsFailedCount.incrementAndGet()
    }

    def incrementTestsIgnoredCount() {
      testsIgnoredCount.incrementAndGet()
    }

    def incrementTestsPendingCount() {
      testsPendingCount.incrementAndGet()
    }

    def incrementTestsCanceledCount() {
      testsCanceledCount.incrementAndGet()
    }

    def incrementSuitesCompletedCount() {
      suitesCompletedCount.incrementAndGet()
    }

    def incrementSuitesAbortedCount() {
      suitesAbortedCount.incrementAndGet()
    }

    def incrementScopesPendingCount() {
      scopesPendingCount.incrementAndGet()
    }

    def recordReminderEvents(events: ExceptionalEvent) {
      synchronized {
        reminderEventsQueue += events
      }
    }
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
