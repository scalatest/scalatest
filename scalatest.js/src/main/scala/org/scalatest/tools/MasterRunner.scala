package org.scalatest.tools

import org.scalatest.Tracker
import org.scalatest.events.Summary
import sbt.testing.{Framework => BaseFramework, Event => SbtEvent, Status => SbtStatus, _}

import scala.compat.Platform

class MasterRunner(theArgs: Array[String], theRemoteArgs: Array[String], testClassLoader: ClassLoader) extends Runner {

  // TODO: To take these from test arguments
  val presentAllDurations: Boolean = true
  val presentInColor: Boolean = true
  val presentShortStackTraces: Boolean = true
  val presentFullStackTraces: Boolean = true
  val presentUnformatted: Boolean = false
  val presentReminder: Boolean = false
  val presentReminderWithShortStackTraces: Boolean = false
  val presentReminderWithFullStackTraces: Boolean = false
  val presentReminderWithoutCanceledTests: Boolean = false

  val runStartTime = Platform.currentTime

  val tracker = new Tracker
  val summaryCounter = new SummaryCounter

  def done(): String = {
    val duration = Platform.currentTime - runStartTime
    val summary = new Summary(summaryCounter.testsSucceededCount, summaryCounter.testsFailedCount, summaryCounter.testsIgnoredCount, summaryCounter.testsPendingCount,
      summaryCounter.testsCanceledCount, summaryCounter.suitesCompletedCount, summaryCounter.suitesAbortedCount, summaryCounter.scopesPendingCount)
    val fragments: Vector[Fragment] =
      StringReporter.summaryFragments(
        true,
        Some(duration),
        Some(summary),
        Vector.empty ++ summaryCounter.reminderEventsQueue,
        presentAllDurations,
        presentReminder,
        presentReminderWithShortStackTraces,
        presentReminderWithFullStackTraces,
        presentReminderWithoutCanceledTests
      )
    fragments.map(_.toPossiblyColoredText(presentInColor)).mkString("\n")
  }

  def remoteArgs(): Array[String] = {
    theRemoteArgs
  }

  def args: Array[String] = {
    theArgs
  }

  def tasks(list: Array[TaskDef]): Array[Task] = {
    list.map(t => new TaskRunner(t, testClassLoader, tracker, presentAllDurations, presentInColor, presentShortStackTraces, presentFullStackTraces, presentUnformatted, presentReminder,
      presentReminderWithShortStackTraces, presentReminderWithFullStackTraces, presentReminderWithoutCanceledTests, None))
  }

  def receiveMessage(msg: String): Option[String] = {
    msg match {
      case "org.scalatest.events.TestPending" =>
        summaryCounter.incrementTestsPendingCount()
      case "org.scalatest.events.TestFailed" =>
        summaryCounter.incrementTestsFailedCount()
      case "org.scalatest.events.TestSucceeded" =>
        summaryCounter.incrementTestsSucceededCount()
      case "org.scalatest.events.TestIgnored" =>
        summaryCounter.incrementTestsIgnoredCount()
      case "org.scalatest.events.TestCanceled" =>
        summaryCounter.incrementTestsCanceledCount()
      case "org.scalatest.events.SuiteCompleted" =>
        summaryCounter.incrementSuitesCompletedCount()
      case "org.scalatest.events.SuiteAborted" =>
        summaryCounter.incrementSuitesAbortedCount()
      case "org.scalatest.events.ScopePending" =>
        summaryCounter.incrementScopesPendingCount()
      case _ =>
    }
    None
  }

  def serializeTask(task: Task, serializer: (TaskDef) => String): String =
    serializer(task.taskDef())

  def deserializeTask(task: String, deserializer: (String) => TaskDef): Task =
    new TaskRunner(deserializer(task), testClassLoader, tracker, presentAllDurations, presentInColor, presentShortStackTraces, presentFullStackTraces, presentUnformatted, presentReminder,
      presentReminderWithShortStackTraces, presentReminderWithFullStackTraces, presentReminderWithoutCanceledTests, None)

}