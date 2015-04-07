package org.scalatest.tools

import org.scalatest.{Tracker, Reporter}
import org.scalatest.events.{ExceptionalEvent, Summary}
import sbt.testing.{Framework => BaseFramework, Event => SbtEvent, Status => SbtStatus, _}
import scala.collection.mutable.ListBuffer
import scala.compat.Platform

class Framework extends BaseFramework {
  
  def name: String = "ScalaTest"

  def fingerprints: Array[Fingerprint] =
    Array(
      new SubclassFingerprint {
        def superclassName = "org.scalatest.Suite"
        def isModule = false
        def requireNoArgConstructor = true
      })


  def slaveRunner(args: Array[String], remoteArgs: Array[String], testClassLoader: ClassLoader, send: (String) => Unit): Runner =
    runner(args, remoteArgs, testClassLoader)

  def runner(args: Array[String], remoteArgs: Array[String], testClassLoader: ClassLoader): Runner = {
    val theseArgs = args
    val theseRemoteArgs = remoteArgs

    new Runner {

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
        ""  // return empty string for now, because done get called multiple time for some reason, let's ask Sebastien about it.
      }

      def remoteArgs(): Array[String] = {
        theseRemoteArgs
      }

      def args: Array[String] = {
        theseArgs
      }

      def tasks(list: Array[TaskDef]): Array[Task] = {
        list.map(t => new TaskRunner(t, testClassLoader, tracker, summaryCounter, presentAllDurations, presentInColor, presentShortStackTraces, presentFullStackTraces, presentUnformatted, presentReminder,
                                     presentReminderWithShortStackTraces, presentReminderWithFullStackTraces, presentReminderWithoutCanceledTests))
      }

      def receiveMessage(msg: String): Option[String] = {
        None
      }

      def serializeTask(task: Task, serializer: (TaskDef) => String): String =
        serializer(task.taskDef())

      def deserializeTask(task: String, deserializer: (String) => TaskDef): Task =
        new TaskRunner(deserializer(task), testClassLoader, tracker, summaryCounter, presentAllDurations, presentInColor, presentShortStackTraces, presentFullStackTraces, presentUnformatted, presentReminder,
                       presentReminderWithShortStackTraces, presentReminderWithFullStackTraces, presentReminderWithoutCanceledTests)
    }
  }
}
