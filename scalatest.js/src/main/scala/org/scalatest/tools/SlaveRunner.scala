package org.scalatest.tools

import org.scalatest.{Resources, Tracker}
import org.scalatest.events.Summary
import org.scalatest.tools.ReporterConfigParam
import sbt.testing.{Framework => BaseFramework, Event => SbtEvent, Status => SbtStatus, _}

import scala.compat.Platform

class SlaveRunner(theArgs: Array[String], theRemoteArgs: Array[String], testClassLoader: ClassLoader, notifyServer: String => Unit) extends Runner {

  private def parseConfigSet(reporterArg: String): Set[ReporterConfigParam] = {

    if (reporterArg == null)
      throw new IllegalArgumentException("reporterArg was null")

    if (reporterArg.length < 2)
      throw new IllegalArgumentException("reporterArg < 2")

    // The reporterArg passed includes the initial -, as in "-oFI",
    // so the first config param will be at index 2
    val configString = reporterArg.substring(2)
    val it = configString.iterator
    var set = Set[ReporterConfigParam]()
    while (it.hasNext)
      it.next match {
        case 'Y' =>  throw new IllegalArgumentException("Use of Y was deprecated in ScalaTest 1.0 and removed in 1.5. Please check the Scaladoc documentation of org.scalatest.Runner for information on valid Reporter config parameters.")
        case 'Z' => throw new IllegalArgumentException("Use of Z was deprecated in ScalaTest 1.0 and removed in 1.5. Please check the Scaladoc documentation of org.scalatest.Runner for information on valid Reporter config parameters.")
        //case 'P' =>throw new IllegalArgumentException("Use of P was deprecated in ScalaTest 1.0 and removed in 1.5. Please check the Scaladoc documentation of org.scalatest.Runner for information on valid Reporter config parameters.")
        case 'B' =>throw new IllegalArgumentException("Use of B was deprecated in ScalaTest 1.0 and removed in 1.5. Please check the Scaladoc documentation of org.scalatest.Runner for information on valid Reporter config parameters.")
        // case 'S' => // Use for Short Stack Traces
        case 'A' =>throw new IllegalArgumentException("Use of A was deprecated in ScalaTest 1.0 and removed in 1.5. Please check the Scaladoc documentation of org.scalatest.Runner for information on valid Reporter config parameters.")
        //case 'R' =>throw new IllegalArgumentException("Use of R was deprecated in ScalaTest 1.0 and removed in 1.5. Please check the Scaladoc documentation of org.scalatest.Runner for information on valid Reporter config parameters.")
        case 'I' => set += PresentReminderWithoutStackTraces
        case 'T' => set += PresentReminderWithShortStackTraces
        case 'G' => set += PresentReminderWithFullStackTraces
        case 'K' => set += PresentReminderWithoutCanceledTests
        case 'N' => set += FilterTestStarting
        case 'C' => set += FilterTestSucceeded
        case 'X' => set += FilterTestIgnored
        case 'E' => set += FilterTestPending
        case 'H' => set += FilterSuiteStarting
        case 'L' => set += FilterSuiteCompleted
        case 'O' => set += FilterInfoProvided
        case 'P' => set += FilterScopeOpened
        case 'Q' => set += FilterScopeClosed
        case 'R' => set += FilterScopePending
        case 'M' => set += FilterMarkupProvided
        case 'W' => set += PresentWithoutColor
        case 'F' => set += PresentFullStackTraces
        case 'S' => set += PresentShortStackTraces
        case 'D' => set += PresentAllDurations
        case 'U' => set += PresentUnformatted
        case c: Char => {

          // this should be moved to the checker, and just throw an exception here with a debug message. Or allow a MatchError.
          val msg1 = Resources.invalidConfigOption(String.valueOf(c)) + '\n'
          val msg2 =  Resources.probarg(reporterArg) + '\n'

          throw new IllegalArgumentException(msg1 + msg2)
        }
      }
    set
  }

  val sbtNoFormat = false   // System property not supported in scala-js
  val (stdoutArgs, nonStdoutArgs) = theArgs.partition(_.startsWith("-o"))

  val (
  presentAllDurations,
  presentInColor,
  presentShortStackTraces,
  presentFullStackTraces,
  presentUnformatted,
  presentReminder,
  presentReminderWithShortStackTraces,
  presentReminderWithFullStackTraces,
  presentReminderWithoutCanceledTests,
  configSet
  ) = {
    if (stdoutArgs.length == 1) {
      val configSet = parseConfigSet(stdoutArgs(0))
      (
        configSet.contains(PresentAllDurations),
        !configSet.contains(PresentWithoutColor) && !sbtNoFormat,
        configSet.contains(PresentShortStackTraces) || configSet.contains(PresentFullStackTraces),
        configSet.contains(PresentFullStackTraces),
        configSet.contains(PresentUnformatted),
        configSet.exists { ele =>
          ele == PresentReminderWithoutStackTraces || ele == PresentReminderWithShortStackTraces || ele == PresentReminderWithFullStackTraces
        },
        configSet.contains(PresentReminderWithShortStackTraces) && !configSet.contains(PresentReminderWithFullStackTraces),
        configSet.contains(PresentReminderWithFullStackTraces),
        configSet.contains(PresentReminderWithoutCanceledTests),
        configSet
      )
    }
    else if (stdoutArgs.length > 1)
      throw new IllegalArgumentException("More than one -o is passed in as test argument.")
    else
      (false, !sbtNoFormat, false, false, false, false, false, false, false, Set.empty[ReporterConfigParam])
  }

  val tracker = new Tracker

  def done(): String = ""

  def remoteArgs(): Array[String] = {
    theRemoteArgs
  }

  def args: Array[String] = {
    theArgs
  }

  def tasks(list: Array[TaskDef]): Array[Task] = {
    list.map(t => new TaskRunner(t, testClassLoader, tracker, presentAllDurations, presentInColor, presentShortStackTraces, presentFullStackTraces, presentUnformatted, presentReminder,
      presentReminderWithShortStackTraces, presentReminderWithFullStackTraces, presentReminderWithoutCanceledTests, Some(notifyServer)))
  }

  def receiveMessage(msg: String): Option[String] =
    None

  def serializeTask(task: Task, serializer: (TaskDef) => String): String =
    serializer(task.taskDef())

  def deserializeTask(task: String, deserializer: (String) => TaskDef): Task =
    new TaskRunner(deserializer(task), testClassLoader, tracker, presentAllDurations, presentInColor, presentShortStackTraces, presentFullStackTraces, presentUnformatted, presentReminder,
      presentReminderWithShortStackTraces, presentReminderWithFullStackTraces, presentReminderWithoutCanceledTests, Some(notifyServer))

}