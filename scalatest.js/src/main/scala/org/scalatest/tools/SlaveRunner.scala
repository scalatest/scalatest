package org.scalatest.tools

import org.scalatest.{Resources, Tracker}
import org.scalatest.events.Summary
import sbt.testing.{Framework => BaseFramework, Event => SbtEvent, Status => SbtStatus, _}
import ArgsParser._

import scala.compat.Platform

class SlaveRunner(theArgs: Array[String], theRemoteArgs: Array[String], testClassLoader: ClassLoader, notifyServer: String => Unit) extends Runner {

  val sbtNoFormat = false   // System property not supported in scala-js
  val ParsedArgs(
    reporterArgs,
    tagsToIncludeArgs,
    tagsToExcludeArgs,
    membersOnlyArgs,
    wildcardArgs,
    suffixes
  ) = parseArgs(args)

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
    if (reporterArgs.length == 1 && reporterArgs(0).startsWith("-o")) {
      val configSet = parseConfigSet(reporterArgs(0))
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
    else if (reporterArgs.length > 1)
      throw new IllegalArgumentException("Only one -o can be passed in as test argument.")
    else
      (false, !sbtNoFormat, false, false, false, false, false, false, false, Set.empty[ReporterConfigParam])
  }

  val tagsToInclude: Set[String] = parseCompoundArgIntoSet(tagsToIncludeArgs, "-n")
  val tagsToExclude: Set[String] = parseCompoundArgIntoSet(tagsToExcludeArgs, "-l")
  val membersOnly: List[String] = parseSuiteArgsIntoNameStrings(membersOnlyArgs, "-m")
  val wildcard: List[String] = parseSuiteArgsIntoNameStrings(wildcardArgs, "-w")

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