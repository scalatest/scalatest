/*
 * Copyright 2001-2015 Artima, Inc.
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

import org.scalatest.{Resources, Tracker}
import org.scalatest.events.Summary
import sbt.testing.{Framework => BaseFramework, Event => SbtEvent, Status => SbtStatus, _}
import ArgsParser._

import scala.compat.Platform
import org.scalatest.prop.Seed

class SlaveRunner(theArgs: Array[String], theRemoteArgs: Array[String], testClassLoader: ClassLoader, notifyServer: String => Unit) extends Runner {

  val sbtNoFormat = false   // System property not supported in scala-js
  val ParsedArgs(
    reporterArgs,
    suiteArgs,
    tagsToIncludeArgs,
    tagsToExcludeArgs,
    membersOnlyArgs,
    wildcardArgs, 
    seedArgs
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
  presentFilePathname,
  presentJson,
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
        configSet.contains(PresentFilePathname),
        configSet.contains(PresentJson),
        configSet
      )
    }
    else if (reporterArgs.length > 1)
      throw new IllegalArgumentException("Only one -o can be passed in as test argument.")
    else
      (false, !sbtNoFormat, false, false, false, false, false, false, false, false, false, Set.empty[ReporterConfigParam])
  }

  parseLongArgument(seedArgs, "-S") match {
    case Some(seed) => Seed.configuredRef.getAndSet(Some(seed))
    case None => // do nothing
  }

  val tagsToInclude: Set[String] = parseCompoundArgIntoSet(tagsToIncludeArgs, "-n")
  val tagsToExclude: Set[String] = parseCompoundArgIntoSet(tagsToExcludeArgs, "-l")
  val membersOnly: List[String] = parseSuiteArgsIntoNameStrings(membersOnlyArgs, "-m")
  val wildcard: List[String] = parseSuiteArgsIntoNameStrings(wildcardArgs, "-w")

  private def parseSuiteArgs(suiteArgs: List[String]): List[Selector] = {
    val itr = suiteArgs.iterator
    val wildcards = new scala.collection.mutable.ListBuffer[Selector]()
    while (itr.hasNext) {
      val next = itr.next
      next match {
        case "-z" =>
          if (itr.hasNext)
            wildcards += new TestWildcardSelector(itr.next)
          else
            new IllegalArgumentException("-z must be followed by a wildcard string.")
        case "-t" =>
          if (itr.hasNext)
            wildcards += new TestSelector(itr.next)
          else
            new IllegalArgumentException("-t must be followed by a test name string.")
        case _ =>
          throw new IllegalArgumentException("Specifying a suite (-s <suite>) or nested suite (-i <nested suite>) is not supported when running ScalaTest from sbt; Please use sbt's test-only instead.")
      }
    }
    wildcards.toList
  }

  val autoSelectors = parseSuiteArgs(suiteArgs)

  val tracker = new Tracker

  def done(): String = ""

  def remoteArgs(): Array[String] = {
    theRemoteArgs
  }

  def args: Array[String] = {
    theArgs
  }

  def tasks(taskDefs: Array[TaskDef]): Array[Task] = {

    def filterWildcard(paths: List[String], taskDefs: Array[TaskDef]): Array[TaskDef] =
      taskDefs.filter(td => paths.exists(td.fullyQualifiedName.startsWith(_)))

    def filterMembersOnly(paths: List[String], taskDefs: Array[TaskDef]): Array[TaskDef] =
      taskDefs.filter { td =>
        paths.exists(path => td.fullyQualifiedName.startsWith(path) && td.fullyQualifiedName.substring(path.length).lastIndexOf('.') <= 0)
      }

    def createTask(t: TaskDef): Task =
      new TaskRunner(t, testClassLoader, tracker, tagsToInclude, tagsToExclude, t.selectors ++ autoSelectors, t.explicitlySpecified, presentAllDurations, presentInColor, presentShortStackTraces, presentFullStackTraces, presentUnformatted, presentReminder,
        presentReminderWithShortStackTraces, presentReminderWithFullStackTraces, presentReminderWithoutCanceledTests, presentFilePathname, presentJson, Some(notifyServer))

    (if (wildcard.isEmpty && membersOnly.isEmpty) taskDefs else (filterWildcard(wildcard, taskDefs) ++ filterMembersOnly(membersOnly, taskDefs)).distinct).map(createTask)
  }

  def receiveMessage(msg: String): Option[String] =
    None

  def serializeTask(task: Task, serializer: (TaskDef) => String): String =
    serializer(task.taskDef())

  def deserializeTask(task: String, deserializer: (String) => TaskDef): Task = {
    val taskDef = deserializer(task)
    new TaskRunner(taskDef, testClassLoader, tracker, tagsToInclude, tagsToExclude, taskDef.selectors ++ autoSelectors, taskDef.explicitlySpecified, presentAllDurations, presentInColor, presentShortStackTraces, presentFullStackTraces, presentUnformatted, presentReminder,
      presentReminderWithShortStackTraces, presentReminderWithFullStackTraces, presentReminderWithoutCanceledTests, presentFilePathname, presentJson, Some(notifyServer))
  }

}