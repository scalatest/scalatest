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

import org.scalatest.Tracker
import org.scalatest.events.{Summary, ExceptionalEvent}
import sbt.testing.{Framework => BaseFramework, Event => SbtEvent, Status => SbtStatus, _}

import scala.scalanative.reflect.Reflect

import scala.compat.Platform
import ArgsParser._
import org.scalatest.prop.Seed

import java.io.PrintWriter

class MasterRunner(theArgs: Array[String], theRemoteArgs: Array[String], testClassLoader: ClassLoader) extends Runner {

  // TODO: To take these from test arguments
  /*val presentAllDurations: Boolean = true
  val presentInColor: Boolean = true
  val presentShortStackTraces: Boolean = true
  val presentFullStackTraces: Boolean = true
  val presentUnformatted: Boolean = false
  val presentReminder: Boolean = false
  val presentReminderWithShortStackTraces: Boolean = false
  val presentReminderWithFullStackTraces: Boolean = false
  val presentReminderWithoutCanceledTests: Boolean = false*/

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

  val runStartTime = Platform.currentTime

  val tracker = new Tracker
  val summaryCounter = new SummaryCounter

  def done(): String = {
    val duration = Platform.currentTime - runStartTime
    val summary = new Summary(summaryCounter.testsSucceededCount.get(), summaryCounter.testsFailedCount.get(), summaryCounter.testsIgnoredCount.get(), summaryCounter.testsPendingCount.get(),
      summaryCounter.testsCanceledCount.get(), summaryCounter.suitesCompletedCount.get(), summaryCounter.suitesAbortedCount.get(), summaryCounter.scopesPendingCount.get())
    val fragments: Vector[Fragment] =
      StringReporter.summaryFragments(
        true,
        Some(duration),
        Some(summary),
        summaryCounter.reminderEventsQueue.toArray(Array.empty[ExceptionalEvent]).toVector,
        presentAllDurations,
        presentReminder,
        presentReminderWithShortStackTraces,
        presentReminderWithFullStackTraces,
        presentReminderWithoutCanceledTests,
        presentFilePathname
      )
    fragments.map(_.toPossiblyColoredText(presentInColor)).mkString("\n")
  }

  def remoteArgs(): Array[String] = 
    theRemoteArgs

  def args: Array[String] = 
    theArgs

  def tasks(taskDefs: Array[TaskDef]): Array[Task] = {
    def filterWildcard(paths: List[String], taskDefs: Array[TaskDef]): Array[TaskDef] =
      taskDefs.filter(td => paths.exists(td.fullyQualifiedName().startsWith(_)))

    def filterMembersOnly(paths: List[String], taskDefs: Array[TaskDef]): Array[TaskDef] =
      taskDefs.filter { td =>
        paths.exists(path => td.fullyQualifiedName().startsWith(path) && td.fullyQualifiedName().substring(path.length).lastIndexOf('.') <= 0)
      }

    val discoveredSuites = taskDefs.map(_.fullyQualifiedName()).toSet
    Runner.setDiscoveredSuites(discoveredSuites)  

    def createTask(t: TaskDef): Task =
      new TaskRunner(t, testClassLoader, tracker, tagsToInclude, tagsToExclude, t.selectors() ++ autoSelectors, false, presentAllDurations, presentInColor, presentShortStackTraces, presentFullStackTraces, presentUnformatted, presentReminder,
        presentReminderWithShortStackTraces, presentReminderWithFullStackTraces, presentReminderWithoutCanceledTests, presentFilePathname, presentJson, Some(send))

    val tasks = (if (wildcard.isEmpty && membersOnly.isEmpty) taskDefs else (filterWildcard(wildcard, taskDefs) ++ filterMembersOnly(membersOnly, taskDefs)).distinct).map(createTask)

    tasks
  }

  private def send(msg: String): Unit = {
    receiveMessage(msg)
    ()
  }

  private lazy val getDiscoveredSuitesPattern = "getDiscoveredSuites-(.+)".r

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
      case getDiscoveredSuitesPattern(filePath)  =>
        val tempFile = new java.io.File(filePath)
        val writer = new PrintWriter(filePath)
        try {
          writer.write(Runner.discoveredSuites.get.mkString(","))
          writer.flush()
        } finally {
          writer.close()
        }  
      case _ =>
    }
    None
  }

  def serializeTask(task: Task, serializer: (TaskDef) => String): String = {
    serializer(task.taskDef())
  }

  def deserializeTask(task: String, deserializer: (String) => TaskDef): Task = {
    val taskDef = deserializer(task)
    new TaskRunner(taskDef, testClassLoader, tracker, tagsToInclude, tagsToExclude, taskDef.selectors() ++ autoSelectors, false, presentAllDurations, presentInColor, presentShortStackTraces, presentFullStackTraces, presentUnformatted, presentReminder,
      presentReminderWithShortStackTraces, presentReminderWithFullStackTraces, presentReminderWithoutCanceledTests, presentFilePathname, presentJson, Some(send))
  }

}
