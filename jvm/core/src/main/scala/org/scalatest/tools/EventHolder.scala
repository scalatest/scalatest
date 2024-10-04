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

import org.scalatest._
import org.scalatest.events._
import EventHolder.suiteAndTestName
import org.scalactic.Requirements._

/**
 * Used to hold Reports in the GUI, so that I can keep track of which report method was called
 * on the reporter to deliver it.
 *
 * @author Bill Venners
 */
private[tools] class EventHolder(val event: Event, val message: Option[String], val throwable: Option[Throwable],
    val rerunner: Option[String], val summary: Option[Summary], val isRerun: Boolean) {

  requireNonNull(event, message, throwable, rerunner, summary)
 
  def this(event: Event, message: Option[String], throwable: Option[Throwable],
      rerunner: Option[String]) = this(event, message, throwable, rerunner, None, false)

  def this(event: Event, message: Option[String], throwable: Option[Throwable],
      rerunner: Option[String], summary: Option[Summary]) = this(event, message, throwable, rerunner, summary, false)

  override def toString = {
    event.formatter match {
      case Some(IndentedText(_, rawText, indentationLevel)) =>
        event match {
          case _: SuiteStarting => rawText + ":"
          case _: TestPending => Resources.specTextAndNote(rawText, Resources.pendingNote)
          case _: ScopePending => Resources.specTextAndNote(rawText, Resources.pendingNote)
          case _ => rawText
        }
      case _ =>
        val firstString: String = RunnerJFrame.getEventToPresentDisplayMessage(event, isRerun)

        def firstAndSecondString(first: String, second: String) = first + " - " + second

        event match {
          case event: DiscoveryStarting => firstString
          case event: DiscoveryCompleted => firstString
          case event: RunStarting => firstString
          case event: RunStopped => firstString
          case event: RunAborted => firstString
          case event: RunCompleted => firstString
          case event: ScopeOpened => firstString + " - " + event.message
          case event: ScopeClosed => firstString
          case event: ScopePending => firstString + " - " + event.message
          case event: InfoProvided => firstString + " - " + event.message
          case event: MarkupProvided => firstString + " - " + event.text
          case event: SuiteStarting => firstAndSecondString(firstString, event.suiteName)
          case event: SuiteCompleted => firstAndSecondString(firstString, event.suiteName)
          case event: SuiteAborted => firstAndSecondString(firstString, event.suiteName)
          case event: TestStarting => firstAndSecondString(firstString, suiteAndTestName(event.suiteName, event.testName))
          case event: TestPending => firstAndSecondString(firstString, suiteAndTestName(event.suiteName, event.testName))
          case event: TestIgnored => firstAndSecondString(firstString, suiteAndTestName(event.suiteName, event.testName))
          case event: TestSucceeded => firstAndSecondString(firstString, suiteAndTestName(event.suiteName, event.testName))
          case event: TestFailed => firstAndSecondString(firstString, suiteAndTestName(event.suiteName, event.testName))
          case event: TestCanceled => firstAndSecondString(firstString, suiteAndTestName(event.suiteName, event.testName))
          case event: AlertProvided => firstString + " - " + event.message
          case event: NoteProvided => firstString + " - " + event.message
        }
    }
  }
}

private[tools] object EventHolder {
  def suiteAndTestName(suiteName: String, testName: String) = suiteName + ": " + testName
}
