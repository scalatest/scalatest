/*
 * Copyright 2001-2013 Artima, Inc.
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

/**
 * An enumeration of the 13 possible confiuration options accepted
 * the Runner for Reporters.
 *
 * @author Bill Venners
 */
private[tools] sealed abstract class EventToPresent

private[tools] case object PresentDiscoveryStarting extends EventToPresent
private[tools] case object PresentDiscoveryCompleted extends EventToPresent
private[tools] case object PresentRunStarting extends EventToPresent
private[tools] case object PresentTestStarting extends EventToPresent
private[tools] case object PresentTestFailed extends EventToPresent
private[tools] case object PresentTestSucceeded extends EventToPresent
private[tools] case object PresentTestIgnored extends EventToPresent
private[tools] case object PresentTestPending extends EventToPresent
private[tools] case object PresentTestCanceled extends EventToPresent
private[tools] case object PresentSuiteStarting extends EventToPresent
private[tools] case object PresentSuiteAborted extends EventToPresent
private[tools] case object PresentSuiteCompleted extends EventToPresent
private[tools] case object PresentInfoProvided extends EventToPresent
private[tools] case object PresentAlertProvided extends EventToPresent
private[tools] case object PresentNoteProvided extends EventToPresent
private[tools] case object PresentScopeOpened extends EventToPresent
private[tools] case object PresentScopeClosed extends EventToPresent
private[tools] case object PresentScopePending extends EventToPresent
private[tools] case object PresentMarkupProvided extends EventToPresent
private[tools] case object PresentRunStopped extends EventToPresent
private[tools] case object PresentRunAborted extends EventToPresent
private[tools] case object PresentRunCompleted extends EventToPresent

private[tools] object EventToPresent {

  val allEventsToPresent: Set[EventToPresent] =
    Set(
      PresentDiscoveryStarting,
      PresentDiscoveryCompleted,
      PresentRunStarting,
      PresentTestStarting,
      PresentTestSucceeded,
      PresentTestFailed,
      PresentTestIgnored,
      PresentTestPending,
      PresentTestCanceled,
      PresentSuiteStarting,
      PresentSuiteCompleted,
      PresentSuiteAborted,
      PresentInfoProvided,
      PresentAlertProvided,
      PresentNoteProvided,
      PresentScopeOpened,
      PresentScopeClosed,
      PresentScopePending, 
      PresentMarkupProvided,
      PresentRunStopped,
      PresentRunCompleted,
      PresentRunAborted
    )

  def eventToEventToPresent(event: org.scalatest.events.Event): EventToPresent =
    event match {
      case _: DiscoveryStarting => PresentDiscoveryStarting
      case _: DiscoveryCompleted => PresentDiscoveryCompleted
      case _: RunStarting => PresentRunStarting
      case _: TestStarting => PresentTestStarting
      case _: TestSucceeded => PresentTestSucceeded
      case _: TestFailed => PresentTestFailed
      case _: TestIgnored => PresentTestIgnored
      case _: TestPending => PresentTestPending
      case _: TestCanceled => PresentTestCanceled
      case _: SuiteStarting => PresentSuiteStarting
      case _: SuiteCompleted => PresentSuiteCompleted
      case _: SuiteAborted => PresentSuiteAborted
      case _: InfoProvided => PresentInfoProvided
      case _: AlertProvided => PresentAlertProvided
      case _: NoteProvided => PresentNoteProvided
      case _: ScopeOpened => PresentScopeOpened
      case _: ScopeClosed => PresentScopeClosed
      case _: ScopePending => PresentScopePending
      case _: MarkupProvided => PresentMarkupProvided // Should never get here, because MarkupProvided events are not registered in the GUI
      case _: RunStopped => PresentRunStopped
      case _: RunCompleted => PresentRunCompleted
      case _: RunAborted => PresentRunAborted
    }
}
