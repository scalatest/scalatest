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
import DispatchReporter.propagateDispose

/**
 * FiterReporter catches exceptions that may be thrown by custom reporters, and doesn't forward
 * reports that were not selected by the passed configuration.
 *
 * @author Bill Venners
 */
private[tools] class FilterReporter(reporter: Reporter, configSet: Set[ReporterConfigParam]) extends ResourcefulReporter {

  // def reFilter(configSet: EventToPresent.Set32) = new FilterReporter(reporter, configSet)

  override def apply(event: Event) {
    val report = reporter
    event match {
      case event: DiscoveryStarting => report(event)
      case event: DiscoveryCompleted => report(event)
      case event: RunStarting => report(event)
      case event: RunCompleted => report(event)
      case event: RunAborted => report(event)
      case event: RunStopped => report(event)
      case event: SuiteAborted => report(event)
      case event: TestFailed => report(event.copy(recordedEvents = event.recordedEvents.filter(filterInfoMarkupProvided(_))))
      case event: SuiteCompleted => if (!configSet.contains(FilterSuiteCompleted)) report(event)
      case event: SuiteStarting => if (!configSet.contains(FilterSuiteStarting)) report(event)
      case event: TestStarting => if (!configSet.contains(FilterTestStarting)) report(event)
      case event: TestSucceeded => 
        if (!configSet.contains(FilterTestSucceeded)) 
          report(event.copy(recordedEvents = event.recordedEvents.filter(filterInfoMarkupProvided(_))))
      case event: TestIgnored => if (!configSet.contains(FilterTestIgnored)) report(event)
      case event: TestCanceled => 
        if (!configSet.contains(FilterTestCanceled)) 
          report(event.copy(recordedEvents = event.recordedEvents.filter(filterInfoMarkupProvided(_))))
      case event: TestPending => 
        if (!configSet.contains(FilterTestPending)) 
          report(event.copy(recordedEvents = event.recordedEvents.filter(filterInfoMarkupProvided(_))))
      case event: InfoProvided => if (!configSet.contains(FilterInfoProvided)) report(event)
      case event: AlertProvided => if (!configSet.contains(FilterAlertProvided)) report(event)
      case event: NoteProvided => if (!configSet.contains(FilterNoteProvided)) report(event)
      case event: ScopeOpened => if (!configSet.contains(FilterScopeOpened)) report(event)
      case event: ScopeClosed => if (!configSet.contains(FilterScopeClosed)) report(event)
      case event: ScopePending => if (!configSet.contains(FilterScopePending)) report(event)
      case event: MarkupProvided => if (!configSet.contains(FilterMarkupProvided)) report(event)
    }
  }
  
  private def filterInfoMarkupProvided(event: Event): Boolean = {
    event match {
      case infoProvided: InfoProvided => !configSet.contains(FilterInfoProvided)
      case markupProvided: MarkupProvided => !configSet.contains(FilterMarkupProvided)
      case _ => true
    }
  }

  override def dispose() = propagateDispose(reporter)
}
// Have some methods that translate chars & strings to Opts things, and vice versa?
