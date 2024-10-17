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

import org.scalatest.events.ExceptionalEvent
import scala.collection.mutable.ListBuffer

private[tools] class SummaryCounter {
  // scala.js is thread-safe
  var testsSucceededCount, testsFailedCount, testsIgnoredCount, testsPendingCount, testsCanceledCount, suitesCompletedCount, suitesAbortedCount, scopesPendingCount = 0
  val reminderEventsQueue = new ListBuffer[ExceptionalEvent]

  def incrementTestsSucceededCount(): Unit = {
    testsSucceededCount = testsSucceededCount + 1
  }

  def incrementTestsFailedCount(): Unit = {
    testsFailedCount = testsFailedCount + 1
  }

  def incrementTestsIgnoredCount(): Unit = {
    testsIgnoredCount = testsIgnoredCount + 1
  }

  def incrementTestsPendingCount(): Unit = {
    testsPendingCount = testsPendingCount + 1
  }

  def incrementTestsCanceledCount(): Unit = {
    testsCanceledCount = testsCanceledCount + 1
  }

  def incrementSuitesCompletedCount(): Unit = {
    suitesCompletedCount = suitesCompletedCount + 1
  }

  def incrementSuitesAbortedCount(): Unit = {
    suitesAbortedCount = suitesAbortedCount + 1
  }

  def incrementScopesPendingCount(): Unit = {
    scopesPendingCount = scopesPendingCount + 1
  }

  def recordReminderEvents(events: ExceptionalEvent): Unit = {
    reminderEventsQueue += events
  }
}