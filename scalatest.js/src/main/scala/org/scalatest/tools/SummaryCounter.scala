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

import org.scalatest.events.ExceptionalEvent
import scala.collection.mutable.ListBuffer

private[tools] class SummaryCounter {
  // scala.js is thread-safe
  var testsSucceededCount, testsFailedCount, testsIgnoredCount, testsPendingCount, testsCanceledCount, suitesCompletedCount, suitesAbortedCount, scopesPendingCount = 0
  val reminderEventsQueue = new ListBuffer[ExceptionalEvent]

  def incrementTestsSucceededCount() {
    testsSucceededCount = testsSucceededCount + 1
  }

  def incrementTestsFailedCount() {
    testsFailedCount = testsFailedCount + 1
  }

  def incrementTestsIgnoredCount() {
    testsIgnoredCount = testsIgnoredCount + 1
  }

  def incrementTestsPendingCount() {
    testsPendingCount = testsPendingCount + 1
  }

  def incrementTestsCanceledCount() {
    testsCanceledCount = testsCanceledCount + 1
  }

  def incrementSuitesCompletedCount() {
    suitesCompletedCount = suitesCompletedCount + 1
  }

  def incrementSuitesAbortedCount() {
    suitesAbortedCount = suitesAbortedCount + 1
  }

  def incrementScopesPendingCount() {
    scopesPendingCount = scopesPendingCount + 1
  }

  def recordReminderEvents(events: ExceptionalEvent) {
    reminderEventsQueue += events
  }
}