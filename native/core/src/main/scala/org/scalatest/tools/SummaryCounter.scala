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
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.LinkedBlockingQueue

private[tools] class SummaryCounter {
  val testsSucceededCount, testsFailedCount, testsIgnoredCount, testsPendingCount, testsCanceledCount, suitesCompletedCount, suitesAbortedCount, scopesPendingCount = new AtomicInteger
  val reminderEventsQueue = new LinkedBlockingQueue[ExceptionalEvent]

  def incrementTestsSucceededCount(): Unit = {
    testsSucceededCount.incrementAndGet()
  }

  def incrementTestsFailedCount(): Unit = {
    testsFailedCount.incrementAndGet()
  }

  def incrementTestsIgnoredCount(): Unit = {
    testsIgnoredCount.incrementAndGet()
  }

  def incrementTestsPendingCount(): Unit = {
    testsPendingCount.incrementAndGet()
  }

  def incrementTestsCanceledCount(): Unit = {
    testsCanceledCount.incrementAndGet()
  }

  def incrementSuitesCompletedCount(): Unit = {
    suitesCompletedCount.incrementAndGet()
  }

  def incrementSuitesAbortedCount(): Unit = {
    suitesAbortedCount.incrementAndGet()
  }

  def incrementScopesPendingCount(): Unit = {
    scopesPendingCount.incrementAndGet()
  }

  def recordReminderEvents(events: ExceptionalEvent): Unit = {
    reminderEventsQueue.put(events)
  }
}