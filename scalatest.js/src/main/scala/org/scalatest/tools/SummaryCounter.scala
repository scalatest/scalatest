package org.scalatest.tools

import org.scalatest.events.ExceptionalEvent
import scala.collection.mutable.ListBuffer

private[tools] class SummaryCounter {
  // scala.js is thread-safe
  var testsSucceededCount, testsFailedCount, testsIgnoredCount, testsPendingCount, testsCanceledCount, suitesCompletedCount, suitesAbortedCount, scopesPendingCount = 0L
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