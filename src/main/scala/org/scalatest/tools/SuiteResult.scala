package org.scalatest.tools

import org.scalatest.events.{Event, SuiteStarting}

private[scalatest] case class SuiteResult(
  suiteId: String, 
  suiteName: String, 
  suiteClassName: Option[String], 
  duration: Option[Long], 
  startEvent: SuiteStarting, 
  endEvent: Event, 
  eventList: collection.immutable.IndexedSeq[Event], 
  testsSucceededCount: Int, 
  testsFailedCount: Int, 
  testsIgnoredCount: Int, 
  testsPendingCount: Int, 
  testsCanceledCount: Int, 
  scopesPendingCount: Int, 
  isCompleted: Boolean)