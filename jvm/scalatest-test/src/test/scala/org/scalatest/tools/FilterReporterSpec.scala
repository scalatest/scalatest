/*
 * Copyright 2001-2025 Artima, Inc.
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

import org.scalatest.SharedHelpers.EventRecordingReporter
import org.scalatest.Args
import org.scalatest.Stopper
import org.scalatest.Filter
import org.scalatest.Tracker
import org.scalatest.events.InfoProvided
import org.scalatest.events.MarkupProvided
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.funsuite.AnyFunSuite

class FilterReporterSpec extends AnyFunSpec {

  class ExampleSpec extends AnyFunSuite {
    test("succeeded test") {
      info("succeeded info")
      markup("succeeded markup")
    }
    test("failed test") {
      info("failed info")
      markup("failed markup")
      fail()
    }
    test("pending test") {
      info("pending info")
      markup("pending markup")
      pending
    }
    test("canceled test") {
      info("canceled info")
      markup("canceled markup")
      cancel()
    }
  }
  
  describe("FilterReporter") {
    
    it("should fire all recorded events when both FilterInfoProvided and FilterMarkupProvided are off") {
      val rec = new EventRecordingReporter
      val rep = new FilterReporter(rec, Set.empty)
      
      val spec = new ExampleSpec()
      spec.run(None, Args(rep))
      
      assert(rec.testSucceededEventsReceived.size === 1)
      val testSucceeded = rec.testSucceededEventsReceived(0)
      assert(testSucceeded.recordedEvents.size === 2)
      val testSucceededInfo = testSucceeded.recordedEvents(0).asInstanceOf[InfoProvided]
      assert(testSucceededInfo.message === "succeeded info")
      val testSucceededMarkup = testSucceeded.recordedEvents(1).asInstanceOf[MarkupProvided]
      assert(testSucceededMarkup.text === "succeeded markup")
      
      assert(rec.testFailedEventsReceived.size === 1)
      val testFailed = rec.testFailedEventsReceived(0)
      assert(testFailed.recordedEvents.size === 2)
      val testFailedInfo = testFailed.recordedEvents(0).asInstanceOf[InfoProvided]
      assert(testFailedInfo.message === "failed info")
      val testFailedMarkup = testFailed.recordedEvents(1).asInstanceOf[MarkupProvided]
      assert(testFailedMarkup.text === "failed markup")
      
      assert(rec.testPendingEventsReceived.size === 1)
      val testPending = rec.testPendingEventsReceived(0)
      assert(testPending.recordedEvents.size === 2)
      val testPendingInfo = testPending.recordedEvents(0).asInstanceOf[InfoProvided]
      assert(testPendingInfo.message === "pending info")
      val testPendingMarkup = testPending.recordedEvents(1).asInstanceOf[MarkupProvided]
      assert(testPendingMarkup.text === "pending markup")
      
      assert(rec.testCanceledEventsReceived.size === 1)
      val testCanceled = rec.testCanceledEventsReceived(0)
      assert(testCanceled.recordedEvents.size === 2)
      val testCanceledInfo = testCanceled.recordedEvents(0).asInstanceOf[InfoProvided]
      assert(testCanceledInfo.message === "canceled info")
      val testCanceledMarkup = testCanceled.recordedEvents(1).asInstanceOf[MarkupProvided]
      assert(testCanceledMarkup.text === "canceled markup")
    }
    
    it("should not fire recorded InfoProvided events when FilterInfoProvided is on") {
      val rec = new EventRecordingReporter
      val rep = new FilterReporter(rec, Set(FilterInfoProvided))
      
      val spec = new ExampleSpec()
      spec.run(None, Args(rep))
      
      assert(rec.testSucceededEventsReceived.size === 1)
      val testSucceeded = rec.testSucceededEventsReceived(0)
      assert(testSucceeded.recordedEvents.size === 1)
      val testSucceededMarkup = testSucceeded.recordedEvents(0).asInstanceOf[MarkupProvided]
      assert(testSucceededMarkup.text === "succeeded markup")
      
      assert(rec.testFailedEventsReceived.size === 1)
      val testFailed = rec.testFailedEventsReceived(0)
      assert(testFailed.recordedEvents.size === 1)
      val testFailedMarkup = testFailed.recordedEvents(0).asInstanceOf[MarkupProvided]
      assert(testFailedMarkup.text === "failed markup")
      
      assert(rec.testPendingEventsReceived.size === 1)
      val testPending = rec.testPendingEventsReceived(0)
      assert(testPending.recordedEvents.size === 1)
      val testPendingMarkup = testPending.recordedEvents(0).asInstanceOf[MarkupProvided]
      assert(testPendingMarkup.text === "pending markup")
      
      assert(rec.testCanceledEventsReceived.size === 1)
      val testCanceled = rec.testCanceledEventsReceived(0)
      assert(testCanceled.recordedEvents.size === 1)
      val testCanceledMarkup = testCanceled.recordedEvents(0).asInstanceOf[MarkupProvided]
      assert(testCanceledMarkup.text === "canceled markup")
    }
    
    it("should not fire recorded MarkupProvided events when FilterMarkupProvided is on") {
      val rec = new EventRecordingReporter
      val rep = new FilterReporter(rec, Set(FilterMarkupProvided))
      
      val spec = new ExampleSpec()
      spec.run(None, Args(rep))
      
      assert(rec.testSucceededEventsReceived.size === 1)
      val testSucceeded = rec.testSucceededEventsReceived(0)
      assert(testSucceeded.recordedEvents.size === 1)
      val testSucceededInfo = testSucceeded.recordedEvents(0).asInstanceOf[InfoProvided]
      assert(testSucceededInfo.message === "succeeded info")
      
      assert(rec.testFailedEventsReceived.size === 1)
      val testFailed = rec.testFailedEventsReceived(0)
      assert(testFailed.recordedEvents.size === 1)
      val testFailedInfo = testFailed.recordedEvents(0).asInstanceOf[InfoProvided]
      assert(testFailedInfo.message === "failed info")
      
      assert(rec.testPendingEventsReceived.size === 1)
      val testPending = rec.testPendingEventsReceived(0)
      assert(testPending.recordedEvents.size === 1)
      val testPendingInfo = testPending.recordedEvents(0).asInstanceOf[InfoProvided]
      assert(testPendingInfo.message === "pending info")
      
      assert(rec.testCanceledEventsReceived.size === 1)
      val testCanceled = rec.testCanceledEventsReceived(0)
      assert(testCanceled.recordedEvents.size === 1)
      val testCanceledInfo = testCanceled.recordedEvents(0).asInstanceOf[InfoProvided]
      assert(testCanceledInfo.message === "canceled info")
    }
    
    it("should not fire recorded InfoProvided and MarkupProvided events when both FilterInfoProvided and FilterMarkupProvided are on") {
      val rec = new EventRecordingReporter
      val rep = new FilterReporter(rec, Set(FilterInfoProvided, FilterMarkupProvided))
      
      val spec = new ExampleSpec()
      spec.run(None, Args(rep))
      
      assert(rec.testSucceededEventsReceived.size === 1)
      val testSucceeded = rec.testSucceededEventsReceived(0)
      assert(testSucceeded.recordedEvents.size === 0)
      
      assert(rec.testFailedEventsReceived.size === 1)
      val testFailed = rec.testFailedEventsReceived(0)
      assert(testFailed.recordedEvents.size === 0)
      
      assert(rec.testPendingEventsReceived.size === 1)
      val testPending = rec.testPendingEventsReceived(0)
      assert(testPending.recordedEvents.size === 0)
      
      assert(rec.testCanceledEventsReceived.size === 1)
      val testCanceled = rec.testCanceledEventsReceived(0)
      assert(testCanceled.recordedEvents.size === 0)
    }
  }
  
}
