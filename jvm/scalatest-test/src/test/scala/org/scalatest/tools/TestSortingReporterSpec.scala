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
import org.scalatest.SharedHelpers.SilentReporter
import org.scalatest.events._
import org.scalatest.time.Span
import org.scalatest.time.Seconds
import org.scalatest.Tracker
import scala.Some
import org.scalatest.events.ScopeClosed
import org.scalatest.events.TestStarting
import org.scalatest.events.ScopeOpened
import org.scalatest.events.TestSucceeded
import java.io.PrintStream
import java.io.ByteArrayOutputStream
import org.scalatest.DistributedSuiteSorter
import scala.collection.mutable.ListBuffer
import org.scalactic.exceptions.NullArgumentException
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.tagobjects.Flicker

class TestSortingReporterSpec extends AnyFunSpec with Matchers {

  describe("TestSortingReporter") {
    
    val tracker = new Tracker()
    val scope1Opened = ScopeOpened(tracker.nextOrdinal(), "Scope 1", NameInfo("aSuite", "aSuite", Some("a.b.aSuite"), None))
    val scope2Opened = ScopeOpened(tracker.nextOrdinal(), "Scope 2", NameInfo("aSuite", "aSuite", Some("a.b.aSuite"), None))
    val s1s2t1Starting = TestStarting(tracker.nextOrdinal(), "aSuite", "aSuite", Some("a.b.aSuite"), "Scope 1 Scope 2 Test 1", "Test 1")
    val s1s2t1Succeeded = TestSucceeded(tracker.nextOrdinal(), "aSuite", "aSuite", Some("a.b.aSuite"), "Scope 1 Scope 2 Test 1", "Test 1", Vector.empty)
    val s1s2t2Starting = TestStarting(tracker.nextOrdinal(), "aSuite", "aSuite", Some("a.b.aSuite"), "Scope 1 Scope 2 Test 2", "Test 2")
    val s1s2t2Succeeded = TestSucceeded(tracker.nextOrdinal(), "aSuite", "aSuite", Some("a.b.aSuite"), "Scope 1 Scope 2 Test 2", "Test 2", Vector.empty)
    val s1s2t3Starting = TestStarting(tracker.nextOrdinal(), "aSuite", "aSuite", Some("a.b.aSuite"), "Scope 1 Scope 2 Test 3", "Test 3")
    val s1s2t3Succeeded = TestSucceeded(tracker.nextOrdinal(), "aSuite", "aSuite", Some("a.b.aSuite"), "Scope 1 Scope 2 Test 3", "Test 3", Vector.empty)
    val scope2Closed = ScopeClosed(tracker.nextOrdinal(), "Scope 2", NameInfo("aSuite", "aSuite", Some("a.b.aSuite"), None))
    val scope1Closed = ScopeClosed(tracker.nextOrdinal(), "Scope 1", NameInfo("aSuite", "aSuite", Some("a.b.aSuite"), None))
    
    val scope3Opened = ScopeOpened(tracker.nextOrdinal(), "Scope 3", NameInfo("aSuite", "aSuite", Some("a.b.aSuite"), None))
    val s3t1Starting = TestStarting(tracker.nextOrdinal(), "aSuite", "aSuite", Some("a.b.aSuite"), "Scope 3 Test 1", "Test 1")
    val s3t1Succeeded = TestSucceeded(tracker.nextOrdinal(), "aSuite", "aSuite", Some("a.b.aSuite"), "Scope 3 Test 1", "Test 1", Vector.empty)
    val s3t2Starting = TestStarting(tracker.nextOrdinal(), "aSuite", "aSuite", Some("a.b.aSuite"), "Scope 3 Test 2", "Test 2")
    val s3t2Succeeded = TestSucceeded(tracker.nextOrdinal(), "aSuite", "aSuite", Some("a.b.aSuite"), "Scope 3 Test 2", "Test 2", Vector.empty)
    val s3t3Starting = TestStarting(tracker.nextOrdinal(), "aSuite", "aSuite", Some("a.b.aSuite"), "Scope 3 Test 3", "Test 3")
    val s3t3Succeeded = TestSucceeded(tracker.nextOrdinal(), "aSuite", "aSuite", Some("a.b.aSuite"), "Scope 3 Test 3", "Test 3", Vector.empty)
    val scope3Closed = ScopeClosed(tracker.nextOrdinal(), "Scope 3", NameInfo("aSuite", "aSuite", Some("a.b.aSuite"), None))
    
    it("should fire event passed to it in the order they arrive if distributingTest, apply and completedTest is not called.") {
      val recordingReporter = new EventRecordingReporter()
      val dispatch = new TestSortingReporter("aSuite", recordingReporter, Span(15, Seconds), 7, None, new PrintStream(new ByteArrayOutputStream))
      
      dispatch(scope1Opened)
      dispatch(scope2Opened)
      dispatch(s1s2t1Starting)
      dispatch(s1s2t1Succeeded)
      dispatch(s1s2t2Starting)
      dispatch(s1s2t2Succeeded)
      dispatch(s1s2t3Starting)
      dispatch(s1s2t3Succeeded)
      dispatch(scope2Closed)
      dispatch(scope1Closed)
      
      dispatch(scope3Opened)
      dispatch(s3t1Starting)
      dispatch(s3t1Succeeded)
      dispatch(s3t2Starting)
      dispatch(s3t2Succeeded)
      dispatch(s3t3Starting)
      dispatch(s3t3Succeeded)
      dispatch(scope3Closed)
      
      val recordedEvents = recordingReporter.eventsReceived
      recordedEvents(0) should be (scope1Opened)
      recordedEvents(1) should be (scope2Opened)
      recordedEvents(2) should be (s1s2t1Starting)
      recordedEvents(3) should be (s1s2t1Succeeded)
      recordedEvents(4) should be (s1s2t2Starting)
      recordedEvents(5) should be (s1s2t2Succeeded)
      recordedEvents(6) should be (s1s2t3Starting)
      recordedEvents(7) should be (s1s2t3Succeeded)
      recordedEvents(8) should be (scope2Closed)
      recordedEvents(9) should be (scope1Closed)
      recordedEvents(10) should be (scope3Opened)
      recordedEvents(11) should be (s3t1Starting)
      recordedEvents(12) should be (s3t1Succeeded)
      recordedEvents(13) should be (s3t2Starting)
      recordedEvents(14) should be (s3t2Succeeded)
      recordedEvents(15) should be (s3t3Starting)
      recordedEvents(16) should be (s3t3Succeeded)
    }
    
    it("should wait and fire event based on the order of distributingTest, apply and completedTest is called.") {
      val recordingReporter = new EventRecordingReporter()
      val dispatch = new TestSortingReporter("aSuite", recordingReporter, Span(15, Seconds), 7, None, new PrintStream(new ByteArrayOutputStream))
      
      dispatch(scope1Opened)
      dispatch(scope2Opened)
      dispatch.distributingTest(s1s2t1Starting.testName)
      dispatch("Scope 1 Scope 2 Test 1", s1s2t1Starting)
      dispatch.distributingTest(s1s2t2Starting.testName)
      dispatch("Scope 1 Scope 2 Test 2", s1s2t2Starting)
      dispatch.distributingTest(s1s2t3Starting.testName)
      dispatch("Scope 1 Scope 2 Test 3", s1s2t3Starting)
      dispatch("Scope 1 Scope 2 Test 3", s1s2t3Succeeded)
      dispatch.completedTest("Scope 1 Scope 2 Test 3")
      dispatch("Scope 1 Scope 2 Test 1", s1s2t1Succeeded)
      dispatch.completedTest("Scope 1 Scope 2 Test 1")
      dispatch("Scope 1 Scope 2 Test 2", s1s2t2Succeeded)
      dispatch.completedTest("Scope 1 Scope 2 Test 2")
      dispatch(scope2Closed)
      dispatch(scope1Closed)
      
      dispatch(scope3Opened)
      dispatch.distributingTest(s3t1Starting.testName)
      dispatch("Scope 3 Test 1", s3t1Starting)
      dispatch.distributingTest(s3t2Starting.testName)
      dispatch("Scope 3 Test 2", s3t2Starting)
      dispatch.distributingTest(s3t3Starting.testName)
      dispatch("Scope 3 Test 3", s3t3Starting)
      dispatch("Scope 3 Test 3", s3t3Succeeded)
      dispatch.completedTest("Scope 3 Test 3")
      dispatch("Scope 3 Test 1", s3t1Succeeded)
      dispatch.completedTest("Scope 3 Test 1")
      dispatch("Scope 3 Test 2", s3t2Succeeded)
      dispatch.completedTest("Scope 3 Test 2")
      dispatch(scope3Closed)
      
      val recordedEvents = recordingReporter.eventsReceived
      recordedEvents(0) should be (scope1Opened)
      recordedEvents(1) should be (scope2Opened)
      recordedEvents(2) should be (s1s2t1Starting)
      recordedEvents(3) should be (s1s2t1Succeeded)
      recordedEvents(4) should be (s1s2t2Starting)
      recordedEvents(5) should be (s1s2t2Succeeded)
      recordedEvents(6) should be (s1s2t3Starting)
      recordedEvents(7) should be (s1s2t3Succeeded)
      recordedEvents(8) should be (scope2Closed)
      recordedEvents(9) should be (scope1Closed)
      recordedEvents(10) should be (scope3Opened)
      recordedEvents(11) should be (s3t1Starting)
      recordedEvents(12) should be (s3t1Succeeded)
      recordedEvents(13) should be (s3t2Starting)
      recordedEvents(14) should be (s3t2Succeeded)
      recordedEvents(15) should be (s3t3Starting)
      recordedEvents(16) should be (s3t3Succeeded)
    }

    // SKIP-SCALATESTJS,NATIVE-START
    it("should wait and fire blocking event when timeout, and just fire the missing event directly without waiting when received later.", Flicker) {
    
      val recordingReporter = new EventRecordingReporter()
      val dispatch = new TestSortingReporter("aSuite", recordingReporter, Span(3, Seconds), 7, None, new PrintStream(new ByteArrayOutputStream))
      
      dispatch(scope1Opened)
      dispatch(scope2Opened)
      dispatch.distributingTest(s1s2t1Starting.testName)
      dispatch("Scope 1 Scope 2 Test 1", s1s2t1Starting)
      dispatch.distributingTest(s1s2t2Starting.testName)
      dispatch("Scope 1 Scope 2 Test 2", s1s2t2Starting)
      dispatch.distributingTest(s1s2t3Starting.testName)
      dispatch("Scope 1 Scope 2 Test 3", s1s2t3Starting)
      dispatch("Scope 1 Scope 2 Test 3", s1s2t3Succeeded)
      dispatch.completedTest("Scope 1 Scope 2 Test 3")
      dispatch("Scope 1 Scope 2 Test 2", s1s2t2Succeeded)
      dispatch.completedTest("Scope 1 Scope 2 Test 2")
      
      Thread.sleep(4000)
      dispatch("Scope 1 Scope 2 Test 1", s1s2t1Succeeded)
      dispatch.completedTest("Scope 1 Scope 2 Test 1")
      
      dispatch(scope2Closed)
      dispatch(scope1Closed)
      
      
      
      val recordedEvents = recordingReporter.eventsReceived
      recordedEvents(0) should be (scope1Opened)
      recordedEvents(1) should be (scope2Opened)
      recordedEvents(2) should be (s1s2t1Starting)
      recordedEvents(3) should be (s1s2t2Starting)
      recordedEvents(4) should be (s1s2t2Succeeded)
      recordedEvents(5) should be (s1s2t3Starting)
      recordedEvents(6) should be (s1s2t3Succeeded)
      recordedEvents(7) should be (s1s2t1Succeeded)
      recordedEvents(8) should be (scope2Closed)
      recordedEvents(9) should be (scope1Closed)
    }
    // SKIP-SCALATESTJS,NATIVE-END

    it("should throw an IAE from completedTest if no tests have been passed to distributingTest") {
      val recordingReporter = new EventRecordingReporter()
      val tsr = new TestSortingReporter("aSuite", recordingReporter, Span(3, Seconds), 7, None, new PrintStream(new ByteArrayOutputStream))
      an [IllegalArgumentException] should be thrownBy { tsr.completedTest("fred") }
    }

    it("should throw an IAE from completedTest if that test is not among those passed to distributingTest") {
      val recordingReporter = new EventRecordingReporter()
      val dispatch = new TestSortingReporter("aSuite", recordingReporter, Span(3, Seconds), 7, None, new PrintStream(new ByteArrayOutputStream))
      dispatch(scope1Opened)
      dispatch(scope2Opened)
      dispatch.distributingTest(s1s2t1Starting.testName)
      dispatch("Scope 1 Scope 2 Test 1", s1s2t1Starting)
      dispatch.distributingTest(s1s2t2Starting.testName)
      an [IllegalArgumentException] should be thrownBy { dispatch.completedTest("fred") }
    }

    it("should throw an IAE from completedTest if that test does not exist in the waiting list") {
      val recordingReporter = new EventRecordingReporter()
      val dispatch = new TestSortingReporter("aSuite", recordingReporter, Span(3, Seconds), 7, None, new PrintStream(new ByteArrayOutputStream))
      dispatch(scope1Opened)
      dispatch(scope2Opened)
      dispatch.distributingTest(s1s2t1Starting.testName)
      dispatch("Scope 1 Scope 2 Test 1", s1s2t1Starting)
      dispatch.distributingTest(s1s2t2Starting.testName)
      dispatch("Scope 1 Scope 2 Test 2", s1s2t2Starting)
      dispatch.distributingTest(s1s2t3Starting.testName)
      dispatch("Scope 1 Scope 2 Test 3", s1s2t3Starting)
      dispatch("Scope 1 Scope 2 Test 3", s1s2t3Succeeded)
      dispatch.completedTest("Scope 1 Scope 2 Test 3")
      an [IllegalArgumentException] should be thrownBy { dispatch.completedTest("Scope 1 Scope 2 Test 3") }
    }

    it("should throw an NPE from completedTest if null is passed") {
      val recordingReporter = new EventRecordingReporter()
      val tsr = new TestSortingReporter("aSuite", recordingReporter, Span(3, Seconds), 7, None, new PrintStream(new ByteArrayOutputStream))
      an [NullArgumentException] should be thrownBy { tsr.completedTest(null) }
    }

    it("should throw an NPE from distributingTest if null is passed") {
      val recordingReporter = new EventRecordingReporter()
      val tsr = new TestSortingReporter("aSuite", recordingReporter, Span(3, Seconds), 7, None, new PrintStream(new ByteArrayOutputStream))
      a [NullArgumentException] should be thrownBy { tsr.distributingTest(null) }
    }

    it("should throw an IAE from distributingTest if that test was already passed to distributingTest and it hasn't completed") {
      val recordingReporter = new EventRecordingReporter()
      val dispatch = new TestSortingReporter("aSuite", recordingReporter, Span(3, Seconds), 7, None, new PrintStream(new ByteArrayOutputStream))
      dispatch(scope1Opened)
      dispatch(scope2Opened)
      dispatch.distributingTest(s1s2t1Starting.testName)
      dispatch("Scope 1 Scope 2 Test 1", s1s2t1Starting)
      dispatch.distributingTest(s1s2t2Starting.testName)
      an [IllegalArgumentException] should be thrownBy { dispatch.distributingTest(s1s2t2Starting.testName) }
    }

    it("should throw an IAE from distributingTest if that test was already passed to distributingTest and its events haven't yet been completely reported about") {
      val recordingReporter = new EventRecordingReporter()
      val dispatch = new TestSortingReporter("aSuite", recordingReporter, Span(3, Seconds), 7, None, new PrintStream(new ByteArrayOutputStream))
      dispatch(scope1Opened)
      dispatch(scope2Opened)
      dispatch.distributingTest(s1s2t1Starting.testName)
      dispatch("Scope 1 Scope 2 Test 1", s1s2t1Starting)
      dispatch.distributingTest(s1s2t2Starting.testName)
      dispatch.completedTest(s1s2t2Starting.testName)
      an [IllegalArgumentException] should be thrownBy { dispatch.distributingTest(s1s2t2Starting.testName) }
    }

    it("should throw an NPE from apply(String, Event) if null is passed for either param") {
      val recordingReporter = new EventRecordingReporter()
      val tsr = new TestSortingReporter("aSuite", recordingReporter, Span(3, Seconds), 7, None, new PrintStream(new ByteArrayOutputStream))
      a [NullArgumentException] should be thrownBy { tsr.apply(null, scope1Opened) }
      a [NullArgumentException] should be thrownBy { tsr.apply("howdy", null) }
      a [NullArgumentException] should be thrownBy { tsr.apply(null, null) }
    }

    // SKIP-SCALATESTJS,NATIVE-START
    it("should timeout if a test with no event fired is blocking", Flicker) {
      val recordingReporter = new EventRecordingReporter()
      val dispatch = new TestSortingReporter("aSuite", recordingReporter, Span(3, Seconds), 7, None, new PrintStream(new ByteArrayOutputStream))
      
      dispatch(scope1Opened)
      dispatch(scope2Opened)
      dispatch.distributingTest(s1s2t1Starting.testName)
      dispatch.distributingTest(s1s2t2Starting.testName)
      dispatch.distributingTest(s1s2t3Starting.testName)
      Thread.sleep(4000)
      dispatch("Scope 1 Scope 2 Test 2", s1s2t2Starting)
      dispatch("Scope 1 Scope 2 Test 3", s1s2t3Starting)
      dispatch("Scope 1 Scope 2 Test 3", s1s2t3Succeeded)
      dispatch.completedTest("Scope 1 Scope 2 Test 3")
      dispatch("Scope 1 Scope 2 Test 2", s1s2t2Succeeded)
      dispatch.completedTest("Scope 1 Scope 2 Test 2")
      
      dispatch("Scope 1 Scope 2 Test 1", s1s2t1Starting)
      dispatch("Scope 1 Scope 2 Test 1", s1s2t1Succeeded)
      dispatch.completedTest("Scope 1 Scope 2 Test 1")
      
      dispatch(scope2Closed)
      dispatch(scope1Closed)
      
      val recordedEvents = recordingReporter.eventsReceived
      recordedEvents(0) should be (scope1Opened)
      recordedEvents(1) should be (scope2Opened)
      recordedEvents(2) should be (s1s2t2Starting)
      recordedEvents(3) should be (s1s2t2Succeeded)
      recordedEvents(4) should be (s1s2t3Starting)
      recordedEvents(5) should be (s1s2t3Succeeded)
      recordedEvents(6) should be (s1s2t1Starting)
      recordedEvents(7) should be (s1s2t1Succeeded)
      recordedEvents(8) should be (scope2Closed)
      recordedEvents(9) should be (scope1Closed)
    }
    // SKIP-SCALATESTJS,NATIVE-END
    
    class RecordingDistributedSuiteSorter extends DistributedSuiteSorter {
      val distributingTestsList = new ListBuffer[String]()
      val completedTestsList = new ListBuffer[String]()
      
      def distributingTests(suiteId: String): Unit = {
        distributingTestsList += suiteId
      }
      def completedTests(suiteId: String): Unit = {
        completedTestsList += suiteId
      }
    }
    
    it("should call passed in DistributedSuiteSorter's completedTests when suite contains test.") {
      val suiteSorter = new RecordingDistributedSuiteSorter
      val dispatch = new TestSortingReporter("aSuite", SilentReporter, Span(3, Seconds), 1, Some(suiteSorter), new PrintStream(new ByteArrayOutputStream))
      
      dispatch(scope1Opened)
      dispatch(scope2Opened)
      dispatch.distributingTest(s1s2t1Starting.testName)
      dispatch("Scope 1 Scope 2 Test 1", s1s2t1Starting)
      dispatch("Scope 1 Scope 2 Test 1", s1s2t1Succeeded)
      dispatch.completedTest("Scope 1 Scope 2 Test 1")
      dispatch(scope2Closed)
      dispatch(scope1Closed)
      
      suiteSorter.distributingTestsList.toList should be (List("aSuite"))
      suiteSorter.completedTestsList.toList should be (List("aSuite"))
    }
    
    it("should call passed in DistributedSuiteSorter's completedTests when suite does not contain test.") {
      val suiteSorter = new RecordingDistributedSuiteSorter
      val dispatch = new TestSortingReporter("aSuite", SilentReporter, Span(3, Seconds), 0, Some(suiteSorter), new PrintStream(new ByteArrayOutputStream))
      
      dispatch(scope1Opened)
      dispatch(scope2Opened)
      dispatch.distributingTest(s1s2t1Starting.testName)
      dispatch.completedTest("Scope 1 Scope 2 Test 1")
      dispatch(scope2Closed)
      dispatch(scope1Closed)
      
      suiteSorter.distributingTestsList.toList should be (List("aSuite"))
      suiteSorter.completedTestsList.toList should be (List("aSuite"))
    }
  }
}
