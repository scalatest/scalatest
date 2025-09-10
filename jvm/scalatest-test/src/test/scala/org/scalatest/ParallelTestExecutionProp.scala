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
package org.scalatest

import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.events.SuiteCompleted
import org.scalatest.events.SuiteStarting
import org.scalatest.time.Millis
import org.scalatest.time.Span
import org.scalatest.tools.SuiteSortingReporter
import scala.collection.mutable.ListBuffer
// SKIP-SCALATESTJS,NATIVE-START
import org.scalatest.concurrent.Eventually._
//DOTTY-ONLY import org.scalatest.concurrent.Eventually.given_PatienceConfig
// SKIP-SCALATESTJS,NATIVE-END
import org.scalatest.Retries._
import SharedHelpers._
import org.scalatest.tagobjects.Retryable
import org.scalatest.tools.DistributedTestRunnerSuite
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.tagobjects.Flicker

class ParallelTestExecutionProp extends AnyFunSuite {

  override def withFixture(test: NoArgTest) = {
    if (isRetryable(test))
      withRetry { super.withFixture(test) }
    else
      super.withFixture(test)
  }

  class ControlledOrderDistributor extends Distributor {
    val buf = ListBuffer.empty[(Suite, Args, ScalaTestStatefulStatus)]
    def apply(suite: Suite, args: Args): Status = {
      val status = new ScalaTestStatefulStatus
      buf += ((suite, args, status))
      status
    }
    def executeInOrder(): Unit = {
      for ((suite, args, status) <- buf) {
        suite.run(None, args)
        if (!status.isCompleted())
          status.setCompleted()
      }
    }
    def executeInReverseOrder(): Unit = {
      for ((suite, args, status) <- buf.reverse) {
        suite.run(None, args)
        if (!status.isCompleted())
          status.setCompleted()
      }
    }

    def apply(suite: Suite, tracker: Tracker): Unit = {
      throw new UnsupportedOperationException("Hey, we're not supposed to be calling this anymore!")
    }
  }
  
  class TestHoldingControlledOrderDistributor extends Distributor {
    val buf = ListBuffer.empty[(DistributedTestRunnerSuite, Args, ScalaTestStatefulStatus)]
    def apply(suite: Suite, args: Args): Status = {
      suite match {
        case dtrs: DistributedTestRunnerSuite => 
          val status = new ScalaTestStatefulStatus
          buf += ((dtrs, args, status))
          status
        case _ => 
          throw new UnsupportedOperationException("TestHoldingControlledOrderDistributor takes only DistributedTestRunnerSuite!")
      }
      
    }
    def executeInOrder(): Unit = {
      for ((suite, args, status) <- buf) {
        suite.run(None, args)
        if (!status.isCompleted())
          status.setCompleted()
      }
    }
    def executeInReverseOrder(): Unit = {
      for ((suite, args, status) <- buf.reverse) {
        suite.run(None, args)
        if (!status.isCompleted())
          status.setCompleted()
      }
    }
    def fireHoldEvent(): Unit = {
      for ((suite, args, status) <- buf) {
        suite.suite match {
          case tter: TestTimeoutExpectedResults => 
            tter.holdingReporter.fireHoldEvent()
          case other => 
            throw new UnsupportedOperationException("Expected TestTimeoutExpectedResults type, but we got: " + other.getClass.getName)
        }
        
      }
    }
    def apply(suite: Suite, tracker: Tracker): Unit = {
      throw new UnsupportedOperationException("Hey, we're not supposed to be calling this anymore!")
    }
  }
  
  def withDistributor(suite: Suite, fun: ControlledOrderDistributor => Unit) = {
    val recordingReporter = new EventRecordingReporter
    val outOfOrderDistributor = new ControlledOrderDistributor
    suite.run(None, Args(recordingReporter, distributor = Some(outOfOrderDistributor)))
    fun(outOfOrderDistributor)

    recordingReporter.eventsReceived
  }

  // SKIP-SCALATESTJS,NATIVE-START
  def withTestHoldingDistributor(suite: Suite with TestTimeoutExpectedResults, fun: TestHoldingControlledOrderDistributor => Unit) = {
    val recordingReporter = new EventRecordingReporter
    val distributor = new TestHoldingControlledOrderDistributor
    suite.run(None, Args(recordingReporter, distributor = Some(distributor)))
    fun(distributor)
    eventually(timeout(suite.sortingTimeout.scaledBy(3.0))) { 
      assert(recordingReporter.eventsReceived.size === suite.holdUntilEventCount) 
    }
    distributor.fireHoldEvent()
    recordingReporter.eventsReceived
  }

  def withSuiteHoldingDistributor(suites: SuiteTimeoutSuites, fun: ControlledOrderDistributor => Unit) = {
    val suite1 = suites.suite1
    val suite2 = suites.suite2
    val recordingReporter = new EventRecordingReporter
    val suiteSortingReporter = new SuiteSortingReporter(recordingReporter, suite1.sortingTimeout, System.err)
    val holdingReporter = new SuiteHoldingReporter(suiteSortingReporter, suite1.suiteId, suites.holdingTestName, suites.holdingScopeClosedName)
    val distributor = new ControlledOrderDistributor
    val tracker = new Tracker()
    holdingReporter(SuiteStarting(tracker.nextOrdinal(), suite1.suiteName, suite1.suiteId, Some(suite1.getClass.getName), None))
    holdingReporter(SuiteStarting(tracker.nextOrdinal(), suite2.suiteName, suite2.suiteId, Some(suite2.getClass.getName), None))
    suite1.run(None, Args(holdingReporter, distributor = Some(distributor), distributedSuiteSorter = Some(suiteSortingReporter)))
    suite2.run(None, Args(holdingReporter, distributor = Some(distributor), distributedSuiteSorter = Some(suiteSortingReporter)))
    holdingReporter(SuiteCompleted(tracker.nextOrdinal(), suite2.suiteName, suite2.suiteId, Some(suite2.getClass.getName), None))
    fun(distributor)
    eventually(timeout(suite1.sortingTimeout.scaledBy(3.0))) {
      assert(recordingReporter.eventsReceived.size === suites.holdUntilEventCount)
    }
    holdingReporter.fireHoldEvents()
    holdingReporter(SuiteCompleted(tracker.nextOrdinal(), suite1.suiteName, suite1.suiteId, Some(suite1.getClass.getName), None))
    recordingReporter.eventsReceived
  }
  // SKIP-SCALATESTJS,NATIVE-END
  
  def withSuiteDistributor(suite1: Suite, suite2: Suite, fun: ControlledOrderDistributor => Unit) = {
    val recordingReporter = new EventRecordingReporter
    val suiteSortingReporter = new SuiteSortingReporter(recordingReporter, Span((Suite.defaultTestSortingReporterTimeoutInSeconds * 1000) + 1000, Millis), System.err)
    val distributor = new ControlledOrderDistributor
    val tracker = new Tracker()
    suiteSortingReporter(SuiteStarting(tracker.nextOrdinal(), suite1.suiteName, suite1.suiteId, Some(suite1.getClass.getName), None))
    suiteSortingReporter(SuiteStarting(tracker.nextOrdinal(), suite2.suiteName, suite2.suiteId, Some(suite2.getClass.getName), None))
    suite1.run(None, Args(suiteSortingReporter, distributor = Some(distributor), distributedSuiteSorter = Some(suiteSortingReporter)))
    suite2.run(None, Args(suiteSortingReporter, distributor = Some(distributor), distributedSuiteSorter = Some(suiteSortingReporter)))
    suiteSortingReporter(SuiteCompleted(tracker.nextOrdinal(), suite2.suiteName, suite2.suiteId, Some(suite2.getClass.getName), None))
    suiteSortingReporter(SuiteCompleted(tracker.nextOrdinal(), suite1.suiteName, suite1.suiteId, Some(suite1.getClass.getName), None))
    fun(distributor)
    recordingReporter.eventsReceived
  }

  test("ParallelTestExecution should have the events reported in correct order when tests are executed in parallel") {
    import ParallelTestExecutionOrderExamples._
    forAll(orderExamples) { example =>
      val inOrderEvents = withDistributor(example, _.executeInOrder())
      example.assertOrderTest(inOrderEvents)
      val reverseOrderEvents = withDistributor(example, _.executeInReverseOrder())
      example.assertOrderTest(reverseOrderEvents)
    }
  }
  
  test("ParallelTestExecution should have InfoProvided fired from before and after block in correct order when tests are executed in parallel") {
    import ParallelTestExecutionInfoExamples._
    forAll(infoExamples) { example =>
      val inOrderEvents = withDistributor(example, _.executeInOrder())
      example.assertBeforeAfterInfo(inOrderEvents)
      val reverseOrderEvents = withDistributor(example, _.executeInReverseOrder())
      example.assertBeforeAfterInfo(reverseOrderEvents)
    }
  }

  // SKIP-SCALATESTJS,NATIVE-START
  test("ParallelTestExecution should have the blocking test's events fired without waiting when timeout reaches, and when the missing event finally reach later, it should just get fired", Retryable) {
    import ParallelTestExecutionTestTimeoutExamples._
    forAll(testTimeoutExamples) { example =>
      val inOrderEvents = withTestHoldingDistributor(example, _.executeInOrder())
      example.assertTestTimeoutTest(inOrderEvents)
      val reverseOrderEvents = withTestHoldingDistributor(example, _.executeInReverseOrder())
      example.assertTestTimeoutTest(reverseOrderEvents)
    }
  }
  // SKIP-SCALATESTJS,NATIVE-END
  
  test("ParallelTestExecution should have the events reported in correct order when multiple suite's tests are executed in parallel") {
    import ParallelTestExecutionParallelSuiteExamples._
    forAll(parallelExamples) { example =>
      val inOrderEvents = withSuiteDistributor(example.suite1, example.suite2, _.executeInOrder())
      example.assertParallelSuites(inOrderEvents)
      //val reverseOrderEvents = withSuiteDistributor(example.suite1, example.suite2, _.executeInReverseOrder)
      //example.assertParallelSuites(reverseOrderEvents)
    }
  }

  // SKIP-SCALATESTJS,NATIVE-START
  test("ParallelTestExecution should have the blocking suite's events fired without waiting when timeout reaches, and when the missing event finally reach later, it should just get fired", Flicker) {
    import ParallelTestExecutionSuiteTimeoutExamples._
    forAll(suiteTimeoutExamples) { example =>
      val events = withSuiteHoldingDistributor(example, _.executeInOrder())
      example.assertSuiteTimeoutTest(events)
    }
  }
  // SKIP-SCALATESTJS,NATIVE-END
}
