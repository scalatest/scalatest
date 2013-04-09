package org.scalatest

import org.scalatest.prop.Tables
import scala.collection.mutable.ListBuffer
import org.scalatest.events.Event
import org.scalatest.prop.TableDrivenPropertyChecks
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.Future
import org.scalatest.tools.SuiteRunner
import java.util.concurrent.Executors
import java.util.concurrent.ExecutorService
import org.scalatest.tools.SuiteSortingReporter
import org.scalatest.time.Span
import org.scalatest.time.Seconds
import org.scalatest.events.SuiteStarting
import org.scalatest.events.SuiteCompleted
import org.scalatest.time.Millis
import java.io.PrintStream
import java.io.ByteArrayOutputStream
import org.scalatest.events.TestSucceeded
import org.scalatest.tools.TestSortingReporter
import org.scalatest.concurrent.Eventually
import org.scalatest.tools.DistributedTestRunnerSuite
import org.scalatest.tools.Runner

class ParallelTestExecutionProp extends FunSuite 
  with TableDrivenPropertyChecks with SharedHelpers with Eventually
  with ParallelTestExecutionOrderExamples 
  with ParallelTestExecutionInfoExamples 
  with ParallelTestExecutionTestTimeoutExamples
  with ParallelTestExecutionParallelSuiteExamples 
  with ParallelTestExecutionSuiteTimeoutExamples {
  
  class ControlledOrderDistributor extends Distributor {
    val buf = ListBuffer.empty[(Suite, Args, ScalaTestStatefulStatus)]
    def apply(suite: Suite, args: Args): Status = {
      val status = new ScalaTestStatefulStatus
      buf += ((suite, args, status))
      status
    }
    def executeInOrder() {
      for ((suite, args, status) <- buf) {
        suite.run(None, args)
        if (!status.isCompleted)
          status.setCompleted()
      }
    }
    def executeInReverseOrder() {
      for ((suite, args, status) <- buf.reverse) {
        suite.run(None, args)
        if (!status.isCompleted)
          status.setCompleted()
      }
    }

    def apply(suite: Suite, tracker: Tracker) {
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
    def executeInOrder() {
      for ((suite, args, status) <- buf) {
        suite.run(None, args)
        if (!status.isCompleted)
          status.setCompleted()
      }
    }
    def executeInReverseOrder() {
      for ((suite, args, status) <- buf.reverse) {
        suite.run(None, args)
        if (!status.isCompleted)
          status.setCompleted()
      }
    }
    def fireHoldEvent() {
      for ((suite, args, status) <- buf) {
        suite.suite match {
          case tter: TestTimeoutExpectedResults => 
            tter.holdingReporter.fireHoldEvent()
          case other => 
            throw new UnsupportedOperationException("Expected TestTimeoutExpectedResults type, but we got: " + other.getClass.getName)
        }
        
      }
    }
    def apply(suite: Suite, tracker: Tracker) {
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
    holdingReporter(SuiteStarting(tracker.nextOrdinal, suite1.suiteName, suite1.suiteId, Some(suite1.getClass.getName), None))
    holdingReporter(SuiteStarting(tracker.nextOrdinal, suite2.suiteName, suite2.suiteId, Some(suite2.getClass.getName), None))
    suite1.run(None, Args(holdingReporter, distributor = Some(distributor), distributedSuiteSorter = Some(suiteSortingReporter)))
    suite2.run(None, Args(holdingReporter, distributor = Some(distributor), distributedSuiteSorter = Some(suiteSortingReporter)))
    holdingReporter(SuiteCompleted(tracker.nextOrdinal, suite2.suiteName, suite2.suiteId, Some(suite2.getClass.getName), None))
    fun(distributor)
    eventually(timeout(suite1.sortingTimeout.scaledBy(3.0))) { 
      assert(recordingReporter.eventsReceived.size === suites.holdUntilEventCount) 
    }
    holdingReporter.fireHoldEvents()
    holdingReporter(SuiteCompleted(tracker.nextOrdinal, suite1.suiteName, suite1.suiteId, Some(suite1.getClass.getName), None))
    recordingReporter.eventsReceived
  }
  
  def withSuiteDistributor(suite1: Suite, suite2: Suite, fun: ControlledOrderDistributor => Unit) = {
    val recordingReporter = new EventRecordingReporter
    val suiteSortingReporter = new SuiteSortingReporter(recordingReporter, Span(Runner.testSortingReporterTimeout.millisPart + 1000, Millis), System.err)
    val distributor = new ControlledOrderDistributor
    val tracker = new Tracker()
    suiteSortingReporter(SuiteStarting(tracker.nextOrdinal, suite1.suiteName, suite1.suiteId, Some(suite1.getClass.getName), None))
    suiteSortingReporter(SuiteStarting(tracker.nextOrdinal, suite2.suiteName, suite2.suiteId, Some(suite2.getClass.getName), None))
    suite1.run(None, Args(suiteSortingReporter, distributor = Some(distributor), distributedSuiteSorter = Some(suiteSortingReporter)))
    suite2.run(None, Args(suiteSortingReporter, distributor = Some(distributor), distributedSuiteSorter = Some(suiteSortingReporter)))
    suiteSortingReporter(SuiteCompleted(tracker.nextOrdinal, suite2.suiteName, suite2.suiteId, Some(suite2.getClass.getName), None))
    suiteSortingReporter(SuiteCompleted(tracker.nextOrdinal, suite1.suiteName, suite1.suiteId, Some(suite1.getClass.getName), None))
    fun(distributor)
    recordingReporter.eventsReceived
  }
  
  test("ParallelTestExecution should have the events reported in correct order when tests are executed in parallel") {
    forAll(orderExamples) { example =>
      val inOrderEvents = withDistributor(example, _.executeInOrder)
      example.assertOrderTest(inOrderEvents)
      val reverseOrderEvents = withDistributor(example, _.executeInReverseOrder)
      example.assertOrderTest(reverseOrderEvents)
    }
  }
  
  test("ParallelTestExecution should have InfoProvided fired from before and after block in correct order when tests are executed in parallel") {
    forAll(infoExamples) { example =>
      val inOrderEvents = withDistributor(example, _.executeInOrder)
      example.assertBeforeAfterInfo(inOrderEvents)
      val reverseOrderEvents = withDistributor(example, _.executeInReverseOrder)
      example.assertBeforeAfterInfo(reverseOrderEvents)
    }
  }
  
  test("ParallelTestExecution should have the blocking test's events fired without waiting when timeout reaches, and when the missing event finally reach later, it should just get fired") {
    forAll(testTimeoutExamples) { example => 
      val inOrderEvents = withTestHoldingDistributor(example, _.executeInOrder)
      example.assertTestTimeoutTest(inOrderEvents)
      val reverseOrderEvents = withTestHoldingDistributor(example, _.executeInReverseOrder)
      example.assertTestTimeoutTest(reverseOrderEvents)
    }
  }
  
  test("ParallelTestExecution should have the events reported in correct order when multiple suite's tests are executed in parallel") {
    forAll(parallelExamples) { example => 
      val inOrderEvents = withSuiteDistributor(example.suite1, example.suite2, _.executeInOrder)
      example.assertParallelSuites(inOrderEvents)
      //val reverseOrderEvents = withSuiteDistributor(example.suite1, example.suite2, _.executeInReverseOrder)
      //example.assertParallelSuites(reverseOrderEvents)
    }
  }
  
  test("ParallelTestExecution should have the blocking suite's events fired without waiting when timeout reaches, and when the missing event finally reach later, it should just get fired") {
    forAll(suiteTimeoutExamples) { example =>
      val events = withSuiteHoldingDistributor(example, _.executeInOrder)
      example.assertSuiteTimeoutTest(events)
    }
  }
}