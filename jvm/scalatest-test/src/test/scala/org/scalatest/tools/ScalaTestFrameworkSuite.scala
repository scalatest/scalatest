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

import org.scalatools.testing._
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.SharedHelpers.EventRecordingReporter
import org.scalatest.funsuite.AnyFunSuite

@RunWith(classOf[JUnitRunner])
class ScalaTestFrameworkSuite extends AnyFunSuite{

  test("framework name"){
    assert(new ScalaTestFramework().name === "ScalaTest")
  }

  test("tests contains 2 correct test fingerprint, TestFingerprint and AnnotatedFingerprint"){
    val framework = new ScalaTestFramework
    val fingerprints = framework.tests
    assert(fingerprints.size === 2)

    val testFingerprint =
      fingerprints(0).asInstanceOf[org.scalatools.testing.TestFingerprint]

    assert(testFingerprint.isModule === false)
    assert(testFingerprint.superClassName === "org.scalatest.Suite")
    
    val annotatedFingerprint = 
      fingerprints(1).asInstanceOf[org.scalatools.testing.AnnotatedFingerprint]
    
    assert(annotatedFingerprint.isModule() === false)
    assert(annotatedFingerprint.annotationName === "org.scalatest.WrapWith")
  }

  test("creates runner with given arguments"){
    val framework = new ScalaTestFramework

    import framework.ScalaTestRunner

    val loggers: Array[Logger] = Array(new TestLogger)
    val runner = framework.testRunner(Thread.currentThread.getContextClassLoader, loggers).asInstanceOf[ScalaTestRunner]
    assert(runner.testLoader == Thread.currentThread.getContextClassLoader)
    assert(runner.loggers === loggers)
  }

  test("should fire SuiteAborted event when after function in BeforeAndAfter throws RuntimeException") {
    val framework = new ScalaTestFramework
    val fingerprints = framework.tests
    assert(fingerprints.size == 2)

    val testFingerprint = fingerprints(0).asInstanceOf[org.scalatools.testing.TestFingerprint]

    val loggers: Array[Logger] = Array(new TestLogger)
    val runner: Runner2 = framework.testRunner(Thread.currentThread.getContextClassLoader, loggers)
    runner.run("org.scalatest.tools.scalasbt.FaulthyBeforeAndAfterSuite", testFingerprint, new TestEventHandler, Array("-C", classOf[EventRecordingReporter].getName))

    val runConfig = framework.RunConfig
    val repOption = runConfig.reporter.get
    assert(repOption.isDefined)

    val rep = repOption.get
    rep.reporters.find(_.isInstanceOf[EventRecordingReporter]) match {
      case Some(recordingRep: EventRecordingReporter) =>
        assert(recordingRep.testSucceededEventsReceived.size == 1)
        assert(recordingRep.suiteStartingEventsReceived.size == 1)
        assert(recordingRep.suiteCompletedEventsReceived.size == 0)
        assert(recordingRep.suiteAbortedEventsReceived.size == 1)

      case _ => fail("Expected to find EventRecordingReporter, but not found.")
    }
  }

  test("should fire SuiteAborted event when afterAll function in BeforeAndAfterAll throws RuntimeException") {
    val framework = new ScalaTestFramework
    val fingerprints = framework.tests
    assert(fingerprints.size == 2)

    val testFingerprint = fingerprints(0).asInstanceOf[org.scalatools.testing.TestFingerprint]

    val loggers: Array[Logger] = Array(new TestLogger)
    val runner: Runner2 = framework.testRunner(Thread.currentThread.getContextClassLoader, loggers)
    runner.run("org.scalatest.tools.scalasbt.FaulthyBeforeAndAfterAllSuite", testFingerprint, new TestEventHandler, Array("-C", classOf[EventRecordingReporter].getName))

    val runConfig = framework.RunConfig
    val repOption = runConfig.reporter.get
    assert(repOption.isDefined)

    val rep = repOption.get
    rep.reporters.find(_.isInstanceOf[EventRecordingReporter]) match {
      case Some(recordingRep: EventRecordingReporter) =>
        assert(recordingRep.testSucceededEventsReceived.size == 1)
        assert(recordingRep.suiteStartingEventsReceived.size == 1)
        assert(recordingRep.suiteCompletedEventsReceived.size == 0)
        assert(recordingRep.suiteAbortedEventsReceived.size == 1)

      case _ => fail("Expected to find EventRecordingReporter, but not found.")
    }
  }

  test("should fire SuiteAborted event when afterAll function in FaulthyBeforeAndAfterAllConfigMapSuite throws RuntimeException") {
    val framework = new ScalaTestFramework
    val fingerprints = framework.tests
    assert(fingerprints.size == 2)

    val testFingerprint = fingerprints(0).asInstanceOf[org.scalatools.testing.TestFingerprint]

    val loggers: Array[Logger] = Array(new TestLogger)
    val runner: Runner2 = framework.testRunner(Thread.currentThread.getContextClassLoader, loggers)
    runner.run("org.scalatest.tools.scalasbt.FaulthyBeforeAndAfterAllConfigMapSuite", testFingerprint, new TestEventHandler, Array("-C", classOf[EventRecordingReporter].getName))

    val runConfig = framework.RunConfig
    val repOption = runConfig.reporter.get
    assert(repOption.isDefined)

    val rep = repOption.get
    rep.reporters.find(_.isInstanceOf[EventRecordingReporter]) match {
      case Some(recordingRep: EventRecordingReporter) =>
        assert(recordingRep.testSucceededEventsReceived.size == 1)
        assert(recordingRep.suiteStartingEventsReceived.size == 1)
        assert(recordingRep.suiteCompletedEventsReceived.size == 0)
        assert(recordingRep.suiteAbortedEventsReceived.size == 1)

      case _ => fail("Expected to find EventRecordingReporter, but not found.")
    }
  }

  test("should fire SuiteAborted event when afterEach function in BeforeAndAfterEach throws RuntimeException") {
    val framework = new ScalaTestFramework
    val fingerprints = framework.tests
    assert(fingerprints.size == 2)

    val testFingerprint = fingerprints(0).asInstanceOf[org.scalatools.testing.TestFingerprint]

    val loggers: Array[Logger] = Array(new TestLogger)
    val runner: Runner2 = framework.testRunner(Thread.currentThread.getContextClassLoader, loggers)
    runner.run("org.scalatest.tools.scalasbt.FaulthyBeforeAndAfterEachSuite", testFingerprint, new TestEventHandler, Array("-C", classOf[EventRecordingReporter].getName))

    val runConfig = framework.RunConfig
    val repOption = runConfig.reporter.get
    assert(repOption.isDefined)

    val rep = repOption.get
    rep.reporters.find(_.isInstanceOf[EventRecordingReporter]) match {
      case Some(recordingRep: EventRecordingReporter) =>
        assert(recordingRep.testSucceededEventsReceived.size == 1)
        assert(recordingRep.suiteStartingEventsReceived.size == 1)
        assert(recordingRep.suiteCompletedEventsReceived.size == 0)
        assert(recordingRep.suiteAbortedEventsReceived.size == 1)

      case _ => fail("Expected to find EventRecordingReporter, but not found.")
    }
  }

  test("should fire SuiteAborted event when afterEach function in BeforeAndAfterEachTestData throws RuntimeException") {
    val framework = new ScalaTestFramework
    val fingerprints = framework.tests
    assert(fingerprints.size == 2)

    val testFingerprint = fingerprints(0).asInstanceOf[org.scalatools.testing.TestFingerprint]

    val loggers: Array[Logger] = Array(new TestLogger)
    val runner: Runner2 = framework.testRunner(Thread.currentThread.getContextClassLoader, loggers)
    runner.run("org.scalatest.tools.scalasbt.FaulthyBeforeAndAfterEachTestDataSuite", testFingerprint, new TestEventHandler, Array("-C", classOf[EventRecordingReporter].getName))

    val runConfig = framework.RunConfig
    val repOption = runConfig.reporter.get
    assert(repOption.isDefined)

    val rep = repOption.get
    rep.reporters.find(_.isInstanceOf[EventRecordingReporter]) match {
      case Some(recordingRep: EventRecordingReporter) =>
        assert(recordingRep.testSucceededEventsReceived.size == 1)
        assert(recordingRep.suiteStartingEventsReceived.size == 1)
        assert(recordingRep.suiteCompletedEventsReceived.size == 0)
        assert(recordingRep.suiteAbortedEventsReceived.size == 1)

      case _ => fail("Expected to find EventRecordingReporter, but not found.")
    }
  }

  class TestLogger extends Logger{
    def trace(t:Throwable): Unit ={}
    def error(msg:String): Unit ={}
    def warn(msg:String): Unit ={}
    def info(msg:String): Unit ={}
    def debug(msg:String): Unit ={}
    def ansiCodesSupported = false
  }

  class TestEventHandler extends EventHandler {

    private var errorEvents = List[Event]()
    private var failureEvents = List[Event]()
    private var skippedEvents = List[Event]()
    private var successEvents = List[Event]()

    override def handle(event: Event): Unit = {
      event.result() match {
        case Result.Success => successEvents ::= event
        case Result.Error => errorEvents ::= event
        case Result.Failure => failureEvents ::= event
        case Result.Skipped => skippedEvents ::= event

      }
    }

    def errorEventsReceived = errorEvents.reverse
    def failureEventsReceived = failureEvents.reverse
    def skippedEventsReceived = skippedEvents.reverse
    def successEventsReceived = successEvents.reverse
  }
}
