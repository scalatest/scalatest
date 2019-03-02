/*
 * Copyright 2001-2013 Artima, Inc.
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
package org.scalatestplus.testng {

  import org.scalatest._
import org.scalatest.events._
import org.scalatestplus.testng.testpackage._
import org.scalatest.fixture
import org.hamcrest.core.IsAnything
import org.scalatest.SharedHelpers.EventRecordingReporter

  class TestNGSuiteSuite extends FunSuite {

    test("Reporter should be notified when test passes") {

      val reporter = new EventRecordingReporter

      val status = new StatefulStatus
      (new SuccessTestNGSuite()).runTestNG(reporter, new Tracker, status)
      status.setCompleted()

      assert(reporter.testSucceededEventsReceived.length == 1)
    }

    test("Reporter should be notified when test fails") {

      val reporter = new EventRecordingReporter

      val status = new StatefulStatus
      (new FailureTestNGSuite()).runTestNG(reporter, new Tracker, status)
      status.setCompleted()

      assert(reporter.testFailedEventsReceived.length == 1)
    }

    test("If a test fails due to an exception, Report should have the exception") {
      
      val testReporter = new EventRecordingReporter

      // when
      val status = new StatefulStatus
      (new FailureTestNGSuite()).runTestNG(testReporter, new Tracker, status)
      status.setCompleted

      // then
      testReporter.eventsReceived.lastOption match {
        case Some(TestFailed(_, _, _, _, _, _, _, _, _, throwable, _, _, _, _, _, _, _)) =>
          assert(throwable.get.getMessage === "fail")
        case _ => fail()
      }
    }

    test("Report should be generated for each invocation") {
      
      val reporter = new EventRecordingReporter

      val status = new StatefulStatus
      (new TestNGSuiteWithInvocationCount()).runTestNG(reporter, new Tracker, status)
      status.setCompleted()

      assert(reporter.testSucceededEventsReceived.length == 10)
    }

    test("Reporter should be notified when test is skipped") {

      val reporter = new EventRecordingReporter

      val status = new StatefulStatus
      (new SuiteWithSkippedTest()).runTestNG(reporter, new Tracker, status)
      status.setCompleted()

      assert(reporter.suiteStartingEventsReceived.isEmpty)
      assert(reporter.testStartingEventsReceived.length == 1)
      assert(reporter.testFailedEventsReceived.length == 1)
      assert(reporter.testIgnoredEventsReceived.length == 1)
      assert(reporter.suiteCompletedEventsReceived.isEmpty)
    }
    
    test("Only the correct method should be run when specifying a single method to run") {
      
      val reporter = new EventRecordingReporter

      val status = new StatefulStatus
      (new SuiteWithTwoTests()).runTestNG("testThatPasses", reporter, new Tracker, status)
      status.setCompleted()

      assert(reporter.testSucceededEventsReceived.length == 1)
    }

    test("Report for failing tests should include rerunner") {
      
      val testReporter = new EventRecordingReporter

      // when - run the failing suite
      val status = new StatefulStatus
      new FailureTestNGSuite().runTestNG(testReporter, new Tracker, status)
      status.setCompleted()

      // then get rerunnable from the event 
      testReporter.eventsReceived.lastOption match {
        case Some(TestFailed(_, _, _, _, _, _, _, _, _, _, _, _, _, rerunnable, _, _, _)) =>
          assert(rerunnable.isDefined)
        case _ => fail()
      }
    }

    test("Report for passing tests should include rerunner") {
      
      val testReporter = new EventRecordingReporter

      // when - run the passing suite
      val status = new StatefulStatus
      new SuccessTestNGSuite().runTestNG(testReporter, new Tracker, status)
      status.setCompleted()

      // then get rerunner from report 
      val rerunner = testReporter.eventsReceived.last.asInstanceOf[TestSucceeded].rerunner
      assert(rerunner != None)
      assert(rerunner.get === classOf[SuccessTestNGSuite].getName)
    }
    
    
    test("infoProvided should be available for BeforeMethod/Class/Suite annotations") {
      // this needs to be written after i figure out the mock integration
    }     
    
    test("infoProvided should be available for AfterMethod/Class/Suite annotations") {
      // this needs to be written after i figure out the mock integration
    }     
  }

  package testpackage {
    
    import org.testng.annotations._
    
    class FailureTestNGSuite extends TestNGSuite {
      @Test def testThatFails(): Unit = { throw new Exception("fail") }
    }
    
    class SuccessTestNGSuite extends TestNGSuite {
      @Test def testThatPasses(): Unit = {}
    }
    
    class TestNGSuiteWithInvocationCount extends TestNGSuite {
      @Test(invocationCount = 10) def testThatPassesTenTimes(): Unit = {}
    }
    
    class SuiteWithSkippedTest extends TestNGSuite {
      @Test(groups = Array("run")) def dependeeThatFails(): Unit = { throw new Exception("fail") }
      @Test(dependsOnGroups = Array("run")) def depender(): Unit = {}
    } 

    class SuiteWithTwoTests extends TestNGSuite {
      @Test def testThatPasses(): Unit = {}
      @Test def anotherTestThatPasses(): Unit = {}
    }      
    
    class SuiteWithBeforeAndAfterAnnotations extends TestNGSuite {
    }
  }
}


