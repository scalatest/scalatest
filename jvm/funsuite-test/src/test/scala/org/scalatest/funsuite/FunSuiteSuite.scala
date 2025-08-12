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
package org.scalatest.funsuite

import org.scalatest._
import org.scalatest.SharedHelpers._
import org.scalatest.events._
import org.scalatest.exceptions.DuplicateTestNameException
import org.scalatest.exceptions.TestRegistrationClosedException
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.funsuite.AnyFunSuite

class YeOldeFunSuiteSpec extends AnyFunSpec {

  describe("FunSuite") {
    it("test that test methods with no tags dont show up in tags map") {
    
      val a = new AnyFunSuite {
        test("test not in a group") {}
      }
      assert(a.tags.keySet.size === 0)
    }

    it("test that test functions that result in non unit are registered") {
      val a = new AnyFunSuite {
        test("test this") { 1 }
        test("test that") { "hi" }
      }
      assert(a.expectedTestCount(Filter()) === 2)
      assert(a.testNames.size === 2)
      assert(a.tags.keySet.size === 0)
    }

    it("test that test name cant be reused") {
      intercept[DuplicateTestNameException] {
        new AnyFunSuite {
          test("test this") {}
          test("test this") {}
        }
      }
      intercept[DuplicateTestNameException] {
        new AnyFunSuite {
          ignore("test this") {}
          test("test this") {}
        }
      }
      intercept[DuplicateTestNameException] {
        new AnyFunSuite {
          test("test this") {}
          ignore("test this") {}
        }
      }
      intercept[DuplicateTestNameException] {
        new AnyFunSuite {
          ignore("test this") {}
          ignore("test this") {}
        }
      }
    }
  
    it("test that if you call test after execute you get an test failed exception and the test doesnt run") {
      class MySuite extends AnyFunSuite {
        var fromMethodTestExecuted = false
        var fromConstructorTestExecuted = false
        test("from constructor") {
          fromConstructorTestExecuted = true
        }
        def registerOne(): Unit = {
          test("from method") {
            fromMethodTestExecuted = true
          }
        }
      }
      val a = new MySuite
      a.run(None, Args(SilentReporter))
      assert(a.fromConstructorTestExecuted)
      assert(!a.fromMethodTestExecuted)
      intercept[TestRegistrationClosedException] {
        a.registerOne()
      }
      a.run(None, Args(SilentReporter))
      assert(!a.fromMethodTestExecuted)
    }
  
    it("test that info inside a test method gets out the door") {
      val msg = "hi there, dude"
      class MySuite extends AnyFunSuite {
        test("test this") {
          info(msg)
        }
      }
      val a = new MySuite
      val myRep = new EventRecordingReporter
      a.run(None, Args(myRep))
      val testSucceeded = myRep.testSucceededEventsReceived
      assert(testSucceeded.size === 1)
      val recordedEvents = testSucceeded(0).recordedEvents
      assert(recordedEvents.size === 1)
      val ip = recordedEvents(0).asInstanceOf[InfoProvided]
      assert(ip.message === msg)
    }
  
    it("test that info in the constructor gets out the door") {
      class MyReporter extends Reporter {
        var infoProvidedReceived = false
        var lastEvent: InfoProvided = null
        def apply(event: Event): Unit = {
          event match {
            case event: InfoProvided =>
              infoProvidedReceived = true
              lastEvent = event
            case _ =>
          }
        }
      }
      val msg = "hi there, dude"
      class MySuite extends AnyFunSuite {
        info(msg)
        test("test this") {
        }
      }
      val a = new MySuite
      val myRep = new MyReporter
      a.run(None, Args(myRep))
      assert(myRep.infoProvidedReceived)
      assert(myRep.lastEvent.message === msg)
    }

    it("test that info in the constructor before a test happens first") {
      var infoProvidedReceived = false
      var infoProvidedReceivedBeforeTest = false
      class MyReporter extends Reporter {
        def apply(event: Event): Unit = {
          event match {
            case event: InfoProvided =>
              infoProvidedReceived = true
            case _ =>
          }
        }
      }
      val msg = "hi there, dude"
      class MySuite extends AnyFunSuite {
        info(msg)
        test("test this") {
          if (infoProvidedReceived)
            infoProvidedReceivedBeforeTest = true
        }
      }
      val a = new MySuite
      val myRep = new MyReporter
      a.run(None,Args( myRep))
      assert(infoProvidedReceivedBeforeTest)
    }

    it("test that info in the constructor after a test happens second") {
      var infoProvidedReceived = false
      var infoProvidedReceivedAfterTest = true
      class MyReporter extends Reporter {
        def apply(event: Event): Unit = {
          event match {
            case event: InfoProvided =>
              infoProvidedReceived = true
            case _ =>
          }
        }
      }
      val msg = "hi there, dude"
      class MySuite extends AnyFunSuite {
        test("test this") {
          if (infoProvidedReceived)
            infoProvidedReceivedAfterTest = false
        }
        info(msg)
      }
      val a = new MySuite
      val myRep = new MyReporter
      a.run(None, Args(myRep))
      assert(infoProvidedReceivedAfterTest)
      assert(infoProvidedReceived)
    }

    def callingTestFromWithinATestClauseResultsInATestFailedErrorAtRuntime(): Unit = {

      var testFailedAsExpected = false
      class MyReporter extends Reporter {
        def apply(event: Event): Unit = {
          event match {
            case event: TestFailed =>
              if (event.testName.indexOf("this test should blow up") != -1)
                testFailedAsExpected = true
            case _ =>
          }
        }
      }

      class MySuite extends AnyFunSuite {
        test("this test should blow up") {
          test("is in the wrong place also") {
            assert(1 === 1)
          }
        }
      }

      val a = new MySuite
      a.run(None, Args(new MyReporter))
      assert(testFailedAsExpected)
    }

    def callingTestFromWithinATestWithTagsClauseResultsInATestFailedErrorAtRuntime(): Unit = {
    
      var testFailedAsExpected = false
      class MyReporter extends Reporter {
        def apply(event: Event): Unit = {
          event match {
            case event: TestFailed =>
            if (event.testName.indexOf("this test should blow up") != -1)
              testFailedAsExpected = true
            case _ =>
          }
        }
      }

      class MySuite extends AnyFunSuite {
        test("this test should blow up") {
          test("is in the wrong place also", new Tag("SlowAsMolasses")) {
            assert(1 === 1)
          }
        }
      }

      val a = new MySuite
      a.run(None, Args(new MyReporter))
      assert(testFailedAsExpected)
    }

    def callingIgnoreFromWithinATestClauseResultsInATestFailedErrorAtRuntime(): Unit = {

      var testFailedAsExpected = false
      class MyReporter extends Reporter {
        def apply(event: Event): Unit = {
          event match {
            case event: TestFailed =>
              if (event.testName.indexOf("this test should blow up") != -1)
                testFailedAsExpected = true
            case _ =>
          }
        }
      }

      class MySuite extends AnyFunSuite {
        test("this test should blow up") {
          ignore("is in the wrong place also") {
            assert(1 === 1)
          }
        }
      }

      val a = new MySuite
      a.run(None, Args(new MyReporter))
      assert(testFailedAsExpected)
    }

    def callingIgnoreWithTagsFromWithinATestClauseResultsInATestFailedErrorAtRuntime(): Unit = {
    
      var testFailedAsExpected = false
      class MyReporter extends Reporter {
        def apply(event: Event): Unit = {
          event match {
            case event: TestFailed =>
              if (event.testName.indexOf("this test should blow up") != -1)
                testFailedAsExpected = true
            case _ =>
          }
        }
      }

      class MySuite extends AnyFunSuite {
        test("this test should blow up") {
          ignore("is in the wrong place also", mytags.SlowAsMolasses) {
            assert(1 === 1)
          }
        }
      }

      val a = new MySuite
      a.run(None, Args(new MyReporter))
      assert(testFailedAsExpected)
    }

    it("test that test durations are included in test failed and test succeeded events fired from FunSuite") {

      class MyFunSuite extends AnyFunSuite {
        test("that it succeeds") {}
        test("that it fails") { fail() }
      }

      val myFunSuite = new MyFunSuite
      val myReporter = new TestDurationReporter
      myFunSuite.run(None, Args(myReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker(new Ordinal(99))))
      assert(myReporter.testSucceededWasFiredAndHadADuration)
      assert(myReporter.testFailedWasFiredAndHadADuration)
    }

    it("test that suite durations are included in suite completed events fired from FunSuite") {

      class MyFunSuite extends AnyFunSuite {
        override def nestedSuites = Vector(new Suite {})
      }

      val myFunSuite = new MyFunSuite
      val myReporter = new SuiteDurationReporter
      myFunSuite.run(None, Args(myReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker(new Ordinal(99))))
      assert(myReporter.suiteCompletedWasFiredAndHadADuration)
    }

    it("test that suite durations are included in suite aborted events fired from FunSuite") {

      class SuiteThatAborts extends Suite {
        override def run(testName: Option[String], args: Args): Status = {
          throw new RuntimeException("Aborting for testing purposes")
        }
      }

      class MyFunSuite extends AnyFunSuite {
        override def nestedSuites = Vector(new SuiteThatAborts {})
      }

      val myFunSuite = new MyFunSuite
      val myReporter = new SuiteDurationReporter
      myFunSuite.run(None, Args(myReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker(new Ordinal(99))))
      assert(myReporter.suiteAbortedWasFiredAndHadADuration)
    }

    it("test pending works in FunSuite") {

      class MyFunSuite extends AnyFunSuite {
        test("this test is pending") (pending)
      }

      val mySuite = new MyFunSuite
      val myReporter = new PendingReporter
      mySuite.run(None, Args(myReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker(new Ordinal(99))))
      assert(myReporter.testPendingWasFired)
    }
  }

}
