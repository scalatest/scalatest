/*
 * Copyright 2001-2008 Artima, Inc.
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
package org.scalatest.tools {

import org.scalatest.{FunSuite, Outcome, DispatchReporter, Resources}
import org.scalatools.testing.{Event, EventHandler, Result, Logger, Runner => TestingRunner}
import org.scalatest.SharedHelpers.EventRecordingReporter
import org.scalatest.exceptions.NotAllowedException

  // testing runner.run:
  // def run(testClassName: String, fingerprint: TestFingerprint, args: Array[String]): Array[Event]
  class ScalaTestRunnerSuite extends FunSuite {
    test("call with simple class") {
      val results = run("org.scalatest.tools.test.SimpleTest")
      assert(results(0).testName === "hello, world")
      assert(results(0).result === Result.Success)
    }

    test("three different results") {
      val results = run("org.scalatest.tools.test.ThreeTestsTest")

      assert(results(0).testName === "hello, world")
      assert(results(0).result === Result.Success)

      assert(results(1).testName === "throw")
      assert(results(1).result === Result.Failure)
      assert(results(1).error.getMessage === "baah")

      assert(results(2).testName === "assert bad")
      assert(results(2).result === Result.Failure)
      assert(results(2).error.getMessage === "1 did not equal 3")

      assert(results.size === 3)
    }

    test("one tag included") {
      val results = run("org.scalatest.tools.test.TagsTest", "-n hello")

      assert(results(0).testName === "hello, world")
      assert(results(0).result === Result.Success)

      assert(results.size === 1)
    }

    test("two tags included") {
      val results = run("org.scalatest.tools.test.TagsTest", Array("-n", "hello helloAgain"))

      assert(results(0).testName === "hello, world")
      assert(results(0).result === Result.Success)

      assert(results(1).testName === "hello, world again")
      assert(results(1).result === Result.Success)

      assert(results.size === 2)
    }

    test("one tag excluded") {
      val results = run("org.scalatest.tools.test.TagsTest", Array("-l", "hello"))

      assert(results(0).testName === "hello, world again")
      assert(results(0).result === Result.Success)

      assert(results(1).testName === "tag3")
      assert(results(1).result === Result.Success)

      assert(results(2).testName === "throw")
      assert(results(2).result === Result.Failure)
      assert(results(2).error.getMessage === "baah")

      assert(results(3).testName === "assert bad")
      assert(results(3).result === Result.Failure)
      assert(results(3).error.getMessage === "1 did not equal 3")

      assert(results.size === 4)
    }

    test("configs") {
      val results = run("org.scalatest.tools.test.TestWithConfigMap", "-Djosh=cool")
      assert(results(0).testName === "get config")
      assert(results(0).result === Result.Success)

      val resultsF = run("org.scalatest.tools.test.TestWithConfigMap", "-Djosh=bad")
      assert(resultsF(0).testName === "get config")
      assert(resultsF(0).result === Result.Failure)
      assert(resultsF(0).error.getMessage === "\"[bad]\" did not equal \"[cool]\"")
    }

    test("configs 2"){
      val results = run("org.scalatest.tools.test.TestWithConfigMap2", "-Da=z -Db=y -Dc=x")
      assert(results(0).testName === "get config")
      assert(results(0).result === Result.Success)
    }

    test("illegal arg on private constructor, inaccessible suite"){
      val results = run("org.scalatest.tools.test.PrivateConstructor")
      assert(results.size === 0)
    }
    
    test("@DoNotDiscover suite"){
      val results = run("org.scalatest.tools.test.DoNotDiscoverSuite")
      assert(results.size === 0)
    }

    test("skipped test results in Result.Skipped") {
      val results = run("org.scalatest.tools.test.SuiteWithSkippedTest")
      assert(results.size === 2)

      assert(results(0).testName === "dependeeThatFails")
      assert(results(0).result === Result.Failure)
      assert(results(0).error.getMessage === "fail")

      assert(results(1).testName === "depender")
      assert(results(1).result === Result.Skipped)
    }


    test("pending test results in Result.Skipped") {
      val results = run("org.scalatest.tools.test.PendingTest")
      assert(results.size === 1)

      assert(results(0).testName === "i am pending")
      assert(results(0).result === Result.Skipped)
    }
    
    test("throw IllegalArgumentException when -g is passed in as argument") {
      intercept[IllegalArgumentException] {
        run("org.scalatest.tools.test.PendingTest", Array("-g"))
      }
    }
    
    test("-w should execute suites that match the specified package and its sub packages") {
      val result1 = run("org.scalatest.tools.test.TagsTest", Array("-w", "org.scalatest.tools"))
      assert(result1.size === 5)
      
      val result2 = run("org.scalatest.tools.test.TagsTest", Array("-w", "org.scalatest.tools.test"))
      assert(result2.size === 5)
      
      val result3 = run("org.scalatest.SuiteSuite", Array("-w", "org.scalatest.tools.test"))
      assert(result3.size === 0)
    }
    
    test("-m should execute suites that match the specified package and not its sub packages") {
      val result1 = run("org.scalatest.tools.test.TagsTest", Array("-m", "org.scalatest.tools"))
      assert(result1.size === 0)
      
      val result2 = run("org.scalatest.tools.test.TagsTest", Array("-m", "org.scalatest.tools.test"))
      assert(result2.size === 5)
      
      val result3 = run("org.scalatest.SuiteSuite", Array("-m", "org.scalatest.tools.test"))
      assert(result3.size === 0)
      
      val result4 = run("org.scalatest.enablers.NoParamSpec", Array("-m", "org.scalatest.concurrent"))
      assert(result4.size === 0)
    }
    
    test("ScalaTestRunner.run should throw IllegalArgumentException when -s is passed in") {
      val iae = intercept[IllegalArgumentException] {
        run("org.scalatest.tools.test.SimpleTest", Array("-s", "org.scalatest.tools.test.SimpleTest"))
      }
      assert(iae.getMessage === "-s (suite) is not supported when runs in SBT, please use SBT's test-only instead.")
    }
    
    test("ScalaTestRunner.run should throw IllegalArgumentException when -j is passed in") {
      val iae = intercept[IllegalArgumentException] {
        run("org.scalatest.tools.test.SimpleTest", Array("-j", "org.scalatest.tools.test.SimpleTest"))
      }
      assert(iae.getMessage === "-j (junit) is not supported when runs in SBT.")
    }
    
    test("ScalaTestRunner.run should throw IllegalArgumentException when -b is passed in") {
      val iae = intercept[IllegalArgumentException] {
        run("org.scalatest.tools.test.SimpleTest", Array("-b", "org.scalatest.tools.test.SimpleTest"))
      }
      assert(iae.getMessage === "-b (testng) is not supported when runs in SBT.")
    }
    
    test("ScalaTestRunner.run should throw IllegalArgumentException when -c is passed in") {
      val iae = intercept[IllegalArgumentException] {
        run("org.scalatest.tools.test.SimpleTest", Array("-c"))
      }
      assert(iae.getMessage === "-c, -P (concurrent) is not supported when runs in SBT.")
    }
    
    test("ScalaTestRunner.run should throw IllegalArgumentException when -P is passed in") {
      val iae = intercept[IllegalArgumentException] {
        run("org.scalatest.tools.test.SimpleTest", Array("-P"))
      }
      assert(iae.getMessage === "-c, -P (concurrent) is not supported when runs in SBT.")
    }
    
    test("ScalaTestRunner.run should throw IllegalArgumentException when -PS is passed in") {
      val iae = intercept[IllegalArgumentException] {
        run("org.scalatest.tools.test.SimpleTest", Array("-PS"))
      }
      assert(iae.getMessage === "-c, -P (concurrent) is not supported when runs in SBT.")
    }
    
    test("ScalaTestRunner.run should be able to pass in custom reporter via -C") {
      val framework = new ScalaTestFramework()
      val runner: TestingRunner = framework.testRunner(Thread.currentThread.getContextClassLoader, Array(new TestLogger))
      val listener = new EventHandler {
        def handle(event: Event) {}
      }
      runner.run("org.scalatest.tools.scalasbt.SampleSuite", fingerprint, listener, Array("-C", classOf[EventRecordingReporter].getName))
      framework.RunConfig.reporter.get match {
        case Some(dispatchRep: DispatchReporter) => 
          dispatchRep.reporters.find(_.isInstanceOf[EventRecordingReporter]) match {
            case Some(recordingRep : EventRecordingReporter) => 
              assert(recordingRep.testSucceededEventsReceived.size === 3)
            case _ => fail("Expected to find EventRecordingReporter, but not found.")
          }
        case _ => fail("Expected to find DispatchReporter, but not found.")
      }
    }
    
    test("-y should do nothing when the task to execute is a chosen style") {
      val framework = new ScalaTestFramework()
      val runner: TestingRunner = framework.testRunner(Thread.currentThread.getContextClassLoader, Array(new TestLogger))
      val listener = new EventHandler {
        def handle(event: Event) {}
      }
      runner.run("org.scalatest.tools.scalasbt.SampleSuite", fingerprint, listener, Array("-y", "org.scalatest.FunSuite", "-C", classOf[EventRecordingReporter].getName))
      framework.RunConfig.reporter.get match {
        case Some(dispatchRep: DispatchReporter) => 
          dispatchRep.reporters.find(_.isInstanceOf[EventRecordingReporter]) match {
            case Some(recordingRep : EventRecordingReporter) => 
              assert(recordingRep.testSucceededEventsReceived.size === 3)
              assert(recordingRep.suiteCompletedEventsReceived.size === 1)
            case _ => fail("Expected to find EventRecordingReporter, but not found.")
          }
        case _ => fail("Expected to find DispatchReporter, but not found.")
      }
    }
    
    test("-y should get SuiteAborted event with NotAllowedException when the task to execute is not a chosen style") {
      val framework = new ScalaTestFramework()
      val runner: TestingRunner = framework.testRunner(Thread.currentThread.getContextClassLoader, Array(new TestLogger))
      val listener = new EventHandler {
        def handle(event: Event) {}
      }
      runner.run("org.scalatest.tools.scalasbt.SampleSuite", fingerprint, listener, Array("-y", "org.scalatest.FunSpec", "-C", classOf[EventRecordingReporter].getName))
      framework.RunConfig.reporter.get match {
        case Some(dispatchRep: DispatchReporter) => 
          dispatchRep.reporters.find(_.isInstanceOf[EventRecordingReporter]) match {
            case Some(recordingRep : EventRecordingReporter) => 
              assert(recordingRep.testSucceededEventsReceived.size === 0)
              val suiteAbortedEvents = recordingRep.suiteAbortedEventsReceived
              assert(suiteAbortedEvents.size === 1)
              suiteAbortedEvents(0).throwable match {
                case Some(e: NotAllowedException) => 
                  assert(e.getMessage === Resources("notTheChosenStyle", "org.scalatest.FunSuite", "org.scalatest.FunSpec"))
                case _ => fail("Expected SuiteAborted to carry NotAllowedException, but it did not.")
              }
            case _ => fail("Expected to find EventRecordingReporter, but not found.")
          }
        case _ => fail("Expected to find DispatchReporter, but not found.")
      }
    }

    def runner: TestingRunner = {
      new ScalaTestFramework().testRunner(Thread.currentThread.getContextClassLoader, Array(new TestLogger))
    }

    val fingerprint = {
      val fingerprints = new ScalaTestFramework().tests
      fingerprints(0).
                    asInstanceOf[org.scalatools.testing.TestFingerprint]
    }

    def run(classname: String): Array[Event] = run(classname, Array[String]())
    def run(classname: String, args:String): Array[Event] = run(classname, args.split(" "))
    def run(classname: String, args:Array[String]): Array[Event] = {
      // val buf = scala.collection.mutable.ArrayBuffer[Event]() // Only worked under 2.8
      val buf = new scala.collection.mutable.ArrayBuffer[Event]
      val listener = new EventHandler {
        def handle(event: Event) {
          buf += event
        }
      }
      runner.run(classname, fingerprint, listener, args)
      buf.toArray
    }

    class TestLogger extends Logger {
      def trace(t:Throwable) {}
      def error(msg: String) {}
      def warn(msg: String) {}
      def info(msg: String) {}
      def debug(msg: String) {}
      def ansiCodesSupported = false
    }
  }

  package test{

    private class SimpleTest extends FunSuite {
      test("hello, world") {"hello, world"}
    }

    private class ThreeTestsTest extends FunSuite {
      test("hello, world") {"hello, world"}
      test("throw") {throw new Exception("baah")}
      test("assert bad") {assert(1 === 3)}
    }

    import org.scalatest.fixture
    private class TestWithConfigMap extends fixture.FunSuite {
      type FixtureParam = String
      override def withFixture(test: OneArgTest): Outcome = {
        test(test.configMap("josh").toString)
      }
      test("get config"){ conf => assert(conf === "cool") }
    }


    private class TestWithConfigMap2 extends fixture.FunSuite {
      type FixtureParam = Map[String,Any]
      override def withFixture(test: OneArgTest): Outcome = {
        test(test.configMap)
      }
      test("get config"){ conf => assert(conf === Map("a" -> "z", "b" -> "y", "c" -> "x")) }
    }

    private class TagsTest extends FunSuite {
      test("hello, world", org.scalatest.Tag("hello")) {"hello, world"}
      test("hello, world again", org.scalatest.Tag("helloAgain")) {"hello, world again"}
      test("tag3", org.scalatest.Tag("tag3")) {"tag3"}
      test("throw") {throw new Exception("baah")}
      test("assert bad") {assert(1 === 3)}
    }

    private class PrivateConstructor private() extends FunSuite
    
    import org.scalatest.DoNotDiscover
    @DoNotDiscover
    private class DoNotDiscoverSuite extends FunSuite {
      test("do not test me") {}
    }

    private class PendingTest extends FunSuite {
      test("i am pending")(pending)
    }

    import org.scalatest.testng.TestNGSuite
    private class SuiteWithSkippedTest extends TestNGSuite {
      import org.testng.annotations.Test
      @Test(groups = Array("run")) def dependeeThatFails() { throw new Exception("fail") }
      @Test(dependsOnGroups = Array("run")) def depender() {}
    }
  }
}
