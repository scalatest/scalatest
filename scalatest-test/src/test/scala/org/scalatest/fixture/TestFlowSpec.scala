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
package org.scalatest.fixture

import scala.concurrent.Future
import scala.util.Success
import org.scalatest.{Matchers, Args, Stopper, Tracker, Filter, ConfigMap, Assertion, PendingStatement}
import org.scalatest.SharedHelpers._
import org.scalatest.exceptions.{DuplicateTestNameException, NotAllowedException}
import org.scalatest.events.{LineInFile, SeeStackDepthException, IndentedText}

class TestFlowSpec extends org.scalatest.AsyncFunSpec with Matchers {
  describe("A Test0") {
    it("should offer a factory method in its companion that takes a by-name of type Future[T]") {
      """Test0("my name") { 99 }: Test0[Int]""" should compile
      """Test0("my name") { "hello" }: Test0[String]""" should compile
      var x = false
      Test0("my name") {
        x = true
      }
      x shouldBe false
    }
    it("should have a name method") {
      Test0("first")(3).name shouldBe "first"
    }
    it("should return the all test names from testNames when andThen is used to compose Test0s and TestFlows, a Set that iterates in left to right order") {
      val flow = Test0("first")(5).andThen(Test1("second") { (i: Int) => (i * 4).toString })
      flow.testNames shouldEqual Set("first", "second")
      flow.testNames.iterator.toList shouldEqual List("first", "second")
    }
    describe("when it was not composed with anything else") {
      describe("when the test succeeds") {
        it("should report a test succeeded event to the passed-in reporter") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow = Test0("happy path")(42)
          }
          assert(suite.testNames == Set("happy path"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 1)
          assert(testStarting(0).testName == "happy path")
          assert(testStarting(0).testText == "happy path")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 8, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 1)
          assert(testSucceeded(0).testName == "happy path")
          assert(testSucceeded(0).testText == "happy path")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 13, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- happy path", "happy path", 1)))
        }
      }
      describe("when the test fails") {
        it("should report a test failed event to the passed-in reporter") {
          val myRep = new EventRecordingReporter
          val suite =
            new TestFlow {
              val flow =
                Test0("happy path") {
                  throw new RuntimeException("oops!")
                }
            }
          assert(suite.testNames == Set("happy path"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 1)
          assert(testStarting(0).testName == "happy path")
          assert(testStarting(0).testText == "happy path")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 10, "TestFlowSpec.scala", testFilePathname)))
          val testFailed = myRep.testFailedEventsReceived
          assert(testFailed.size == 1)
          assert(testFailed(0).testName == "happy path")
          assert(testFailed(0).testText == "happy path")
          assert(testFailed(0).location == Some(SeeStackDepthException))
          assert(testFailed(0).formatter == Some(IndentedText("- happy path", "happy path", 1)))
        }
      }
    }
    describe("when it was composed with something else") {
      describe("when the test succeeds") {
        it("should report 2 test succeeded events to the passed-in reporter when andThen with another TestFlow") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              Test0("first")(3).andThen(
                Test1("second") { (i: Int) => (i * 4).toString }
              )
          }
          assert(suite.testNames == Set("first", "second"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 2)
          assert(testStarting(0).testName == "first")
          assert(testStarting(0).testText == "first")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 10, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "second")
          assert(testStarting(1).testText == "second")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 12, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 2)
          assert(testSucceeded(0).testName == "first")
          assert(testSucceeded(0).testText == "first")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 18, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- first", "first", 1)))
          assert(testSucceeded(1).testName == "second")
          assert(testSucceeded(1).testText == "second")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 21, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- second", "second", 1)))
        }
        it("should report 3 test succeeded events to the passed-in reporter when andThen with TestFlow that andThen with another TestFlow") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              Test0("first")(3).andThen(
                (Test1("second") { (i: Int) => (i * 4) }).andThen(
                  Test1("third") { (i: Int) => (i * 7).toString }
                )
              )
          }
          assert(suite.testNames == Set("first", "second", "third"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 3)
          assert(testStarting(0).testName == "first")
          assert(testStarting(0).testText == "first")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 12, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "second")
          assert(testStarting(1).testText == "second")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 14, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "third")
          assert(testStarting(2).testText == "third")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 16, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 3)
          assert(testSucceeded(0).testName == "first")
          assert(testSucceeded(0).testText == "first")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 23, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- first", "first", 1)))
          assert(testSucceeded(1).testName == "second")
          assert(testSucceeded(1).testText == "second")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 26, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- second", "second", 1)))
          assert(testSucceeded(2).testName == "third")
          assert(testSucceeded(2).testText == "third")
          assert(testSucceeded(2).location == Some(LineInFile(thisLineNumber - 29, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(2).formatter == Some(IndentedText("- third", "third", 1)))
        }
        it("should report 3 test succeeded events to the passed-in reporter when andThen with TestFlow that compose with another TestFlow") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              Test0("first")(3).andThen(
                (Test1("third") { (i: Int) => (i * 7).toString }).compose(
                  Test1("second") { (i: Int) => (i * 4) }
                )
              )
          }
          assert(suite.testNames == Set("first", "second", "third"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 3)
          assert(testStarting(0).testName == "first")
          assert(testStarting(0).testText == "first")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 12, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "second")
          assert(testStarting(1).testText == "second")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 13, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "third")
          assert(testStarting(2).testText == "third")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 17, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 3)
          assert(testSucceeded(0).testName == "first")
          assert(testSucceeded(0).testText == "first")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 23, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- first", "first", 1)))
          assert(testSucceeded(1).testName == "second")
          assert(testSucceeded(1).testText == "second")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 25, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- second", "second", 1)))
          assert(testSucceeded(2).testName == "third")
          assert(testSucceeded(2).testText == "third")
          assert(testSucceeded(2).location == Some(LineInFile(thisLineNumber - 30, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(2).formatter == Some(IndentedText("- third", "third", 1)))
        }
        it("should report 3 test succeeded events to the passed-in reporter when andThen with another 2 succeeded Test1") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              Test0("test 1")(3).andThen(
                Test1("test 2") { (i: Int) => succeed },
                Test1("test 3") { (i: Int) => succeed }
              )
          }
          assert(suite.testNames == Set("test 1", "test 2", "test 3"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 3)
          assert(testStarting(0).testName == "test 1")
          assert(testStarting(0).testText == "test 1")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 11, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "test 2")
          assert(testStarting(1).testText == "test 2")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 13, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "test 3")
          assert(testStarting(2).testText == "test 3")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 15, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 3)
          assert(testSucceeded(0).testName == "test 1")
          assert(testSucceeded(0).testText == "test 1")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 22, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- test 1", "test 1", 1)))
          assert(testSucceeded(1).testName == "test 2")
          assert(testSucceeded(1).testText == "test 2")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 25, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- test 2", "test 2", 1)))
          assert(testSucceeded(2).testName == "test 3")
          assert(testSucceeded(2).testText == "test 3")
          assert(testSucceeded(2).location == Some(LineInFile(thisLineNumber - 28, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(2).formatter == Some(IndentedText("- test 3", "test 3", 1)))
        }
        it("should report 4 test succeeded events to the passed-in reporter when andThen with another 3 succeeded Test1") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              Test0("test 1")(3).andThen(
                Test1("test 2") { (i: Int) => succeed },
                Test1("test 3") { (i: Int) => succeed },
                Test1("test 4") { (i: Int) => succeed }
              )
          }
          assert(suite.testNames == Set("test 1", "test 2", "test 3", "test 4"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 4)
          assert(testStarting(0).testName == "test 1")
          assert(testStarting(0).testText == "test 1")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 12, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "test 2")
          assert(testStarting(1).testText == "test 2")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 14, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "test 3")
          assert(testStarting(2).testText == "test 3")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 16, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(3).testName == "test 4")
          assert(testStarting(3).testText == "test 4")
          assert(testStarting(3).location == Some(LineInFile(thisLineNumber - 18, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 4)
          assert(testSucceeded(0).testName == "test 1")
          assert(testSucceeded(0).testText == "test 1")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 26, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- test 1", "test 1", 1)))
          assert(testSucceeded(1).testName == "test 2")
          assert(testSucceeded(1).testText == "test 2")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 29, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- test 2", "test 2", 1)))
          assert(testSucceeded(2).testName == "test 3")
          assert(testSucceeded(2).testText == "test 3")
          assert(testSucceeded(2).location == Some(LineInFile(thisLineNumber - 32, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(2).formatter == Some(IndentedText("- test 3", "test 3", 1)))
          assert(testSucceeded(3).testName == "test 4")
          assert(testSucceeded(3).testText == "test 4")
          assert(testSucceeded(3).location == Some(LineInFile(thisLineNumber - 35, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(3).formatter == Some(IndentedText("- test 4", "test 4", 1)))
        }
        it("should report 5 test succeeded events to the passed-in reporter when andThen with another 4 succeeded Test1") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              Test0("test 1")(3).andThen(
                Test1("test 2") { (i: Int) => succeed },
                Test1("test 3") { (i: Int) => succeed },
                Test1("test 4") { (i: Int) => succeed },
                Test1("test 5") { (i: Int) => succeed }
              )
          }
          assert(suite.testNames == Set("test 1", "test 2", "test 3", "test 4", "test 5"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 5)
          assert(testStarting(0).testName == "test 1")
          assert(testStarting(0).testText == "test 1")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 13, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "test 2")
          assert(testStarting(1).testText == "test 2")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 15, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "test 3")
          assert(testStarting(2).testText == "test 3")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 17, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(3).testName == "test 4")
          assert(testStarting(3).testText == "test 4")
          assert(testStarting(3).location == Some(LineInFile(thisLineNumber - 19, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(4).testName == "test 5")
          assert(testStarting(4).testText == "test 5")
          assert(testStarting(4).location == Some(LineInFile(thisLineNumber - 21, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 5)
          assert(testSucceeded(0).testName == "test 1")
          assert(testSucceeded(0).testText == "test 1")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 30, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- test 1", "test 1", 1)))
          assert(testSucceeded(1).testName == "test 2")
          assert(testSucceeded(1).testText == "test 2")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 33, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- test 2", "test 2", 1)))
          assert(testSucceeded(2).testName == "test 3")
          assert(testSucceeded(2).testText == "test 3")
          assert(testSucceeded(2).location == Some(LineInFile(thisLineNumber - 36, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(2).formatter == Some(IndentedText("- test 3", "test 3", 1)))
          assert(testSucceeded(3).testName == "test 4")
          assert(testSucceeded(3).testText == "test 4")
          assert(testSucceeded(3).location == Some(LineInFile(thisLineNumber - 39, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(3).formatter == Some(IndentedText("- test 4", "test 4", 1)))
          assert(testSucceeded(4).testName == "test 5")
          assert(testSucceeded(4).testText == "test 5")
          assert(testSucceeded(4).location == Some(LineInFile(thisLineNumber - 42, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(4).formatter == Some(IndentedText("- test 5", "test 5", 1)))
        }
        it("should report 6 test succeeded events to the passed-in reporter when andThen with another 5 succeeded Test1") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              Test0("test 1")(3).andThen(
                Test1("test 2") { (i: Int) => succeed },
                Test1("test 3") { (i: Int) => succeed },
                Test1("test 4") { (i: Int) => succeed },
                Test1("test 5") { (i: Int) => succeed },
                Test1("test 6") { (i: Int) => succeed }
              )
          }
          assert(suite.testNames == Set("test 1", "test 2", "test 3", "test 4", "test 5", "test 6"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 6)
          assert(testStarting(0).testName == "test 1")
          assert(testStarting(0).testText == "test 1")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 14, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "test 2")
          assert(testStarting(1).testText == "test 2")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 16, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "test 3")
          assert(testStarting(2).testText == "test 3")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 18, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(3).testName == "test 4")
          assert(testStarting(3).testText == "test 4")
          assert(testStarting(3).location == Some(LineInFile(thisLineNumber - 20, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(4).testName == "test 5")
          assert(testStarting(4).testText == "test 5")
          assert(testStarting(4).location == Some(LineInFile(thisLineNumber - 22, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(5).testName == "test 6")
          assert(testStarting(5).testText == "test 6")
          assert(testStarting(5).location == Some(LineInFile(thisLineNumber - 24, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 6)
          assert(testSucceeded(0).testName == "test 1")
          assert(testSucceeded(0).testText == "test 1")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 34, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- test 1", "test 1", 1)))
          assert(testSucceeded(1).testName == "test 2")
          assert(testSucceeded(1).testText == "test 2")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 37, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- test 2", "test 2", 1)))
          assert(testSucceeded(2).testName == "test 3")
          assert(testSucceeded(2).testText == "test 3")
          assert(testSucceeded(2).location == Some(LineInFile(thisLineNumber - 40, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(2).formatter == Some(IndentedText("- test 3", "test 3", 1)))
          assert(testSucceeded(3).testName == "test 4")
          assert(testSucceeded(3).testText == "test 4")
          assert(testSucceeded(3).location == Some(LineInFile(thisLineNumber - 43, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(3).formatter == Some(IndentedText("- test 4", "test 4", 1)))
          assert(testSucceeded(4).testName == "test 5")
          assert(testSucceeded(4).testText == "test 5")
          assert(testSucceeded(4).location == Some(LineInFile(thisLineNumber - 46, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(4).formatter == Some(IndentedText("- test 5", "test 5", 1)))
          assert(testSucceeded(5).testName == "test 6")
          assert(testSucceeded(5).testText == "test 6")
          assert(testSucceeded(5).location == Some(LineInFile(thisLineNumber - 49, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(5).formatter == Some(IndentedText("- test 6", "test 6", 1)))
        }
        it("should report 7 test succeeded events to the passed-in reporter when andThen with another 6 succeeded Test1") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              Test0("test 1")(3).andThen(
                Test1("test 2") { (i: Int) => succeed },
                Test1("test 3") { (i: Int) => succeed },
                Test1("test 4") { (i: Int) => succeed },
                Test1("test 5") { (i: Int) => succeed },
                Test1("test 6") { (i: Int) => succeed },
                Test1("test 7") { (i: Int) => succeed }
              )
          }
          assert(suite.testNames == Set("test 1", "test 2", "test 3", "test 4", "test 5", "test 6", "test 7"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 7)
          assert(testStarting(0).testName == "test 1")
          assert(testStarting(0).testText == "test 1")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 15, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "test 2")
          assert(testStarting(1).testText == "test 2")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 17, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "test 3")
          assert(testStarting(2).testText == "test 3")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 19, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(3).testName == "test 4")
          assert(testStarting(3).testText == "test 4")
          assert(testStarting(3).location == Some(LineInFile(thisLineNumber - 21, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(4).testName == "test 5")
          assert(testStarting(4).testText == "test 5")
          assert(testStarting(4).location == Some(LineInFile(thisLineNumber - 23, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(5).testName == "test 6")
          assert(testStarting(5).testText == "test 6")
          assert(testStarting(5).location == Some(LineInFile(thisLineNumber - 25, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(6).testName == "test 7")
          assert(testStarting(6).testText == "test 7")
          assert(testStarting(6).location == Some(LineInFile(thisLineNumber - 27, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 7)
          assert(testSucceeded(0).testName == "test 1")
          assert(testSucceeded(0).testText == "test 1")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 38, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- test 1", "test 1", 1)))
          assert(testSucceeded(1).testName == "test 2")
          assert(testSucceeded(1).testText == "test 2")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 41, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- test 2", "test 2", 1)))
          assert(testSucceeded(2).testName == "test 3")
          assert(testSucceeded(2).testText == "test 3")
          assert(testSucceeded(2).location == Some(LineInFile(thisLineNumber - 44, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(2).formatter == Some(IndentedText("- test 3", "test 3", 1)))
          assert(testSucceeded(3).testName == "test 4")
          assert(testSucceeded(3).testText == "test 4")
          assert(testSucceeded(3).location == Some(LineInFile(thisLineNumber - 47, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(3).formatter == Some(IndentedText("- test 4", "test 4", 1)))
          assert(testSucceeded(4).testName == "test 5")
          assert(testSucceeded(4).testText == "test 5")
          assert(testSucceeded(4).location == Some(LineInFile(thisLineNumber - 50, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(4).formatter == Some(IndentedText("- test 5", "test 5", 1)))
          assert(testSucceeded(5).testName == "test 6")
          assert(testSucceeded(5).testText == "test 6")
          assert(testSucceeded(5).location == Some(LineInFile(thisLineNumber - 53, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(5).formatter == Some(IndentedText("- test 6", "test 6", 1)))
          assert(testSucceeded(6).testName == "test 7")
          assert(testSucceeded(6).testText == "test 7")
          assert(testSucceeded(6).location == Some(LineInFile(thisLineNumber - 56, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(6).formatter == Some(IndentedText("- test 7", "test 7", 1)))
        }
        it("should report 8 test succeeded events to the passed-in reporter when andThen with another 7 succeeded Test1") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              Test0("test 1")(3).andThen(
                Test1("test 2") { (i: Int) => succeed },
                Test1("test 3") { (i: Int) => succeed },
                Test1("test 4") { (i: Int) => succeed },
                Test1("test 5") { (i: Int) => succeed },
                Test1("test 6") { (i: Int) => succeed },
                Test1("test 7") { (i: Int) => succeed },
                Test1("test 8") { (i: Int) => succeed }
              )
          }
          assert(suite.testNames == Set("test 1", "test 2", "test 3", "test 4", "test 5", "test 6", "test 7", "test 8"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 8)
          assert(testStarting(0).testName == "test 1")
          assert(testStarting(0).testText == "test 1")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 16, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "test 2")
          assert(testStarting(1).testText == "test 2")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 18, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "test 3")
          assert(testStarting(2).testText == "test 3")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 20, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(3).testName == "test 4")
          assert(testStarting(3).testText == "test 4")
          assert(testStarting(3).location == Some(LineInFile(thisLineNumber - 22, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(4).testName == "test 5")
          assert(testStarting(4).testText == "test 5")
          assert(testStarting(4).location == Some(LineInFile(thisLineNumber - 24, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(5).testName == "test 6")
          assert(testStarting(5).testText == "test 6")
          assert(testStarting(5).location == Some(LineInFile(thisLineNumber - 26, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(6).testName == "test 7")
          assert(testStarting(6).testText == "test 7")
          assert(testStarting(6).location == Some(LineInFile(thisLineNumber - 28, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(7).testName == "test 8")
          assert(testStarting(7).testText == "test 8")
          assert(testStarting(7).location == Some(LineInFile(thisLineNumber - 30, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 8)
          assert(testSucceeded(0).testName == "test 1")
          assert(testSucceeded(0).testText == "test 1")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 42, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- test 1", "test 1", 1)))
          assert(testSucceeded(1).testName == "test 2")
          assert(testSucceeded(1).testText == "test 2")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 45, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- test 2", "test 2", 1)))
          assert(testSucceeded(2).testName == "test 3")
          assert(testSucceeded(2).testText == "test 3")
          assert(testSucceeded(2).location == Some(LineInFile(thisLineNumber - 48, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(2).formatter == Some(IndentedText("- test 3", "test 3", 1)))
          assert(testSucceeded(3).testName == "test 4")
          assert(testSucceeded(3).testText == "test 4")
          assert(testSucceeded(3).location == Some(LineInFile(thisLineNumber - 51, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(3).formatter == Some(IndentedText("- test 4", "test 4", 1)))
          assert(testSucceeded(4).testName == "test 5")
          assert(testSucceeded(4).testText == "test 5")
          assert(testSucceeded(4).location == Some(LineInFile(thisLineNumber - 54, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(4).formatter == Some(IndentedText("- test 5", "test 5", 1)))
          assert(testSucceeded(5).testName == "test 6")
          assert(testSucceeded(5).testText == "test 6")
          assert(testSucceeded(5).location == Some(LineInFile(thisLineNumber - 57, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(5).formatter == Some(IndentedText("- test 6", "test 6", 1)))
          assert(testSucceeded(6).testName == "test 7")
          assert(testSucceeded(6).testText == "test 7")
          assert(testSucceeded(6).location == Some(LineInFile(thisLineNumber - 60, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(6).formatter == Some(IndentedText("- test 7", "test 7", 1)))
          assert(testSucceeded(7).testName == "test 8")
          assert(testSucceeded(7).testText == "test 8")
          assert(testSucceeded(7).location == Some(LineInFile(thisLineNumber - 63, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(7).formatter == Some(IndentedText("- test 8", "test 8", 1)))
        }
        it("should report 9 test succeeded events to the passed-in reporter when andThen with another 8 succeeded Test1") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              Test0("test 1")(3).andThen(
                Test1("test 2") { (i: Int) => succeed },
                Test1("test 3") { (i: Int) => succeed },
                Test1("test 4") { (i: Int) => succeed },
                Test1("test 5") { (i: Int) => succeed },
                Test1("test 6") { (i: Int) => succeed },
                Test1("test 7") { (i: Int) => succeed },
                Test1("test 8") { (i: Int) => succeed },
                Test1("test 9") { (i: Int) => succeed }
              )
          }
          assert(suite.testNames == Set("test 1", "test 2", "test 3", "test 4", "test 5", "test 6", "test 7", "test 8", "test 9"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 9)
          assert(testStarting(0).testName == "test 1")
          assert(testStarting(0).testText == "test 1")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 17, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "test 2")
          assert(testStarting(1).testText == "test 2")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 19, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "test 3")
          assert(testStarting(2).testText == "test 3")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 21, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(3).testName == "test 4")
          assert(testStarting(3).testText == "test 4")
          assert(testStarting(3).location == Some(LineInFile(thisLineNumber - 23, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(4).testName == "test 5")
          assert(testStarting(4).testText == "test 5")
          assert(testStarting(4).location == Some(LineInFile(thisLineNumber - 25, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(5).testName == "test 6")
          assert(testStarting(5).testText == "test 6")
          assert(testStarting(5).location == Some(LineInFile(thisLineNumber - 27, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(6).testName == "test 7")
          assert(testStarting(6).testText == "test 7")
          assert(testStarting(6).location == Some(LineInFile(thisLineNumber - 29, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(7).testName == "test 8")
          assert(testStarting(7).testText == "test 8")
          assert(testStarting(7).location == Some(LineInFile(thisLineNumber - 31, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(8).testName == "test 9")
          assert(testStarting(8).testText == "test 9")
          assert(testStarting(8).location == Some(LineInFile(thisLineNumber - 33, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 9)
          assert(testSucceeded(0).testName == "test 1")
          assert(testSucceeded(0).testText == "test 1")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 46, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- test 1", "test 1", 1)))
          assert(testSucceeded(1).testName == "test 2")
          assert(testSucceeded(1).testText == "test 2")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 49, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- test 2", "test 2", 1)))
          assert(testSucceeded(2).testName == "test 3")
          assert(testSucceeded(2).testText == "test 3")
          assert(testSucceeded(2).location == Some(LineInFile(thisLineNumber - 52, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(2).formatter == Some(IndentedText("- test 3", "test 3", 1)))
          assert(testSucceeded(3).testName == "test 4")
          assert(testSucceeded(3).testText == "test 4")
          assert(testSucceeded(3).location == Some(LineInFile(thisLineNumber - 55, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(3).formatter == Some(IndentedText("- test 4", "test 4", 1)))
          assert(testSucceeded(4).testName == "test 5")
          assert(testSucceeded(4).testText == "test 5")
          assert(testSucceeded(4).location == Some(LineInFile(thisLineNumber - 58, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(4).formatter == Some(IndentedText("- test 5", "test 5", 1)))
          assert(testSucceeded(5).testName == "test 6")
          assert(testSucceeded(5).testText == "test 6")
          assert(testSucceeded(5).location == Some(LineInFile(thisLineNumber - 61, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(5).formatter == Some(IndentedText("- test 6", "test 6", 1)))
          assert(testSucceeded(6).testName == "test 7")
          assert(testSucceeded(6).testText == "test 7")
          assert(testSucceeded(6).location == Some(LineInFile(thisLineNumber - 64, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(6).formatter == Some(IndentedText("- test 7", "test 7", 1)))
          assert(testSucceeded(7).testName == "test 8")
          assert(testSucceeded(7).testText == "test 8")
          assert(testSucceeded(7).location == Some(LineInFile(thisLineNumber - 67, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(7).formatter == Some(IndentedText("- test 8", "test 8", 1)))
          assert(testSucceeded(8).testName == "test 9")
          assert(testSucceeded(8).testText == "test 9")
          assert(testSucceeded(8).location == Some(LineInFile(thisLineNumber - 70, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(8).formatter == Some(IndentedText("- test 9", "test 9", 1)))
        }
        it("should report 10 test succeeded events to the passed-in reporter when andThen with another 9 succeeded Test1") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              Test0("test 1")(3).andThen(
                Test1("test 2") { (i: Int) => succeed },
                Test1("test 3") { (i: Int) => succeed },
                Test1("test 4") { (i: Int) => succeed },
                Test1("test 5") { (i: Int) => succeed },
                Test1("test 6") { (i: Int) => succeed },
                Test1("test 7") { (i: Int) => succeed },
                Test1("test 8") { (i: Int) => succeed },
                Test1("test 9") { (i: Int) => succeed },
                Test1("test 10") { (i: Int) => succeed }
              )
          }
          assert(suite.testNames == Set("test 1", "test 2", "test 3", "test 4", "test 5", "test 6", "test 7", "test 8", "test 9",
                                        "test 10"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 10)
          assert(testStarting(0).testName == "test 1")
          assert(testStarting(0).testText == "test 1")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 19, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "test 2")
          assert(testStarting(1).testText == "test 2")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 21, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "test 3")
          assert(testStarting(2).testText == "test 3")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 23, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(3).testName == "test 4")
          assert(testStarting(3).testText == "test 4")
          assert(testStarting(3).location == Some(LineInFile(thisLineNumber - 25, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(4).testName == "test 5")
          assert(testStarting(4).testText == "test 5")
          assert(testStarting(4).location == Some(LineInFile(thisLineNumber - 27, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(5).testName == "test 6")
          assert(testStarting(5).testText == "test 6")
          assert(testStarting(5).location == Some(LineInFile(thisLineNumber - 29, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(6).testName == "test 7")
          assert(testStarting(6).testText == "test 7")
          assert(testStarting(6).location == Some(LineInFile(thisLineNumber - 31, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(7).testName == "test 8")
          assert(testStarting(7).testText == "test 8")
          assert(testStarting(7).location == Some(LineInFile(thisLineNumber - 33, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(8).testName == "test 9")
          assert(testStarting(8).testText == "test 9")
          assert(testStarting(8).location == Some(LineInFile(thisLineNumber - 35, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(9).testName == "test 10")
          assert(testStarting(9).testText == "test 10")
          assert(testStarting(9).location == Some(LineInFile(thisLineNumber - 37, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 10)
          assert(testSucceeded(0).testName == "test 1")
          assert(testSucceeded(0).testText == "test 1")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 51, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- test 1", "test 1", 1)))
          assert(testSucceeded(1).testName == "test 2")
          assert(testSucceeded(1).testText == "test 2")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 54, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- test 2", "test 2", 1)))
          assert(testSucceeded(2).testName == "test 3")
          assert(testSucceeded(2).testText == "test 3")
          assert(testSucceeded(2).location == Some(LineInFile(thisLineNumber - 57, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(2).formatter == Some(IndentedText("- test 3", "test 3", 1)))
          assert(testSucceeded(3).testName == "test 4")
          assert(testSucceeded(3).testText == "test 4")
          assert(testSucceeded(3).location == Some(LineInFile(thisLineNumber - 60, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(3).formatter == Some(IndentedText("- test 4", "test 4", 1)))
          assert(testSucceeded(4).testName == "test 5")
          assert(testSucceeded(4).testText == "test 5")
          assert(testSucceeded(4).location == Some(LineInFile(thisLineNumber - 63, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(4).formatter == Some(IndentedText("- test 5", "test 5", 1)))
          assert(testSucceeded(5).testName == "test 6")
          assert(testSucceeded(5).testText == "test 6")
          assert(testSucceeded(5).location == Some(LineInFile(thisLineNumber - 66, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(5).formatter == Some(IndentedText("- test 6", "test 6", 1)))
          assert(testSucceeded(6).testName == "test 7")
          assert(testSucceeded(6).testText == "test 7")
          assert(testSucceeded(6).location == Some(LineInFile(thisLineNumber - 69, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(6).formatter == Some(IndentedText("- test 7", "test 7", 1)))
          assert(testSucceeded(7).testName == "test 8")
          assert(testSucceeded(7).testText == "test 8")
          assert(testSucceeded(7).location == Some(LineInFile(thisLineNumber - 72, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(7).formatter == Some(IndentedText("- test 8", "test 8", 1)))
          assert(testSucceeded(8).testName == "test 9")
          assert(testSucceeded(8).testText == "test 9")
          assert(testSucceeded(8).location == Some(LineInFile(thisLineNumber - 75, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(8).formatter == Some(IndentedText("- test 9", "test 9", 1)))
        }
        it("should report 11 test succeeded events to the passed-in reporter when andThen with another 10 succeeded Test1") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              Test0("test 1")(3).andThen(
                Test1("test 2") { (i: Int) => succeed },
                Test1("test 3") { (i: Int) => succeed },
                Test1("test 4") { (i: Int) => succeed },
                Test1("test 5") { (i: Int) => succeed },
                Test1("test 6") { (i: Int) => succeed },
                Test1("test 7") { (i: Int) => succeed },
                Test1("test 8") { (i: Int) => succeed },
                Test1("test 9") { (i: Int) => succeed },
                Test1("test 10") { (i: Int) => succeed },
                Test1("test 11") { (i: Int) => succeed }
              )
          }
          assert(suite.testNames == Set("test 1", "test 2", "test 3", "test 4", "test 5", "test 6", "test 7", "test 8", "test 9",
                                        "test 10", "test 11"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 11)
          assert(testStarting(0).testName == "test 1")
          assert(testStarting(0).testText == "test 1")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 20, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "test 2")
          assert(testStarting(1).testText == "test 2")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 22, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "test 3")
          assert(testStarting(2).testText == "test 3")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 24, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(3).testName == "test 4")
          assert(testStarting(3).testText == "test 4")
          assert(testStarting(3).location == Some(LineInFile(thisLineNumber - 26, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(4).testName == "test 5")
          assert(testStarting(4).testText == "test 5")
          assert(testStarting(4).location == Some(LineInFile(thisLineNumber - 28, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(5).testName == "test 6")
          assert(testStarting(5).testText == "test 6")
          assert(testStarting(5).location == Some(LineInFile(thisLineNumber - 30, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(6).testName == "test 7")
          assert(testStarting(6).testText == "test 7")
          assert(testStarting(6).location == Some(LineInFile(thisLineNumber - 32, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(7).testName == "test 8")
          assert(testStarting(7).testText == "test 8")
          assert(testStarting(7).location == Some(LineInFile(thisLineNumber - 34, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(8).testName == "test 9")
          assert(testStarting(8).testText == "test 9")
          assert(testStarting(8).location == Some(LineInFile(thisLineNumber - 36, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(9).testName == "test 10")
          assert(testStarting(9).testText == "test 10")
          assert(testStarting(9).location == Some(LineInFile(thisLineNumber - 38, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(10).testName == "test 11")
          assert(testStarting(10).testText == "test 11")
          assert(testStarting(10).location == Some(LineInFile(thisLineNumber - 40, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 11)
          assert(testSucceeded(0).testName == "test 1")
          assert(testSucceeded(0).testText == "test 1")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 55, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- test 1", "test 1", 1)))
          assert(testSucceeded(1).testName == "test 2")
          assert(testSucceeded(1).testText == "test 2")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 58, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- test 2", "test 2", 1)))
          assert(testSucceeded(2).testName == "test 3")
          assert(testSucceeded(2).testText == "test 3")
          assert(testSucceeded(2).location == Some(LineInFile(thisLineNumber - 61, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(2).formatter == Some(IndentedText("- test 3", "test 3", 1)))
          assert(testSucceeded(3).testName == "test 4")
          assert(testSucceeded(3).testText == "test 4")
          assert(testSucceeded(3).location == Some(LineInFile(thisLineNumber - 64, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(3).formatter == Some(IndentedText("- test 4", "test 4", 1)))
          assert(testSucceeded(4).testName == "test 5")
          assert(testSucceeded(4).testText == "test 5")
          assert(testSucceeded(4).location == Some(LineInFile(thisLineNumber - 67, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(4).formatter == Some(IndentedText("- test 5", "test 5", 1)))
          assert(testSucceeded(5).testName == "test 6")
          assert(testSucceeded(5).testText == "test 6")
          assert(testSucceeded(5).location == Some(LineInFile(thisLineNumber - 70, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(5).formatter == Some(IndentedText("- test 6", "test 6", 1)))
          assert(testSucceeded(6).testName == "test 7")
          assert(testSucceeded(6).testText == "test 7")
          assert(testSucceeded(6).location == Some(LineInFile(thisLineNumber - 73, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(6).formatter == Some(IndentedText("- test 7", "test 7", 1)))
          assert(testSucceeded(7).testName == "test 8")
          assert(testSucceeded(7).testText == "test 8")
          assert(testSucceeded(7).location == Some(LineInFile(thisLineNumber - 76, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(7).formatter == Some(IndentedText("- test 8", "test 8", 1)))
          assert(testSucceeded(8).testName == "test 9")
          assert(testSucceeded(8).testText == "test 9")
          assert(testSucceeded(8).location == Some(LineInFile(thisLineNumber - 79, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(8).formatter == Some(IndentedText("- test 9", "test 9", 1)))
          assert(testSucceeded(9).testName == "test 10")
          assert(testSucceeded(9).testText == "test 10")
          assert(testSucceeded(9).location == Some(LineInFile(thisLineNumber - 82, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(9).formatter == Some(IndentedText("- test 10", "test 10", 1)))
        }
        it("should report 12 test succeeded events to the passed-in reporter when andThen with another 11 succeeded Test1") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              Test0("test 1")(3).andThen(
                Test1("test 2") { (i: Int) => succeed },
                Test1("test 3") { (i: Int) => succeed },
                Test1("test 4") { (i: Int) => succeed },
                Test1("test 5") { (i: Int) => succeed },
                Test1("test 6") { (i: Int) => succeed },
                Test1("test 7") { (i: Int) => succeed },
                Test1("test 8") { (i: Int) => succeed },
                Test1("test 9") { (i: Int) => succeed },
                Test1("test 10") { (i: Int) => succeed },
                Test1("test 11") { (i: Int) => succeed },
                Test1("test 12") { (i: Int) => succeed }
              )
          }
          assert(suite.testNames == Set("test 1", "test 2", "test 3", "test 4", "test 5", "test 6", "test 7", "test 8", "test 9",
            "test 10", "test 11", "test 12"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 12)
          assert(testStarting(0).testName == "test 1")
          assert(testStarting(0).testText == "test 1")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 21, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "test 2")
          assert(testStarting(1).testText == "test 2")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 23, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "test 3")
          assert(testStarting(2).testText == "test 3")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 25, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(3).testName == "test 4")
          assert(testStarting(3).testText == "test 4")
          assert(testStarting(3).location == Some(LineInFile(thisLineNumber - 27, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(4).testName == "test 5")
          assert(testStarting(4).testText == "test 5")
          assert(testStarting(4).location == Some(LineInFile(thisLineNumber - 29, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(5).testName == "test 6")
          assert(testStarting(5).testText == "test 6")
          assert(testStarting(5).location == Some(LineInFile(thisLineNumber - 31, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(6).testName == "test 7")
          assert(testStarting(6).testText == "test 7")
          assert(testStarting(6).location == Some(LineInFile(thisLineNumber - 33, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(7).testName == "test 8")
          assert(testStarting(7).testText == "test 8")
          assert(testStarting(7).location == Some(LineInFile(thisLineNumber - 35, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(8).testName == "test 9")
          assert(testStarting(8).testText == "test 9")
          assert(testStarting(8).location == Some(LineInFile(thisLineNumber - 37, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(9).testName == "test 10")
          assert(testStarting(9).testText == "test 10")
          assert(testStarting(9).location == Some(LineInFile(thisLineNumber - 39, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(10).testName == "test 11")
          assert(testStarting(10).testText == "test 11")
          assert(testStarting(10).location == Some(LineInFile(thisLineNumber - 41, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(11).testName == "test 12")
          assert(testStarting(11).testText == "test 12")
          assert(testStarting(11).location == Some(LineInFile(thisLineNumber - 43, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 12)
          assert(testSucceeded(0).testName == "test 1")
          assert(testSucceeded(0).testText == "test 1")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 59, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- test 1", "test 1", 1)))
          assert(testSucceeded(1).testName == "test 2")
          assert(testSucceeded(1).testText == "test 2")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 62, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- test 2", "test 2", 1)))
          assert(testSucceeded(2).testName == "test 3")
          assert(testSucceeded(2).testText == "test 3")
          assert(testSucceeded(2).location == Some(LineInFile(thisLineNumber - 65, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(2).formatter == Some(IndentedText("- test 3", "test 3", 1)))
          assert(testSucceeded(3).testName == "test 4")
          assert(testSucceeded(3).testText == "test 4")
          assert(testSucceeded(3).location == Some(LineInFile(thisLineNumber - 68, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(3).formatter == Some(IndentedText("- test 4", "test 4", 1)))
          assert(testSucceeded(4).testName == "test 5")
          assert(testSucceeded(4).testText == "test 5")
          assert(testSucceeded(4).location == Some(LineInFile(thisLineNumber - 71, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(4).formatter == Some(IndentedText("- test 5", "test 5", 1)))
          assert(testSucceeded(5).testName == "test 6")
          assert(testSucceeded(5).testText == "test 6")
          assert(testSucceeded(5).location == Some(LineInFile(thisLineNumber - 74, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(5).formatter == Some(IndentedText("- test 6", "test 6", 1)))
          assert(testSucceeded(6).testName == "test 7")
          assert(testSucceeded(6).testText == "test 7")
          assert(testSucceeded(6).location == Some(LineInFile(thisLineNumber - 77, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(6).formatter == Some(IndentedText("- test 7", "test 7", 1)))
          assert(testSucceeded(7).testName == "test 8")
          assert(testSucceeded(7).testText == "test 8")
          assert(testSucceeded(7).location == Some(LineInFile(thisLineNumber - 80, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(7).formatter == Some(IndentedText("- test 8", "test 8", 1)))
          assert(testSucceeded(8).testName == "test 9")
          assert(testSucceeded(8).testText == "test 9")
          assert(testSucceeded(8).location == Some(LineInFile(thisLineNumber - 83, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(8).formatter == Some(IndentedText("- test 9", "test 9", 1)))
          assert(testSucceeded(9).testName == "test 10")
          assert(testSucceeded(9).testText == "test 10")
          assert(testSucceeded(9).location == Some(LineInFile(thisLineNumber - 86, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(9).formatter == Some(IndentedText("- test 10", "test 10", 1)))
          assert(testSucceeded(10).testName == "test 11")
          assert(testSucceeded(10).testText == "test 11")
          assert(testSucceeded(10).location == Some(LineInFile(thisLineNumber - 89, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(10).formatter == Some(IndentedText("- test 11", "test 11", 1)))
          assert(testSucceeded(11).testName == "test 12")
          assert(testSucceeded(11).testText == "test 12")
          assert(testSucceeded(11).location == Some(LineInFile(thisLineNumber - 92, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(11).formatter == Some(IndentedText("- test 12", "test 12", 1)))
        }
        it("should report 13 test succeeded events to the passed-in reporter when andThen with another 12 succeeded Test1") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              Test0("test 1")(3).andThen(
                Test1("test 2") { (i: Int) => succeed },
                Test1("test 3") { (i: Int) => succeed },
                Test1("test 4") { (i: Int) => succeed },
                Test1("test 5") { (i: Int) => succeed },
                Test1("test 6") { (i: Int) => succeed },
                Test1("test 7") { (i: Int) => succeed },
                Test1("test 8") { (i: Int) => succeed },
                Test1("test 9") { (i: Int) => succeed },
                Test1("test 10") { (i: Int) => succeed },
                Test1("test 11") { (i: Int) => succeed },
                Test1("test 12") { (i: Int) => succeed },
                Test1("test 13") { (i: Int) => succeed }
              )
          }
          assert(suite.testNames == Set("test 1", "test 2", "test 3", "test 4", "test 5", "test 6", "test 7", "test 8", "test 9",
            "test 10", "test 11", "test 12", "test 13"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 13)
          assert(testStarting(0).testName == "test 1")
          assert(testStarting(0).testText == "test 1")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 22, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "test 2")
          assert(testStarting(1).testText == "test 2")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 24, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "test 3")
          assert(testStarting(2).testText == "test 3")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 26, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(3).testName == "test 4")
          assert(testStarting(3).testText == "test 4")
          assert(testStarting(3).location == Some(LineInFile(thisLineNumber - 28, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(4).testName == "test 5")
          assert(testStarting(4).testText == "test 5")
          assert(testStarting(4).location == Some(LineInFile(thisLineNumber - 30, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(5).testName == "test 6")
          assert(testStarting(5).testText == "test 6")
          assert(testStarting(5).location == Some(LineInFile(thisLineNumber - 32, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(6).testName == "test 7")
          assert(testStarting(6).testText == "test 7")
          assert(testStarting(6).location == Some(LineInFile(thisLineNumber - 34, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(7).testName == "test 8")
          assert(testStarting(7).testText == "test 8")
          assert(testStarting(7).location == Some(LineInFile(thisLineNumber - 36, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(8).testName == "test 9")
          assert(testStarting(8).testText == "test 9")
          assert(testStarting(8).location == Some(LineInFile(thisLineNumber - 38, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(9).testName == "test 10")
          assert(testStarting(9).testText == "test 10")
          assert(testStarting(9).location == Some(LineInFile(thisLineNumber - 40, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(10).testName == "test 11")
          assert(testStarting(10).testText == "test 11")
          assert(testStarting(10).location == Some(LineInFile(thisLineNumber - 42, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(11).testName == "test 12")
          assert(testStarting(11).testText == "test 12")
          assert(testStarting(11).location == Some(LineInFile(thisLineNumber - 44, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(12).testName == "test 13")
          assert(testStarting(12).testText == "test 13")
          assert(testStarting(12).location == Some(LineInFile(thisLineNumber - 46, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 13)
          assert(testSucceeded(0).testName == "test 1")
          assert(testSucceeded(0).testText == "test 1")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 63, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- test 1", "test 1", 1)))
          assert(testSucceeded(1).testName == "test 2")
          assert(testSucceeded(1).testText == "test 2")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 66, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- test 2", "test 2", 1)))
          assert(testSucceeded(2).testName == "test 3")
          assert(testSucceeded(2).testText == "test 3")
          assert(testSucceeded(2).location == Some(LineInFile(thisLineNumber - 69, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(2).formatter == Some(IndentedText("- test 3", "test 3", 1)))
          assert(testSucceeded(3).testName == "test 4")
          assert(testSucceeded(3).testText == "test 4")
          assert(testSucceeded(3).location == Some(LineInFile(thisLineNumber - 72, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(3).formatter == Some(IndentedText("- test 4", "test 4", 1)))
          assert(testSucceeded(4).testName == "test 5")
          assert(testSucceeded(4).testText == "test 5")
          assert(testSucceeded(4).location == Some(LineInFile(thisLineNumber - 75, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(4).formatter == Some(IndentedText("- test 5", "test 5", 1)))
          assert(testSucceeded(5).testName == "test 6")
          assert(testSucceeded(5).testText == "test 6")
          assert(testSucceeded(5).location == Some(LineInFile(thisLineNumber - 78, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(5).formatter == Some(IndentedText("- test 6", "test 6", 1)))
          assert(testSucceeded(6).testName == "test 7")
          assert(testSucceeded(6).testText == "test 7")
          assert(testSucceeded(6).location == Some(LineInFile(thisLineNumber - 81, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(6).formatter == Some(IndentedText("- test 7", "test 7", 1)))
          assert(testSucceeded(7).testName == "test 8")
          assert(testSucceeded(7).testText == "test 8")
          assert(testSucceeded(7).location == Some(LineInFile(thisLineNumber - 84, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(7).formatter == Some(IndentedText("- test 8", "test 8", 1)))
          assert(testSucceeded(8).testName == "test 9")
          assert(testSucceeded(8).testText == "test 9")
          assert(testSucceeded(8).location == Some(LineInFile(thisLineNumber - 87, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(8).formatter == Some(IndentedText("- test 9", "test 9", 1)))
          assert(testSucceeded(9).testName == "test 10")
          assert(testSucceeded(9).testText == "test 10")
          assert(testSucceeded(9).location == Some(LineInFile(thisLineNumber - 90, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(9).formatter == Some(IndentedText("- test 10", "test 10", 1)))
          assert(testSucceeded(10).testName == "test 11")
          assert(testSucceeded(10).testText == "test 11")
          assert(testSucceeded(10).location == Some(LineInFile(thisLineNumber - 93, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(10).formatter == Some(IndentedText("- test 11", "test 11", 1)))
          assert(testSucceeded(11).testName == "test 12")
          assert(testSucceeded(11).testText == "test 12")
          assert(testSucceeded(11).location == Some(LineInFile(thisLineNumber - 96, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(11).formatter == Some(IndentedText("- test 12", "test 12", 1)))
          assert(testSucceeded(12).testName == "test 13")
          assert(testSucceeded(12).testText == "test 13")
          assert(testSucceeded(12).location == Some(LineInFile(thisLineNumber - 99, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(12).formatter == Some(IndentedText("- test 13", "test 13", 1)))
        }
        it("should report 14 test succeeded events to the passed-in reporter when andThen with another 13 succeeded Test1") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              Test0("test 1")(3).andThen(
                Test1("test 2") { (i: Int) => succeed },
                Test1("test 3") { (i: Int) => succeed },
                Test1("test 4") { (i: Int) => succeed },
                Test1("test 5") { (i: Int) => succeed },
                Test1("test 6") { (i: Int) => succeed },
                Test1("test 7") { (i: Int) => succeed },
                Test1("test 8") { (i: Int) => succeed },
                Test1("test 9") { (i: Int) => succeed },
                Test1("test 10") { (i: Int) => succeed },
                Test1("test 11") { (i: Int) => succeed },
                Test1("test 12") { (i: Int) => succeed },
                Test1("test 13") { (i: Int) => succeed },
                Test1("test 14") { (i: Int) => succeed }
              )
          }
          assert(suite.testNames == Set("test 1", "test 2", "test 3", "test 4", "test 5", "test 6", "test 7", "test 8", "test 9",
            "test 10", "test 11", "test 12", "test 13", "test 14"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 14)
          assert(testStarting(0).testName == "test 1")
          assert(testStarting(0).testText == "test 1")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 23, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "test 2")
          assert(testStarting(1).testText == "test 2")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 25, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "test 3")
          assert(testStarting(2).testText == "test 3")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 27, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(3).testName == "test 4")
          assert(testStarting(3).testText == "test 4")
          assert(testStarting(3).location == Some(LineInFile(thisLineNumber - 29, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(4).testName == "test 5")
          assert(testStarting(4).testText == "test 5")
          assert(testStarting(4).location == Some(LineInFile(thisLineNumber - 31, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(5).testName == "test 6")
          assert(testStarting(5).testText == "test 6")
          assert(testStarting(5).location == Some(LineInFile(thisLineNumber - 33, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(6).testName == "test 7")
          assert(testStarting(6).testText == "test 7")
          assert(testStarting(6).location == Some(LineInFile(thisLineNumber - 35, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(7).testName == "test 8")
          assert(testStarting(7).testText == "test 8")
          assert(testStarting(7).location == Some(LineInFile(thisLineNumber - 37, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(8).testName == "test 9")
          assert(testStarting(8).testText == "test 9")
          assert(testStarting(8).location == Some(LineInFile(thisLineNumber - 39, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(9).testName == "test 10")
          assert(testStarting(9).testText == "test 10")
          assert(testStarting(9).location == Some(LineInFile(thisLineNumber - 41, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(10).testName == "test 11")
          assert(testStarting(10).testText == "test 11")
          assert(testStarting(10).location == Some(LineInFile(thisLineNumber - 43, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(11).testName == "test 12")
          assert(testStarting(11).testText == "test 12")
          assert(testStarting(11).location == Some(LineInFile(thisLineNumber - 45, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(12).testName == "test 13")
          assert(testStarting(12).testText == "test 13")
          assert(testStarting(12).location == Some(LineInFile(thisLineNumber - 47, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(13).testName == "test 14")
          assert(testStarting(13).testText == "test 14")
          assert(testStarting(13).location == Some(LineInFile(thisLineNumber - 49, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 14)
          assert(testSucceeded(0).testName == "test 1")
          assert(testSucceeded(0).testText == "test 1")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 67, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- test 1", "test 1", 1)))
          assert(testSucceeded(1).testName == "test 2")
          assert(testSucceeded(1).testText == "test 2")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 70, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- test 2", "test 2", 1)))
          assert(testSucceeded(2).testName == "test 3")
          assert(testSucceeded(2).testText == "test 3")
          assert(testSucceeded(2).location == Some(LineInFile(thisLineNumber - 73, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(2).formatter == Some(IndentedText("- test 3", "test 3", 1)))
          assert(testSucceeded(3).testName == "test 4")
          assert(testSucceeded(3).testText == "test 4")
          assert(testSucceeded(3).location == Some(LineInFile(thisLineNumber - 76, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(3).formatter == Some(IndentedText("- test 4", "test 4", 1)))
          assert(testSucceeded(4).testName == "test 5")
          assert(testSucceeded(4).testText == "test 5")
          assert(testSucceeded(4).location == Some(LineInFile(thisLineNumber - 79, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(4).formatter == Some(IndentedText("- test 5", "test 5", 1)))
          assert(testSucceeded(5).testName == "test 6")
          assert(testSucceeded(5).testText == "test 6")
          assert(testSucceeded(5).location == Some(LineInFile(thisLineNumber - 82, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(5).formatter == Some(IndentedText("- test 6", "test 6", 1)))
          assert(testSucceeded(6).testName == "test 7")
          assert(testSucceeded(6).testText == "test 7")
          assert(testSucceeded(6).location == Some(LineInFile(thisLineNumber - 85, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(6).formatter == Some(IndentedText("- test 7", "test 7", 1)))
          assert(testSucceeded(7).testName == "test 8")
          assert(testSucceeded(7).testText == "test 8")
          assert(testSucceeded(7).location == Some(LineInFile(thisLineNumber - 88, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(7).formatter == Some(IndentedText("- test 8", "test 8", 1)))
          assert(testSucceeded(8).testName == "test 9")
          assert(testSucceeded(8).testText == "test 9")
          assert(testSucceeded(8).location == Some(LineInFile(thisLineNumber - 91, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(8).formatter == Some(IndentedText("- test 9", "test 9", 1)))
          assert(testSucceeded(9).testName == "test 10")
          assert(testSucceeded(9).testText == "test 10")
          assert(testSucceeded(9).location == Some(LineInFile(thisLineNumber - 94, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(9).formatter == Some(IndentedText("- test 10", "test 10", 1)))
          assert(testSucceeded(10).testName == "test 11")
          assert(testSucceeded(10).testText == "test 11")
          assert(testSucceeded(10).location == Some(LineInFile(thisLineNumber - 97, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(10).formatter == Some(IndentedText("- test 11", "test 11", 1)))
          assert(testSucceeded(11).testName == "test 12")
          assert(testSucceeded(11).testText == "test 12")
          assert(testSucceeded(11).location == Some(LineInFile(thisLineNumber - 100, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(11).formatter == Some(IndentedText("- test 12", "test 12", 1)))
          assert(testSucceeded(12).testName == "test 13")
          assert(testSucceeded(12).testText == "test 13")
          assert(testSucceeded(12).location == Some(LineInFile(thisLineNumber - 103, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(12).formatter == Some(IndentedText("- test 13", "test 13", 1)))
          assert(testSucceeded(13).testName == "test 14")
          assert(testSucceeded(13).testText == "test 14")
          assert(testSucceeded(13).location == Some(LineInFile(thisLineNumber - 106, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(13).formatter == Some(IndentedText("- test 14", "test 14", 1)))
        }
        it("should report 15 test succeeded events to the passed-in reporter when andThen with another 14 succeeded Test1") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              Test0("test 1")(3).andThen(
                Test1("test 2") { (i: Int) => succeed },
                Test1("test 3") { (i: Int) => succeed },
                Test1("test 4") { (i: Int) => succeed },
                Test1("test 5") { (i: Int) => succeed },
                Test1("test 6") { (i: Int) => succeed },
                Test1("test 7") { (i: Int) => succeed },
                Test1("test 8") { (i: Int) => succeed },
                Test1("test 9") { (i: Int) => succeed },
                Test1("test 10") { (i: Int) => succeed },
                Test1("test 11") { (i: Int) => succeed },
                Test1("test 12") { (i: Int) => succeed },
                Test1("test 13") { (i: Int) => succeed },
                Test1("test 14") { (i: Int) => succeed },
                Test1("test 15") { (i: Int) => succeed }
              )
          }
          assert(suite.testNames == Set("test 1", "test 2", "test 3", "test 4", "test 5", "test 6", "test 7", "test 8", "test 9",
            "test 10", "test 11", "test 12", "test 13", "test 14", "test 15"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 15)
          assert(testStarting(0).testName == "test 1")
          assert(testStarting(0).testText == "test 1")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 24, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "test 2")
          assert(testStarting(1).testText == "test 2")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 26, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "test 3")
          assert(testStarting(2).testText == "test 3")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 28, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(3).testName == "test 4")
          assert(testStarting(3).testText == "test 4")
          assert(testStarting(3).location == Some(LineInFile(thisLineNumber - 30, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(4).testName == "test 5")
          assert(testStarting(4).testText == "test 5")
          assert(testStarting(4).location == Some(LineInFile(thisLineNumber - 32, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(5).testName == "test 6")
          assert(testStarting(5).testText == "test 6")
          assert(testStarting(5).location == Some(LineInFile(thisLineNumber - 34, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(6).testName == "test 7")
          assert(testStarting(6).testText == "test 7")
          assert(testStarting(6).location == Some(LineInFile(thisLineNumber - 36, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(7).testName == "test 8")
          assert(testStarting(7).testText == "test 8")
          assert(testStarting(7).location == Some(LineInFile(thisLineNumber - 38, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(8).testName == "test 9")
          assert(testStarting(8).testText == "test 9")
          assert(testStarting(8).location == Some(LineInFile(thisLineNumber - 40, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(9).testName == "test 10")
          assert(testStarting(9).testText == "test 10")
          assert(testStarting(9).location == Some(LineInFile(thisLineNumber - 42, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(10).testName == "test 11")
          assert(testStarting(10).testText == "test 11")
          assert(testStarting(10).location == Some(LineInFile(thisLineNumber - 44, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(11).testName == "test 12")
          assert(testStarting(11).testText == "test 12")
          assert(testStarting(11).location == Some(LineInFile(thisLineNumber - 46, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(12).testName == "test 13")
          assert(testStarting(12).testText == "test 13")
          assert(testStarting(12).location == Some(LineInFile(thisLineNumber - 48, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(13).testName == "test 14")
          assert(testStarting(13).testText == "test 14")
          assert(testStarting(13).location == Some(LineInFile(thisLineNumber - 50, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(14).testName == "test 15")
          assert(testStarting(14).testText == "test 15")
          assert(testStarting(14).location == Some(LineInFile(thisLineNumber - 52, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 15)
          assert(testSucceeded(0).testName == "test 1")
          assert(testSucceeded(0).testText == "test 1")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 71, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- test 1", "test 1", 1)))
          assert(testSucceeded(1).testName == "test 2")
          assert(testSucceeded(1).testText == "test 2")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 74, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- test 2", "test 2", 1)))
          assert(testSucceeded(2).testName == "test 3")
          assert(testSucceeded(2).testText == "test 3")
          assert(testSucceeded(2).location == Some(LineInFile(thisLineNumber - 77, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(2).formatter == Some(IndentedText("- test 3", "test 3", 1)))
          assert(testSucceeded(3).testName == "test 4")
          assert(testSucceeded(3).testText == "test 4")
          assert(testSucceeded(3).location == Some(LineInFile(thisLineNumber - 80, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(3).formatter == Some(IndentedText("- test 4", "test 4", 1)))
          assert(testSucceeded(4).testName == "test 5")
          assert(testSucceeded(4).testText == "test 5")
          assert(testSucceeded(4).location == Some(LineInFile(thisLineNumber - 83, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(4).formatter == Some(IndentedText("- test 5", "test 5", 1)))
          assert(testSucceeded(5).testName == "test 6")
          assert(testSucceeded(5).testText == "test 6")
          assert(testSucceeded(5).location == Some(LineInFile(thisLineNumber - 86, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(5).formatter == Some(IndentedText("- test 6", "test 6", 1)))
          assert(testSucceeded(6).testName == "test 7")
          assert(testSucceeded(6).testText == "test 7")
          assert(testSucceeded(6).location == Some(LineInFile(thisLineNumber - 89, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(6).formatter == Some(IndentedText("- test 7", "test 7", 1)))
          assert(testSucceeded(7).testName == "test 8")
          assert(testSucceeded(7).testText == "test 8")
          assert(testSucceeded(7).location == Some(LineInFile(thisLineNumber - 92, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(7).formatter == Some(IndentedText("- test 8", "test 8", 1)))
          assert(testSucceeded(8).testName == "test 9")
          assert(testSucceeded(8).testText == "test 9")
          assert(testSucceeded(8).location == Some(LineInFile(thisLineNumber - 95, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(8).formatter == Some(IndentedText("- test 9", "test 9", 1)))
          assert(testSucceeded(9).testName == "test 10")
          assert(testSucceeded(9).testText == "test 10")
          assert(testSucceeded(9).location == Some(LineInFile(thisLineNumber - 98, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(9).formatter == Some(IndentedText("- test 10", "test 10", 1)))
          assert(testSucceeded(10).testName == "test 11")
          assert(testSucceeded(10).testText == "test 11")
          assert(testSucceeded(10).location == Some(LineInFile(thisLineNumber - 101, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(10).formatter == Some(IndentedText("- test 11", "test 11", 1)))
          assert(testSucceeded(11).testName == "test 12")
          assert(testSucceeded(11).testText == "test 12")
          assert(testSucceeded(11).location == Some(LineInFile(thisLineNumber - 104, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(11).formatter == Some(IndentedText("- test 12", "test 12", 1)))
          assert(testSucceeded(12).testName == "test 13")
          assert(testSucceeded(12).testText == "test 13")
          assert(testSucceeded(12).location == Some(LineInFile(thisLineNumber - 107, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(12).formatter == Some(IndentedText("- test 13", "test 13", 1)))
          assert(testSucceeded(13).testName == "test 14")
          assert(testSucceeded(13).testText == "test 14")
          assert(testSucceeded(13).location == Some(LineInFile(thisLineNumber - 110, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(13).formatter == Some(IndentedText("- test 14", "test 14", 1)))
          assert(testSucceeded(14).testName == "test 15")
          assert(testSucceeded(14).testText == "test 15")
          assert(testSucceeded(14).location == Some(LineInFile(thisLineNumber - 113, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(14).formatter == Some(IndentedText("- test 15", "test 15", 1)))
        }
        it("should report 16 test succeeded events to the passed-in reporter when andThen with another 15 succeeded Test1") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              Test0("test 1")(3).andThen(
                Test1("test 2") { (i: Int) => succeed },
                Test1("test 3") { (i: Int) => succeed },
                Test1("test 4") { (i: Int) => succeed },
                Test1("test 5") { (i: Int) => succeed },
                Test1("test 6") { (i: Int) => succeed },
                Test1("test 7") { (i: Int) => succeed },
                Test1("test 8") { (i: Int) => succeed },
                Test1("test 9") { (i: Int) => succeed },
                Test1("test 10") { (i: Int) => succeed },
                Test1("test 11") { (i: Int) => succeed },
                Test1("test 12") { (i: Int) => succeed },
                Test1("test 13") { (i: Int) => succeed },
                Test1("test 14") { (i: Int) => succeed },
                Test1("test 15") { (i: Int) => succeed },
                Test1("test 16") { (i: Int) => succeed }
              )
          }
          assert(suite.testNames == Set("test 1", "test 2", "test 3", "test 4", "test 5", "test 6", "test 7", "test 8", "test 9",
            "test 10", "test 11", "test 12", "test 13", "test 14", "test 15", "test 16"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 16)
          assert(testStarting(0).testName == "test 1")
          assert(testStarting(0).testText == "test 1")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 25, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "test 2")
          assert(testStarting(1).testText == "test 2")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 27, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "test 3")
          assert(testStarting(2).testText == "test 3")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 29, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(3).testName == "test 4")
          assert(testStarting(3).testText == "test 4")
          assert(testStarting(3).location == Some(LineInFile(thisLineNumber - 31, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(4).testName == "test 5")
          assert(testStarting(4).testText == "test 5")
          assert(testStarting(4).location == Some(LineInFile(thisLineNumber - 33, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(5).testName == "test 6")
          assert(testStarting(5).testText == "test 6")
          assert(testStarting(5).location == Some(LineInFile(thisLineNumber - 35, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(6).testName == "test 7")
          assert(testStarting(6).testText == "test 7")
          assert(testStarting(6).location == Some(LineInFile(thisLineNumber - 37, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(7).testName == "test 8")
          assert(testStarting(7).testText == "test 8")
          assert(testStarting(7).location == Some(LineInFile(thisLineNumber - 39, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(8).testName == "test 9")
          assert(testStarting(8).testText == "test 9")
          assert(testStarting(8).location == Some(LineInFile(thisLineNumber - 41, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(9).testName == "test 10")
          assert(testStarting(9).testText == "test 10")
          assert(testStarting(9).location == Some(LineInFile(thisLineNumber - 43, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(10).testName == "test 11")
          assert(testStarting(10).testText == "test 11")
          assert(testStarting(10).location == Some(LineInFile(thisLineNumber - 45, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(11).testName == "test 12")
          assert(testStarting(11).testText == "test 12")
          assert(testStarting(11).location == Some(LineInFile(thisLineNumber - 47, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(12).testName == "test 13")
          assert(testStarting(12).testText == "test 13")
          assert(testStarting(12).location == Some(LineInFile(thisLineNumber - 49, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(13).testName == "test 14")
          assert(testStarting(13).testText == "test 14")
          assert(testStarting(13).location == Some(LineInFile(thisLineNumber - 51, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(14).testName == "test 15")
          assert(testStarting(14).testText == "test 15")
          assert(testStarting(14).location == Some(LineInFile(thisLineNumber - 53, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(15).testName == "test 16")
          assert(testStarting(15).testText == "test 16")
          assert(testStarting(15).location == Some(LineInFile(thisLineNumber - 55, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 16)
          assert(testSucceeded(0).testName == "test 1")
          assert(testSucceeded(0).testText == "test 1")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 75, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- test 1", "test 1", 1)))
          assert(testSucceeded(1).testName == "test 2")
          assert(testSucceeded(1).testText == "test 2")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 78, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- test 2", "test 2", 1)))
          assert(testSucceeded(2).testName == "test 3")
          assert(testSucceeded(2).testText == "test 3")
          assert(testSucceeded(2).location == Some(LineInFile(thisLineNumber - 81, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(2).formatter == Some(IndentedText("- test 3", "test 3", 1)))
          assert(testSucceeded(3).testName == "test 4")
          assert(testSucceeded(3).testText == "test 4")
          assert(testSucceeded(3).location == Some(LineInFile(thisLineNumber - 84, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(3).formatter == Some(IndentedText("- test 4", "test 4", 1)))
          assert(testSucceeded(4).testName == "test 5")
          assert(testSucceeded(4).testText == "test 5")
          assert(testSucceeded(4).location == Some(LineInFile(thisLineNumber - 87, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(4).formatter == Some(IndentedText("- test 5", "test 5", 1)))
          assert(testSucceeded(5).testName == "test 6")
          assert(testSucceeded(5).testText == "test 6")
          assert(testSucceeded(5).location == Some(LineInFile(thisLineNumber - 90, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(5).formatter == Some(IndentedText("- test 6", "test 6", 1)))
          assert(testSucceeded(6).testName == "test 7")
          assert(testSucceeded(6).testText == "test 7")
          assert(testSucceeded(6).location == Some(LineInFile(thisLineNumber - 93, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(6).formatter == Some(IndentedText("- test 7", "test 7", 1)))
          assert(testSucceeded(7).testName == "test 8")
          assert(testSucceeded(7).testText == "test 8")
          assert(testSucceeded(7).location == Some(LineInFile(thisLineNumber - 96, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(7).formatter == Some(IndentedText("- test 8", "test 8", 1)))
          assert(testSucceeded(8).testName == "test 9")
          assert(testSucceeded(8).testText == "test 9")
          assert(testSucceeded(8).location == Some(LineInFile(thisLineNumber - 99, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(8).formatter == Some(IndentedText("- test 9", "test 9", 1)))
          assert(testSucceeded(9).testName == "test 10")
          assert(testSucceeded(9).testText == "test 10")
          assert(testSucceeded(9).location == Some(LineInFile(thisLineNumber - 102, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(9).formatter == Some(IndentedText("- test 10", "test 10", 1)))
          assert(testSucceeded(10).testName == "test 11")
          assert(testSucceeded(10).testText == "test 11")
          assert(testSucceeded(10).location == Some(LineInFile(thisLineNumber - 105, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(10).formatter == Some(IndentedText("- test 11", "test 11", 1)))
          assert(testSucceeded(11).testName == "test 12")
          assert(testSucceeded(11).testText == "test 12")
          assert(testSucceeded(11).location == Some(LineInFile(thisLineNumber - 108, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(11).formatter == Some(IndentedText("- test 12", "test 12", 1)))
          assert(testSucceeded(12).testName == "test 13")
          assert(testSucceeded(12).testText == "test 13")
          assert(testSucceeded(12).location == Some(LineInFile(thisLineNumber - 111, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(12).formatter == Some(IndentedText("- test 13", "test 13", 1)))
          assert(testSucceeded(13).testName == "test 14")
          assert(testSucceeded(13).testText == "test 14")
          assert(testSucceeded(13).location == Some(LineInFile(thisLineNumber - 114, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(13).formatter == Some(IndentedText("- test 14", "test 14", 1)))
          assert(testSucceeded(14).testName == "test 15")
          assert(testSucceeded(14).testText == "test 15")
          assert(testSucceeded(14).location == Some(LineInFile(thisLineNumber - 117, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(14).formatter == Some(IndentedText("- test 15", "test 15", 1)))
          assert(testSucceeded(15).testName == "test 16")
          assert(testSucceeded(15).testText == "test 16")
          assert(testSucceeded(15).location == Some(LineInFile(thisLineNumber - 120, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(15).formatter == Some(IndentedText("- test 16", "test 16", 1)))
        }
        it("should report 17 test succeeded events to the passed-in reporter when andThen with another 16 succeeded Test1") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              Test0("test 1")(3).andThen(
                Test1("test 2") { (i: Int) => succeed },
                Test1("test 3") { (i: Int) => succeed },
                Test1("test 4") { (i: Int) => succeed },
                Test1("test 5") { (i: Int) => succeed },
                Test1("test 6") { (i: Int) => succeed },
                Test1("test 7") { (i: Int) => succeed },
                Test1("test 8") { (i: Int) => succeed },
                Test1("test 9") { (i: Int) => succeed },
                Test1("test 10") { (i: Int) => succeed },
                Test1("test 11") { (i: Int) => succeed },
                Test1("test 12") { (i: Int) => succeed },
                Test1("test 13") { (i: Int) => succeed },
                Test1("test 14") { (i: Int) => succeed },
                Test1("test 15") { (i: Int) => succeed },
                Test1("test 16") { (i: Int) => succeed },
                Test1("test 17") { (i: Int) => succeed }
              )
          }
          assert(suite.testNames == Set("test 1", "test 2", "test 3", "test 4", "test 5", "test 6", "test 7", "test 8", "test 9",
            "test 10", "test 11", "test 12", "test 13", "test 14", "test 15", "test 16", "test 17"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 17)
          assert(testStarting(0).testName == "test 1")
          assert(testStarting(0).testText == "test 1")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 26, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "test 2")
          assert(testStarting(1).testText == "test 2")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 28, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "test 3")
          assert(testStarting(2).testText == "test 3")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 30, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(3).testName == "test 4")
          assert(testStarting(3).testText == "test 4")
          assert(testStarting(3).location == Some(LineInFile(thisLineNumber - 32, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(4).testName == "test 5")
          assert(testStarting(4).testText == "test 5")
          assert(testStarting(4).location == Some(LineInFile(thisLineNumber - 34, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(5).testName == "test 6")
          assert(testStarting(5).testText == "test 6")
          assert(testStarting(5).location == Some(LineInFile(thisLineNumber - 36, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(6).testName == "test 7")
          assert(testStarting(6).testText == "test 7")
          assert(testStarting(6).location == Some(LineInFile(thisLineNumber - 38, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(7).testName == "test 8")
          assert(testStarting(7).testText == "test 8")
          assert(testStarting(7).location == Some(LineInFile(thisLineNumber - 40, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(8).testName == "test 9")
          assert(testStarting(8).testText == "test 9")
          assert(testStarting(8).location == Some(LineInFile(thisLineNumber - 42, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(9).testName == "test 10")
          assert(testStarting(9).testText == "test 10")
          assert(testStarting(9).location == Some(LineInFile(thisLineNumber - 44, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(10).testName == "test 11")
          assert(testStarting(10).testText == "test 11")
          assert(testStarting(10).location == Some(LineInFile(thisLineNumber - 46, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(11).testName == "test 12")
          assert(testStarting(11).testText == "test 12")
          assert(testStarting(11).location == Some(LineInFile(thisLineNumber - 48, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(12).testName == "test 13")
          assert(testStarting(12).testText == "test 13")
          assert(testStarting(12).location == Some(LineInFile(thisLineNumber - 50, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(13).testName == "test 14")
          assert(testStarting(13).testText == "test 14")
          assert(testStarting(13).location == Some(LineInFile(thisLineNumber - 52, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(14).testName == "test 15")
          assert(testStarting(14).testText == "test 15")
          assert(testStarting(14).location == Some(LineInFile(thisLineNumber - 54, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(15).testName == "test 16")
          assert(testStarting(15).testText == "test 16")
          assert(testStarting(15).location == Some(LineInFile(thisLineNumber - 56, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(16).testName == "test 17")
          assert(testStarting(16).testText == "test 17")
          assert(testStarting(16).location == Some(LineInFile(thisLineNumber - 58, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 17)
          assert(testSucceeded(0).testName == "test 1")
          assert(testSucceeded(0).testText == "test 1")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 79, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- test 1", "test 1", 1)))
          assert(testSucceeded(1).testName == "test 2")
          assert(testSucceeded(1).testText == "test 2")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 82, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- test 2", "test 2", 1)))
          assert(testSucceeded(2).testName == "test 3")
          assert(testSucceeded(2).testText == "test 3")
          assert(testSucceeded(2).location == Some(LineInFile(thisLineNumber - 85, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(2).formatter == Some(IndentedText("- test 3", "test 3", 1)))
          assert(testSucceeded(3).testName == "test 4")
          assert(testSucceeded(3).testText == "test 4")
          assert(testSucceeded(3).location == Some(LineInFile(thisLineNumber - 88, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(3).formatter == Some(IndentedText("- test 4", "test 4", 1)))
          assert(testSucceeded(4).testName == "test 5")
          assert(testSucceeded(4).testText == "test 5")
          assert(testSucceeded(4).location == Some(LineInFile(thisLineNumber - 91, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(4).formatter == Some(IndentedText("- test 5", "test 5", 1)))
          assert(testSucceeded(5).testName == "test 6")
          assert(testSucceeded(5).testText == "test 6")
          assert(testSucceeded(5).location == Some(LineInFile(thisLineNumber - 94, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(5).formatter == Some(IndentedText("- test 6", "test 6", 1)))
          assert(testSucceeded(6).testName == "test 7")
          assert(testSucceeded(6).testText == "test 7")
          assert(testSucceeded(6).location == Some(LineInFile(thisLineNumber - 97, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(6).formatter == Some(IndentedText("- test 7", "test 7", 1)))
          assert(testSucceeded(7).testName == "test 8")
          assert(testSucceeded(7).testText == "test 8")
          assert(testSucceeded(7).location == Some(LineInFile(thisLineNumber - 100, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(7).formatter == Some(IndentedText("- test 8", "test 8", 1)))
          assert(testSucceeded(8).testName == "test 9")
          assert(testSucceeded(8).testText == "test 9")
          assert(testSucceeded(8).location == Some(LineInFile(thisLineNumber - 103, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(8).formatter == Some(IndentedText("- test 9", "test 9", 1)))
          assert(testSucceeded(9).testName == "test 10")
          assert(testSucceeded(9).testText == "test 10")
          assert(testSucceeded(9).location == Some(LineInFile(thisLineNumber - 106, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(9).formatter == Some(IndentedText("- test 10", "test 10", 1)))
          assert(testSucceeded(10).testName == "test 11")
          assert(testSucceeded(10).testText == "test 11")
          assert(testSucceeded(10).location == Some(LineInFile(thisLineNumber - 109, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(10).formatter == Some(IndentedText("- test 11", "test 11", 1)))
          assert(testSucceeded(11).testName == "test 12")
          assert(testSucceeded(11).testText == "test 12")
          assert(testSucceeded(11).location == Some(LineInFile(thisLineNumber - 112, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(11).formatter == Some(IndentedText("- test 12", "test 12", 1)))
          assert(testSucceeded(12).testName == "test 13")
          assert(testSucceeded(12).testText == "test 13")
          assert(testSucceeded(12).location == Some(LineInFile(thisLineNumber - 115, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(12).formatter == Some(IndentedText("- test 13", "test 13", 1)))
          assert(testSucceeded(13).testName == "test 14")
          assert(testSucceeded(13).testText == "test 14")
          assert(testSucceeded(13).location == Some(LineInFile(thisLineNumber - 118, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(13).formatter == Some(IndentedText("- test 14", "test 14", 1)))
          assert(testSucceeded(14).testName == "test 15")
          assert(testSucceeded(14).testText == "test 15")
          assert(testSucceeded(14).location == Some(LineInFile(thisLineNumber - 121, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(14).formatter == Some(IndentedText("- test 15", "test 15", 1)))
          assert(testSucceeded(15).testName == "test 16")
          assert(testSucceeded(15).testText == "test 16")
          assert(testSucceeded(15).location == Some(LineInFile(thisLineNumber - 124, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(15).formatter == Some(IndentedText("- test 16", "test 16", 1)))
          assert(testSucceeded(16).testName == "test 17")
          assert(testSucceeded(16).testText == "test 17")
          assert(testSucceeded(16).location == Some(LineInFile(thisLineNumber - 127, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(16).formatter == Some(IndentedText("- test 17", "test 17", 1)))
        }
        it("should report 18 test succeeded events to the passed-in reporter when andThen with another 17 succeeded Test1") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              Test0("test 1")(3).andThen(
                Test1("test 2") { (i: Int) => succeed },
                Test1("test 3") { (i: Int) => succeed },
                Test1("test 4") { (i: Int) => succeed },
                Test1("test 5") { (i: Int) => succeed },
                Test1("test 6") { (i: Int) => succeed },
                Test1("test 7") { (i: Int) => succeed },
                Test1("test 8") { (i: Int) => succeed },
                Test1("test 9") { (i: Int) => succeed },
                Test1("test 10") { (i: Int) => succeed },
                Test1("test 11") { (i: Int) => succeed },
                Test1("test 12") { (i: Int) => succeed },
                Test1("test 13") { (i: Int) => succeed },
                Test1("test 14") { (i: Int) => succeed },
                Test1("test 15") { (i: Int) => succeed },
                Test1("test 16") { (i: Int) => succeed },
                Test1("test 17") { (i: Int) => succeed },
                Test1("test 18") { (i: Int) => succeed }
              )
          }
          assert(suite.testNames == Set("test 1", "test 2", "test 3", "test 4", "test 5", "test 6", "test 7", "test 8", "test 9",
            "test 10", "test 11", "test 12", "test 13", "test 14", "test 15", "test 16", "test 17", "test 18"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 18)
          assert(testStarting(0).testName == "test 1")
          assert(testStarting(0).testText == "test 1")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 27, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "test 2")
          assert(testStarting(1).testText == "test 2")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 29, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "test 3")
          assert(testStarting(2).testText == "test 3")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 31, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(3).testName == "test 4")
          assert(testStarting(3).testText == "test 4")
          assert(testStarting(3).location == Some(LineInFile(thisLineNumber - 33, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(4).testName == "test 5")
          assert(testStarting(4).testText == "test 5")
          assert(testStarting(4).location == Some(LineInFile(thisLineNumber - 35, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(5).testName == "test 6")
          assert(testStarting(5).testText == "test 6")
          assert(testStarting(5).location == Some(LineInFile(thisLineNumber - 37, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(6).testName == "test 7")
          assert(testStarting(6).testText == "test 7")
          assert(testStarting(6).location == Some(LineInFile(thisLineNumber - 39, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(7).testName == "test 8")
          assert(testStarting(7).testText == "test 8")
          assert(testStarting(7).location == Some(LineInFile(thisLineNumber - 41, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(8).testName == "test 9")
          assert(testStarting(8).testText == "test 9")
          assert(testStarting(8).location == Some(LineInFile(thisLineNumber - 43, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(9).testName == "test 10")
          assert(testStarting(9).testText == "test 10")
          assert(testStarting(9).location == Some(LineInFile(thisLineNumber - 45, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(10).testName == "test 11")
          assert(testStarting(10).testText == "test 11")
          assert(testStarting(10).location == Some(LineInFile(thisLineNumber - 47, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(11).testName == "test 12")
          assert(testStarting(11).testText == "test 12")
          assert(testStarting(11).location == Some(LineInFile(thisLineNumber - 49, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(12).testName == "test 13")
          assert(testStarting(12).testText == "test 13")
          assert(testStarting(12).location == Some(LineInFile(thisLineNumber - 51, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(13).testName == "test 14")
          assert(testStarting(13).testText == "test 14")
          assert(testStarting(13).location == Some(LineInFile(thisLineNumber - 53, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(14).testName == "test 15")
          assert(testStarting(14).testText == "test 15")
          assert(testStarting(14).location == Some(LineInFile(thisLineNumber - 55, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(15).testName == "test 16")
          assert(testStarting(15).testText == "test 16")
          assert(testStarting(15).location == Some(LineInFile(thisLineNumber - 57, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(16).testName == "test 17")
          assert(testStarting(16).testText == "test 17")
          assert(testStarting(16).location == Some(LineInFile(thisLineNumber - 59, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(17).testName == "test 18")
          assert(testStarting(17).testText == "test 18")
          assert(testStarting(17).location == Some(LineInFile(thisLineNumber - 61, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 18)
          assert(testSucceeded(0).testName == "test 1")
          assert(testSucceeded(0).testText == "test 1")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 83, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- test 1", "test 1", 1)))
          assert(testSucceeded(1).testName == "test 2")
          assert(testSucceeded(1).testText == "test 2")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 86, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- test 2", "test 2", 1)))
          assert(testSucceeded(2).testName == "test 3")
          assert(testSucceeded(2).testText == "test 3")
          assert(testSucceeded(2).location == Some(LineInFile(thisLineNumber - 89, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(2).formatter == Some(IndentedText("- test 3", "test 3", 1)))
          assert(testSucceeded(3).testName == "test 4")
          assert(testSucceeded(3).testText == "test 4")
          assert(testSucceeded(3).location == Some(LineInFile(thisLineNumber - 92, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(3).formatter == Some(IndentedText("- test 4", "test 4", 1)))
          assert(testSucceeded(4).testName == "test 5")
          assert(testSucceeded(4).testText == "test 5")
          assert(testSucceeded(4).location == Some(LineInFile(thisLineNumber - 95, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(4).formatter == Some(IndentedText("- test 5", "test 5", 1)))
          assert(testSucceeded(5).testName == "test 6")
          assert(testSucceeded(5).testText == "test 6")
          assert(testSucceeded(5).location == Some(LineInFile(thisLineNumber - 98, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(5).formatter == Some(IndentedText("- test 6", "test 6", 1)))
          assert(testSucceeded(6).testName == "test 7")
          assert(testSucceeded(6).testText == "test 7")
          assert(testSucceeded(6).location == Some(LineInFile(thisLineNumber - 101, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(6).formatter == Some(IndentedText("- test 7", "test 7", 1)))
          assert(testSucceeded(7).testName == "test 8")
          assert(testSucceeded(7).testText == "test 8")
          assert(testSucceeded(7).location == Some(LineInFile(thisLineNumber - 104, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(7).formatter == Some(IndentedText("- test 8", "test 8", 1)))
          assert(testSucceeded(8).testName == "test 9")
          assert(testSucceeded(8).testText == "test 9")
          assert(testSucceeded(8).location == Some(LineInFile(thisLineNumber - 107, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(8).formatter == Some(IndentedText("- test 9", "test 9", 1)))
          assert(testSucceeded(9).testName == "test 10")
          assert(testSucceeded(9).testText == "test 10")
          assert(testSucceeded(9).location == Some(LineInFile(thisLineNumber - 110, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(9).formatter == Some(IndentedText("- test 10", "test 10", 1)))
          assert(testSucceeded(10).testName == "test 11")
          assert(testSucceeded(10).testText == "test 11")
          assert(testSucceeded(10).location == Some(LineInFile(thisLineNumber - 113, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(10).formatter == Some(IndentedText("- test 11", "test 11", 1)))
          assert(testSucceeded(11).testName == "test 12")
          assert(testSucceeded(11).testText == "test 12")
          assert(testSucceeded(11).location == Some(LineInFile(thisLineNumber - 116, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(11).formatter == Some(IndentedText("- test 12", "test 12", 1)))
          assert(testSucceeded(12).testName == "test 13")
          assert(testSucceeded(12).testText == "test 13")
          assert(testSucceeded(12).location == Some(LineInFile(thisLineNumber - 119, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(12).formatter == Some(IndentedText("- test 13", "test 13", 1)))
          assert(testSucceeded(13).testName == "test 14")
          assert(testSucceeded(13).testText == "test 14")
          assert(testSucceeded(13).location == Some(LineInFile(thisLineNumber - 122, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(13).formatter == Some(IndentedText("- test 14", "test 14", 1)))
          assert(testSucceeded(14).testName == "test 15")
          assert(testSucceeded(14).testText == "test 15")
          assert(testSucceeded(14).location == Some(LineInFile(thisLineNumber - 125, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(14).formatter == Some(IndentedText("- test 15", "test 15", 1)))
          assert(testSucceeded(15).testName == "test 16")
          assert(testSucceeded(15).testText == "test 16")
          assert(testSucceeded(15).location == Some(LineInFile(thisLineNumber - 128, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(15).formatter == Some(IndentedText("- test 16", "test 16", 1)))
          assert(testSucceeded(16).testName == "test 17")
          assert(testSucceeded(16).testText == "test 17")
          assert(testSucceeded(16).location == Some(LineInFile(thisLineNumber - 131, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(16).formatter == Some(IndentedText("- test 17", "test 17", 1)))
          assert(testSucceeded(17).testName == "test 18")
          assert(testSucceeded(17).testText == "test 18")
          assert(testSucceeded(17).location == Some(LineInFile(thisLineNumber - 134, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(17).formatter == Some(IndentedText("- test 18", "test 18", 1)))
        }
        it("should report 19 test succeeded events to the passed-in reporter when andThen with another 18 succeeded Test1") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              Test0("test 1")(3).andThen(
                Test1("test 2") { (i: Int) => succeed },
                Test1("test 3") { (i: Int) => succeed },
                Test1("test 4") { (i: Int) => succeed },
                Test1("test 5") { (i: Int) => succeed },
                Test1("test 6") { (i: Int) => succeed },
                Test1("test 7") { (i: Int) => succeed },
                Test1("test 8") { (i: Int) => succeed },
                Test1("test 9") { (i: Int) => succeed },
                Test1("test 10") { (i: Int) => succeed },
                Test1("test 11") { (i: Int) => succeed },
                Test1("test 12") { (i: Int) => succeed },
                Test1("test 13") { (i: Int) => succeed },
                Test1("test 14") { (i: Int) => succeed },
                Test1("test 15") { (i: Int) => succeed },
                Test1("test 16") { (i: Int) => succeed },
                Test1("test 17") { (i: Int) => succeed },
                Test1("test 18") { (i: Int) => succeed },
                Test1("test 19") { (i: Int) => succeed }
              )
          }
          assert(suite.testNames == Set("test 1", "test 2", "test 3", "test 4", "test 5", "test 6", "test 7", "test 8", "test 9",
            "test 10", "test 11", "test 12", "test 13", "test 14", "test 15", "test 16", "test 17", "test 18", "test 19"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 19)
          assert(testStarting(0).testName == "test 1")
          assert(testStarting(0).testText == "test 1")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 28, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "test 2")
          assert(testStarting(1).testText == "test 2")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 30, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "test 3")
          assert(testStarting(2).testText == "test 3")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 32, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(3).testName == "test 4")
          assert(testStarting(3).testText == "test 4")
          assert(testStarting(3).location == Some(LineInFile(thisLineNumber - 34, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(4).testName == "test 5")
          assert(testStarting(4).testText == "test 5")
          assert(testStarting(4).location == Some(LineInFile(thisLineNumber - 36, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(5).testName == "test 6")
          assert(testStarting(5).testText == "test 6")
          assert(testStarting(5).location == Some(LineInFile(thisLineNumber - 38, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(6).testName == "test 7")
          assert(testStarting(6).testText == "test 7")
          assert(testStarting(6).location == Some(LineInFile(thisLineNumber - 40, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(7).testName == "test 8")
          assert(testStarting(7).testText == "test 8")
          assert(testStarting(7).location == Some(LineInFile(thisLineNumber - 42, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(8).testName == "test 9")
          assert(testStarting(8).testText == "test 9")
          assert(testStarting(8).location == Some(LineInFile(thisLineNumber - 44, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(9).testName == "test 10")
          assert(testStarting(9).testText == "test 10")
          assert(testStarting(9).location == Some(LineInFile(thisLineNumber - 46, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(10).testName == "test 11")
          assert(testStarting(10).testText == "test 11")
          assert(testStarting(10).location == Some(LineInFile(thisLineNumber - 48, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(11).testName == "test 12")
          assert(testStarting(11).testText == "test 12")
          assert(testStarting(11).location == Some(LineInFile(thisLineNumber - 50, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(12).testName == "test 13")
          assert(testStarting(12).testText == "test 13")
          assert(testStarting(12).location == Some(LineInFile(thisLineNumber - 52, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(13).testName == "test 14")
          assert(testStarting(13).testText == "test 14")
          assert(testStarting(13).location == Some(LineInFile(thisLineNumber - 54, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(14).testName == "test 15")
          assert(testStarting(14).testText == "test 15")
          assert(testStarting(14).location == Some(LineInFile(thisLineNumber - 56, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(15).testName == "test 16")
          assert(testStarting(15).testText == "test 16")
          assert(testStarting(15).location == Some(LineInFile(thisLineNumber - 58, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(16).testName == "test 17")
          assert(testStarting(16).testText == "test 17")
          assert(testStarting(16).location == Some(LineInFile(thisLineNumber - 60, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(17).testName == "test 18")
          assert(testStarting(17).testText == "test 18")
          assert(testStarting(17).location == Some(LineInFile(thisLineNumber - 62, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(18).testName == "test 19")
          assert(testStarting(18).testText == "test 19")
          assert(testStarting(18).location == Some(LineInFile(thisLineNumber - 64, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 19)
          assert(testSucceeded(0).testName == "test 1")
          assert(testSucceeded(0).testText == "test 1")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 87, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- test 1", "test 1", 1)))
          assert(testSucceeded(1).testName == "test 2")
          assert(testSucceeded(1).testText == "test 2")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 90, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- test 2", "test 2", 1)))
          assert(testSucceeded(2).testName == "test 3")
          assert(testSucceeded(2).testText == "test 3")
          assert(testSucceeded(2).location == Some(LineInFile(thisLineNumber - 93, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(2).formatter == Some(IndentedText("- test 3", "test 3", 1)))
          assert(testSucceeded(3).testName == "test 4")
          assert(testSucceeded(3).testText == "test 4")
          assert(testSucceeded(3).location == Some(LineInFile(thisLineNumber - 96, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(3).formatter == Some(IndentedText("- test 4", "test 4", 1)))
          assert(testSucceeded(4).testName == "test 5")
          assert(testSucceeded(4).testText == "test 5")
          assert(testSucceeded(4).location == Some(LineInFile(thisLineNumber - 99, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(4).formatter == Some(IndentedText("- test 5", "test 5", 1)))
          assert(testSucceeded(5).testName == "test 6")
          assert(testSucceeded(5).testText == "test 6")
          assert(testSucceeded(5).location == Some(LineInFile(thisLineNumber - 102, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(5).formatter == Some(IndentedText("- test 6", "test 6", 1)))
          assert(testSucceeded(6).testName == "test 7")
          assert(testSucceeded(6).testText == "test 7")
          assert(testSucceeded(6).location == Some(LineInFile(thisLineNumber - 105, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(6).formatter == Some(IndentedText("- test 7", "test 7", 1)))
          assert(testSucceeded(7).testName == "test 8")
          assert(testSucceeded(7).testText == "test 8")
          assert(testSucceeded(7).location == Some(LineInFile(thisLineNumber - 108, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(7).formatter == Some(IndentedText("- test 8", "test 8", 1)))
          assert(testSucceeded(8).testName == "test 9")
          assert(testSucceeded(8).testText == "test 9")
          assert(testSucceeded(8).location == Some(LineInFile(thisLineNumber - 111, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(8).formatter == Some(IndentedText("- test 9", "test 9", 1)))
          assert(testSucceeded(9).testName == "test 10")
          assert(testSucceeded(9).testText == "test 10")
          assert(testSucceeded(9).location == Some(LineInFile(thisLineNumber - 114, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(9).formatter == Some(IndentedText("- test 10", "test 10", 1)))
          assert(testSucceeded(10).testName == "test 11")
          assert(testSucceeded(10).testText == "test 11")
          assert(testSucceeded(10).location == Some(LineInFile(thisLineNumber - 117, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(10).formatter == Some(IndentedText("- test 11", "test 11", 1)))
          assert(testSucceeded(11).testName == "test 12")
          assert(testSucceeded(11).testText == "test 12")
          assert(testSucceeded(11).location == Some(LineInFile(thisLineNumber - 120, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(11).formatter == Some(IndentedText("- test 12", "test 12", 1)))
          assert(testSucceeded(12).testName == "test 13")
          assert(testSucceeded(12).testText == "test 13")
          assert(testSucceeded(12).location == Some(LineInFile(thisLineNumber - 123, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(12).formatter == Some(IndentedText("- test 13", "test 13", 1)))
          assert(testSucceeded(13).testName == "test 14")
          assert(testSucceeded(13).testText == "test 14")
          assert(testSucceeded(13).location == Some(LineInFile(thisLineNumber - 126, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(13).formatter == Some(IndentedText("- test 14", "test 14", 1)))
          assert(testSucceeded(14).testName == "test 15")
          assert(testSucceeded(14).testText == "test 15")
          assert(testSucceeded(14).location == Some(LineInFile(thisLineNumber - 129, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(14).formatter == Some(IndentedText("- test 15", "test 15", 1)))
          assert(testSucceeded(15).testName == "test 16")
          assert(testSucceeded(15).testText == "test 16")
          assert(testSucceeded(15).location == Some(LineInFile(thisLineNumber - 132, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(15).formatter == Some(IndentedText("- test 16", "test 16", 1)))
          assert(testSucceeded(16).testName == "test 17")
          assert(testSucceeded(16).testText == "test 17")
          assert(testSucceeded(16).location == Some(LineInFile(thisLineNumber - 135, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(16).formatter == Some(IndentedText("- test 17", "test 17", 1)))
          assert(testSucceeded(17).testName == "test 18")
          assert(testSucceeded(17).testText == "test 18")
          assert(testSucceeded(17).location == Some(LineInFile(thisLineNumber - 138, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(17).formatter == Some(IndentedText("- test 18", "test 18", 1)))
          assert(testSucceeded(18).testName == "test 19")
          assert(testSucceeded(18).testText == "test 19")
          assert(testSucceeded(18).location == Some(LineInFile(thisLineNumber - 141, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(18).formatter == Some(IndentedText("- test 19", "test 19", 1)))
        }
        it("should report 20 test succeeded events to the passed-in reporter when andThen with another 19 succeeded Test1") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              Test0("test 1")(3).andThen(
                Test1("test 2") { (i: Int) => succeed },
                Test1("test 3") { (i: Int) => succeed },
                Test1("test 4") { (i: Int) => succeed },
                Test1("test 5") { (i: Int) => succeed },
                Test1("test 6") { (i: Int) => succeed },
                Test1("test 7") { (i: Int) => succeed },
                Test1("test 8") { (i: Int) => succeed },
                Test1("test 9") { (i: Int) => succeed },
                Test1("test 10") { (i: Int) => succeed },
                Test1("test 11") { (i: Int) => succeed },
                Test1("test 12") { (i: Int) => succeed },
                Test1("test 13") { (i: Int) => succeed },
                Test1("test 14") { (i: Int) => succeed },
                Test1("test 15") { (i: Int) => succeed },
                Test1("test 16") { (i: Int) => succeed },
                Test1("test 17") { (i: Int) => succeed },
                Test1("test 18") { (i: Int) => succeed },
                Test1("test 19") { (i: Int) => succeed },
                Test1("test 20") { (i: Int) => succeed }
              )
          }
          assert(suite.testNames == Set("test 1", "test 2", "test 3", "test 4", "test 5", "test 6", "test 7", "test 8", "test 9",
            "test 10", "test 11", "test 12", "test 13", "test 14", "test 15", "test 16", "test 17", "test 18", "test 19", "test 20"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 20)
          assert(testStarting(0).testName == "test 1")
          assert(testStarting(0).testText == "test 1")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 29, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "test 2")
          assert(testStarting(1).testText == "test 2")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 31, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "test 3")
          assert(testStarting(2).testText == "test 3")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 33, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(3).testName == "test 4")
          assert(testStarting(3).testText == "test 4")
          assert(testStarting(3).location == Some(LineInFile(thisLineNumber - 35, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(4).testName == "test 5")
          assert(testStarting(4).testText == "test 5")
          assert(testStarting(4).location == Some(LineInFile(thisLineNumber - 37, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(5).testName == "test 6")
          assert(testStarting(5).testText == "test 6")
          assert(testStarting(5).location == Some(LineInFile(thisLineNumber - 39, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(6).testName == "test 7")
          assert(testStarting(6).testText == "test 7")
          assert(testStarting(6).location == Some(LineInFile(thisLineNumber - 41, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(7).testName == "test 8")
          assert(testStarting(7).testText == "test 8")
          assert(testStarting(7).location == Some(LineInFile(thisLineNumber - 43, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(8).testName == "test 9")
          assert(testStarting(8).testText == "test 9")
          assert(testStarting(8).location == Some(LineInFile(thisLineNumber - 45, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(9).testName == "test 10")
          assert(testStarting(9).testText == "test 10")
          assert(testStarting(9).location == Some(LineInFile(thisLineNumber - 47, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(10).testName == "test 11")
          assert(testStarting(10).testText == "test 11")
          assert(testStarting(10).location == Some(LineInFile(thisLineNumber - 49, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(11).testName == "test 12")
          assert(testStarting(11).testText == "test 12")
          assert(testStarting(11).location == Some(LineInFile(thisLineNumber - 51, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(12).testName == "test 13")
          assert(testStarting(12).testText == "test 13")
          assert(testStarting(12).location == Some(LineInFile(thisLineNumber - 53, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(13).testName == "test 14")
          assert(testStarting(13).testText == "test 14")
          assert(testStarting(13).location == Some(LineInFile(thisLineNumber - 55, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(14).testName == "test 15")
          assert(testStarting(14).testText == "test 15")
          assert(testStarting(14).location == Some(LineInFile(thisLineNumber - 57, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(15).testName == "test 16")
          assert(testStarting(15).testText == "test 16")
          assert(testStarting(15).location == Some(LineInFile(thisLineNumber - 59, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(16).testName == "test 17")
          assert(testStarting(16).testText == "test 17")
          assert(testStarting(16).location == Some(LineInFile(thisLineNumber - 61, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(17).testName == "test 18")
          assert(testStarting(17).testText == "test 18")
          assert(testStarting(17).location == Some(LineInFile(thisLineNumber - 63, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(18).testName == "test 19")
          assert(testStarting(18).testText == "test 19")
          assert(testStarting(18).location == Some(LineInFile(thisLineNumber - 65, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(19).testName == "test 20")
          assert(testStarting(19).testText == "test 20")
          assert(testStarting(19).location == Some(LineInFile(thisLineNumber - 67, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 20)
          assert(testSucceeded(0).testName == "test 1")
          assert(testSucceeded(0).testText == "test 1")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 91, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- test 1", "test 1", 1)))
          assert(testSucceeded(1).testName == "test 2")
          assert(testSucceeded(1).testText == "test 2")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 94, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- test 2", "test 2", 1)))
          assert(testSucceeded(2).testName == "test 3")
          assert(testSucceeded(2).testText == "test 3")
          assert(testSucceeded(2).location == Some(LineInFile(thisLineNumber - 97, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(2).formatter == Some(IndentedText("- test 3", "test 3", 1)))
          assert(testSucceeded(3).testName == "test 4")
          assert(testSucceeded(3).testText == "test 4")
          assert(testSucceeded(3).location == Some(LineInFile(thisLineNumber - 100, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(3).formatter == Some(IndentedText("- test 4", "test 4", 1)))
          assert(testSucceeded(4).testName == "test 5")
          assert(testSucceeded(4).testText == "test 5")
          assert(testSucceeded(4).location == Some(LineInFile(thisLineNumber - 103, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(4).formatter == Some(IndentedText("- test 5", "test 5", 1)))
          assert(testSucceeded(5).testName == "test 6")
          assert(testSucceeded(5).testText == "test 6")
          assert(testSucceeded(5).location == Some(LineInFile(thisLineNumber - 106, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(5).formatter == Some(IndentedText("- test 6", "test 6", 1)))
          assert(testSucceeded(6).testName == "test 7")
          assert(testSucceeded(6).testText == "test 7")
          assert(testSucceeded(6).location == Some(LineInFile(thisLineNumber - 109, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(6).formatter == Some(IndentedText("- test 7", "test 7", 1)))
          assert(testSucceeded(7).testName == "test 8")
          assert(testSucceeded(7).testText == "test 8")
          assert(testSucceeded(7).location == Some(LineInFile(thisLineNumber - 112, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(7).formatter == Some(IndentedText("- test 8", "test 8", 1)))
          assert(testSucceeded(8).testName == "test 9")
          assert(testSucceeded(8).testText == "test 9")
          assert(testSucceeded(8).location == Some(LineInFile(thisLineNumber - 115, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(8).formatter == Some(IndentedText("- test 9", "test 9", 1)))
          assert(testSucceeded(9).testName == "test 10")
          assert(testSucceeded(9).testText == "test 10")
          assert(testSucceeded(9).location == Some(LineInFile(thisLineNumber - 118, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(9).formatter == Some(IndentedText("- test 10", "test 10", 1)))
          assert(testSucceeded(10).testName == "test 11")
          assert(testSucceeded(10).testText == "test 11")
          assert(testSucceeded(10).location == Some(LineInFile(thisLineNumber - 121, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(10).formatter == Some(IndentedText("- test 11", "test 11", 1)))
          assert(testSucceeded(11).testName == "test 12")
          assert(testSucceeded(11).testText == "test 12")
          assert(testSucceeded(11).location == Some(LineInFile(thisLineNumber - 124, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(11).formatter == Some(IndentedText("- test 12", "test 12", 1)))
          assert(testSucceeded(12).testName == "test 13")
          assert(testSucceeded(12).testText == "test 13")
          assert(testSucceeded(12).location == Some(LineInFile(thisLineNumber - 127, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(12).formatter == Some(IndentedText("- test 13", "test 13", 1)))
          assert(testSucceeded(13).testName == "test 14")
          assert(testSucceeded(13).testText == "test 14")
          assert(testSucceeded(13).location == Some(LineInFile(thisLineNumber - 130, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(13).formatter == Some(IndentedText("- test 14", "test 14", 1)))
          assert(testSucceeded(14).testName == "test 15")
          assert(testSucceeded(14).testText == "test 15")
          assert(testSucceeded(14).location == Some(LineInFile(thisLineNumber - 133, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(14).formatter == Some(IndentedText("- test 15", "test 15", 1)))
          assert(testSucceeded(15).testName == "test 16")
          assert(testSucceeded(15).testText == "test 16")
          assert(testSucceeded(15).location == Some(LineInFile(thisLineNumber - 136, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(15).formatter == Some(IndentedText("- test 16", "test 16", 1)))
          assert(testSucceeded(16).testName == "test 17")
          assert(testSucceeded(16).testText == "test 17")
          assert(testSucceeded(16).location == Some(LineInFile(thisLineNumber - 139, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(16).formatter == Some(IndentedText("- test 17", "test 17", 1)))
          assert(testSucceeded(17).testName == "test 18")
          assert(testSucceeded(17).testText == "test 18")
          assert(testSucceeded(17).location == Some(LineInFile(thisLineNumber - 142, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(17).formatter == Some(IndentedText("- test 18", "test 18", 1)))
          assert(testSucceeded(18).testName == "test 19")
          assert(testSucceeded(18).testText == "test 19")
          assert(testSucceeded(18).location == Some(LineInFile(thisLineNumber - 145, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(18).formatter == Some(IndentedText("- test 19", "test 19", 1)))
          assert(testSucceeded(19).testName == "test 20")
          assert(testSucceeded(19).testText == "test 20")
          assert(testSucceeded(19).location == Some(LineInFile(thisLineNumber - 148, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(19).formatter == Some(IndentedText("- test 20", "test 20", 1)))
        }
        it("should report 21 test succeeded events to the passed-in reporter when andThen with another 20 succeeded Test1") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              Test0("test 1")(3).andThen(
                Test1("test 2") { (i: Int) => succeed },
                Test1("test 3") { (i: Int) => succeed },
                Test1("test 4") { (i: Int) => succeed },
                Test1("test 5") { (i: Int) => succeed },
                Test1("test 6") { (i: Int) => succeed },
                Test1("test 7") { (i: Int) => succeed },
                Test1("test 8") { (i: Int) => succeed },
                Test1("test 9") { (i: Int) => succeed },
                Test1("test 10") { (i: Int) => succeed },
                Test1("test 11") { (i: Int) => succeed },
                Test1("test 12") { (i: Int) => succeed },
                Test1("test 13") { (i: Int) => succeed },
                Test1("test 14") { (i: Int) => succeed },
                Test1("test 15") { (i: Int) => succeed },
                Test1("test 16") { (i: Int) => succeed },
                Test1("test 17") { (i: Int) => succeed },
                Test1("test 18") { (i: Int) => succeed },
                Test1("test 19") { (i: Int) => succeed },
                Test1("test 20") { (i: Int) => succeed },
                Test1("test 21") { (i: Int) => succeed }
              )
          }
          assert(suite.testNames == Set("test 1", "test 2", "test 3", "test 4", "test 5", "test 6", "test 7", "test 8", "test 9",
            "test 10", "test 11", "test 12", "test 13", "test 14", "test 15", "test 16", "test 17", "test 18", "test 19", "test 20",
            "test 21"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 21)
          assert(testStarting(0).testName == "test 1")
          assert(testStarting(0).testText == "test 1")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 31, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "test 2")
          assert(testStarting(1).testText == "test 2")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 33, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "test 3")
          assert(testStarting(2).testText == "test 3")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 35, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(3).testName == "test 4")
          assert(testStarting(3).testText == "test 4")
          assert(testStarting(3).location == Some(LineInFile(thisLineNumber - 37, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(4).testName == "test 5")
          assert(testStarting(4).testText == "test 5")
          assert(testStarting(4).location == Some(LineInFile(thisLineNumber - 39, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(5).testName == "test 6")
          assert(testStarting(5).testText == "test 6")
          assert(testStarting(5).location == Some(LineInFile(thisLineNumber - 41, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(6).testName == "test 7")
          assert(testStarting(6).testText == "test 7")
          assert(testStarting(6).location == Some(LineInFile(thisLineNumber - 43, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(7).testName == "test 8")
          assert(testStarting(7).testText == "test 8")
          assert(testStarting(7).location == Some(LineInFile(thisLineNumber - 45, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(8).testName == "test 9")
          assert(testStarting(8).testText == "test 9")
          assert(testStarting(8).location == Some(LineInFile(thisLineNumber - 47, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(9).testName == "test 10")
          assert(testStarting(9).testText == "test 10")
          assert(testStarting(9).location == Some(LineInFile(thisLineNumber - 49, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(10).testName == "test 11")
          assert(testStarting(10).testText == "test 11")
          assert(testStarting(10).location == Some(LineInFile(thisLineNumber - 51, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(11).testName == "test 12")
          assert(testStarting(11).testText == "test 12")
          assert(testStarting(11).location == Some(LineInFile(thisLineNumber - 53, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(12).testName == "test 13")
          assert(testStarting(12).testText == "test 13")
          assert(testStarting(12).location == Some(LineInFile(thisLineNumber - 55, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(13).testName == "test 14")
          assert(testStarting(13).testText == "test 14")
          assert(testStarting(13).location == Some(LineInFile(thisLineNumber - 57, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(14).testName == "test 15")
          assert(testStarting(14).testText == "test 15")
          assert(testStarting(14).location == Some(LineInFile(thisLineNumber - 59, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(15).testName == "test 16")
          assert(testStarting(15).testText == "test 16")
          assert(testStarting(15).location == Some(LineInFile(thisLineNumber - 61, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(16).testName == "test 17")
          assert(testStarting(16).testText == "test 17")
          assert(testStarting(16).location == Some(LineInFile(thisLineNumber - 63, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(17).testName == "test 18")
          assert(testStarting(17).testText == "test 18")
          assert(testStarting(17).location == Some(LineInFile(thisLineNumber - 65, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(18).testName == "test 19")
          assert(testStarting(18).testText == "test 19")
          assert(testStarting(18).location == Some(LineInFile(thisLineNumber - 67, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(19).testName == "test 20")
          assert(testStarting(19).testText == "test 20")
          assert(testStarting(19).location == Some(LineInFile(thisLineNumber - 69, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(20).testName == "test 21")
          assert(testStarting(20).testText == "test 21")
          assert(testStarting(20).location == Some(LineInFile(thisLineNumber - 71, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 21)
          assert(testSucceeded(0).testName == "test 1")
          assert(testSucceeded(0).testText == "test 1")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 96, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- test 1", "test 1", 1)))
          assert(testSucceeded(1).testName == "test 2")
          assert(testSucceeded(1).testText == "test 2")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 99, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- test 2", "test 2", 1)))
          assert(testSucceeded(2).testName == "test 3")
          assert(testSucceeded(2).testText == "test 3")
          assert(testSucceeded(2).location == Some(LineInFile(thisLineNumber - 102, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(2).formatter == Some(IndentedText("- test 3", "test 3", 1)))
          assert(testSucceeded(3).testName == "test 4")
          assert(testSucceeded(3).testText == "test 4")
          assert(testSucceeded(3).location == Some(LineInFile(thisLineNumber - 105, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(3).formatter == Some(IndentedText("- test 4", "test 4", 1)))
          assert(testSucceeded(4).testName == "test 5")
          assert(testSucceeded(4).testText == "test 5")
          assert(testSucceeded(4).location == Some(LineInFile(thisLineNumber - 108, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(4).formatter == Some(IndentedText("- test 5", "test 5", 1)))
          assert(testSucceeded(5).testName == "test 6")
          assert(testSucceeded(5).testText == "test 6")
          assert(testSucceeded(5).location == Some(LineInFile(thisLineNumber - 111, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(5).formatter == Some(IndentedText("- test 6", "test 6", 1)))
          assert(testSucceeded(6).testName == "test 7")
          assert(testSucceeded(6).testText == "test 7")
          assert(testSucceeded(6).location == Some(LineInFile(thisLineNumber - 114, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(6).formatter == Some(IndentedText("- test 7", "test 7", 1)))
          assert(testSucceeded(7).testName == "test 8")
          assert(testSucceeded(7).testText == "test 8")
          assert(testSucceeded(7).location == Some(LineInFile(thisLineNumber - 117, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(7).formatter == Some(IndentedText("- test 8", "test 8", 1)))
          assert(testSucceeded(8).testName == "test 9")
          assert(testSucceeded(8).testText == "test 9")
          assert(testSucceeded(8).location == Some(LineInFile(thisLineNumber - 120, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(8).formatter == Some(IndentedText("- test 9", "test 9", 1)))
          assert(testSucceeded(9).testName == "test 10")
          assert(testSucceeded(9).testText == "test 10")
          assert(testSucceeded(9).location == Some(LineInFile(thisLineNumber - 123, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(9).formatter == Some(IndentedText("- test 10", "test 10", 1)))
          assert(testSucceeded(10).testName == "test 11")
          assert(testSucceeded(10).testText == "test 11")
          assert(testSucceeded(10).location == Some(LineInFile(thisLineNumber - 126, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(10).formatter == Some(IndentedText("- test 11", "test 11", 1)))
          assert(testSucceeded(11).testName == "test 12")
          assert(testSucceeded(11).testText == "test 12")
          assert(testSucceeded(11).location == Some(LineInFile(thisLineNumber - 129, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(11).formatter == Some(IndentedText("- test 12", "test 12", 1)))
          assert(testSucceeded(12).testName == "test 13")
          assert(testSucceeded(12).testText == "test 13")
          assert(testSucceeded(12).location == Some(LineInFile(thisLineNumber - 132, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(12).formatter == Some(IndentedText("- test 13", "test 13", 1)))
          assert(testSucceeded(13).testName == "test 14")
          assert(testSucceeded(13).testText == "test 14")
          assert(testSucceeded(13).location == Some(LineInFile(thisLineNumber - 135, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(13).formatter == Some(IndentedText("- test 14", "test 14", 1)))
          assert(testSucceeded(14).testName == "test 15")
          assert(testSucceeded(14).testText == "test 15")
          assert(testSucceeded(14).location == Some(LineInFile(thisLineNumber - 138, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(14).formatter == Some(IndentedText("- test 15", "test 15", 1)))
          assert(testSucceeded(15).testName == "test 16")
          assert(testSucceeded(15).testText == "test 16")
          assert(testSucceeded(15).location == Some(LineInFile(thisLineNumber - 141, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(15).formatter == Some(IndentedText("- test 16", "test 16", 1)))
          assert(testSucceeded(16).testName == "test 17")
          assert(testSucceeded(16).testText == "test 17")
          assert(testSucceeded(16).location == Some(LineInFile(thisLineNumber - 144, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(16).formatter == Some(IndentedText("- test 17", "test 17", 1)))
          assert(testSucceeded(17).testName == "test 18")
          assert(testSucceeded(17).testText == "test 18")
          assert(testSucceeded(17).location == Some(LineInFile(thisLineNumber - 147, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(17).formatter == Some(IndentedText("- test 18", "test 18", 1)))
          assert(testSucceeded(18).testName == "test 19")
          assert(testSucceeded(18).testText == "test 19")
          assert(testSucceeded(18).location == Some(LineInFile(thisLineNumber - 150, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(18).formatter == Some(IndentedText("- test 19", "test 19", 1)))
          assert(testSucceeded(19).testName == "test 20")
          assert(testSucceeded(19).testText == "test 20")
          assert(testSucceeded(19).location == Some(LineInFile(thisLineNumber - 153, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(19).formatter == Some(IndentedText("- test 20", "test 20", 1)))
          assert(testSucceeded(20).testName == "test 21")
          assert(testSucceeded(20).testText == "test 21")
          assert(testSucceeded(20).location == Some(LineInFile(thisLineNumber - 156, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(20).formatter == Some(IndentedText("- test 21", "test 21", 1)))
        }
        it("should report 22 test succeeded events to the passed-in reporter when andThen with another 21 succeeded Test1") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              Test0("test 1")(3).andThen(
                Test1("test 2") { (i: Int) => succeed },
                Test1("test 3") { (i: Int) => succeed },
                Test1("test 4") { (i: Int) => succeed },
                Test1("test 5") { (i: Int) => succeed },
                Test1("test 6") { (i: Int) => succeed },
                Test1("test 7") { (i: Int) => succeed },
                Test1("test 8") { (i: Int) => succeed },
                Test1("test 9") { (i: Int) => succeed },
                Test1("test 10") { (i: Int) => succeed },
                Test1("test 11") { (i: Int) => succeed },
                Test1("test 12") { (i: Int) => succeed },
                Test1("test 13") { (i: Int) => succeed },
                Test1("test 14") { (i: Int) => succeed },
                Test1("test 15") { (i: Int) => succeed },
                Test1("test 16") { (i: Int) => succeed },
                Test1("test 17") { (i: Int) => succeed },
                Test1("test 18") { (i: Int) => succeed },
                Test1("test 19") { (i: Int) => succeed },
                Test1("test 20") { (i: Int) => succeed },
                Test1("test 21") { (i: Int) => succeed },
                Test1("test 22") { (i: Int) => succeed }
              )
          }
          assert(suite.testNames == Set("test 1", "test 2", "test 3", "test 4", "test 5", "test 6", "test 7", "test 8", "test 9",
            "test 10", "test 11", "test 12", "test 13", "test 14", "test 15", "test 16", "test 17", "test 18", "test 19", "test 20",
            "test 21", "test 22"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 22)
          assert(testStarting(0).testName == "test 1")
          assert(testStarting(0).testText == "test 1")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 32, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "test 2")
          assert(testStarting(1).testText == "test 2")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 34, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "test 3")
          assert(testStarting(2).testText == "test 3")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 36, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(3).testName == "test 4")
          assert(testStarting(3).testText == "test 4")
          assert(testStarting(3).location == Some(LineInFile(thisLineNumber - 38, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(4).testName == "test 5")
          assert(testStarting(4).testText == "test 5")
          assert(testStarting(4).location == Some(LineInFile(thisLineNumber - 40, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(5).testName == "test 6")
          assert(testStarting(5).testText == "test 6")
          assert(testStarting(5).location == Some(LineInFile(thisLineNumber - 42, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(6).testName == "test 7")
          assert(testStarting(6).testText == "test 7")
          assert(testStarting(6).location == Some(LineInFile(thisLineNumber - 44, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(7).testName == "test 8")
          assert(testStarting(7).testText == "test 8")
          assert(testStarting(7).location == Some(LineInFile(thisLineNumber - 46, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(8).testName == "test 9")
          assert(testStarting(8).testText == "test 9")
          assert(testStarting(8).location == Some(LineInFile(thisLineNumber - 48, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(9).testName == "test 10")
          assert(testStarting(9).testText == "test 10")
          assert(testStarting(9).location == Some(LineInFile(thisLineNumber - 50, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(10).testName == "test 11")
          assert(testStarting(10).testText == "test 11")
          assert(testStarting(10).location == Some(LineInFile(thisLineNumber - 52, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(11).testName == "test 12")
          assert(testStarting(11).testText == "test 12")
          assert(testStarting(11).location == Some(LineInFile(thisLineNumber - 54, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(12).testName == "test 13")
          assert(testStarting(12).testText == "test 13")
          assert(testStarting(12).location == Some(LineInFile(thisLineNumber - 56, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(13).testName == "test 14")
          assert(testStarting(13).testText == "test 14")
          assert(testStarting(13).location == Some(LineInFile(thisLineNumber - 58, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(14).testName == "test 15")
          assert(testStarting(14).testText == "test 15")
          assert(testStarting(14).location == Some(LineInFile(thisLineNumber - 60, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(15).testName == "test 16")
          assert(testStarting(15).testText == "test 16")
          assert(testStarting(15).location == Some(LineInFile(thisLineNumber - 62, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(16).testName == "test 17")
          assert(testStarting(16).testText == "test 17")
          assert(testStarting(16).location == Some(LineInFile(thisLineNumber - 64, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(17).testName == "test 18")
          assert(testStarting(17).testText == "test 18")
          assert(testStarting(17).location == Some(LineInFile(thisLineNumber - 66, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(18).testName == "test 19")
          assert(testStarting(18).testText == "test 19")
          assert(testStarting(18).location == Some(LineInFile(thisLineNumber - 68, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(19).testName == "test 20")
          assert(testStarting(19).testText == "test 20")
          assert(testStarting(19).location == Some(LineInFile(thisLineNumber - 70, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(20).testName == "test 21")
          assert(testStarting(20).testText == "test 21")
          assert(testStarting(20).location == Some(LineInFile(thisLineNumber - 72, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(21).testName == "test 22")
          assert(testStarting(21).testText == "test 22")
          assert(testStarting(21).location == Some(LineInFile(thisLineNumber - 74, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 22)
          assert(testSucceeded(0).testName == "test 1")
          assert(testSucceeded(0).testText == "test 1")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 100, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- test 1", "test 1", 1)))
          assert(testSucceeded(1).testName == "test 2")
          assert(testSucceeded(1).testText == "test 2")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 103, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- test 2", "test 2", 1)))
          assert(testSucceeded(2).testName == "test 3")
          assert(testSucceeded(2).testText == "test 3")
          assert(testSucceeded(2).location == Some(LineInFile(thisLineNumber - 106, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(2).formatter == Some(IndentedText("- test 3", "test 3", 1)))
          assert(testSucceeded(3).testName == "test 4")
          assert(testSucceeded(3).testText == "test 4")
          assert(testSucceeded(3).location == Some(LineInFile(thisLineNumber - 109, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(3).formatter == Some(IndentedText("- test 4", "test 4", 1)))
          assert(testSucceeded(4).testName == "test 5")
          assert(testSucceeded(4).testText == "test 5")
          assert(testSucceeded(4).location == Some(LineInFile(thisLineNumber - 112, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(4).formatter == Some(IndentedText("- test 5", "test 5", 1)))
          assert(testSucceeded(5).testName == "test 6")
          assert(testSucceeded(5).testText == "test 6")
          assert(testSucceeded(5).location == Some(LineInFile(thisLineNumber - 115, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(5).formatter == Some(IndentedText("- test 6", "test 6", 1)))
          assert(testSucceeded(6).testName == "test 7")
          assert(testSucceeded(6).testText == "test 7")
          assert(testSucceeded(6).location == Some(LineInFile(thisLineNumber - 118, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(6).formatter == Some(IndentedText("- test 7", "test 7", 1)))
          assert(testSucceeded(7).testName == "test 8")
          assert(testSucceeded(7).testText == "test 8")
          assert(testSucceeded(7).location == Some(LineInFile(thisLineNumber - 121, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(7).formatter == Some(IndentedText("- test 8", "test 8", 1)))
          assert(testSucceeded(8).testName == "test 9")
          assert(testSucceeded(8).testText == "test 9")
          assert(testSucceeded(8).location == Some(LineInFile(thisLineNumber - 124, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(8).formatter == Some(IndentedText("- test 9", "test 9", 1)))
          assert(testSucceeded(9).testName == "test 10")
          assert(testSucceeded(9).testText == "test 10")
          assert(testSucceeded(9).location == Some(LineInFile(thisLineNumber - 127, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(9).formatter == Some(IndentedText("- test 10", "test 10", 1)))
          assert(testSucceeded(10).testName == "test 11")
          assert(testSucceeded(10).testText == "test 11")
          assert(testSucceeded(10).location == Some(LineInFile(thisLineNumber - 130, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(10).formatter == Some(IndentedText("- test 11", "test 11", 1)))
          assert(testSucceeded(11).testName == "test 12")
          assert(testSucceeded(11).testText == "test 12")
          assert(testSucceeded(11).location == Some(LineInFile(thisLineNumber - 133, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(11).formatter == Some(IndentedText("- test 12", "test 12", 1)))
          assert(testSucceeded(12).testName == "test 13")
          assert(testSucceeded(12).testText == "test 13")
          assert(testSucceeded(12).location == Some(LineInFile(thisLineNumber - 136, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(12).formatter == Some(IndentedText("- test 13", "test 13", 1)))
          assert(testSucceeded(13).testName == "test 14")
          assert(testSucceeded(13).testText == "test 14")
          assert(testSucceeded(13).location == Some(LineInFile(thisLineNumber - 139, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(13).formatter == Some(IndentedText("- test 14", "test 14", 1)))
          assert(testSucceeded(14).testName == "test 15")
          assert(testSucceeded(14).testText == "test 15")
          assert(testSucceeded(14).location == Some(LineInFile(thisLineNumber - 142, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(14).formatter == Some(IndentedText("- test 15", "test 15", 1)))
          assert(testSucceeded(15).testName == "test 16")
          assert(testSucceeded(15).testText == "test 16")
          assert(testSucceeded(15).location == Some(LineInFile(thisLineNumber - 145, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(15).formatter == Some(IndentedText("- test 16", "test 16", 1)))
          assert(testSucceeded(16).testName == "test 17")
          assert(testSucceeded(16).testText == "test 17")
          assert(testSucceeded(16).location == Some(LineInFile(thisLineNumber - 148, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(16).formatter == Some(IndentedText("- test 17", "test 17", 1)))
          assert(testSucceeded(17).testName == "test 18")
          assert(testSucceeded(17).testText == "test 18")
          assert(testSucceeded(17).location == Some(LineInFile(thisLineNumber - 151, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(17).formatter == Some(IndentedText("- test 18", "test 18", 1)))
          assert(testSucceeded(18).testName == "test 19")
          assert(testSucceeded(18).testText == "test 19")
          assert(testSucceeded(18).location == Some(LineInFile(thisLineNumber - 154, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(18).formatter == Some(IndentedText("- test 19", "test 19", 1)))
          assert(testSucceeded(19).testName == "test 20")
          assert(testSucceeded(19).testText == "test 20")
          assert(testSucceeded(19).location == Some(LineInFile(thisLineNumber - 157, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(19).formatter == Some(IndentedText("- test 20", "test 20", 1)))
          assert(testSucceeded(20).testName == "test 21")
          assert(testSucceeded(20).testText == "test 21")
          assert(testSucceeded(20).location == Some(LineInFile(thisLineNumber - 160, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(20).formatter == Some(IndentedText("- test 21", "test 21", 1)))
          assert(testSucceeded(21).testName == "test 22")
          assert(testSucceeded(21).testText == "test 22")
          assert(testSucceeded(21).location == Some(LineInFile(thisLineNumber - 163, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(21).formatter == Some(IndentedText("- test 22", "test 22", 1)))
        }
        it("should throw DuplicateTestNameException if a duplicate test name registration is detected when doing andThen with another TestFlow") {
          assertThrows[DuplicateTestNameException] {
            Test0("same")(0).andThen(Test1("same") { (i: Int) => (i * 4).toString })
          }
        }
      }
      describe("when the test fails") {
        it("should report 1 test succeeded and 1 test failed events to the passed-in reporter when first test passed and second test failed") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              Test0("first")(3).andThen(
                Test1("second") { (i: Int) => throw new RuntimeException("oops!!"); 1 }
              )
          }
          assert(suite.testNames == Set("first", "second"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 2)
          assert(testStarting(0).testName == "first")
          assert(testStarting(0).testText == "first")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 10, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "second")
          assert(testStarting(1).testText == "second")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 12, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 1)
          assert(testSucceeded(0).testName == "first")
          assert(testSucceeded(0).testText == "first")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 18, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- first", "first", 1)))
          val testFailed = myRep.testFailedEventsReceived
          assert(testFailed.size == 1)
          assert(testFailed(0).testName == "second")
          assert(testFailed(0).testText == "second")
          assert(testFailed(0).location == Some(SeeStackDepthException))
          assert(testFailed(0).formatter == Some(IndentedText("- second", "second", 1)))
        }
        it("should report 2 test succeeded events and 1 failed event to the passed-in reporter when andThen with 1 succeeded and 1 failed Test1") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              Test0("first")(3).andThen(
                Test1("second") { (i: Int) => throw new RuntimeException("oops!"); succeed },
                Test1("third") { (i: Int) => succeed }
              )
          }
          assert(suite.testNames == Set("first", "second", "third"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 3)
          assert(testStarting(0).testName == "first")
          assert(testStarting(0).testText == "first")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 11, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "second")
          assert(testStarting(1).testText == "second")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 13, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "third")
          assert(testStarting(2).testText == "third")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 15, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 2)
          assert(testSucceeded(0).testName == "first")
          assert(testSucceeded(0).testText == "first")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 22, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- first", "first", 1)))
          assert(testSucceeded(1).testName == "third")
          assert(testSucceeded(1).testText == "third")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 24, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- third", "third", 1)))
          val testFailed = myRep.testFailedEventsReceived
          assert(testFailed(0).testName == "second")
          assert(testFailed(0).testText == "second")
          assert(testFailed(0).location == Some(SeeStackDepthException))
          assert(testFailed(0).formatter == Some(IndentedText("- second", "second", 1)))
        }
        it("should report 2 test succeeded events, 1 failed event and 1 cancel evet to the passed-in reporter when andThen with 1 succeeded and 1 failed Test1 and andThen with another succeeded test") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              Test0("first")(3).andThen(
                Test1("second") { (i: Int) => throw new RuntimeException("oops!"); succeed },
                Test1("third") { (i: Int) => succeed }
              ).andThen(
                Test1("forth") { case (a: Assertion, b: Assertion) => succeed }
              )
          }
          assert(suite.testNames == Set("first", "second", "third", "forth"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 4)
          assert(testStarting(0).testName == "first")
          assert(testStarting(0).testText == "first")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 13, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "second")
          assert(testStarting(1).testText == "second")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 15, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "third")
          assert(testStarting(2).testText == "third")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 17, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(3).testName == "forth")
          assert(testStarting(3).testText == "forth")
          assert(testStarting(3).location == Some(LineInFile(thisLineNumber - 18, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 2)
          assert(testSucceeded(0).testName == "first")
          assert(testSucceeded(0).testText == "first")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 27, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- first", "first", 1)))
          assert(testSucceeded(1).testName == "third")
          assert(testSucceeded(1).testText == "third")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 29, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- third", "third", 1)))
          val testFailed = myRep.testFailedEventsReceived
          assert(testFailed(0).testName == "second")
          assert(testFailed(0).testText == "second")
          assert(testFailed(0).location == Some(SeeStackDepthException))
          assert(testFailed(0).formatter == Some(IndentedText("- second", "second", 1)))
          val testCanceled = myRep.testCanceledEventsReceived
          assert(testCanceled(0).testName == "forth")
          assert(testCanceled(0).testText == "forth")
          assert(testCanceled(0).location == Some(LineInFile(thisLineNumber - 37, "TestFlowSpec.scala", testFilePathname)))
          assert(testCanceled(0).formatter == Some(IndentedText("- forth", "forth", 1)))
        }
      }
      describe("when the test cancel") {
        it("should report 1 test canceled evetns to the passed-in reporter when the test canceled") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              Test0("first") {
                cancel
              }
          }
          assert(suite.testNames == Set("first"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 1)
          assert(testStarting(0).testName == "first")
          assert(testStarting(0).testText == "first")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 10, "TestFlowSpec.scala", testFilePathname)))
          val testCanceled = myRep.testCanceledEventsReceived
          assert(testCanceled.size == 1)
          assert(testCanceled(0).testName == "first")
          assert(testCanceled(0).testText == "first")
          assert(testCanceled(0).location == Some(LineInFile(thisLineNumber - 14, "TestFlowSpec.scala", testFilePathname)))
          assert(testCanceled(0).formatter == Some(IndentedText("- first", "first", 1)))
        }
        it("should report 2 test canceled events to the passed-in reporter when there are 2 tests and the first test canceled") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val t0 =
              Test0("first") {
                cancel
                3
              }
            val flow =
              t0.andThen(
                Test1("second") { (i: Int) =>
                  i + 1
                }
              )
          }
          assert(suite.testNames == Set("first", "second"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 2)
          assert(testStarting(0).testName == "first")
          assert(testStarting(0).testText == "first")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 17, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "second")
          assert(testStarting(1).testText == "second")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 14, "TestFlowSpec.scala", testFilePathname)))
          val testCanceled = myRep.testCanceledEventsReceived
          assert(testCanceled.size == 2)
          assert(testCanceled(0).testName == "first")
          assert(testCanceled(0).testText == "first")
          assert(testCanceled(0).location == Some(LineInFile(thisLineNumber - 24, "TestFlowSpec.scala", testFilePathname)))
          assert(testCanceled(0).formatter == Some(IndentedText("- first", "first", 1)))
          assert(testCanceled(1).testName == "second")
          assert(testCanceled(1).testText == "second")
          assert(testCanceled(1).location == Some(LineInFile(thisLineNumber - 23, "TestFlowSpec.scala", testFilePathname)))
          assert(testCanceled(1).formatter == Some(IndentedText("- second", "second", 1)))
        }
        it("should report 1 test succeeded and 1 test canceled events to the passed-in reporter when first test passed and second test canceled") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              Test0("first")(3).andThen(
                Test1("second") { (i: Int) =>
                  cancel
                  1
                }
              )
          }
          assert(suite.testNames == Set("first", "second"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 2)
          assert(testStarting(0).testName == "first")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 12, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "second")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 13, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 1)
          assert(testSucceeded(0).testName == "first")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 18, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- first", "first", 1)))
          val testCanceled = myRep.testCanceledEventsReceived
          assert(testCanceled.size == 1)
          assert(testCanceled(0).testName == "second")
          assert(testCanceled(0).location == Some(LineInFile(thisLineNumber - 21, "TestFlowSpec.scala", testFilePathname)))
          assert(testCanceled(0).formatter == Some(IndentedText("- second", "second", 1)))
        }

        it("should report 2 test succeeded events and 1 canceled event to the passed-in reporter when andThen with 1 succeeded and 1 canceled Test1") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              Test0("first")(3).andThen(
                Test1("second") { (i: Int) => cancel; succeed },
                Test1("third") { (i: Int) => succeed }
              )
          }
          assert(suite.testNames == Set("first", "second", "third"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 3)
          assert(testStarting(0).testName == "first")
          assert(testStarting(0).testText == "first")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 11, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "second")
          assert(testStarting(1).testText == "second")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 13, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "third")
          assert(testStarting(2).testText == "third")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 15, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 2)
          assert(testSucceeded(0).testName == "first")
          assert(testSucceeded(0).testText == "first")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 22, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- first", "first", 1)))
          assert(testSucceeded(1).testName == "third")
          assert(testSucceeded(1).testText == "third")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 24, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- third", "third", 1)))
          val testCanceled = myRep.testCanceledEventsReceived
          assert(testCanceled(0).testName == "second")
          assert(testCanceled(0).testText == "second")
          assert(testCanceled(0).location == Some(LineInFile(thisLineNumber - 30, "TestFlowSpec.scala", testFilePathname)))
          assert(testCanceled(0).formatter == Some(IndentedText("- second", "second", 1)))
        }
        it("should report 2 test succeeded events, 2 cancel events to the passed-in reporter when andThen with 1 succeeded and 1 canceled Test1 and andThen with another succeeded test") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              Test0("first")(3).andThen(
                Test1("second") { (i: Int) => cancel; succeed },
                Test1("third") { (i: Int) => succeed }
              ).andThen(
                Test1("forth") { case (a: Assertion, b: Assertion) => succeed }
              )
          }
          assert(suite.testNames == Set("first", "second", "third", "forth"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 4)
          assert(testStarting(0).testName == "first")
          assert(testStarting(0).testText == "first")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 13, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "second")
          assert(testStarting(1).testText == "second")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 15, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "third")
          assert(testStarting(2).testText == "third")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 17, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(3).testName == "forth")
          assert(testStarting(3).testText == "forth")
          assert(testStarting(3).location == Some(LineInFile(thisLineNumber - 18, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 2)
          assert(testSucceeded(0).testName == "first")
          assert(testSucceeded(0).testText == "first")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 27, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- first", "first", 1)))
          assert(testSucceeded(1).testName == "third")
          assert(testSucceeded(1).testText == "third")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 29, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- third", "third", 1)))
          val testCanceled = myRep.testCanceledEventsReceived
          assert(testCanceled.size == 2)
          assert(testCanceled(0).testName == "second")
          assert(testCanceled(0).testText == "second")
          assert(testCanceled(0).location == Some(LineInFile(thisLineNumber - 36, "TestFlowSpec.scala", testFilePathname)))
          assert(testCanceled(0).formatter == Some(IndentedText("- second", "second", 1)))
          assert(testCanceled(1).testName == "forth")
          assert(testCanceled(1).testText == "forth")
          assert(testCanceled(1).location == Some(LineInFile(thisLineNumber - 37, "TestFlowSpec.scala", testFilePathname)))
          assert(testCanceled(1).formatter == Some(IndentedText("- forth", "forth", 1)))
        }
      }
      describe("when the test is pending") {
        it("should report 1 test pending event to the passed-in reporter when the test is pending") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val t: Test0[PendingStatement] =
              Test0("first") {
                pending
              }
            val flow = t
          }
          assert(suite.testNames == Set("first"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 1)
          assert(testStarting(0).testName == "first")
          assert(testStarting(0).testText == "first")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 11, "TestFlowSpec.scala", testFilePathname)))
          val testPending = myRep.testPendingEventsReceived
          assert(testPending.size == 1)
          assert(testPending(0).testName == "first")
          assert(testPending(0).testText == "first")
          assert(testPending(0).location == Some(LineInFile(thisLineNumber - 16, "TestFlowSpec.scala", testFilePathname)))
          assert(testPending(0).formatter == Some(IndentedText("- first", "first", 1)))
        }
        it("should report 1 test pending and 1 test canceled events to the passed-in reporter when there are 2 tests and the first test is pending") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val t0 = Test0("first") {
              pending
              3
            }
            val flow =
              t0.andThen(
                Test1("second") { (i: Int) =>
                  i + 1
                }
              )
          }
          assert(suite.testNames == Set("first", "second"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 2)
          assert(testStarting(0).testName == "first")
          assert(testStarting(0).testText == "first")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 17, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "second")
          assert(testStarting(1).testText == "second")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 14, "TestFlowSpec.scala", testFilePathname)))
          val testPending = myRep.testPendingEventsReceived
          assert(testPending.size == 1)
          assert(testPending(0).testName == "first")
          assert(testPending(0).testText == "first")
          assert(testPending(0).location == Some(LineInFile(thisLineNumber - 25, "TestFlowSpec.scala", testFilePathname)))
          assert(testPending(0).formatter == Some(IndentedText("- first", "first", 1)))
          val testCanceled = myRep.testCanceledEventsReceived
          assert(testCanceled.size == 1)
          assert(testCanceled(0).testName == "second")
          assert(testCanceled(0).testText == "second")
          assert(testCanceled(0).location == Some(LineInFile(thisLineNumber - 25, "TestFlowSpec.scala", testFilePathname)))
          assert(testCanceled(0).formatter == Some(IndentedText("- second", "second", 1)))
        }
        it("should report 1 test succeeded and 1 test pending events to the passed-in reporter when first test passed and second test is pending") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              Test0("first")(3).andThen(
                Test1[Int, PendingStatement]("second") { (i: Int) =>
                  pending
                }
              )
          }
          assert(suite.testNames == Set("first", "second"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 2)
          assert(testStarting(0).testName == "first")
          assert(testStarting(0).testText == "first")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 12, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "second")
          assert(testStarting(1).testText == "second")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 14, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 1)
          assert(testSucceeded(0).testName == "first")
          assert(testSucceeded(0).testText == "first")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 20, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- first", "first", 1)))
          val testPending = myRep.testPendingEventsReceived
          assert(testPending.size == 1)
          assert(testPending(0).testName == "second")
          assert(testPending(0).testText == "second")
          assert(testPending(0).location == Some(LineInFile(thisLineNumber - 25, "TestFlowSpec.scala", testFilePathname)))
          assert(testPending(0).formatter == Some(IndentedText("- second", "second", 1)))
        }

        it("should report 2 test succeeded events and 1 pending event to the passed-in reporter when andThen with 1 succeeded and 1 canceled Test1") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              Test0("first")(3).andThen(
                Test1("second") { (i: Int) => pending; succeed },
                Test1("third") { (i: Int) => succeed }
              )
          }
          assert(suite.testNames == Set("first", "second", "third"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 3)
          assert(testStarting(0).testName == "first")
          assert(testStarting(0).testText == "first")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 11, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "second")
          assert(testStarting(1).testText == "second")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 13, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "third")
          assert(testStarting(2).testText == "third")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 15, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 2)
          assert(testSucceeded(0).testName == "first")
          assert(testSucceeded(0).testText == "first")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 22, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- first", "first", 1)))
          assert(testSucceeded(1).testName == "third")
          assert(testSucceeded(1).testText == "third")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 24, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- third", "third", 1)))
          val testPending = myRep.testPendingEventsReceived
          assert(testPending(0).testName == "second")
          assert(testPending(0).testText == "second")
          assert(testPending(0).location == Some(LineInFile(thisLineNumber - 30, "TestFlowSpec.scala", testFilePathname)))
          assert(testPending(0).formatter == Some(IndentedText("- second", "second", 1)))
        }
        it("should report 2 test succeeded events, 1 pending and 1 cancel events to the passed-in reporter when andThen with 1 succeeded and 1 pending Test1 and andThen with another succeeded test") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              Test0("first")(3).andThen(
                Test1("second") { (i: Int) => pending; succeed },
                Test1("third") { (i: Int) => succeed }
              ).andThen(
                Test1("forth") { case (a: Assertion, b: Assertion) => succeed }
              )
          }
          assert(suite.testNames == Set("first", "second", "third", "forth"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 4)
          assert(testStarting(0).testName == "first")
          assert(testStarting(0).testText == "first")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 13, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "second")
          assert(testStarting(1).testText == "second")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 15, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "third")
          assert(testStarting(2).testText == "third")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 17, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(3).testName == "forth")
          assert(testStarting(3).testText == "forth")
          assert(testStarting(3).location == Some(LineInFile(thisLineNumber - 18, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 2)
          assert(testSucceeded(0).testName == "first")
          assert(testSucceeded(0).testText == "first")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 27, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- first", "first", 1)))
          assert(testSucceeded(1).testName == "third")
          assert(testSucceeded(1).testText == "third")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 29, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- third", "third", 1)))
          val testCanceled = myRep.testCanceledEventsReceived
          assert(testCanceled.size == 1)
          assert(testCanceled(0).testName == "forth")
          assert(testCanceled(0).testText == "forth")
          assert(testCanceled(0).location == Some(LineInFile(thisLineNumber - 33, "TestFlowSpec.scala", testFilePathname)))
          assert(testCanceled(0).formatter == Some(IndentedText("- forth", "forth", 1)))
          val testPending = myRep.testPendingEventsReceived
          assert(testPending(0).testName == "second")
          assert(testPending(0).testText == "second")
          assert(testPending(0).location == Some(LineInFile(thisLineNumber - 41, "TestFlowSpec.scala", testFilePathname)))
          assert(testPending(0).formatter == Some(IndentedText("- second", "second", 1)))
        }
      }
    }
  }
  describe("A Test1") {
    it("should offer a factory method in its companion that takes a by-name of type Future[T]") {
      """Test1("my name") { (u: Unit) => 99 }: Test1[Unit, Int]""" should compile
      """Test1("my name") { (i: Long) => "hello" }: Test1[Long, String]""" should compile
      var x = false
      Test1("my name") { (i: Int) =>
        x = true
      }
      x shouldBe false
    }
    val fut = Future.successful(99)
    it("should return a Set with one test name when the TestFlow factory is used") {
      Test1("my test name")((i: Int) => i + 1).testNames shouldEqual Set("my test name")
      Test1("your test name")((i: Int) => i + 1).testNames shouldEqual Set("your test name")
    }
    it("should return the all test names from testNames when andThen is used to compose TestFlows, a Set that iterates in left to right order") {
      val flow = Test1("first")((i: Int) => i + 1).andThen(Test1("second") { (i: Int) => (i * 4).toString })
      flow.testNames shouldEqual Set("first", "second")
      flow.testNames.iterator.toList shouldEqual List("first", "second")
    }
    it("should return the all test names from testNames when compose is used to compose TestFlows, a Set that iterates in right to left order") {
      val flow = Test1("second") { (i: Int) => (i * 4).toString }.compose(Test1("first")((i: Int) => i + 1))
      flow.testNames shouldEqual Set("first", "second")
      flow.testNames.iterator.toList shouldEqual List("first", "second")
    }
    it("should return the all test names from testNames when compose is used to compose TestFlows with Test0s, a Set that iterates in right to left order") {
      val flow = Test1("second") { (i: Int) => (i * 4).toString }.compose(Test0("first")(4))
      flow.testNames shouldEqual Set("first", "second")
      flow.testNames.iterator.toList shouldEqual List("first", "second")
    }
    describe("when it was composed with something else") {
      describe("when the test succeeds") {
        it("should report 2 test succeeded events to the passed-in reporter when compose with another Test0") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              Test1("second") { (i: Int) =>
                (i * 4).toString
              }.compose(
                Test0("first")(5)
              )
          }
          assert(suite.testNames == Set("first", "second"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 2)
          assert(testStarting(0).testName == "first")
          assert(testStarting(0).testText == "first")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 9, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "second")
          assert(testStarting(1).testText == "second")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 15, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 2)
          assert(testSucceeded(0).testName == "first")
          assert(testSucceeded(0).testText == "first")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 17, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- first", "first", 1)))
          assert(testSucceeded(1).testName == "second")
          assert(testSucceeded(1).testText == "second")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 24, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- second", "second", 1)))
        }
        it("should throw DuplicateTestNameException if a duplicate test name registration is detected when doing andThen with another TestFlow") {
          assertThrows[DuplicateTestNameException] {
            Test1("same")((i: Int) => i + 1).andThen(Test1("same") { (i: Int) => (i * 4).toString })
          }
        }
        it("should throw DuplicateTestNameException if a duplicate test name registration is detected when doing compose with another TestFlow") {
          assertThrows[DuplicateTestNameException] {
            (Test1("same")  { (i: Int) => (i * 4).toString }).compose(Test1("same")((i: Int) => i + 1))
          }
        }
        it("should throw DuplicateTestNameException if a duplicate test name registration is detected when doing compose with another Test0") {
          assertThrows[DuplicateTestNameException] {
            (Test1("same")  { (i: Int) => (i * 4).toString }).compose(Test0("same")(0))
          }
        }
        it("should report 3 test succeeded events to the passed-in reporter when andThen with another 2 succeeded Test1") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              BeforeNode(3).andThen(
                Test1("test 1") { (i: Int) => i + 1 }.andThen(
                  Test1("test 2") { (i: Int) => succeed },
                  Test1("test 3") { (i: Int) => succeed }
                )
              )
          }
          assert(suite.testNames == Set("test 1", "test 2", "test 3"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 3)
          assert(testStarting(0).testName == "test 1")
          assert(testStarting(0).testText == "test 1")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 12, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "test 2")
          assert(testStarting(1).testText == "test 2")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 14, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "test 3")
          assert(testStarting(2).testText == "test 3")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 16, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 3)
          assert(testSucceeded(0).testName == "test 1")
          assert(testSucceeded(0).testText == "test 1")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 23, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- test 1", "test 1", 1)))
          assert(testSucceeded(1).testName == "test 2")
          assert(testSucceeded(1).testText == "test 2")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 26, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- test 2", "test 2", 1)))
          assert(testSucceeded(2).testName == "test 3")
          assert(testSucceeded(2).testText == "test 3")
          assert(testSucceeded(2).location == Some(LineInFile(thisLineNumber - 29, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(2).formatter == Some(IndentedText("- test 3", "test 3", 1)))
        }
        it("should report 4 test succeeded events to the passed-in reporter when andThen with another 3 succeeded Test1") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              BeforeNode(3).andThen(
                Test1("test 1") { (i: Int) => i + 1 }.andThen(
                  Test1("test 2") { (i: Int) => succeed },
                  Test1("test 3") { (i: Int) => succeed },
                  Test1("test 4") { (i: Int) => succeed }
                )
              )
          }
          assert(suite.testNames == Set("test 1", "test 2", "test 3", "test 4"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 4)
          assert(testStarting(0).testName == "test 1")
          assert(testStarting(0).testText == "test 1")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 13, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "test 2")
          assert(testStarting(1).testText == "test 2")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 15, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "test 3")
          assert(testStarting(2).testText == "test 3")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 17, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(3).testName == "test 4")
          assert(testStarting(3).testText == "test 4")
          assert(testStarting(3).location == Some(LineInFile(thisLineNumber - 19, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 4)
          assert(testSucceeded(0).testName == "test 1")
          assert(testSucceeded(0).testText == "test 1")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 27, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- test 1", "test 1", 1)))
          assert(testSucceeded(1).testName == "test 2")
          assert(testSucceeded(1).testText == "test 2")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 30, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- test 2", "test 2", 1)))
          assert(testSucceeded(2).testName == "test 3")
          assert(testSucceeded(2).testText == "test 3")
          assert(testSucceeded(2).location == Some(LineInFile(thisLineNumber - 33, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(2).formatter == Some(IndentedText("- test 3", "test 3", 1)))
          assert(testSucceeded(3).testName == "test 4")
          assert(testSucceeded(3).testText == "test 4")
          assert(testSucceeded(3).location == Some(LineInFile(thisLineNumber - 36, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(3).formatter == Some(IndentedText("- test 4", "test 4", 1)))
        }
        it("should report 5 test succeeded events to the passed-in reporter when andThen with another 4 succeeded Test1") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              BeforeNode(3).andThen(
                Test1("test 1") { (i: Int) => i + 1 }.andThen(
                  Test1("test 2") { (i: Int) => succeed },
                  Test1("test 3") { (i: Int) => succeed },
                  Test1("test 4") { (i: Int) => succeed },
                  Test1("test 5") { (i: Int) => succeed }
                )
              )
          }
          assert(suite.testNames == Set("test 1", "test 2", "test 3", "test 4", "test 5"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 5)
          assert(testStarting(0).testName == "test 1")
          assert(testStarting(0).testText == "test 1")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 14, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "test 2")
          assert(testStarting(1).testText == "test 2")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 16, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "test 3")
          assert(testStarting(2).testText == "test 3")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 18, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(3).testName == "test 4")
          assert(testStarting(3).testText == "test 4")
          assert(testStarting(3).location == Some(LineInFile(thisLineNumber - 20, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(4).testName == "test 5")
          assert(testStarting(4).testText == "test 5")
          assert(testStarting(4).location == Some(LineInFile(thisLineNumber - 22, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 5)
          assert(testSucceeded(0).testName == "test 1")
          assert(testSucceeded(0).testText == "test 1")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 31, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- test 1", "test 1", 1)))
          assert(testSucceeded(1).testName == "test 2")
          assert(testSucceeded(1).testText == "test 2")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 34, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- test 2", "test 2", 1)))
          assert(testSucceeded(2).testName == "test 3")
          assert(testSucceeded(2).testText == "test 3")
          assert(testSucceeded(2).location == Some(LineInFile(thisLineNumber - 37, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(2).formatter == Some(IndentedText("- test 3", "test 3", 1)))
          assert(testSucceeded(3).testName == "test 4")
          assert(testSucceeded(3).testText == "test 4")
          assert(testSucceeded(3).location == Some(LineInFile(thisLineNumber - 40, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(3).formatter == Some(IndentedText("- test 4", "test 4", 1)))
          assert(testSucceeded(4).testName == "test 5")
          assert(testSucceeded(4).testText == "test 5")
          assert(testSucceeded(4).location == Some(LineInFile(thisLineNumber - 43, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(4).formatter == Some(IndentedText("- test 5", "test 5", 1)))
        }
        it("should report 6 test succeeded events to the passed-in reporter when andThen with another 5 succeeded Test1") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              BeforeNode(3).andThen(
                Test1("test 1") { (i: Int) => i + 1 }.andThen(
                  Test1("test 2") { (i: Int) => succeed },
                  Test1("test 3") { (i: Int) => succeed },
                  Test1("test 4") { (i: Int) => succeed },
                  Test1("test 5") { (i: Int) => succeed },
                  Test1("test 6") { (i: Int) => succeed }
                )
              )
          }
          assert(suite.testNames == Set("test 1", "test 2", "test 3", "test 4", "test 5", "test 6"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 6)
          assert(testStarting(0).testName == "test 1")
          assert(testStarting(0).testText == "test 1")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 15, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "test 2")
          assert(testStarting(1).testText == "test 2")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 17, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "test 3")
          assert(testStarting(2).testText == "test 3")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 19, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(3).testName == "test 4")
          assert(testStarting(3).testText == "test 4")
          assert(testStarting(3).location == Some(LineInFile(thisLineNumber - 21, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(4).testName == "test 5")
          assert(testStarting(4).testText == "test 5")
          assert(testStarting(4).location == Some(LineInFile(thisLineNumber - 23, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(5).testName == "test 6")
          assert(testStarting(5).testText == "test 6")
          assert(testStarting(5).location == Some(LineInFile(thisLineNumber - 25, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 6)
          assert(testSucceeded(0).testName == "test 1")
          assert(testSucceeded(0).testText == "test 1")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 35, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- test 1", "test 1", 1)))
          assert(testSucceeded(1).testName == "test 2")
          assert(testSucceeded(1).testText == "test 2")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 38, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- test 2", "test 2", 1)))
          assert(testSucceeded(2).testName == "test 3")
          assert(testSucceeded(2).testText == "test 3")
          assert(testSucceeded(2).location == Some(LineInFile(thisLineNumber - 41, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(2).formatter == Some(IndentedText("- test 3", "test 3", 1)))
          assert(testSucceeded(3).testName == "test 4")
          assert(testSucceeded(3).testText == "test 4")
          assert(testSucceeded(3).location == Some(LineInFile(thisLineNumber - 44, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(3).formatter == Some(IndentedText("- test 4", "test 4", 1)))
          assert(testSucceeded(4).testName == "test 5")
          assert(testSucceeded(4).testText == "test 5")
          assert(testSucceeded(4).location == Some(LineInFile(thisLineNumber - 47, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(4).formatter == Some(IndentedText("- test 5", "test 5", 1)))
          assert(testSucceeded(5).testName == "test 6")
          assert(testSucceeded(5).testText == "test 6")
          assert(testSucceeded(5).location == Some(LineInFile(thisLineNumber - 50, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(5).formatter == Some(IndentedText("- test 6", "test 6", 1)))
        }
        it("should report 7 test succeeded events to the passed-in reporter when andThen with another 6 succeeded Test1") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              BeforeNode(3).andThen(
                Test1("test 1") { (i: Int) => i + 1 }.andThen(
                  Test1("test 2") { (i: Int) => succeed },
                  Test1("test 3") { (i: Int) => succeed },
                  Test1("test 4") { (i: Int) => succeed },
                  Test1("test 5") { (i: Int) => succeed },
                  Test1("test 6") { (i: Int) => succeed },
                  Test1("test 7") { (i: Int) => succeed }
                )
              )
          }
          assert(suite.testNames == Set("test 1", "test 2", "test 3", "test 4", "test 5", "test 6", "test 7"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 7)
          assert(testStarting(0).testName == "test 1")
          assert(testStarting(0).testText == "test 1")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 16, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "test 2")
          assert(testStarting(1).testText == "test 2")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 18, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "test 3")
          assert(testStarting(2).testText == "test 3")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 20, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(3).testName == "test 4")
          assert(testStarting(3).testText == "test 4")
          assert(testStarting(3).location == Some(LineInFile(thisLineNumber - 22, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(4).testName == "test 5")
          assert(testStarting(4).testText == "test 5")
          assert(testStarting(4).location == Some(LineInFile(thisLineNumber - 24, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(5).testName == "test 6")
          assert(testStarting(5).testText == "test 6")
          assert(testStarting(5).location == Some(LineInFile(thisLineNumber - 26, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(6).testName == "test 7")
          assert(testStarting(6).testText == "test 7")
          assert(testStarting(6).location == Some(LineInFile(thisLineNumber - 28, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 7)
          assert(testSucceeded(0).testName == "test 1")
          assert(testSucceeded(0).testText == "test 1")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 39, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- test 1", "test 1", 1)))
          assert(testSucceeded(1).testName == "test 2")
          assert(testSucceeded(1).testText == "test 2")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 42, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- test 2", "test 2", 1)))
          assert(testSucceeded(2).testName == "test 3")
          assert(testSucceeded(2).testText == "test 3")
          assert(testSucceeded(2).location == Some(LineInFile(thisLineNumber - 45, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(2).formatter == Some(IndentedText("- test 3", "test 3", 1)))
          assert(testSucceeded(3).testName == "test 4")
          assert(testSucceeded(3).testText == "test 4")
          assert(testSucceeded(3).location == Some(LineInFile(thisLineNumber - 48, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(3).formatter == Some(IndentedText("- test 4", "test 4", 1)))
          assert(testSucceeded(4).testName == "test 5")
          assert(testSucceeded(4).testText == "test 5")
          assert(testSucceeded(4).location == Some(LineInFile(thisLineNumber - 51, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(4).formatter == Some(IndentedText("- test 5", "test 5", 1)))
          assert(testSucceeded(5).testName == "test 6")
          assert(testSucceeded(5).testText == "test 6")
          assert(testSucceeded(5).location == Some(LineInFile(thisLineNumber - 54, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(5).formatter == Some(IndentedText("- test 6", "test 6", 1)))
          assert(testSucceeded(6).testName == "test 7")
          assert(testSucceeded(6).testText == "test 7")
          assert(testSucceeded(6).location == Some(LineInFile(thisLineNumber - 57, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(6).formatter == Some(IndentedText("- test 7", "test 7", 1)))
        }
        it("should report 8 test succeeded events to the passed-in reporter when andThen with another 7 succeeded Test1") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              BeforeNode(3).andThen(
                Test1("test 1") { (i: Int) => i + 1 }.andThen(
                  Test1("test 2") { (i: Int) => succeed },
                  Test1("test 3") { (i: Int) => succeed },
                  Test1("test 4") { (i: Int) => succeed },
                  Test1("test 5") { (i: Int) => succeed },
                  Test1("test 6") { (i: Int) => succeed },
                  Test1("test 7") { (i: Int) => succeed },
                  Test1("test 8") { (i: Int) => succeed }
                )
              )
          }
          assert(suite.testNames == Set("test 1", "test 2", "test 3", "test 4", "test 5", "test 6", "test 7", "test 8"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 8)
          assert(testStarting(0).testName == "test 1")
          assert(testStarting(0).testText == "test 1")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 17, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "test 2")
          assert(testStarting(1).testText == "test 2")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 19, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "test 3")
          assert(testStarting(2).testText == "test 3")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 21, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(3).testName == "test 4")
          assert(testStarting(3).testText == "test 4")
          assert(testStarting(3).location == Some(LineInFile(thisLineNumber - 23, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(4).testName == "test 5")
          assert(testStarting(4).testText == "test 5")
          assert(testStarting(4).location == Some(LineInFile(thisLineNumber - 25, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(5).testName == "test 6")
          assert(testStarting(5).testText == "test 6")
          assert(testStarting(5).location == Some(LineInFile(thisLineNumber - 27, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(6).testName == "test 7")
          assert(testStarting(6).testText == "test 7")
          assert(testStarting(6).location == Some(LineInFile(thisLineNumber - 29, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(7).testName == "test 8")
          assert(testStarting(7).testText == "test 8")
          assert(testStarting(7).location == Some(LineInFile(thisLineNumber - 31, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 8)
          assert(testSucceeded(0).testName == "test 1")
          assert(testSucceeded(0).testText == "test 1")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 43, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- test 1", "test 1", 1)))
          assert(testSucceeded(1).testName == "test 2")
          assert(testSucceeded(1).testText == "test 2")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 46, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- test 2", "test 2", 1)))
          assert(testSucceeded(2).testName == "test 3")
          assert(testSucceeded(2).testText == "test 3")
          assert(testSucceeded(2).location == Some(LineInFile(thisLineNumber - 49, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(2).formatter == Some(IndentedText("- test 3", "test 3", 1)))
          assert(testSucceeded(3).testName == "test 4")
          assert(testSucceeded(3).testText == "test 4")
          assert(testSucceeded(3).location == Some(LineInFile(thisLineNumber - 52, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(3).formatter == Some(IndentedText("- test 4", "test 4", 1)))
          assert(testSucceeded(4).testName == "test 5")
          assert(testSucceeded(4).testText == "test 5")
          assert(testSucceeded(4).location == Some(LineInFile(thisLineNumber - 55, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(4).formatter == Some(IndentedText("- test 5", "test 5", 1)))
          assert(testSucceeded(5).testName == "test 6")
          assert(testSucceeded(5).testText == "test 6")
          assert(testSucceeded(5).location == Some(LineInFile(thisLineNumber - 58, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(5).formatter == Some(IndentedText("- test 6", "test 6", 1)))
          assert(testSucceeded(6).testName == "test 7")
          assert(testSucceeded(6).testText == "test 7")
          assert(testSucceeded(6).location == Some(LineInFile(thisLineNumber - 61, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(6).formatter == Some(IndentedText("- test 7", "test 7", 1)))
          assert(testSucceeded(7).testName == "test 8")
          assert(testSucceeded(7).testText == "test 8")
          assert(testSucceeded(7).location == Some(LineInFile(thisLineNumber - 64, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(7).formatter == Some(IndentedText("- test 8", "test 8", 1)))
        }
        it("should report 9 test succeeded events to the passed-in reporter when andThen with another 8 succeeded Test1") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              BeforeNode(3).andThen(
                Test1("test 1") { (i: Int) => i + 1 }.andThen(
                  Test1("test 2") { (i: Int) => succeed },
                  Test1("test 3") { (i: Int) => succeed },
                  Test1("test 4") { (i: Int) => succeed },
                  Test1("test 5") { (i: Int) => succeed },
                  Test1("test 6") { (i: Int) => succeed },
                  Test1("test 7") { (i: Int) => succeed },
                  Test1("test 8") { (i: Int) => succeed },
                  Test1("test 9") { (i: Int) => succeed }
                )
              )
          }
          assert(suite.testNames == Set("test 1", "test 2", "test 3", "test 4", "test 5", "test 6", "test 7", "test 8", "test 9"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 9)
          assert(testStarting(0).testName == "test 1")
          assert(testStarting(0).testText == "test 1")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 18, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "test 2")
          assert(testStarting(1).testText == "test 2")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 20, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "test 3")
          assert(testStarting(2).testText == "test 3")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 22, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(3).testName == "test 4")
          assert(testStarting(3).testText == "test 4")
          assert(testStarting(3).location == Some(LineInFile(thisLineNumber - 24, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(4).testName == "test 5")
          assert(testStarting(4).testText == "test 5")
          assert(testStarting(4).location == Some(LineInFile(thisLineNumber - 26, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(5).testName == "test 6")
          assert(testStarting(5).testText == "test 6")
          assert(testStarting(5).location == Some(LineInFile(thisLineNumber - 28, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(6).testName == "test 7")
          assert(testStarting(6).testText == "test 7")
          assert(testStarting(6).location == Some(LineInFile(thisLineNumber - 30, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(7).testName == "test 8")
          assert(testStarting(7).testText == "test 8")
          assert(testStarting(7).location == Some(LineInFile(thisLineNumber - 32, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(8).testName == "test 9")
          assert(testStarting(8).testText == "test 9")
          assert(testStarting(8).location == Some(LineInFile(thisLineNumber - 34, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 9)
          assert(testSucceeded(0).testName == "test 1")
          assert(testSucceeded(0).testText == "test 1")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 47, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- test 1", "test 1", 1)))
          assert(testSucceeded(1).testName == "test 2")
          assert(testSucceeded(1).testText == "test 2")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 50, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- test 2", "test 2", 1)))
          assert(testSucceeded(2).testName == "test 3")
          assert(testSucceeded(2).testText == "test 3")
          assert(testSucceeded(2).location == Some(LineInFile(thisLineNumber - 53, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(2).formatter == Some(IndentedText("- test 3", "test 3", 1)))
          assert(testSucceeded(3).testName == "test 4")
          assert(testSucceeded(3).testText == "test 4")
          assert(testSucceeded(3).location == Some(LineInFile(thisLineNumber - 56, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(3).formatter == Some(IndentedText("- test 4", "test 4", 1)))
          assert(testSucceeded(4).testName == "test 5")
          assert(testSucceeded(4).testText == "test 5")
          assert(testSucceeded(4).location == Some(LineInFile(thisLineNumber - 59, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(4).formatter == Some(IndentedText("- test 5", "test 5", 1)))
          assert(testSucceeded(5).testName == "test 6")
          assert(testSucceeded(5).testText == "test 6")
          assert(testSucceeded(5).location == Some(LineInFile(thisLineNumber - 62, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(5).formatter == Some(IndentedText("- test 6", "test 6", 1)))
          assert(testSucceeded(6).testName == "test 7")
          assert(testSucceeded(6).testText == "test 7")
          assert(testSucceeded(6).location == Some(LineInFile(thisLineNumber - 65, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(6).formatter == Some(IndentedText("- test 7", "test 7", 1)))
          assert(testSucceeded(7).testName == "test 8")
          assert(testSucceeded(7).testText == "test 8")
          assert(testSucceeded(7).location == Some(LineInFile(thisLineNumber - 68, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(7).formatter == Some(IndentedText("- test 8", "test 8", 1)))
          assert(testSucceeded(8).testName == "test 9")
          assert(testSucceeded(8).testText == "test 9")
          assert(testSucceeded(8).location == Some(LineInFile(thisLineNumber - 71, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(8).formatter == Some(IndentedText("- test 9", "test 9", 1)))
        }
        it("should report 10 test succeeded events to the passed-in reporter when andThen with another 9 succeeded Test1") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              BeforeNode(3).andThen(
                Test1("test 1") { (i: Int) => i + 1 }.andThen(
                  Test1("test 2") { (i: Int) => succeed },
                  Test1("test 3") { (i: Int) => succeed },
                  Test1("test 4") { (i: Int) => succeed },
                  Test1("test 5") { (i: Int) => succeed },
                  Test1("test 6") { (i: Int) => succeed },
                  Test1("test 7") { (i: Int) => succeed },
                  Test1("test 8") { (i: Int) => succeed },
                  Test1("test 9") { (i: Int) => succeed },
                  Test1("test 10") { (i: Int) => succeed }
                )
              )
          }
          assert(suite.testNames == Set("test 1", "test 2", "test 3", "test 4", "test 5", "test 6", "test 7", "test 8", "test 9", "test 10"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 10)
          assert(testStarting(0).testName == "test 1")
          assert(testStarting(0).testText == "test 1")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 19, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "test 2")
          assert(testStarting(1).testText == "test 2")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 21, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "test 3")
          assert(testStarting(2).testText == "test 3")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 23, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(3).testName == "test 4")
          assert(testStarting(3).testText == "test 4")
          assert(testStarting(3).location == Some(LineInFile(thisLineNumber - 25, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(4).testName == "test 5")
          assert(testStarting(4).testText == "test 5")
          assert(testStarting(4).location == Some(LineInFile(thisLineNumber - 27, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(5).testName == "test 6")
          assert(testStarting(5).testText == "test 6")
          assert(testStarting(5).location == Some(LineInFile(thisLineNumber - 29, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(6).testName == "test 7")
          assert(testStarting(6).testText == "test 7")
          assert(testStarting(6).location == Some(LineInFile(thisLineNumber - 31, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(7).testName == "test 8")
          assert(testStarting(7).testText == "test 8")
          assert(testStarting(7).location == Some(LineInFile(thisLineNumber - 33, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(8).testName == "test 9")
          assert(testStarting(8).testText == "test 9")
          assert(testStarting(8).location == Some(LineInFile(thisLineNumber - 35, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(9).testName == "test 10")
          assert(testStarting(9).testText == "test 10")
          assert(testStarting(9).location == Some(LineInFile(thisLineNumber - 37, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 10)
          assert(testSucceeded(0).testName == "test 1")
          assert(testSucceeded(0).testText == "test 1")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 51, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- test 1", "test 1", 1)))
          assert(testSucceeded(1).testName == "test 2")
          assert(testSucceeded(1).testText == "test 2")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 54, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- test 2", "test 2", 1)))
          assert(testSucceeded(2).testName == "test 3")
          assert(testSucceeded(2).testText == "test 3")
          assert(testSucceeded(2).location == Some(LineInFile(thisLineNumber - 57, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(2).formatter == Some(IndentedText("- test 3", "test 3", 1)))
          assert(testSucceeded(3).testName == "test 4")
          assert(testSucceeded(3).testText == "test 4")
          assert(testSucceeded(3).location == Some(LineInFile(thisLineNumber - 60, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(3).formatter == Some(IndentedText("- test 4", "test 4", 1)))
          assert(testSucceeded(4).testName == "test 5")
          assert(testSucceeded(4).testText == "test 5")
          assert(testSucceeded(4).location == Some(LineInFile(thisLineNumber - 63, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(4).formatter == Some(IndentedText("- test 5", "test 5", 1)))
          assert(testSucceeded(5).testName == "test 6")
          assert(testSucceeded(5).testText == "test 6")
          assert(testSucceeded(5).location == Some(LineInFile(thisLineNumber - 66, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(5).formatter == Some(IndentedText("- test 6", "test 6", 1)))
          assert(testSucceeded(6).testName == "test 7")
          assert(testSucceeded(6).testText == "test 7")
          assert(testSucceeded(6).location == Some(LineInFile(thisLineNumber - 69, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(6).formatter == Some(IndentedText("- test 7", "test 7", 1)))
          assert(testSucceeded(7).testName == "test 8")
          assert(testSucceeded(7).testText == "test 8")
          assert(testSucceeded(7).location == Some(LineInFile(thisLineNumber - 72, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(7).formatter == Some(IndentedText("- test 8", "test 8", 1)))
          assert(testSucceeded(8).testName == "test 9")
          assert(testSucceeded(8).testText == "test 9")
          assert(testSucceeded(8).location == Some(LineInFile(thisLineNumber - 75, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(8).formatter == Some(IndentedText("- test 9", "test 9", 1)))
          assert(testSucceeded(9).testName == "test 10")
          assert(testSucceeded(9).testText == "test 10")
          assert(testSucceeded(9).location == Some(LineInFile(thisLineNumber - 78, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(9).formatter == Some(IndentedText("- test 10", "test 10", 1)))
        }
        it("should report 11 test succeeded events to the passed-in reporter when andThen with another 10 succeeded Test1") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              BeforeNode(3).andThen(
                Test1("test 1") { (i: Int) => i + 1 }.andThen(
                  Test1("test 2") { (i: Int) => succeed },
                  Test1("test 3") { (i: Int) => succeed },
                  Test1("test 4") { (i: Int) => succeed },
                  Test1("test 5") { (i: Int) => succeed },
                  Test1("test 6") { (i: Int) => succeed },
                  Test1("test 7") { (i: Int) => succeed },
                  Test1("test 8") { (i: Int) => succeed },
                  Test1("test 9") { (i: Int) => succeed },
                  Test1("test 10") { (i: Int) => succeed },
                  Test1("test 11") { (i: Int) => succeed }
                )
              )
          }
          assert(suite.testNames == Set("test 1", "test 2", "test 3", "test 4", "test 5", "test 6", "test 7", "test 8", "test 9", "test 10",
                                        "test 11"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 11)
          assert(testStarting(0).testName == "test 1")
          assert(testStarting(0).testText == "test 1")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 21, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "test 2")
          assert(testStarting(1).testText == "test 2")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 23, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "test 3")
          assert(testStarting(2).testText == "test 3")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 25, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(3).testName == "test 4")
          assert(testStarting(3).testText == "test 4")
          assert(testStarting(3).location == Some(LineInFile(thisLineNumber - 27, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(4).testName == "test 5")
          assert(testStarting(4).testText == "test 5")
          assert(testStarting(4).location == Some(LineInFile(thisLineNumber - 29, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(5).testName == "test 6")
          assert(testStarting(5).testText == "test 6")
          assert(testStarting(5).location == Some(LineInFile(thisLineNumber - 31, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(6).testName == "test 7")
          assert(testStarting(6).testText == "test 7")
          assert(testStarting(6).location == Some(LineInFile(thisLineNumber - 33, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(7).testName == "test 8")
          assert(testStarting(7).testText == "test 8")
          assert(testStarting(7).location == Some(LineInFile(thisLineNumber - 35, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(8).testName == "test 9")
          assert(testStarting(8).testText == "test 9")
          assert(testStarting(8).location == Some(LineInFile(thisLineNumber - 37, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(9).testName == "test 10")
          assert(testStarting(9).testText == "test 10")
          assert(testStarting(9).location == Some(LineInFile(thisLineNumber - 39, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(10).testName == "test 11")
          assert(testStarting(10).testText == "test 11")
          assert(testStarting(10).location == Some(LineInFile(thisLineNumber - 41, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 11)
          assert(testSucceeded(0).testName == "test 1")
          assert(testSucceeded(0).testText == "test 1")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 56, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- test 1", "test 1", 1)))
          assert(testSucceeded(1).testName == "test 2")
          assert(testSucceeded(1).testText == "test 2")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 59, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- test 2", "test 2", 1)))
          assert(testSucceeded(2).testName == "test 3")
          assert(testSucceeded(2).testText == "test 3")
          assert(testSucceeded(2).location == Some(LineInFile(thisLineNumber - 62, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(2).formatter == Some(IndentedText("- test 3", "test 3", 1)))
          assert(testSucceeded(3).testName == "test 4")
          assert(testSucceeded(3).testText == "test 4")
          assert(testSucceeded(3).location == Some(LineInFile(thisLineNumber - 65, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(3).formatter == Some(IndentedText("- test 4", "test 4", 1)))
          assert(testSucceeded(4).testName == "test 5")
          assert(testSucceeded(4).testText == "test 5")
          assert(testSucceeded(4).location == Some(LineInFile(thisLineNumber - 68, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(4).formatter == Some(IndentedText("- test 5", "test 5", 1)))
          assert(testSucceeded(5).testName == "test 6")
          assert(testSucceeded(5).testText == "test 6")
          assert(testSucceeded(5).location == Some(LineInFile(thisLineNumber - 71, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(5).formatter == Some(IndentedText("- test 6", "test 6", 1)))
          assert(testSucceeded(6).testName == "test 7")
          assert(testSucceeded(6).testText == "test 7")
          assert(testSucceeded(6).location == Some(LineInFile(thisLineNumber - 74, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(6).formatter == Some(IndentedText("- test 7", "test 7", 1)))
          assert(testSucceeded(7).testName == "test 8")
          assert(testSucceeded(7).testText == "test 8")
          assert(testSucceeded(7).location == Some(LineInFile(thisLineNumber - 77, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(7).formatter == Some(IndentedText("- test 8", "test 8", 1)))
          assert(testSucceeded(8).testName == "test 9")
          assert(testSucceeded(8).testText == "test 9")
          assert(testSucceeded(8).location == Some(LineInFile(thisLineNumber - 80, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(8).formatter == Some(IndentedText("- test 9", "test 9", 1)))
          assert(testSucceeded(9).testName == "test 10")
          assert(testSucceeded(9).testText == "test 10")
          assert(testSucceeded(9).location == Some(LineInFile(thisLineNumber - 83, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(9).formatter == Some(IndentedText("- test 10", "test 10", 1)))
          assert(testSucceeded(10).testName == "test 11")
          assert(testSucceeded(10).testText == "test 11")
          assert(testSucceeded(10).location == Some(LineInFile(thisLineNumber - 86, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(10).formatter == Some(IndentedText("- test 11", "test 11", 1)))
        }
        it("should report 12 test succeeded events to the passed-in reporter when andThen with another 11 succeeded Test1") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              BeforeNode(3).andThen(
                Test1("test 1") { (i: Int) => i + 1 }.andThen(
                  Test1("test 2") { (i: Int) => succeed },
                  Test1("test 3") { (i: Int) => succeed },
                  Test1("test 4") { (i: Int) => succeed },
                  Test1("test 5") { (i: Int) => succeed },
                  Test1("test 6") { (i: Int) => succeed },
                  Test1("test 7") { (i: Int) => succeed },
                  Test1("test 8") { (i: Int) => succeed },
                  Test1("test 9") { (i: Int) => succeed },
                  Test1("test 10") { (i: Int) => succeed },
                  Test1("test 11") { (i: Int) => succeed },
                  Test1("test 12") { (i: Int) => succeed }
                )
              )
          }
          assert(suite.testNames == Set("test 1", "test 2", "test 3", "test 4", "test 5", "test 6", "test 7", "test 8", "test 9", "test 10",
                                        "test 11", "test 12"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 12)
          assert(testStarting(0).testName == "test 1")
          assert(testStarting(0).testText == "test 1")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 22, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "test 2")
          assert(testStarting(1).testText == "test 2")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 24, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "test 3")
          assert(testStarting(2).testText == "test 3")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 26, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(3).testName == "test 4")
          assert(testStarting(3).testText == "test 4")
          assert(testStarting(3).location == Some(LineInFile(thisLineNumber - 28, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(4).testName == "test 5")
          assert(testStarting(4).testText == "test 5")
          assert(testStarting(4).location == Some(LineInFile(thisLineNumber - 30, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(5).testName == "test 6")
          assert(testStarting(5).testText == "test 6")
          assert(testStarting(5).location == Some(LineInFile(thisLineNumber - 32, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(6).testName == "test 7")
          assert(testStarting(6).testText == "test 7")
          assert(testStarting(6).location == Some(LineInFile(thisLineNumber - 34, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(7).testName == "test 8")
          assert(testStarting(7).testText == "test 8")
          assert(testStarting(7).location == Some(LineInFile(thisLineNumber - 36, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(8).testName == "test 9")
          assert(testStarting(8).testText == "test 9")
          assert(testStarting(8).location == Some(LineInFile(thisLineNumber - 38, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(9).testName == "test 10")
          assert(testStarting(9).testText == "test 10")
          assert(testStarting(9).location == Some(LineInFile(thisLineNumber - 40, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(10).testName == "test 11")
          assert(testStarting(10).testText == "test 11")
          assert(testStarting(10).location == Some(LineInFile(thisLineNumber - 42, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(11).testName == "test 12")
          assert(testStarting(11).testText == "test 12")
          assert(testStarting(11).location == Some(LineInFile(thisLineNumber - 44, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 12)
          assert(testSucceeded(0).testName == "test 1")
          assert(testSucceeded(0).testText == "test 1")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 60, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- test 1", "test 1", 1)))
          assert(testSucceeded(1).testName == "test 2")
          assert(testSucceeded(1).testText == "test 2")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 63, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- test 2", "test 2", 1)))
          assert(testSucceeded(2).testName == "test 3")
          assert(testSucceeded(2).testText == "test 3")
          assert(testSucceeded(2).location == Some(LineInFile(thisLineNumber - 66, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(2).formatter == Some(IndentedText("- test 3", "test 3", 1)))
          assert(testSucceeded(3).testName == "test 4")
          assert(testSucceeded(3).testText == "test 4")
          assert(testSucceeded(3).location == Some(LineInFile(thisLineNumber - 69, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(3).formatter == Some(IndentedText("- test 4", "test 4", 1)))
          assert(testSucceeded(4).testName == "test 5")
          assert(testSucceeded(4).testText == "test 5")
          assert(testSucceeded(4).location == Some(LineInFile(thisLineNumber - 72, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(4).formatter == Some(IndentedText("- test 5", "test 5", 1)))
          assert(testSucceeded(5).testName == "test 6")
          assert(testSucceeded(5).testText == "test 6")
          assert(testSucceeded(5).location == Some(LineInFile(thisLineNumber - 75, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(5).formatter == Some(IndentedText("- test 6", "test 6", 1)))
          assert(testSucceeded(6).testName == "test 7")
          assert(testSucceeded(6).testText == "test 7")
          assert(testSucceeded(6).location == Some(LineInFile(thisLineNumber - 78, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(6).formatter == Some(IndentedText("- test 7", "test 7", 1)))
          assert(testSucceeded(7).testName == "test 8")
          assert(testSucceeded(7).testText == "test 8")
          assert(testSucceeded(7).location == Some(LineInFile(thisLineNumber - 81, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(7).formatter == Some(IndentedText("- test 8", "test 8", 1)))
          assert(testSucceeded(8).testName == "test 9")
          assert(testSucceeded(8).testText == "test 9")
          assert(testSucceeded(8).location == Some(LineInFile(thisLineNumber - 84, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(8).formatter == Some(IndentedText("- test 9", "test 9", 1)))
          assert(testSucceeded(9).testName == "test 10")
          assert(testSucceeded(9).testText == "test 10")
          assert(testSucceeded(9).location == Some(LineInFile(thisLineNumber - 87, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(9).formatter == Some(IndentedText("- test 10", "test 10", 1)))
          assert(testSucceeded(10).testName == "test 11")
          assert(testSucceeded(10).testText == "test 11")
          assert(testSucceeded(10).location == Some(LineInFile(thisLineNumber - 90, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(10).formatter == Some(IndentedText("- test 11", "test 11", 1)))
          assert(testSucceeded(11).testName == "test 12")
          assert(testSucceeded(11).testText == "test 12")
          assert(testSucceeded(11).location == Some(LineInFile(thisLineNumber - 93, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(11).formatter == Some(IndentedText("- test 12", "test 12", 1)))
        }
        it("should report 13 test succeeded events to the passed-in reporter when andThen with another 12 succeeded Test1") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              BeforeNode(3).andThen(
                Test1("test 1") { (i: Int) => i + 1 }.andThen(
                  Test1("test 2") { (i: Int) => succeed },
                  Test1("test 3") { (i: Int) => succeed },
                  Test1("test 4") { (i: Int) => succeed },
                  Test1("test 5") { (i: Int) => succeed },
                  Test1("test 6") { (i: Int) => succeed },
                  Test1("test 7") { (i: Int) => succeed },
                  Test1("test 8") { (i: Int) => succeed },
                  Test1("test 9") { (i: Int) => succeed },
                  Test1("test 10") { (i: Int) => succeed },
                  Test1("test 11") { (i: Int) => succeed },
                  Test1("test 12") { (i: Int) => succeed },
                  Test1("test 13") { (i: Int) => succeed }
                )
              )
          }
          assert(suite.testNames == Set("test 1", "test 2", "test 3", "test 4", "test 5", "test 6", "test 7", "test 8", "test 9", "test 10",
            "test 11", "test 12", "test 13"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 13)
          assert(testStarting(0).testName == "test 1")
          assert(testStarting(0).testText == "test 1")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 23, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "test 2")
          assert(testStarting(1).testText == "test 2")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 25, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "test 3")
          assert(testStarting(2).testText == "test 3")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 27, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(3).testName == "test 4")
          assert(testStarting(3).testText == "test 4")
          assert(testStarting(3).location == Some(LineInFile(thisLineNumber - 29, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(4).testName == "test 5")
          assert(testStarting(4).testText == "test 5")
          assert(testStarting(4).location == Some(LineInFile(thisLineNumber - 31, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(5).testName == "test 6")
          assert(testStarting(5).testText == "test 6")
          assert(testStarting(5).location == Some(LineInFile(thisLineNumber - 33, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(6).testName == "test 7")
          assert(testStarting(6).testText == "test 7")
          assert(testStarting(6).location == Some(LineInFile(thisLineNumber - 35, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(7).testName == "test 8")
          assert(testStarting(7).testText == "test 8")
          assert(testStarting(7).location == Some(LineInFile(thisLineNumber - 37, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(8).testName == "test 9")
          assert(testStarting(8).testText == "test 9")
          assert(testStarting(8).location == Some(LineInFile(thisLineNumber - 39, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(9).testName == "test 10")
          assert(testStarting(9).testText == "test 10")
          assert(testStarting(9).location == Some(LineInFile(thisLineNumber - 41, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(10).testName == "test 11")
          assert(testStarting(10).testText == "test 11")
          assert(testStarting(10).location == Some(LineInFile(thisLineNumber - 43, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(11).testName == "test 12")
          assert(testStarting(11).testText == "test 12")
          assert(testStarting(11).location == Some(LineInFile(thisLineNumber - 45, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(12).testName == "test 13")
          assert(testStarting(12).testText == "test 13")
          assert(testStarting(12).location == Some(LineInFile(thisLineNumber - 47, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 13)
          assert(testSucceeded(0).testName == "test 1")
          assert(testSucceeded(0).testText == "test 1")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 64, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- test 1", "test 1", 1)))
          assert(testSucceeded(1).testName == "test 2")
          assert(testSucceeded(1).testText == "test 2")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 67, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- test 2", "test 2", 1)))
          assert(testSucceeded(2).testName == "test 3")
          assert(testSucceeded(2).testText == "test 3")
          assert(testSucceeded(2).location == Some(LineInFile(thisLineNumber - 70, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(2).formatter == Some(IndentedText("- test 3", "test 3", 1)))
          assert(testSucceeded(3).testName == "test 4")
          assert(testSucceeded(3).testText == "test 4")
          assert(testSucceeded(3).location == Some(LineInFile(thisLineNumber - 73, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(3).formatter == Some(IndentedText("- test 4", "test 4", 1)))
          assert(testSucceeded(4).testName == "test 5")
          assert(testSucceeded(4).testText == "test 5")
          assert(testSucceeded(4).location == Some(LineInFile(thisLineNumber - 76, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(4).formatter == Some(IndentedText("- test 5", "test 5", 1)))
          assert(testSucceeded(5).testName == "test 6")
          assert(testSucceeded(5).testText == "test 6")
          assert(testSucceeded(5).location == Some(LineInFile(thisLineNumber - 79, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(5).formatter == Some(IndentedText("- test 6", "test 6", 1)))
          assert(testSucceeded(6).testName == "test 7")
          assert(testSucceeded(6).testText == "test 7")
          assert(testSucceeded(6).location == Some(LineInFile(thisLineNumber - 82, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(6).formatter == Some(IndentedText("- test 7", "test 7", 1)))
          assert(testSucceeded(7).testName == "test 8")
          assert(testSucceeded(7).testText == "test 8")
          assert(testSucceeded(7).location == Some(LineInFile(thisLineNumber - 85, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(7).formatter == Some(IndentedText("- test 8", "test 8", 1)))
          assert(testSucceeded(8).testName == "test 9")
          assert(testSucceeded(8).testText == "test 9")
          assert(testSucceeded(8).location == Some(LineInFile(thisLineNumber - 88, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(8).formatter == Some(IndentedText("- test 9", "test 9", 1)))
          assert(testSucceeded(9).testName == "test 10")
          assert(testSucceeded(9).testText == "test 10")
          assert(testSucceeded(9).location == Some(LineInFile(thisLineNumber - 91, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(9).formatter == Some(IndentedText("- test 10", "test 10", 1)))
          assert(testSucceeded(10).testName == "test 11")
          assert(testSucceeded(10).testText == "test 11")
          assert(testSucceeded(10).location == Some(LineInFile(thisLineNumber - 94, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(10).formatter == Some(IndentedText("- test 11", "test 11", 1)))
          assert(testSucceeded(11).testName == "test 12")
          assert(testSucceeded(11).testText == "test 12")
          assert(testSucceeded(11).location == Some(LineInFile(thisLineNumber - 97, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(11).formatter == Some(IndentedText("- test 12", "test 12", 1)))
          assert(testSucceeded(12).testName == "test 13")
          assert(testSucceeded(12).testText == "test 13")
          assert(testSucceeded(12).location == Some(LineInFile(thisLineNumber - 100, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(12).formatter == Some(IndentedText("- test 13", "test 13", 1)))
        }
        it("should report 14 test succeeded events to the passed-in reporter when andThen with another 13 succeeded Test1") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              BeforeNode(3).andThen(
                Test1("test 1") { (i: Int) => i + 1 }.andThen(
                  Test1("test 2") { (i: Int) => succeed },
                  Test1("test 3") { (i: Int) => succeed },
                  Test1("test 4") { (i: Int) => succeed },
                  Test1("test 5") { (i: Int) => succeed },
                  Test1("test 6") { (i: Int) => succeed },
                  Test1("test 7") { (i: Int) => succeed },
                  Test1("test 8") { (i: Int) => succeed },
                  Test1("test 9") { (i: Int) => succeed },
                  Test1("test 10") { (i: Int) => succeed },
                  Test1("test 11") { (i: Int) => succeed },
                  Test1("test 12") { (i: Int) => succeed },
                  Test1("test 13") { (i: Int) => succeed },
                  Test1("test 14") { (i: Int) => succeed }
                )
              )
          }
          assert(suite.testNames == Set("test 1", "test 2", "test 3", "test 4", "test 5", "test 6", "test 7", "test 8", "test 9", "test 10",
            "test 11", "test 12", "test 13", "test 14"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 14)
          assert(testStarting(0).testName == "test 1")
          assert(testStarting(0).testText == "test 1")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 24, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "test 2")
          assert(testStarting(1).testText == "test 2")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 26, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "test 3")
          assert(testStarting(2).testText == "test 3")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 28, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(3).testName == "test 4")
          assert(testStarting(3).testText == "test 4")
          assert(testStarting(3).location == Some(LineInFile(thisLineNumber - 30, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(4).testName == "test 5")
          assert(testStarting(4).testText == "test 5")
          assert(testStarting(4).location == Some(LineInFile(thisLineNumber - 32, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(5).testName == "test 6")
          assert(testStarting(5).testText == "test 6")
          assert(testStarting(5).location == Some(LineInFile(thisLineNumber - 34, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(6).testName == "test 7")
          assert(testStarting(6).testText == "test 7")
          assert(testStarting(6).location == Some(LineInFile(thisLineNumber - 36, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(7).testName == "test 8")
          assert(testStarting(7).testText == "test 8")
          assert(testStarting(7).location == Some(LineInFile(thisLineNumber - 38, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(8).testName == "test 9")
          assert(testStarting(8).testText == "test 9")
          assert(testStarting(8).location == Some(LineInFile(thisLineNumber - 40, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(9).testName == "test 10")
          assert(testStarting(9).testText == "test 10")
          assert(testStarting(9).location == Some(LineInFile(thisLineNumber - 42, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(10).testName == "test 11")
          assert(testStarting(10).testText == "test 11")
          assert(testStarting(10).location == Some(LineInFile(thisLineNumber - 44, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(11).testName == "test 12")
          assert(testStarting(11).testText == "test 12")
          assert(testStarting(11).location == Some(LineInFile(thisLineNumber - 46, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(12).testName == "test 13")
          assert(testStarting(12).testText == "test 13")
          assert(testStarting(12).location == Some(LineInFile(thisLineNumber - 48, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(13).testName == "test 14")
          assert(testStarting(13).testText == "test 14")
          assert(testStarting(13).location == Some(LineInFile(thisLineNumber - 50, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 14)
          assert(testSucceeded(0).testName == "test 1")
          assert(testSucceeded(0).testText == "test 1")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 68, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- test 1", "test 1", 1)))
          assert(testSucceeded(1).testName == "test 2")
          assert(testSucceeded(1).testText == "test 2")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 71, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- test 2", "test 2", 1)))
          assert(testSucceeded(2).testName == "test 3")
          assert(testSucceeded(2).testText == "test 3")
          assert(testSucceeded(2).location == Some(LineInFile(thisLineNumber - 74, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(2).formatter == Some(IndentedText("- test 3", "test 3", 1)))
          assert(testSucceeded(3).testName == "test 4")
          assert(testSucceeded(3).testText == "test 4")
          assert(testSucceeded(3).location == Some(LineInFile(thisLineNumber - 77, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(3).formatter == Some(IndentedText("- test 4", "test 4", 1)))
          assert(testSucceeded(4).testName == "test 5")
          assert(testSucceeded(4).testText == "test 5")
          assert(testSucceeded(4).location == Some(LineInFile(thisLineNumber - 80, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(4).formatter == Some(IndentedText("- test 5", "test 5", 1)))
          assert(testSucceeded(5).testName == "test 6")
          assert(testSucceeded(5).testText == "test 6")
          assert(testSucceeded(5).location == Some(LineInFile(thisLineNumber - 83, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(5).formatter == Some(IndentedText("- test 6", "test 6", 1)))
          assert(testSucceeded(6).testName == "test 7")
          assert(testSucceeded(6).testText == "test 7")
          assert(testSucceeded(6).location == Some(LineInFile(thisLineNumber - 86, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(6).formatter == Some(IndentedText("- test 7", "test 7", 1)))
          assert(testSucceeded(7).testName == "test 8")
          assert(testSucceeded(7).testText == "test 8")
          assert(testSucceeded(7).location == Some(LineInFile(thisLineNumber - 89, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(7).formatter == Some(IndentedText("- test 8", "test 8", 1)))
          assert(testSucceeded(8).testName == "test 9")
          assert(testSucceeded(8).testText == "test 9")
          assert(testSucceeded(8).location == Some(LineInFile(thisLineNumber - 92, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(8).formatter == Some(IndentedText("- test 9", "test 9", 1)))
          assert(testSucceeded(9).testName == "test 10")
          assert(testSucceeded(9).testText == "test 10")
          assert(testSucceeded(9).location == Some(LineInFile(thisLineNumber - 95, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(9).formatter == Some(IndentedText("- test 10", "test 10", 1)))
          assert(testSucceeded(10).testName == "test 11")
          assert(testSucceeded(10).testText == "test 11")
          assert(testSucceeded(10).location == Some(LineInFile(thisLineNumber - 98, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(10).formatter == Some(IndentedText("- test 11", "test 11", 1)))
          assert(testSucceeded(11).testName == "test 12")
          assert(testSucceeded(11).testText == "test 12")
          assert(testSucceeded(11).location == Some(LineInFile(thisLineNumber - 101, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(11).formatter == Some(IndentedText("- test 12", "test 12", 1)))
          assert(testSucceeded(12).testName == "test 13")
          assert(testSucceeded(12).testText == "test 13")
          assert(testSucceeded(12).location == Some(LineInFile(thisLineNumber - 104, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(12).formatter == Some(IndentedText("- test 13", "test 13", 1)))
          assert(testSucceeded(13).testName == "test 14")
          assert(testSucceeded(13).testText == "test 14")
          assert(testSucceeded(13).location == Some(LineInFile(thisLineNumber - 107, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(13).formatter == Some(IndentedText("- test 14", "test 14", 1)))
        }
        it("should report 15 test succeeded events to the passed-in reporter when andThen with another 14 succeeded Test1") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              BeforeNode(3).andThen(
                Test1("test 1") { (i: Int) => i + 1 }.andThen(
                  Test1("test 2") { (i: Int) => succeed },
                  Test1("test 3") { (i: Int) => succeed },
                  Test1("test 4") { (i: Int) => succeed },
                  Test1("test 5") { (i: Int) => succeed },
                  Test1("test 6") { (i: Int) => succeed },
                  Test1("test 7") { (i: Int) => succeed },
                  Test1("test 8") { (i: Int) => succeed },
                  Test1("test 9") { (i: Int) => succeed },
                  Test1("test 10") { (i: Int) => succeed },
                  Test1("test 11") { (i: Int) => succeed },
                  Test1("test 12") { (i: Int) => succeed },
                  Test1("test 13") { (i: Int) => succeed },
                  Test1("test 14") { (i: Int) => succeed },
                  Test1("test 15") { (i: Int) => succeed }
                )
              )
          }
          assert(suite.testNames == Set("test 1", "test 2", "test 3", "test 4", "test 5", "test 6", "test 7", "test 8", "test 9", "test 10",
            "test 11", "test 12", "test 13", "test 14", "test 15"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 15)
          assert(testStarting(0).testName == "test 1")
          assert(testStarting(0).testText == "test 1")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 25, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "test 2")
          assert(testStarting(1).testText == "test 2")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 27, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "test 3")
          assert(testStarting(2).testText == "test 3")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 29, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(3).testName == "test 4")
          assert(testStarting(3).testText == "test 4")
          assert(testStarting(3).location == Some(LineInFile(thisLineNumber - 31, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(4).testName == "test 5")
          assert(testStarting(4).testText == "test 5")
          assert(testStarting(4).location == Some(LineInFile(thisLineNumber - 33, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(5).testName == "test 6")
          assert(testStarting(5).testText == "test 6")
          assert(testStarting(5).location == Some(LineInFile(thisLineNumber - 35, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(6).testName == "test 7")
          assert(testStarting(6).testText == "test 7")
          assert(testStarting(6).location == Some(LineInFile(thisLineNumber - 37, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(7).testName == "test 8")
          assert(testStarting(7).testText == "test 8")
          assert(testStarting(7).location == Some(LineInFile(thisLineNumber - 39, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(8).testName == "test 9")
          assert(testStarting(8).testText == "test 9")
          assert(testStarting(8).location == Some(LineInFile(thisLineNumber - 41, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(9).testName == "test 10")
          assert(testStarting(9).testText == "test 10")
          assert(testStarting(9).location == Some(LineInFile(thisLineNumber - 43, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(10).testName == "test 11")
          assert(testStarting(10).testText == "test 11")
          assert(testStarting(10).location == Some(LineInFile(thisLineNumber - 45, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(11).testName == "test 12")
          assert(testStarting(11).testText == "test 12")
          assert(testStarting(11).location == Some(LineInFile(thisLineNumber - 47, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(12).testName == "test 13")
          assert(testStarting(12).testText == "test 13")
          assert(testStarting(12).location == Some(LineInFile(thisLineNumber - 49, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(13).testName == "test 14")
          assert(testStarting(13).testText == "test 14")
          assert(testStarting(13).location == Some(LineInFile(thisLineNumber - 51, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(14).testName == "test 15")
          assert(testStarting(14).testText == "test 15")
          assert(testStarting(14).location == Some(LineInFile(thisLineNumber - 53, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 15)
          assert(testSucceeded(0).testName == "test 1")
          assert(testSucceeded(0).testText == "test 1")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 72, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- test 1", "test 1", 1)))
          assert(testSucceeded(1).testName == "test 2")
          assert(testSucceeded(1).testText == "test 2")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 75, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- test 2", "test 2", 1)))
          assert(testSucceeded(2).testName == "test 3")
          assert(testSucceeded(2).testText == "test 3")
          assert(testSucceeded(2).location == Some(LineInFile(thisLineNumber - 78, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(2).formatter == Some(IndentedText("- test 3", "test 3", 1)))
          assert(testSucceeded(3).testName == "test 4")
          assert(testSucceeded(3).testText == "test 4")
          assert(testSucceeded(3).location == Some(LineInFile(thisLineNumber - 81, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(3).formatter == Some(IndentedText("- test 4", "test 4", 1)))
          assert(testSucceeded(4).testName == "test 5")
          assert(testSucceeded(4).testText == "test 5")
          assert(testSucceeded(4).location == Some(LineInFile(thisLineNumber - 84, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(4).formatter == Some(IndentedText("- test 5", "test 5", 1)))
          assert(testSucceeded(5).testName == "test 6")
          assert(testSucceeded(5).testText == "test 6")
          assert(testSucceeded(5).location == Some(LineInFile(thisLineNumber - 87, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(5).formatter == Some(IndentedText("- test 6", "test 6", 1)))
          assert(testSucceeded(6).testName == "test 7")
          assert(testSucceeded(6).testText == "test 7")
          assert(testSucceeded(6).location == Some(LineInFile(thisLineNumber - 90, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(6).formatter == Some(IndentedText("- test 7", "test 7", 1)))
          assert(testSucceeded(7).testName == "test 8")
          assert(testSucceeded(7).testText == "test 8")
          assert(testSucceeded(7).location == Some(LineInFile(thisLineNumber - 93, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(7).formatter == Some(IndentedText("- test 8", "test 8", 1)))
          assert(testSucceeded(8).testName == "test 9")
          assert(testSucceeded(8).testText == "test 9")
          assert(testSucceeded(8).location == Some(LineInFile(thisLineNumber - 96, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(8).formatter == Some(IndentedText("- test 9", "test 9", 1)))
          assert(testSucceeded(9).testName == "test 10")
          assert(testSucceeded(9).testText == "test 10")
          assert(testSucceeded(9).location == Some(LineInFile(thisLineNumber - 99, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(9).formatter == Some(IndentedText("- test 10", "test 10", 1)))
          assert(testSucceeded(10).testName == "test 11")
          assert(testSucceeded(10).testText == "test 11")
          assert(testSucceeded(10).location == Some(LineInFile(thisLineNumber - 102, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(10).formatter == Some(IndentedText("- test 11", "test 11", 1)))
          assert(testSucceeded(11).testName == "test 12")
          assert(testSucceeded(11).testText == "test 12")
          assert(testSucceeded(11).location == Some(LineInFile(thisLineNumber - 105, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(11).formatter == Some(IndentedText("- test 12", "test 12", 1)))
          assert(testSucceeded(12).testName == "test 13")
          assert(testSucceeded(12).testText == "test 13")
          assert(testSucceeded(12).location == Some(LineInFile(thisLineNumber - 108, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(12).formatter == Some(IndentedText("- test 13", "test 13", 1)))
          assert(testSucceeded(13).testName == "test 14")
          assert(testSucceeded(13).testText == "test 14")
          assert(testSucceeded(13).location == Some(LineInFile(thisLineNumber - 111, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(13).formatter == Some(IndentedText("- test 14", "test 14", 1)))
          assert(testSucceeded(14).testName == "test 15")
          assert(testSucceeded(14).testText == "test 15")
          assert(testSucceeded(14).location == Some(LineInFile(thisLineNumber - 114, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(14).formatter == Some(IndentedText("- test 15", "test 15", 1)))
        }
        it("should report 16 test succeeded events to the passed-in reporter when andThen with another 15 succeeded Test1") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              BeforeNode(3).andThen(
                Test1("test 1") { (i: Int) => i + 1 }.andThen(
                  Test1("test 2") { (i: Int) => succeed },
                  Test1("test 3") { (i: Int) => succeed },
                  Test1("test 4") { (i: Int) => succeed },
                  Test1("test 5") { (i: Int) => succeed },
                  Test1("test 6") { (i: Int) => succeed },
                  Test1("test 7") { (i: Int) => succeed },
                  Test1("test 8") { (i: Int) => succeed },
                  Test1("test 9") { (i: Int) => succeed },
                  Test1("test 10") { (i: Int) => succeed },
                  Test1("test 11") { (i: Int) => succeed },
                  Test1("test 12") { (i: Int) => succeed },
                  Test1("test 13") { (i: Int) => succeed },
                  Test1("test 14") { (i: Int) => succeed },
                  Test1("test 15") { (i: Int) => succeed },
                  Test1("test 16") { (i: Int) => succeed }
                )
              )
          }
          assert(suite.testNames == Set("test 1", "test 2", "test 3", "test 4", "test 5", "test 6", "test 7", "test 8", "test 9", "test 10",
            "test 11", "test 12", "test 13", "test 14", "test 15", "test 16"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 16)
          assert(testStarting(0).testName == "test 1")
          assert(testStarting(0).testText == "test 1")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 26, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "test 2")
          assert(testStarting(1).testText == "test 2")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 28, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "test 3")
          assert(testStarting(2).testText == "test 3")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 30, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(3).testName == "test 4")
          assert(testStarting(3).testText == "test 4")
          assert(testStarting(3).location == Some(LineInFile(thisLineNumber - 32, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(4).testName == "test 5")
          assert(testStarting(4).testText == "test 5")
          assert(testStarting(4).location == Some(LineInFile(thisLineNumber - 34, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(5).testName == "test 6")
          assert(testStarting(5).testText == "test 6")
          assert(testStarting(5).location == Some(LineInFile(thisLineNumber - 36, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(6).testName == "test 7")
          assert(testStarting(6).testText == "test 7")
          assert(testStarting(6).location == Some(LineInFile(thisLineNumber - 38, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(7).testName == "test 8")
          assert(testStarting(7).testText == "test 8")
          assert(testStarting(7).location == Some(LineInFile(thisLineNumber - 40, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(8).testName == "test 9")
          assert(testStarting(8).testText == "test 9")
          assert(testStarting(8).location == Some(LineInFile(thisLineNumber - 42, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(9).testName == "test 10")
          assert(testStarting(9).testText == "test 10")
          assert(testStarting(9).location == Some(LineInFile(thisLineNumber - 44, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(10).testName == "test 11")
          assert(testStarting(10).testText == "test 11")
          assert(testStarting(10).location == Some(LineInFile(thisLineNumber - 46, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(11).testName == "test 12")
          assert(testStarting(11).testText == "test 12")
          assert(testStarting(11).location == Some(LineInFile(thisLineNumber - 48, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(12).testName == "test 13")
          assert(testStarting(12).testText == "test 13")
          assert(testStarting(12).location == Some(LineInFile(thisLineNumber - 50, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(13).testName == "test 14")
          assert(testStarting(13).testText == "test 14")
          assert(testStarting(13).location == Some(LineInFile(thisLineNumber - 52, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(14).testName == "test 15")
          assert(testStarting(14).testText == "test 15")
          assert(testStarting(14).location == Some(LineInFile(thisLineNumber - 54, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(15).testName == "test 16")
          assert(testStarting(15).testText == "test 16")
          assert(testStarting(15).location == Some(LineInFile(thisLineNumber - 56, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 16)
          assert(testSucceeded(0).testName == "test 1")
          assert(testSucceeded(0).testText == "test 1")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 76, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- test 1", "test 1", 1)))
          assert(testSucceeded(1).testName == "test 2")
          assert(testSucceeded(1).testText == "test 2")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 79, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- test 2", "test 2", 1)))
          assert(testSucceeded(2).testName == "test 3")
          assert(testSucceeded(2).testText == "test 3")
          assert(testSucceeded(2).location == Some(LineInFile(thisLineNumber - 82, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(2).formatter == Some(IndentedText("- test 3", "test 3", 1)))
          assert(testSucceeded(3).testName == "test 4")
          assert(testSucceeded(3).testText == "test 4")
          assert(testSucceeded(3).location == Some(LineInFile(thisLineNumber - 85, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(3).formatter == Some(IndentedText("- test 4", "test 4", 1)))
          assert(testSucceeded(4).testName == "test 5")
          assert(testSucceeded(4).testText == "test 5")
          assert(testSucceeded(4).location == Some(LineInFile(thisLineNumber - 88, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(4).formatter == Some(IndentedText("- test 5", "test 5", 1)))
          assert(testSucceeded(5).testName == "test 6")
          assert(testSucceeded(5).testText == "test 6")
          assert(testSucceeded(5).location == Some(LineInFile(thisLineNumber - 91, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(5).formatter == Some(IndentedText("- test 6", "test 6", 1)))
          assert(testSucceeded(6).testName == "test 7")
          assert(testSucceeded(6).testText == "test 7")
          assert(testSucceeded(6).location == Some(LineInFile(thisLineNumber - 94, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(6).formatter == Some(IndentedText("- test 7", "test 7", 1)))
          assert(testSucceeded(7).testName == "test 8")
          assert(testSucceeded(7).testText == "test 8")
          assert(testSucceeded(7).location == Some(LineInFile(thisLineNumber - 97, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(7).formatter == Some(IndentedText("- test 8", "test 8", 1)))
          assert(testSucceeded(8).testName == "test 9")
          assert(testSucceeded(8).testText == "test 9")
          assert(testSucceeded(8).location == Some(LineInFile(thisLineNumber - 100, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(8).formatter == Some(IndentedText("- test 9", "test 9", 1)))
          assert(testSucceeded(9).testName == "test 10")
          assert(testSucceeded(9).testText == "test 10")
          assert(testSucceeded(9).location == Some(LineInFile(thisLineNumber - 103, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(9).formatter == Some(IndentedText("- test 10", "test 10", 1)))
          assert(testSucceeded(10).testName == "test 11")
          assert(testSucceeded(10).testText == "test 11")
          assert(testSucceeded(10).location == Some(LineInFile(thisLineNumber - 106, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(10).formatter == Some(IndentedText("- test 11", "test 11", 1)))
          assert(testSucceeded(11).testName == "test 12")
          assert(testSucceeded(11).testText == "test 12")
          assert(testSucceeded(11).location == Some(LineInFile(thisLineNumber - 109, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(11).formatter == Some(IndentedText("- test 12", "test 12", 1)))
          assert(testSucceeded(12).testName == "test 13")
          assert(testSucceeded(12).testText == "test 13")
          assert(testSucceeded(12).location == Some(LineInFile(thisLineNumber - 112, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(12).formatter == Some(IndentedText("- test 13", "test 13", 1)))
          assert(testSucceeded(13).testName == "test 14")
          assert(testSucceeded(13).testText == "test 14")
          assert(testSucceeded(13).location == Some(LineInFile(thisLineNumber - 115, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(13).formatter == Some(IndentedText("- test 14", "test 14", 1)))
          assert(testSucceeded(14).testName == "test 15")
          assert(testSucceeded(14).testText == "test 15")
          assert(testSucceeded(14).location == Some(LineInFile(thisLineNumber - 118, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(14).formatter == Some(IndentedText("- test 15", "test 15", 1)))
          assert(testSucceeded(15).testName == "test 16")
          assert(testSucceeded(15).testText == "test 16")
          assert(testSucceeded(15).location == Some(LineInFile(thisLineNumber - 121, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(15).formatter == Some(IndentedText("- test 16", "test 16", 1)))
        }
        it("should report 17 test succeeded events to the passed-in reporter when andThen with another 16 succeeded Test1") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              BeforeNode(3).andThen(
                Test1("test 1") { (i: Int) => i + 1 }.andThen(
                  Test1("test 2") { (i: Int) => succeed },
                  Test1("test 3") { (i: Int) => succeed },
                  Test1("test 4") { (i: Int) => succeed },
                  Test1("test 5") { (i: Int) => succeed },
                  Test1("test 6") { (i: Int) => succeed },
                  Test1("test 7") { (i: Int) => succeed },
                  Test1("test 8") { (i: Int) => succeed },
                  Test1("test 9") { (i: Int) => succeed },
                  Test1("test 10") { (i: Int) => succeed },
                  Test1("test 11") { (i: Int) => succeed },
                  Test1("test 12") { (i: Int) => succeed },
                  Test1("test 13") { (i: Int) => succeed },
                  Test1("test 14") { (i: Int) => succeed },
                  Test1("test 15") { (i: Int) => succeed },
                  Test1("test 16") { (i: Int) => succeed },
                  Test1("test 17") { (i: Int) => succeed }
                )
              )
          }
          assert(suite.testNames == Set("test 1", "test 2", "test 3", "test 4", "test 5", "test 6", "test 7", "test 8", "test 9", "test 10",
            "test 11", "test 12", "test 13", "test 14", "test 15", "test 16", "test 17"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 17)
          assert(testStarting(0).testName == "test 1")
          assert(testStarting(0).testText == "test 1")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 27, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "test 2")
          assert(testStarting(1).testText == "test 2")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 29, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "test 3")
          assert(testStarting(2).testText == "test 3")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 31, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(3).testName == "test 4")
          assert(testStarting(3).testText == "test 4")
          assert(testStarting(3).location == Some(LineInFile(thisLineNumber - 33, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(4).testName == "test 5")
          assert(testStarting(4).testText == "test 5")
          assert(testStarting(4).location == Some(LineInFile(thisLineNumber - 35, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(5).testName == "test 6")
          assert(testStarting(5).testText == "test 6")
          assert(testStarting(5).location == Some(LineInFile(thisLineNumber - 37, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(6).testName == "test 7")
          assert(testStarting(6).testText == "test 7")
          assert(testStarting(6).location == Some(LineInFile(thisLineNumber - 39, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(7).testName == "test 8")
          assert(testStarting(7).testText == "test 8")
          assert(testStarting(7).location == Some(LineInFile(thisLineNumber - 41, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(8).testName == "test 9")
          assert(testStarting(8).testText == "test 9")
          assert(testStarting(8).location == Some(LineInFile(thisLineNumber - 43, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(9).testName == "test 10")
          assert(testStarting(9).testText == "test 10")
          assert(testStarting(9).location == Some(LineInFile(thisLineNumber - 45, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(10).testName == "test 11")
          assert(testStarting(10).testText == "test 11")
          assert(testStarting(10).location == Some(LineInFile(thisLineNumber - 47, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(11).testName == "test 12")
          assert(testStarting(11).testText == "test 12")
          assert(testStarting(11).location == Some(LineInFile(thisLineNumber - 49, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(12).testName == "test 13")
          assert(testStarting(12).testText == "test 13")
          assert(testStarting(12).location == Some(LineInFile(thisLineNumber - 51, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(13).testName == "test 14")
          assert(testStarting(13).testText == "test 14")
          assert(testStarting(13).location == Some(LineInFile(thisLineNumber - 53, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(14).testName == "test 15")
          assert(testStarting(14).testText == "test 15")
          assert(testStarting(14).location == Some(LineInFile(thisLineNumber - 55, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(15).testName == "test 16")
          assert(testStarting(15).testText == "test 16")
          assert(testStarting(15).location == Some(LineInFile(thisLineNumber - 57, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(16).testName == "test 17")
          assert(testStarting(16).testText == "test 17")
          assert(testStarting(16).location == Some(LineInFile(thisLineNumber - 59, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 17)
          assert(testSucceeded(0).testName == "test 1")
          assert(testSucceeded(0).testText == "test 1")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 80, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- test 1", "test 1", 1)))
          assert(testSucceeded(1).testName == "test 2")
          assert(testSucceeded(1).testText == "test 2")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 83, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- test 2", "test 2", 1)))
          assert(testSucceeded(2).testName == "test 3")
          assert(testSucceeded(2).testText == "test 3")
          assert(testSucceeded(2).location == Some(LineInFile(thisLineNumber - 86, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(2).formatter == Some(IndentedText("- test 3", "test 3", 1)))
          assert(testSucceeded(3).testName == "test 4")
          assert(testSucceeded(3).testText == "test 4")
          assert(testSucceeded(3).location == Some(LineInFile(thisLineNumber - 89, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(3).formatter == Some(IndentedText("- test 4", "test 4", 1)))
          assert(testSucceeded(4).testName == "test 5")
          assert(testSucceeded(4).testText == "test 5")
          assert(testSucceeded(4).location == Some(LineInFile(thisLineNumber - 92, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(4).formatter == Some(IndentedText("- test 5", "test 5", 1)))
          assert(testSucceeded(5).testName == "test 6")
          assert(testSucceeded(5).testText == "test 6")
          assert(testSucceeded(5).location == Some(LineInFile(thisLineNumber - 95, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(5).formatter == Some(IndentedText("- test 6", "test 6", 1)))
          assert(testSucceeded(6).testName == "test 7")
          assert(testSucceeded(6).testText == "test 7")
          assert(testSucceeded(6).location == Some(LineInFile(thisLineNumber - 98, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(6).formatter == Some(IndentedText("- test 7", "test 7", 1)))
          assert(testSucceeded(7).testName == "test 8")
          assert(testSucceeded(7).testText == "test 8")
          assert(testSucceeded(7).location == Some(LineInFile(thisLineNumber - 101, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(7).formatter == Some(IndentedText("- test 8", "test 8", 1)))
          assert(testSucceeded(8).testName == "test 9")
          assert(testSucceeded(8).testText == "test 9")
          assert(testSucceeded(8).location == Some(LineInFile(thisLineNumber - 104, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(8).formatter == Some(IndentedText("- test 9", "test 9", 1)))
          assert(testSucceeded(9).testName == "test 10")
          assert(testSucceeded(9).testText == "test 10")
          assert(testSucceeded(9).location == Some(LineInFile(thisLineNumber - 107, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(9).formatter == Some(IndentedText("- test 10", "test 10", 1)))
          assert(testSucceeded(10).testName == "test 11")
          assert(testSucceeded(10).testText == "test 11")
          assert(testSucceeded(10).location == Some(LineInFile(thisLineNumber - 110, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(10).formatter == Some(IndentedText("- test 11", "test 11", 1)))
          assert(testSucceeded(11).testName == "test 12")
          assert(testSucceeded(11).testText == "test 12")
          assert(testSucceeded(11).location == Some(LineInFile(thisLineNumber - 113, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(11).formatter == Some(IndentedText("- test 12", "test 12", 1)))
          assert(testSucceeded(12).testName == "test 13")
          assert(testSucceeded(12).testText == "test 13")
          assert(testSucceeded(12).location == Some(LineInFile(thisLineNumber - 116, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(12).formatter == Some(IndentedText("- test 13", "test 13", 1)))
          assert(testSucceeded(13).testName == "test 14")
          assert(testSucceeded(13).testText == "test 14")
          assert(testSucceeded(13).location == Some(LineInFile(thisLineNumber - 119, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(13).formatter == Some(IndentedText("- test 14", "test 14", 1)))
          assert(testSucceeded(14).testName == "test 15")
          assert(testSucceeded(14).testText == "test 15")
          assert(testSucceeded(14).location == Some(LineInFile(thisLineNumber - 122, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(14).formatter == Some(IndentedText("- test 15", "test 15", 1)))
          assert(testSucceeded(15).testName == "test 16")
          assert(testSucceeded(15).testText == "test 16")
          assert(testSucceeded(15).location == Some(LineInFile(thisLineNumber - 125, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(15).formatter == Some(IndentedText("- test 16", "test 16", 1)))
          assert(testSucceeded(16).testName == "test 17")
          assert(testSucceeded(16).testText == "test 17")
          assert(testSucceeded(16).location == Some(LineInFile(thisLineNumber - 128, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(16).formatter == Some(IndentedText("- test 17", "test 17", 1)))
        }
        it("should report 18 test succeeded events to the passed-in reporter when andThen with another 17 succeeded Test1") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              BeforeNode(3).andThen(
                Test1("test 1") { (i: Int) => i + 1 }.andThen(
                  Test1("test 2") { (i: Int) => succeed },
                  Test1("test 3") { (i: Int) => succeed },
                  Test1("test 4") { (i: Int) => succeed },
                  Test1("test 5") { (i: Int) => succeed },
                  Test1("test 6") { (i: Int) => succeed },
                  Test1("test 7") { (i: Int) => succeed },
                  Test1("test 8") { (i: Int) => succeed },
                  Test1("test 9") { (i: Int) => succeed },
                  Test1("test 10") { (i: Int) => succeed },
                  Test1("test 11") { (i: Int) => succeed },
                  Test1("test 12") { (i: Int) => succeed },
                  Test1("test 13") { (i: Int) => succeed },
                  Test1("test 14") { (i: Int) => succeed },
                  Test1("test 15") { (i: Int) => succeed },
                  Test1("test 16") { (i: Int) => succeed },
                  Test1("test 17") { (i: Int) => succeed },
                  Test1("test 18") { (i: Int) => succeed }
                )
              )
          }
          assert(suite.testNames == Set("test 1", "test 2", "test 3", "test 4", "test 5", "test 6", "test 7", "test 8", "test 9", "test 10",
            "test 11", "test 12", "test 13", "test 14", "test 15", "test 16", "test 17", "test 18"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 18)
          assert(testStarting(0).testName == "test 1")
          assert(testStarting(0).testText == "test 1")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 28, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "test 2")
          assert(testStarting(1).testText == "test 2")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 30, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "test 3")
          assert(testStarting(2).testText == "test 3")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 32, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(3).testName == "test 4")
          assert(testStarting(3).testText == "test 4")
          assert(testStarting(3).location == Some(LineInFile(thisLineNumber - 34, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(4).testName == "test 5")
          assert(testStarting(4).testText == "test 5")
          assert(testStarting(4).location == Some(LineInFile(thisLineNumber - 36, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(5).testName == "test 6")
          assert(testStarting(5).testText == "test 6")
          assert(testStarting(5).location == Some(LineInFile(thisLineNumber - 38, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(6).testName == "test 7")
          assert(testStarting(6).testText == "test 7")
          assert(testStarting(6).location == Some(LineInFile(thisLineNumber - 40, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(7).testName == "test 8")
          assert(testStarting(7).testText == "test 8")
          assert(testStarting(7).location == Some(LineInFile(thisLineNumber - 42, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(8).testName == "test 9")
          assert(testStarting(8).testText == "test 9")
          assert(testStarting(8).location == Some(LineInFile(thisLineNumber - 44, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(9).testName == "test 10")
          assert(testStarting(9).testText == "test 10")
          assert(testStarting(9).location == Some(LineInFile(thisLineNumber - 46, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(10).testName == "test 11")
          assert(testStarting(10).testText == "test 11")
          assert(testStarting(10).location == Some(LineInFile(thisLineNumber - 48, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(11).testName == "test 12")
          assert(testStarting(11).testText == "test 12")
          assert(testStarting(11).location == Some(LineInFile(thisLineNumber - 50, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(12).testName == "test 13")
          assert(testStarting(12).testText == "test 13")
          assert(testStarting(12).location == Some(LineInFile(thisLineNumber - 52, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(13).testName == "test 14")
          assert(testStarting(13).testText == "test 14")
          assert(testStarting(13).location == Some(LineInFile(thisLineNumber - 54, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(14).testName == "test 15")
          assert(testStarting(14).testText == "test 15")
          assert(testStarting(14).location == Some(LineInFile(thisLineNumber - 56, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(15).testName == "test 16")
          assert(testStarting(15).testText == "test 16")
          assert(testStarting(15).location == Some(LineInFile(thisLineNumber - 58, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(16).testName == "test 17")
          assert(testStarting(16).testText == "test 17")
          assert(testStarting(16).location == Some(LineInFile(thisLineNumber - 60, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(17).testName == "test 18")
          assert(testStarting(17).testText == "test 18")
          assert(testStarting(17).location == Some(LineInFile(thisLineNumber - 62, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 18)
          assert(testSucceeded(0).testName == "test 1")
          assert(testSucceeded(0).testText == "test 1")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 84, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- test 1", "test 1", 1)))
          assert(testSucceeded(1).testName == "test 2")
          assert(testSucceeded(1).testText == "test 2")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 87, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- test 2", "test 2", 1)))
          assert(testSucceeded(2).testName == "test 3")
          assert(testSucceeded(2).testText == "test 3")
          assert(testSucceeded(2).location == Some(LineInFile(thisLineNumber - 90, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(2).formatter == Some(IndentedText("- test 3", "test 3", 1)))
          assert(testSucceeded(3).testName == "test 4")
          assert(testSucceeded(3).testText == "test 4")
          assert(testSucceeded(3).location == Some(LineInFile(thisLineNumber - 93, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(3).formatter == Some(IndentedText("- test 4", "test 4", 1)))
          assert(testSucceeded(4).testName == "test 5")
          assert(testSucceeded(4).testText == "test 5")
          assert(testSucceeded(4).location == Some(LineInFile(thisLineNumber - 96, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(4).formatter == Some(IndentedText("- test 5", "test 5", 1)))
          assert(testSucceeded(5).testName == "test 6")
          assert(testSucceeded(5).testText == "test 6")
          assert(testSucceeded(5).location == Some(LineInFile(thisLineNumber - 99, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(5).formatter == Some(IndentedText("- test 6", "test 6", 1)))
          assert(testSucceeded(6).testName == "test 7")
          assert(testSucceeded(6).testText == "test 7")
          assert(testSucceeded(6).location == Some(LineInFile(thisLineNumber - 102, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(6).formatter == Some(IndentedText("- test 7", "test 7", 1)))
          assert(testSucceeded(7).testName == "test 8")
          assert(testSucceeded(7).testText == "test 8")
          assert(testSucceeded(7).location == Some(LineInFile(thisLineNumber - 105, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(7).formatter == Some(IndentedText("- test 8", "test 8", 1)))
          assert(testSucceeded(8).testName == "test 9")
          assert(testSucceeded(8).testText == "test 9")
          assert(testSucceeded(8).location == Some(LineInFile(thisLineNumber - 108, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(8).formatter == Some(IndentedText("- test 9", "test 9", 1)))
          assert(testSucceeded(9).testName == "test 10")
          assert(testSucceeded(9).testText == "test 10")
          assert(testSucceeded(9).location == Some(LineInFile(thisLineNumber - 111, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(9).formatter == Some(IndentedText("- test 10", "test 10", 1)))
          assert(testSucceeded(10).testName == "test 11")
          assert(testSucceeded(10).testText == "test 11")
          assert(testSucceeded(10).location == Some(LineInFile(thisLineNumber - 114, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(10).formatter == Some(IndentedText("- test 11", "test 11", 1)))
          assert(testSucceeded(11).testName == "test 12")
          assert(testSucceeded(11).testText == "test 12")
          assert(testSucceeded(11).location == Some(LineInFile(thisLineNumber - 117, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(11).formatter == Some(IndentedText("- test 12", "test 12", 1)))
          assert(testSucceeded(12).testName == "test 13")
          assert(testSucceeded(12).testText == "test 13")
          assert(testSucceeded(12).location == Some(LineInFile(thisLineNumber - 120, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(12).formatter == Some(IndentedText("- test 13", "test 13", 1)))
          assert(testSucceeded(13).testName == "test 14")
          assert(testSucceeded(13).testText == "test 14")
          assert(testSucceeded(13).location == Some(LineInFile(thisLineNumber - 123, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(13).formatter == Some(IndentedText("- test 14", "test 14", 1)))
          assert(testSucceeded(14).testName == "test 15")
          assert(testSucceeded(14).testText == "test 15")
          assert(testSucceeded(14).location == Some(LineInFile(thisLineNumber - 126, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(14).formatter == Some(IndentedText("- test 15", "test 15", 1)))
          assert(testSucceeded(15).testName == "test 16")
          assert(testSucceeded(15).testText == "test 16")
          assert(testSucceeded(15).location == Some(LineInFile(thisLineNumber - 129, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(15).formatter == Some(IndentedText("- test 16", "test 16", 1)))
          assert(testSucceeded(16).testName == "test 17")
          assert(testSucceeded(16).testText == "test 17")
          assert(testSucceeded(16).location == Some(LineInFile(thisLineNumber - 132, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(16).formatter == Some(IndentedText("- test 17", "test 17", 1)))
          assert(testSucceeded(17).testName == "test 18")
          assert(testSucceeded(17).testText == "test 18")
          assert(testSucceeded(17).location == Some(LineInFile(thisLineNumber - 135, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(17).formatter == Some(IndentedText("- test 18", "test 18", 1)))
        }
        it("should report 19 test succeeded events to the passed-in reporter when andThen with another 18 succeeded Test1") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              BeforeNode(3).andThen(
                Test1("test 1") { (i: Int) => i + 1 }.andThen(
                  Test1("test 2") { (i: Int) => succeed },
                  Test1("test 3") { (i: Int) => succeed },
                  Test1("test 4") { (i: Int) => succeed },
                  Test1("test 5") { (i: Int) => succeed },
                  Test1("test 6") { (i: Int) => succeed },
                  Test1("test 7") { (i: Int) => succeed },
                  Test1("test 8") { (i: Int) => succeed },
                  Test1("test 9") { (i: Int) => succeed },
                  Test1("test 10") { (i: Int) => succeed },
                  Test1("test 11") { (i: Int) => succeed },
                  Test1("test 12") { (i: Int) => succeed },
                  Test1("test 13") { (i: Int) => succeed },
                  Test1("test 14") { (i: Int) => succeed },
                  Test1("test 15") { (i: Int) => succeed },
                  Test1("test 16") { (i: Int) => succeed },
                  Test1("test 17") { (i: Int) => succeed },
                  Test1("test 18") { (i: Int) => succeed },
                  Test1("test 19") { (i: Int) => succeed }
                )
              )
          }
          assert(suite.testNames == Set("test 1", "test 2", "test 3", "test 4", "test 5", "test 6", "test 7", "test 8", "test 9", "test 10",
            "test 11", "test 12", "test 13", "test 14", "test 15", "test 16", "test 17", "test 18", "test 19"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 19)
          assert(testStarting(0).testName == "test 1")
          assert(testStarting(0).testText == "test 1")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 29, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "test 2")
          assert(testStarting(1).testText == "test 2")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 31, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "test 3")
          assert(testStarting(2).testText == "test 3")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 33, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(3).testName == "test 4")
          assert(testStarting(3).testText == "test 4")
          assert(testStarting(3).location == Some(LineInFile(thisLineNumber - 35, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(4).testName == "test 5")
          assert(testStarting(4).testText == "test 5")
          assert(testStarting(4).location == Some(LineInFile(thisLineNumber - 37, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(5).testName == "test 6")
          assert(testStarting(5).testText == "test 6")
          assert(testStarting(5).location == Some(LineInFile(thisLineNumber - 39, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(6).testName == "test 7")
          assert(testStarting(6).testText == "test 7")
          assert(testStarting(6).location == Some(LineInFile(thisLineNumber - 41, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(7).testName == "test 8")
          assert(testStarting(7).testText == "test 8")
          assert(testStarting(7).location == Some(LineInFile(thisLineNumber - 43, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(8).testName == "test 9")
          assert(testStarting(8).testText == "test 9")
          assert(testStarting(8).location == Some(LineInFile(thisLineNumber - 45, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(9).testName == "test 10")
          assert(testStarting(9).testText == "test 10")
          assert(testStarting(9).location == Some(LineInFile(thisLineNumber - 47, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(10).testName == "test 11")
          assert(testStarting(10).testText == "test 11")
          assert(testStarting(10).location == Some(LineInFile(thisLineNumber - 49, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(11).testName == "test 12")
          assert(testStarting(11).testText == "test 12")
          assert(testStarting(11).location == Some(LineInFile(thisLineNumber - 51, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(12).testName == "test 13")
          assert(testStarting(12).testText == "test 13")
          assert(testStarting(12).location == Some(LineInFile(thisLineNumber - 53, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(13).testName == "test 14")
          assert(testStarting(13).testText == "test 14")
          assert(testStarting(13).location == Some(LineInFile(thisLineNumber - 55, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(14).testName == "test 15")
          assert(testStarting(14).testText == "test 15")
          assert(testStarting(14).location == Some(LineInFile(thisLineNumber - 57, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(15).testName == "test 16")
          assert(testStarting(15).testText == "test 16")
          assert(testStarting(15).location == Some(LineInFile(thisLineNumber - 59, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(16).testName == "test 17")
          assert(testStarting(16).testText == "test 17")
          assert(testStarting(16).location == Some(LineInFile(thisLineNumber - 61, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(17).testName == "test 18")
          assert(testStarting(17).testText == "test 18")
          assert(testStarting(17).location == Some(LineInFile(thisLineNumber - 63, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(18).testName == "test 19")
          assert(testStarting(18).testText == "test 19")
          assert(testStarting(18).location == Some(LineInFile(thisLineNumber - 65, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 19)
          assert(testSucceeded(0).testName == "test 1")
          assert(testSucceeded(0).testText == "test 1")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 88, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- test 1", "test 1", 1)))
          assert(testSucceeded(1).testName == "test 2")
          assert(testSucceeded(1).testText == "test 2")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 91, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- test 2", "test 2", 1)))
          assert(testSucceeded(2).testName == "test 3")
          assert(testSucceeded(2).testText == "test 3")
          assert(testSucceeded(2).location == Some(LineInFile(thisLineNumber - 94, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(2).formatter == Some(IndentedText("- test 3", "test 3", 1)))
          assert(testSucceeded(3).testName == "test 4")
          assert(testSucceeded(3).testText == "test 4")
          assert(testSucceeded(3).location == Some(LineInFile(thisLineNumber - 97, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(3).formatter == Some(IndentedText("- test 4", "test 4", 1)))
          assert(testSucceeded(4).testName == "test 5")
          assert(testSucceeded(4).testText == "test 5")
          assert(testSucceeded(4).location == Some(LineInFile(thisLineNumber - 100, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(4).formatter == Some(IndentedText("- test 5", "test 5", 1)))
          assert(testSucceeded(5).testName == "test 6")
          assert(testSucceeded(5).testText == "test 6")
          assert(testSucceeded(5).location == Some(LineInFile(thisLineNumber - 103, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(5).formatter == Some(IndentedText("- test 6", "test 6", 1)))
          assert(testSucceeded(6).testName == "test 7")
          assert(testSucceeded(6).testText == "test 7")
          assert(testSucceeded(6).location == Some(LineInFile(thisLineNumber - 106, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(6).formatter == Some(IndentedText("- test 7", "test 7", 1)))
          assert(testSucceeded(7).testName == "test 8")
          assert(testSucceeded(7).testText == "test 8")
          assert(testSucceeded(7).location == Some(LineInFile(thisLineNumber - 109, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(7).formatter == Some(IndentedText("- test 8", "test 8", 1)))
          assert(testSucceeded(8).testName == "test 9")
          assert(testSucceeded(8).testText == "test 9")
          assert(testSucceeded(8).location == Some(LineInFile(thisLineNumber - 112, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(8).formatter == Some(IndentedText("- test 9", "test 9", 1)))
          assert(testSucceeded(9).testName == "test 10")
          assert(testSucceeded(9).testText == "test 10")
          assert(testSucceeded(9).location == Some(LineInFile(thisLineNumber - 115, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(9).formatter == Some(IndentedText("- test 10", "test 10", 1)))
          assert(testSucceeded(10).testName == "test 11")
          assert(testSucceeded(10).testText == "test 11")
          assert(testSucceeded(10).location == Some(LineInFile(thisLineNumber - 118, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(10).formatter == Some(IndentedText("- test 11", "test 11", 1)))
          assert(testSucceeded(11).testName == "test 12")
          assert(testSucceeded(11).testText == "test 12")
          assert(testSucceeded(11).location == Some(LineInFile(thisLineNumber - 121, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(11).formatter == Some(IndentedText("- test 12", "test 12", 1)))
          assert(testSucceeded(12).testName == "test 13")
          assert(testSucceeded(12).testText == "test 13")
          assert(testSucceeded(12).location == Some(LineInFile(thisLineNumber - 124, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(12).formatter == Some(IndentedText("- test 13", "test 13", 1)))
          assert(testSucceeded(13).testName == "test 14")
          assert(testSucceeded(13).testText == "test 14")
          assert(testSucceeded(13).location == Some(LineInFile(thisLineNumber - 127, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(13).formatter == Some(IndentedText("- test 14", "test 14", 1)))
          assert(testSucceeded(14).testName == "test 15")
          assert(testSucceeded(14).testText == "test 15")
          assert(testSucceeded(14).location == Some(LineInFile(thisLineNumber - 130, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(14).formatter == Some(IndentedText("- test 15", "test 15", 1)))
          assert(testSucceeded(15).testName == "test 16")
          assert(testSucceeded(15).testText == "test 16")
          assert(testSucceeded(15).location == Some(LineInFile(thisLineNumber - 133, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(15).formatter == Some(IndentedText("- test 16", "test 16", 1)))
          assert(testSucceeded(16).testName == "test 17")
          assert(testSucceeded(16).testText == "test 17")
          assert(testSucceeded(16).location == Some(LineInFile(thisLineNumber - 136, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(16).formatter == Some(IndentedText("- test 17", "test 17", 1)))
          assert(testSucceeded(17).testName == "test 18")
          assert(testSucceeded(17).testText == "test 18")
          assert(testSucceeded(17).location == Some(LineInFile(thisLineNumber - 139, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(17).formatter == Some(IndentedText("- test 18", "test 18", 1)))
          assert(testSucceeded(18).testName == "test 19")
          assert(testSucceeded(18).testText == "test 19")
          assert(testSucceeded(18).location == Some(LineInFile(thisLineNumber - 142, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(18).formatter == Some(IndentedText("- test 19", "test 19", 1)))
        }
        it("should report 20 test succeeded events to the passed-in reporter when andThen with another 19 succeeded Test1") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              BeforeNode(3).andThen(
                Test1("test 1") { (i: Int) => i + 1 }.andThen(
                  Test1("test 2") { (i: Int) => succeed },
                  Test1("test 3") { (i: Int) => succeed },
                  Test1("test 4") { (i: Int) => succeed },
                  Test1("test 5") { (i: Int) => succeed },
                  Test1("test 6") { (i: Int) => succeed },
                  Test1("test 7") { (i: Int) => succeed },
                  Test1("test 8") { (i: Int) => succeed },
                  Test1("test 9") { (i: Int) => succeed },
                  Test1("test 10") { (i: Int) => succeed },
                  Test1("test 11") { (i: Int) => succeed },
                  Test1("test 12") { (i: Int) => succeed },
                  Test1("test 13") { (i: Int) => succeed },
                  Test1("test 14") { (i: Int) => succeed },
                  Test1("test 15") { (i: Int) => succeed },
                  Test1("test 16") { (i: Int) => succeed },
                  Test1("test 17") { (i: Int) => succeed },
                  Test1("test 18") { (i: Int) => succeed },
                  Test1("test 19") { (i: Int) => succeed },
                  Test1("test 20") { (i: Int) => succeed }
                )
              )
          }
          assert(suite.testNames == Set("test 1", "test 2", "test 3", "test 4", "test 5", "test 6", "test 7", "test 8", "test 9", "test 10",
            "test 11", "test 12", "test 13", "test 14", "test 15", "test 16", "test 17", "test 18", "test 19", "test 20"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 20)
          assert(testStarting(0).testName == "test 1")
          assert(testStarting(0).testText == "test 1")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 30, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "test 2")
          assert(testStarting(1).testText == "test 2")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 32, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "test 3")
          assert(testStarting(2).testText == "test 3")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 34, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(3).testName == "test 4")
          assert(testStarting(3).testText == "test 4")
          assert(testStarting(3).location == Some(LineInFile(thisLineNumber - 36, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(4).testName == "test 5")
          assert(testStarting(4).testText == "test 5")
          assert(testStarting(4).location == Some(LineInFile(thisLineNumber - 38, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(5).testName == "test 6")
          assert(testStarting(5).testText == "test 6")
          assert(testStarting(5).location == Some(LineInFile(thisLineNumber - 40, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(6).testName == "test 7")
          assert(testStarting(6).testText == "test 7")
          assert(testStarting(6).location == Some(LineInFile(thisLineNumber - 42, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(7).testName == "test 8")
          assert(testStarting(7).testText == "test 8")
          assert(testStarting(7).location == Some(LineInFile(thisLineNumber - 44, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(8).testName == "test 9")
          assert(testStarting(8).testText == "test 9")
          assert(testStarting(8).location == Some(LineInFile(thisLineNumber - 46, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(9).testName == "test 10")
          assert(testStarting(9).testText == "test 10")
          assert(testStarting(9).location == Some(LineInFile(thisLineNumber - 48, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(10).testName == "test 11")
          assert(testStarting(10).testText == "test 11")
          assert(testStarting(10).location == Some(LineInFile(thisLineNumber - 50, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(11).testName == "test 12")
          assert(testStarting(11).testText == "test 12")
          assert(testStarting(11).location == Some(LineInFile(thisLineNumber - 52, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(12).testName == "test 13")
          assert(testStarting(12).testText == "test 13")
          assert(testStarting(12).location == Some(LineInFile(thisLineNumber - 54, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(13).testName == "test 14")
          assert(testStarting(13).testText == "test 14")
          assert(testStarting(13).location == Some(LineInFile(thisLineNumber - 56, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(14).testName == "test 15")
          assert(testStarting(14).testText == "test 15")
          assert(testStarting(14).location == Some(LineInFile(thisLineNumber - 58, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(15).testName == "test 16")
          assert(testStarting(15).testText == "test 16")
          assert(testStarting(15).location == Some(LineInFile(thisLineNumber - 60, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(16).testName == "test 17")
          assert(testStarting(16).testText == "test 17")
          assert(testStarting(16).location == Some(LineInFile(thisLineNumber - 62, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(17).testName == "test 18")
          assert(testStarting(17).testText == "test 18")
          assert(testStarting(17).location == Some(LineInFile(thisLineNumber - 64, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(18).testName == "test 19")
          assert(testStarting(18).testText == "test 19")
          assert(testStarting(18).location == Some(LineInFile(thisLineNumber - 66, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(19).testName == "test 20")
          assert(testStarting(19).testText == "test 20")
          assert(testStarting(19).location == Some(LineInFile(thisLineNumber - 68, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 20)
          assert(testSucceeded(0).testName == "test 1")
          assert(testSucceeded(0).testText == "test 1")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 92, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- test 1", "test 1", 1)))
          assert(testSucceeded(1).testName == "test 2")
          assert(testSucceeded(1).testText == "test 2")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 95, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- test 2", "test 2", 1)))
          assert(testSucceeded(2).testName == "test 3")
          assert(testSucceeded(2).testText == "test 3")
          assert(testSucceeded(2).location == Some(LineInFile(thisLineNumber - 98, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(2).formatter == Some(IndentedText("- test 3", "test 3", 1)))
          assert(testSucceeded(3).testName == "test 4")
          assert(testSucceeded(3).testText == "test 4")
          assert(testSucceeded(3).location == Some(LineInFile(thisLineNumber - 101, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(3).formatter == Some(IndentedText("- test 4", "test 4", 1)))
          assert(testSucceeded(4).testName == "test 5")
          assert(testSucceeded(4).testText == "test 5")
          assert(testSucceeded(4).location == Some(LineInFile(thisLineNumber - 104, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(4).formatter == Some(IndentedText("- test 5", "test 5", 1)))
          assert(testSucceeded(5).testName == "test 6")
          assert(testSucceeded(5).testText == "test 6")
          assert(testSucceeded(5).location == Some(LineInFile(thisLineNumber - 107, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(5).formatter == Some(IndentedText("- test 6", "test 6", 1)))
          assert(testSucceeded(6).testName == "test 7")
          assert(testSucceeded(6).testText == "test 7")
          assert(testSucceeded(6).location == Some(LineInFile(thisLineNumber - 110, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(6).formatter == Some(IndentedText("- test 7", "test 7", 1)))
          assert(testSucceeded(7).testName == "test 8")
          assert(testSucceeded(7).testText == "test 8")
          assert(testSucceeded(7).location == Some(LineInFile(thisLineNumber - 113, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(7).formatter == Some(IndentedText("- test 8", "test 8", 1)))
          assert(testSucceeded(8).testName == "test 9")
          assert(testSucceeded(8).testText == "test 9")
          assert(testSucceeded(8).location == Some(LineInFile(thisLineNumber - 116, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(8).formatter == Some(IndentedText("- test 9", "test 9", 1)))
          assert(testSucceeded(9).testName == "test 10")
          assert(testSucceeded(9).testText == "test 10")
          assert(testSucceeded(9).location == Some(LineInFile(thisLineNumber - 119, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(9).formatter == Some(IndentedText("- test 10", "test 10", 1)))
          assert(testSucceeded(10).testName == "test 11")
          assert(testSucceeded(10).testText == "test 11")
          assert(testSucceeded(10).location == Some(LineInFile(thisLineNumber - 122, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(10).formatter == Some(IndentedText("- test 11", "test 11", 1)))
          assert(testSucceeded(11).testName == "test 12")
          assert(testSucceeded(11).testText == "test 12")
          assert(testSucceeded(11).location == Some(LineInFile(thisLineNumber - 125, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(11).formatter == Some(IndentedText("- test 12", "test 12", 1)))
          assert(testSucceeded(12).testName == "test 13")
          assert(testSucceeded(12).testText == "test 13")
          assert(testSucceeded(12).location == Some(LineInFile(thisLineNumber - 128, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(12).formatter == Some(IndentedText("- test 13", "test 13", 1)))
          assert(testSucceeded(13).testName == "test 14")
          assert(testSucceeded(13).testText == "test 14")
          assert(testSucceeded(13).location == Some(LineInFile(thisLineNumber - 131, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(13).formatter == Some(IndentedText("- test 14", "test 14", 1)))
          assert(testSucceeded(14).testName == "test 15")
          assert(testSucceeded(14).testText == "test 15")
          assert(testSucceeded(14).location == Some(LineInFile(thisLineNumber - 134, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(14).formatter == Some(IndentedText("- test 15", "test 15", 1)))
          assert(testSucceeded(15).testName == "test 16")
          assert(testSucceeded(15).testText == "test 16")
          assert(testSucceeded(15).location == Some(LineInFile(thisLineNumber - 137, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(15).formatter == Some(IndentedText("- test 16", "test 16", 1)))
          assert(testSucceeded(16).testName == "test 17")
          assert(testSucceeded(16).testText == "test 17")
          assert(testSucceeded(16).location == Some(LineInFile(thisLineNumber - 140, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(16).formatter == Some(IndentedText("- test 17", "test 17", 1)))
          assert(testSucceeded(17).testName == "test 18")
          assert(testSucceeded(17).testText == "test 18")
          assert(testSucceeded(17).location == Some(LineInFile(thisLineNumber - 143, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(17).formatter == Some(IndentedText("- test 18", "test 18", 1)))
          assert(testSucceeded(18).testName == "test 19")
          assert(testSucceeded(18).testText == "test 19")
          assert(testSucceeded(18).location == Some(LineInFile(thisLineNumber - 146, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(18).formatter == Some(IndentedText("- test 19", "test 19", 1)))
          assert(testSucceeded(19).testName == "test 20")
          assert(testSucceeded(19).testText == "test 20")
          assert(testSucceeded(19).location == Some(LineInFile(thisLineNumber - 149, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(19).formatter == Some(IndentedText("- test 20", "test 20", 1)))
        }
        it("should report 21 test succeeded events to the passed-in reporter when andThen with another 20 succeeded Test1") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              BeforeNode(3).andThen(
                Test1("test 1") { (i: Int) => i + 1 }.andThen(
                  Test1("test 2") { (i: Int) => succeed },
                  Test1("test 3") { (i: Int) => succeed },
                  Test1("test 4") { (i: Int) => succeed },
                  Test1("test 5") { (i: Int) => succeed },
                  Test1("test 6") { (i: Int) => succeed },
                  Test1("test 7") { (i: Int) => succeed },
                  Test1("test 8") { (i: Int) => succeed },
                  Test1("test 9") { (i: Int) => succeed },
                  Test1("test 10") { (i: Int) => succeed },
                  Test1("test 11") { (i: Int) => succeed },
                  Test1("test 12") { (i: Int) => succeed },
                  Test1("test 13") { (i: Int) => succeed },
                  Test1("test 14") { (i: Int) => succeed },
                  Test1("test 15") { (i: Int) => succeed },
                  Test1("test 16") { (i: Int) => succeed },
                  Test1("test 17") { (i: Int) => succeed },
                  Test1("test 18") { (i: Int) => succeed },
                  Test1("test 19") { (i: Int) => succeed },
                  Test1("test 20") { (i: Int) => succeed },
                  Test1("test 21") { (i: Int) => succeed }
                )
              )
          }
          assert(suite.testNames == Set("test 1", "test 2", "test 3", "test 4", "test 5", "test 6", "test 7", "test 8", "test 9", "test 10",
            "test 11", "test 12", "test 13", "test 14", "test 15", "test 16", "test 17", "test 18", "test 19", "test 20", "test 21"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 21)
          assert(testStarting(0).testName == "test 1")
          assert(testStarting(0).testText == "test 1")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 31, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "test 2")
          assert(testStarting(1).testText == "test 2")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 33, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "test 3")
          assert(testStarting(2).testText == "test 3")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 35, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(3).testName == "test 4")
          assert(testStarting(3).testText == "test 4")
          assert(testStarting(3).location == Some(LineInFile(thisLineNumber - 37, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(4).testName == "test 5")
          assert(testStarting(4).testText == "test 5")
          assert(testStarting(4).location == Some(LineInFile(thisLineNumber - 39, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(5).testName == "test 6")
          assert(testStarting(5).testText == "test 6")
          assert(testStarting(5).location == Some(LineInFile(thisLineNumber - 41, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(6).testName == "test 7")
          assert(testStarting(6).testText == "test 7")
          assert(testStarting(6).location == Some(LineInFile(thisLineNumber - 43, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(7).testName == "test 8")
          assert(testStarting(7).testText == "test 8")
          assert(testStarting(7).location == Some(LineInFile(thisLineNumber - 45, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(8).testName == "test 9")
          assert(testStarting(8).testText == "test 9")
          assert(testStarting(8).location == Some(LineInFile(thisLineNumber - 47, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(9).testName == "test 10")
          assert(testStarting(9).testText == "test 10")
          assert(testStarting(9).location == Some(LineInFile(thisLineNumber - 49, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(10).testName == "test 11")
          assert(testStarting(10).testText == "test 11")
          assert(testStarting(10).location == Some(LineInFile(thisLineNumber - 51, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(11).testName == "test 12")
          assert(testStarting(11).testText == "test 12")
          assert(testStarting(11).location == Some(LineInFile(thisLineNumber - 53, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(12).testName == "test 13")
          assert(testStarting(12).testText == "test 13")
          assert(testStarting(12).location == Some(LineInFile(thisLineNumber - 55, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(13).testName == "test 14")
          assert(testStarting(13).testText == "test 14")
          assert(testStarting(13).location == Some(LineInFile(thisLineNumber - 57, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(14).testName == "test 15")
          assert(testStarting(14).testText == "test 15")
          assert(testStarting(14).location == Some(LineInFile(thisLineNumber - 59, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(15).testName == "test 16")
          assert(testStarting(15).testText == "test 16")
          assert(testStarting(15).location == Some(LineInFile(thisLineNumber - 61, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(16).testName == "test 17")
          assert(testStarting(16).testText == "test 17")
          assert(testStarting(16).location == Some(LineInFile(thisLineNumber - 63, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(17).testName == "test 18")
          assert(testStarting(17).testText == "test 18")
          assert(testStarting(17).location == Some(LineInFile(thisLineNumber - 65, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(18).testName == "test 19")
          assert(testStarting(18).testText == "test 19")
          assert(testStarting(18).location == Some(LineInFile(thisLineNumber - 67, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(19).testName == "test 20")
          assert(testStarting(19).testText == "test 20")
          assert(testStarting(19).location == Some(LineInFile(thisLineNumber - 69, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(20).testName == "test 21")
          assert(testStarting(20).testText == "test 21")
          assert(testStarting(20).location == Some(LineInFile(thisLineNumber - 71, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 21)
          assert(testSucceeded(0).testName == "test 1")
          assert(testSucceeded(0).testText == "test 1")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 96, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- test 1", "test 1", 1)))
          assert(testSucceeded(1).testName == "test 2")
          assert(testSucceeded(1).testText == "test 2")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 99, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- test 2", "test 2", 1)))
          assert(testSucceeded(2).testName == "test 3")
          assert(testSucceeded(2).testText == "test 3")
          assert(testSucceeded(2).location == Some(LineInFile(thisLineNumber - 102, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(2).formatter == Some(IndentedText("- test 3", "test 3", 1)))
          assert(testSucceeded(3).testName == "test 4")
          assert(testSucceeded(3).testText == "test 4")
          assert(testSucceeded(3).location == Some(LineInFile(thisLineNumber - 105, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(3).formatter == Some(IndentedText("- test 4", "test 4", 1)))
          assert(testSucceeded(4).testName == "test 5")
          assert(testSucceeded(4).testText == "test 5")
          assert(testSucceeded(4).location == Some(LineInFile(thisLineNumber - 108, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(4).formatter == Some(IndentedText("- test 5", "test 5", 1)))
          assert(testSucceeded(5).testName == "test 6")
          assert(testSucceeded(5).testText == "test 6")
          assert(testSucceeded(5).location == Some(LineInFile(thisLineNumber - 111, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(5).formatter == Some(IndentedText("- test 6", "test 6", 1)))
          assert(testSucceeded(6).testName == "test 7")
          assert(testSucceeded(6).testText == "test 7")
          assert(testSucceeded(6).location == Some(LineInFile(thisLineNumber - 114, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(6).formatter == Some(IndentedText("- test 7", "test 7", 1)))
          assert(testSucceeded(7).testName == "test 8")
          assert(testSucceeded(7).testText == "test 8")
          assert(testSucceeded(7).location == Some(LineInFile(thisLineNumber - 117, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(7).formatter == Some(IndentedText("- test 8", "test 8", 1)))
          assert(testSucceeded(8).testName == "test 9")
          assert(testSucceeded(8).testText == "test 9")
          assert(testSucceeded(8).location == Some(LineInFile(thisLineNumber - 120, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(8).formatter == Some(IndentedText("- test 9", "test 9", 1)))
          assert(testSucceeded(9).testName == "test 10")
          assert(testSucceeded(9).testText == "test 10")
          assert(testSucceeded(9).location == Some(LineInFile(thisLineNumber - 123, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(9).formatter == Some(IndentedText("- test 10", "test 10", 1)))
          assert(testSucceeded(10).testName == "test 11")
          assert(testSucceeded(10).testText == "test 11")
          assert(testSucceeded(10).location == Some(LineInFile(thisLineNumber - 126, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(10).formatter == Some(IndentedText("- test 11", "test 11", 1)))
          assert(testSucceeded(11).testName == "test 12")
          assert(testSucceeded(11).testText == "test 12")
          assert(testSucceeded(11).location == Some(LineInFile(thisLineNumber - 129, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(11).formatter == Some(IndentedText("- test 12", "test 12", 1)))
          assert(testSucceeded(12).testName == "test 13")
          assert(testSucceeded(12).testText == "test 13")
          assert(testSucceeded(12).location == Some(LineInFile(thisLineNumber - 132, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(12).formatter == Some(IndentedText("- test 13", "test 13", 1)))
          assert(testSucceeded(13).testName == "test 14")
          assert(testSucceeded(13).testText == "test 14")
          assert(testSucceeded(13).location == Some(LineInFile(thisLineNumber - 135, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(13).formatter == Some(IndentedText("- test 14", "test 14", 1)))
          assert(testSucceeded(14).testName == "test 15")
          assert(testSucceeded(14).testText == "test 15")
          assert(testSucceeded(14).location == Some(LineInFile(thisLineNumber - 138, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(14).formatter == Some(IndentedText("- test 15", "test 15", 1)))
          assert(testSucceeded(15).testName == "test 16")
          assert(testSucceeded(15).testText == "test 16")
          assert(testSucceeded(15).location == Some(LineInFile(thisLineNumber - 141, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(15).formatter == Some(IndentedText("- test 16", "test 16", 1)))
          assert(testSucceeded(16).testName == "test 17")
          assert(testSucceeded(16).testText == "test 17")
          assert(testSucceeded(16).location == Some(LineInFile(thisLineNumber - 144, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(16).formatter == Some(IndentedText("- test 17", "test 17", 1)))
          assert(testSucceeded(17).testName == "test 18")
          assert(testSucceeded(17).testText == "test 18")
          assert(testSucceeded(17).location == Some(LineInFile(thisLineNumber - 147, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(17).formatter == Some(IndentedText("- test 18", "test 18", 1)))
          assert(testSucceeded(18).testName == "test 19")
          assert(testSucceeded(18).testText == "test 19")
          assert(testSucceeded(18).location == Some(LineInFile(thisLineNumber - 150, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(18).formatter == Some(IndentedText("- test 19", "test 19", 1)))
          assert(testSucceeded(19).testName == "test 20")
          assert(testSucceeded(19).testText == "test 20")
          assert(testSucceeded(19).location == Some(LineInFile(thisLineNumber - 153, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(19).formatter == Some(IndentedText("- test 20", "test 20", 1)))
          assert(testSucceeded(20).testName == "test 21")
          assert(testSucceeded(20).testText == "test 21")
          assert(testSucceeded(20).location == Some(LineInFile(thisLineNumber - 156, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(20).formatter == Some(IndentedText("- test 21", "test 21", 1)))
        }
        it("should report 22 test succeeded events to the passed-in reporter when andThen with another 21 succeeded Test1") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              BeforeNode(3).andThen(
                Test1("test 1") { (i: Int) => i + 1 }.andThen(
                  Test1("test 2") { (i: Int) => succeed },
                  Test1("test 3") { (i: Int) => succeed },
                  Test1("test 4") { (i: Int) => succeed },
                  Test1("test 5") { (i: Int) => succeed },
                  Test1("test 6") { (i: Int) => succeed },
                  Test1("test 7") { (i: Int) => succeed },
                  Test1("test 8") { (i: Int) => succeed },
                  Test1("test 9") { (i: Int) => succeed },
                  Test1("test 10") { (i: Int) => succeed },
                  Test1("test 11") { (i: Int) => succeed },
                  Test1("test 12") { (i: Int) => succeed },
                  Test1("test 13") { (i: Int) => succeed },
                  Test1("test 14") { (i: Int) => succeed },
                  Test1("test 15") { (i: Int) => succeed },
                  Test1("test 16") { (i: Int) => succeed },
                  Test1("test 17") { (i: Int) => succeed },
                  Test1("test 18") { (i: Int) => succeed },
                  Test1("test 19") { (i: Int) => succeed },
                  Test1("test 20") { (i: Int) => succeed },
                  Test1("test 21") { (i: Int) => succeed },
                  Test1("test 22") { (i: Int) => succeed }
                )
              )
          }
          assert(suite.testNames == Set("test 1", "test 2", "test 3", "test 4", "test 5", "test 6", "test 7", "test 8", "test 9", "test 10",
            "test 11", "test 12", "test 13", "test 14", "test 15", "test 16", "test 17", "test 18", "test 19", "test 20", "test 21", "test 22"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 22)
          assert(testStarting(0).testName == "test 1")
          assert(testStarting(0).testText == "test 1")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 32, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "test 2")
          assert(testStarting(1).testText == "test 2")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 34, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "test 3")
          assert(testStarting(2).testText == "test 3")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 36, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(3).testName == "test 4")
          assert(testStarting(3).testText == "test 4")
          assert(testStarting(3).location == Some(LineInFile(thisLineNumber - 38, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(4).testName == "test 5")
          assert(testStarting(4).testText == "test 5")
          assert(testStarting(4).location == Some(LineInFile(thisLineNumber - 40, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(5).testName == "test 6")
          assert(testStarting(5).testText == "test 6")
          assert(testStarting(5).location == Some(LineInFile(thisLineNumber - 42, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(6).testName == "test 7")
          assert(testStarting(6).testText == "test 7")
          assert(testStarting(6).location == Some(LineInFile(thisLineNumber - 44, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(7).testName == "test 8")
          assert(testStarting(7).testText == "test 8")
          assert(testStarting(7).location == Some(LineInFile(thisLineNumber - 46, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(8).testName == "test 9")
          assert(testStarting(8).testText == "test 9")
          assert(testStarting(8).location == Some(LineInFile(thisLineNumber - 48, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(9).testName == "test 10")
          assert(testStarting(9).testText == "test 10")
          assert(testStarting(9).location == Some(LineInFile(thisLineNumber - 50, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(10).testName == "test 11")
          assert(testStarting(10).testText == "test 11")
          assert(testStarting(10).location == Some(LineInFile(thisLineNumber - 52, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(11).testName == "test 12")
          assert(testStarting(11).testText == "test 12")
          assert(testStarting(11).location == Some(LineInFile(thisLineNumber - 54, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(12).testName == "test 13")
          assert(testStarting(12).testText == "test 13")
          assert(testStarting(12).location == Some(LineInFile(thisLineNumber - 56, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(13).testName == "test 14")
          assert(testStarting(13).testText == "test 14")
          assert(testStarting(13).location == Some(LineInFile(thisLineNumber - 58, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(14).testName == "test 15")
          assert(testStarting(14).testText == "test 15")
          assert(testStarting(14).location == Some(LineInFile(thisLineNumber - 60, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(15).testName == "test 16")
          assert(testStarting(15).testText == "test 16")
          assert(testStarting(15).location == Some(LineInFile(thisLineNumber - 62, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(16).testName == "test 17")
          assert(testStarting(16).testText == "test 17")
          assert(testStarting(16).location == Some(LineInFile(thisLineNumber - 64, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(17).testName == "test 18")
          assert(testStarting(17).testText == "test 18")
          assert(testStarting(17).location == Some(LineInFile(thisLineNumber - 66, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(18).testName == "test 19")
          assert(testStarting(18).testText == "test 19")
          assert(testStarting(18).location == Some(LineInFile(thisLineNumber - 68, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(19).testName == "test 20")
          assert(testStarting(19).testText == "test 20")
          assert(testStarting(19).location == Some(LineInFile(thisLineNumber - 70, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(20).testName == "test 21")
          assert(testStarting(20).testText == "test 21")
          assert(testStarting(20).location == Some(LineInFile(thisLineNumber - 72, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(21).testName == "test 22")
          assert(testStarting(21).testText == "test 22")
          assert(testStarting(21).location == Some(LineInFile(thisLineNumber - 74, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 22)
          assert(testSucceeded(0).testName == "test 1")
          assert(testSucceeded(0).testText == "test 1")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 100, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- test 1", "test 1", 1)))
          assert(testSucceeded(1).testName == "test 2")
          assert(testSucceeded(1).testText == "test 2")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 103, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- test 2", "test 2", 1)))
          assert(testSucceeded(2).testName == "test 3")
          assert(testSucceeded(2).testText == "test 3")
          assert(testSucceeded(2).location == Some(LineInFile(thisLineNumber - 106, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(2).formatter == Some(IndentedText("- test 3", "test 3", 1)))
          assert(testSucceeded(3).testName == "test 4")
          assert(testSucceeded(3).testText == "test 4")
          assert(testSucceeded(3).location == Some(LineInFile(thisLineNumber - 109, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(3).formatter == Some(IndentedText("- test 4", "test 4", 1)))
          assert(testSucceeded(4).testName == "test 5")
          assert(testSucceeded(4).testText == "test 5")
          assert(testSucceeded(4).location == Some(LineInFile(thisLineNumber - 112, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(4).formatter == Some(IndentedText("- test 5", "test 5", 1)))
          assert(testSucceeded(5).testName == "test 6")
          assert(testSucceeded(5).testText == "test 6")
          assert(testSucceeded(5).location == Some(LineInFile(thisLineNumber - 115, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(5).formatter == Some(IndentedText("- test 6", "test 6", 1)))
          assert(testSucceeded(6).testName == "test 7")
          assert(testSucceeded(6).testText == "test 7")
          assert(testSucceeded(6).location == Some(LineInFile(thisLineNumber - 118, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(6).formatter == Some(IndentedText("- test 7", "test 7", 1)))
          assert(testSucceeded(7).testName == "test 8")
          assert(testSucceeded(7).testText == "test 8")
          assert(testSucceeded(7).location == Some(LineInFile(thisLineNumber - 121, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(7).formatter == Some(IndentedText("- test 8", "test 8", 1)))
          assert(testSucceeded(8).testName == "test 9")
          assert(testSucceeded(8).testText == "test 9")
          assert(testSucceeded(8).location == Some(LineInFile(thisLineNumber - 124, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(8).formatter == Some(IndentedText("- test 9", "test 9", 1)))
          assert(testSucceeded(9).testName == "test 10")
          assert(testSucceeded(9).testText == "test 10")
          assert(testSucceeded(9).location == Some(LineInFile(thisLineNumber - 127, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(9).formatter == Some(IndentedText("- test 10", "test 10", 1)))
          assert(testSucceeded(10).testName == "test 11")
          assert(testSucceeded(10).testText == "test 11")
          assert(testSucceeded(10).location == Some(LineInFile(thisLineNumber - 130, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(10).formatter == Some(IndentedText("- test 11", "test 11", 1)))
          assert(testSucceeded(11).testName == "test 12")
          assert(testSucceeded(11).testText == "test 12")
          assert(testSucceeded(11).location == Some(LineInFile(thisLineNumber - 133, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(11).formatter == Some(IndentedText("- test 12", "test 12", 1)))
          assert(testSucceeded(12).testName == "test 13")
          assert(testSucceeded(12).testText == "test 13")
          assert(testSucceeded(12).location == Some(LineInFile(thisLineNumber - 136, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(12).formatter == Some(IndentedText("- test 13", "test 13", 1)))
          assert(testSucceeded(13).testName == "test 14")
          assert(testSucceeded(13).testText == "test 14")
          assert(testSucceeded(13).location == Some(LineInFile(thisLineNumber - 139, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(13).formatter == Some(IndentedText("- test 14", "test 14", 1)))
          assert(testSucceeded(14).testName == "test 15")
          assert(testSucceeded(14).testText == "test 15")
          assert(testSucceeded(14).location == Some(LineInFile(thisLineNumber - 142, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(14).formatter == Some(IndentedText("- test 15", "test 15", 1)))
          assert(testSucceeded(15).testName == "test 16")
          assert(testSucceeded(15).testText == "test 16")
          assert(testSucceeded(15).location == Some(LineInFile(thisLineNumber - 145, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(15).formatter == Some(IndentedText("- test 16", "test 16", 1)))
          assert(testSucceeded(16).testName == "test 17")
          assert(testSucceeded(16).testText == "test 17")
          assert(testSucceeded(16).location == Some(LineInFile(thisLineNumber - 148, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(16).formatter == Some(IndentedText("- test 17", "test 17", 1)))
          assert(testSucceeded(17).testName == "test 18")
          assert(testSucceeded(17).testText == "test 18")
          assert(testSucceeded(17).location == Some(LineInFile(thisLineNumber - 151, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(17).formatter == Some(IndentedText("- test 18", "test 18", 1)))
          assert(testSucceeded(18).testName == "test 19")
          assert(testSucceeded(18).testText == "test 19")
          assert(testSucceeded(18).location == Some(LineInFile(thisLineNumber - 154, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(18).formatter == Some(IndentedText("- test 19", "test 19", 1)))
          assert(testSucceeded(19).testName == "test 20")
          assert(testSucceeded(19).testText == "test 20")
          assert(testSucceeded(19).location == Some(LineInFile(thisLineNumber - 157, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(19).formatter == Some(IndentedText("- test 20", "test 20", 1)))
          assert(testSucceeded(20).testName == "test 21")
          assert(testSucceeded(20).testText == "test 21")
          assert(testSucceeded(20).location == Some(LineInFile(thisLineNumber - 160, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(20).formatter == Some(IndentedText("- test 21", "test 21", 1)))
          assert(testSucceeded(21).testName == "test 22")
          assert(testSucceeded(21).testText == "test 22")
          assert(testSucceeded(21).location == Some(LineInFile(thisLineNumber - 163, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(21).formatter == Some(IndentedText("- test 22", "test 22", 1)))
        }
        it("should report 23 test succeeded events to the passed-in reporter when andThen with another 22 succeeded Test1") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              BeforeNode(3).andThen(
                Test1("test 1") { (i: Int) => i + 1 }.andThen(
                  Test1("test 2") { (i: Int) => succeed },
                  Test1("test 3") { (i: Int) => succeed },
                  Test1("test 4") { (i: Int) => succeed },
                  Test1("test 5") { (i: Int) => succeed },
                  Test1("test 6") { (i: Int) => succeed },
                  Test1("test 7") { (i: Int) => succeed },
                  Test1("test 8") { (i: Int) => succeed },
                  Test1("test 9") { (i: Int) => succeed },
                  Test1("test 10") { (i: Int) => succeed },
                  Test1("test 11") { (i: Int) => succeed },
                  Test1("test 12") { (i: Int) => succeed },
                  Test1("test 13") { (i: Int) => succeed },
                  Test1("test 14") { (i: Int) => succeed },
                  Test1("test 15") { (i: Int) => succeed },
                  Test1("test 16") { (i: Int) => succeed },
                  Test1("test 17") { (i: Int) => succeed },
                  Test1("test 18") { (i: Int) => succeed },
                  Test1("test 19") { (i: Int) => succeed },
                  Test1("test 20") { (i: Int) => succeed },
                  Test1("test 21") { (i: Int) => succeed },
                  Test1("test 22") { (i: Int) => succeed },
                  Test1("test 23") { (i: Int) => succeed }
                )
              )
          }
          assert(suite.testNames == Set("test 1", "test 2", "test 3", "test 4", "test 5", "test 6", "test 7", "test 8", "test 9", "test 10",
            "test 11", "test 12", "test 13", "test 14", "test 15", "test 16", "test 17", "test 18", "test 19", "test 20", "test 21", "test 22", "test 23"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 23)
          assert(testStarting(0).testName == "test 1")
          assert(testStarting(0).testText == "test 1")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 33, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "test 2")
          assert(testStarting(1).testText == "test 2")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 35, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "test 3")
          assert(testStarting(2).testText == "test 3")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 37, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(3).testName == "test 4")
          assert(testStarting(3).testText == "test 4")
          assert(testStarting(3).location == Some(LineInFile(thisLineNumber - 39, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(4).testName == "test 5")
          assert(testStarting(4).testText == "test 5")
          assert(testStarting(4).location == Some(LineInFile(thisLineNumber - 41, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(5).testName == "test 6")
          assert(testStarting(5).testText == "test 6")
          assert(testStarting(5).location == Some(LineInFile(thisLineNumber - 43, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(6).testName == "test 7")
          assert(testStarting(6).testText == "test 7")
          assert(testStarting(6).location == Some(LineInFile(thisLineNumber - 45, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(7).testName == "test 8")
          assert(testStarting(7).testText == "test 8")
          assert(testStarting(7).location == Some(LineInFile(thisLineNumber - 47, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(8).testName == "test 9")
          assert(testStarting(8).testText == "test 9")
          assert(testStarting(8).location == Some(LineInFile(thisLineNumber - 49, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(9).testName == "test 10")
          assert(testStarting(9).testText == "test 10")
          assert(testStarting(9).location == Some(LineInFile(thisLineNumber - 51, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(10).testName == "test 11")
          assert(testStarting(10).testText == "test 11")
          assert(testStarting(10).location == Some(LineInFile(thisLineNumber - 53, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(11).testName == "test 12")
          assert(testStarting(11).testText == "test 12")
          assert(testStarting(11).location == Some(LineInFile(thisLineNumber - 55, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(12).testName == "test 13")
          assert(testStarting(12).testText == "test 13")
          assert(testStarting(12).location == Some(LineInFile(thisLineNumber - 57, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(13).testName == "test 14")
          assert(testStarting(13).testText == "test 14")
          assert(testStarting(13).location == Some(LineInFile(thisLineNumber - 59, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(14).testName == "test 15")
          assert(testStarting(14).testText == "test 15")
          assert(testStarting(14).location == Some(LineInFile(thisLineNumber - 61, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(15).testName == "test 16")
          assert(testStarting(15).testText == "test 16")
          assert(testStarting(15).location == Some(LineInFile(thisLineNumber - 63, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(16).testName == "test 17")
          assert(testStarting(16).testText == "test 17")
          assert(testStarting(16).location == Some(LineInFile(thisLineNumber - 65, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(17).testName == "test 18")
          assert(testStarting(17).testText == "test 18")
          assert(testStarting(17).location == Some(LineInFile(thisLineNumber - 67, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(18).testName == "test 19")
          assert(testStarting(18).testText == "test 19")
          assert(testStarting(18).location == Some(LineInFile(thisLineNumber - 69, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(19).testName == "test 20")
          assert(testStarting(19).testText == "test 20")
          assert(testStarting(19).location == Some(LineInFile(thisLineNumber - 71, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(20).testName == "test 21")
          assert(testStarting(20).testText == "test 21")
          assert(testStarting(20).location == Some(LineInFile(thisLineNumber - 73, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(21).testName == "test 22")
          assert(testStarting(21).testText == "test 22")
          assert(testStarting(21).location == Some(LineInFile(thisLineNumber - 75, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(22).testName == "test 23")
          assert(testStarting(22).testText == "test 23")
          assert(testStarting(22).location == Some(LineInFile(thisLineNumber - 77, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 23)
          assert(testSucceeded(0).testName == "test 1")
          assert(testSucceeded(0).testText == "test 1")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 104, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- test 1", "test 1", 1)))
          assert(testSucceeded(1).testName == "test 2")
          assert(testSucceeded(1).testText == "test 2")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 107, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- test 2", "test 2", 1)))
          assert(testSucceeded(2).testName == "test 3")
          assert(testSucceeded(2).testText == "test 3")
          assert(testSucceeded(2).location == Some(LineInFile(thisLineNumber - 110, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(2).formatter == Some(IndentedText("- test 3", "test 3", 1)))
          assert(testSucceeded(3).testName == "test 4")
          assert(testSucceeded(3).testText == "test 4")
          assert(testSucceeded(3).location == Some(LineInFile(thisLineNumber - 113, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(3).formatter == Some(IndentedText("- test 4", "test 4", 1)))
          assert(testSucceeded(4).testName == "test 5")
          assert(testSucceeded(4).testText == "test 5")
          assert(testSucceeded(4).location == Some(LineInFile(thisLineNumber - 116, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(4).formatter == Some(IndentedText("- test 5", "test 5", 1)))
          assert(testSucceeded(5).testName == "test 6")
          assert(testSucceeded(5).testText == "test 6")
          assert(testSucceeded(5).location == Some(LineInFile(thisLineNumber - 119, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(5).formatter == Some(IndentedText("- test 6", "test 6", 1)))
          assert(testSucceeded(6).testName == "test 7")
          assert(testSucceeded(6).testText == "test 7")
          assert(testSucceeded(6).location == Some(LineInFile(thisLineNumber - 122, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(6).formatter == Some(IndentedText("- test 7", "test 7", 1)))
          assert(testSucceeded(7).testName == "test 8")
          assert(testSucceeded(7).testText == "test 8")
          assert(testSucceeded(7).location == Some(LineInFile(thisLineNumber - 125, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(7).formatter == Some(IndentedText("- test 8", "test 8", 1)))
          assert(testSucceeded(8).testName == "test 9")
          assert(testSucceeded(8).testText == "test 9")
          assert(testSucceeded(8).location == Some(LineInFile(thisLineNumber - 128, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(8).formatter == Some(IndentedText("- test 9", "test 9", 1)))
          assert(testSucceeded(9).testName == "test 10")
          assert(testSucceeded(9).testText == "test 10")
          assert(testSucceeded(9).location == Some(LineInFile(thisLineNumber - 131, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(9).formatter == Some(IndentedText("- test 10", "test 10", 1)))
          assert(testSucceeded(10).testName == "test 11")
          assert(testSucceeded(10).testText == "test 11")
          assert(testSucceeded(10).location == Some(LineInFile(thisLineNumber - 134, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(10).formatter == Some(IndentedText("- test 11", "test 11", 1)))
          assert(testSucceeded(11).testName == "test 12")
          assert(testSucceeded(11).testText == "test 12")
          assert(testSucceeded(11).location == Some(LineInFile(thisLineNumber - 137, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(11).formatter == Some(IndentedText("- test 12", "test 12", 1)))
          assert(testSucceeded(12).testName == "test 13")
          assert(testSucceeded(12).testText == "test 13")
          assert(testSucceeded(12).location == Some(LineInFile(thisLineNumber - 140, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(12).formatter == Some(IndentedText("- test 13", "test 13", 1)))
          assert(testSucceeded(13).testName == "test 14")
          assert(testSucceeded(13).testText == "test 14")
          assert(testSucceeded(13).location == Some(LineInFile(thisLineNumber - 143, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(13).formatter == Some(IndentedText("- test 14", "test 14", 1)))
          assert(testSucceeded(14).testName == "test 15")
          assert(testSucceeded(14).testText == "test 15")
          assert(testSucceeded(14).location == Some(LineInFile(thisLineNumber - 146, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(14).formatter == Some(IndentedText("- test 15", "test 15", 1)))
          assert(testSucceeded(15).testName == "test 16")
          assert(testSucceeded(15).testText == "test 16")
          assert(testSucceeded(15).location == Some(LineInFile(thisLineNumber - 149, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(15).formatter == Some(IndentedText("- test 16", "test 16", 1)))
          assert(testSucceeded(16).testName == "test 17")
          assert(testSucceeded(16).testText == "test 17")
          assert(testSucceeded(16).location == Some(LineInFile(thisLineNumber - 152, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(16).formatter == Some(IndentedText("- test 17", "test 17", 1)))
          assert(testSucceeded(17).testName == "test 18")
          assert(testSucceeded(17).testText == "test 18")
          assert(testSucceeded(17).location == Some(LineInFile(thisLineNumber - 155, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(17).formatter == Some(IndentedText("- test 18", "test 18", 1)))
          assert(testSucceeded(18).testName == "test 19")
          assert(testSucceeded(18).testText == "test 19")
          assert(testSucceeded(18).location == Some(LineInFile(thisLineNumber - 158, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(18).formatter == Some(IndentedText("- test 19", "test 19", 1)))
          assert(testSucceeded(19).testName == "test 20")
          assert(testSucceeded(19).testText == "test 20")
          assert(testSucceeded(19).location == Some(LineInFile(thisLineNumber - 161, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(19).formatter == Some(IndentedText("- test 20", "test 20", 1)))
          assert(testSucceeded(20).testName == "test 21")
          assert(testSucceeded(20).testText == "test 21")
          assert(testSucceeded(20).location == Some(LineInFile(thisLineNumber - 164, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(20).formatter == Some(IndentedText("- test 21", "test 21", 1)))
          assert(testSucceeded(21).testName == "test 22")
          assert(testSucceeded(21).testText == "test 22")
          assert(testSucceeded(21).location == Some(LineInFile(thisLineNumber - 167, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(21).formatter == Some(IndentedText("- test 22", "test 22", 1)))
          assert(testSucceeded(22).testName == "test 23")
          assert(testSucceeded(22).testText == "test 23")
          assert(testSucceeded(22).location == Some(LineInFile(thisLineNumber - 170, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(22).formatter == Some(IndentedText("- test 23", "test 23", 1)))
        }
      }
      describe("when the test fails") {
        it("should report 1 test succeeded and 1 test failed event to the passed-in reporter when compose with another Test0") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow = Test1("second") { (i: Int) =>
              throw new RuntimeException("oops!")
            }.compose(
              Test0("first")(5)
            )
          }
          assert(suite.testNames == Set("first", "second"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 2)
          assert(testStarting(0).testName == "first")
          assert(testStarting(0).testText == "first")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 9, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "second")
          assert(testStarting(1).testText == "second")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 15, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 1)
          assert(testSucceeded(0).testName == "first")
          assert(testSucceeded(0).testText == "first")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 17, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- first", "first", 1)))
          val testFailed = myRep.testFailedEventsReceived
          assert(testFailed.size == 1)
          assert(testFailed(0).testName == "second")
          assert(testFailed(0).testText == "second")
          assert(testFailed(0).location == Some(SeeStackDepthException))
          assert(testFailed(0).formatter == Some(IndentedText("- second", "second", 1)))
        }
        it("should report 2 test succeeded and 1 test failed event to the passed-in reporter when tests combined with andThen passed first 2 and failed in last") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              Test0("first")(30).andThen(
                (Test1("second") { (i: Int) =>
                  i + 1
                }).andThen(
                  Test1("third") { (i: Int) =>
                    throw new RuntimeException("oops!")
                    1
                  }
                )
              )
          }
          assert(suite.testNames == Set("first", "second", "third"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 3)
          assert(testStarting(0).testName == "first")
          assert(testStarting(0).testText == "first")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 17, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "second")
          assert(testStarting(1).testText== "second")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 19, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "third")
          assert(testStarting(2).testText == "third")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 19, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 2)
          assert(testSucceeded(0).testName == "first")
          assert(testSucceeded(0).testText == "first")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 28, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- first", "first", 1)))
          assert(testSucceeded(1).testName == "second")
          assert(testSucceeded(1).testText == "second")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 31, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- second", "second", 1)))
          val testFailed = myRep.testFailedEventsReceived
          assert(testFailed.size == 1)
          assert(testFailed(0).testName == "third")
          assert(testFailed(0).testText == "third")
          assert(testFailed(0).location == Some(SeeStackDepthException))
          assert(testFailed(0).formatter == Some(IndentedText("- third", "third", 1)))
        }
        it("should report 1 test succeeded, 1 test failed and 1 test canceled event to the passed-in reporter when tests combined with andThen passed first but failed in second") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              Test0("first")(30).andThen(
                (Test1("second") { (i: Int) => throw new RuntimeException("oops!"); 1 }).andThen(
                  Test1("third") { (i: Int) => i + 1 }
                )
              )
          }
          assert(suite.testNames == Set("first", "second", "third"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 3)
          assert(testStarting(0).testName == "first")
          assert(testStarting(0).testText == "first")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 12, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "second")
          assert(testStarting(1).testText == "second")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 14, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "third")
          assert(testStarting(2).testText == "third")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 16, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 1)
          assert(testSucceeded(0).testName == "first")
          assert(testSucceeded(0).testText == "first")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 23, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- first", "first", 1)))
          val testFailed = myRep.testFailedEventsReceived
          assert(testFailed.size == 1)
          assert(testFailed(0).testName == "second")
          assert(testFailed(0).testText == "second")
          assert(testFailed(0).location == Some(SeeStackDepthException))
          assert(testFailed(0).formatter == Some(IndentedText("- second", "second", 1)))
          val testCanceled = myRep.testCanceledEventsReceived
          assert(testCanceled.size == 1)
          assert(testCanceled(0).testName == "third")
          assert(testCanceled(0).testText == "third")
          assert(testCanceled(0).location == Some(LineInFile(thisLineNumber - 33, "TestFlowSpec.scala", testFilePathname)))
          assert(testCanceled(0).formatter == Some(IndentedText("- third", "third", 1)))
        }
        it("should report 1 test succeeded and 1 test failed and 1 test canceled event to the passed-in reporter when compose with another Test1 and Test0, where the second one failed") {
          val myRep = new EventRecordingReporter
          val first = Test0("first")(5)
          val second = Test1("second") { (i: Int) => throw new RuntimeException("oops!"); 1 }
          val third = Test1("third") { (i: Int) => i + 1 }
          val suite = new TestFlow {
            val flow = third.compose(second.compose(first))
          }
          assert(suite.testNames == Set("first", "second", "third"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 3)
          assert(testStarting(0).testName == "first")
          assert(testStarting(0).testText == "first")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 12, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "second")
          assert(testStarting(1).testText == "second")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 14, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "third")
          assert(testStarting(2).testText == "third")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 16, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 1)
          assert(testSucceeded(0).testName == "first")
          assert(testSucceeded(0).testText == "first")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 23, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- first", "first", 1)))
          val testFailed = myRep.testFailedEventsReceived
          assert(testFailed.size == 1)
          assert(testFailed(0).testName == "second")
          assert(testFailed(0).testText == "second")
          assert(testFailed(0).location == Some(SeeStackDepthException))
          assert(testFailed(0).formatter == Some(IndentedText("- second", "second", 1)))
          val testCanceled = myRep.testCanceledEventsReceived
          assert(testCanceled.size == 1)
          assert(testCanceled(0).testName == "third")
          assert(testCanceled(0).testText == "third")
          assert(testCanceled(0).location == Some(LineInFile(thisLineNumber - 33, "TestFlowSpec.scala", testFilePathname)))
          assert(testCanceled(0).formatter == Some(IndentedText("- third", "third", 1)))
        }
      }
      describe("when the test cancels") {
        it("should report 1 test succeeded and 1 test canceled event to the passed-in reporter when compose with another Test0") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow = Test1("second") { (i: Int) =>
              cancel
            }.compose(Test0("first")(5))
          }
          assert(suite.testNames == Set("first", "second"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 2)
          assert(testStarting(0).testName == "first")
          assert(testStarting(0).testText == "first")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 8, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "second")
          assert(testStarting(1).testText == "second")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 13, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 1)
          assert(testSucceeded(0).testName == "first")
          assert(testSucceeded(0).testText == "first")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 16, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- first", "first", 1)))
          val testCanceled = myRep.testCanceledEventsReceived
          assert(testCanceled.size == 1)
          assert(testCanceled(0).testName == "second")
          assert(testCanceled(0).testText == "second")
          assert(testCanceled(0).location == Some(LineInFile(thisLineNumber - 23, "TestFlowSpec.scala", testFilePathname)))
          assert(testCanceled(0).formatter == Some(IndentedText("- second", "second", 1)))
        }
        it("should report 2 test succeeded and 1 test canceled event to the passed-in reporter when tests combined with andThen passed first 2 and canceled in last") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              Test0("first")(30).andThen(
                (Test1("second") { (i: Int) => i + 1 }).andThen(
                  Test1("third") { (i: Int) => cancel; 1 }
                )
              )
          }
          assert(suite.testNames == Set("first", "second", "third"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 3)
          assert(testStarting(0).testName == "first")
          assert(testStarting(0).testText == "first")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 12, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "second")
          assert(testStarting(1).testText == "second")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 14, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "third")
          assert(testStarting(2).testText == "third")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 16, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 2)
          assert(testSucceeded(0).testName == "first")
          assert(testSucceeded(0).testText == "first")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 23, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- first", "first", 1)))
          assert(testSucceeded(1).testName == "second")
          assert(testSucceeded(1).testText == "second")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 26, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- second", "second", 1)))
          val testCanceled = myRep.testCanceledEventsReceived
          assert(testCanceled.size == 1)
          assert(testCanceled(0).testName == "third")
          assert(testCanceled(0).testText == "third")
          assert(testCanceled(0).location == Some(LineInFile(thisLineNumber - 31, "TestFlowSpec.scala", testFilePathname)))
          assert(testCanceled(0).formatter == Some(IndentedText("- third", "third", 1)))
        }
        it("should report 1 test succeeded, 2 test canceled event to the passed-in reporter when tests combined with andThen passed first but canceled in second") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              Test0("first")(30).andThen(
                (Test1("second") { (i: Int) => cancel; 1 }).andThen(
                  Test1("third") { (i: Int) => i + 1 }
                )
              )
          }
          assert(suite.testNames == Set("first", "second", "third"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 3)
          assert(testStarting(0).testName == "first")
          assert(testStarting(0).testText == "first")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 12, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "second")
          assert(testStarting(1).testText == "second")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 14, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "third")
          assert(testStarting(2).testText == "third")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 16, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 1)
          assert(testSucceeded(0).testName == "first")
          assert(testSucceeded(0).testText == "first")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 23, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- first", "first", 1)))
          val testCanceled = myRep.testCanceledEventsReceived
          assert(testCanceled.size == 2)
          assert(testCanceled(0).testName == "second")
          assert(testCanceled(0).testText == "second")
          assert(testCanceled(0).location == Some(LineInFile(thisLineNumber - 28, "TestFlowSpec.scala", testFilePathname)))
          assert(testCanceled(0).formatter == Some(IndentedText("- second", "second", 1)))
          assert(testCanceled(1).testName == "third")
          assert(testCanceled(1).testText == "third")
          assert(testCanceled(1).location == Some(LineInFile(thisLineNumber - 31, "TestFlowSpec.scala", testFilePathname)))
          assert(testCanceled(1).formatter == Some(IndentedText("- third", "third", 1)))
        }
        it("should report 1 test succeeded and 2 test canceled event to the passed-in reporter when compose with another Test1 and Test0, where the second one canceled") {
          val myRep = new EventRecordingReporter
          val first = Test0("first")(5)
          val second = Test1("second") { (i: Int) => cancel; 1 }
          val third = Test1("third") { (i: Int) => i + 1 }
          val suite = new TestFlow {
            val flow = third.compose(second.compose(first))
          }
          assert(suite.testNames == Set("first", "second", "third"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 3)
          assert(testStarting(0).testName == "first")
          assert(testStarting(0).testText == "first")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 12, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "second")
          assert(testStarting(1).testText == "second")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 14, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "third")
          assert(testStarting(2).testText == "third")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 16, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 1)
          assert(testSucceeded(0).testName == "first")
          assert(testSucceeded(0).testText == "first")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 23, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- first", "first", 1)))
          val testCanceled = myRep.testCanceledEventsReceived
          assert(testCanceled.size == 2)
          assert(testCanceled(0).testName == "second")
          assert(testCanceled(0).testText == "second")
          assert(testCanceled(0).location == Some(LineInFile(thisLineNumber - 28, "TestFlowSpec.scala", testFilePathname)))
          assert(testCanceled(0).formatter == Some(IndentedText("- second", "second", 1)))
          assert(testCanceled(1).testName == "third")
          assert(testCanceled(1).testText == "third")
          assert(testCanceled(1).location == Some(LineInFile(thisLineNumber - 31, "TestFlowSpec.scala", testFilePathname)))
          assert(testCanceled(1).formatter == Some(IndentedText("- third", "third", 1)))
        }
        it("should report 2 test succeeded and 1 test canceled event to the passed-in reporter when compose with another Test1 and Test0, where the third one canceled") {
          val myRep = new EventRecordingReporter
          val first = Test0("first")(5)
          val second = Test1("second") { (i: Int) => i + 1 }
          val third = Test1("third") { (i: Int) => cancel; 1 }
          val suite = new TestFlow {
            val flow = third.compose(second).compose(first)
          }
          assert(suite.testNames == Set("first", "second", "third"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 3)
          assert(testStarting(0).testName == "first")
          assert(testStarting(0).testText == "first")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 12, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "second")
          assert(testStarting(1).testText == "second")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 14, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "third")
          assert(testStarting(2).testText == "third")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 16, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 2)
          assert(testSucceeded(0).testName == "first")
          assert(testSucceeded(0).testText == "first")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 23, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- first", "first", 1)))
          assert(testSucceeded(1).testName == "second")
          assert(testSucceeded(1).testText == "second")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 26, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- second", "second", 1)))
          val testCanceled = myRep.testCanceledEventsReceived
          assert(testCanceled.size == 1)
          assert(testCanceled(0).testName == "third")
          assert(testCanceled(0).testText == "third")
          assert(testCanceled(0).location == Some(LineInFile(thisLineNumber - 31, "TestFlowSpec.scala", testFilePathname)))
          assert(testCanceled(0).formatter == Some(IndentedText("- third", "third", 1)))
        }
      }
      describe("when the test is pending") {
        it("should report 1 test succeeded and 1 test pending event to the passed-in reporter when compose with another Test0") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              Test1("second") { (i: Int) =>
                pending
              }.compose(
                Test0("first")(5)
              )
          }
          assert(suite.testNames == Set("first", "second"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 2)
          assert(testStarting(0).testName == "first")
          assert(testStarting(0).testText == "first")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 9, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "second")
          assert(testStarting(1).testText == "second")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 15, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 1)
          assert(testSucceeded(0).testName == "first")
          assert(testSucceeded(0).testText == "first")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 17, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- first", "first", 1)))
          val testPending = myRep.testPendingEventsReceived
          assert(testPending.size == 1)
          assert(testPending(0).testName == "second")
          assert(testPending(0).testText == "second")
          assert(testPending(0).location == Some(LineInFile(thisLineNumber - 26, "TestFlowSpec.scala", testFilePathname)))
          assert(testPending(0).formatter == Some(IndentedText("- second", "second", 1)))
        }
        it("should report 2 test succeeded and 1 test pending event to the passed-in reporter when tests combined with andThen passed first 2 and is pending in last") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              Test0("first")(30).andThen(
                (Test1("second") { (i: Int) =>
                  i + 1
                }).andThen(
                  Test1("third") { (i: Int) =>
                    pending
                  }
                )
              )
          }
          assert(suite.testNames == Set("first", "second", "third"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 3)
          assert(testStarting(0).testName == "first")
          assert(testStarting(0).testText == "first")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 16, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "second")
          assert(testStarting(1).testText == "second")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 18, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "third")
          assert(testStarting(2).testText == "third")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 18, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 2)
          assert(testSucceeded(0).testName == "first")
          assert(testSucceeded(0).testText == "first")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 27, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- first", "first", 1)))
          assert(testSucceeded(1).testName == "second")
          assert(testSucceeded(1).testText == "second")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 30, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- second", "second", 1)))
          val testPending = myRep.testPendingEventsReceived
          assert(testPending.size == 1)
          assert(testPending(0).testName == "third")
          assert(testPending(0).testText == "third")
          assert(testPending(0).location == Some(LineInFile(thisLineNumber - 33, "TestFlowSpec.scala", testFilePathname)))
          assert(testPending(0).formatter == Some(IndentedText("- third", "third", 1)))
        }
        it("should report 1 test succeeded, 1 test pending and 1 test canceled event to the passed-in reporter when tests combined with andThen passed first but is pending in second") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              Test0("first")(30).andThen(
                (Test1("second") { (i: Int) =>
                  pending; 1
                }).andThen(
                  Test1("third") { (i: Int) =>
                    i + 1
                  }
                )
              )
          }
          assert(suite.testNames == Set("first", "second", "third"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 3)
          assert(testStarting(0).testName == "first")
          assert(testStarting(0).testText == "first")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 16, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "second")
          assert(testStarting(1).testName == "second")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 18, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "third")
          assert(testStarting(2).testText == "third")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 18, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 1)
          assert(testSucceeded(0).testName == "first")
          assert(testSucceeded(0).testText == "first")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 27, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- first", "first", 1)))
          val testPending = myRep.testPendingEventsReceived
          assert(testPending.size == 1)
          assert(testPending(0).testName == "second")
          assert(testPending(0).testText == "second")
          assert(testPending(0).location == Some(LineInFile(thisLineNumber - 32, "TestFlowSpec.scala", testFilePathname)))
          assert(testPending(0).formatter == Some(IndentedText("- second", "second", 1)))
          val testCanceled = myRep.testCanceledEventsReceived
          assert(testCanceled.size == 1)
          assert(testCanceled(0).testName == "third")
          assert(testCanceled(0).testText == "third")
          assert(testCanceled(0).location == Some(LineInFile(thisLineNumber - 35, "TestFlowSpec.scala", testFilePathname)))
          assert(testCanceled(0).formatter == Some(IndentedText("- third", "third", 1)))
        }
        it("should report 1 test succeeded 1 test pending and 1 test canceled event to the passed-in reporter when compose with another Test1 and Test0, where the second one is pending") {
          val myRep = new EventRecordingReporter
          val first = Test0("first")(5)
          val second = Test1("second") { (i: Int) => pending; 1 }
          val third = Test1("third") { (i: Int) => i + 1 }
          val suite = new TestFlow {
            val flow = third.compose(second.compose(first))
          }
          assert(suite.testNames == Set("first", "second", "third"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 3)
          assert(testStarting(0).testName == "first")
          assert(testStarting(0).testText == "first")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 12, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "second")
          assert(testStarting(1).testText == "second")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 14, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "third")
          assert(testStarting(2).testText == "third")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 16, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 1)
          assert(testSucceeded(0).testName == "first")
          assert(testSucceeded(0).testText == "first")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 23, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- first", "first", 1)))
          val testPending = myRep.testPendingEventsReceived
          assert(testPending.size == 1)
          assert(testPending(0).testName == "second")
          assert(testPending(0).testText == "second")
          assert(testPending(0).location == Some(LineInFile(thisLineNumber - 28, "TestFlowSpec.scala", testFilePathname)))
          assert(testPending(0).formatter == Some(IndentedText("- second", "second", 1)))
          val testCanceled = myRep.testCanceledEventsReceived
          assert(testCanceled.size == 1)
          assert(testCanceled(0).testName == "third")
          assert(testCanceled(0).testText == "third")
          assert(testCanceled(0).location == Some(LineInFile(thisLineNumber - 33, "TestFlowSpec.scala", testFilePathname)))
          assert(testCanceled(0).formatter == Some(IndentedText("- third", "third", 1)))
        }
        it("should report 2 test succeeded and 1 test pending event to the passed-in reporter when compose with another Test1 and Test0, where the third one is pending") {
          val myRep = new EventRecordingReporter
          val first = Test0("first")(5)
          val second = Test1("second") { (i: Int) => i + 1 }
          val third = Test1("third") { (i: Int) => pending; 1 }
          val suite = new TestFlow {
            val flow = third.compose(second).compose(first)
          }
          assert(suite.testNames == Set("first", "second", "third"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 3)
          assert(testStarting(0).testName == "first")
          assert(testStarting(0).testText == "first")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 12, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "second")
          assert(testStarting(1).testText == "second")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 14, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "third")
          assert(testStarting(2).testText == "third")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 16, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 2)
          assert(testSucceeded(0).testName == "first")
          assert(testSucceeded(0).testText == "first")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 23, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- first", "first", 1)))
          assert(testSucceeded(1).testName == "second")
          assert(testSucceeded(1).testText == "second")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 26, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- second", "second", 1)))
          val testPending = myRep.testPendingEventsReceived
          assert(testPending.size == 1)
          assert(testPending(0).testName == "third")
          assert(testPending(0).testText == "third")
          assert(testPending(0).location == Some(LineInFile(thisLineNumber - 31, "TestFlowSpec.scala", testFilePathname)))
          assert(testPending(0).formatter == Some(IndentedText("- third", "third", 1)))
        }
        it("should report 2 test succeeded event to the passed-in reporter when andThen with Test1 and InBetweenNode where 2 tests are passing") {
          val myRep = new EventRecordingReporter
          val first = Test0("first")(5)
          val second = Test1("second") { (i: Int) => i + 1 }
          val third = InBetweenNode { (i: Int) => i + 1 }
          val suite = new TestFlow {
            val flow = first.andThen(second.andThen(third))
          }
          assert(suite.testNames == Set("first", "second"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 2)
          assert(testStarting(0).testName == "first")
          assert(testStarting(0).testText == "first")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 12, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "second")
          assert(testStarting(1).testText == "second")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 14, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 2)
          assert(testSucceeded(0).testName == "first")
          assert(testSucceeded(0).testText == "first")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 20, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- first", "first", 1)))
          assert(testSucceeded(1).testName == "second")
          assert(testSucceeded(1).testText == "second")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 23, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- second", "second", 1)))
          val testPending = myRep.testPendingEventsReceived
          assert(testPending.size == 0)
        }
      }
    }
    it("should, perhaps, have a way to put a non-test in there, in the middle, for Fixture setup and teardown?") {
      pending
    }
    it("should, if above is true, perhaps pass the correct test name in the NoArgTest passed to withFixture") {
      pending
    }
    it("should, again if we do some kind of Fixture thing, pass the correct config map in the NoArgTest passed to withFixture") {
      pending
    }
    describe("(with info calls)") {
      it("should, when the info appears in the body before a test (which means that's one kind of Fixture thingy to insert in between), report the info before the test") {
        pending
      }
      it("should, when the info appears in the body after a test, report the info after the test runs") {
        pending
      }
      it("should (unless we pass in a communicator, which I think I don't want to do) print to stdout when info is called by a method invoked after the suite has been executed") {
        pending
      }
    }
    it("should throw NullArgumentException if a null test tag is provided") {
      pending // Tests can be tagged, so try this. What we could do if someone asks for a particular test is
              // just cancel everything downstream. Oh, and we'd need to do everything upstream.
    }
    it("should execute all tests when run is called with testName None") {
      pending
    }
    it("should execute a test when run is called with a defined testName, all upstream tests to that test, and cancel any downstream or sibling tests") {
      pending
    }
    it("should report as ignored, and not run, tests marked ignored, and ignore anything downstream") {
      pending
    }
    it("should ignore a test marked as ignored if run is invoked with that testName, and ignore anything downstream") {
      pending
    }
    it("should exclude a test with a tag included in the tagsToExclude set even if run is invoked with that testName (anything excluded is canceled, and downstreams are canceled too.") {
      pending
    }
    it("should offer a runTests method!") {
      pending
    }
    it("should exclude a test with a tag included in the tagsToExclude set even if run is invoked with that testName.") {
      pending
    }
    it("should exclude a test with a tag included in the tagsToExclude set even if run is invoked with that testName") {
      pending
    }
    it("should run only those tests selected by the tags to include and exclude sets") {
      pending
    }
    it("should return the correct test count from its expectedTestCount method") {
      pending
    }
    it("should generate a TestPending message when the test body is (pending)") {
      pending
    }
    it("should generate a test failure if a Throwable, or an Error other than direct Error subtypes " +
            "known in JDK 1.5, excluding AssertionError, is thrown") {
      pending
    }
    it("should propagate out Errors that are direct subtypes of Error in JDK 1.5, other than " +
            "AssertionError, causing Suites and Runs to abort.") {
      pending
    }
    it("should throw IllegalArgumentException if passed a testName that doesn't exist") {
      pending
    }
    it("should throw a NotAllowedException if chosenStyles is defined and does not include FlowSpec") {
      pending
    }
    it("should not mention tests with no tags in the tags map") {
      pending
    }
    it("should send strings passed to info calls inside a test to the reporter") {
      pending
    }
    it("should send stirngs passed to info calls in the constructor to the reporter") {
      pending
    }
    it("should send strings passed to info calls in the constructor to the reporter before the first test executes") {
      pending
    }
    it("should include test durations in test failed and test succeeded events fired from a FlowSpec") {
      pending
    }
  }

  describe("A BeforeNode") {
    it("should offer a factory method in its companion that takes a by-name of type Future[T]") {
      """BeforeNode { 99 }: BeforeNode[Int]""" should compile
      """BeforeNode { "hello" }: BeforeNode[String]""" should compile
      var x = false
      BeforeNode {
        x = true
      }
      x shouldBe false
    }
    describe("when it was not composed with anything else") {
      describe("when the test succeeds") {
        it("should report a test succeeded event to the passed-in reporter") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow = BeforeNode(42)
          }
          assert(suite.testNames == Set())
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 0)
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 0)
        }
      }
      describe("when the test fails") {
        it("should report a test failed event to the passed-in reporter") {
          val myRep = new EventRecordingReporter
          val suite =
            new TestFlow {
              val flow =
                BeforeNode {
                  throw new RuntimeException("oops!")
                }
            }
          assert(suite.testNames == Set())
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 0)
          val testFailed = myRep.testFailedEventsReceived
          assert(testFailed.size == 0)
        }
      }
    }
    describe("when it was composed with something else") {
      describe("when the test succeeds") {
        it("should report 2 test succeeded events to the passed-in reporter when andThen with another TestFlow") {
          val myRep = new EventRecordingReporter
          var isRun = false
          val suite = new TestFlow {
            val flow =
              BeforeNode(3).andThen(
                Test1("second") { (i: Int) => isRun = true; (i * 4).toString }
              )
          }
          assert(suite.testNames == Set("second"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 1)
          assert(testStarting(0).testName == "second")
          assert(testStarting(0).testText == "second")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 9, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 1)
          assert(testSucceeded(0).testName == "second")
          assert(testSucceeded(0).testText == "second")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 14, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- second", "second", 1)))
          assert(isRun)
        }
        it("should report 2 test succeeded events to the passed-in reporter when andThen with TestFlow that andThen with another TestFlow") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              BeforeNode(3).andThen(
                (Test1("second") { (i: Int) => (i * 4) }).andThen(
                  Test1("third") { (i: Int) => (i * 7).toString }
                )
              )
          }
          assert(suite.testNames == Set("second", "third"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 2)
          assert(testStarting(0).testName == "second")
          assert(testStarting(0).testText == "second")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 11, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "third")
          assert(testStarting(1).testText == "third")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 13, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 2)
          assert(testSucceeded(0).testName == "second")
          assert(testSucceeded(0).testText == "second")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 19, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- second", "second", 1)))
          assert(testSucceeded(1).testName == "third")
          assert(testSucceeded(1).testText == "third")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 22, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- third", "third", 1)))
        }
        it("should report 2 test succeeded events to the passed-in reporter when andThen with TestFlow that compose with another TestFlow") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              BeforeNode(3).andThen(
                (Test1("third") { (i: Int) => (i * 7).toString }).compose(
                  Test1("second") { (i: Int) => (i * 4) }
                )
              )
          }
          assert(suite.testNames == Set("second", "third"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 2)
          assert(testStarting(0).testName == "second")
          assert(testStarting(0).testText == "second")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 10, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "third")
          assert(testStarting(1).testText == "third")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 14, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 2)
          assert(testSucceeded(0).testName == "second")
          assert(testSucceeded(0).testText == "second")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 18, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- second", "second", 1)))
          assert(testSucceeded(1).testName == "third")
          assert(testSucceeded(1).testText == "third")
          assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 23, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(1).formatter == Some(IndentedText("- third", "third", 1)))
        }
      }
      describe("when the test fails") {
        it("should report 1 test succeeded and 1 test failed events to the passed-in reporter when first test passed and second test failed") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              BeforeNode(3).andThen(
                Test1("second") { (i: Int) => throw new RuntimeException("oops!!"); 1 }
              )
          }
          assert(suite.testNames == Set("second"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 1)
          assert(testStarting(0).testName == "second")
          assert(testStarting(0).testText == "second")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 9, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 0)
          val testFailed = myRep.testFailedEventsReceived
          assert(testFailed.size == 1)
          assert(testFailed(0).testName == "second")
          assert(testFailed(0).testText == "second")
          assert(testFailed(0).location == Some(SeeStackDepthException))
          assert(testFailed(0).formatter == Some(IndentedText("- second", "second", 1)))
        }
      }
      describe("when the test cancel") {
        it("should report 1 test canceled evetns to the passed-in reporter when the test canceled") {
          val myRep = new EventRecordingReporter
          var isRun = false
          val suite = new TestFlow {
            val flow =
              BeforeNode {
                isRun = true
                cancel
              }
          }
          assert(suite.testNames == Set())
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 0)
          val testCanceled = myRep.testCanceledEventsReceived
          assert(testCanceled.size == 0)
          assert(isRun)
        }
        it("should report 2 test canceled events to the passed-in reporter when there are 2 tests and the first test canceled") {
          val myRep = new EventRecordingReporter
          var isRun = false
          val suite = new TestFlow {
            val t0 =
              BeforeNode {
                isRun = true
                cancel
                3
              }
            val flow =
              t0.andThen(
                Test1("second") { (i: Int) =>
                  i + 1
                }
              )
          }
          assert(suite.testNames == Set("second"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 1)
          assert(testStarting(0).testName == "second")
          assert(testStarting(0).testText == "second")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 11, "TestFlowSpec.scala", testFilePathname)))
          val testCanceled = myRep.testCanceledEventsReceived
          assert(testCanceled.size == 1)
          assert(testCanceled(0).testName == "second")
          assert(testCanceled(0).testText == "second")
          assert(testCanceled(0).location == Some(LineInFile(thisLineNumber - 16, "TestFlowSpec.scala", testFilePathname)))
          assert(testCanceled(0).formatter == Some(IndentedText("- second", "second", 1)))
          assert(isRun)
        }
        it("should report 1 test succeeded and 1 test canceled events to the passed-in reporter when first test passed and second test canceled") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              BeforeNode(3).andThen(
                Test1("second") { (i: Int) =>
                  cancel
                  1
                }
              )
          }
          assert(suite.testNames == Set("second"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 1)
          assert(testStarting(0).testName == "second")
          assert(testStarting(0).testText == "second")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 12, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 0)
          val testCanceled = myRep.testCanceledEventsReceived
          assert(testCanceled.size == 1)
          assert(testCanceled(0).testName == "second")
          assert(testCanceled(0).testText == "second")
          assert(testCanceled(0).location == Some(LineInFile(thisLineNumber - 18, "TestFlowSpec.scala", testFilePathname)))
          assert(testCanceled(0).formatter == Some(IndentedText("- second", "second", 1)))
        }
      }
      describe("when the test cancel") {
        it("should report 1 test pending evetns to the passed-in reporter when the test is pending") {
          val myRep = new EventRecordingReporter
          var isRun = false
          val suite = new TestFlow {
            val t: BeforeNode[PendingStatement] =
              BeforeNode {
                isRun = true
                pending
              }
            val flow = t
          }
          assert(suite.testNames == Set())
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 0)
          val testPending = myRep.testPendingEventsReceived
          assert(testPending.size == 0)
          assert(isRun)
        }
        it("should report 1 test pending and 1 test canceled events to the passed-in reporter when there are 2 tests and the first test is pending") {
          val myRep = new EventRecordingReporter
          var isRun = false
          val suite = new TestFlow {
            val t0 = BeforeNode {
              isRun = true
              pending
              3
            }
            val flow =
              t0.andThen(
                Test1("second") { (i: Int) =>
                  i + 1
                }
              )
          }
          assert(suite.testNames == Set("second"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 1)
          assert(testStarting(0).testName == "second")
          assert(testStarting(0).testText == "second")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 11, "TestFlowSpec.scala", testFilePathname)))
          val testPending = myRep.testPendingEventsReceived
          assert(testPending.size == 0)
          val testCanceled = myRep.testCanceledEventsReceived
          assert(testCanceled.size == 1)
          assert(testCanceled(0).testName == "second")
          assert(testCanceled(0).testText == "second")
          assert(testCanceled(0).location == Some(LineInFile(thisLineNumber - 18, "TestFlowSpec.scala", testFilePathname)))
          assert(testCanceled(0).formatter == Some(IndentedText("- second", "second", 1)))
          assert(isRun)
        }
        it("should report 1 test succeeded and 1 test pending events to the passed-in reporter when first test passed and second test is pending") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              BeforeNode(3).andThen(
                Test1("second") { (i: Int) =>
                  pending
                  1
                }
              )
          }
          assert(suite.testNames == Set("second"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 1)
          assert(testStarting(0).testName == "second")
          assert(testStarting(0).testText == "second")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 12, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 0)
          val testPending = myRep.testPendingEventsReceived
          assert(testPending.size == 1)
          assert(testPending(0).testName == "second")
          assert(testPending(0).testText == "second")
          assert(testPending(0).location == Some(LineInFile(thisLineNumber - 19, "TestFlowSpec.scala", testFilePathname)))
          assert(testPending(0).formatter == Some(IndentedText("- second", "second", 1)))
        }
      }
    }
  }

  describe("A InBetweenNode") {
    it("should offer a factory method in its companion that takes a by-name of type Future[T]") {
      """InBetweenNode { (u: Unit) => 99 }: InBetweenNode[Unit, Int]""" should compile
      """InBetweenNode { (i: Long) => "hello" }: InBetweenNode[Long, String]""" should compile
      var x = false
      InBetweenNode { (i: Int) =>
        x = true
      }
      x shouldBe false
    }
    val fut = Future.successful(99)
    describe("when it was composed with something else") {
      describe("when the code succeeds") {
        it("should report 1 test succeeded events to the passed-in reporter when compose with another Test0") {
          val myRep = new EventRecordingReporter
          var isRun = false
          val suite = new TestFlow {
            val flow =
              InBetweenNode { (i: Int) =>
                isRun = true
                (i * 4).toString
              }.compose(
                Test0("first")(5)
              )
          }
          assert(suite.testNames == Set("first"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 1)
          assert(testStarting(0).testName == "first")
          assert(testStarting(0).testText == "first")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 9, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 1)
          assert(testSucceeded(0).testName == "first")
          assert(testSucceeded(0).testText == "first")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 14, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- first", "first", 1)))
          assert(isRun)
        }
      }
      describe("when the test fails") {
        it("should report 1 test succeeded and 1 test failed event to the passed-in reporter when compose with another Test0") {
          val myRep = new EventRecordingReporter
          var isRun = false
          val suite = new TestFlow {
            val flow = InBetweenNode { (i: Int) =>
              isRun = true
              throw new RuntimeException("oops!")
            }.compose(
              Test0("first")(5)
            )
          }
          assert(suite.testNames == Set("first"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 1)
          assert(testStarting(0).testName == "first")
          assert(testStarting(0).testText == "first")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 9, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 1)
          assert(testSucceeded(0).testName == "first")
          assert(testSucceeded(0).testText == "first")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 14, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- first", "first", 1)))
          val testFailed = myRep.testFailedEventsReceived
          assert(testFailed.size == 0)
          assert(isRun)
        }
        it("should report 1 test succeeded and 1 test failed event to the passed-in reporter when tests combined with andThen passed first and failed in second") {
          val myRep = new EventRecordingReporter
          var isRun = false
          val suite = new TestFlow {
            val flow =
              Test0("first")(30).andThen(
                (InBetweenNode { (i: Int) =>
                  isRun = true
                  i + 1
                }).andThen(
                  Test1("second") { (i: Int) =>
                    throw new RuntimeException("oops!")
                    1
                  }
                )
              )
          }
          assert(suite.testNames == Set("first", "second"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 2)
          assert(testStarting(0).testName == "first")
          assert(testStarting(0).testText == "first")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 18, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "second")
          assert(testStarting(1).testText == "second")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 16, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 1)
          assert(testSucceeded(0).testName == "first")
          assert(testSucceeded(0).testText == "first")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 26, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- first", "first", 1)))
          val testFailed = myRep.testFailedEventsReceived
          assert(testFailed.size == 1)
          assert(testFailed(0).testName == "second")
          assert(testFailed(0).testText == "second")
          assert(testFailed(0).location == Some(SeeStackDepthException))
          assert(testFailed(0).formatter == Some(IndentedText("- second", "second", 1)))
          assert(isRun)
        }
        it("should report 1 test succeeded, 1 test failed and 1 test canceled event to the passed-in reporter when tests combined with andThen passed first but failed in second") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              Test0("first")(30).andThen(
                (Test1("second") { (i: Int) => throw new RuntimeException("oops!"); 1 }).andThen(
                  Test1("third") { (i: Int) => i + 1 }
                )
              )
          }
          assert(suite.testNames == Set("first", "second", "third"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 3)
          assert(testStarting(0).testName == "first")
          assert(testStarting(0).testText == "first")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 12, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "second")
          assert(testStarting(1).testText == "second")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 14, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(2).testName == "third")
          assert(testStarting(2).testText == "third")
          assert(testStarting(2).location == Some(LineInFile(thisLineNumber - 16, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 1)
          assert(testSucceeded(0).testName == "first")
          assert(testSucceeded(0).testText == "first")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 23, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- first", "first", 1)))
          val testFailed = myRep.testFailedEventsReceived
          assert(testFailed.size == 1)
          assert(testFailed(0).testName == "second")
          assert(testFailed(0).testText == "second")
          assert(testFailed(0).location == Some(SeeStackDepthException))
          assert(testFailed(0).formatter == Some(IndentedText("- second", "second", 1)))
          val testCanceled = myRep.testCanceledEventsReceived
          assert(testCanceled.size == 1)
          assert(testCanceled(0).testName == "third")
          assert(testCanceled(0).testText == "third")
          assert(testCanceled(0).location == Some(LineInFile(thisLineNumber - 33, "TestFlowSpec.scala", testFilePathname)))
          assert(testCanceled(0).formatter == Some(IndentedText("- third", "third", 1)))
        }
        it("should report 1 test succeeded and 1 test canceled event to the passed-in reporter when compose with another Test1 and Test0, where the second one failed") {
          val myRep = new EventRecordingReporter
          var isRun = false
          val first = Test0("first")(5)
          val second = InBetweenNode { (i: Int) => isRun = true; throw new RuntimeException("oops!"); 1 }
          val third = Test1("second") { (i: Int) => i + 1 }
          val suite = new TestFlow {
            val flow = third.compose(second.compose(first))
          }
          assert(suite.testNames == Set("first", "second"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 2)
          assert(testStarting(0).testName == "first")
          assert(testStarting(0).testText == "first")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 12, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "second")
          assert(testStarting(1).testText == "second")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 13, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 1)
          assert(testSucceeded(0).testName == "first")
          assert(testSucceeded(0).testText == "first")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 20, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- first", "first", 1)))
          val testFailed = myRep.testFailedEventsReceived
          assert(testFailed.size == 0)
          val testCanceled = myRep.testCanceledEventsReceived
          assert(testCanceled.size == 1)
          assert(testCanceled(0).testName == "second")
          assert(testCanceled(0).testText == "second")
          assert(testCanceled(0).location == Some(LineInFile(thisLineNumber - 26, "TestFlowSpec.scala", testFilePathname)))
          assert(testCanceled(0).formatter == Some(IndentedText("- second", "second", 1)))
          assert(isRun)
        }
      }
      describe("when the test cancels") {
        it("should report 1 test canceled event to the passed-in reporter when compose with another Test0") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow = InBetweenNode { (i: Int) =>
              cancel
            }.compose(Test0("first")(5))
          }
          assert(suite.testNames == Set("first"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 1)
          assert(testStarting(0).testName == "first")
          assert(testStarting(0).testText == "first")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 8, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 1)
          assert(testSucceeded(0).testName == "first")
          assert(testSucceeded(0).testText == "first")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 13, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- first", "first", 1)))
          val testCanceled = myRep.testCanceledEventsReceived
          assert(testCanceled.size == 0)
        }
        it("should report 2 test succeeded and 1 test canceled event to the passed-in reporter when tests combined with andThen passed first 2 and canceled in last") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              Test0("first")(30).andThen(
                (InBetweenNode { (i: Int) => i + 1 }).andThen(
                  Test1("second") { (i: Int) => cancel; 1 }
                )
              )
          }
          assert(suite.testNames == Set("first", "second"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 2)
          assert(testStarting(0).testName == "first")
          assert(testStarting(0).testText == "first")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 12, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "second")
          assert(testStarting(1).testText == "second")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 13, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 1)
          assert(testSucceeded(0).testName == "first")
          assert(testSucceeded(0).testText == "first")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 20, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- first", "first", 1)))
          val testCanceled = myRep.testCanceledEventsReceived
          assert(testCanceled.size == 1)
          assert(testCanceled(0).testName == "second")
          assert(testCanceled(0).testText == "second")
          assert(testCanceled(0).location == Some(LineInFile(thisLineNumber - 24, "TestFlowSpec.scala", testFilePathname)))
          assert(testCanceled(0).formatter == Some(IndentedText("- second", "second", 1)))
        }
        it("should report 1 test succeeded, 1 test canceled event to the passed-in reporter when tests combined with andThen passed first but canceled in second") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              Test0("first")(30).andThen(
                (InBetweenNode { (i: Int) => cancel; 1 }).andThen(
                  Test1("second") { (i: Int) => i + 1 }
                )
              )
          }
          assert(suite.testNames == Set("first", "second"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 2)
          assert(testStarting(0).testName == "first")
          assert(testStarting(0).testText == "first")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 12, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "second")
          assert(testStarting(1).testText == "second")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 13, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 1)
          assert(testSucceeded(0).testName == "first")
          assert(testSucceeded(0).testText == "first")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 20, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- first", "first", 1)))
          val testCanceled = myRep.testCanceledEventsReceived
          assert(testCanceled.size == 1)
          assert(testCanceled(0).testName == "second")
          assert(testCanceled(0).testText == "second")
          assert(testCanceled(0).location == Some(LineInFile(thisLineNumber - 24, "TestFlowSpec.scala", testFilePathname)))
          assert(testCanceled(0).formatter == Some(IndentedText("- second", "second", 1)))
        }
        it("should report 1 test succeeded and 1 test canceled event to the passed-in reporter when compose with another Test1 and Test0, where the second one canceled") {
          val myRep = new EventRecordingReporter
          val first = Test0("first")(5)
          val second = InBetweenNode { (i: Int) => cancel; 1 }
          val third = Test1("second") { (i: Int) => i + 1 }
          val suite = new TestFlow {
            val flow = third.compose(second.compose(first))
          }
          assert(suite.testNames == Set("first", "second"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 2)
          assert(testStarting(0).testName == "first")
          assert(testStarting(0).testText == "first")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 12, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "second")
          assert(testStarting(1).testText == "second")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 13, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 1)
          assert(testSucceeded(0).testName == "first")
          assert(testSucceeded(0).testText == "first")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 20, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- first", "first", 1)))
          val testCanceled = myRep.testCanceledEventsReceived
          assert(testCanceled.size == 1)
          assert(testCanceled(0).testName == "second")
          assert(testCanceled(0).testText == "second")
          assert(testCanceled(0).location == Some(LineInFile(thisLineNumber - 24, "TestFlowSpec.scala", testFilePathname)))
          assert(testCanceled(0).formatter == Some(IndentedText("- second", "second", 1)))
        }
        it("should report 2 test succeeded and 1 test canceled event to the passed-in reporter when compose with another Test1 and Test0, where the third one canceled") {
          val myRep = new EventRecordingReporter
          val first = Test0("first")(5)
          val second = InBetweenNode { (i: Int) => i + 1 }
          val third = Test1("second") { (i: Int) => cancel; 1 }
          val suite = new TestFlow {
            val flow = third.compose(second).compose(first)
          }
          assert(suite.testNames == Set("first", "second"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 2)
          assert(testStarting(0).testName == "first")
          assert(testStarting(0).testText == "first")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 12, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "second")
          assert(testStarting(1).testText == "second")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 13, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 1)
          assert(testSucceeded(0).testName == "first")
          assert(testSucceeded(0).testText == "first")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 20, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- first", "first", 1)))
          val testCanceled = myRep.testCanceledEventsReceived
          assert(testCanceled.size == 1)
          assert(testCanceled(0).testName == "second")
          assert(testCanceled(0).testText == "second")
          assert(testCanceled(0).location == Some(LineInFile(thisLineNumber - 24, "TestFlowSpec.scala", testFilePathname)))
          assert(testCanceled(0).formatter == Some(IndentedText("- second", "second", 1)))
        }
      }
      describe("when the test is pending") {
        it("should report 1 test succeeded event to the passed-in reporter when compose with another Test0") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              InBetweenNode { (i: Int) =>
                pending
              }.compose(
                Test0("first")(5)
              )
          }
          assert(suite.testNames == Set("first"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 1)
          assert(testStarting(0).testName == "first")
          assert(testStarting(0).testText == "first")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 9, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 1)
          assert(testSucceeded(0).testName == "first")
          assert(testSucceeded(0).testText == "first")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 14, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- first", "first", 1)))
          val testPending = myRep.testPendingEventsReceived
          assert(testPending.size == 0)
        }
        it("should report 1 test succeeded and 1 test pending event to the passed-in reporter when tests combined with andThen passed first and is pending in last") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              Test0("first")(30).andThen(
                (InBetweenNode { (i: Int) =>
                  i + 1
                }).andThen(
                  Test1("second") { (i: Int) =>
                    pending
                  }
                )
              )
          }
          assert(suite.testNames == Set("first", "second"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 2)
          assert(testStarting(0).testName == "first")
          assert(testStarting(0).testText == "first")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 16, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "second")
          assert(testStarting(1).testText == "second")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 15, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 1)
          assert(testSucceeded(0).testName == "first")
          assert(testSucceeded(0).testText == "first")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 24, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- first", "first", 1)))
          val testPending = myRep.testPendingEventsReceived
          assert(testPending.size == 1)
          assert(testPending(0).testName == "second")
          assert(testPending(0).testText == "second")
          assert(testPending(0).location == Some(LineInFile(thisLineNumber - 26, "TestFlowSpec.scala", testFilePathname)))
          assert(testPending(0).formatter == Some(IndentedText("- second", "second", 1)))
        }
        it("should report 1 test succeeded and 1 test canceled event to the passed-in reporter when tests combined with andThen passed first but is pending in InBetweenNode") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow {
            val flow =
              Test0("first")(30).andThen(
                (InBetweenNode { (i: Int) =>
                  pending; 1
                }).andThen(
                  Test1("second") { (i: Int) =>
                    i + 1
                  }
                )
              )
          }
          assert(suite.testNames == Set("first", "second"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 2)
          assert(testStarting(0).testName == "first")
          assert(testStarting(0).testText == "first")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 16, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "second")
          assert(testStarting(1).testName == "second")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 15, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 1)
          assert(testSucceeded(0).testName == "first")
          assert(testSucceeded(0).testText == "first")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 24, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- first", "first", 1)))
          val testPending = myRep.testPendingEventsReceived
          assert(testPending.size == 0)
          val testCanceled = myRep.testCanceledEventsReceived
          assert(testCanceled.size == 1)
          assert(testCanceled(0).testName == "second")
          assert(testCanceled(0).testText == "second")
          assert(testCanceled(0).location == Some(LineInFile(thisLineNumber - 28, "TestFlowSpec.scala", testFilePathname)))
          assert(testCanceled(0).formatter == Some(IndentedText("- second", "second", 1)))
        }
        it("should report 1 test succeeded and 1 test canceled event to the passed-in reporter when compose with another Test1 and Test0, where the second InBetweenNode is pending") {
          val myRep = new EventRecordingReporter
          val first = Test0("first")(5)
          val second = InBetweenNode { (i: Int) => pending; 1 }
          val third = Test1("second") { (i: Int) => i + 1 }
          val suite = new TestFlow {
            val flow = third.compose(second.compose(first))
          }
          assert(suite.testNames == Set("first", "second"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 2)
          assert(testStarting(0).testName == "first")
          assert(testStarting(0).testText == "first")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 12, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "second")
          assert(testStarting(1).testText == "second")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 13, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 1)
          assert(testSucceeded(0).testName == "first")
          assert(testSucceeded(0).testText == "first")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 20, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- first", "first", 1)))
          val testPending = myRep.testPendingEventsReceived
          assert(testPending.size == 0)
          val testCanceled = myRep.testCanceledEventsReceived
          assert(testCanceled.size == 1)
          assert(testCanceled(0).testName == "second")
          assert(testCanceled(0).testText == "second")
          assert(testCanceled(0).location == Some(LineInFile(thisLineNumber - 26, "TestFlowSpec.scala", testFilePathname)))
          assert(testCanceled(0).formatter == Some(IndentedText("- second", "second", 1)))
        }
        it("should report 1 test succeeded and 1 test pending event to the passed-in reporter when compose with another InBetweenNode and Test0, where the last one is pending") {
          val myRep = new EventRecordingReporter
          val first = Test0("first")(5)
          val second = InBetweenNode { (i: Int) => i + 1 }
          val third = Test1("second") { (i: Int) => pending; 1 }
          val suite = new TestFlow {
            val flow = third.compose(second).compose(first)
          }
          assert(suite.testNames == Set("first", "second"))
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 2)
          assert(testStarting(0).testName == "first")
          assert(testStarting(0).testText == "first")
          assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 12, "TestFlowSpec.scala", testFilePathname)))
          assert(testStarting(1).testName == "second")
          assert(testStarting(1).testText == "second")
          assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 13, "TestFlowSpec.scala", testFilePathname)))
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 1)
          assert(testSucceeded(0).testName == "first")
          assert(testSucceeded(0).testText == "first")
          assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 20, "TestFlowSpec.scala", testFilePathname)))
          assert(testSucceeded(0).formatter == Some(IndentedText("- first", "first", 1)))
          val testPending = myRep.testPendingEventsReceived
          assert(testPending.size == 1)
          assert(testPending(0).testName == "second")
          assert(testPending(0).testText == "second")
          assert(testPending(0).location == Some(LineInFile(thisLineNumber - 24, "TestFlowSpec.scala", testFilePathname)))
          assert(testPending(0).formatter == Some(IndentedText("- second", "second", 1)))
        }
      }
    }
    it("should, perhaps, have a way to put a non-test in there, in the middle, for Fixture setup and teardown?") {
      pending
    }
    it("should, if above is true, perhaps pass the correct test name in the NoArgTest passed to withFixture") {
      pending
    }
    it("should, again if we do some kind of Fixture thing, pass the correct config map in the NoArgTest passed to withFixture") {
      pending
    }
    describe("(with info calls)") {
      it("should, when the info appears in the body before a test (which means that's one kind of Fixture thingy to insert in between), report the info before the test") {
        pending
      }
      it("should, when the info appears in the body after a test, report the info after the test runs") {
        pending
      }
      it("should (unless we pass in a communicator, which I think I don't want to do) print to stdout when info is called by a method invoked after the suite has been executed") {
        pending
      }
    }
    it("should throw NullArgumentException if a null test tag is provided") {
      pending // Tests can be tagged, so try this. What we could do if someone asks for a particular test is
      // just cancel everything downstream. Oh, and we'd need to do everything upstream.
    }
    it("should execute all tests when run is called with testName None") {
      pending
    }
    it("should execute a test when run is called with a defined testName, all upstream tests to that test, and cancel any downstream or sibling tests") {
      pending
    }
    it("should report as ignored, and not run, tests marked ignored, and ignore anything downstream") {
      pending
    }
    it("should ignore a test marked as ignored if run is invoked with that testName, and ignore anything downstream") {
      pending
    }
    it("should exclude a test with a tag included in the tagsToExclude set even if run is invoked with that testName (anything excluded is canceled, and downstreams are canceled too.") {
      pending
    }
    it("should offer a runTests method!") {
      pending
    }
    it("should exclude a test with a tag included in the tagsToExclude set even if run is invoked with that testName.") {
      pending
    }
    it("should exclude a test with a tag included in the tagsToExclude set even if run is invoked with that testName") {
      pending
    }
    it("should run only those tests selected by the tags to include and exclude sets") {
      pending
    }
    it("should return the correct test count from its expectedTestCount method") {
      pending
    }
    it("should generate a TestPending message when the test body is (pending)") {
      pending
    }
    it("should generate a test failure if a Throwable, or an Error other than direct Error subtypes " +
      "known in JDK 1.5, excluding AssertionError, is thrown") {
      pending
    }
    it("should propagate out Errors that are direct subtypes of Error in JDK 1.5, other than " +
      "AssertionError, causing Suites and Runs to abort.") {
      pending
    }
    it("should throw IllegalArgumentException if passed a testName that doesn't exist") {
      pending
    }
    it("should throw a NotAllowedException if chosenStyles is defined and does not include FlowSpec") {
      pending
    }
    it("should not mention tests with no tags in the tags map") {
      pending
    }
    it("should send strings passed to info calls inside a test to the reporter") {
      pending
    }
    it("should send stirngs passed to info calls in the constructor to the reporter") {
      pending
    }
    it("should send strings passed to info calls in the constructor to the reporter before the first test executes") {
      pending
    }
    it("should include test durations in test failed and test succeeded events fired from a FlowSpec") {
      pending
    }
  }

  describe("A AfterNode") {
    it("should offer a factory method in its companion that takes a by-name of type Future[T]") {
      """AfterNode { i: Int => 99 }: AfterNode[Int]""" should compile
      """AfterNode { i: String => }: AfterNode[String]""" should compile
      var x = false
      AfterNode { i: Int =>
        x = true
      }
      x shouldBe false
    }
    it("can be used with BeforeNode's andThen") {
      val myRep = new EventRecordingReporter
      var x = false
      val suite = new TestFlow {
        val flow =
          BeforeNode(30).andThen(
            AfterNode { i: Int =>
              x = true
              ()
            }
          )
      }
      suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
      val testStarting = myRep.testStartingEventsReceived
      assert(testStarting.size == 0)
      val testSucceeded = myRep.testSucceededEventsReceived
      assert(testSucceeded.size == 0)
      assert(x)
    }
    it("can be used with Test0's andThen") {
      val myRep = new EventRecordingReporter
      var x = false
      val suite = new TestFlow {
        val flow =
          Test0("first")(30).andThen(
            AfterNode { i: Int =>
              x = true
              ()
            }
          )
      }
      assert(suite.testNames == Set("first"))
      suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
      val testStarting = myRep.testStartingEventsReceived
      assert(testStarting.size == 1)
      assert(testStarting(0).testName == "first")
      assert(testStarting(0).testText == "first")
      assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 13, "TestFlowSpec.scala", testFilePathname)))
      val testSucceeded = myRep.testSucceededEventsReceived
      assert(testSucceeded.size == 1)
      assert(testSucceeded(0).testName == "first")
      assert(testSucceeded(0).testText == "first")
      assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 18, "TestFlowSpec.scala", testFilePathname)))
      assert(x)
    }
    it("can be used with Test1's andThen") {
      val myRep = new EventRecordingReporter
      var x = false
      val suite = new TestFlow {
        val flow =
          Test0("first")(30).andThen(
            Test1("second") { i: Int =>
              i + 1
            }.andThen(
              AfterNode { i: Int =>
                x = true
                ()
              }
            )
          )
      }
      assert(suite.testNames == Set("first", "second"))
      suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
      val testStarting = myRep.testStartingEventsReceived
      assert(testStarting.size == 2)
      assert(testStarting(0).testName == "first")
      assert(testStarting(0).testText == "first")
      assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 17, "TestFlowSpec.scala", testFilePathname)))
      assert(testStarting(1).testName == "second")
      assert(testStarting(1).testText == "second")
      assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 19, "TestFlowSpec.scala", testFilePathname)))
      val testSucceeded = myRep.testSucceededEventsReceived
      assert(testSucceeded.size == 2)
      assert(testSucceeded(0).testName == "first")
      assert(testSucceeded(0).testText == "first")
      assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 25, "TestFlowSpec.scala", testFilePathname)))
      assert(testSucceeded(1).testName == "second")
      assert(testSucceeded(1).testText == "second")
      assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 27, "TestFlowSpec.scala", testFilePathname)))
      assert(x)
    }
    it("can be used with InBetweenNode's andThen") {
      val myRep = new EventRecordingReporter
      var x = false
      val suite = new TestFlow {
        val flow =
          Test0("first")(30).andThen(
            InBetweenNode { i: Int =>
              i + 1
            }.andThen(
              AfterNode { i: Int =>
                x = true
                ()
              }
            )
          )
      }
      assert(suite.testNames == Set("first"))
      suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
      val testStarting = myRep.testStartingEventsReceived
      assert(testStarting.size == 1)
      assert(testStarting(0).testName == "first")
      assert(testStarting(0).testText == "first")
      assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 17, "TestFlowSpec.scala", testFilePathname)))
      val testSucceeded = myRep.testSucceededEventsReceived
      assert(testSucceeded.size == 1)
      assert(testSucceeded(0).testName == "first")
      assert(testSucceeded(0).testText == "first")
      assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 22, "TestFlowSpec.scala", testFilePathname)))
      assert(x)
    }
    it("should have a compose method that takes Test0") {
      val myRep = new EventRecordingReporter
      var x = false
      val suite = new TestFlow {
        val flow =
          AfterNode { i: Int =>
            x = true
            ()
          } compose (Test0("first")(30))
      }
      assert(suite.testNames == Set("first"))
      suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
      val testStarting = myRep.testStartingEventsReceived
      assert(testStarting.size == 1)
      assert(testStarting(0).testName == "first")
      assert(testStarting(0).testText == "first")
      assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 8, "TestFlowSpec.scala", testFilePathname)))
      val testSucceeded = myRep.testSucceededEventsReceived
      assert(testSucceeded.size == 1)
      assert(testSucceeded(0).testName == "first")
      assert(testSucceeded(0).testText == "first")
      assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 13, "TestFlowSpec.scala", testFilePathname)))
      assert(x)
    }
    it("should have a compose method that takes Test1") {
      val myRep = new EventRecordingReporter
      var x = false
      val suite = new TestFlow {
        val flow =
          Test0("first")(30).andThen(
            AfterNode { i: Int =>
              x = true
              ()
            } compose (
              Test1("second") { i: Int =>
                i + 1
              }
            )
          )
      }
      assert(suite.testNames == Set("first", "second"))
      suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
      val testStarting = myRep.testStartingEventsReceived
      assert(testStarting.size == 2)
      assert(testStarting(0).testName == "first")
      assert(testStarting(0).testText == "first")
      assert(testStarting(0).location == Some(LineInFile(thisLineNumber - 17, "TestFlowSpec.scala", testFilePathname)))
      assert(testStarting(1).testName == "second")
      assert(testStarting(1).testText == "second")
      assert(testStarting(1).location == Some(LineInFile(thisLineNumber - 15, "TestFlowSpec.scala", testFilePathname)))
      val testSucceeded = myRep.testSucceededEventsReceived
      assert(testSucceeded.size == 2)
      assert(testSucceeded(0).testName == "first")
      assert(testSucceeded(0).testText == "first")
      assert(testSucceeded(0).location == Some(LineInFile(thisLineNumber - 25, "TestFlowSpec.scala", testFilePathname)))
      assert(testSucceeded(1).testName == "second")
      assert(testSucceeded(1).testText == "second")
      assert(testSucceeded(1).location == Some(LineInFile(thisLineNumber - 23, "TestFlowSpec.scala", testFilePathname)))
      assert(x)
    }
    it("should have a compose method that takes BeforeNode") {
      val myRep = new EventRecordingReporter
      var x = false
      val suite = new TestFlow {
        val flow =
          AfterNode { i: Int =>
            x = true
            ()
          } compose (BeforeNode(30))
      }
      assert(suite.testNames == Set())
      suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
      val testStarting = myRep.testStartingEventsReceived
      assert(testStarting.size == 0)
      val testSucceeded = myRep.testSucceededEventsReceived
      assert(testSucceeded.size == 0)
      assert(x)
    }
    it("should have a compose method that takes InBetweenNode") {
      val myRep = new EventRecordingReporter
      var x = false
      val suite = new TestFlow {
        val flow =
          BeforeNode(30).andThen(
            AfterNode { i: Int =>
              x = true
              ()
            } compose (
              InBetweenNode { i: Int =>
                i + 1
              }
            )
          )
      }
      assert(suite.testNames == Set())
      suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
      val testStarting = myRep.testStartingEventsReceived
      assert(testStarting.size == 0)
      val testSucceeded = myRep.testSucceededEventsReceived
      assert(testSucceeded.size == 0)
      assert(x)
    }
  }

  /*describe("when failure happens") {
    it("should fire TestFailed event with correct stack depth info when test failed") {
      pending
    }
    it("should generate a DuplicateTestNameException when duplicate test name is detected") {
      pending
    }
    it("should generate a DuplicateTestNameException when duplicate test name is detected using ignore") {
      pending
    }
  }*/
}
/*
Going back at 9:24. Next tsts are adding test names.
TestFlow("bla bla bla") {
  Future(99)
}
This is wierd. Either it is completely thrown exception indicates failure.
Anything else indicates success. Or if I want a result, then it would need
to be a Tuple. No maybe an Else. No, an Or. Int Or Assertion. And could
be Int Or Expectation.

No, that's not Bad.

Int Or (No Else 

That's complex. If I just use exceptions, then I can return a result plain and simple.

I'll start there. Ok really 9:32 when I closed the lid.

Simple. Must throw an exception. No Logic.

Started again on Bart to the airport on Sunday April 2, 2017 at 5:25PM
*/

