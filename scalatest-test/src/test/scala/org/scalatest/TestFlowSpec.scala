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
package org.scalatest

import scala.concurrent.Future
import scala.util.Success
import SharedHelpers._
import exceptions.{DuplicateTestNameException, NotAllowedException}

class TestFlowSpec extends AsyncFunSpec with Matchers {
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
      Test0("first")(3).andThen(Test1("second") { (i: Int) => (i * 4).toString }).name shouldBe "first"
    }
    it("should have an andThen method") {
      Test0("first")(3).andThen(Test1("second") { (i: Int) => (i * 4).toString }).apply() shouldEqual "12"
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
          val suite = new TestFlow[Int] {
            val flow = Test0("happy path")(42)//.runTests(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          }
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 1)
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 1)
        }
      }
      describe("when the test fails") {
        it("should report a test failed event to the passed-in reporter") {
          val myRep = new EventRecordingReporter
          val suite =
            new TestFlow[Nothing] {
              val flow =
                Test0("happy path") {
                  throw new RuntimeException("oops!")
                }
            }
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 1)
          val testFailed = myRep.testFailedEventsReceived
          assert(testFailed.size == 1)
        }
      }
    }
    describe("when it was composed with something else") {
      describe("when the test succeeds") {
        it("should report 2 test succeeded events to the passed-in reporter when andThen with another TestFlow") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow[String] {
            val flow = Test0("first")(3).andThen(Test1("second") { (i: Int) => (i * 4).toString })
          }
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 2)
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 2)
        }
        it("should report 3 test succeeded events to the passed-in reporter when andThen with TestFlow that andThen with another TestFlow") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow[String] {
            val flow =
              Test0("first")(3).andThen(
                (Test1("second") { (i: Int) => (i * 4) }).andThen(
                  Test1("third") { (i: Int) => (i * 7).toString }
                )
              )
          }
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 3)
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 3)
        }
        it("should report 3 test succeeded events to the passed-in reporter when andThen with TestFlow that compose with another TestFlow") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow[String] {
            val flow =
              Test0("first")(3).andThen(
                (Test1("third") { (i: Int) => (i * 7).toString }).compose(
                  Test1("second") { (i: Int) => (i * 4) }
                )
              )
          }
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 3)
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 3)
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
          val suite = new TestFlow[Nothing] {
            val flow = Test0("first")(3).andThen(Test1("second") { (i: Int) => throw new RuntimeException("oops!!") })
          }
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 2)
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 1)
          val testFailed = myRep.testFailedEventsReceived
          assert(testFailed.size == 1)
        }
      }
      describe("when the test cancel") {
        it("should report 1 test canceled evetns to the passed-in reporter when the test canceled") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow[Nothing] {
            val flow = Test0("first") { cancel }
          }
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 1)
          val testCanceled = myRep.testCanceledEventsReceived
          assert(testCanceled.size == 1)
        }
        it("should report 2 test canceled events to the passed-in reporter when there are 2 tests and the first test canceled") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow[Int] {
            val t0 =
              Test0("first") {
                cancel
                3
              }
            val flow = t0.andThen(Test1("second") { (i: Int) => i + 1 })
          }
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 2)
          val testCanceled = myRep.testCanceledEventsReceived
          assert(testCanceled.size == 2)
        }
        it("should report 1 test succeeded and 1 test canceled events to the passed-in reporter when first test passed and second test canceled") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow[Nothing] {
            val flow = Test0("first")(3).andThen(Test1("second") { (i: Int) => cancel })
          }
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 2)
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 1)
          val testCanceled = myRep.testCanceledEventsReceived
          assert(testCanceled.size == 1)
        }
      }
      describe("when the test cancel") {
        it("should report 1 test pending evetns to the passed-in reporter when the test is pending") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow[PendingStatement] {
            val t: Test0[PendingStatement] = Test0("first") { pending }
            val flow = t
          }
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 1)
          val testPending = myRep.testPendingEventsReceived
          assert(testPending.size == 1)
        }
        it("should report 1 test pending and 1 test canceled events to the passed-in reporter when there are 2 tests and the first test is pending") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow[Int] {
            val t0 = Test0("first") {
              pending
              3
            }
            val flow = t0.andThen(Test1("second") { (i: Int) => i + 1 })
          }
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 2)
          val testPending = myRep.testPendingEventsReceived
          assert(testPending.size == 1)
          val testCanceled = myRep.testCanceledEventsReceived
          assert(testCanceled.size == 1)
        }
        it("should report 1 test succeeded and 1 test pending events to the passed-in reporter when first test passed and second test is pending") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow[PendingStatement] {
            val flow: Test0[PendingStatement] = Test0("first")(3).andThen(Test1("second") { (i: Int) => pending })
          }
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 2)
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 1)
          val testPending = myRep.testPendingEventsReceived
          assert(testPending.size == 1)
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
    it("should have a name method") {
      Test1("first")((i: Int) => i + 1).name shouldBe "first"
      Test1("first")((i: Int) => i + 1).andThen(Test1("second") { (i: Int) => (i * 4).toString }).name shouldBe "first"
      Test1("second") { (i: Int) => (i * 4).toString }.compose(Test1("first")((i: Int) => i + 1)).name shouldEqual "first"
      Test1("second") { (i: Int) => (i * 4).toString }.compose(Test0("first")(4)).name shouldEqual "first"
    }
    val fut = Future.successful(99)
    it("should have an andThen method") {
      Test1("first")((i: Int) => i + 1).andThen(Test1("second") { (i: Int) => (i * 4).toString }).apply(1) shouldEqual "8"
      // Test1(fut).andThen(Test1 { futI => futI.map(i => i + 1) }).value.map(i => i shouldEqual 100)
    }
    it("should have an overloaded compose method that takes another Test1") {
      Test1("first") { (i: Int) => (i * 4).toString }.compose(Test1("second")((i: Int) => i + 1)).apply(1) shouldEqual "8"
      // Test1(fut).andThen(Test1 { futI => futI.map(i => i + 1) }).value.map(i => i shouldEqual 100)
    }
    it("should have an overloaded compose method that takes a Test0") {
      Test1("second") { (i: Int) => (i * 4).toString }.compose(Test0("first")(5)).apply() shouldEqual "20"
      // Test1(fut).andThen(Test1 { futI => futI.map(i => i + 1) }).value.map(i => i shouldEqual 100)
    }
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
          val suite = new TestFlow[String] {
            val flow = Test1("second") { (i: Int) => (i * 4).toString }.compose(Test0("first")(5))
          }
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 2)
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 2)
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
      }
      describe("when the test fails") {
        it("should report 1 test succeeded and 1 test failed event to the passed-in reporter when compose with another Test0") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow[Nothing] {
            val flow = Test1("second") { (i: Int) => throw new RuntimeException("oops!") }.compose(Test0("first")(5))
          }
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 2)
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 1)
          val testFailed = myRep.testFailedEventsReceived
          assert(testFailed.size == 1)
        }
        it("should report 2 test succeeded and 1 test failed event to the passed-in reporter when tests combined with andThen passed first 2 and failed in last") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow[Nothing] {
            val flow = Test0("first")(30).andThen((Test1("second") { (i: Int) => i + 1 }).andThen(Test1("third") { (i: Int) => throw new RuntimeException("oops!") }))
          }
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 3)
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 2)
          val testFailed = myRep.testFailedEventsReceived
          assert(testFailed.size == 1)
        }
        it("should report 1 test succeeded, 1 test failed and 1 test canceled event to the passed-in reporter when tests combined with andThen passed first but failed in second") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow[Int] {
            val flow = Test0("first")(30).andThen((Test1("second") { (i: Int) => throw new RuntimeException("oops!"); 1 }).andThen(Test1("third") { (i: Int) => i + 1 }))
          }
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 3)
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 1)
          val testFailed = myRep.testFailedEventsReceived
          assert(testFailed.size == 1)
          val testCanceled = myRep.testCanceledEventsReceived
          assert(testCanceled.size == 1)
        }
        it("should report 1 test succeeded and 1 test failed and 1 test canceled event to the passed-in reporter when compose with another Test1 and Test0, where the second one failed") {
          val myRep = new EventRecordingReporter
          val first = Test0("first")(5)
          val second = Test1("second") { (i: Int) => throw new RuntimeException("oops!"); 1 }
          val third = Test1("third") { (i: Int) => i + 1 }
          val suite = new TestFlow[Int] {
            val flow = third.compose(second.compose(first))
          }
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 3)
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 1)
          val testFailed = myRep.testFailedEventsReceived
          assert(testFailed.size == 1)
          val testCanceled = myRep.testCanceledEventsReceived
          assert(testCanceled.size == 1)
        }
      }
      describe("when the test cancels") {
        it("should report 1 test succeeded and 1 test canceled event to the passed-in reporter when compose with another Test0") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow[Nothing] {
            val flow = Test1("second") { (i: Int) => cancel }.compose(Test0("first")(5))
          }
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 2)
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 1)
          val testCanceled = myRep.testCanceledEventsReceived
          assert(testCanceled.size == 1)
        }
        it("should report 2 test succeeded and 1 test canceled event to the passed-in reporter when tests combined with andThen passed first 2 and canceled in last") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow[Nothing] {
            val flow = Test0("first")(30).andThen((Test1("second") { (i: Int) => i + 1 }).andThen(Test1("third") { (i: Int) => cancel }))
          }
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 3)
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 2)
          val testCanceled = myRep.testCanceledEventsReceived
          assert(testCanceled.size == 1)
        }
        it("should report 1 test succeeded, 2 test canceled event to the passed-in reporter when tests combined with andThen passed first but canceled in second") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow[Int] {
            val flow = Test0("first")(30).andThen((Test1("second") { (i: Int) => cancel; 1 }).andThen(Test1("third") { (i: Int) => i + 1 }))
          }
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 3)
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 1)
          val testCanceled = myRep.testCanceledEventsReceived
          assert(testCanceled.size == 2)
        }
        it("should report 1 test succeeded and 2 test canceled event to the passed-in reporter when compose with another Test1 and Test0, where the second one canceled") {
          val myRep = new EventRecordingReporter
          val first = Test0("first")(5)
          val second = Test1("second") { (i: Int) => cancel; 1 }
          val third = Test1("third") { (i: Int) => i + 1 }
          val suite = new TestFlow[Int] {
            val flow = third.compose(second.compose(first))
          }
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 3)
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 1)
          val testCanceled = myRep.testCanceledEventsReceived
          assert(testCanceled.size == 2)
        }
        it("should report 2 test succeeded and 1 test canceled event to the passed-in reporter when compose with another Test1 and Test0, where the third one canceled") {
          val myRep = new EventRecordingReporter
          val first = Test0("first")(5)
          val second = Test1("second") { (i: Int) => i + 1 }
          val third = Test1("third") { (i: Int) => cancel; 1 }
          val suite = new TestFlow[Int] {
            val flow = third.compose(second).compose(first)
          }
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 3)
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 2)
          val testCanceled = myRep.testCanceledEventsReceived
          assert(testCanceled.size == 1)
        }
      }
      describe("when the test is pending") {
        it("should report 1 test succeeded and 1 test pending event to the passed-in reporter when compose with another Test0") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow[Assertion with PendingStatement] {
            val flow = Test1("second") { (i: Int) => pending }.compose(Test0("first")(5))
          }
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 2)
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 1)
          val testPending = myRep.testPendingEventsReceived
          assert(testPending.size == 1)
        }
        it("should report 2 test succeeded and 1 test pending event to the passed-in reporter when tests combined with andThen passed first 2 and is pending in last") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow[Assertion with PendingStatement] {
            val flow = Test0("first")(30).andThen((Test1("second") { (i: Int) => i + 1 }).andThen(Test1("third") { (i: Int) => pending }))
          }
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 3)
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 2)
          val testPending = myRep.testPendingEventsReceived
          assert(testPending.size == 1)
        }
        it("should report 1 test succeeded, 1 test pending and 1 test canceled event to the passed-in reporter when tests combined with andThen passed first but is pending in second") {
          val myRep = new EventRecordingReporter
          val suite = new TestFlow[Int] {
            val flow = Test0("first")(30).andThen((Test1("second") { (i: Int) => pending; 1 }).andThen(Test1("third") { (i: Int) => i + 1 }))
          }
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 3)
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 1)
          val testPending = myRep.testPendingEventsReceived
          assert(testPending.size == 1)
          val testCanceled = myRep.testCanceledEventsReceived
          assert(testCanceled.size == 1)
        }
        it("should report 1 test succeeded 1 test pending and 1 test canceled event to the passed-in reporter when compose with another Test1 and Test0, where the second one is pending") {
          val myRep = new EventRecordingReporter
          val first = Test0("first")(5)
          val second = Test1("second") { (i: Int) => pending; 1 }
          val third = Test1("third") { (i: Int) => i + 1 }
          val suite = new TestFlow[Int] {
            val flow = third.compose(second.compose(first))
          }
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 3)
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 1)
          val testPending = myRep.testPendingEventsReceived
          assert(testPending.size == 1)
          val testCanceled = myRep.testCanceledEventsReceived
          assert(testCanceled.size == 1)
        }
        it("should report 2 test succeeded and 1 test pending event to the passed-in reporter when compose with another Test1 and Test0, where the third one is pending") {
          val myRep = new EventRecordingReporter
          val first = Test0("first")(5)
          val second = Test1("second") { (i: Int) => i + 1 }
          val third = Test1("third") { (i: Int) => pending; 1 }
          val suite = new TestFlow[Int] {
            val flow = third.compose(second).compose(first)
          }
          suite.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
          val testStarting = myRep.testStartingEventsReceived
          assert(testStarting.size == 3)
          val testSucceeded = myRep.testSucceededEventsReceived
          assert(testSucceeded.size == 2)
          val testPending = myRep.testPendingEventsReceived
          assert(testPending.size == 1)
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

