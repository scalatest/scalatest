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

import SharedHelpers._
import org.scalatest.events._
import tools.Runner.CHOSEN_STYLES
/* Uncomment after remove type aliases in org.scalatest package object
import org.scalatest.exceptions.DuplicateTestNameException
import org.scalatest.exceptions.TestFailedException
import org.scalatest.exceptions.TestRegistrationClosedException
*/

class FunSuiteSpec extends FunSpec {

  describe("A FunSuite") {

    it("should return the test names in registration order from testNames") {
      
      val a = new FunSuite {
        test("test this") {}
        test("test that") {}
      }

      assertResult(List("test this", "test that")) {
        a.testNames.iterator.toList
      }

      val b = new FunSuite {}

      assertResult(List[String]()) {
        b.testNames.iterator.toList
      }

      val c = new FunSuite {
        test("test that") {}
        test("test this") {}
      }

      assertResult(List("test that", "test this")) {
        c.testNames.iterator.toList
      }
    }

    it("should throw NotAllowedException if a duplicate test name registration is attempted") {

      intercept[DuplicateTestNameException] {
        new FunSuite {
          test("test this") {}
          test("test this") {}
        }
      }
      intercept[DuplicateTestNameException] {
        new FunSuite {
          test("test this") {}
          ignore("test this") {}
        }
      }
      intercept[DuplicateTestNameException] {
        new FunSuite {
          ignore("test this") {}
          ignore("test this") {}
        }
      }
      intercept[DuplicateTestNameException] {
        new FunSuite {
          ignore("test this") {}
          test("test this") {}
        }
      }
    }

    it("should throw NotAllowedException if test registration is attempted after run has been invoked on a suite") {
      class InvokedWhenNotRunningSuite extends FunSuite {
        var fromMethodTestExecuted = false
        var fromConstructorTestExecuted = false
        test("from constructor") {
          fromConstructorTestExecuted = true
        }
        def tryToRegisterATest() {
          test("from method") {
            fromMethodTestExecuted = true
          }
        }
      }
      val suite = new InvokedWhenNotRunningSuite
      suite.run(None, Args(SilentReporter))
      assert(suite.fromConstructorTestExecuted)
      assert(!suite.fromMethodTestExecuted)
      intercept[TestRegistrationClosedException] {
        suite.tryToRegisterATest()
      }
      suite.run(None, Args(SilentReporter))
      assert(!suite.fromMethodTestExecuted)
/*
      class InvokedWhenRunningSuite extends FunSuite {
        var fromMethodTestExecuted = false
        var fromConstructorTestExecuted = false
        test("from constructor") {
          tryToRegisterATest()
          fromConstructorTestExecuted = true
        }
        def tryToRegisterATest() {
          test("from method") {
            fromMethodTestExecuted = true
          }
        }
      }
      val a = new InvokedWhenNotRunningSuite
      a.run()
      intercept[TestFailedException] {
        new InvokedWhenRunningSuite
      } */
    }

    it("should invoke withFixture from runTest") {
      val a = new FunSuite {
        var withFixtureWasInvoked = false
        var testWasInvoked = false
        override def withFixture(test: NoArgTest): Outcome = {
          withFixtureWasInvoked = true
          super.withFixture(test)
        }
        test("something") {
          testWasInvoked = true
        }
      }
      a.run(None, Args(SilentReporter))
      assert(a.withFixtureWasInvoked)
      assert(a.testWasInvoked)
    }
    it("should pass the correct test name in the NoArgTest passed to withFixture") {
      val a = new FunSuite {
        var correctTestNameWasPassed = false
        override def withFixture(test: NoArgTest): Outcome = {
          correctTestNameWasPassed = test.name == "something"
          super.withFixture(test)
        }
        test("something") {}
      }
      a.run(None, Args(SilentReporter))
      assert(a.correctTestNameWasPassed)
    }
    it("should pass the correct config map in the NoArgTest passed to withFixture") {
      val a = new FunSuite {
        var correctConfigMapWasPassed = false
        override def withFixture(test: NoArgTest): Outcome = {
          correctConfigMapWasPassed = (test.configMap == ConfigMap("hi" -> 7))
          super.withFixture(test)
        }
        test("something") {}
      }
      a.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap("hi" -> 7), None, new Tracker(), Set.empty))
      assert(a.correctConfigMapWasPassed)
    }

    describe("(with info calls)") {
      it("should, when the info appears in the body before a test, report the info before the test") {
        val msg = "hi there, dude"
        val testName = "test name"
        class MySuite extends FunSuite {
          info(msg)
          test(testName) {}
        }
        val (infoProvidedIndex, testStartingIndex, testSucceededIndex) =
          getIndexesForInformerEventOrderTests(new MySuite, testName, msg)
        assert(infoProvidedIndex < testStartingIndex)
        assert(testStartingIndex < testSucceededIndex)
      }
      it("should, when the info appears in the body after a test, report the info after the test runs") {
        val msg = "hi there, dude"
        val testName = "test name"
        class MySuite extends FunSuite {
          test(testName) {}
          info(msg)
        }
        val (infoProvidedIndex, testStartingIndex, testSucceededIndex) =
          getIndexesForInformerEventOrderTests(new MySuite, testName, msg)
        assert(testStartingIndex < testSucceededIndex)
        assert(testSucceededIndex < infoProvidedIndex)
      }
      it("should print to stdout when info is called by a method invoked after the suite has been executed") {
        class MySuite extends FunSuite {
          callInfo() // This should work fine
          def callInfo() {
            info("howdy")
          }
          test("howdy also") {
            callInfo() // This should work fine
          }
        }
        val suite = new MySuite
        val myRep = new EventRecordingReporter
        suite.run(None, Args(myRep))
        suite.callInfo() // TODO: Actually test that it prints to stdout
      }
    }
    it("should run tests registered via the testsFor syntax") {
      trait SharedFunSuiteTests { this: FunSuite =>
        def nonEmptyStack(s: String)(i: Int) {
          test("I am shared") {}
        }
      }
      class MySuite extends FunSuite with SharedFunSuiteTests {
        testsFor(nonEmptyStack("hi")(1))
      }
      val suite = new MySuite
      val reporter = new EventRecordingReporter
      suite.run(None, Args(reporter))

      val indexedList = reporter.eventsReceived

      val testStartingOption = indexedList.find(_.isInstanceOf[TestStarting])
      assert(testStartingOption.isDefined)
      assert(testStartingOption.get.asInstanceOf[TestStarting].testName === "I am shared")
    }
    it("should throw NullPointerException if a null test tag is provided") {
      // test
      intercept[NullPointerException] {
        new FunSuite {
          test("hi", null) {}
        }
      }
      val caught = intercept[NullPointerException] {
        new FunSuite {
          test("hi", mytags.SlowAsMolasses, null) {}
        }
      }
      assert(caught.getMessage === "a test tag was null")
      intercept[NullPointerException] {
        new FunSuite {
          test("hi", mytags.SlowAsMolasses, null, mytags.WeakAsAKitten) {}
        }
      }
      // ignore
      intercept[NullPointerException] {
        new FunSuite {
          ignore("hi", null) {}
        }
      }
      val caught2 = intercept[NullPointerException] {
        new FunSuite {
          ignore("hi", mytags.SlowAsMolasses, null) {}
        }
      }
      assert(caught2.getMessage === "a test tag was null")
      intercept[NullPointerException] {
        new FunSuite {
          ignore("hi", mytags.SlowAsMolasses, null, mytags.WeakAsAKitten) {}
        }
      }
    }

    class TestWasCalledSuite extends FunSuite {
      var theTestThisCalled = false
      var theTestThatCalled = false
      test("this") { theTestThisCalled = true }
      test("that") { theTestThatCalled = true }
    }

    it("should execute all tests when run is called with testName None") {

      val b = new TestWasCalledSuite
      b.run(None, Args(SilentReporter))
      assert(b.theTestThisCalled)
      assert(b.theTestThatCalled)
    }

    it("should execute one test when run is called with a defined testName") {

      val a = new TestWasCalledSuite
      a.run(Some("this"), Args(SilentReporter))
      assert(a.theTestThisCalled)
      assert(!a.theTestThatCalled)
    }

    it("should report as ignored, and not run, tests marked ignored") {

      val a = new FunSuite {
        var theTestThisCalled = false
        var theTestThatCalled = false
        test("test this") { theTestThisCalled = true }
        test("test that") { theTestThatCalled = true }
      }

      val repA = new TestIgnoredTrackingReporter
      a.run(None, Args(repA))
      assert(!repA.testIgnoredReceived)
      assert(a.theTestThisCalled)
      assert(a.theTestThatCalled)

      val b = new FunSuite {
        var theTestThisCalled = false
        var theTestThatCalled = false
        ignore("test this") { theTestThisCalled = true }
        test("test that") { theTestThatCalled = true }
      }

      val repB = new TestIgnoredTrackingReporter
      b.run(None, Args(repB))
      assert(repB.testIgnoredReceived)
      assert(repB.lastEvent.isDefined)
      assert(repB.lastEvent.get.testName endsWith "test this")
      assert(!b.theTestThisCalled)
      assert(b.theTestThatCalled)

      val c = new FunSuite {
        var theTestThisCalled = false
        var theTestThatCalled = false
        test("test this") { theTestThisCalled = true }
        ignore("test that") { theTestThatCalled = true }
      }

      val repC = new TestIgnoredTrackingReporter
      c.run(None, Args(repC))
      assert(repC.testIgnoredReceived)
      assert(repC.lastEvent.isDefined)
      assert(repC.lastEvent.get.testName endsWith "test that", repC.lastEvent.get.testName)
      assert(c.theTestThisCalled)
      assert(!c.theTestThatCalled)

      // The order I want is order of appearance in the file.
      // Will try and implement that tomorrow. Subtypes will be able to change the order.
      val d = new FunSuite {
        var theTestThisCalled = false
        var theTestThatCalled = false
        ignore("test this") { theTestThisCalled = true }
        ignore("test that") { theTestThatCalled = true }
      }

      val repD = new TestIgnoredTrackingReporter
      d.run(None, Args(repD))
      assert(repD.testIgnoredReceived)
      assert(repD.lastEvent.isDefined)
      assert(repD.lastEvent.get.testName endsWith "test that") // last because should be in order of appearance
      assert(!d.theTestThisCalled)
      assert(!d.theTestThatCalled)
    }

    it("should ignore a test marked as ignored if run is invoked with that testName") {
      val e = new FunSuite {
        var theTestThisCalled = false
        var theTestThatCalled = false
        ignore("test this") { theTestThisCalled = true }
        test("test that") { theTestThatCalled = true }
      }

      val repE = new TestIgnoredTrackingReporter
      e.run(Some("test this"), Args(repE))
      assert(repE.testIgnoredReceived)
      assert(!e.theTestThisCalled)
      assert(!e.theTestThatCalled)
    }

    it("should exclude a test with a tag included in the tagsToExclude set even if run is invoked with that testName") {
      val e = new FunSuite {
        var theTestThisCalled = false
        var theTestThatCalled = false
        test("test this", mytags.SlowAsMolasses) { theTestThisCalled = true }
        test("test that") { theTestThatCalled = true }
      }

      val repE = new TestIgnoredTrackingReporter
      e.run(Some("test this"), Args(repE, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repE.testIgnoredReceived)
      assert(!e.theTestThisCalled)
      assert(!e.theTestThatCalled)
    }

    it("should run only those tests selected by the tags to include and exclude sets") {

      // Nothing is excluded
      val a = new FunSuite {
        var theTestThisCalled = false
        var theTestThatCalled = false
        test("test this", mytags.SlowAsMolasses) { theTestThisCalled = true }
        test("test that") { theTestThatCalled = true }
      }
      val repA = new TestIgnoredTrackingReporter
      a.run(None, Args(repA))
      assert(!repA.testIgnoredReceived)
      assert(a.theTestThisCalled)
      assert(a.theTestThatCalled)

      // SlowAsMolasses is included, one test should be excluded
      val b = new FunSuite {
        var theTestThisCalled = false
        var theTestThatCalled = false
        test("test this", mytags.SlowAsMolasses) { theTestThisCalled = true }
        test("test that") { theTestThatCalled = true }
      }
      val repB = new TestIgnoredTrackingReporter
      b.run(None, Args(repB, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repB.testIgnoredReceived)
      assert(b.theTestThisCalled)
      assert(!b.theTestThatCalled)

      // SlowAsMolasses is included, and both tests should be included
      val c = new FunSuite {
        var theTestThisCalled = false
        var theTestThatCalled = false
        test("test this", mytags.SlowAsMolasses) { theTestThisCalled = true }
        test("test that", mytags.SlowAsMolasses) { theTestThatCalled = true }
      }
      val repC = new TestIgnoredTrackingReporter
      c.run(None, Args(repB, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repC.testIgnoredReceived)
      assert(c.theTestThisCalled)
      assert(c.theTestThatCalled)

      // SlowAsMolasses is included. both tests should be included but one ignored
      val d = new FunSuite {
        var theTestThisCalled = false
        var theTestThatCalled = false
        ignore("test this", mytags.SlowAsMolasses) { theTestThisCalled = true }
        test("test that", mytags.SlowAsMolasses) { theTestThatCalled = true }
      }
      val repD = new TestIgnoredTrackingReporter
      d.run(None, Args(repD, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.Ignore")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(repD.testIgnoredReceived)
      assert(!d.theTestThisCalled)
      assert(d.theTestThatCalled)

      // SlowAsMolasses included, FastAsLight excluded
      val e = new FunSuite {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        test("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true }
        test("test that", mytags.SlowAsMolasses) { theTestThatCalled = true }
        test("test the other") { theTestTheOtherCalled = true }
      }
      val repE = new TestIgnoredTrackingReporter
      e.run(None, Args(repE, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
                ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repE.testIgnoredReceived)
      assert(!e.theTestThisCalled)
      assert(e.theTestThatCalled)
      assert(!e.theTestTheOtherCalled)

      // An Ignored test that was both included and excluded should not generate a TestIgnored event
      val f = new FunSuite {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        ignore("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true }
        test("test that", mytags.SlowAsMolasses) { theTestThatCalled = true }
        test("test the other") { theTestTheOtherCalled = true }
      }
      val repF = new TestIgnoredTrackingReporter
      f.run(None, Args(repF, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
                ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repF.testIgnoredReceived)
      assert(!f.theTestThisCalled)
      assert(f.theTestThatCalled)
      assert(!f.theTestTheOtherCalled)

      // An Ignored test that was not included should not generate a TestIgnored event
      val g = new FunSuite {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        test("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true }
        test("test that", mytags.SlowAsMolasses) { theTestThatCalled = true }
        ignore("test the other") { theTestTheOtherCalled = true }
      }
      val repG = new TestIgnoredTrackingReporter
      g.run(None, Args(repG, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
                ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repG.testIgnoredReceived)
      assert(!g.theTestThisCalled)
      assert(g.theTestThatCalled)
      assert(!g.theTestTheOtherCalled)

      // No tagsToInclude set, FastAsLight excluded
      val h = new FunSuite {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        test("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true }
        test("test that", mytags.SlowAsMolasses) { theTestThatCalled = true }
        test("test the other") { theTestTheOtherCalled = true }
      }
      val repH = new TestIgnoredTrackingReporter
      h.run(None, Args(repH, Stopper.default, Filter(None, Set("org.scalatest.FastAsLight")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repH.testIgnoredReceived)
      assert(!h.theTestThisCalled)
      assert(h.theTestThatCalled)
      assert(h.theTestTheOtherCalled)

      // No tagsToInclude set, SlowAsMolasses excluded
      val i = new FunSuite {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        test("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true }
        test("test that", mytags.SlowAsMolasses) { theTestThatCalled = true }
        test("test the other") { theTestTheOtherCalled = true }
      }
      val repI = new TestIgnoredTrackingReporter
      i.run(None, Args(repI, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repI.testIgnoredReceived)
      assert(!i.theTestThisCalled)
      assert(!i.theTestThatCalled)
      assert(i.theTestTheOtherCalled)

      // No tagsToInclude set, SlowAsMolasses excluded, TestIgnored should not be received on excluded ones
      val j = new FunSuite {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        ignore("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true }
        ignore("test that", mytags.SlowAsMolasses) { theTestThatCalled = true }
        test("test the other") { theTestTheOtherCalled = true }
      }
      val repJ = new TestIgnoredTrackingReporter
      j.run(None, Args(repJ, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repI.testIgnoredReceived)
      assert(!j.theTestThisCalled)
      assert(!j.theTestThatCalled)
      assert(j.theTestTheOtherCalled)

      // Same as previous, except Ignore specifically mentioned in excludes set
      val k = new FunSuite {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        ignore("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true }
        ignore("test that", mytags.SlowAsMolasses) { theTestThatCalled = true }
        ignore("test the other") { theTestTheOtherCalled = true }
      }
      val repK = new TestIgnoredTrackingReporter
      k.run(None, Args(repK, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses", "org.scalatest.Ignore")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(repK.testIgnoredReceived)
      assert(!k.theTestThisCalled)
      assert(!k.theTestThatCalled)
      assert(!k.theTestTheOtherCalled)
    }
    
    it("should return the correct test count from its expectedTestCount method") {

      val a = new FunSuite {
        test("test this") {}
        test("test that") {}
      }
      assert(a.expectedTestCount(Filter()) === 2)

      val b = new FunSuite {
        ignore("test this") {}
        test("test that") {}
      }
      assert(b.expectedTestCount(Filter()) === 1)

      val c = new FunSuite {
        test("test this", mytags.FastAsLight) {}
        test("test that") {}
      }
      assert(c.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 1)
      assert(c.expectedTestCount(Filter(None, Set("org.scalatest.FastAsLight"))) === 1)

      val d = new FunSuite {
        test("test this", mytags.FastAsLight, mytags.SlowAsMolasses) {}
        test("test that", mytags.SlowAsMolasses) {}
        test("test the other thing") {}
      }
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 1)
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight"))) === 1)
      assert(d.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) === 1)
      assert(d.expectedTestCount(Filter()) === 3)

      val e = new FunSuite {
        test("test this", mytags.FastAsLight, mytags.SlowAsMolasses) {}
        test("test that", mytags.SlowAsMolasses) {}
        ignore("test the other thing") {}
      }
      assert(e.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 1)
      assert(e.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight"))) === 1)
      assert(e.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) === 0)
      assert(e.expectedTestCount(Filter()) === 2)

      val f = new Suites(a, b, c, d, e)
      assert(f.expectedTestCount(Filter()) === 10)
    }
    it("should generate a TestPending message when the test body is (pending)") {
      val a = new FunSuite {

        test("should do this") (pending)

        test("should do that") {
          assert(2 + 2 === 4)
        }

        test("should do something else") {
          assert(2 + 2 === 4)
          pending
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      val tp = rep.testPendingEventsReceived
      assert(tp.size === 2)
    }
    it("should generate a test failure if a Throwable, or an Error other than direct Error subtypes " +
            "known in JDK 1.5, excluding AssertionError") {
      val a = new FunSuite {
        test("throws AssertionError") { throw new AssertionError }
        test("throws plain old Error") { throw new Error }
        test("throws Throwable") { throw new Throwable }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      val tf = rep.testFailedEventsReceived
      assert(tf.size === 3)
    }
    it("should propagate out Errors that are direct subtypes of Error in JDK 1.5, other than " +
            "AssertionError, causing Suites and Runs to abort.") {
      val a = new FunSuite {
        test("throws AssertionError") { throw new OutOfMemoryError }
      }
      intercept[OutOfMemoryError] {
        a.run(None, Args(SilentReporter))
      }
    }
    describe("(when a nesting rule has been violated)") {

      it("should, if they call a nested it from within an it clause, result in a TestFailedException when running the test") {

        class MySuite extends FunSuite {
          test("should blow up") {
            test("should never run") {
              assert(1 === 1)
            }
          }
        }

        val spec = new MySuite
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a nested it with tags from within an it clause, result in a TestFailedException when running the test") {

        class MySuite extends FunSuite {
          test("should blow up") {
            test("should never run", mytags.SlowAsMolasses) {
              assert(1 === 1)
            }
          }
        }

        val spec = new MySuite
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a nested ignore from within an it clause, result in a TestFailedException when running the test") {

        class MySuite extends FunSuite {
          test("should blow up") {
            ignore("should never run") {
              assert(1 === 1)
            }
          }
        }

        val spec = new MySuite
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a nested ignore with tags from within an it clause, result in a TestFailedException when running the test") {

        class MySuite extends FunSuite {
          test("should blow up") {
            ignore("should never run", mytags.SlowAsMolasses) {
              assert(1 === 1)
            }
          }
        }

        val spec = new MySuite
        ensureTestFailedEventReceived(spec, "should blow up")
      }
    }

    it("should throw IllegalArgumentException if passed a testName that doesn't exist") {
      class MySuite extends FunSuite {
        test("one") {}
        test("two") {}
      }
      val suite = new MySuite
      intercept[IllegalArgumentException] {
        suite.run(Some("three"), Args(SilentReporter))
      }
    }

    it("should throw a NotAllowedException if chosenStyles is defined and does not include FunSuite") {

      class SimpleSuite extends FunSuite {
        test("one") {}
        test("two") {}
        test("three") {}
      }

      val simpleSuite = new SimpleSuite()
      simpleSuite.run(None, Args(SilentReporter))
      simpleSuite.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap(CHOSEN_STYLES -> Set("org.scalatest.FunSuite")), None, new Tracker, Set.empty))
      val caught =
        intercept[NotAllowedException] {
          simpleSuite.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap(CHOSEN_STYLES -> Set("org.scalatest.FunSpec")), None, new Tracker, Set.empty))
        }
      import OptionValues._
      assert(caught.message.value === Resources("notTheChosenStyle", "org.scalatest.FunSuite", "org.scalatest.FunSpec"))
      val caught2 =
        intercept[NotAllowedException] {
          simpleSuite.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap(CHOSEN_STYLES -> Set("org.scalatest.FunSpec", "org.scalatest.FreeSpec")), None, new Tracker, Set.empty))
        }
      assert(caught2.message.value === Resources("notOneOfTheChosenStyles", "org.scalatest.FunSuite", Suite.makeListForHumans(Vector("org.scalatest.FunSpec", "org.scalatest.FreeSpec"))))
      val caught3 =
        intercept[NotAllowedException] {
          simpleSuite.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap(CHOSEN_STYLES -> Set("org.scalatest.FunSpec", "org.scalatest.FreeSpec", "org.scalatest.FlatSpec")), None, new Tracker, Set.empty))
        }
      assert(caught3.message.value === Resources("notOneOfTheChosenStyles", "org.scalatest.FunSuite", Suite.makeListForHumans(Vector("org.scalatest.FunSpec", "org.scalatest.FreeSpec", "org.scalatest.FlatSpec"))))
    }
  }
  
  describe("when failure happens") {
    it("should fire TestFailed event with correct stack depth info when test failed") {
      class TestSpec extends FunSuite {
        test("fail scenario") {
          assert(1 === 2)
        }
      }
      val rep = new EventRecordingReporter
      val s1 = new TestSpec
      s1.run(None, Args(rep))
      assert(rep.testFailedEventsReceived.size === 1)
      assert(rep.testFailedEventsReceived(0).throwable.get.asInstanceOf[TestFailedException].failedCodeFileName.get === "FunSuiteSpec.scala")
      assert(rep.testFailedEventsReceived(0).throwable.get.asInstanceOf[TestFailedException].failedCodeLineNumber.get === thisLineNumber - 8)
    }
    
    it("should generate TestRegistrationClosedException with correct stack depth info when has a test nested inside a test") {
      class TestSpec extends FunSuite {
        var registrationClosedThrown = false
        test("a scenario") {
          test("nested scenario") {
            assert(1 === 2)
          }
        }
        override def withFixture(test: NoArgTest): Outcome = {
          val outcome = test.apply()
          outcome match {
            case Exceptional(ex: TestRegistrationClosedException) => 
              registrationClosedThrown = true
            case _ =>
          }
          outcome
        }
      }
      val rep = new EventRecordingReporter
      val s = new TestSpec
      s.run(None, Args(rep))
      assert(s.registrationClosedThrown == true)
      val testFailedEvents = rep.testFailedEventsReceived
      assert(testFailedEvents.size === 1)
      assert(testFailedEvents(0).throwable.get.getClass() === classOf[TestRegistrationClosedException])
      val trce = testFailedEvents(0).throwable.get.asInstanceOf[TestRegistrationClosedException]
      assert("FunSuiteSpec.scala" === trce.failedCodeFileName.get)
      assert(trce.failedCodeLineNumber.get === thisLineNumber - 23)
    }
  }
}


