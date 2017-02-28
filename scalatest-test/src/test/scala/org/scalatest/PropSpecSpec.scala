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
import org.scalactic.exceptions.NullArgumentException
import org.scalatest.exceptions.DuplicateTestNameException
import org.scalatest.exceptions.TestFailedException
import org.scalatest.exceptions.TestRegistrationClosedException

class PropSpecSpec extends FunSpec {

  describe("A PropSpec") {

    it("should return the test names in registration order from testNames") {
      
      val a = new PropSpec {
        property("test this") {/* ASSERTION_SUCCEED */}
        property("test that") {/* ASSERTION_SUCCEED */}
      }

      assertResult(List("test this", "test that")) {
        a.testNames.iterator.toList
      }

      val b = new PropSpec {}

      assertResult(List[String]()) {
        b.testNames.iterator.toList
      }

      val c = new PropSpec {
        property("test that") {/* ASSERTION_SUCCEED */}
        property("test this") {/* ASSERTION_SUCCEED */}
      }

      assertResult(List("test that", "test this")) {
        c.testNames.iterator.toList
      }
    }

    it("should throw NotAllowedException if a duplicate test name registration is attempted") {

      intercept[DuplicateTestNameException] {
        new PropSpec {
          property("test this") {/* ASSERTION_SUCCEED */}
          property("test this") {/* ASSERTION_SUCCEED */}
        }
      }
      intercept[DuplicateTestNameException] {
        new PropSpec {
          property("test this") {/* ASSERTION_SUCCEED */}
          ignore("test this") {/* ASSERTION_SUCCEED */}
        }
      }
      intercept[DuplicateTestNameException] {
        new PropSpec {
          ignore("test this") {/* ASSERTION_SUCCEED */}
          ignore("test this") {/* ASSERTION_SUCCEED */}
        }
      }
      intercept[DuplicateTestNameException] {
        new PropSpec {
          ignore("test this") {/* ASSERTION_SUCCEED */}
          property("test this") {/* ASSERTION_SUCCEED */}
        }
      }
    }

    it("should throw NotAllowedException if test registration is attempted after run has been invoked on a suite") {
      class InvokedWhenNotRunningSuite extends PropSpec {
        var fromMethodTestExecuted = false
        var fromConstructorTestExecuted = false
        property("from constructor") {
          fromConstructorTestExecuted = true
          /* ASSERTION_SUCCEED */
        }
        def tryToRegisterATest(): Unit = {
          property("from method") {
            fromMethodTestExecuted = true
            /* ASSERTION_SUCCEED */
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
      class InvokedWhenRunningSuite extends PropSpec {
        var fromMethodTestExecuted = false
        var fromConstructorTestExecuted = false
        property("from constructor") {
          tryToRegisterATest()
          fromConstructorTestExecuted = true
        }
        def tryToRegisterATest() {
          property("from method") {
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
      val a = new PropSpec {
        var withFixtureWasInvoked = false
        var testWasInvoked = false
        override def withFixture(test: NoArgTest): Outcome = {
          withFixtureWasInvoked = true
          super.withFixture(test)
        }
        property("something") {
          testWasInvoked = true
          /* ASSERTION_SUCCEED */
        }
      }

      import scala.language.reflectiveCalls

      a.run(None, Args(SilentReporter))
      assert(a.withFixtureWasInvoked)
      assert(a.testWasInvoked)
    }
    it("should pass the correct test name in the NoArgTest passed to withFixture") {
      val a = new PropSpec {
        var correctTestNameWasPassed = false
        override def withFixture(test: NoArgTest): Outcome = {
          correctTestNameWasPassed = test.name == "something"
          super.withFixture(test)
        }
        property("something") {/* ASSERTION_SUCCEED */}
      }

      import scala.language.reflectiveCalls

      a.run(None, Args(SilentReporter))
      assert(a.correctTestNameWasPassed)
    }
    it("should pass the correct config map in the NoArgTest passed to withFixture") {
      val a = new PropSpec {
        var correctConfigMapWasPassed = false
        override def withFixture(test: NoArgTest): Outcome = {
          correctConfigMapWasPassed = (test.configMap == ConfigMap("hi" -> 7))
          super.withFixture(test)
        }
        property("something") {/* ASSERTION_SUCCEED */}
      }

      import scala.language.reflectiveCalls

      a.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap("hi" -> 7), None, new Tracker(), Set.empty))
      assert(a.correctConfigMapWasPassed)
    }

    describe("(with info calls)") {
      it("should, when the info appears in the body before a test, report the info before the test") {
        val msg = "hi there, dude"
        val testName = "test name"
        class MySuite extends PropSpec {
          info(msg)
          property(testName) {/* ASSERTION_SUCCEED */}
        }
        val (infoProvidedIndex, testStartingIndex, testSucceededIndex) =
          getIndexesForInformerEventOrderTests(new MySuite, testName, msg)
        assert(infoProvidedIndex < testStartingIndex)
        assert(testStartingIndex < testSucceededIndex)
      }
      it("should, when the info appears in the body after a test, report the info after the test runs") {
        val msg = "hi there, dude"
        val testName = "test name"
        class MySuite extends PropSpec {
          property(testName) {/* ASSERTION_SUCCEED */}
          info(msg)
        }
        val (infoProvidedIndex, testStartingIndex, testSucceededIndex) =
          getIndexesForInformerEventOrderTests(new MySuite, testName, msg)
        assert(testStartingIndex < testSucceededIndex)
        assert(testSucceededIndex < infoProvidedIndex)
      }
      it("should print to stdout when info is called by a method invoked after the suite has been executed") {
        class MySuite extends PropSpec {
          callInfo() // This should work fine
          def callInfo(): Unit = {
            info("howdy")
          }
          property("howdy also") {
            callInfo() // This should work fine
            /* ASSERTION_SUCCEED */
          }
        }
        val suite = new MySuite
        val myRep = new EventRecordingReporter
        suite.run(None, Args(myRep))
        suite.callInfo() // TODO: Actually test that it prints to stdout
      }
    }
    it("should run tests registered via the propertiesFor syntax") {
      trait SharedPropSpecTests { this: PropSpec =>
        def nonEmptyStack(s: String)(i: Int): Unit = {
          property("I am shared") {/* ASSERTION_SUCCEED */}
        }
      }
      class MySuite extends PropSpec with SharedPropSpecTests {
        propertiesFor(nonEmptyStack("hi")(1))
      }
      val suite = new MySuite
      val reporter = new EventRecordingReporter
      suite.run(None, Args(reporter))

      val indexedList = reporter.eventsReceived

      val testStartingOption = indexedList.find(_.isInstanceOf[TestStarting])
      assert(testStartingOption.isDefined)
      assert(testStartingOption.get.asInstanceOf[TestStarting].testName === "I am shared")
    }
    it("should throw NullArgumentException if a null test tag is provided") {
      // test
      intercept[NullArgumentException] {
        new PropSpec {
          property("hi", null) {/* ASSERTION_SUCCEED */}
        }
      }
      val caught = intercept[NullArgumentException] {
        new PropSpec {
          property("hi", mytags.SlowAsMolasses, null) {/* ASSERTION_SUCCEED */}
        }
      }
      assert(caught.getMessage == "a test tag was null")
      intercept[NullArgumentException] {
        new PropSpec {
          property("hi", mytags.SlowAsMolasses, null, mytags.WeakAsAKitten) {/* ASSERTION_SUCCEED */}
        }
      }

      // ignore
      intercept[NullArgumentException] {
        new PropSpec {
          ignore("hi", null) {/* ASSERTION_SUCCEED */}
        }
      }
      val caught2 = intercept[NullArgumentException] {
        new PropSpec {
          ignore("hi", mytags.SlowAsMolasses, null) {/* ASSERTION_SUCCEED */}
        }
      }
      assert(caught2.getMessage == "a test tag was null")
      intercept[NullArgumentException] {
        new PropSpec {
          ignore("hi", mytags.SlowAsMolasses, null, mytags.WeakAsAKitten) {/* ASSERTION_SUCCEED */}
        }
      }

      // registerTest
      intercept[NullArgumentException] {
        new PropSpec {
          registerTest("hi", null) {/* ASSERTION_SUCCEED */}
        }
      }
      val caught3 = intercept[NullArgumentException] {
        new PropSpec {
          registerTest("hi", mytags.SlowAsMolasses, null) {/* ASSERTION_SUCCEED */}
        }
      }
      assert(caught3.getMessage == "a test tag was null")
      intercept[NullArgumentException] {
        new PropSpec {
          property("hi", mytags.SlowAsMolasses, null, mytags.WeakAsAKitten) {/* ASSERTION_SUCCEED */}
        }
      }

      // registerIgnoredTest
      intercept[NullArgumentException] {
        new PropSpec {
          registerIgnoredTest("hi", null) {/* ASSERTION_SUCCEED */}
        }
      }
      val caught4 = intercept[NullArgumentException] {
        new PropSpec {
          registerIgnoredTest("hi", mytags.SlowAsMolasses, null) {/* ASSERTION_SUCCEED */}
        }
      }
      assert(caught4.getMessage == "a test tag was null")
      intercept[NullArgumentException] {
        new PropSpec {
          registerIgnoredTest("hi", mytags.SlowAsMolasses, null, mytags.WeakAsAKitten) {/* ASSERTION_SUCCEED */}
        }
      }
    }

    class TestWasCalledSuite extends PropSpec {
      var theTestThisCalled = false
      var theTestThatCalled = false
      property("this") { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
      property("that") { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
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

      val a = new PropSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        property("test this") { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        property("test that") { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }

      import scala.language.reflectiveCalls

      val repA = new TestIgnoredTrackingReporter
      a.run(None, Args(repA))
      assert(!repA.testIgnoredReceived)
      assert(a.theTestThisCalled)
      assert(a.theTestThatCalled)

      val b = new PropSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        ignore("test this") { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        property("test that") { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }

      val repB = new TestIgnoredTrackingReporter
      b.run(None, Args(repB))
      assert(repB.testIgnoredReceived)
      assert(repB.lastEvent.isDefined)
      assert(repB.lastEvent.get.testName endsWith "test this")
      assert(!b.theTestThisCalled)
      assert(b.theTestThatCalled)

      val c = new PropSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        property("test this") { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        ignore("test that") { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
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
      val d = new PropSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        ignore("test this") { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        ignore("test that") { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
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
      // If I provide a specific testName to run, then it should ignore an Ignore on that test
      // method and actually invoke it.
      val e = new PropSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        ignore("test this") { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        property("test that") { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }

      import scala.language.reflectiveCalls

      val repE = new TestIgnoredTrackingReporter
      e.run(Some("test this"), Args(repE))
      assert(repE.testIgnoredReceived)
      assert(!e.theTestThisCalled)
      assert(!e.theTestThatCalled)
    }

    it("should run only those tests selected by the tags to include and exclude sets") {

      // Nothing is excluded
      val a = new PropSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        property("test this", mytags.SlowAsMolasses) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        property("test that") { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }

      import scala.language.reflectiveCalls

      val repA = new TestIgnoredTrackingReporter
      a.run(None, Args(repA))
      assert(!repA.testIgnoredReceived)
      assert(a.theTestThisCalled)
      assert(a.theTestThatCalled)

      // SlowAsMolasses is included, one test should be excluded
      val b = new PropSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        property("test this", mytags.SlowAsMolasses) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        property("test that") { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repB = new TestIgnoredTrackingReporter
      b.run(None, Args(repB, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repB.testIgnoredReceived)
      assert(b.theTestThisCalled)
      assert(!b.theTestThatCalled)

      // SlowAsMolasses is included, and both tests should be included
      val c = new PropSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        property("test this", mytags.SlowAsMolasses) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        property("test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repC = new TestIgnoredTrackingReporter
      c.run(None, Args(repB, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repC.testIgnoredReceived)
      assert(c.theTestThisCalled)
      assert(c.theTestThatCalled)

      // SlowAsMolasses is included. both tests should be included but one ignored
      val d = new PropSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        ignore("test this", mytags.SlowAsMolasses) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        property("test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repD = new TestIgnoredTrackingReporter
      d.run(None, Args(repD, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.Ignore")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(repD.testIgnoredReceived)
      assert(!d.theTestThisCalled)
      assert(d.theTestThatCalled)

      // SlowAsMolasses included, FastAsLight excluded
      val e = new PropSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        property("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        property("test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        property("test the other") { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repE = new TestIgnoredTrackingReporter
      e.run(None, Args(repE, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
                ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repE.testIgnoredReceived)
      assert(!e.theTestThisCalled)
      assert(e.theTestThatCalled)
      assert(!e.theTestTheOtherCalled)

      // An Ignored test that was both included and excluded should not generate a TestIgnored event
      val f = new PropSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        ignore("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        property("test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        property("test the other") { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repF = new TestIgnoredTrackingReporter
      f.run(None, Args(repF, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
                ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repF.testIgnoredReceived)
      assert(!f.theTestThisCalled)
      assert(f.theTestThatCalled)
      assert(!f.theTestTheOtherCalled)

      // An Ignored test that was not included should not generate a TestIgnored event
      val g = new PropSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        property("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        property("test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        ignore("test the other") { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repG = new TestIgnoredTrackingReporter
      g.run(None, Args(repG, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
                ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repG.testIgnoredReceived)
      assert(!g.theTestThisCalled)
      assert(g.theTestThatCalled)
      assert(!g.theTestTheOtherCalled)

      // No tagsToInclude set, FastAsLight excluded
      val h = new PropSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        property("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        property("test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        property("test the other") { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repH = new TestIgnoredTrackingReporter
      h.run(None, Args(repH, Stopper.default, Filter(None, Set("org.scalatest.FastAsLight")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repH.testIgnoredReceived)
      assert(!h.theTestThisCalled)
      assert(h.theTestThatCalled)
      assert(h.theTestTheOtherCalled)

      // No tagsToInclude set, SlowAsMolasses excluded
      val i = new PropSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        property("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        property("test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        property("test the other") { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repI = new TestIgnoredTrackingReporter
      i.run(None, Args(repI, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repI.testIgnoredReceived)
      assert(!i.theTestThisCalled)
      assert(!i.theTestThatCalled)
      assert(i.theTestTheOtherCalled)

      // No tagsToInclude set, SlowAsMolasses excluded, TestIgnored should not be received on excluded ones
      val j = new PropSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        ignore("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        ignore("test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        property("test the other") { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repJ = new TestIgnoredTrackingReporter
      j.run(None, Args(repJ, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repI.testIgnoredReceived)
      assert(!j.theTestThisCalled)
      assert(!j.theTestThatCalled)
      assert(j.theTestTheOtherCalled)

      // Same as previous, except Ignore specifically mentioned in excludes set
      val k = new PropSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        ignore("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        ignore("test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        ignore("test the other") { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repK = new TestIgnoredTrackingReporter
      k.run(None, Args(repK, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses", "org.scalatest.Ignore")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(repK.testIgnoredReceived)
      assert(!k.theTestThisCalled)
      assert(!k.theTestThatCalled)
      assert(!k.theTestTheOtherCalled)
    }

    it("should run only those registered tests selected by the tags to include and exclude sets") {

      // Nothing is excluded
      val a = new PropSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        registerTest("test this", mytags.SlowAsMolasses) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test that") { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }

      import scala.language.reflectiveCalls

      val repA = new TestIgnoredTrackingReporter
      a.run(None, Args(repA))
      assert(!repA.testIgnoredReceived)
      assert(a.theTestThisCalled)
      assert(a.theTestThatCalled)

      // SlowAsMolasses is included, one test should be excluded
      val b = new PropSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        registerTest("test this", mytags.SlowAsMolasses) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test that") { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repB = new TestIgnoredTrackingReporter
      b.run(None, Args(repB, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repB.testIgnoredReceived)
      assert(b.theTestThisCalled)
      assert(!b.theTestThatCalled)

      // SlowAsMolasses is included, and both tests should be included
      val c = new PropSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        registerTest("test this", mytags.SlowAsMolasses) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repC = new TestIgnoredTrackingReporter
      c.run(None, Args(repB, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repC.testIgnoredReceived)
      assert(c.theTestThisCalled)
      assert(c.theTestThatCalled)

      // SlowAsMolasses is included. both tests should be included but one ignored
      val d = new PropSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        registerIgnoredTest("test this", mytags.SlowAsMolasses) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repD = new TestIgnoredTrackingReporter
      d.run(None, Args(repD, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.Ignore")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(repD.testIgnoredReceived)
      assert(!d.theTestThisCalled)
      assert(d.theTestThatCalled)

      // SlowAsMolasses included, FastAsLight excluded
      val e = new PropSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        registerTest("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test the other") { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repE = new TestIgnoredTrackingReporter
      e.run(None, Args(repE, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
        ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repE.testIgnoredReceived)
      assert(!e.theTestThisCalled)
      assert(e.theTestThatCalled)
      assert(!e.theTestTheOtherCalled)

      // An Ignored test that was both included and excluded should not generate a TestIgnored event
      val f = new PropSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        registerIgnoredTest("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test the other") { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repF = new TestIgnoredTrackingReporter
      f.run(None, Args(repF, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
        ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repF.testIgnoredReceived)
      assert(!f.theTestThisCalled)
      assert(f.theTestThatCalled)
      assert(!f.theTestTheOtherCalled)

      // An Ignored test that was not included should not generate a TestIgnored event
      val g = new PropSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        registerTest("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        registerIgnoredTest("test the other") { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repG = new TestIgnoredTrackingReporter
      g.run(None, Args(repG, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
        ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repG.testIgnoredReceived)
      assert(!g.theTestThisCalled)
      assert(g.theTestThatCalled)
      assert(!g.theTestTheOtherCalled)

      // No tagsToInclude set, FastAsLight excluded
      val h = new PropSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        registerTest("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test the other") { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repH = new TestIgnoredTrackingReporter
      h.run(None, Args(repH, Stopper.default, Filter(None, Set("org.scalatest.FastAsLight")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repH.testIgnoredReceived)
      assert(!h.theTestThisCalled)
      assert(h.theTestThatCalled)
      assert(h.theTestTheOtherCalled)

      // No tagsToInclude set, SlowAsMolasses excluded
      val i = new PropSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        registerTest("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test the other") { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repI = new TestIgnoredTrackingReporter
      i.run(None, Args(repI, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repI.testIgnoredReceived)
      assert(!i.theTestThisCalled)
      assert(!i.theTestThatCalled)
      assert(i.theTestTheOtherCalled)

      // No tagsToInclude set, SlowAsMolasses excluded, TestIgnored should not be received on excluded ones
      val j = new PropSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        registerIgnoredTest("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerIgnoredTest("test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test the other") { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repJ = new TestIgnoredTrackingReporter
      j.run(None, Args(repJ, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repI.testIgnoredReceived)
      assert(!j.theTestThisCalled)
      assert(!j.theTestThatCalled)
      assert(j.theTestTheOtherCalled)

      // Same as previous, except Ignore specifically mentioned in excludes set
      val k = new PropSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        registerIgnoredTest("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerIgnoredTest("test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        registerIgnoredTest("test the other") { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repK = new TestIgnoredTrackingReporter
      k.run(None, Args(repK, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses", "org.scalatest.Ignore")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(repK.testIgnoredReceived)
      assert(!k.theTestThisCalled)
      assert(!k.theTestThatCalled)
      assert(!k.theTestTheOtherCalled)
    }
    
    it("should return the correct test count from its expectedTestCount method") {

      val a = new PropSpec {
        property("test this") {/* ASSERTION_SUCCEED */}
        property("test that") {/* ASSERTION_SUCCEED */}
      }
      assert(a.expectedTestCount(Filter()) == 2)

      val b = new PropSpec {
        ignore("test this") {/* ASSERTION_SUCCEED */}
        property("test that") {/* ASSERTION_SUCCEED */}
      }
      assert(b.expectedTestCount(Filter()) == 1)

      val c = new PropSpec {
        property("test this", mytags.FastAsLight) {/* ASSERTION_SUCCEED */}
        property("test that") {/* ASSERTION_SUCCEED */}
      }
      assert(c.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) == 1)
      assert(c.expectedTestCount(Filter(None, Set("org.scalatest.FastAsLight"))) == 1)

      val d = new PropSpec {
        property("test this", mytags.FastAsLight, mytags.SlowAsMolasses) {/* ASSERTION_SUCCEED */}
        property("test that", mytags.SlowAsMolasses) {/* ASSERTION_SUCCEED */}
        property("test the other thing") {/* ASSERTION_SUCCEED */}
      }
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) == 1)
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight"))) == 1)
      assert(d.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) == 1)
      assert(d.expectedTestCount(Filter()) === 3)

      val e = new PropSpec {
        property("test this", mytags.FastAsLight, mytags.SlowAsMolasses) {/* ASSERTION_SUCCEED */}
        property("test that", mytags.SlowAsMolasses) {/* ASSERTION_SUCCEED */}
        ignore("test the other thing") {/* ASSERTION_SUCCEED */}
      }
      assert(e.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) == 1)
      assert(e.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight"))) == 1)
      assert(e.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) == 0)
      assert(e.expectedTestCount(Filter()) == 2)

      val f = new Suites(a, b, c, d, e)
      assert(f.expectedTestCount(Filter()) === 10)
    }
    it("should return the correct test count from its expectedTestCount method when uses registerTest and registerIgnoredTest to register tests") {

      val a = new PropSpec {
        registerTest("test this") {/* ASSERTION_SUCCEED */}
        registerTest("test that") {/* ASSERTION_SUCCEED */}
      }
      assert(a.expectedTestCount(Filter()) == 2)

      val b = new PropSpec {
        registerIgnoredTest("test this") {/* ASSERTION_SUCCEED */}
        registerTest("test that") {/* ASSERTION_SUCCEED */}
      }
      assert(b.expectedTestCount(Filter()) == 1)

      val c = new PropSpec {
        registerTest("test this", mytags.FastAsLight) {/* ASSERTION_SUCCEED */}
        registerTest("test that") {/* ASSERTION_SUCCEED */}
      }
      assert(c.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) == 1)
      assert(c.expectedTestCount(Filter(None, Set("org.scalatest.FastAsLight"))) == 1)

      val d = new PropSpec {
        registerTest("test this", mytags.FastAsLight, mytags.SlowAsMolasses) {/* ASSERTION_SUCCEED */}
        registerTest("test that", mytags.SlowAsMolasses) {/* ASSERTION_SUCCEED */}
        registerTest("test the other thing") {/* ASSERTION_SUCCEED */}
      }
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) == 1)
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight"))) == 1)
      assert(d.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) == 1)
      assert(d.expectedTestCount(Filter()) == 3)

      val e = new PropSpec {
        registerTest("test this", mytags.FastAsLight, mytags.SlowAsMolasses) {/* ASSERTION_SUCCEED */}
        registerTest("test that", mytags.SlowAsMolasses) {/* ASSERTION_SUCCEED */}
        registerIgnoredTest("test the other thing") {/* ASSERTION_SUCCEED */}
      }
      assert(e.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) == 1)
      assert(e.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight"))) == 1)
      assert(e.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) == 0)
      assert(e.expectedTestCount(Filter()) == 2)

      val f = new Suites(a, b, c, d, e)
      assert(f.expectedTestCount(Filter()) == 10)
    }
    it("should generate a TestPending message when the test body is (pending)") {
      val a = new PropSpec {

        property("should do this") (pending)

        property("should do that") {
          assert(2 + 2 === 4)
        }

        property("should do something else") {
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
      val a = new PropSpec {
        property("throws AssertionError") { throw new AssertionError }
        property("throws plain old Error") { throw new Error }
        property("throws Throwable") { throw new Throwable }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      val tf = rep.testFailedEventsReceived
      assert(tf.size === 3)
    }
    // SKIP-SCALATESTJS-START
    it("should propagate out Errors that are direct subtypes of Error in JDK 1.5, other than " +
            "AssertionError, causing Suites and Runs to abort.") {
      val a = new PropSpec {
        property("throws AssertionError") { throw new OutOfMemoryError }
      }
      intercept[OutOfMemoryError] {
        a.run(None, Args(SilentReporter))
      }
    }
    // SKIP-SCALATESTJS-END
    describe("(when a nesting rule has been violated)") {

      it("should, if they call a nested it from within an it clause, result in a TestFailedException when running the test") {

        class MySuite extends PropSpec {
          property("should blow up") {
            property("should never run") {
              assert(1 === 1)
            }
            /* ASSERTION_SUCCEED */
          }
        }

        val spec = new MySuite
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a nested it with tags from within an it clause, result in a TestFailedException when running the test") {

        class MySuite extends PropSpec {
          property("should blow up") {
            property("should never run", mytags.SlowAsMolasses) {
              assert(1 == 1)
            }
            /* ASSERTION_SUCCEED */
          }
        }

        val spec = new MySuite
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a nested registerTest with tags from within a registerTest clause, result in a TestFailedException when running the test") {

        class MySuite extends PropSpec {
          registerTest("should blow up") {
            registerTest("should never run", mytags.SlowAsMolasses) {
              assert(1 == 1)
            }
            /* ASSERTION_SUCCEED */
          }
        }

        val spec = new MySuite
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a nested ignore from within an it clause, result in a TestFailedException when running the test") {

        class MySuite extends PropSpec {
          property("should blow up") {
            ignore("should never run") {
              assert(1 === 1)
            }
            /* ASSERTION_SUCCEED */
          }
        }

        val spec = new MySuite
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a nested ignore with tags from within an it clause, result in a TestFailedException when running the test") {

        class MySuite extends PropSpec {
          property("should blow up") {
            ignore("should never run", mytags.SlowAsMolasses) {
              assert(1 === 1)
            }
            /* ASSERTION_SUCCEED */
          }
        }

        val spec = new MySuite
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a nested registerIgnoredTest with tags from within a registerTest clause, result in a TestFailedException when running the test") {

        class MySuite extends PropSpec {
          registerTest("should blow up") {
            registerIgnoredTest("should never run", mytags.SlowAsMolasses) {
              assert(1 === 1)
            }
            /* ASSERTION_SUCCEED */
          }
        }

        val spec = new MySuite
        ensureTestFailedEventReceived(spec, "should blow up")
      }
    }

    it("should throw IllegalArgumentException if passed a testName that doesn't exist") {
      class MySuite extends PropSpec {
        property("one") {/* ASSERTION_SUCCEED */}
        property("two") {/* ASSERTION_SUCCEED */}
      }
      val suite = new MySuite
      intercept[IllegalArgumentException] {
        suite.run(Some("three"), Args(SilentReporter))
      }
    }

    it("should allow test registration with registerTest and registerIgnoredTest") {
      class TestSpec extends PropSpec {
        val a = 1
        registerTest("test 1") {
          val e = intercept[TestFailedException] {
            assert(a == 2)
          }
          assert(e.message == Some("1 did not equal 2"))
          assert(e.failedCodeFileName == Some("PropSpecSpec.scala"))
          assert(e.failedCodeLineNumber == Some(thisLineNumber - 4))
        }
        registerTest("test 2") {
          assert(a == 2)
        }
        registerTest("test 3") {
          pending
        }
        registerTest("test 4") {
          cancel
        }
        registerIgnoredTest("test 5") {
          assert(a == 2)
        }
      }

      val rep = new EventRecordingReporter
      val s = new TestSpec
      s.run(None, Args(rep))

      assert(rep.testStartingEventsReceived.length == 4)
      assert(rep.testSucceededEventsReceived.length == 1)
      assert(rep.testSucceededEventsReceived(0).testName == "test 1")
      assert(rep.testFailedEventsReceived.length == 1)
      assert(rep.testFailedEventsReceived(0).testName == "test 2")
      assert(rep.testPendingEventsReceived.length == 1)
      assert(rep.testPendingEventsReceived(0).testName == "test 3")
      assert(rep.testCanceledEventsReceived.length == 1)
      assert(rep.testCanceledEventsReceived(0).testName == "test 4")
      assert(rep.testIgnoredEventsReceived.length == 1)
      assert(rep.testIgnoredEventsReceived(0).testName == "test 5")
    }
    ignore("should support expectations") { // Unignore after we uncomment the expectation implicits in RegistrationPolicy
      class TestSpec extends PropSpec with Expectations {
        property("fail scenario") {
          expect(1 === 2); /* ASSERTION_SUCCEED */
        }
        property("nested fail scenario") {
          expect(1 === 2); /* ASSERTION_SUCCEED */
        }
      }
      val rep = new EventRecordingReporter
      val s1 = new TestSpec
      s1.run(None, Args(rep))
      assert(rep.testFailedEventsReceived.size === 2)
      assert(rep.testFailedEventsReceived(0).throwable.get.asInstanceOf[TestFailedException].failedCodeFileName.get === "PropSpecSpec.scala")
      assert(rep.testFailedEventsReceived(0).throwable.get.asInstanceOf[TestFailedException].failedCodeLineNumber.get === thisLineNumber - 11)
      assert(rep.testFailedEventsReceived(1).throwable.get.asInstanceOf[TestFailedException].failedCodeFileName.get === "PropSpecSpec.scala")
      assert(rep.testFailedEventsReceived(1).throwable.get.asInstanceOf[TestFailedException].failedCodeLineNumber.get === thisLineNumber - 10)
    }
  }
  
  describe("when failure happens") {
    
    it("should fire TestFailed event with correct stack depth info when test failed") {
      class TestSpec extends PropSpec {
        property("fail scenario") {
          assert(1 === 2)
        }
      }
      val rep = new EventRecordingReporter
      val s1 = new TestSpec
      s1.run(None, Args(rep))
      assert(rep.testFailedEventsReceived.size === 1)
      assert(rep.testFailedEventsReceived(0).throwable.get.asInstanceOf[TestFailedException].failedCodeFileName.get === "PropSpecSpec.scala")
      assert(rep.testFailedEventsReceived(0).throwable.get.asInstanceOf[TestFailedException].failedCodeLineNumber.get === thisLineNumber - 8)
    }
    
    it("should generate TestRegistrationClosedException with correct stack depth info when has a property nested inside a property") {
      class TestSpec extends PropSpec {
        var registrationClosedThrown = false
        property("a scenario") {
          property("nested scenario") {
            assert(1 == 2)
          }; /* ASSERTION_SUCCEED */
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
      assert("PropSpecSpec.scala" === trce.failedCodeFileName.get)
      assert(trce.failedCodeLineNumber.get === thisLineNumber - 23)
      assert(trce.message == Some("A property clause may not appear inside another property clause."))
    }

    it("should generate TestRegistrationClosedException with correct stack depth info when has an ignore nested inside a property") {
      class TestSpec extends PropSpec {
        var registrationClosedThrown = false
        property("a scenario") {
          ignore("nested scenario") {
            assert(1 == 2)
          }; /* ASSERTION_SUCCEED */
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
      assert("PropSpecSpec.scala" === trce.failedCodeFileName.get)
      assert(trce.failedCodeLineNumber.get === thisLineNumber - 23)
      assert(trce.message == Some("An ignore clause may not appear inside a property clause."))
    }

    it("should generate TestRegistrationClosedException with correct stack depth info when has a registerTest nested inside a registerTest") {
      class TestSpec extends PropSpec {
        var registrationClosedThrown = false
        registerTest("a scenario") {
          registerTest("nested scenario") {
            assert(1 == 2)
          }; /* ASSERTION_SUCCEED */
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
      assert("PropSpecSpec.scala" === trce.failedCodeFileName.get)
      assert(trce.failedCodeLineNumber.get === thisLineNumber - 23)
      assert(trce.message == Some("Test cannot be nested inside another test."))
    }

    it("should generate TestRegistrationClosedException with correct stack depth info when has a registerIgnoredTest nested inside a registerTest") {
      class TestSpec extends PropSpec {
        var registrationClosedThrown = false
        registerTest("a scenario") {
          registerIgnoredTest("nested scenario") {
            assert(1 == 2)
          }; /* ASSERTION_SUCCEED */
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
      assert("PropSpecSpec.scala" === trce.failedCodeFileName.get)
      assert(trce.failedCodeLineNumber.get === thisLineNumber - 23)
      assert(trce.message == Some("Test cannot be nested inside another test."))
    }

    it("should generate a DuplicateTestNameException when duplicate test name is detected") {
      class TestSpec extends PropSpec {
        property("test 1") { /* ASSERTION_SUCCEED */ }
        property("test 1") { /* ASSERTION_SUCCEED */ }
      }
      val e = intercept[DuplicateTestNameException] {
        new TestSpec
      }
      assert("PropSpecSpec.scala" == e.failedCodeFileName.get)
      assert(e.failedCodeLineNumber.get == thisLineNumber - 6)
      assert(!e.cause.isDefined)
    }

    it("should generate a DuplicateTestNameException when duplicate test name is detected using ignore") {
      class TestSpec extends PropSpec {
        property("test 1") { /* ASSERTION_SUCCEED */ }
        ignore("test 1") { /* ASSERTION_SUCCEED */ }
      }
      val e = intercept[DuplicateTestNameException] {
        new TestSpec
      }
      assert("PropSpecSpec.scala" == e.failedCodeFileName.get)
      assert(e.failedCodeLineNumber.get == thisLineNumber - 6)
      assert(!e.cause.isDefined)
    }
  }
}