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
package org.scalatest.funspec

import org.scalatest.SharedHelpers._
import org.scalatest.GivenWhenThen
import org.scalatest.Args
import org.scalatest.Outcome
import org.scalatest.Filter
import org.scalatest.Tracker
import org.scalatest.mytags
import org.scalatest.ConfigMap
import org.scalatest.Stopper
import org.scalatest.Exceptional
import org.scalatest.FailureMessages
import org.scalatest.UnquotedString
import org.scalatest.Suites
import org.scalatest.expectations
import org.scalatest.events._
import org.scalactic.Prettifier
import java.awt.AWTError
import java.lang.annotation.AnnotationFormatError
import java.nio.charset.CoderMalfunctionError
import javax.xml.parsers.FactoryConfigurationError
import javax.xml.transform.TransformerFactoryConfigurationError
import org.scalactic.exceptions.NullArgumentException
import org.scalatest.exceptions.DuplicateTestNameException
import org.scalatest.exceptions.NotAllowedException
import org.scalatest.exceptions.TestCanceledException
import org.scalatest.exceptions.TestFailedException
import org.scalatest.exceptions.TestRegistrationClosedException
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.funspec.AnyFunSpec

class AnyFunSpecSpec extends AnyFreeSpec with GivenWhenThen {

  private val prettifier = Prettifier.default

  println("########debug AnyFunSpecSpec: " + org.scalatest.tools.Runner.discoveredSuites + ", Runner: " + org.scalatest.tools.Runner.toString)

  "A FunSpec" - {

    "should return the test names in registration order from testNames" in {

      val a = new AnyFunSpec {
        it("should test this") {/* ASSERTION_SUCCEED */}
        it("should test that") {/* ASSERTION_SUCCEED */}
      }

      assertResult(List("should test this", "should test that")) {
        a.testNames.iterator.toList
      }

      val b = new AnyFunSpec {}

      assertResult(List[String]()) {
        b.testNames.iterator.toList
      }

      val c = new AnyFunSpec {
        it("should test that") {/* ASSERTION_SUCCEED */}
        it("should test this") {/* ASSERTION_SUCCEED */}
      }

      assertResult(List("should test that", "should test this")) {
        c.testNames.iterator.toList
      }

      val d = new AnyFunSpec {
        describe("A Tester") {
          it("should test that") {/* ASSERTION_SUCCEED */}
          it("should test this") {/* ASSERTION_SUCCEED */}
        }
      }

      assertResult(List("A Tester should test that", "A Tester should test this")) {
        d.testNames.iterator.toList
      }

      val e = new AnyFunSpec {
        describe("A Tester") {
          it("should test this") {/* ASSERTION_SUCCEED */}
          it("should test that") {/* ASSERTION_SUCCEED */}
        }
      }

      assertResult(List("A Tester should test this", "A Tester should test that")) {
        e.testNames.iterator.toList
      }
    }

    "should throw DuplicateTestNameException if a duplicate test name registration is attempted" in {
      
      intercept[DuplicateTestNameException] {
        new AnyFunSpec {
          it("should test this") {/* ASSERTION_SUCCEED */}
          it("should test this") {/* ASSERTION_SUCCEED */}
        }
      }
      intercept[DuplicateTestNameException] {
        new AnyFunSpec {
          it("should test this") {/* ASSERTION_SUCCEED */}
          ignore("should test this") {/* ASSERTION_SUCCEED */}
        }
      }
      intercept[DuplicateTestNameException] {
        new AnyFunSpec {
          ignore("should test this") {/* ASSERTION_SUCCEED */}
          ignore("should test this") {/* ASSERTION_SUCCEED */}
        }
      }
      intercept[DuplicateTestNameException] {
        new AnyFunSpec {
          ignore("should test this") {/* ASSERTION_SUCCEED */}
          it("should test this") {/* ASSERTION_SUCCEED */}
        }
      }
    }

    trait InvokeChecking {
      var withFixtureWasInvoked = false
      var testWasInvoked = false
    }

    "should invoke withFixture from runTest" in {
      val a = new AnyFunSpec with InvokeChecking {
        override def withFixture(test: NoArgTest): Outcome = {
          withFixtureWasInvoked = true
          super.withFixture(test)
        }
        it("should do something") {
          testWasInvoked = true
          /* ASSERTION_SUCCEED */
        }
      }

      import scala.language.reflectiveCalls

      a.run(None, Args(SilentReporter))
      assert(a.withFixtureWasInvoked)
      assert(a.testWasInvoked)
    }

    "should pass the correct test name in the NoArgTest passed to withFixture" in {
      trait TestNameChecking {
        var correctTestNameWasPassed = false
      }
      val a = new AnyFunSpec with TestNameChecking {
        override def withFixture(test: NoArgTest): Outcome = {
          correctTestNameWasPassed = test.name == "should do something"
          super.withFixture(test)
        }
        it("should do something") {/* ASSERTION_SUCCEED */}
      }

      import scala.language.reflectiveCalls

      a.run(None, Args(SilentReporter))
      assert(a.correctTestNameWasPassed)
    }
    "should pass the correct config map in the NoArgTest passed to withFixture" in {
      trait ConfigMapChecking {
        var correctConfigMapWasPassed = false
      }
      val a = new AnyFunSpec with ConfigMapChecking {
        override def withFixture(test: NoArgTest): Outcome = {
          correctConfigMapWasPassed = (test.configMap == ConfigMap("hi" -> 7))
          super.withFixture(test)
        }
        it("should do something") {/* ASSERTION_SUCCEED */}
      }

      import scala.language.reflectiveCalls

      a.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap("hi" -> 7), None, new Tracker()))
      assert(a.correctConfigMapWasPassed)
    }
    "(with info calls)" - {
      class InfoInsideTestSpec extends AnyFunSpec {
        val msg = "hi there, dude"
        val testName = "test name"
        it(testName) {
          info(msg)
          /* ASSERTION_SUCCEED */
        }
      }
      // In a Spec, any InfoProvided's fired during the test should be cached and sent out together with
      // test completed event. This makes the report look nicer, because the info is tucked under the "specifier'
      // text for that test.
      "should, when the info appears in the code of a successful test, report the info in the TestSucceeded" in {
        val spec = new InfoInsideTestSpec
        val (testStartingIndex, testSucceededIndex) =
          getIndexesForTestInformerEventOrderTests(spec, spec.testName, spec.msg)
        assert(testStartingIndex < testSucceededIndex)
      }
      class InfoBeforeTestSpec extends AnyFunSpec {
        val msg = "hi there, dude"
        val testName = "test name"
        info(msg)
        it(testName) {/* ASSERTION_SUCCEED */}
      }
      "should, when the info appears in the body before a test, report the info before the test" in {
        val spec = new InfoBeforeTestSpec
        val (infoProvidedIndex, testStartingIndex, testSucceededIndex) =
          getIndexesForInformerEventOrderTests(spec, spec.testName, spec.msg)
        assert(infoProvidedIndex < testStartingIndex)
        assert(testStartingIndex < testSucceededIndex)
      }
      "should, when the info appears in the body after a test, report the info after the test runs" in {
        val msg = "hi there, dude"
        val testName = "test name"
        class MySpec extends AnyFunSpec {
          it(testName) {/* ASSERTION_SUCCEED */}
          info(msg)
        }
        val (infoProvidedIndex, testStartingIndex, testSucceededIndex) =
          getIndexesForInformerEventOrderTests(new MySpec, testName, msg)
        assert(testStartingIndex < testSucceededIndex)
        assert(testSucceededIndex < infoProvidedIndex)
      }
      "should print to stdout when info is called by a method invoked after the suite has been executed" in {
        class MySpec extends AnyFunSpec {
          callInfo() // This should work fine
          def callInfo(): Unit = {
            info("howdy")
          }
          it("howdy also") {
            callInfo() // This should work fine
            /* ASSERTION_SUCCEED */
          }
        }
        val spec = new MySpec
        val myRep = new EventRecordingReporter
        spec.run(None, Args(myRep))
        spec.callInfo() // TODO: Actually test that This prints to stdout
      }
      "should send an InfoProvided with an IndentedText formatter with level 1 when called outside a test" in {
        val spec = new InfoBeforeTestSpec
        val indentedText = getIndentedTextFromInfoProvided(spec)
        assert(indentedText == IndentedText("+ " + spec.msg, spec.msg, 0))
      }
      "should send an InfoProvided with an IndentedText formatter with level 2 when called within a test" in {
        val spec = new InfoInsideTestSpec
        val indentedText = getIndentedTextFromTestInfoProvided(spec)
        assert(indentedText == IndentedText("  + " + spec.msg, spec.msg, 1))
      }
    }

    "(when a nesting rule has been violated)" - {

      "should, if they call a describe from within an it clause, result in a TestFailedException when running the test" in {

        class MySpec extends AnyFunSpec {
          it("should blow up") {
            describe("in the wrong place, at the wrong time") {
            }
            /* ASSERTION_SUCCEED */
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      "should, if they call a describe with a nested it from within an it clause, result in a TestFailedException when running the test" in {

        class MySpec extends AnyFunSpec {
          it("should blow up") {
            describe("in the wrong place, at the wrong time") {
              it("should never run") {
                assert(1 == 1)
              }
            }
            /* ASSERTION_SUCCEED */
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      "should, if they call a nested it from within an it clause, result in a TestFailedException when running the test" in {

        class MySpec extends AnyFunSpec {
          it("should blow up") {
            it("should never run") {
              assert(1 == 1)
            }
            /* ASSERTION_SUCCEED */
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      "should, if they call a nested it with tags from within an it clause, result in a TestFailedException when running the test" in {

        class MySpec extends AnyFunSpec {
          it("should blow up") {
            it("should never run", mytags.SlowAsMolasses) {
              assert(1 == 1)
            }
            /* ASSERTION_SUCCEED */
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      "should, if they call a nested registerTest with tags from within an registerTest clause, result in a TestFailedException when running the test" in {

        class MySpec extends AnyFunSpec {
          registerTest("should blow up") {
            registerTest("should never run", mytags.SlowAsMolasses) {
              assert(1 == 1)
            }
            /* ASSERTION_SUCCEED */
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      "should, if they call a describe with a nested ignore from within an it clause, result in a TestFailedException when running the test" in {

        class MySpec extends AnyFunSpec {
          it("should blow up") {
            describe("in the wrong place, at the wrong time") {
              ignore("should never run") {
                assert(1 == 1)
              }
            }
            /* ASSERTION_SUCCEED */
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      "should, if they call a nested ignore from within an it clause, result in a TestFailedException when running the test" in {

        class MySpec extends AnyFunSpec {
          it("should blow up") {
            ignore("should never run") {
              assert(1 == 1)
            }
            /* ASSERTION_SUCCEED */
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      "should, if they call a nested ignore with tags from within an it clause, result in a TestFailedException when running the test" in {

        class MySpec extends AnyFunSpec {
          it("should blow up") {
            ignore("should never run", mytags.SlowAsMolasses) {
              assert(1 == 1)
            }
            /* ASSERTION_SUCCEED */
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      "should, if they call a nested registerIgnoredTest with tags from within an registerTest clause, result in a TestFailedException when running the test" in {

        class MySpec extends AnyFunSpec {
          registerTest("should blow up") {
            registerIgnoredTest("should never run", mytags.SlowAsMolasses) {
              assert(1 == 1)
            }
            /* ASSERTION_SUCCEED */
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
    }
    "should run tests registered via the 'it should behave like' syntax" in {
      trait SharedSpecTests { this: AnyFunSpec =>
        def nonEmptyStack(s: String)(i: Int): Unit = {
          it("should be that I am shared") {/* ASSERTION_SUCCEED */}
        }
      }
      class MySpec extends AnyFunSpec with SharedSpecTests {
        it should behave like nonEmptyStack("hi")(1)
      }
      val suite = new MySpec
      val reporter = new EventRecordingReporter
      suite.run(None, Args(reporter))

      val indexedList = reporter.eventsReceived

      val testStartingOption = indexedList.find(_.isInstanceOf[TestStarting])
      assert(testStartingOption.isDefined)
      assert(testStartingOption.get.asInstanceOf[TestStarting].testName == "should be that I am shared")
    }

    "should throw NullArgumentException if a null test tag is provided" in {
      // it
      intercept[NullArgumentException] {
        new AnyFunSpec {
          it("hi", null) {/* ASSERTION_SUCCEED */}
        }
      }
      val caught = intercept[NullArgumentException] {
        new AnyFunSpec {
          it("hi", mytags.SlowAsMolasses, null) {/* ASSERTION_SUCCEED */}
        }
      }
      assert(caught.getMessage == "a test tag was null")
      intercept[NullArgumentException] {
        new AnyFunSpec {
          it("hi", mytags.SlowAsMolasses, null, mytags.WeakAsAKitten) {/* ASSERTION_SUCCEED */}
        }
      }

      // ignore
      intercept[NullArgumentException] {
        new AnyFunSpec {
          ignore("hi", null) {/* ASSERTION_SUCCEED */}
        }
      }
      val caught2 = intercept[NullArgumentException] {
        new AnyFunSpec {
          ignore("hi", mytags.SlowAsMolasses, null) {/* ASSERTION_SUCCEED */}
        }
      }
      assert(caught2.getMessage == "a test tag was null")
      intercept[NullArgumentException] {
        new AnyFunSpec {
          ignore("hi", mytags.SlowAsMolasses, null, mytags.WeakAsAKitten) {/* ASSERTION_SUCCEED */}
        }
      }

      // registerTest
      intercept[NullArgumentException] {
        new AnyFunSpec {
          registerTest("hi", null) {/* ASSERTION_SUCCEED */}
        }
      }
      val caught3 = intercept[NullArgumentException] {
        new AnyFunSpec {
          registerTest("hi", mytags.SlowAsMolasses, null) {/* ASSERTION_SUCCEED */}
        }
      }
      assert(caught3.getMessage == "a test tag was null")
      intercept[NullArgumentException] {
        new AnyFunSpec {
          registerTest("hi", mytags.SlowAsMolasses, null, mytags.WeakAsAKitten) {/* ASSERTION_SUCCEED */}
        }
      }

      // registerIgnoredTest
      intercept[NullArgumentException] {
        new AnyFunSpec {
          registerIgnoredTest("hi", null) {/* ASSERTION_SUCCEED */}
        }
      }
      val caught4 = intercept[NullArgumentException] {
        new AnyFunSpec {
          registerIgnoredTest("hi", mytags.SlowAsMolasses, null) {/* ASSERTION_SUCCEED */}
        }
      }
      assert(caught4.getMessage == "a test tag was null")
      intercept[NullArgumentException] {
        new AnyFunSpec {
          registerIgnoredTest("hi", mytags.SlowAsMolasses, null, mytags.WeakAsAKitten) {/* ASSERTION_SUCCEED */}
        }
      }
    }
    class TestWasCalledSuite extends AnyFunSpec {
      var theTestThisCalled = false
      var theTestThatCalled = false
      it("should run this") { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
      it("should run that, maybe") { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
    }

    "should execute all tests when run is called with testName None" in {

      val b = new TestWasCalledSuite
      b.run(None, Args(SilentReporter))
      assert(b.theTestThisCalled)
      assert(b.theTestThatCalled)
    }

    "should execute one test when run is called with a defined testName" in {

      val a = new TestWasCalledSuite
      a.run(Some("should run this"), Args(SilentReporter))
      assert(a.theTestThisCalled)
      assert(!a.theTestThatCalled)
    }

    trait CallChecking {
      var theTestThisCalled = false
      var theTestThatCalled = false
      var theTestTheOtherCalled = false
    }

    "should report as ignored, and not run, tests marked ignored" in {

      val a = new AnyFunSpec with CallChecking {
        it("test this") { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        it("test that") { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }

      import scala.language.reflectiveCalls

      val repA = new TestIgnoredTrackingReporter
      a.run(None, Args(repA))
      assert(!repA.testIgnoredReceived)
      assert(a.theTestThisCalled)
      assert(a.theTestThatCalled)

      val b = new AnyFunSpec with CallChecking {
        ignore("test this") { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        it("test that") { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }

      val repB = new TestIgnoredTrackingReporter
      b.run(None, Args(repB))
      assert(repB.testIgnoredReceived)
      assert(repB.lastEvent.isDefined)
      assert(repB.lastEvent.get.testName endsWith "test this")
      assert(!b.theTestThisCalled)
      assert(b.theTestThatCalled)

      val c = new AnyFunSpec with CallChecking {
        it("test this") { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
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
      val d = new AnyFunSpec with CallChecking {
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

    "should ignore a test marked as ignored if run is invoked with that testName" in {
      // If I provide a specific testName to run, then it should ignore an Ignore on that test
      // method and actually invoke it.
      val e = new AnyFunSpec with CallChecking {
        ignore("test this") { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        it("test that") { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }

      import scala.language.reflectiveCalls

      val repE = new TestIgnoredTrackingReporter
      e.run(Some("test this"), Args(repE))
      assert(repE.testIgnoredReceived)
      assert(!e.theTestThisCalled)
      assert(!e.theTestThatCalled)
    }

    "should run only those tests selected by the tags to include and exclude sets" in {

      // Nothing is excluded
      val a = new AnyFunSpec with CallChecking {
        it("test this", mytags.SlowAsMolasses) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        it("test that") { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }

      import scala.language.reflectiveCalls

      val repA = new TestIgnoredTrackingReporter
      a.run(None, Args(repA))
      assert(!repA.testIgnoredReceived)
      assert(a.theTestThisCalled)
      assert(a.theTestThatCalled)

      // SlowAsMolasses is included, one test should be excluded
      val b = new AnyFunSpec with CallChecking {
        it("test this", mytags.SlowAsMolasses) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        it("test that") { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repB = new TestIgnoredTrackingReporter
      b.run(None, Args(repB, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), ConfigMap.empty, None, new Tracker))
      assert(!repB.testIgnoredReceived)
      assert(b.theTestThisCalled)
      assert(!b.theTestThatCalled)

      // SlowAsMolasses is included, and both tests should be included
      val c = new AnyFunSpec with CallChecking {
        it("test this", mytags.SlowAsMolasses) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        it("test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repC = new TestIgnoredTrackingReporter
      c.run(None, Args(repB, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), ConfigMap.empty, None, new Tracker))
      assert(!repC.testIgnoredReceived)
      assert(c.theTestThisCalled)
      assert(c.theTestThatCalled)

      // SlowAsMolasses is included. both tests should be included but one ignored
      val d = new AnyFunSpec with CallChecking {
        ignore("test this", mytags.SlowAsMolasses) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        it("test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repD = new TestIgnoredTrackingReporter
      d.run(None, Args(repD, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.Ignore")), ConfigMap.empty, None, new Tracker))
      assert(repD.testIgnoredReceived)
      assert(!d.theTestThisCalled)
      assert(d.theTestThatCalled)

      // SlowAsMolasses included, FastAsLight excluded
      val e = new AnyFunSpec with CallChecking {
        it("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        it("test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        it("test the other") { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repE = new TestIgnoredTrackingReporter
      e.run(None, Args(repE, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
        ConfigMap.empty, None, new Tracker))
      assert(!repE.testIgnoredReceived)
      assert(!e.theTestThisCalled)
      assert(e.theTestThatCalled)
      assert(!e.theTestTheOtherCalled)

      // An Ignored test that was both included and excluded should not generate a TestIgnored event
      val f = new AnyFunSpec with CallChecking {
        ignore("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        it("test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        it("test the other") { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repF = new TestIgnoredTrackingReporter
      f.run(None, Args(repF, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
        ConfigMap.empty, None, new Tracker))
      assert(!repF.testIgnoredReceived)
      assert(!f.theTestThisCalled)
      assert(f.theTestThatCalled)
      assert(!f.theTestTheOtherCalled)

      // An Ignored test that was not included should not generate a TestIgnored event
      val g = new AnyFunSpec with CallChecking {
        it("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        it("test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        ignore("test the other") { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repG = new TestIgnoredTrackingReporter
      g.run(None, Args(repG, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
        ConfigMap.empty, None, new Tracker))
      assert(!repG.testIgnoredReceived)
      assert(!g.theTestThisCalled)
      assert(g.theTestThatCalled)
      assert(!g.theTestTheOtherCalled)

      // No tagsToInclude set, FastAsLight excluded
      val h = new AnyFunSpec with CallChecking {
        it("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        it("test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        it("test the other") { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repH = new TestIgnoredTrackingReporter
      h.run(None, Args(repH, Stopper.default, Filter(None, Set("org.scalatest.FastAsLight")), ConfigMap.empty, None, new Tracker))
      assert(!repH.testIgnoredReceived)
      assert(!h.theTestThisCalled)
      assert(h.theTestThatCalled)
      assert(h.theTestTheOtherCalled)

      // No tagsToInclude set, mytags.SlowAsMolasses excluded
      val i = new AnyFunSpec with CallChecking {
        it("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        it("test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        it("test the other") { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repI = new TestIgnoredTrackingReporter
      i.run(None, Args(repI, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses")), ConfigMap.empty, None, new Tracker))
      assert(!repI.testIgnoredReceived)
      assert(!i.theTestThisCalled)
      assert(!i.theTestThatCalled)
      assert(i.theTestTheOtherCalled)

      // No tagsToInclude set, mytags.SlowAsMolasses excluded, TestIgnored should not be received on excluded ones
      val j = new AnyFunSpec with CallChecking {
        ignore("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        ignore("test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        it("test the other") { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repJ = new TestIgnoredTrackingReporter
      j.run(None, Args(repJ, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses")), ConfigMap.empty, None, new Tracker))
      assert(!repI.testIgnoredReceived)
      assert(!j.theTestThisCalled)
      assert(!j.theTestThatCalled)
      assert(j.theTestTheOtherCalled)

      // Same as previous, except Ignore specifically mentioned in excludes set
      val k = new AnyFunSpec with CallChecking {
        ignore("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        ignore("test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        ignore("test the other") { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repK = new TestIgnoredTrackingReporter
      k.run(None, Args(repK, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses", "org.scalatest.Ignore")), ConfigMap.empty, None, new Tracker))
      assert(repK.testIgnoredReceived)
      assert(!k.theTestThisCalled)
      assert(!k.theTestThatCalled)
      assert(!k.theTestTheOtherCalled)
    }

    "should run only those registered tests selected by the tags to include and exclude sets" in {

      // Nothing is excluded
      val a = new AnyFunSpec with CallChecking {
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
      val b = new AnyFunSpec with CallChecking {
        registerTest("test this", mytags.SlowAsMolasses) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test that") { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repB = new TestIgnoredTrackingReporter
      b.run(None, Args(repB, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), ConfigMap.empty, None, new Tracker))
      assert(!repB.testIgnoredReceived)
      assert(b.theTestThisCalled)
      assert(!b.theTestThatCalled)

      // SlowAsMolasses is included, and both tests should be included
      val c = new AnyFunSpec with CallChecking {
        registerTest("test this", mytags.SlowAsMolasses) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repC = new TestIgnoredTrackingReporter
      c.run(None, Args(repB, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), ConfigMap.empty, None, new Tracker))
      assert(!repC.testIgnoredReceived)
      assert(c.theTestThisCalled)
      assert(c.theTestThatCalled)

      // SlowAsMolasses is included. both tests should be included but one ignored
      val d = new AnyFunSpec with CallChecking {
        registerIgnoredTest("test this", mytags.SlowAsMolasses) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repD = new TestIgnoredTrackingReporter
      d.run(None, Args(repD, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.Ignore")), ConfigMap.empty, None, new Tracker))
      assert(repD.testIgnoredReceived)
      assert(!d.theTestThisCalled)
      assert(d.theTestThatCalled)

      // SlowAsMolasses included, FastAsLight excluded
      val e = new AnyFunSpec with CallChecking {
        registerTest("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test the other") { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repE = new TestIgnoredTrackingReporter
      e.run(None, Args(repE, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
        ConfigMap.empty, None, new Tracker))
      assert(!repE.testIgnoredReceived)
      assert(!e.theTestThisCalled)
      assert(e.theTestThatCalled)
      assert(!e.theTestTheOtherCalled)

      // An Ignored test that was both included and excluded should not generate a TestIgnored event
      val f = new AnyFunSpec with CallChecking {
        registerIgnoredTest("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test the other") { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repF = new TestIgnoredTrackingReporter
      f.run(None, Args(repF, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
        ConfigMap.empty, None, new Tracker))
      assert(!repF.testIgnoredReceived)
      assert(!f.theTestThisCalled)
      assert(f.theTestThatCalled)
      assert(!f.theTestTheOtherCalled)

      // An Ignored test that was not included should not generate a TestIgnored event
      val g = new AnyFunSpec with CallChecking {
        registerTest("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        registerIgnoredTest("test the other") { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repG = new TestIgnoredTrackingReporter
      g.run(None, Args(repG, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
        ConfigMap.empty, None, new Tracker))
      assert(!repG.testIgnoredReceived)
      assert(!g.theTestThisCalled)
      assert(g.theTestThatCalled)
      assert(!g.theTestTheOtherCalled)

      // No tagsToInclude set, FastAsLight excluded
      val h = new AnyFunSpec with CallChecking {
        registerTest("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test the other") { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repH = new TestIgnoredTrackingReporter
      h.run(None, Args(repH, Stopper.default, Filter(None, Set("org.scalatest.FastAsLight")), ConfigMap.empty, None, new Tracker))
      assert(!repH.testIgnoredReceived)
      assert(!h.theTestThisCalled)
      assert(h.theTestThatCalled)
      assert(h.theTestTheOtherCalled)

      // No tagsToInclude set, mytags.SlowAsMolasses excluded
      val i = new AnyFunSpec with CallChecking {
        registerTest("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test the other") { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repI = new TestIgnoredTrackingReporter
      i.run(None, Args(repI, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses")), ConfigMap.empty, None, new Tracker))
      assert(!repI.testIgnoredReceived)
      assert(!i.theTestThisCalled)
      assert(!i.theTestThatCalled)
      assert(i.theTestTheOtherCalled)

      // No tagsToInclude set, mytags.SlowAsMolasses excluded, TestIgnored should not be received on excluded ones
      val j = new AnyFunSpec with CallChecking {
        registerIgnoredTest("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerIgnoredTest("test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test the other") { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repJ = new TestIgnoredTrackingReporter
      j.run(None, Args(repJ, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses")), ConfigMap.empty, None, new Tracker))
      assert(!repI.testIgnoredReceived)
      assert(!j.theTestThisCalled)
      assert(!j.theTestThatCalled)
      assert(j.theTestTheOtherCalled)

      // Same as previous, except Ignore specifically mentioned in excludes set
      val k = new AnyFunSpec with CallChecking {
        registerIgnoredTest("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerIgnoredTest("test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        registerIgnoredTest("test the other") { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repK = new TestIgnoredTrackingReporter
      k.run(None, Args(repK, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses", "org.scalatest.Ignore")), ConfigMap.empty, None, new Tracker))
      assert(repK.testIgnoredReceived)
      assert(!k.theTestThisCalled)
      assert(!k.theTestThatCalled)
      assert(!k.theTestTheOtherCalled)
    }

    "should return the correct test count from its expectedTestCount method" in {

      val a = new AnyFunSpec {
        it("test this") {/* ASSERTION_SUCCEED */}
        it("test that") {/* ASSERTION_SUCCEED */}
      }
      assert(a.expectedTestCount(Filter()) == 2)

      val b = new AnyFunSpec {
        ignore("test this") {/* ASSERTION_SUCCEED */}
        it("test that") {/* ASSERTION_SUCCEED */}
      }
      assert(b.expectedTestCount(Filter()) == 1)

      val c = new AnyFunSpec {
        it("test this", mytags.FastAsLight) {/* ASSERTION_SUCCEED */}
        it("test that") {/* ASSERTION_SUCCEED */}
      }
      assert(c.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) == 1)
      assert(c.expectedTestCount(Filter(None, Set("org.scalatest.FastAsLight"))) == 1)

      val d = new AnyFunSpec {
        it("test this", mytags.FastAsLight, mytags.SlowAsMolasses) {/* ASSERTION_SUCCEED */}
        it("test that", mytags.SlowAsMolasses) {/* ASSERTION_SUCCEED */}
        it("test the other thing") {/* ASSERTION_SUCCEED */}
      }
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) == 1)
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight"))) == 1)
      assert(d.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) == 1)
      assert(d.expectedTestCount(Filter()) == 3)

      val e = new AnyFunSpec {
        it("test this", mytags.FastAsLight, mytags.SlowAsMolasses) {/* ASSERTION_SUCCEED */}
        it("test that", mytags.SlowAsMolasses) {/* ASSERTION_SUCCEED */}
        ignore("test the other thing") {/* ASSERTION_SUCCEED */}
      }
      assert(e.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) == 1)
      assert(e.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight"))) == 1)
      assert(e.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) == 0)
      assert(e.expectedTestCount(Filter()) == 2)

      val f = new Suites(a, b, c, d, e)
      assert(f.expectedTestCount(Filter()) == 10)
    }

    "should return the correct test count from its expectedTestCount method when register tests with registerTest and registerIgnoredTest" in {

      val a = new AnyFunSpec {
        registerTest("test this") {/* ASSERTION_SUCCEED */}
        registerTest("test that") {/* ASSERTION_SUCCEED */}
      }
      assert(a.expectedTestCount(Filter()) == 2)

      val b = new AnyFunSpec {
        registerIgnoredTest("test this") {/* ASSERTION_SUCCEED */}
        registerTest("test that") {/* ASSERTION_SUCCEED */}
      }
      assert(b.expectedTestCount(Filter()) == 1)

      val c = new AnyFunSpec {
        registerTest("test this", mytags.FastAsLight) {/* ASSERTION_SUCCEED */}
        registerTest("test that") {/* ASSERTION_SUCCEED */}
      }
      assert(c.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) == 1)
      assert(c.expectedTestCount(Filter(None, Set("org.scalatest.FastAsLight"))) == 1)

      val d = new AnyFunSpec {
        registerTest("test this", mytags.FastAsLight, mytags.SlowAsMolasses) {/* ASSERTION_SUCCEED */}
        registerTest("test that", mytags.SlowAsMolasses) {/* ASSERTION_SUCCEED */}
        registerTest("test the other thing") {/* ASSERTION_SUCCEED */}
      }
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) == 1)
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight"))) == 1)
      assert(d.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) == 1)
      assert(d.expectedTestCount(Filter()) == 3)

      val e = new AnyFunSpec {
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
    
    "should generate a TestPending message when the test body is (pending)" in {

      val a = new AnyFunSpec {

        it("should do this") (pending)

        it("should do that") {
          assert(2 + 2 == 4)
        }
        
        it("should do something else") {
          assert(2 + 2 == 4)
          pending
        }
      }
      
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      val tp = rep.testPendingEventsReceived
      assert(tp.size == 2)
    }
    "should generate a test failure if a Throwable, or an Error other than direct Error subtypes " +
            "known in JDK 1.5, excluding AssertionError" in {
      val a = new AnyFunSpec {
        it("throws AssertionError") { throw new AssertionError }
        it("throws plain old Error") { throw new Error }
        it("throws Throwable") { throw new Throwable }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      val tf = rep.testFailedEventsReceived
      assert(tf.size == 3)
    }
    // SKIP-SCALATESTJS,NATIVE-START
    "should propagate out Errors that are direct subtypes of Error in JDK 1.5, other than " +
            "AssertionError, causing Suites and Runs to abort." in {
      val a = new AnyFunSpec {
        it("throws AssertionError") { throw new OutOfMemoryError }
      }
      intercept[OutOfMemoryError] {
        a.run(None, Args(SilentReporter))
      }
    }
    // SKIP-SCALATESTJS,NATIVE-END
/*
    it("should send InfoProvided events with aboutAPendingTest set to true for info " +
            "calls made from a test that is pending") {
      val a = new FunSpec with GivenWhenThen {
        it("should do something else") {
          given("two integers")
          when("one is subracted from the other")
          then("the result is the difference between the two numbers")
          pending
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      val testPending = rep.testPendingEventsReceived
      assert(testPending.size == 1)
      val recordedEvents = testPending(0).recordedEvents
      assert(recordedEvents.size == 3)
      for (event <- recordedEvents) {
        val ip = event.asInstanceOf[InfoProvided]
        assert(ip.aboutAPendingTest.isDefined && ip.aboutAPendingTest.get)
      }
    }
    it("should send InfoProvided events with aboutAPendingTest set to false for info " +
            "calls made from a test that is not pending") {
      val a = new FunSpec with GivenWhenThen {
        it("should do something else") {
          given("two integers")
          when("one is subracted from the other")
          then("the result is the difference between the two numbers")
          assert(1 + 1 == 2)
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      val testSucceeded = rep.testSucceededEventsReceived
      assert(testSucceeded.size == 1)
      val recordedEvents = testSucceeded(0).recordedEvents
      assert(recordedEvents.size == 3)
      for (event <- recordedEvents) {
        val ip = event.asInstanceOf[InfoProvided]
        assert(ip.aboutAPendingTest.isDefined && !ip.aboutAPendingTest.get)
      }
    }
*/
    "should support expectations" ignore { // Unignore after we uncomment the expectation implicits in RegistrationPolicy
      class TestSpec extends AnyFunSpec with expectations.Expectations {
        it("fail scenario") {
          expect(1 == 2); /* ASSERTION_SUCCEED */
        }
        describe("a feature") {
          it("nested fail scenario") {
            expect(1 == 2); /* ASSERTION_SUCCEED */
          }
        }
      }
      val rep = new EventRecordingReporter
      val s1 = new TestSpec
      s1.run(None, Args(rep))
      assert(rep.testFailedEventsReceived.size == 2)
      assert(rep.testFailedEventsReceived(0).throwable.get.asInstanceOf[TestFailedException].failedCodeFileName.get == "AnyFunSpecSpec.scala")
      assert(rep.testFailedEventsReceived(0).throwable.get.asInstanceOf[TestFailedException].failedCodeLineNumber.get == thisLineNumber - 13)
      assert(rep.testFailedEventsReceived(1).throwable.get.asInstanceOf[TestFailedException].failedCodeFileName.get == "AnyFunSpecSpec.scala")
      assert(rep.testFailedEventsReceived(1).throwable.get.asInstanceOf[TestFailedException].failedCodeLineNumber.get == thisLineNumber - 11)
    }
  }
  
  "when failure happens" - {
    
    "should fire TestFailed event with correct stack depth info when test failed" in {
      class TestSpec extends AnyFunSpec {
        it("fail scenario") {
          assert(1 == 2)
        }
        describe("a feature") {
          it("nested fail scenario") {
            assert(1 == 2)
          }
        }
      }
      val rep = new EventRecordingReporter
      val s1 = new TestSpec
      s1.run(None, Args(rep))
      assert(rep.testFailedEventsReceived.size == 2)
      assert(rep.testFailedEventsReceived(0).throwable.get.asInstanceOf[TestFailedException].failedCodeFileName.get == "AnyFunSpecSpec.scala")
      assert(rep.testFailedEventsReceived(0).throwable.get.asInstanceOf[TestFailedException].failedCodeLineNumber.get == thisLineNumber - 13)
      assert(rep.testFailedEventsReceived(1).throwable.get.asInstanceOf[TestFailedException].failedCodeFileName.get == "AnyFunSpecSpec.scala")
      assert(rep.testFailedEventsReceived(1).throwable.get.asInstanceOf[TestFailedException].failedCodeLineNumber.get == thisLineNumber - 11)
    }
    
    // Line checking not working yet.
    "should generate TestRegistrationClosedException with correct stack depth info when has a it nested inside a it" in {
      class TestSpec extends AnyFunSpec {
        var registrationClosedThrown = false
        describe("a feature") {
          it("a scenario") {
            it("nested scenario") {
              assert(1 == 2)
            }; /* ASSERTION_SUCCEED */
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
      assert(testFailedEvents.size == 1)
      assert(testFailedEvents(0).throwable.get.getClass() == classOf[TestRegistrationClosedException])
      val trce = testFailedEvents(0).throwable.get.asInstanceOf[TestRegistrationClosedException]
      assert("AnyFunSpecSpec.scala" == trce.failedCodeFileName.get)
      assert(trce.failedCodeLineNumber.get == thisLineNumber - 24)
      assert(trce.message == Some("An it clause may not appear inside another it or they clause."))
    }

    "should generate TestRegistrationClosedException with correct stack depth info when has a ignore nested inside a it" in {
      class TestSpec extends AnyFunSpec {
        var registrationClosedThrown = false
        describe("a feature") {
          it("a scenario") {
            ignore("nested scenario") {
              assert(1 == 2)
            }; /* ASSERTION_SUCCEED */
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
      assert(testFailedEvents.size == 1)
      assert(testFailedEvents(0).throwable.get.getClass() == classOf[TestRegistrationClosedException])
      val trce = testFailedEvents(0).throwable.get.asInstanceOf[TestRegistrationClosedException]
      assert("AnyFunSpecSpec.scala" == trce.failedCodeFileName.get)
      assert(trce.failedCodeLineNumber.get == thisLineNumber - 24)
      assert(trce.message == Some("An ignore clause may not appear inside an it or a they clause."))
    }

    "should generate TestRegistrationClosedException with correct stack depth info when has a they nested inside a they" in {
      class TestSpec extends AnyFunSpec {
        var registrationClosedThrown = false
        describe("a feature") {
          they("a scenario") {
            they("nested scenario") {
              assert(1 == 2)
            }; /* ASSERTION_SUCCEED */
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
      assert(testFailedEvents.size == 1)
      assert(testFailedEvents(0).throwable.get.getClass() == classOf[TestRegistrationClosedException])
      val trce = testFailedEvents(0).throwable.get.asInstanceOf[TestRegistrationClosedException]
      assert("AnyFunSpecSpec.scala" == trce.failedCodeFileName.get)
      assert(trce.failedCodeLineNumber.get == thisLineNumber - 24)
      assert(trce.message == Some("A they clause may not appear inside another it or they clause."))
    }

    "should generate TestRegistrationClosedException with correct stack depth info when has a ignore nested inside a they" in {
      class TestSpec extends AnyFunSpec {
        var registrationClosedThrown = false
        describe("a feature") {
          they("a scenario") {
            ignore("nested scenario") {
              assert(1 == 2)
            }; /* ASSERTION_SUCCEED */
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
      assert(testFailedEvents.size == 1)
      assert(testFailedEvents(0).throwable.get.getClass() == classOf[TestRegistrationClosedException])
      val trce = testFailedEvents(0).throwable.get.asInstanceOf[TestRegistrationClosedException]
      assert("AnyFunSpecSpec.scala" == trce.failedCodeFileName.get)
      assert(trce.failedCodeLineNumber.get == thisLineNumber - 24)
      assert(trce.message == Some("An ignore clause may not appear inside an it or a they clause."))
    }

    "should allow test registration with registerTest and registerIgnoredTest" in {
      class TestSpec extends AnyFunSpec {
        val a = 1
        registerTest("test 1") {
          val e = intercept[TestFailedException] {
            assert(a == 2)
          }
          assert(e.message == Some("1 did not equal 2"))
          assert(e.failedCodeFileName == Some("AnyFunSpecSpec.scala"))
          assert(e.failedCodeLineNumber == Some(thisLineNumber - 4))
        }
        registerTest("test 2") {
          assert(a == 2)
        }
        registerTest("test 3") {
          pending
        }
        registerTest("test 4") {
          cancel()
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

    "should generate TestRegistrationClosedException with correct stack depth info when has a registerTest nested inside a registerTest" in {
      class TestSpec extends AnyFunSpec {
        var registrationClosedThrown = false
        describe("a feature") {
          registerTest("a scenario") {
            registerTest("nested scenario") {
              assert(1 == 2)
            }; /* ASSERTION_SUCCEED */
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
      assert(testFailedEvents.size == 1)
      assert(testFailedEvents(0).throwable.get.getClass() == classOf[TestRegistrationClosedException])
      val trce = testFailedEvents(0).throwable.get.asInstanceOf[TestRegistrationClosedException]
      assert("AnyFunSpecSpec.scala" == trce.failedCodeFileName.get)
      assert(trce.failedCodeLineNumber.get == thisLineNumber - 24)
      assert(trce.message == Some("Test cannot be nested inside another test."))
    }

    "should generate TestRegistrationClosedException with correct stack depth info when has a registerIgnoredTest nested inside a registerTest" in {
      class TestSpec extends AnyFunSpec {
        var registrationClosedThrown = false
        describe("a feature") {
          registerTest("a scenario") {
            registerIgnoredTest("nested scenario") {
              assert(1 == 2)
            }; /* ASSERTION_SUCCEED */
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
      assert(testFailedEvents.size == 1)
      assert(testFailedEvents(0).throwable.get.getClass() == classOf[TestRegistrationClosedException])
      val trce = testFailedEvents(0).throwable.get.asInstanceOf[TestRegistrationClosedException]
      assert("AnyFunSpecSpec.scala" == trce.failedCodeFileName.get)
      assert(trce.failedCodeLineNumber.get == thisLineNumber - 24)
      assert(trce.message == Some("Test cannot be nested inside another test."))
    }

    "should generate NotAllowedException wrapping a TestFailedException when assert fails in scope" in {
      class TestSpec extends AnyFunSpec {
        describe("a feature") {
          val a = 1
          assert(a == 2)
        }
      }
      val e = intercept[NotAllowedException] {
        new TestSpec
      }
      assert("AnyFunSpecSpec.scala" == e.failedCodeFileName.get)
      assert(e.failedCodeLineNumber.get == thisLineNumber - 7)
      assert(e.message == Some(FailureMessages.assertionShouldBePutInsideItOrTheyClauseNotDescribeClause))

      assert(e.cause.isDefined)
      val causeThrowable = e.cause.get
      assert(causeThrowable.isInstanceOf[TestFailedException])
      val cause = causeThrowable.asInstanceOf[TestFailedException]
      assert("AnyFunSpecSpec.scala" == cause.failedCodeFileName.get)
      assert(cause.failedCodeLineNumber.get == thisLineNumber - 15)
      assert(cause.message == Some(FailureMessages.didNotEqual(prettifier, 1, 2)))
    }

    "should generate NotAllowedException wrapping a TestCanceledException when assume fails in scope" in {
      class TestSpec extends AnyFunSpec {
        describe("a feature") {
          val a = 1
          assume(a == 2)
        }
      }
      val e = intercept[NotAllowedException] {
        new TestSpec
      }
      assert("AnyFunSpecSpec.scala" == e.failedCodeFileName.get)
      assert(e.failedCodeLineNumber.get == thisLineNumber - 7)
      assert(e.message == Some(FailureMessages.assertionShouldBePutInsideItOrTheyClauseNotDescribeClause))

      assert(e.cause.isDefined)
      val causeThrowable = e.cause.get
      assert(causeThrowable.isInstanceOf[TestCanceledException])
      val cause = causeThrowable.asInstanceOf[TestCanceledException]
      assert("AnyFunSpecSpec.scala" == cause.failedCodeFileName.get)
      assert(cause.failedCodeLineNumber.get == thisLineNumber - 15)
      assert(cause.message == Some(FailureMessages.didNotEqual(prettifier, 1, 2)))
    }

    "should generate NotAllowedException wrapping a non-fatal RuntimeException is thrown inside scope" in {
      class TestSpec extends AnyFunSpec {
        describe("a feature") {
          throw new RuntimeException("on purpose")
        }
      }
      val e = intercept[NotAllowedException] {
        new TestSpec
      }
      assert("AnyFunSpecSpec.scala" == e.failedCodeFileName.get)
      assert(e.failedCodeLineNumber.get == thisLineNumber - 8)
      assert(e.cause.isDefined)
      val causeThrowable = e.cause.get
      assert(e.message == Some(FailureMessages.exceptionWasThrownInDescribeClause(prettifier, UnquotedString(causeThrowable.getClass.getName), "a feature", "on purpose")))

      assert(causeThrowable.isInstanceOf[RuntimeException])
      val cause = causeThrowable.asInstanceOf[RuntimeException]
      assert(cause.getMessage == "on purpose")
    }

    "should generate NotAllowedException wrapping a DuplicateTestNameException is thrown inside scope" in {
      class TestSpec extends AnyFunSpec {
        describe("a feature") {
          it("test 1") {/* ASSERTION_SUCCEED */}
          it("test 1") {/* ASSERTION_SUCCEED */}
        }
      }
      val e = intercept[NotAllowedException] {
        new TestSpec
      }
      assert("AnyFunSpecSpec.scala" == e.failedCodeFileName.get)
      assert(e.failedCodeLineNumber.get == thisLineNumber - 7)
      assert(e.cause.isDefined)
      val causeThrowable = e.cause.get
      assert(e.message == Some(FailureMessages.exceptionWasThrownInDescribeClause(prettifier, UnquotedString(causeThrowable.getClass.getName), "a feature", FailureMessages.duplicateTestName(prettifier, UnquotedString("a feature test 1")))))

      assert(causeThrowable.isInstanceOf[DuplicateTestNameException])
      val cause = causeThrowable.asInstanceOf[DuplicateTestNameException]
      assert(cause.getMessage == FailureMessages.duplicateTestName(prettifier, UnquotedString("a feature test 1")))
    }

    // SKIP-SCALATESTJS,NATIVE-START
    "should propagate AnnotationFormatError when it is thrown inside scope" in {
      class TestSpec extends AnyFunSpec {
        describe("a feature") {
          throw new AnnotationFormatError("on purpose")
        }
      }
      val e = intercept[AnnotationFormatError] {
        new TestSpec
      }
      assert(e.getMessage == "on purpose")
    }

    "should propagate AWTError when it is thrown inside scope" in {
      class TestSpec extends AnyFunSpec {
        describe("a feature") {
          throw new AWTError("on purpose")
        }
      }
      val e = intercept[AWTError] {
        new TestSpec
      }
      assert(e.getMessage == "on purpose")
    }

    "should propagate CoderMalfunctionError when it is thrown inside scope" in {
      class TestSpec extends AnyFunSpec {
        describe("a feature") {
          throw new CoderMalfunctionError(new RuntimeException("on purpose"))
        }
      }
      val e = intercept[CoderMalfunctionError] {
        new TestSpec
      }
      assert(e.getMessage == "java.lang.RuntimeException: on purpose")
    }

    "should propagate FactoryConfigurationError when it is thrown inside scope" in {
      class TestSpec extends AnyFunSpec {
        describe("a feature") {
          throw new FactoryConfigurationError("on purpose")
        }
      }
      val e = intercept[FactoryConfigurationError] {
        new TestSpec
      }
      assert(e.getMessage == "on purpose")
    }

    "should propagate LinkageError when it is thrown inside scope" in {
      class TestSpec extends AnyFunSpec {
        describe("a feature") {
          throw new LinkageError("on purpose")
        }
      }
      val e = intercept[LinkageError] {
        new TestSpec
      }
      assert(e.getMessage == "on purpose")
    }

    "should propagate ThreadDeath when it is thrown inside scope" in {
      class TestSpec extends AnyFunSpec {
        describe("a feature") {
          throw new ThreadDeath
        }
      }
      val e = intercept[ThreadDeath] {
        new TestSpec
      }
      assert(e.getMessage == null)
    }

    "should propagate TransformerFactoryConfigurationError when it is thrown inside scope" in {
      class TestSpec extends AnyFunSpec {
        describe("a feature") {
          throw new TransformerFactoryConfigurationError("on purpose")
        }
      }
      val e = intercept[TransformerFactoryConfigurationError] {
        new TestSpec
      }
      assert(e.getMessage == "on purpose")
    }

    "should propagate VirtualMachineError when it is thrown inside scope" in {
      class TestSpec extends AnyFunSpec {
        describe("a feature") {
          throw new VirtualMachineError("on purpose") {}
        }
      }
      val e = intercept[VirtualMachineError] {
        new TestSpec
      }
      assert(e.getMessage == "on purpose")
    }
    // SKIP-SCALATESTJS,NATIVE-END
  }
}

