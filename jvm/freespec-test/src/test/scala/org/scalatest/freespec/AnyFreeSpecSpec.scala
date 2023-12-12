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
package org.scalatest.freespec

// elements
import org.scalatest.SharedHelpers._
import org.scalatest.GivenWhenThen
import org.scalatest.Outcome
import org.scalatest.ConfigMap
import org.scalatest.mytags
import org.scalatest.Args
import org.scalatest.Stopper
import org.scalatest.Filter
import org.scalatest.Tracker
import org.scalatest.Suites
import org.scalatest.expectations
import org.scalatest.Exceptional
import org.scalatest.FailureMessages
import org.scalatest.UnquotedString
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

class AnyFreeSpecSpec extends AnyFunSpec with GivenWhenThen {

  private val prettifier = Prettifier.default

  describe("A FreeSpec") {

    trait InvokeChecking {
      var withFixtureWasInvoked = false
      var testWasInvoked = false
    }

    it("should invoke withFixture from runTest") {
      val a = new AnyFreeSpec with InvokeChecking {
        override def withFixture(test: NoArgTest): Outcome = {
          withFixtureWasInvoked = true
          super.withFixture(test)
        }
        "do something" in {
          testWasInvoked = true
          /* ASSERTION_SUCCEED */
        }
      }

      a.run(None, Args(SilentReporter))
      assert(a.withFixtureWasInvoked)
      assert(a.testWasInvoked)
    }

    trait NameChecking {
      var correctTestNameWasPassed = false
    }

    it("should pass the correct test name in the NoArgTest passed to withFixture") {
      val a = new AnyFreeSpec with NameChecking {
        override def withFixture(test: NoArgTest): Outcome = {
          correctTestNameWasPassed = test.name == "do something"
          super.withFixture(test)
        }
        "do something" in {/* ASSERTION_SUCCEED */}
      }

      a.run(None, Args(SilentReporter))
      assert(a.correctTestNameWasPassed)
    }

    trait ConfigMapChecking {
      var correctConfigMapWasPassed = false
    }

    it("should pass the correct config map in the NoArgTest passed to withFixture") {
      val a = new AnyFreeSpec with ConfigMapChecking {
        override def withFixture(test: NoArgTest): Outcome = {
          correctConfigMapWasPassed = (test.configMap == ConfigMap("hi" -> 7))
          super.withFixture(test)
        }
        "do something" in {/* ASSERTION_SUCCEED */}
      }

      a.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap("hi" -> 7), None, new Tracker()))
      assert(a.correctConfigMapWasPassed)
    }

    describe("(when a nesting rule has been violated)") {

      it("should, if they call a - from within an in clause, result in a TestFailedException when running the test") {

        class MySpec extends AnyFreeSpec {
          "should blow up" in {
            "in the wrong place, at the wrong time" - {
            }
            /* ASSERTION_SUCCEED */
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceivedWithCorrectMessage(spec, "should blow up", "a \"-\" clause may not appear inside an \"in\" clause")
      }
      it("should, if they call a - with a nested in from within an in clause, result in a TestFailedException when running the test") {

        class MySpec extends AnyFreeSpec {
          "should blow up" in {
            "in the wrong place, at the wrong time" - {
              "should never run" in {
                assert(1 == 1)
              }
            }
            /* ASSERTION_SUCCEED */
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceivedWithCorrectMessage(spec, "should blow up", "a \"-\" clause may not appear inside an \"in\" clause")
      }
      it("should, if they call a nested it from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends AnyFreeSpec {
          "should blow up" in {
            "should never run" in {
              assert(1 == 1)
            }
            /* ASSERTION_SUCCEED */
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a nested it with tags from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends AnyFreeSpec {
          "should blow up" in {
            "should never run" taggedAs(mytags.SlowAsMolasses) in {
              assert(1 == 1)
            }
            /* ASSERTION_SUCCEED */
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a nested registerTest with tags from within a registerTest clause, result in a TestFailedException when running the test") {

        class MySpec extends AnyFreeSpec {
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
      it("should, if they call a describe with a nested ignore from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends AnyFreeSpec {
          "should blow up" in {
            "in the wrong place, at the wrong time" - {
              "should never run" ignore {
                assert(1 == 1)
              }
            }
            /* ASSERTION_SUCCEED */
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a nested ignore from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends AnyFreeSpec {
          "should blow up" in {
            "should never run" ignore {
              assert(1 == 1)
            }
            /* ASSERTION_SUCCEED */
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a nested ignore with tags from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends AnyFreeSpec {
          "should blow up" in {
            "should never run" taggedAs(mytags.SlowAsMolasses) ignore {
              assert(1 == 1)
            }
            /* ASSERTION_SUCCEED */
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a nested registerIgnoredTest with tags from within a registerTest clause, result in a TestFailedException when running the test") {

        class MySpec extends AnyFreeSpec {
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

    it("should return the test names in registration order from testNames") {

      val a = new AnyFreeSpec {
        "it should test this" in {/* ASSERTION_SUCCEED */}
        "it should test that" in {/* ASSERTION_SUCCEED */}
      }

      assertResult(List("it should test this", "it should test that")) {
        a.testNames.iterator.toList
      }

      val b = new AnyFreeSpec {}

      assertResult(List[String]()) {
        b.testNames.iterator.toList
      }

      val c = new AnyFreeSpec {
        "it should test that" in {/* ASSERTION_SUCCEED */}
        "it should test this" in {/* ASSERTION_SUCCEED */}
      }

      assertResult(List("it should test that", "it should test this")) {
        c.testNames.iterator.toList
      }

      val d = new AnyFreeSpec {
        "A Tester" - {
          "should test that" in {/* ASSERTION_SUCCEED */}
          "should test this" in {/* ASSERTION_SUCCEED */}
        }
      }

      assertResult(List("A Tester should test that", "A Tester should test this")) {
        d.testNames.iterator.toList
      }

      val e = new AnyFreeSpec {
        "A Tester" - {
          "should test this" in {/* ASSERTION_SUCCEED */}
          "should test that" in {/* ASSERTION_SUCCEED */}
        }
      }

      assertResult(List("A Tester should test this", "A Tester should test that")) {
        e.testNames.iterator.toList
      }
    }

    it("should throw DuplicateTestNameException if a duplicate test name registration is attempted") {
      
      intercept[DuplicateTestNameException] {
        new AnyFreeSpec {
          "should test this" in {/* ASSERTION_SUCCEED */}
          "should test this" in {/* ASSERTION_SUCCEED */}
        }
      }
      intercept[DuplicateTestNameException] {
        new AnyFreeSpec {
          "should test this" in {/* ASSERTION_SUCCEED */}
          "should test this" ignore {/* ASSERTION_SUCCEED */}
        }
      }
      intercept[DuplicateTestNameException] {
        new AnyFreeSpec {
          "should test this" ignore {/* ASSERTION_SUCCEED */}
          "should test this" ignore {/* ASSERTION_SUCCEED */}
        }
      }
      intercept[DuplicateTestNameException] {
        new AnyFreeSpec {
          "should test this" ignore {/* ASSERTION_SUCCEED */}
          "should test this" in {/* ASSERTION_SUCCEED */}
        }
      }
    }

    describe("(with info calls)") {
      class InfoInsideTestSpec extends AnyFreeSpec {
        val msg = "hi there, dude"
        val testName = "test name"
        testName in {
          info(msg); /* ASSERTION_SUCCEED */
        }
      }
      // In a Spec, any InfoProvided's fired during the test should be cached and sent out after the test has
      // suceeded or failed. This makes the report look nicer, because the info is tucked under the "specifier'
      // text for that test.
      it("should, when the info appears in the code of a successful test, report the info in the TestSucceeded") {
        val spec = new InfoInsideTestSpec
        val (testStartingIndex, testSucceededIndex) =
          getIndexesForTestInformerEventOrderTests(spec, spec.testName, spec.msg)
        assert(testStartingIndex < testSucceededIndex)
      }
      class InfoBeforeTestSpec extends AnyFreeSpec {
        val msg = "hi there, dude"
        val testName = "test name"
        info(msg)
        testName in {/* ASSERTION_SUCCEED */}
      }
      it("should, when the info appears in the body before a test, report the info before the test") {
        val spec = new InfoBeforeTestSpec
        val (infoProvidedIndex, testStartingIndex, testSucceededIndex) =
          getIndexesForInformerEventOrderTests(spec, spec.testName, spec.msg)
        assert(infoProvidedIndex < testStartingIndex)
        assert(testStartingIndex < testSucceededIndex)
      }
      it("should, when the info appears in the body after a test, report the info after the test runs") {
        val msg = "hi there, dude"
        val testName = "test name"
        class MySpec extends AnyFreeSpec {
          testName in {/* ASSERTION_SUCCEED */}
          info(msg)
        }
        val (infoProvidedIndex, testStartingIndex, testSucceededIndex) =
          getIndexesForInformerEventOrderTests(new MySpec, testName, msg)
        assert(testStartingIndex < testSucceededIndex)
        assert(testSucceededIndex < infoProvidedIndex)
      }
      it("should print to stdout when info is called by a method invoked after the suite has been executed") {
        class MySpec extends AnyFreeSpec {
          callInfo() // This should work fine
          def callInfo(): Unit = {
            info("howdy")
          }
          "howdy also" in {
            callInfo() // This should work fine
            /* ASSERTION_SUCCEED */
          }
        }
        val spec = new MySpec
        val myRep = new EventRecordingReporter
        spec.run(None, Args(myRep))
        spec.callInfo() // TODO: Actually test that This prints to stdout
      }
      it("should send an InfoProvided with an IndentedText formatter with level 1 when called outside a test") {
        val spec = new InfoBeforeTestSpec
        val indentedText = getIndentedTextFromInfoProvided(spec)
        assert(indentedText == IndentedText("+ " + spec.msg, spec.msg, 0))
      }
      it("should send an InfoProvided with an IndentedText formatter with level 2 when called within a test") {
        val spec = new InfoInsideTestSpec
        val indentedText = getIndentedTextFromTestInfoProvided(spec)
        assert(indentedText == IndentedText("  + " + spec.msg, spec.msg, 1))
      }
    }
    it("should throw NullArgumentException if a null test tag is provided") {
      // it
      intercept[NullArgumentException] {
        new AnyFreeSpec {
          "hi" taggedAs(null) in {/* ASSERTION_SUCCEED */}
        }
      }
      val caught = intercept[NullArgumentException] {
        new AnyFreeSpec {
          "hi" taggedAs(mytags.SlowAsMolasses, null) in {/* ASSERTION_SUCCEED */}
        }
      }
      assert(caught.getMessage == "a test tag was null")
      intercept[NullArgumentException] {
        new AnyFreeSpec {
          "hi" taggedAs(mytags.SlowAsMolasses, null, mytags.WeakAsAKitten) in {/* ASSERTION_SUCCEED */}
        }
      }

      // ignore
      intercept[NullArgumentException] {
        new AnyFreeSpec {
          "hi" taggedAs(null) ignore {/* ASSERTION_SUCCEED */}
        }
      }
      val caught2 = intercept[NullArgumentException] {
        new AnyFreeSpec {
          "hi" taggedAs(mytags.SlowAsMolasses, null) ignore {/* ASSERTION_SUCCEED */}
        }
      }
      assert(caught2.getMessage == "a test tag was null")
      intercept[NullArgumentException] {
        new AnyFreeSpec {
          "hi" taggedAs(mytags.SlowAsMolasses, null, mytags.WeakAsAKitten) ignore {/* ASSERTION_SUCCEED */}
        }
      }

      // registerTest
      intercept[NullArgumentException] {
        new AnyFreeSpec {
          registerTest("hi", null) {/* ASSERTION_SUCCEED */}
        }
      }
      val caught3 = intercept[NullArgumentException] {
        new AnyFreeSpec {
          registerTest("hi", mytags.SlowAsMolasses, null) {/* ASSERTION_SUCCEED */}
        }
      }
      assert(caught3.getMessage == "a test tag was null")
      intercept[NullArgumentException] {
        new AnyFreeSpec {
          registerTest("hi", mytags.SlowAsMolasses, null, mytags.WeakAsAKitten) {/* ASSERTION_SUCCEED */}
        }
      }

      // ignore
      intercept[NullArgumentException] {
        new AnyFreeSpec {
          registerIgnoredTest("hi", null) {/* ASSERTION_SUCCEED */}
        }
      }
      val caught4 = intercept[NullArgumentException] {
        new AnyFreeSpec {
          registerIgnoredTest("hi", mytags.SlowAsMolasses, null) {/* ASSERTION_SUCCEED */}
        }
      }
      assert(caught4.getMessage == "a test tag was null")
      intercept[NullArgumentException] {
        new AnyFreeSpec {
          registerIgnoredTest("hi", mytags.SlowAsMolasses, null, mytags.WeakAsAKitten) {/* ASSERTION_SUCCEED */}
        }
      }
    }
    it("should return a correct tags map from the tags method using is (pending)") {

      val a = new AnyFreeSpec {
        "test this" ignore {/* ASSERTION_SUCCEED */}
        "test that" is (pending)
      }
      assertResult(Map("test this" -> Set("org.scalatest.Ignore"))) {
        a.tags
      }

      val b = new AnyFreeSpec {
        "test this" is (pending)
        "test that" ignore {/* ASSERTION_SUCCEED */}
      }
      assertResult(Map("test that" -> Set("org.scalatest.Ignore"))) {
        b.tags
      }

      val c = new AnyFreeSpec {
        "test this" ignore {/* ASSERTION_SUCCEED */}
        "test that" ignore {/* ASSERTION_SUCCEED */}
      }
      assertResult(Map("test this" -> Set("org.scalatest.Ignore"), "test that" -> Set("org.scalatest.Ignore"))) {
        c.tags
      }

      val d = new AnyFreeSpec {
        "test this" taggedAs(mytags.SlowAsMolasses) is (pending)
        "test that" taggedAs(mytags.SlowAsMolasses) ignore {/* ASSERTION_SUCCEED */}
      }
      assertResult(Map("test this" -> Set("org.scalatest.SlowAsMolasses"), "test that" -> Set("org.scalatest.Ignore", "org.scalatest.SlowAsMolasses"))) {
        d.tags
      }

      val e = new AnyFreeSpec {
        "test this" is (pending)
        "test that" is (pending)
      }
      assertResult(Map()) {
        e.tags
      }

      val f = new AnyFreeSpec {
        "test this" taggedAs(mytags.SlowAsMolasses, mytags.WeakAsAKitten) is (pending)
        "test that" taggedAs(mytags.SlowAsMolasses) in  {/* ASSERTION_SUCCEED */}
      }
      assertResult(Map("test this" -> Set("org.scalatest.SlowAsMolasses", "org.scalatest.WeakAsAKitten"), "test that" -> Set("org.scalatest.SlowAsMolasses"))) {
        f.tags
      }

      val g = new AnyFreeSpec {
        "test this" taggedAs(mytags.SlowAsMolasses, mytags.WeakAsAKitten) is (pending)
        "test that" taggedAs(mytags.SlowAsMolasses) in  {/* ASSERTION_SUCCEED */}
      }
      assertResult(Map("test this" -> Set("org.scalatest.SlowAsMolasses", "org.scalatest.WeakAsAKitten"), "test that" -> Set("org.scalatest.SlowAsMolasses"))) {
        g.tags
      }
    }

    class TestWasCalledSuite extends AnyFreeSpec {
      var theTestThisCalled = false
      var theTestThatCalled = false
      "run this" in { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
      "run that, maybe" in { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
    }

    it("should execute all tests when run is called with testName None") {

      val b = new TestWasCalledSuite
      b.run(None, Args(SilentReporter))
      assert(b.theTestThisCalled)
      assert(b.theTestThatCalled)
    }

    it("should execute one test when run is called with a defined testName") {

      val a = new TestWasCalledSuite
      a.run(Some("run this"), Args(SilentReporter))
      assert(a.theTestThisCalled)
      assert(!a.theTestThatCalled)
    }

    trait CallChecking {
      var theTestThisCalled = false
      var theTestThatCalled = false
      var theTestTheOtherCalled = false
    }

    it("should report as ignored, and not run, tests marked ignored") {

      val a = new AnyFreeSpec with CallChecking {
        "test this" in { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        "test that" in { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }

      val repA = new TestIgnoredTrackingReporter
      a.run(None, Args(repA))
      assert(!repA.testIgnoredReceived)
      assert(a.theTestThisCalled)
      assert(a.theTestThatCalled)

      val b = new AnyFreeSpec with CallChecking {
        "test this" ignore { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        "test that" in { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }

      val repB = new TestIgnoredTrackingReporter
      b.run(None, Args(repB))
      assert(repB.testIgnoredReceived)
      assert(repB.lastEvent.isDefined)
      assert(repB.lastEvent.get.testName endsWith "test this")
      assert(!b.theTestThisCalled)
      assert(b.theTestThatCalled)

      val c = new AnyFreeSpec with CallChecking {
        "test this" in { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        "test that" ignore { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
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
      val d = new AnyFreeSpec with CallChecking {
        "test this" ignore { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        "test that" ignore { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
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
      val e = new AnyFreeSpec with CallChecking {
        "test this" ignore { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        "test that" in { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }

      val repE = new TestIgnoredTrackingReporter
      e.run(Some("test this"), Args(repE))
      assert(repE.testIgnoredReceived)
      assert(!e.theTestThisCalled)
      assert(!e.theTestThatCalled)
    }

    it("should run only those tests selected by the tags to include and exclude sets") {

      // Nothing is excluded
      val a = new AnyFreeSpec with CallChecking {
        "test this" taggedAs(mytags.SlowAsMolasses) in { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        "test that" in { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }

      val repA = new TestIgnoredTrackingReporter
      a.run(None, Args(repA))
      assert(!repA.testIgnoredReceived)
      assert(a.theTestThisCalled)
      assert(a.theTestThatCalled)

      // SlowAsMolasses is included, one test should be excluded
      val b = new AnyFreeSpec with CallChecking {
        "test this" taggedAs(mytags.SlowAsMolasses) in { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        "test that" in { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repB = new TestIgnoredTrackingReporter
      b.run(None, Args(repB, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), ConfigMap.empty, None, new Tracker))
      assert(!repB.testIgnoredReceived)
      assert(b.theTestThisCalled)
      assert(!b.theTestThatCalled)

      // SlowAsMolasses is included, and both tests should be included
      val c = new AnyFreeSpec with CallChecking {
        "test this" taggedAs(mytags.SlowAsMolasses) in { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        "test that" taggedAs(mytags.SlowAsMolasses) in { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repC = new TestIgnoredTrackingReporter
      c.run(None, Args(repB, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), ConfigMap.empty, None, new Tracker))
      assert(!repC.testIgnoredReceived)
      assert(c.theTestThisCalled)
      assert(c.theTestThatCalled)

      // SlowAsMolasses is included. both tests should be included but one ignored
      val d = new AnyFreeSpec with CallChecking {
        "test this" taggedAs(mytags.SlowAsMolasses) ignore { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        "test that" taggedAs(mytags.SlowAsMolasses) in { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repD = new TestIgnoredTrackingReporter
      d.run(None, Args(repD, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.Ignore")), ConfigMap.empty, None, new Tracker))
      assert(repD.testIgnoredReceived)
      assert(!d.theTestThisCalled)
      assert(d.theTestThatCalled)

      // SlowAsMolasses included, FastAsLight excluded
      val e = new AnyFreeSpec with CallChecking {
        "test this" taggedAs(mytags.SlowAsMolasses, mytags.FastAsLight) in { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        "test that" taggedAs(mytags.SlowAsMolasses) in { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        "test the other" in { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repE = new TestIgnoredTrackingReporter
      e.run(None, Args(repE, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
                ConfigMap.empty, None, new Tracker))
      assert(!repE.testIgnoredReceived)
      assert(!e.theTestThisCalled)
      assert(e.theTestThatCalled)
      assert(!e.theTestTheOtherCalled)

      // An Ignored test that was both included and excluded should not generate a TestIgnored event
      val f = new AnyFreeSpec with CallChecking {
        "test this" taggedAs(mytags.SlowAsMolasses, mytags.FastAsLight) ignore { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        "test that" taggedAs(mytags.SlowAsMolasses) in { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        "test the other" in { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repF = new TestIgnoredTrackingReporter
      f.run(None, Args(repF, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
                ConfigMap.empty, None, new Tracker))
      assert(!repF.testIgnoredReceived)
      assert(!f.theTestThisCalled)
      assert(f.theTestThatCalled)
      assert(!f.theTestTheOtherCalled)

      // An Ignored test that was not included should not generate a TestIgnored event
      val g = new AnyFreeSpec with CallChecking {
        "test this" taggedAs(mytags.SlowAsMolasses, mytags.FastAsLight) in { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        "test that" taggedAs(mytags.SlowAsMolasses) in { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        "test the other" ignore { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repG = new TestIgnoredTrackingReporter
      g.run(None, Args(repG, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
                ConfigMap.empty, None, new Tracker))
      assert(!repG.testIgnoredReceived)
      assert(!g.theTestThisCalled)
      assert(g.theTestThatCalled)
      assert(!g.theTestTheOtherCalled)

      // No tagsToInclude set, FastAsLight excluded
      val h = new AnyFreeSpec with CallChecking {
        "test this" taggedAs(mytags.SlowAsMolasses, mytags.FastAsLight) in { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        "test that" taggedAs(mytags.SlowAsMolasses) in { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        "test the other" in { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repH = new TestIgnoredTrackingReporter
      h.run(None, Args(repH, Stopper.default, Filter(None, Set("org.scalatest.FastAsLight")), ConfigMap.empty, None, new Tracker))
      assert(!repH.testIgnoredReceived)
      assert(!h.theTestThisCalled)
      assert(h.theTestThatCalled)
      assert(h.theTestTheOtherCalled)

      // No tagsToInclude set, mytags.SlowAsMolasses excluded
      val i = new AnyFreeSpec with CallChecking {
        "test this" taggedAs(mytags.SlowAsMolasses, mytags.FastAsLight) in { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        "test that" taggedAs(mytags.SlowAsMolasses) in { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        "test the other" in { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repI = new TestIgnoredTrackingReporter
      i.run(None, Args(repI, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses")), ConfigMap.empty, None, new Tracker))
      assert(!repI.testIgnoredReceived)
      assert(!i.theTestThisCalled)
      assert(!i.theTestThatCalled)
      assert(i.theTestTheOtherCalled)

      // No tagsToInclude set, mytags.SlowAsMolasses excluded, TestIgnored should not be received on excluded ones
      val j = new AnyFreeSpec with CallChecking {
        "test this" taggedAs(mytags.SlowAsMolasses, mytags.FastAsLight) ignore { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        "test that" taggedAs(mytags.SlowAsMolasses) ignore { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        "test the other" in { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repJ = new TestIgnoredTrackingReporter
      j.run(None, Args(repJ, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses")), ConfigMap.empty, None, new Tracker))
      assert(!repI.testIgnoredReceived)
      assert(!j.theTestThisCalled)
      assert(!j.theTestThatCalled)
      assert(j.theTestTheOtherCalled)

      // Same as previous, except Ignore specifically mentioned in excludes set
      val k = new AnyFreeSpec with CallChecking {
        "test this" taggedAs(mytags.SlowAsMolasses, mytags.FastAsLight) ignore { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        "test that" taggedAs(mytags.SlowAsMolasses) ignore { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        "test the other" ignore { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repK = new TestIgnoredTrackingReporter
      k.run(None, Args(repK, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses", "org.scalatest.Ignore")), ConfigMap.empty, None, new Tracker))
      assert(repK.testIgnoredReceived)
      assert(!k.theTestThisCalled)
      assert(!k.theTestThatCalled)
      assert(!k.theTestTheOtherCalled)
    }

    it("should run only those registered tests selected by the tags to include and exclude sets") {

      // Nothing is excluded
      val a = new AnyFreeSpec with CallChecking {
        registerTest("test this", mytags.SlowAsMolasses) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test that") { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }

      val repA = new TestIgnoredTrackingReporter
      a.run(None, Args(repA))
      assert(!repA.testIgnoredReceived)
      assert(a.theTestThisCalled)
      assert(a.theTestThatCalled)

      // SlowAsMolasses is included, one test should be excluded
      val b = new AnyFreeSpec with CallChecking {
        registerTest("test this", mytags.SlowAsMolasses) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test that") { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repB = new TestIgnoredTrackingReporter
      b.run(None, Args(repB, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), ConfigMap.empty, None, new Tracker))
      assert(!repB.testIgnoredReceived)
      assert(b.theTestThisCalled)
      assert(!b.theTestThatCalled)

      // SlowAsMolasses is included, and both tests should be included
      val c = new AnyFreeSpec with CallChecking {
        registerTest("test this", mytags.SlowAsMolasses) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repC = new TestIgnoredTrackingReporter
      c.run(None, Args(repB, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), ConfigMap.empty, None, new Tracker))
      assert(!repC.testIgnoredReceived)
      assert(c.theTestThisCalled)
      assert(c.theTestThatCalled)

      // SlowAsMolasses is included. both tests should be included but one ignored
      val d = new AnyFreeSpec with CallChecking {
        registerIgnoredTest("test this", mytags.SlowAsMolasses) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repD = new TestIgnoredTrackingReporter
      d.run(None, Args(repD, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.Ignore")), ConfigMap.empty, None, new Tracker))
      assert(repD.testIgnoredReceived)
      assert(!d.theTestThisCalled)
      assert(d.theTestThatCalled)

      // SlowAsMolasses included, FastAsLight excluded
      val e = new AnyFreeSpec with CallChecking {
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
      val f = new AnyFreeSpec with CallChecking {
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
      val g = new AnyFreeSpec with CallChecking {
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
      val h = new AnyFreeSpec with CallChecking {
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
      val i = new AnyFreeSpec with CallChecking {
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
      val j = new AnyFreeSpec with CallChecking {
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
      val k = new AnyFreeSpec with CallChecking {
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

    it("should return the correct test count from its expectedTestCount method") {

      val a = new AnyFreeSpec {
        "test this" in {/* ASSERTION_SUCCEED */}
        "test that" in {/* ASSERTION_SUCCEED */}
      }
      assert(a.expectedTestCount(Filter()) == 2)

      val b = new AnyFreeSpec {
        "test this" ignore {/* ASSERTION_SUCCEED */}
        "test that" in {/* ASSERTION_SUCCEED */}
      }
      assert(b.expectedTestCount(Filter()) == 1)

      val c = new AnyFreeSpec {
        "test this" taggedAs(mytags.FastAsLight) in {/* ASSERTION_SUCCEED */}
        "test that" in {/* ASSERTION_SUCCEED */}
      }
      assert(c.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) == 1)
      assert(c.expectedTestCount(Filter(None, Set("org.scalatest.FastAsLight"))) == 1)

      val d = new AnyFreeSpec {
        "test this" taggedAs(mytags.FastAsLight, mytags.SlowAsMolasses) in {/* ASSERTION_SUCCEED */}
        "test that" taggedAs(mytags.SlowAsMolasses) in {/* ASSERTION_SUCCEED */}
        "test the other thing" in {/* ASSERTION_SUCCEED */}
      }
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) == 1)
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight"))) == 1)
      assert(d.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) == 1)
      assert(d.expectedTestCount(Filter()) == 3)

      val e = new AnyFreeSpec {
        "test this" taggedAs(mytags.FastAsLight, mytags.SlowAsMolasses) in {/* ASSERTION_SUCCEED */}
        "test that" taggedAs(mytags.SlowAsMolasses) in {/* ASSERTION_SUCCEED */}
        "test the other thing" ignore {/* ASSERTION_SUCCEED */}
      }
      assert(e.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) == 1)
      assert(e.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight"))) == 1)
      assert(e.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) == 0)
      assert(e.expectedTestCount(Filter()) == 2)

      val f = new Suites(a, b, c, d, e)
      assert(f.expectedTestCount(Filter()) == 10)
    }
    it("should return the correct test count from its expectedTestCount method when uses registerTest and registerIgnoredTest to register tests") {

      val a = new AnyFreeSpec {
        registerTest("test this") {/* ASSERTION_SUCCEED */}
        registerTest("test that") {/* ASSERTION_SUCCEED */}
      }
      assert(a.expectedTestCount(Filter()) == 2)

      val b = new AnyFreeSpec {
        registerIgnoredTest("test this") {/* ASSERTION_SUCCEED */}
        registerTest("test that") {/* ASSERTION_SUCCEED */}
      }
      assert(b.expectedTestCount(Filter()) == 1)

      val c = new AnyFreeSpec {
        registerTest("test this", mytags.FastAsLight) {/* ASSERTION_SUCCEED */}
        registerTest("test that") {/* ASSERTION_SUCCEED */}
      }
      assert(c.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) == 1)
      assert(c.expectedTestCount(Filter(None, Set("org.scalatest.FastAsLight"))) == 1)

      val d = new AnyFreeSpec {
        registerTest("test this", mytags.FastAsLight, mytags.SlowAsMolasses) {/* ASSERTION_SUCCEED */}
        registerTest("test that", mytags.SlowAsMolasses) {/* ASSERTION_SUCCEED */}
        registerTest("test the other thing") {/* ASSERTION_SUCCEED */}
      }
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) == 1)
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight"))) == 1)
      assert(d.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) == 1)
      assert(d.expectedTestCount(Filter()) == 3)

      val e = new AnyFreeSpec {
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
      val a = new AnyFreeSpec {

        "should do this" is (pending)

        "should do that" in {
          assert(2 + 2 == 4)
        }
        "should do something else" in {
          assert(2 + 2 == 4)
          pending
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      val tp = rep.testPendingEventsReceived
      assert(tp.size == 2)
    }
    it("should allow is pendingUntilFixed to be used after is") {
      val a = new AnyFreeSpec {

        "should do this" is pendingUntilFixed {
          fail("i meant to do that")
        }

        "should do that" in {
          assert(2 + 2 == 4)
        }
        "should do something else" in {
          assert(2 + 2 == 4)
          pending
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      val tp = rep.testPendingEventsReceived
      assert(tp.size == 2)
    }
    it("should generate a TestCanceled message when the test body includes a cancel() invocation") {
      val a = new AnyFreeSpec {

        "should do this" in { cancel("changed my mind") }

        "should do that" in {
          assert(2 + 2 == 4)
        }
        "should do something else" in {
          assert(2 + 2 == 4)
          cancel()
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      val tp = rep.testCanceledEventsReceived
      assert(tp.size == 2)
    }
    it("should generate a TestCanceled message when the test body includes a failed assume() invocation") {
      val a = new AnyFreeSpec {

        "should do this" in { assume(2 == 3, "changed my mind") }

        "should do that" in {
          assume(1 + 1 == 2)
          assert(2 + 2 == 4)
        }
        "should do something else" in {
          assert(2 + 2 == 4)
          assume(1 + 1 == 3)
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      val tp = rep.testCanceledEventsReceived
      assert(tp.size == 2)
    }
    it("should generate a test failure if a Throwable, or an Error other than direct Error subtypes " +
            "known in JDK 1.5, excluding AssertionError") {
      val a = new AnyFreeSpec {
        "This FreeSpec" - {
          "should throw AssertionError" in { throw new AssertionError }
          "should throw plain old Error" in { throw new Error }
          "should throw Throwable" in { throw new Throwable }
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      val tf = rep.testFailedEventsReceived
      assert(tf.size == 3)
    }
    // SKIP-SCALATESTJS,NATIVE-START
    it("should propagate out Errors that are direct subtypes of Error in JDK 1.5, other than " +
            "AssertionError, causing Suites and Runs to abort.") {
      val a = new AnyFreeSpec {
        "This FreeSpec" - {
          "should throw AssertionError" in { throw new OutOfMemoryError }
        }
      }
      intercept[OutOfMemoryError] {
        a.run(None, Args(SilentReporter))
      }
    }
    // SKIP-SCALATESTJS,NATIVE-END
/*
    it("should send InfoProvided events with aboutAPendingTest set to true for info " +
            "calls made from a test that is pending") {
      val a = new FreeSpec with GivenWhenThen {
        "A FreeSpec" - {
          "should do something" in {
            given("two integers")
            when("one is subracted from the other")
            then("the result is the difference between the two numbers")
            pending
          }
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
      val so = rep.scopeOpenedEventsReceived
      assert(so.size == 1)
      for (event <- so) {
        assert(event.message == "A FreeSpec")
      }
      val sc = rep.scopeClosedEventsReceived
      assert(so.size == 1)
      for (event <- sc) {
        assert(event.message == "A FreeSpec")
      }
    }
    it("should send InfoProvided events with aboutAPendingTest set to false for info " +
            "calls made from a test that is not pending") {
      val a = new FreeSpec with GivenWhenThen {
        "A FreeSpec" - {
          "should do something" in {
            given("two integers")
            when("one is subracted from the other")
            then("the result is the difference between the two numbers")
            assert(1 + 1 == 2)
          }
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
      val so = rep.scopeOpenedEventsReceived
      assert(so.size == 1)
      for (event <- so) {
        assert(event.message == "A FreeSpec")
      }
      val sc = rep.scopeClosedEventsReceived
      assert(so.size == 1)
      for (event <- sc) {
        assert(event.message == "A FreeSpec")
      }
    }
*/
    it("should not put parentheses around should clauses that follow when") {
      val a = new AnyFreeSpec {
        "A Stack" - {
          "when empty" - {
            "should chill out" in {
              assert(1 + 1 == 2)
            }
          }
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      val ts = rep.testSucceededEventsReceived
      assert(ts.size == 1)
      assert(ts.head.testName == "A Stack when empty should chill out")
    }
    it("should not put parentheses around should clauses that don't follow when") {
      val a = new AnyFreeSpec {
        "A Stack" - {
          "should chill out" in {
            assert(1 + 1 == 2)
          }
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      val ts = rep.testSucceededEventsReceived
      assert(ts.size == 1)
      assert(ts.head.testName == "A Stack should chill out")
    }

    it("should allow test registration with registerTest and registerIgnoredTest") {
      class TestSpec extends AnyFreeSpec {
        val a = 1
        registerTest("test 1") {
          val e = intercept[TestFailedException] {
            assert(a == 2)
          }
          assert(e.message == Some("1 did not equal 2"))
          assert(e.failedCodeFileName == Some("AnyFreeSpecSpec.scala"))
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
    ignore("should support expectations") { // Unignore after we uncomment the expectation implicits in RegistrationPolicy
      class TestSpec extends AnyFreeSpec with expectations.Expectations {
        "it should do something" in {
          expect(1 == 2); /* ASSERTION_SUCCEED */
        }
        "a widget" - {
          "should do something else" in {
            expect(1 == 2); /* ASSERTION_SUCCEED */
          }
        }
      }
      val rep = new EventRecordingReporter
      val s1 = new TestSpec
      s1.run(None, Args(rep))
      assert(rep.testFailedEventsReceived.size == 2)
      assert(rep.testFailedEventsReceived(0).throwable.get.asInstanceOf[TestFailedException].failedCodeFileName.get == "AnyFreeSpecSpec.scala")
      assert(rep.testFailedEventsReceived(0).throwable.get.asInstanceOf[TestFailedException].failedCodeLineNumber.get == thisLineNumber - 13)
      assert(rep.testFailedEventsReceived(1).throwable.get.asInstanceOf[TestFailedException].failedCodeFileName.get == "AnyFreeSpecSpec.scala")
      assert(rep.testFailedEventsReceived(1).throwable.get.asInstanceOf[TestFailedException].failedCodeLineNumber.get == thisLineNumber - 11)
    }
  }
  
  describe("when failure happens") {
    
    it("should fire TestFailed event with correct stack depth info when test failed") {
      class TestSpec extends AnyFreeSpec {
        "fail scenario" in {
          assert(1 == 2)
        }
        "a feature" - {
          "nested fail scenario" in {
            assert(1 == 2)
          }
        }
      }
      val rep = new EventRecordingReporter
      val s1 = new TestSpec
      s1.run(None, Args(rep))
      assert(rep.testFailedEventsReceived.size == 2)
      assert(rep.testFailedEventsReceived(0).throwable.get.asInstanceOf[TestFailedException].failedCodeFileName.get == "AnyFreeSpecSpec.scala")
      assert(rep.testFailedEventsReceived(0).throwable.get.asInstanceOf[TestFailedException].failedCodeLineNumber.get == thisLineNumber - 13)
      assert(rep.testFailedEventsReceived(1).throwable.get.asInstanceOf[TestFailedException].failedCodeFileName.get == "AnyFreeSpecSpec.scala")
      assert(rep.testFailedEventsReceived(1).throwable.get.asInstanceOf[TestFailedException].failedCodeLineNumber.get == thisLineNumber - 11)
    }
    
    it("should generate TestRegistrationClosedException with correct stack depth info when has an in nested inside an in") {
      class TestSpec extends AnyFreeSpec {
        var registrationClosedThrown = false
        "a feature" - {
          "a scenario" in {
            "nested scenario" in {
              /* ASSERTION_SUCCEED */
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
      assert("AnyFreeSpecSpec.scala" == trce.failedCodeFileName.get)
      assert(trce.failedCodeLineNumber.get == thisLineNumber - 24)
      assert(trce.message == Some("An in clause may not appear inside another in clause."))
    }

    it("should generate TestRegistrationClosedException with correct stack depth info when has an ignore nested inside an in") {
      class TestSpec extends AnyFreeSpec {
        var registrationClosedThrown = false
        "a feature" - {
          "a scenario" in {
            "nested scenario" ignore {
              /* ASSERTION_SUCCEED */
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
      assert("AnyFreeSpecSpec.scala" == trce.failedCodeFileName.get)
      assert(trce.failedCodeLineNumber.get == thisLineNumber - 24)
      assert(trce.message == Some("An ignore clause may not appear inside an in clause."))
    }

    it("should generate TestRegistrationClosedException with correct stack depth info when has a registerTest nested inside a registerTest") {
      class TestSpec extends AnyFreeSpec {
        var registrationClosedThrown = false
        "a feature" - {
          registerTest("a scenario") {
            registerTest("nested scenario") {
              /* ASSERTION_SUCCEED */
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
      assert("AnyFreeSpecSpec.scala" == trce.failedCodeFileName.get)
      assert(trce.failedCodeLineNumber.get == thisLineNumber - 24)
      assert(trce.message == Some("Test cannot be nested inside another test."))
    }

    it("should generate TestRegistrationClosedException with correct stack depth info when has a registerTest nested inside a registerIgnoredTest") {
      class TestSpec extends AnyFreeSpec {
        var registrationClosedThrown = false
        "a feature" - {
          registerTest("a scenario") {
            registerIgnoredTest("nested scenario") {
              /* ASSERTION_SUCCEED */
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
      assert("AnyFreeSpecSpec.scala" == trce.failedCodeFileName.get)
      assert(trce.failedCodeLineNumber.get == thisLineNumber - 24)
      assert(trce.message == Some("Test cannot be nested inside another test."))
    }

    it("should generate NotAllowedException wrapping a TestFailedException when assert fails in scope") {
      class TestSpec extends AnyFreeSpec {
        "a feature" - {
          val a = 1
          assert(a == 2)
        }
      }
      val e = intercept[NotAllowedException] {
        new TestSpec
      }
      assert("AnyFreeSpecSpec.scala" == e.failedCodeFileName.get)
      assert(e.failedCodeLineNumber.get == thisLineNumber - 7)
      assert(e.message == Some(FailureMessages.assertionShouldBePutInsideInClauseNotDashClause))

      assert(e.cause.isDefined)
      val causeThrowable = e.cause.get
      assert(causeThrowable.isInstanceOf[TestFailedException])
      val cause = causeThrowable.asInstanceOf[TestFailedException]
      assert("AnyFreeSpecSpec.scala" == cause.failedCodeFileName.get)
      assert(cause.failedCodeLineNumber.get == thisLineNumber - 15)
      assert(cause.message == Some(FailureMessages.didNotEqual(prettifier, 1, 2)))
    }

    it("should generate NotAllowedException wrapping a TestCanceledException when assume fails in scope") {
      class TestSpec extends AnyFreeSpec {
        "a feature" - {
          val a = 1
          assume(a == 2)
        }
      }
      val e = intercept[NotAllowedException] {
        new TestSpec
      }
      assert("AnyFreeSpecSpec.scala" == e.failedCodeFileName.get)
      assert(e.failedCodeLineNumber.get == thisLineNumber - 7)
      assert(e.message == Some(FailureMessages.assertionShouldBePutInsideInClauseNotDashClause))

      assert(e.cause.isDefined)
      val causeThrowable = e.cause.get
      assert(causeThrowable.isInstanceOf[TestCanceledException])
      val cause = causeThrowable.asInstanceOf[TestCanceledException]
      assert("AnyFreeSpecSpec.scala" == cause.failedCodeFileName.get)
      assert(cause.failedCodeLineNumber.get == thisLineNumber - 15)
      assert(cause.message == Some(FailureMessages.didNotEqual(prettifier, 1, 2)))
    }

    it("should generate NotAllowedException wrapping a non-fatal RuntimeException is thrown inside scope") {
      class TestSpec extends AnyFreeSpec {
        "a feature" - {
          throw new RuntimeException("on purpose")
        }
      }
      val e = intercept[NotAllowedException] {
        new TestSpec
      }
      assert("AnyFreeSpecSpec.scala" == e.failedCodeFileName.get)
      assert(e.failedCodeLineNumber.get == thisLineNumber - 8)
      assert(e.cause.isDefined)
      val causeThrowable = e.cause.get
      assert(e.message == Some(FailureMessages.exceptionWasThrownInDashClause(prettifier, UnquotedString(causeThrowable.getClass.getName), "a feature", "on purpose")))

      assert(causeThrowable.isInstanceOf[RuntimeException])
      val cause = causeThrowable.asInstanceOf[RuntimeException]
      assert(cause.getMessage == "on purpose")
    }

    it("should generate NotAllowedException wrapping a DuplicateTestNameException is thrown inside scope") {
      class TestSpec extends AnyFreeSpec {
        "a feature" - {
          "test 1" in {/* ASSERTION_SUCCEED */}
          "test 1" in {/* ASSERTION_SUCCEED */}
        }
      }
      val e = intercept[NotAllowedException] {
        new TestSpec
      }
      assert("AnyFreeSpecSpec.scala" == e.failedCodeFileName.get)
      assert(e.failedCodeLineNumber.get == thisLineNumber - 7)
      assert(e.cause.isDefined)
      val causeThrowable = e.cause.get
      assert(e.message == Some(FailureMessages.exceptionWasThrownInDashClause(prettifier, UnquotedString(causeThrowable.getClass.getName), "a feature", FailureMessages.duplicateTestName(prettifier, UnquotedString("a feature test 1")))))

      assert(causeThrowable.isInstanceOf[DuplicateTestNameException])
      val cause = causeThrowable.asInstanceOf[DuplicateTestNameException]
      assert(cause.getMessage == FailureMessages.duplicateTestName(prettifier, UnquotedString("a feature test 1")))
    }

    // SKIP-SCALATESTJS,NATIVE-START
    it("should propagate AnnotationFormatError when it is thrown inside scope") {
      class TestSpec extends AnyFreeSpec {
        "a feature" - {
          throw new AnnotationFormatError("on purpose")
        }
      }
      val e = intercept[AnnotationFormatError] {
        new TestSpec
      }
      assert(e.getMessage == "on purpose")
    }

    it("should propagate AWTError when it is thrown inside scope") {
      class TestSpec extends AnyFreeSpec {
        "a feature" - {
          throw new AWTError("on purpose")
        }
      }
      val e = intercept[AWTError] {
        new TestSpec
      }
      assert(e.getMessage == "on purpose")
    }

    it("should propagate CoderMalfunctionError when it is thrown inside scope") {
      class TestSpec extends AnyFreeSpec {
        "a feature" - {
          throw new CoderMalfunctionError(new RuntimeException("on purpose"))
        }
      }
      val e = intercept[CoderMalfunctionError] {
        new TestSpec
      }
      assert(e.getMessage == "java.lang.RuntimeException: on purpose")
    }

    it("should propagate FactoryConfigurationError when it is thrown inside scope") {
      class TestSpec extends AnyFreeSpec {
        "a feature" - {
          throw new FactoryConfigurationError("on purpose")
        }
      }
      val e = intercept[FactoryConfigurationError] {
        new TestSpec
      }
      assert(e.getMessage == "on purpose")
    }

    it("should propagate LinkageError when it is thrown inside scope") {
      class TestSpec extends AnyFreeSpec {
        "a feature" - {
          throw new LinkageError("on purpose")
        }
      }
      val e = intercept[LinkageError] {
        new TestSpec
      }
      assert(e.getMessage == "on purpose")
    }

    it("should propagate ThreadDeath when it is thrown inside scope") {
      class TestSpec extends AnyFreeSpec {
        "a feature" - {
          throw new ThreadDeath
        }
      }
      val e = intercept[ThreadDeath] {
        new TestSpec
      }
      assert(e.getMessage == null)
    }

    it("should propagate TransformerFactoryConfigurationError when it is thrown inside scope") {
      class TestSpec extends AnyFreeSpec {
        "a feature" - {
          throw new TransformerFactoryConfigurationError("on purpose")
        }
      }
      val e = intercept[TransformerFactoryConfigurationError] {
        new TestSpec
      }
      assert(e.getMessage == "on purpose")
    }

    it("should propagate VirtualMachineError when it is thrown inside scope") {
      class TestSpec extends AnyFreeSpec {
        "a feature" - {
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
