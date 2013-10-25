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

// elements
import org.scalatest.events._
import SharedHelpers._
/* Uncomment after remove type aliases in org.scalatest package object
import org.scalatest.exceptions.DuplicateTestNameException
import org.scalatest.exceptions.TestRegistrationClosedException
import org.scalatest.exceptions.TestFailedException
*/

class WordSpecSpec extends FunSpec with GivenWhenThen {

  describe("A WordSpec") {

    it("should invoke withFixture from runTest") {
      val a = new WordSpec {
        var withFixtureWasInvoked = false
        var testWasInvoked = false
        override def withFixture(test: NoArgTest): Outcome = {
          withFixtureWasInvoked = true
          super.withFixture(test)
        }
        "do something" in {
          testWasInvoked = true
        }
      }
      a.run(None, Args(SilentReporter))
      assert(a.withFixtureWasInvoked)
      assert(a.testWasInvoked)
    }
    it("should pass the correct test name in the NoArgTest passed to withFixture") {
      val a = new WordSpec {
        var correctTestNameWasPassed = false
        override def withFixture(test: NoArgTest): Outcome = {
          correctTestNameWasPassed = test.name == "do something"
          super.withFixture(test)
        }
        "do something" in {}
      }
      a.run(None, Args(SilentReporter))
      assert(a.correctTestNameWasPassed)
    }
    it("should pass the correct config map in the NoArgTest passed to withFixture") {
      val a = new WordSpec {
        var correctConfigMapWasPassed = false
        override def withFixture(test: NoArgTest): Outcome = {
          correctConfigMapWasPassed = (test.configMap == ConfigMap("hi" -> 7))
          super.withFixture(test)
        }
        "do something" in {}
      }
      a.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap("hi" -> 7), None, new Tracker(), Set.empty))
      assert(a.correctConfigMapWasPassed)
    }

    describe("(when a nesting rule has been violated)") {

      it("should, if they call a should from within an in clause, result in a TestFailedException when running the test") {

        class MySpec extends WordSpec {
          "should blow up" in {
            "in the wrong place, at the wrong time" should {
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceivedWithCorrectMessage(spec, "should blow up", "a \"should\" clause may not appear inside an \"in\" clause")
      }
      it("should, if they call a should with a nested in from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends WordSpec {
          "should blow up" in {
            "in the wrong place, at the wrong time" should {
              "should never run" in {
                assert(1 === 1)
              }
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceivedWithCorrectMessage(spec, "should blow up", "a \"should\" clause may not appear inside an \"in\" clause")
      }

      it("should, if they call a when from within an in clause, result in a TestFailedException when running the test") {

        class MySpec extends WordSpec {
          "should blow up" in {
            "in the wrong place, at the wrong time" when {
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceivedWithCorrectMessage(spec, "should blow up", "a \"when\" clause may not appear inside an \"in\" clause")
      }
      it("should, if they call a when with a nested in from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends WordSpec {
          "should blow up" in {
            "in the wrong place, at the wrong time" when {
              "should never run" in {
                assert(1 === 1)
              }
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceivedWithCorrectMessage(spec, "should blow up", "a \"when\" clause may not appear inside an \"in\" clause")
      }

      it("should, if they call a that from within an in clause, result in a TestFailedException when running the test") {

        class MySpec extends WordSpec {
          "should blow up" in {
            "in the wrong place, at the wrong time" that {
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceivedWithCorrectMessage(spec, "should blow up", "a \"that\" clause may not appear inside an \"in\" clause")
      }
      it("should, if they call a that with a nested in from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends WordSpec {
          "should blow up" in {
            "in the wrong place, at the wrong time" that {
              "should never run" in {
                assert(1 === 1)
              }
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceivedWithCorrectMessage(spec, "should blow up", "a \"that\" clause may not appear inside an \"in\" clause")
      }

      it("should, if they call a which from within an in clause, result in a TestFailedException when running the test") {

        class MySpec extends WordSpec {
          "should blow up" in {
            "in the wrong place, at the wrong time" which {
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceivedWithCorrectMessage(spec, "should blow up", "a \"which\" clause may not appear inside an \"in\" clause")
      }
      it("should, if they call a which with a nested in from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends WordSpec {
          "should blow up" in {
            "in the wrong place, at the wrong time" which {
              "should never run" in {
                assert(1 === 1)
              }
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceivedWithCorrectMessage(spec, "should blow up", "a \"which\" clause may not appear inside an \"in\" clause")
      }

      it("should, if they call a can from within an in clause, result in a TestFailedException when running the test") {

        class MySpec extends WordSpec {
          "should blow up" in {
            "in the wrong place, at the wrong time" can {
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceivedWithCorrectMessage(spec, "should blow up", "a \"can\" clause may not appear inside an \"in\" clause")
      }
      it("should, if they call a can with a nested in from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends WordSpec {
          "should blow up" in {
            "in the wrong place, at the wrong time" can {
              "should never run" in {
                assert(1 === 1)
              }
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceivedWithCorrectMessage(spec, "should blow up", "a \"can\" clause may not appear inside an \"in\" clause")
      }

      it("should, if they call a nested it from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends WordSpec {
          "should blow up" in {
            "should never run" in {
              assert(1 === 1)
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a nested it with tags from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends WordSpec {
          "should blow up" in {
            "should never run" taggedAs(mytags.SlowAsMolasses) in {
              assert(1 === 1)
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a describe with a nested ignore from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends WordSpec {
          "should blow up" in {
            "in the wrong place, at the wrong time" should {
              "should never run" ignore {
                assert(1 === 1)
              }
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a nested ignore from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends WordSpec {
          "should blow up" in {
            "should never run" ignore {
              assert(1 === 1)
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a nested ignore with tags from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends WordSpec {
          "should blow up" in {
            "should never run" taggedAs(mytags.SlowAsMolasses) ignore {
              assert(1 === 1)
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
    }

    it("should return the test names in registration order from testNames") {

      val a = new WordSpec {
        "it should test this" in {}
        "it should test that" in {}
      }

      assertResult(List("it should test this", "it should test that")) {
        a.testNames.iterator.toList
      }

      val b = new WordSpec {}

      assertResult(List[String]()) {
        b.testNames.iterator.toList
      }

      val c = new WordSpec {
        "it should test that" in {}
        "it should test this" in {}
      }

      assertResult(List("it should test that", "it should test this")) {
        c.testNames.iterator.toList
      }

      val d = new WordSpec {
        "A Tester" should {
          "test that" in {}
          "test this" in {}
        }
      }

      assertResult(List("A Tester should test that", "A Tester should test this")) {
        d.testNames.iterator.toList
      }

      val e = new WordSpec {
        "A Tester" should {
          "test this" in {}
          "test that" in {}
        }
      }

      assertResult(List("A Tester should test this", "A Tester should test that")) {
        e.testNames.iterator.toList
      }
    }

    it("should throw DuplicateTestNameException if a duplicate test name registration is attempted") {
      
      intercept[DuplicateTestNameException] {
        new WordSpec {
          "should test this" in {}
          "should test this" in {}
        }
      }
      intercept[DuplicateTestNameException] {
        new WordSpec {
          "should test this" in {}
          "should test this" ignore {}
        }
      }
      intercept[DuplicateTestNameException] {
        new WordSpec {
          "should test this" ignore {}
          "should test this" ignore {}
        }
      }
      intercept[DuplicateTestNameException] {
        new WordSpec {
          "should test this" ignore {}
          "should test this" in {}
        }
      }
    }

    describe("(with info calls)") {
      class InfoInsideTestSpec extends WordSpec {
        val msg = "hi there, dude"
        val testName = "test name"
        testName in {
          info(msg)
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
      class InfoBeforeTestSpec extends WordSpec {
        val msg = "hi there, dude"
        val testName = "test name"
        info(msg)
        testName in {}
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
        class MySpec extends WordSpec {
          testName in {}
          info(msg)
        }
        val (infoProvidedIndex, testStartingIndex, testSucceededIndex) =
          getIndexesForInformerEventOrderTests(new MySpec, testName, msg)
        assert(testStartingIndex < testSucceededIndex)
        assert(testSucceededIndex < infoProvidedIndex)
      }
      it("should print to stdout when info is called by a method invoked after the suite has been executed") {
        class MySpec extends WordSpec {
          callInfo() // This should work fine
          def callInfo() {
            info("howdy")
          }
          "howdy also" in {
            callInfo() // This should work fine
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
        assert(indentedText === IndentedText("+ " + spec.msg, spec.msg, 0))
      }
      it("should send an InfoProvided with an IndentedText formatter with level 2 when called within a test") {
        val spec = new InfoInsideTestSpec
        val indentedText = getIndentedTextFromTestInfoProvided(spec)
        assert(indentedText === IndentedText("  + " + spec.msg, spec.msg, 1))
      }
    }
    it("should throw NullPointerException if a null test tag is provided") {
      // it
      intercept[NullPointerException] {
        new WordSpec {
          "hi" taggedAs(null) in {}
        }
      }
      val caught = intercept[NullPointerException] {
        new WordSpec {
          "hi" taggedAs(mytags.SlowAsMolasses, null) in {}
        }
      }
      assert(caught.getMessage === "a test tag was null")
      intercept[NullPointerException] {
        new WordSpec {
          "hi" taggedAs(mytags.SlowAsMolasses, null, mytags.WeakAsAKitten) in {}
        }
      }
      // ignore
      intercept[NullPointerException] {
        new WordSpec {
          "hi" taggedAs(null) ignore {}
        }
      }
      val caught2 = intercept[NullPointerException] {
        new WordSpec {
          "hi" taggedAs(mytags.SlowAsMolasses, null) ignore {}
        }
      }
      assert(caught2.getMessage === "a test tag was null")
      intercept[NullPointerException] {
        new WordSpec {
          "hi" taggedAs(mytags.SlowAsMolasses, null, mytags.WeakAsAKitten) ignore {}
        }
      }
    }
    it("should return a correct tags map from the tags method using is (pending)") {

      val a = new WordSpec {
        "test this" ignore {}
        "test that" is (pending)
      }
      assertResult(Map("test this" -> Set("org.scalatest.Ignore"))) {
        a.tags
      }

      val b = new WordSpec {
        "test this" is (pending)
        "test that" ignore {}
      }
      assertResult(Map("test that" -> Set("org.scalatest.Ignore"))) {
        b.tags
      }

      val c = new WordSpec {
        "test this" ignore {}
        "test that" ignore {}
      }
      assertResult(Map("test this" -> Set("org.scalatest.Ignore"), "test that" -> Set("org.scalatest.Ignore"))) {
        c.tags
      }

      val d = new WordSpec {
        "test this" taggedAs(mytags.SlowAsMolasses) is (pending)
        "test that" taggedAs(mytags.SlowAsMolasses) ignore {}
      }
      assertResult(Map("test this" -> Set("org.scalatest.SlowAsMolasses"), "test that" -> Set("org.scalatest.Ignore", "org.scalatest.SlowAsMolasses"))) {
        d.tags
      }

      val e = new WordSpec {
        "test this" is (pending)
        "test that" is (pending)
      }
      assertResult(Map()) {
        e.tags
      }

      val f = new WordSpec {
        "test this" taggedAs(mytags.SlowAsMolasses, mytags.WeakAsAKitten) is (pending)
        "test that" taggedAs(mytags.SlowAsMolasses) in  {}
      }
      assertResult(Map("test this" -> Set("org.scalatest.SlowAsMolasses", "org.scalatest.WeakAsAKitten"), "test that" -> Set("org.scalatest.SlowAsMolasses"))) {
        f.tags
      }

      val g = new WordSpec {
        "test this" taggedAs(mytags.SlowAsMolasses, mytags.WeakAsAKitten) is (pending)
        "test that" taggedAs(mytags.SlowAsMolasses) in  {}
      }
      assertResult(Map("test this" -> Set("org.scalatest.SlowAsMolasses", "org.scalatest.WeakAsAKitten"), "test that" -> Set("org.scalatest.SlowAsMolasses"))) {
        g.tags
      }
    }

    class TestWasCalledSuite extends WordSpec {
      var theTestThisCalled = false
      var theTestThatCalled = false
      "run this" in { theTestThisCalled = true }
      "run that, maybe" in { theTestThatCalled = true }
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

    it("should report as ignored, and not run, tests marked ignored") {

      val a = new WordSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        "test this" in { theTestThisCalled = true }
        "test that" in { theTestThatCalled = true }
      }

      val repA = new TestIgnoredTrackingReporter
      a.run(None, Args(repA))
      assert(!repA.testIgnoredReceived)
      assert(a.theTestThisCalled)
      assert(a.theTestThatCalled)

      val b = new WordSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        "test this" ignore { theTestThisCalled = true }
        "test that" in { theTestThatCalled = true }
      }

      val repB = new TestIgnoredTrackingReporter
      b.run(None, Args(repB))
      assert(repB.testIgnoredReceived)
      assert(repB.lastEvent.isDefined)
      assert(repB.lastEvent.get.testName endsWith "test this")
      assert(!b.theTestThisCalled)
      assert(b.theTestThatCalled)

      val c = new WordSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        "test this" in { theTestThisCalled = true }
        "test that" ignore { theTestThatCalled = true }
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
      val d = new WordSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        "test this" ignore { theTestThisCalled = true }
        "test that" ignore { theTestThatCalled = true }
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
      val e = new WordSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        "test this" ignore { theTestThisCalled = true }
        "test that" in { theTestThatCalled = true }
      }

      val repE = new TestIgnoredTrackingReporter
      e.run(Some("test this"), Args(repE))
      assert(repE.testIgnoredReceived)
      assert(!e.theTestThisCalled)
      assert(!e.theTestThatCalled)
    }

    it("should run only those tests selected by the tags to include and exclude sets") {

      // Nothing is excluded
      val a = new WordSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        "test this" taggedAs(mytags.SlowAsMolasses) in { theTestThisCalled = true }
        "test that" in { theTestThatCalled = true }
      }
      val repA = new TestIgnoredTrackingReporter
      a.run(None, Args(repA))
      assert(!repA.testIgnoredReceived)
      assert(a.theTestThisCalled)
      assert(a.theTestThatCalled)

      // SlowAsMolasses is included, one test should be excluded
      val b = new WordSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        "test this" taggedAs(mytags.SlowAsMolasses) in { theTestThisCalled = true }
        "test that" in { theTestThatCalled = true }
      }
      val repB = new TestIgnoredTrackingReporter
      b.run(None, Args(repB, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repB.testIgnoredReceived)
      assert(b.theTestThisCalled)
      assert(!b.theTestThatCalled)

      // SlowAsMolasses is included, and both tests should be included
      val c = new WordSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        "test this" taggedAs(mytags.SlowAsMolasses) in { theTestThisCalled = true }
        "test that" taggedAs(mytags.SlowAsMolasses) in { theTestThatCalled = true }
      }
      val repC = new TestIgnoredTrackingReporter
      c.run(None, Args(repB, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repC.testIgnoredReceived)
      assert(c.theTestThisCalled)
      assert(c.theTestThatCalled)

      // SlowAsMolasses is included. both tests should be included but one ignored
      val d = new WordSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        "test this" taggedAs(mytags.SlowAsMolasses) ignore { theTestThisCalled = true }
        "test that" taggedAs(mytags.SlowAsMolasses) in { theTestThatCalled = true }
      }
      val repD = new TestIgnoredTrackingReporter
      d.run(None, Args(repD, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.Ignore")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(repD.testIgnoredReceived)
      assert(!d.theTestThisCalled)
      assert(d.theTestThatCalled)

      // SlowAsMolasses included, FastAsLight excluded
      val e = new WordSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        "test this" taggedAs(mytags.SlowAsMolasses, mytags.FastAsLight) in { theTestThisCalled = true }
        "test that" taggedAs(mytags.SlowAsMolasses) in { theTestThatCalled = true }
        "test the other" in { theTestTheOtherCalled = true }
      }
      val repE = new TestIgnoredTrackingReporter
      e.run(None, Args(repE, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
                ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repE.testIgnoredReceived)
      assert(!e.theTestThisCalled)
      assert(e.theTestThatCalled)
      assert(!e.theTestTheOtherCalled)

      // An Ignored test that was both included and excluded should not generate a TestIgnored event
      val f = new WordSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        "test this" taggedAs(mytags.SlowAsMolasses, mytags.FastAsLight) ignore { theTestThisCalled = true }
        "test that" taggedAs(mytags.SlowAsMolasses) in { theTestThatCalled = true }
        "test the other" in { theTestTheOtherCalled = true }
      }
      val repF = new TestIgnoredTrackingReporter
      f.run(None, Args(repF, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
                ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repF.testIgnoredReceived)
      assert(!f.theTestThisCalled)
      assert(f.theTestThatCalled)
      assert(!f.theTestTheOtherCalled)

      // An Ignored test that was not included should not generate a TestIgnored event
      val g = new WordSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        "test this" taggedAs(mytags.SlowAsMolasses, mytags.FastAsLight) in { theTestThisCalled = true }
        "test that" taggedAs(mytags.SlowAsMolasses) in { theTestThatCalled = true }
        "test the other" ignore { theTestTheOtherCalled = true }
      }
      val repG = new TestIgnoredTrackingReporter
      g.run(None, Args(repG, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
                ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repG.testIgnoredReceived)
      assert(!g.theTestThisCalled)
      assert(g.theTestThatCalled)
      assert(!g.theTestTheOtherCalled)

      // No tagsToInclude set, FastAsLight excluded
      val h = new WordSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        "test this" taggedAs(mytags.SlowAsMolasses, mytags.FastAsLight) in { theTestThisCalled = true }
        "test that" taggedAs(mytags.SlowAsMolasses) in { theTestThatCalled = true }
        "test the other" in { theTestTheOtherCalled = true }
      }
      val repH = new TestIgnoredTrackingReporter
      h.run(None, Args(repH, Stopper.default, Filter(None, Set("org.scalatest.FastAsLight")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repH.testIgnoredReceived)
      assert(!h.theTestThisCalled)
      assert(h.theTestThatCalled)
      assert(h.theTestTheOtherCalled)

      // No tagsToInclude set, mytags.SlowAsMolasses excluded
      val i = new WordSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        "test this" taggedAs(mytags.SlowAsMolasses, mytags.FastAsLight) in { theTestThisCalled = true }
        "test that" taggedAs(mytags.SlowAsMolasses) in { theTestThatCalled = true }
        "test the other" in { theTestTheOtherCalled = true }
      }
      val repI = new TestIgnoredTrackingReporter
      i.run(None, Args(repI, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repI.testIgnoredReceived)
      assert(!i.theTestThisCalled)
      assert(!i.theTestThatCalled)
      assert(i.theTestTheOtherCalled)

      // No tagsToInclude set, mytags.SlowAsMolasses excluded, TestIgnored should not be received on excluded ones
      val j = new WordSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        "test this" taggedAs(mytags.SlowAsMolasses, mytags.FastAsLight) ignore { theTestThisCalled = true }
        "test that" taggedAs(mytags.SlowAsMolasses) ignore { theTestThatCalled = true }
        "test the other" in { theTestTheOtherCalled = true }
      }
      val repJ = new TestIgnoredTrackingReporter
      j.run(None, Args(repJ, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repI.testIgnoredReceived)
      assert(!j.theTestThisCalled)
      assert(!j.theTestThatCalled)
      assert(j.theTestTheOtherCalled)

      // Same as previous, except Ignore specifically mentioned in excludes set
      val k = new WordSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        "test this" taggedAs(mytags.SlowAsMolasses, mytags.FastAsLight) ignore { theTestThisCalled = true }
        "test that" taggedAs(mytags.SlowAsMolasses) ignore { theTestThatCalled = true }
        "test the other" ignore { theTestTheOtherCalled = true }
      }
      val repK = new TestIgnoredTrackingReporter
      k.run(None, Args(repK, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses", "org.scalatest.Ignore")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(repK.testIgnoredReceived)
      assert(!k.theTestThisCalled)
      assert(!k.theTestThatCalled)
      assert(!k.theTestTheOtherCalled)
    }

    it("should return the correct test count from its expectedTestCount method") {

      val a = new WordSpec {
        "test this" in {}
        "test that" in {}
      }
      assert(a.expectedTestCount(Filter()) === 2)

      val b = new WordSpec {
        "test this" ignore {}
        "test that" in {}
      }
      assert(b.expectedTestCount(Filter()) === 1)

      val c = new WordSpec {
        "test this" taggedAs(mytags.FastAsLight) in {}
        "test that" in {}
      }
      assert(c.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 1)
      assert(c.expectedTestCount(Filter(None, Set("org.scalatest.FastAsLight"))) === 1)

      val d = new WordSpec {
        "test this" taggedAs(mytags.FastAsLight, mytags.SlowAsMolasses) in {}
        "test that" taggedAs(mytags.SlowAsMolasses) in {}
        "test the other thing" in {}
      }
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 1)
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight"))) === 1)
      assert(d.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) === 1)
      assert(d.expectedTestCount(Filter()) === 3)

      val e = new WordSpec {
        "test this" taggedAs(mytags.FastAsLight, mytags.SlowAsMolasses) in {}
        "test that" taggedAs(mytags.SlowAsMolasses) in {}
        "test the other thing" ignore {}
      }
      assert(e.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 1)
      assert(e.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight"))) === 1)
      assert(e.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) === 0)
      assert(e.expectedTestCount(Filter()) === 2)

      val f = new Suites(a, b, c, d, e)
      assert(f.expectedTestCount(Filter()) === 10)
    }
    it("should generate a TestPending message when the test body is (pending)") {
      val a = new WordSpec {

        "should do this" is (pending)

        "should do that" in {
          assert(2 + 2 === 4)
        }
        "should do something else" in {
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
      val a = new WordSpec {
        "This WordSpec" should {
          "throw AssertionError" in { throw new AssertionError }
          "throw plain old Error" in { throw new Error }
          "throw Throwable" in { throw new Throwable }
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      val tf = rep.testFailedEventsReceived
      assert(tf.size === 3)
    }
    it("should propagate out Errors that are direct subtypes of Error in JDK 1.5, other than " +
            "AssertionError, causing Suites and Runs to abort.") {
      val a = new WordSpec {
        "This WordSpec" should {
          "throw AssertionError" in { throw new OutOfMemoryError }
        }
      }
      intercept[OutOfMemoryError] {
        a.run(None, Args(SilentReporter))
      }
    }
/*
    it("should send InfoProvided events with aboutAPendingTest set to true for info " +
            "calls made from a test that is pending") {
      val a = new WordSpec with GivenWhenThen {
        "A WordSpec" should {
          "do something" in {
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
      assert(testPending.size === 1)
      val recordedEvents = testPending(0).recordedEvents
      assert(recordedEvents.size === 3)
      for (event <- recordedEvents) {
        val ip = event.asInstanceOf[InfoProvided]
        assert(ip.aboutAPendingTest.isDefined && ip.aboutAPendingTest.get)
      }
      val so = rep.scopeOpenedEventsReceived
      assert(so.size === 1)
      for (event <- so) {
        assert(event.message == "A WordSpec")
      }
      val sc = rep.scopeClosedEventsReceived
      assert(so.size === 1)
      for (event <- sc) {
        assert(event.message == "A WordSpec")
      }
    }
    it("should send InfoProvided events with aboutAPendingTest set to false for info " +
            "calls made from a test that is not pending") {
      val a = new WordSpec with GivenWhenThen {
        "A WordSpec" should {
          "do something" in {
            given("two integers")
            when("one is subracted from the other")
            then("the result is the difference between the two numbers")
            assert(1 + 1 === 2)
          }
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      val testSucceeded = rep.testSucceededEventsReceived
      assert(testSucceeded.size === 1)
      val recordedEvents = testSucceeded(0).recordedEvents
      assert(recordedEvents.size === 3)
      for (event <- recordedEvents) {
        val ip = event.asInstanceOf[InfoProvided]
        assert(ip.aboutAPendingTest.isDefined && !ip.aboutAPendingTest.get)
      }
      val so = rep.scopeOpenedEventsReceived
      assert(so.size === 1)
      for (event <- so) {
        assert(event.message == "A WordSpec")
      }
      val sc = rep.scopeClosedEventsReceived
      assert(so.size === 1)
      for (event <- sc) {
        assert(event.message == "A WordSpec")
      }
    }
*/
    it("should not put parentheses around should clauses that follow when") {
      val a = new WordSpec {
        "A Stack" when {
          "empty" should {
            "chill out" in {
              assert(1 + 1 === 2)
            }
          }
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      val ts = rep.testSucceededEventsReceived
      assert(ts.size === 1)
      assert(ts.head.testName === "A Stack when empty should chill out")
    }
    it("should not put parentheses around should clauses that don't follow when") {
      val a = new WordSpec {
        "A Stack" should {
          "chill out" in {
            assert(1 + 1 === 2)
          }
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      val ts = rep.testSucceededEventsReceived
      assert(ts.size === 1)
      assert(ts.head.testName === "A Stack should chill out")
    }
    
    it("should contains correct formatter for TestStarting, TestSucceeded, TestFailed, TestPending, TestCanceled and TestIgnored") {
      class TestSpec extends WordSpec {
        "a feature" should {
          "succeeded here" in {}
          "failed here" in { fail }
          "pending here" in { pending }
          "cancel here" in { cancel }
          "ignore here" ignore {}
        }
      }
      val rep = new EventRecordingReporter
      val s = new TestSpec
      s.run(None, Args(rep))
      val testStartingList = rep.testStartingEventsReceived
      assert(testStartingList.size === 4)
      assert(testStartingList(0).formatter === Some(MotionToSuppress), "Expected testStartingList(0).formatter to be Some(MotionToSuppress), but got: " + testStartingList(0).formatter.getClass.getName)
      assert(testStartingList(1).formatter === Some(MotionToSuppress), "Expected testStartingList(1).formatter to be Some(MotionToSuppress), but got: " + testStartingList(1).formatter.getClass.getName)
      assert(testStartingList(2).formatter === Some(MotionToSuppress), "Expected testStartingList(2).formatter to be Some(MotionToSuppress), but got: " + testStartingList(2).formatter.getClass.getName)
      assert(testStartingList(3).formatter === Some(MotionToSuppress), "Expected testStartingList(3).formatter to be Some(MotionToSuppress), but got: " + testStartingList(3).formatter.getClass.getName)
      
      val testSucceededList = rep.testSucceededEventsReceived
      assert(testSucceededList.size === 1)
      assert(testSucceededList(0).formatter.isDefined, "Expected testSucceededList(0).formatter to be defined, but it is not.")
      assert(testSucceededList(0).formatter.get.isInstanceOf[IndentedText], "Expected testSucceededList(0).formatter to be Some(IndentedText), but got: " + testSucceededList(0).formatter)
      val testSucceededFormatter = testSucceededList(0).formatter.get.asInstanceOf[IndentedText]
      assert(testSucceededFormatter.formattedText === "- should succeeded here")
      assert(testSucceededFormatter.rawText === "should succeeded here")
      
      val testFailedList = rep.testFailedEventsReceived
      assert(testFailedList.size === 1)
      assert(testFailedList(0).formatter.isDefined, "Expected testFailedList(0).formatter to be defined, but it is not.")
      assert(testFailedList(0).formatter.get.isInstanceOf[IndentedText], "Expected testFailedList(0).formatter to be Some(IndentedText), but got: " + testSucceededList(0).formatter)
      val testFailedFormatter = testFailedList(0).formatter.get.asInstanceOf[IndentedText]
      assert(testFailedFormatter.formattedText === "- should failed here")
      assert(testFailedFormatter.rawText === "should failed here")
      
      val testPendingList = rep.testPendingEventsReceived
      assert(testPendingList.size === 1)
      assert(testPendingList(0).formatter.isDefined, "Expected testPendingList(0).formatter to be defined, but it is not.")
      assert(testPendingList(0).formatter.get.isInstanceOf[IndentedText], "Expected testPendingList(0).formatter to be Some(IndentedText), but got: " + testSucceededList(0).formatter)
      val testPendingFormatter = testPendingList(0).formatter.get.asInstanceOf[IndentedText]
      assert(testPendingFormatter.formattedText === "- should pending here")
      assert(testPendingFormatter.rawText === "should pending here")
      
      val testCanceledList = rep.testCanceledEventsReceived
      assert(testCanceledList.size === 1)
      assert(testCanceledList(0).formatter.isDefined, "Expected testCanceledList(0).formatter to be defined, but it is not.")
      assert(testCanceledList(0).formatter.get.isInstanceOf[IndentedText], "Expected testCanceledList(0).formatter to be Some(IndentedText), but got: " + testSucceededList(0).formatter)
      val testCanceledFormatter = testCanceledList(0).formatter.get.asInstanceOf[IndentedText]
      assert(testCanceledFormatter.formattedText === "- should cancel here")
      assert(testCanceledFormatter.rawText === "should cancel here")
      
      val testIgnoredList = rep.testIgnoredEventsReceived
      assert(testIgnoredList.size === 1)
      assert(testIgnoredList(0).formatter.isDefined, "Expected testIgnoredList(0).formatter to be defined, but it is not.")
      assert(testIgnoredList(0).formatter.get.isInstanceOf[IndentedText], "Expected testIgnoredList(0).formatter to be Some(IndentedText), but got: " + testSucceededList(0).formatter)
      val testIgnoredFormatter = testIgnoredList(0).formatter.get.asInstanceOf[IndentedText]
      assert(testIgnoredFormatter.formattedText === "- should ignore here")
      assert(testIgnoredFormatter.rawText === "should ignore here")
    }
  }
  
  describe("when failure happens") {
    
    it("should fire TestFailed event with correct stack depth info when test failed") {
      class TestSpec extends WordSpec {
        "A Stack" should {
          "chill out" in {
            assert(1 === 2)
          }
        }
      }
      val rep = new EventRecordingReporter
      val s1 = new TestSpec
      s1.run(None, Args(rep))
      assert(rep.testFailedEventsReceived.size === 1)
      assert(rep.testFailedEventsReceived(0).throwable.get.asInstanceOf[TestFailedException].failedCodeFileName.get === "WordSpecSpec.scala")
      assert(rep.testFailedEventsReceived(0).throwable.get.asInstanceOf[TestFailedException].failedCodeLineNumber.get === thisLineNumber - 9)
    }
    
    it("should generate TestRegistrationClosedException with correct stack depth info when has an in nested inside an in") {
      class TestSpec extends WordSpec {
        var registrationClosedThrown = false
        "a feature" should {
          "a scenario" in {
            "nested scenario" in {
              
            }
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
      assert("WordSpecSpec.scala" === trce.failedCodeFileName.get)
      assert(trce.failedCodeLineNumber.get === thisLineNumber - 24)
    }
  }
  
  describe("shorthand syntax") {
    
    describe("'it'") {
      
      describe("under top level") {
        
        it("should work with subject") {
          class TestSpec extends WordSpec { 

            "A Stack" when { 
              "empty" should { 
                "be empty" in {} 
              } 
            } 

            it should { 
              "do something interesting 1" in {} 
            }
        
            it can {
              "do something interesting 2" in {}
            }
        
            it must {
              "do something interesting 3" in {}
            }
        
            it when {
              "do something interesting 4" in {}
            }
          }
      
          val rep = new EventRecordingReporter
          val s = new TestSpec
          s.run(None, Args(rep))
      
          val testStartingList = rep.testStartingEventsReceived
          assert(testStartingList.size === 5)
          assert(testStartingList(0).testName === "A Stack when empty should be empty")
          assert(testStartingList(1).testName === "A Stack should do something interesting 1")
          assert(testStartingList(2).testName === "A Stack can do something interesting 2")
          assert(testStartingList(3).testName === "A Stack must do something interesting 3")
          assert(testStartingList(4).testName === "A Stack when do something interesting 4")
      
          val testSucceededList = rep.testSucceededEventsReceived
          assert(testSucceededList.size === 5)
          assert(testSucceededList(0).testName === "A Stack when empty should be empty")
          assert(testSucceededList(1).testName === "A Stack should do something interesting 1")
          assert(testSucceededList(2).testName === "A Stack can do something interesting 2")
          assert(testSucceededList(3).testName === "A Stack must do something interesting 3")
          assert(testSucceededList(4).testName === "A Stack when do something interesting 4")
        }
    
        it("should throw NotAllowedException with correct stack depth and message when 'it should' is called without subject") {
          class TestSpec extends WordSpec { 
            it should {
              "do something interesting" in {}
            }
          }
          val e = intercept[exceptions.NotAllowedException] {
            new TestSpec
          }
          assert(e.getMessage === "An it clause must only appear after a top level subject clause.")
          assert(e.failedCodeFileName === Some("WordSpecSpec.scala"))
          assert(e.failedCodeLineNumber === Some(thisLineNumber - 9))
        }
        
        it("should throw NotAllowedException with correct stack depth and message when 'it can' is called without subject") {
          class TestSpec extends WordSpec { 
            it can {
              "do something interesting" in {}
            }
          }
          val e = intercept[exceptions.NotAllowedException] {
            new TestSpec
          }
          assert(e.getMessage === "An it clause must only appear after a top level subject clause.")
          assert(e.failedCodeFileName === Some("WordSpecSpec.scala"))
          assert(e.failedCodeLineNumber === Some(thisLineNumber - 9))
        }
        
        it("should throw NotAllowedException with correct stack depth and message when 'it must' is called without subject") {
          class TestSpec extends WordSpec { 
            it must {
              "do something interesting" in {}
            }
          }
          val e = intercept[exceptions.NotAllowedException] {
            new TestSpec
          }
          assert(e.getMessage === "An it clause must only appear after a top level subject clause.")
          assert(e.failedCodeFileName === Some("WordSpecSpec.scala"))
          assert(e.failedCodeLineNumber === Some(thisLineNumber - 9))
        }
        
        it("should throw NotAllowedException with correct stack depth and message when 'it when' is called without subject") {
          class TestSpec extends WordSpec { 
            it when {
              "do something interesting" in {}
            }
          }
          val e = intercept[exceptions.NotAllowedException] {
            new TestSpec
          }
          assert(e.getMessage === "An it clause must only appear after a top level subject clause.")
          assert(e.failedCodeFileName === Some("WordSpecSpec.scala"))
          assert(e.failedCodeLineNumber === Some(thisLineNumber - 9))
        }
        
        it("should throw NotAllowedException with correct stack depth and message when 'it should' is called after an 'in' clause") {
          class TestSpec extends WordSpec { 

            "A Stack" when { 
              "empty" should { 
                "be empty" in {} 
              } 
            }
            
            "Other do something special" in {}
            
            it should {
              "do something interesting" in {}
            }
          }
          val e = intercept[exceptions.NotAllowedException] {
            new TestSpec
          }
          assert(e.getMessage === "An it clause must only appear after a top level subject clause.")
          assert(e.failedCodeFileName === Some("WordSpecSpec.scala"))
          assert(e.failedCodeLineNumber === Some(thisLineNumber - 9))
        }
        
        it("should throw NotAllowedException with correct stack depth and message when 'it can' is called after an 'in' clause") {
          class TestSpec extends WordSpec { 

            "A Stack" when { 
              "empty" should { 
                "be empty" in {} 
              } 
            }
            
            "Other do something special" in {}
            
            it can {
              "do something interesting" in {}
            }
          }
          val e = intercept[exceptions.NotAllowedException] {
            new TestSpec
          }
          assert(e.getMessage === "An it clause must only appear after a top level subject clause.")
          assert(e.failedCodeFileName === Some("WordSpecSpec.scala"))
          assert(e.failedCodeLineNumber === Some(thisLineNumber - 9))
        }
        
        it("should throw NotAllowedException with correct stack depth and message when 'it must' is called after an 'in' clause") {
          class TestSpec extends WordSpec { 

            "A Stack" when { 
              "empty" should { 
                "be empty" in {} 
              } 
            }
            
            "Other do something special" in {}
            
            it must {
              "do something interesting" in {}
            }
          }
          val e = intercept[exceptions.NotAllowedException] {
            new TestSpec
          }
          assert(e.getMessage === "An it clause must only appear after a top level subject clause.")
          assert(e.failedCodeFileName === Some("WordSpecSpec.scala"))
          assert(e.failedCodeLineNumber === Some(thisLineNumber - 9))
        }
        
        it("should throw NotAllowedException with correct stack depth and message when 'it when' is called after an 'in' clause") {
          class TestSpec extends WordSpec { 

            "A Stack" when { 
              "empty" should { 
                "be empty" in {} 
              } 
            }
            
            "Other do something special" in {}
            
            it when {
              "do something interesting" in {}
            }
          }
          val e = intercept[exceptions.NotAllowedException] {
            new TestSpec
          }
          assert(e.getMessage === "An it clause must only appear after a top level subject clause.")
          assert(e.failedCodeFileName === Some("WordSpecSpec.scala"))
          assert(e.failedCodeLineNumber === Some(thisLineNumber - 9))
        }
      }
      
      describe("under inner level") {
        
        it("should throw NotAllowedException with correct stack depth and message when 'it should' is called with inner branch") {
          class TestSpec extends WordSpec { 
            "A Stack" when { 
              "empty" should { 
                "be empty" in {} 
              }
              it should {
                "do something interesting" in {}
              }
            }
          }
          val e = intercept[exceptions.NotAllowedException] {
            new TestSpec
          }
          assert(e.getMessage === "An it clause must only appear after a top level subject clause.")
          assert(e.failedCodeFileName === Some("WordSpecSpec.scala"))
          assert(e.failedCodeLineNumber === Some(thisLineNumber - 10))
        }
        
        it("should throw NotAllowedException with correct stack depth and message when 'it can' is called with inner branch") {
          class TestSpec extends WordSpec { 
            "A Stack" when { 
              "empty" should { 
                "be empty" in {} 
              }
              it can {
                "do something interesting" in {}
              }
            }
          }
          val e = intercept[exceptions.NotAllowedException] {
            new TestSpec
          }
          assert(e.getMessage === "An it clause must only appear after a top level subject clause.")
          assert(e.failedCodeFileName === Some("WordSpecSpec.scala"))
          assert(e.failedCodeLineNumber === Some(thisLineNumber - 10))
        }
        
        it("should throw NotAllowedException with correct stack depth and message when 'it must' is called with inner branch") {
          class TestSpec extends WordSpec { 
            "A Stack" when { 
              "empty" should { 
                "be empty" in {} 
              }
              it must {
                "do something interesting" in {}
              }
            }
          }
          val e = intercept[exceptions.NotAllowedException] {
            new TestSpec
          }
          assert(e.getMessage === "An it clause must only appear after a top level subject clause.")
          assert(e.failedCodeFileName === Some("WordSpecSpec.scala"))
          assert(e.failedCodeLineNumber === Some(thisLineNumber - 10))
        }
        
        it("should throw NotAllowedException with correct stack depth and message when 'it when' is called with inner branch") {
          class TestSpec extends WordSpec { 
            "A Stack" when { 
              "empty" should { 
                "be empty" in {} 
              }
              it when {
                "do something interesting" in {}
              }
            }
          }
          val e = intercept[exceptions.NotAllowedException] {
            new TestSpec
          }
          assert(e.getMessage === "An it clause must only appear after a top level subject clause.")
          assert(e.failedCodeFileName === Some("WordSpecSpec.scala"))
          assert(e.failedCodeLineNumber === Some(thisLineNumber - 10))
        }
        
        it("should throw NotAllowedException with correct stack depth and message when 'it should' is called without inner branch") {
          class TestSpec extends WordSpec { 
            "A Stack" when { 
              it should {
                "do something interesting" in {}
              }
            }
          }
          val e = intercept[exceptions.NotAllowedException] {
            new TestSpec
          }
          assert(e.getMessage === "An it clause must only appear after a top level subject clause.")
          assert(e.failedCodeFileName === Some("WordSpecSpec.scala"))
          assert(e.failedCodeLineNumber === Some(thisLineNumber - 10))
        }
        
        it("should throw NotAllowedException with correct stack depth and message when 'it can' is called without inner branch") {
          class TestSpec extends WordSpec { 
            "A Stack" when { 
              it can {
                "do something interesting" in {}
              }
            }
          }
          val e = intercept[exceptions.NotAllowedException] {
            new TestSpec
          }
          assert(e.getMessage === "An it clause must only appear after a top level subject clause.")
          assert(e.failedCodeFileName === Some("WordSpecSpec.scala"))
          assert(e.failedCodeLineNumber === Some(thisLineNumber - 10))
        }
        
        it("should throw NotAllowedException with correct stack depth and message when 'it must' is called without inner branch") {
          class TestSpec extends WordSpec { 
            "A Stack" when { 
              it must {
                "do something interesting" in {}
              }
            }
          }
          val e = intercept[exceptions.NotAllowedException] {
            new TestSpec
          }
          assert(e.getMessage === "An it clause must only appear after a top level subject clause.")
          assert(e.failedCodeFileName === Some("WordSpecSpec.scala"))
          assert(e.failedCodeLineNumber === Some(thisLineNumber - 10))
        }
        
        it("should throw NotAllowedException with correct stack depth and message when 'it when' is called without inner branch") {
          class TestSpec extends WordSpec { 
            "A Stack" when { 
              it when {
                "do something interesting" in {}
              }
            }
          }
          val e = intercept[exceptions.NotAllowedException] {
            new TestSpec
          }
          assert(e.getMessage === "An it clause must only appear after a top level subject clause.")
          assert(e.failedCodeFileName === Some("WordSpecSpec.scala"))
          assert(e.failedCodeLineNumber === Some(thisLineNumber - 10))
        }
        
        it("should throw NotAllowedException with correct stack depth and message when 'it should' is called with inner branch but after an 'in' clause") {
          class TestSpec extends WordSpec { 
            "A Stack" when { 
              "empty" should { 
                "be empty" in {} 
              }
              "do something" in {}
              it should {
                "do something interesting" in {}
              }
            }
          }
          val e = intercept[exceptions.NotAllowedException] {
            new TestSpec
          }
          assert(e.getMessage === "An it clause must only appear after a top level subject clause.")
          assert(e.failedCodeFileName === Some("WordSpecSpec.scala"))
          assert(e.failedCodeLineNumber === Some(thisLineNumber - 10))
        }
        
        it("should throw NotAllowedException with correct stack depth and message when 'it can' is called with inner branch but after an 'in' clause") {
          class TestSpec extends WordSpec { 
            "A Stack" when { 
              "empty" should { 
                "be empty" in {} 
              }
              "do something" in {}
              it can {
                "do something interesting" in {}
              }
            }
          }
          val e = intercept[exceptions.NotAllowedException] {
            new TestSpec
          }
          assert(e.getMessage === "An it clause must only appear after a top level subject clause.")
          assert(e.failedCodeFileName === Some("WordSpecSpec.scala"))
          assert(e.failedCodeLineNumber === Some(thisLineNumber - 10))
        }
        
        it("should throw NotAllowedException with correct stack depth and message when 'it must' is called with inner branch but after an 'in' clause") {
          class TestSpec extends WordSpec { 
            "A Stack" when { 
              "empty" should { 
                "be empty" in {} 
              }
              "do something" in {}
              it must {
                "do something interesting" in {}
              }
            }
          }
          val e = intercept[exceptions.NotAllowedException] {
            new TestSpec
          }
          assert(e.getMessage === "An it clause must only appear after a top level subject clause.")
          assert(e.failedCodeFileName === Some("WordSpecSpec.scala"))
          assert(e.failedCodeLineNumber === Some(thisLineNumber - 10))
        }
        
        it("should throw NotAllowedException with correct stack depth and message when 'it when' is called with inner branch but after an 'in' clause") {
          class TestSpec extends WordSpec { 
            "A Stack" when { 
              "empty" should { 
                "be empty" in {} 
              }
              "do something" in {}
              it when {
                "do something interesting" in {}
              }
            }
          }
          val e = intercept[exceptions.NotAllowedException] {
            new TestSpec
          }
          assert(e.getMessage === "An it clause must only appear after a top level subject clause.")
          assert(e.failedCodeFileName === Some("WordSpecSpec.scala"))
          assert(e.failedCodeLineNumber === Some(thisLineNumber - 10))
        }
      }
      
      describe("under 'in' clause") {
        
        it("should throw NotAllowedException with correct stack depth and message when 'it should' is called") {
          class TestSpec extends WordSpec { 
            var notAllowedThrown = false
            "Something special" in {
              it should {
                "do something interesting" in {}
              }
            }
            override def withFixture(test: NoArgTest): Outcome = {
              val outcome = test.apply()
              outcome match {
                case Exceptional(ex: exceptions.NotAllowedException) => 
                  notAllowedThrown = true
                case _ =>
              }
              outcome
            }
          }
          val rep = new EventRecordingReporter
          val s = new TestSpec
          s.run(None, Args(rep))
          assert(s.notAllowedThrown == true)
          val testFailedEvents = rep.testFailedEventsReceived
          assert(testFailedEvents.size === 1)
          assert(testFailedEvents(0).throwable.get.getClass() === classOf[exceptions.NotAllowedException])
          val trce = testFailedEvents(0).throwable.get.asInstanceOf[exceptions.NotAllowedException]
          assert("WordSpecSpec.scala" === trce.failedCodeFileName.get)
          assert(trce.failedCodeLineNumber.get === thisLineNumber - 23)
          assert(trce.getMessage === "An it clause must only appear after a top level subject clause.")
        }
        
        it("should throw NotAllowedException with correct stack depth and message when 'it can' is called") {
          class TestSpec extends WordSpec { 
            var notAllowedThrown = false
            "Something special" in {
              it can {
                "do something interesting" in {}
              }
            }
            override def withFixture(test: NoArgTest): Outcome = {
              val outcome = test.apply()
              outcome match {
                case Exceptional(ex: exceptions.NotAllowedException) => 
                  notAllowedThrown = true
                case _ =>
              }
              outcome
            }
          }
          val rep = new EventRecordingReporter
          val s = new TestSpec
          s.run(None, Args(rep))
          assert(s.notAllowedThrown == true)
          val testFailedEvents = rep.testFailedEventsReceived
          assert(testFailedEvents.size === 1)
          assert(testFailedEvents(0).throwable.get.getClass() === classOf[exceptions.NotAllowedException])
          val trce = testFailedEvents(0).throwable.get.asInstanceOf[exceptions.NotAllowedException]
          assert("WordSpecSpec.scala" === trce.failedCodeFileName.get)
          assert(trce.failedCodeLineNumber.get === thisLineNumber - 23)
          assert(trce.getMessage === "An it clause must only appear after a top level subject clause.")
        }
        
        it("should throw NotAllowedException with correct stack depth and message when 'it must' is called") {
          class TestSpec extends WordSpec { 
            var notAllowedThrown = false
            "Something special" in {
              it must {
                "do something interesting" in {}
              }
            }
            override def withFixture(test: NoArgTest): Outcome = {
              val outcome = test.apply()
              outcome match {
                case Exceptional(ex: exceptions.NotAllowedException) => 
                  notAllowedThrown = true
                case _ =>
              }
              outcome
            }
          }
          val rep = new EventRecordingReporter
          val s = new TestSpec
          s.run(None, Args(rep))
          assert(s.notAllowedThrown == true)
          val testFailedEvents = rep.testFailedEventsReceived
          assert(testFailedEvents.size === 1)
          assert(testFailedEvents(0).throwable.get.getClass() === classOf[exceptions.NotAllowedException])
          val trce = testFailedEvents(0).throwable.get.asInstanceOf[exceptions.NotAllowedException]
          assert("WordSpecSpec.scala" === trce.failedCodeFileName.get)
          assert(trce.failedCodeLineNumber.get === thisLineNumber - 23)
          assert(trce.getMessage === "An it clause must only appear after a top level subject clause.")
        }
        
        it("should throw NotAllowedException with correct stack depth and message when 'it when' is called") {
          class TestSpec extends WordSpec { 
            var notAllowedThrown = false
            "Something special" in {
              it when {
                "do something interesting" in {}
              }
            }
            override def withFixture(test: NoArgTest): Outcome = {
              val outcome = test.apply()
              outcome match {
                case Exceptional(ex: exceptions.NotAllowedException) => 
                  notAllowedThrown = true
                case _ =>
              }
              outcome
            }
          }
          val rep = new EventRecordingReporter
          val s = new TestSpec
          s.run(None, Args(rep))
          assert(s.notAllowedThrown == true)
          val testFailedEvents = rep.testFailedEventsReceived
          assert(testFailedEvents.size === 1)
          assert(testFailedEvents(0).throwable.get.getClass() === classOf[exceptions.NotAllowedException])
          val trce = testFailedEvents(0).throwable.get.asInstanceOf[exceptions.NotAllowedException]
          assert("WordSpecSpec.scala" === trce.failedCodeFileName.get)
          assert(trce.failedCodeLineNumber.get === thisLineNumber - 23)
          assert(trce.getMessage === "An it clause must only appear after a top level subject clause.")
        }
        
      }
    }
    
    describe("'they'") {
      
      describe("under top level") {
        
        it("should work with subject") {
          class TestSpec extends WordSpec { 

            "A Stack" when { 
              "empty" should { 
                "be empty" in {} 
              } 
            } 

            they should { 
              "do something interesting 1" in {} 
            }
        
            they can {
              "do something interesting 2" in {}
            }
        
            they must {
              "do something interesting 3" in {}
            }
        
            they when {
              "do something interesting 4" in {}
            }
          }
      
          val rep = new EventRecordingReporter
          val s = new TestSpec
          s.run(None, Args(rep))
      
          val testStartingList = rep.testStartingEventsReceived
          assert(testStartingList.size === 5)
          assert(testStartingList(0).testName === "A Stack when empty should be empty")
          assert(testStartingList(1).testName === "A Stack should do something interesting 1")
          assert(testStartingList(2).testName === "A Stack can do something interesting 2")
          assert(testStartingList(3).testName === "A Stack must do something interesting 3")
          assert(testStartingList(4).testName === "A Stack when do something interesting 4")
      
          val testSucceededList = rep.testSucceededEventsReceived
          assert(testSucceededList.size === 5)
          assert(testSucceededList(0).testName === "A Stack when empty should be empty")
          assert(testSucceededList(1).testName === "A Stack should do something interesting 1")
          assert(testSucceededList(2).testName === "A Stack can do something interesting 2")
          assert(testSucceededList(3).testName === "A Stack must do something interesting 3")
          assert(testSucceededList(4).testName === "A Stack when do something interesting 4")
        }
    
        it("should throw NotAllowedException with correct stack depth and message when 'they should' is called without subject") {
          class TestSpec extends WordSpec { 
            they should {
              "do something interesting" in {}
            }
          }
          val e = intercept[exceptions.NotAllowedException] {
            new TestSpec
          }
          assert(e.getMessage === "A they clause must only appear after a top level subject clause.")
          assert(e.failedCodeFileName === Some("WordSpecSpec.scala"))
          assert(e.failedCodeLineNumber === Some(thisLineNumber - 9))
        }
        
        it("should throw NotAllowedException with correct stack depth and message when 'they can' is called without subject") {
          class TestSpec extends WordSpec { 
            they can {
              "do something interesting" in {}
            }
          }
          val e = intercept[exceptions.NotAllowedException] {
            new TestSpec
          }
          assert(e.getMessage === "A they clause must only appear after a top level subject clause.")
          assert(e.failedCodeFileName === Some("WordSpecSpec.scala"))
          assert(e.failedCodeLineNumber === Some(thisLineNumber - 9))
        }
        
        it("should throw NotAllowedException with correct stack depth and message when 'they must' is called without subject") {
          class TestSpec extends WordSpec { 
            they must {
              "do something interesting" in {}
            }
          }
          val e = intercept[exceptions.NotAllowedException] {
            new TestSpec
          }
          assert(e.getMessage === "A they clause must only appear after a top level subject clause.")
          assert(e.failedCodeFileName === Some("WordSpecSpec.scala"))
          assert(e.failedCodeLineNumber === Some(thisLineNumber - 9))
        }
        
        it("should throw NotAllowedException with correct stack depth and message when 'they when' is called without subject") {
          class TestSpec extends WordSpec { 
            they when {
              "do something interesting" in {}
            }
          }
          val e = intercept[exceptions.NotAllowedException] {
            new TestSpec
          }
          assert(e.getMessage === "A they clause must only appear after a top level subject clause.")
          assert(e.failedCodeFileName === Some("WordSpecSpec.scala"))
          assert(e.failedCodeLineNumber === Some(thisLineNumber - 9))
        }
        
        it("should throw NotAllowedException with correct stack depth and message when 'they should' is called after an 'in' clause") {
          class TestSpec extends WordSpec { 

            "A Stack" when { 
              "empty" should { 
                "be empty" in {} 
              } 
            }
            
            "Other do something special" in {}
            
            they should {
              "do something interesting" in {}
            }
          }
          val e = intercept[exceptions.NotAllowedException] {
            new TestSpec
          }
          assert(e.getMessage === "A they clause must only appear after a top level subject clause.")
          assert(e.failedCodeFileName === Some("WordSpecSpec.scala"))
          assert(e.failedCodeLineNumber === Some(thisLineNumber - 9))
        }
        
        it("should throw NotAllowedException with correct stack depth and message when 'they can' is called after an 'in' clause") {
          class TestSpec extends WordSpec { 

            "A Stack" when { 
              "empty" should { 
                "be empty" in {} 
              } 
            }
            
            "Other do something special" in {}
            
            they can {
              "do something interesting" in {}
            }
          }
          val e = intercept[exceptions.NotAllowedException] {
            new TestSpec
          }
          assert(e.getMessage === "A they clause must only appear after a top level subject clause.")
          assert(e.failedCodeFileName === Some("WordSpecSpec.scala"))
          assert(e.failedCodeLineNumber === Some(thisLineNumber - 9))
        }
        
        it("should throw NotAllowedException with correct stack depth and message when 'they must' is called after an 'in' clause") {
          class TestSpec extends WordSpec { 

            "A Stack" when { 
              "empty" should { 
                "be empty" in {} 
              } 
            }
            
            "Other do something special" in {}
            
            they must {
              "do something interesting" in {}
            }
          }
          val e = intercept[exceptions.NotAllowedException] {
            new TestSpec
          }
          assert(e.getMessage === "A they clause must only appear after a top level subject clause.")
          assert(e.failedCodeFileName === Some("WordSpecSpec.scala"))
          assert(e.failedCodeLineNumber === Some(thisLineNumber - 9))
        }
        
        it("should throw NotAllowedException with correct stack depth and message when 'they when' is called after an 'in' clause") {
          class TestSpec extends WordSpec { 

            "A Stack" when { 
              "empty" should { 
                "be empty" in {} 
              } 
            }
            
            "Other do something special" in {}
            
            they when {
              "do something interesting" in {}
            }
          }
          val e = intercept[exceptions.NotAllowedException] {
            new TestSpec
          }
          assert(e.getMessage === "A they clause must only appear after a top level subject clause.")
          assert(e.failedCodeFileName === Some("WordSpecSpec.scala"))
          assert(e.failedCodeLineNumber === Some(thisLineNumber - 9))
        }
      }
      
      describe("under inner level") {
        
        it("should throw NotAllowedException with correct stack depth and message when 'they should' is called with inner branch") {
          class TestSpec extends WordSpec { 
            "A Stack" when { 
              "empty" should { 
                "be empty" in {} 
              }
              they should {
                "do something interesting" in {}
              }
            }
          }
          val e = intercept[exceptions.NotAllowedException] {
            new TestSpec
          }
          assert(e.getMessage === "A they clause must only appear after a top level subject clause.")
          assert(e.failedCodeFileName === Some("WordSpecSpec.scala"))
          assert(e.failedCodeLineNumber === Some(thisLineNumber - 10))
        }
        
        it("should throw NotAllowedException with correct stack depth and message when 'they can' is called with inner branch") {
          class TestSpec extends WordSpec { 
            "A Stack" when { 
              "empty" should { 
                "be empty" in {} 
              }
              they can {
                "do something interesting" in {}
              }
            }
          }
          val e = intercept[exceptions.NotAllowedException] {
            new TestSpec
          }
          assert(e.getMessage === "A they clause must only appear after a top level subject clause.")
          assert(e.failedCodeFileName === Some("WordSpecSpec.scala"))
          assert(e.failedCodeLineNumber === Some(thisLineNumber - 10))
        }
        
        it("should throw NotAllowedException with correct stack depth and message when 'they must' is called with inner branch") {
          class TestSpec extends WordSpec { 
            "A Stack" when { 
              "empty" should { 
                "be empty" in {} 
              }
              they must {
                "do something interesting" in {}
              }
            }
          }
          val e = intercept[exceptions.NotAllowedException] {
            new TestSpec
          }
          assert(e.getMessage === "A they clause must only appear after a top level subject clause.")
          assert(e.failedCodeFileName === Some("WordSpecSpec.scala"))
          assert(e.failedCodeLineNumber === Some(thisLineNumber - 10))
        }
        
        it("should throw NotAllowedException with correct stack depth and message when 'they when' is called with inner branch") {
          class TestSpec extends WordSpec { 
            "A Stack" when { 
              "empty" should { 
                "be empty" in {} 
              }
              they when {
                "do something interesting" in {}
              }
            }
          }
          val e = intercept[exceptions.NotAllowedException] {
            new TestSpec
          }
          assert(e.getMessage === "A they clause must only appear after a top level subject clause.")
          assert(e.failedCodeFileName === Some("WordSpecSpec.scala"))
          assert(e.failedCodeLineNumber === Some(thisLineNumber - 10))
        }
        
        it("should throw NotAllowedException with correct stack depth and message when 'they should' is called without inner branch") {
          class TestSpec extends WordSpec { 
            "A Stack" when { 
              they should {
                "do something interesting" in {}
              }
            }
          }
          val e = intercept[exceptions.NotAllowedException] {
            new TestSpec
          }
          assert(e.getMessage === "A they clause must only appear after a top level subject clause.")
          assert(e.failedCodeFileName === Some("WordSpecSpec.scala"))
          assert(e.failedCodeLineNumber === Some(thisLineNumber - 10))
        }
        
        it("should throw NotAllowedException with correct stack depth and message when 'they can' is called without inner branch") {
          class TestSpec extends WordSpec { 
            "A Stack" when { 
              they can {
                "do something interesting" in {}
              }
            }
          }
          val e = intercept[exceptions.NotAllowedException] {
            new TestSpec
          }
          assert(e.getMessage === "A they clause must only appear after a top level subject clause.")
          assert(e.failedCodeFileName === Some("WordSpecSpec.scala"))
          assert(e.failedCodeLineNumber === Some(thisLineNumber - 10))
        }
        
        it("should throw NotAllowedException with correct stack depth and message when 'they must' is called without inner branch") {
          class TestSpec extends WordSpec { 
            "A Stack" when { 
              they must {
                "do something interesting" in {}
              }
            }
          }
          val e = intercept[exceptions.NotAllowedException] {
            new TestSpec
          }
          assert(e.getMessage === "A they clause must only appear after a top level subject clause.")
          assert(e.failedCodeFileName === Some("WordSpecSpec.scala"))
          assert(e.failedCodeLineNumber === Some(thisLineNumber - 10))
        }
        
        it("should throw NotAllowedException with correct stack depth and message when 'they when' is called without inner branch") {
          class TestSpec extends WordSpec { 
            "A Stack" when { 
              they when {
                "do something interesting" in {}
              }
            }
          }
          val e = intercept[exceptions.NotAllowedException] {
            new TestSpec
          }
          assert(e.getMessage === "A they clause must only appear after a top level subject clause.")
          assert(e.failedCodeFileName === Some("WordSpecSpec.scala"))
          assert(e.failedCodeLineNumber === Some(thisLineNumber - 10))
        }
        
        it("should throw NotAllowedException with correct stack depth and message when 'they should' is called with inner branch but after an 'in' clause") {
          class TestSpec extends WordSpec { 
            "A Stack" when { 
              "empty" should { 
                "be empty" in {} 
              }
              "do something" in {}
              they should {
                "do something interesting" in {}
              }
            }
          }
          val e = intercept[exceptions.NotAllowedException] {
            new TestSpec
          }
          assert(e.getMessage === "A they clause must only appear after a top level subject clause.")
          assert(e.failedCodeFileName === Some("WordSpecSpec.scala"))
          assert(e.failedCodeLineNumber === Some(thisLineNumber - 10))
        }
        
        it("should throw NotAllowedException with correct stack depth and message when 'they can' is called with inner branch but after an 'in' clause") {
          class TestSpec extends WordSpec { 
            "A Stack" when { 
              "empty" should { 
                "be empty" in {} 
              }
              "do something" in {}
              they can {
                "do something interesting" in {}
              }
            }
          }
          val e = intercept[exceptions.NotAllowedException] {
            new TestSpec
          }
          assert(e.getMessage === "A they clause must only appear after a top level subject clause.")
          assert(e.failedCodeFileName === Some("WordSpecSpec.scala"))
          assert(e.failedCodeLineNumber === Some(thisLineNumber - 10))
        }
        
        it("should throw NotAllowedException with correct stack depth and message when 'they must' is called with inner branch but after an 'in' clause") {
          class TestSpec extends WordSpec { 
            "A Stack" when { 
              "empty" should { 
                "be empty" in {} 
              }
              "do something" in {}
              they must {
                "do something interesting" in {}
              }
            }
          }
          val e = intercept[exceptions.NotAllowedException] {
            new TestSpec
          }
          assert(e.getMessage === "A they clause must only appear after a top level subject clause.")
          assert(e.failedCodeFileName === Some("WordSpecSpec.scala"))
          assert(e.failedCodeLineNumber === Some(thisLineNumber - 10))
        }
        
        it("should throw NotAllowedException with correct stack depth and message when 'they when' is called with inner branch but after an 'in' clause") {
          class TestSpec extends WordSpec { 
            "A Stack" when { 
              "empty" should { 
                "be empty" in {} 
              }
              "do something" in {}
              they when {
                "do something interesting" in {}
              }
            }
          }
          val e = intercept[exceptions.NotAllowedException] {
            new TestSpec
          }
          assert(e.getMessage === "A they clause must only appear after a top level subject clause.")
          assert(e.failedCodeFileName === Some("WordSpecSpec.scala"))
          assert(e.failedCodeLineNumber === Some(thisLineNumber - 10))
        }
      }
      
      describe("under 'in' clause") {
        
        it("should throw NotAllowedException with correct stack depth and message when 'they should' is called") {
          class TestSpec extends WordSpec { 
            var notAllowedThrown = false
            "Something special" in {
              they should {
                "do something interesting" in {}
              }
            }
            override def withFixture(test: NoArgTest): Outcome = {
              val outcome = test.apply()
              outcome match {
                case Exceptional(ex: exceptions.NotAllowedException) => 
                  notAllowedThrown = true
                case _ =>
              }
              outcome
            }
          }
          val rep = new EventRecordingReporter
          val s = new TestSpec
          s.run(None, Args(rep))
          assert(s.notAllowedThrown == true)
          val testFailedEvents = rep.testFailedEventsReceived
          assert(testFailedEvents.size === 1)
          assert(testFailedEvents(0).throwable.get.getClass() === classOf[exceptions.NotAllowedException])
          val trce = testFailedEvents(0).throwable.get.asInstanceOf[exceptions.NotAllowedException]
          assert("WordSpecSpec.scala" === trce.failedCodeFileName.get)
          assert(trce.failedCodeLineNumber.get === thisLineNumber - 23)
          assert(trce.getMessage === "A they clause must only appear after a top level subject clause.")
        }
        
        it("should throw NotAllowedException with correct stack depth and message when 'they can' is called") {
          class TestSpec extends WordSpec { 
            var notAllowedThrown = false
            "Something special" in {
              they can {
                "do something interesting" in {}
              }
            }
            override def withFixture(test: NoArgTest): Outcome = {
              val outcome = test.apply()
              outcome match {
                case Exceptional(ex: exceptions.NotAllowedException) => 
                  notAllowedThrown = true
                case _ =>
              }
              outcome
            }
          }
          val rep = new EventRecordingReporter
          val s = new TestSpec
          s.run(None, Args(rep))
          assert(s.notAllowedThrown == true)
          val testFailedEvents = rep.testFailedEventsReceived
          assert(testFailedEvents.size === 1)
          assert(testFailedEvents(0).throwable.get.getClass() === classOf[exceptions.NotAllowedException])
          val trce = testFailedEvents(0).throwable.get.asInstanceOf[exceptions.NotAllowedException]
          assert("WordSpecSpec.scala" === trce.failedCodeFileName.get)
          assert(trce.failedCodeLineNumber.get === thisLineNumber - 23)
          assert(trce.getMessage === "A they clause must only appear after a top level subject clause.")
        }
        
        it("should throw NotAllowedException with correct stack depth and message when 'they must' is called") {
          class TestSpec extends WordSpec { 
            var notAllowedThrown = false
            "Something special" in {
              they must {
                "do something interesting" in {}
              }
            }
            override def withFixture(test: NoArgTest): Outcome = {
              val outcome = test.apply()
              outcome match {
                case Exceptional(ex: exceptions.NotAllowedException) => 
                  notAllowedThrown = true
                case _ =>
              }
              outcome
            }
          }
          val rep = new EventRecordingReporter
          val s = new TestSpec
          s.run(None, Args(rep))
          assert(s.notAllowedThrown == true)
          val testFailedEvents = rep.testFailedEventsReceived
          assert(testFailedEvents.size === 1)
          assert(testFailedEvents(0).throwable.get.getClass() === classOf[exceptions.NotAllowedException])
          val trce = testFailedEvents(0).throwable.get.asInstanceOf[exceptions.NotAllowedException]
          assert("WordSpecSpec.scala" === trce.failedCodeFileName.get)
          assert(trce.failedCodeLineNumber.get === thisLineNumber - 23)
          assert(trce.getMessage === "A they clause must only appear after a top level subject clause.")
        }
        
        it("should throw NotAllowedException with correct stack depth and message when 'they when' is called") {
          class TestSpec extends WordSpec { 
            var notAllowedThrown = false
            "Something special" in {
              they when {
                "do something interesting" in {}
              }
            }
            override def withFixture(test: NoArgTest): Outcome = {
              val outcome = test.apply()
              outcome match {
                case Exceptional(ex: exceptions.NotAllowedException) => 
                  notAllowedThrown = true
                case _ =>
              }
              outcome
            }
          }
          val rep = new EventRecordingReporter
          val s = new TestSpec
          s.run(None, Args(rep))
          assert(s.notAllowedThrown == true)
          val testFailedEvents = rep.testFailedEventsReceived
          assert(testFailedEvents.size === 1)
          assert(testFailedEvents(0).throwable.get.getClass() === classOf[exceptions.NotAllowedException])
          val trce = testFailedEvents(0).throwable.get.asInstanceOf[exceptions.NotAllowedException]
          assert("WordSpecSpec.scala" === trce.failedCodeFileName.get)
          assert(trce.failedCodeLineNumber.get === thisLineNumber - 23)
          assert(trce.getMessage === "A they clause must only appear after a top level subject clause.")
        }
      }
    }
  }
}
