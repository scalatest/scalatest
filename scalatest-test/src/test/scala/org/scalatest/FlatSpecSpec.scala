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
import Matchers._
import org.scalatest.exceptions.DuplicateTestNameException
import org.scalatest.exceptions.TestFailedException
import org.scalatest.exceptions.TestRegistrationClosedException
import org.scalactic.exceptions.NullArgumentException

class FlatSpecSpec extends FunSpec with GivenWhenThen {

  describe("A FlatSpec") {

    it("should return the test names in registration order from testNames when using 'it should'") {

      val a = new FlatSpec {
        it should "test this" in {/* ASSERTION_SUCCEED */}
        it should "test that" in {/* ASSERTION_SUCCEED */}
      }

      assertResult(List("should test this", "should test that")) {
        a.testNames.iterator.toList
      }

      val b = new FlatSpec {}

      assertResult(List[String]()) {
        b.testNames.iterator.toList
      }

      val c = new FlatSpec {
        it should "test that" in {/* ASSERTION_SUCCEED */}
        it should "test this" in {/* ASSERTION_SUCCEED */}
      }

      assertResult(List("should test that", "should test this")) {
        c.testNames.iterator.toList
      }

      val d = new FlatSpec {
        behavior of "A Tester"
        it should "test that" in {/* ASSERTION_SUCCEED */}
        it should "test this" in {/* ASSERTION_SUCCEED */}
      }

      assertResult(List("A Tester should test that", "A Tester should test this")) {
        d.testNames.iterator.toList
      }

      val e = new FlatSpec {
        behavior of "A Tester"
        it should "test this" in {/* ASSERTION_SUCCEED */}
        it should "test that" in {/* ASSERTION_SUCCEED */}
      }

      assertResult(List("A Tester should test this", "A Tester should test that")) {
        e.testNames.iterator.toList
      }
    }

    it("should throw DuplicateTestNameException if a duplicate test name registration is attempted") {
      
      intercept[DuplicateTestNameException] {
        new FlatSpec {
          it should "test this" in {/* ASSERTION_SUCCEED */}
          it should "test this" in {/* ASSERTION_SUCCEED */}
        }
      }
      intercept[DuplicateTestNameException] {
        new FlatSpec {
          it should "test this" in {/* ASSERTION_SUCCEED */}
          ignore should "test this" in {/* ASSERTION_SUCCEED */}
        }
      }
      intercept[DuplicateTestNameException] {
        new FlatSpec {
          ignore should "test this" in {/* ASSERTION_SUCCEED */}
          it should "test this" ignore {/* ASSERTION_SUCCEED */}
        }
      }
      intercept[DuplicateTestNameException] {
        new FlatSpec {
          ignore should "test this" in {/* ASSERTION_SUCCEED */}
          it should "test this" in {/* ASSERTION_SUCCEED */}
        }
      }
    }

    it("should invoke withFixture from runTest") {
      val a = new FlatSpec {
        var withFixtureWasInvoked = false
        var testWasInvoked = false
        override def withFixture(test: NoArgTest): Outcome = {
          withFixtureWasInvoked = true
          super.withFixture(test)
        }
        it should "do something" in {
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
      val a = new FlatSpec {
        var correctTestNameWasPassed = false
        override def withFixture(test: NoArgTest): Outcome = {
          correctTestNameWasPassed = test.name == "should do something"
          super.withFixture(test)
        }
        it should "do something" in {/* ASSERTION_SUCCEED */}
      }

      import scala.language.reflectiveCalls

      a.run(None, Args(SilentReporter))
      assert(a.correctTestNameWasPassed)
    }
    it("should pass the correct config map in the NoArgTest passed to withFixture") {
      val a = new FlatSpec {
        var correctConfigMapWasPassed = false
        override def withFixture(test: NoArgTest): Outcome = {
          correctConfigMapWasPassed = (test.configMap == ConfigMap("hi" -> 7))
          super.withFixture(test)
        }
        it should "do something" in {/* ASSERTION_SUCCEED */}
      }

      import scala.language.reflectiveCalls

      a.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap("hi" -> 7), None, new Tracker(), Set.empty))
      assert(a.correctConfigMapWasPassed)
    }
    describe("(with info calls)") {
      class InfoInsideTestFlatSpec extends FlatSpec {
        val msg = "hi there, dude"
        val partialTestName = "test name"
        val testName = "should " + partialTestName
        it should partialTestName in {
          info(msg); /* ASSERTION_SUCCEED */
        }
      }
      // In a FlatSpec, any InfoProvided's fired during the test should be cached and sent out after the test has
      // suceeded or failed. This makes the report look nicer, because the info is tucked under the "specifier'
      // text for that test.
      it("should, when the info appears in the code of a successful test, report the info in the TestSucceeded") {
        val spec = new InfoInsideTestFlatSpec
        /*val (infoProvidedIndex, testStartingIndex, testSucceededIndex) =
          getIndexesForInformerEventOrderTests(spec, spec.testName, spec.msg)
        assert(testSucceededIndex < infoProvidedIndex)*/
        val myRep = new EventRecordingReporter
        spec.run(None, Args(myRep))
        val testStarting = myRep.testStartingEventsReceived
        assert(1 === testStarting.size)
        val testSucceeded = myRep.testSucceededEventsReceived
        assert(1 === testSucceeded.size)
        assert(1 === testSucceeded(0).recordedEvents.size)
        val ip: InfoProvided = testSucceeded(0).recordedEvents(0).asInstanceOf[InfoProvided]
        assert(spec.msg === ip.message)
      }
      class InfoBeforeTestFlatSpec extends FlatSpec {
        val msg = "hi there, dude"
        val partialTestName = "test name"
        val testName = "should " + partialTestName
        info(msg)
        it should partialTestName in {/* ASSERTION_SUCCEED */}
      }
      it("should, when the info appears in the body before a test, report the info before the test") {
        val spec = new InfoBeforeTestFlatSpec
        val (infoProvidedIndex, testStartingIndex, testSucceededIndex) =
          getIndexesForInformerEventOrderTests(spec, spec.testName, spec.msg)
        assert(infoProvidedIndex < testStartingIndex)
        assert(testStartingIndex < testSucceededIndex)
      }
      it("should, when the info appears in the body after a test, report the info after the test runs") {
        val msg = "hi there, dude"
        val partialTestName = "test name"
        val testName = "should " + partialTestName
        class MyFlatSpec extends FlatSpec {
          it should partialTestName in {/* ASSERTION_SUCCEED */}
          info(msg)
        }
        val (infoProvidedIndex, testStartingIndex, testSucceededIndex) =
          getIndexesForInformerEventOrderTests(new MyFlatSpec, testName, msg)
        assert(testStartingIndex < testSucceededIndex)
        assert(testSucceededIndex < infoProvidedIndex)
      }
      it("should print to stdout when info is called by a method invoked after the suite has been executed") {
        class MyFlatSpec extends FlatSpec {
          callInfo() // This should work fine
          def callInfo() {
            info("howdy")
          }
          it should "howdy also" in {
            callInfo() // This should work fine
            /* ASSERTION_SUCCEED */
          }
        }
        val spec = new MyFlatSpec
        val myRep = new EventRecordingReporter
        spec.run(None, Args(myRep))
        spec.callInfo() // TODO: Actually test that This prints to stdout
      }
      it("should send an InfoProvided with an IndentedText formatter with level 0 when called outside a test") {
        val spec = new InfoBeforeTestFlatSpec
        val indentedText = getIndentedTextFromInfoProvided(spec)
        assert(indentedText === IndentedText("+ " + spec.msg, spec.msg, 0))
      }
      it("should send an InfoProvided with an IndentedText formatter with level 1 when called within a test") {
        val spec = new InfoInsideTestFlatSpec
        val myRep = new EventRecordingReporter
        spec.run(None, Args(myRep))
        val indentedText = getIndentedTextFromTestInfoProvided(spec)
        assert(indentedText === IndentedText("  + " + spec.msg, spec.msg, 1))
      }
      it("should work when using the shorthand notation for 'behavior of'") {
        val e = new FlatSpec with Matchers {
          "A Tester" should "test this" in {/* ASSERTION_SUCCEED */}
          it should "test that" in {/* ASSERTION_SUCCEED */}
        }

        assertResult(List("A Tester should test this", "A Tester should test that")) {
          e.testNames.iterator.toList
        }

      }
    }
    describe("(when a nesting rule has been violated)") {

      it("should, if they call a behavior-of from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends FlatSpec {
          it should "blow up" in {
            behavior of "in the wrong place, at the wrong time"
            /* ASSERTION_SUCCEED */
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceivedWithCorrectMessage(spec, "should blow up", "a \"behavior of\" clause may not appear inside an \"in\" clause")
      }
      it("should, if they call a should from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends FlatSpec {
          it should "blow up" in {
            "in the wrong place, at the wrong time" should "definitely blow up" in { /* ASSERTION_SUCCEED */ }
            /* ASSERTION_SUCCEED */
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceivedWithCorrectMessage(spec, "should blow up", "a \"should\" clause may not appear inside an \"in\" clause")
      }
      it("should, if they call a should behave from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends FlatSpec {
          def aTest {}
          it should "blow up" in {
            "in the wrong place, at the wrong time" should behave like aTest
            /* ASSERTION_SUCCEED */
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceivedWithCorrectMessage(spec, "should blow up", "a \"should\" clause may not appear inside an \"in\" clause")
      }
      it("should, if they call a behavior-of with a nested it from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends FlatSpec {
          it should "blow up" in {
            behavior of "in the wrong place, at the wrong time"
            it should "never run" in {
              assert(1 === 1)
            }; /* ASSERTION_SUCCEED */
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceivedWithCorrectMessage(spec, "should blow up", "a \"behavior of\" clause may not appear inside an \"in\" clause")
      }
      it("should, if they call a should with a nested it from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends FlatSpec {
          it should "blow up" in {
            "in the wrong place, at the wrong time" should "definitely blow up" in {
              it should "never run" in {
                assert(1 === 1)
              }
              /* ASSERTION_SUCCEED */
            }
            /* ASSERTION_SUCCEED */
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceivedWithCorrectMessage(spec, "should blow up", "a \"should\" clause may not appear inside an \"in\" clause")
      }
      it("should, if they call a nested it from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends FlatSpec {
          it should "blow up" in {
            it should "never run" in {
              assert(1 === 1)
            }
            /* ASSERTION_SUCCEED */
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a nested it with tags from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends FlatSpec {
          it should "blow up" in {
            it should "never run" taggedAs(mytags.SlowAsMolasses) in {
              assert(1 === 1)
            }
            /* ASSERTION_SUCCEED */
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a nested registerTest with tags from within a registerTest clause, result in a TestFailedException when running the test") {

        class MySpec extends FlatSpec {
          registerTest("should blow up")  {
            registerTest("should never run", mytags.SlowAsMolasses) {
              assert(1 === 1)
            }
            /* ASSERTION_SUCCEED */
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a behavior-of with a nested ignore from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends FlatSpec {
          it should "blow up" in {
            behavior of "in the wrong place, at the wrong time"
            ignore should "never run" in {
              assert(1 === 1)
            }
            /* ASSERTION_SUCCEED */
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a nested ignore from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends FlatSpec {
          it should "blow up" in {
            ignore should "never run" in {
              assert(1 === 1)
            }
            /* ASSERTION_SUCCEED */
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a nested ignore with tags from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends FlatSpec {
          it should "blow up" in {
            ignore should "never run" taggedAs(mytags.SlowAsMolasses) in {
              assert(1 === 1)
            }
            /* ASSERTION_SUCCEED */
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a nested registerIgnoredTest with tags from within a registerTest clause, result in a TestFailedException when running the test") {

        class MySpec extends FlatSpec {
          registerTest("should blow up") {
            registerIgnoredTest("should never run", mytags.SlowAsMolasses) {
              assert(1 === 1)
            }
            /* ASSERTION_SUCCEED */
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
    }
    it("should run tests registered via the 'it should behave like' syntax") {
      trait SharedFlatSpecTests { this: FlatSpec =>
        def nonEmptyStack(s: String)(i: Int) {
          it should "I am shared" in {/* ASSERTION_SUCCEED */}
        }
      }
      class MyFlatSpec extends FlatSpec with SharedFlatSpecTests {
        it should behave like nonEmptyStack("hi")(1)
      }
      val suite = new MyFlatSpec
      val reporter = new EventRecordingReporter
      suite.run(None, Args(reporter))

      val indexedList = reporter.eventsReceived

      val testStartingOption = indexedList.find(_.isInstanceOf[TestStarting])
      assert(testStartingOption.isDefined)
      assert(testStartingOption.get.asInstanceOf[TestStarting].testName === "should I am shared")
    }
    it("should run tests registered via the 'it can behave like' syntax") {
      trait SharedFlatSpecTests { this: FlatSpec =>
        def nonEmptyStack(s: String)(i: Int) {
          it can "I am shared" in {/* ASSERTION_SUCCEED */}
        }
      }
      class MyFlatSpec extends FlatSpec with SharedFlatSpecTests {
        it can behave like nonEmptyStack("hi")(1)
      }
      val suite = new MyFlatSpec
      val reporter = new EventRecordingReporter
      suite.run(None, Args(reporter))

      val indexedList = reporter.eventsReceived

      val testStartingOption = indexedList.find(_.isInstanceOf[TestStarting])
      assert(testStartingOption.isDefined)
      assert(testStartingOption.get.asInstanceOf[TestStarting].testName === "can I am shared")
    }
    it("should throw NullArgumentException if a null test tag is provided") {
      // it
      intercept[NullArgumentException] {
        new FlatSpec {
          it should "hi" taggedAs(null) in {/* ASSERTION_SUCCEED */}
        }
      }
      val caught = intercept[NullArgumentException] {
        new FlatSpec {
          it should "hi" taggedAs(mytags.SlowAsMolasses, null) in {/* ASSERTION_SUCCEED */}
        }
      }
      assert(caught.getMessage === "a test tag was null")
      intercept[NullArgumentException] {
        new FlatSpec {
          it should "hi" taggedAs(mytags.SlowAsMolasses, null, mytags.WeakAsAKitten) in {/* ASSERTION_SUCCEED */}
        }
      }

      // ignore
      intercept[NullArgumentException] {
        new FlatSpec {
          ignore should "hi" taggedAs(null) in {/* ASSERTION_SUCCEED */}
        }
      }
      val caught2 = intercept[NullArgumentException] {
        new FlatSpec {
          ignore should "hi" taggedAs(mytags.SlowAsMolasses, null) in {/* ASSERTION_SUCCEED */}
        }
      }
      assert(caught2.getMessage === "a test tag was null")
      intercept[NullArgumentException] {
        new FlatSpec {
          ignore should "hi" taggedAs(mytags.SlowAsMolasses, null, mytags.WeakAsAKitten) in {/* ASSERTION_SUCCEED */}
        }
      }
      intercept[NullArgumentException] {
        new FlatSpec {
          it should "hi" taggedAs(null) ignore {/* ASSERTION_SUCCEED */}
        }
      }
      val caught3 = intercept[NullArgumentException] {
        new FlatSpec {
          it should "hi" taggedAs(mytags.SlowAsMolasses, null) ignore {/* ASSERTION_SUCCEED */}
        }
      }
      assert(caught3.getMessage === "a test tag was null")
      intercept[NullArgumentException] {
        new FlatSpec {
          it should "hi" taggedAs(mytags.SlowAsMolasses, null, mytags.WeakAsAKitten) ignore {/* ASSERTION_SUCCEED */}
        }
      }

      // registerTest
      intercept[NullArgumentException] {
        new FlatSpec {
          registerTest("should hi", null) {/* ASSERTION_SUCCEED */}
        }
      }
      val caught4 = intercept[NullArgumentException] {
        new FlatSpec {
          registerTest("should hi", mytags.SlowAsMolasses, null) {/* ASSERTION_SUCCEED */}
        }
      }
      assert(caught4.getMessage == "a test tag was null")
      intercept[NullArgumentException] {
        new FlatSpec {
          registerTest("should hi", mytags.SlowAsMolasses, null, mytags.WeakAsAKitten) {/* ASSERTION_SUCCEED */}
        }
      }

      // registerIgnoredTest
      intercept[NullArgumentException] {
        new FlatSpec {
          registerIgnoredTest("should hi", null) {/* ASSERTION_SUCCEED */}
        }
      }
      val caught5 = intercept[NullArgumentException] {
        new FlatSpec {
          registerIgnoredTest("should hi", mytags.SlowAsMolasses, null) {/* ASSERTION_SUCCEED */}
        }
      }
      assert(caught5.getMessage == "a test tag was null")
      intercept[NullArgumentException] {
        new FlatSpec {
          registerIgnoredTest("should hi", mytags.SlowAsMolasses, null, mytags.WeakAsAKitten) {/* ASSERTION_SUCCEED */}
        }
      }
      intercept[NullArgumentException] {
        new FlatSpec {
          registerIgnoredTest("should hi", null) {/* ASSERTION_SUCCEED */}
        }
      }
      val caught6 = intercept[NullArgumentException] {
        new FlatSpec {
          registerIgnoredTest("should hi", mytags.SlowAsMolasses, null) {/* ASSERTION_SUCCEED */}
        }
      }
      assert(caught6.getMessage == "a test tag was null")
      intercept[NullArgumentException] {
        new FlatSpec {
          registerIgnoredTest("should hi", mytags.SlowAsMolasses, null, mytags.WeakAsAKitten) {/* ASSERTION_SUCCEED */}
        }
      }
    }

    it("should return a correct tags map from the tags method when is (pending), when using regular (not shorthand)" +
            " notation and ignore replacing it") {

      val a = new FlatSpec {
        ignore should "test this" is (pending)
        it should "test that" is (pending)
      }
      assertResult(Map("should test this" -> Set("org.scalatest.Ignore"))) {
        a.tags
      }

      val b = new FlatSpec {
        it can "test this" is (pending)
        ignore can "test that" is (pending)
      }
      assertResult(Map("can test that" -> Set("org.scalatest.Ignore"))) {
        b.tags
      }

      val c = new FlatSpec {
        ignore must "test this" is (pending)
        ignore must "test that" is (pending)
      }
      assertResult(Map("must test this" -> Set("org.scalatest.Ignore"), "must test that" -> Set("org.scalatest.Ignore"))) {
        c.tags
      }

      val d = new FlatSpec {
        it must "test this" taggedAs(mytags.SlowAsMolasses) is (pending)
        ignore must "test that" taggedAs(mytags.SlowAsMolasses) is (pending)
      }
      assertResult(Map("must test this" -> Set("org.scalatest.SlowAsMolasses"), "must test that" -> Set("org.scalatest.Ignore", "org.scalatest.SlowAsMolasses"))) {
        d.tags
      }

      val e = new FlatSpec {
        it must "test this" is (pending)
        it must "test that" is (pending)
      }
      assertResult(Map()) {
        e.tags
      }

      val f = new FlatSpec {
        it can "test this" taggedAs(mytags.SlowAsMolasses, mytags.WeakAsAKitten) is (pending)
        it can "test that" taggedAs(mytags.SlowAsMolasses) in  {/* ASSERTION_SUCCEED */}
      }
      assertResult(Map("can test this" -> Set("org.scalatest.SlowAsMolasses", "org.scalatest.WeakAsAKitten"), "can test that" -> Set("org.scalatest.SlowAsMolasses"))) {
        f.tags
      }

      val g = new FlatSpec {
        it should "test this" taggedAs(mytags.SlowAsMolasses, mytags.WeakAsAKitten) is (pending)
        it should "test that" taggedAs(mytags.SlowAsMolasses) in  {/* ASSERTION_SUCCEED */}
      }
      assertResult(Map("should test this" -> Set("org.scalatest.SlowAsMolasses", "org.scalatest.WeakAsAKitten"), "should test that" -> Set("org.scalatest.SlowAsMolasses"))) {
        g.tags
      }
    }

    it("should return a correct tags map from the tags method is (pending), when using regular (not shorthand)" +
            " notation and ignore replacing is") {

      val a = new FlatSpec {
        it should "test this" ignore {/* ASSERTION_SUCCEED */}
        it should "test that" is (pending)
      }
      assertResult(Map("should test this" -> Set("org.scalatest.Ignore"))) {
        a.tags
      }

      val b = new FlatSpec {
        it can "test this" is (pending)
        it can "test that" ignore {/* ASSERTION_SUCCEED */}
      }
      assertResult(Map("can test that" -> Set("org.scalatest.Ignore"))) {
        b.tags
      }

      val c = new FlatSpec {
        it must "test this" ignore {/* ASSERTION_SUCCEED */}
        it must "test that" ignore {/* ASSERTION_SUCCEED */}
      }
      assertResult(Map("must test this" -> Set("org.scalatest.Ignore"), "must test that" -> Set("org.scalatest.Ignore"))) {
        c.tags
      }

      val d = new FlatSpec {
        it must "test this" taggedAs(mytags.SlowAsMolasses) is (pending)
        it must "test that" taggedAs(mytags.SlowAsMolasses) ignore {/* ASSERTION_SUCCEED */}
      }
      assertResult(Map("must test this" -> Set("org.scalatest.SlowAsMolasses"), "must test that" -> Set("org.scalatest.Ignore", "org.scalatest.SlowAsMolasses"))) {
        d.tags
      }
    }

    it("should return a correct tags map from the tags method is (pending), when using shorthand notation") {

      val a = new FlatSpec {
        "A Stack" should "test this" ignore {/* ASSERTION_SUCCEED */}
        "A Stack" should "test that" is (pending)
      }
      assertResult(Map("A Stack should test this" -> Set("org.scalatest.Ignore"))) {
        a.tags
      }

      val b = new FlatSpec {
        "A Stack" can "test this" is (pending)
        "A Stack" can "test that" ignore {/* ASSERTION_SUCCEED */}
      }
      assertResult(Map("A Stack can test that" -> Set("org.scalatest.Ignore"))) {
        b.tags
      }

      val c = new FlatSpec {
        "A Stack" must "test this" ignore {/* ASSERTION_SUCCEED */}
        "A Stack" must "test that" ignore {/* ASSERTION_SUCCEED */}
      }
      assertResult(Map("A Stack must test this" -> Set("org.scalatest.Ignore"), "A Stack must test that" -> Set("org.scalatest.Ignore"))) {
        c.tags
      }

      val d = new FlatSpec {
        "A Stack" must "test this" taggedAs(mytags.SlowAsMolasses) is (pending)
        "A Stack" must "test that" taggedAs(mytags.SlowAsMolasses) ignore {/* ASSERTION_SUCCEED */}
      }
      assertResult(Map("A Stack must test this" -> Set("org.scalatest.SlowAsMolasses"), "A Stack must test that" -> Set("org.scalatest.Ignore", "org.scalatest.SlowAsMolasses"))) {
        d.tags
      }

      val e = new FlatSpec {
        "A Stack" must "test this" is (pending)
        "A Stack" must "test that" is (pending)
      }
      assertResult(Map()) {
        e.tags
      }

      val f = new FlatSpec {
        "A Stack" can "test this" taggedAs(mytags.SlowAsMolasses, mytags.WeakAsAKitten) is (pending)
        "A Stack" can "test that" taggedAs(mytags.SlowAsMolasses) is (pending)
      }
      assertResult(Map("A Stack can test this" -> Set("org.scalatest.SlowAsMolasses", "org.scalatest.WeakAsAKitten"), "A Stack can test that" -> Set("org.scalatest.SlowAsMolasses"))) {
        f.tags
      }

      val g = new FlatSpec {
        "A Stack" should "test this" taggedAs(mytags.SlowAsMolasses, mytags.WeakAsAKitten) is (pending)
        "A Stack" should "test that" taggedAs(mytags.SlowAsMolasses) is (pending)
      }
      assertResult(Map("A Stack should test this" -> Set("org.scalatest.SlowAsMolasses", "org.scalatest.WeakAsAKitten"), "A Stack should test that" -> Set("org.scalatest.SlowAsMolasses"))) {
        g.tags
      }
    }

    class TestWasCalledSuite extends FlatSpec {
      var theTestThisCalled = false
      var theTestThatCalled = false
      it should "run this" in { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
      it should "run that, maybe" in { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
    }

    it("should execute all tests when run is called with testName None") {

      val b = new TestWasCalledSuite
      b.run(None, Args(SilentReporter))
      assert(b.theTestThisCalled)
      assert(b.theTestThatCalled)
    }

    it("should execute one test when run is called with a defined testName") {

      val a = new TestWasCalledSuite
      a.run(Some("should run this"), Args(SilentReporter))
      assert(a.theTestThisCalled)
      assert(!a.theTestThatCalled)
    }

    it("should report as ignored, and not run, tests marked ignored") {

      val a = new FlatSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        it can "test this" in { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        it can "test that" in { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }

      import scala.language.reflectiveCalls

      val repA = new TestIgnoredTrackingReporter
      a.run(None, Args(repA))
      assert(!repA.testIgnoredReceived)
      assert(a.theTestThisCalled)
      assert(a.theTestThatCalled)

      val b = new FlatSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        ignore must "test this" in { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        it must "test that" in { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }

      val repB = new TestIgnoredTrackingReporter
      b.run(None, Args(repB))
      assert(repB.testIgnoredReceived)
      assert(repB.lastEvent.isDefined)
      assert(repB.lastEvent.get.testName endsWith "test this")
      assert(!b.theTestThisCalled)
      assert(b.theTestThatCalled)

      val c = new FlatSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        it can "test this" in { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        ignore can "test that" in { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
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
      val d = new FlatSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        ignore should "test this" in { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        ignore should "test that" in { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
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
      val e = new FlatSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        ignore must "test this" in { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        it must "test that" in { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }

      import scala.language.reflectiveCalls

      val repE = new TestIgnoredTrackingReporter
      e.run(Some("must test this"), Args(repE))
      assert(repE.testIgnoredReceived)
      assert(!e.theTestThisCalled)
      assert(!e.theTestThatCalled)
    }

    it("should run only those tests selected by the tags to include and exclude sets") {

      // Nothing is excluded
      val a = new FlatSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        it should "test this" taggedAs(mytags.SlowAsMolasses) in { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        it should "test that" in { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }

      import scala.language.reflectiveCalls

      val repA = new TestIgnoredTrackingReporter
      a.run(None, Args(repA))
      assert(!repA.testIgnoredReceived)
      assert(a.theTestThisCalled)
      assert(a.theTestThatCalled)

      // SlowAsMolasses is included, one test should be excluded
      val b = new FlatSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        it should "test this" taggedAs(mytags.SlowAsMolasses) in { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        it should "test that" in { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repB = new TestIgnoredTrackingReporter
      b.run(None, Args(repB, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repB.testIgnoredReceived)
      assert(b.theTestThisCalled)
      assert(!b.theTestThatCalled)

      // SlowAsMolasses is included, and both tests should be included
      val c = new FlatSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        it should "test this" taggedAs(mytags.SlowAsMolasses) in { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        it should "test that" taggedAs(mytags.SlowAsMolasses) in { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repC = new TestIgnoredTrackingReporter
      c.run(None, Args(repB, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repC.testIgnoredReceived)
      assert(c.theTestThisCalled)
      assert(c.theTestThatCalled)

      // SlowAsMolasses is included. both tests should be included but one ignored
      val d = new FlatSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        ignore should "test this" taggedAs(mytags.SlowAsMolasses) in { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        it should "test that" taggedAs(mytags.SlowAsMolasses) in { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repD = new TestIgnoredTrackingReporter
      d.run(None, Args(repD, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.Ignore")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(repD.testIgnoredReceived)
      assert(!d.theTestThisCalled)
      assert(d.theTestThatCalled)

      // SlowAsMolasses included, FastAsLight excluded
      val e = new FlatSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        it should "test this" taggedAs(mytags.SlowAsMolasses, mytags.FastAsLight) in { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        it should "test that" taggedAs(mytags.SlowAsMolasses) in { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        it should "test the other" in { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repE = new TestIgnoredTrackingReporter
      e.run(None, Args(repE, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
                ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repE.testIgnoredReceived)
      assert(!e.theTestThisCalled)
      assert(e.theTestThatCalled)
      assert(!e.theTestTheOtherCalled)

      // An Ignored test that was both included and excluded should not generate a TestIgnored event
      val f = new FlatSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        ignore should "test this" taggedAs(mytags.SlowAsMolasses, mytags.FastAsLight) in { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        it should "test that" taggedAs(mytags.SlowAsMolasses) in { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        it should "test the other" in { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repF = new TestIgnoredTrackingReporter
      f.run(None, Args(repF, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
                ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repF.testIgnoredReceived)
      assert(!f.theTestThisCalled)
      assert(f.theTestThatCalled)
      assert(!f.theTestTheOtherCalled)

      // An Ignored test that was not included should not generate a TestIgnored event
      val g = new FlatSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        it should "test this" taggedAs(mytags.SlowAsMolasses, mytags.FastAsLight) in { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        it should "test that" taggedAs(mytags.SlowAsMolasses) in { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        ignore should "test the other" in { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repG = new TestIgnoredTrackingReporter
      g.run(None, Args(repG, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
                ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repG.testIgnoredReceived)
      assert(!g.theTestThisCalled)
      assert(g.theTestThatCalled)
      assert(!g.theTestTheOtherCalled)

      // No tagsToInclude set, FastAsLight excluded
      val h = new FlatSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        it should "test this" taggedAs(mytags.SlowAsMolasses, mytags.FastAsLight) in { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        it should "test that" taggedAs(mytags.SlowAsMolasses) in { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        it should "test the other" in { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repH = new TestIgnoredTrackingReporter
      h.run(None, Args(repH, Stopper.default, Filter(None, Set("org.scalatest.FastAsLight")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repH.testIgnoredReceived)
      assert(!h.theTestThisCalled)
      assert(h.theTestThatCalled)
      assert(h.theTestTheOtherCalled)

      // No tagsToInclude set, mytags.SlowAsMolasses excluded
      val i = new FlatSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        it should "test this" taggedAs(mytags.SlowAsMolasses, mytags.FastAsLight) in { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        it should "test that" taggedAs(mytags.SlowAsMolasses) in { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        it should "test the other" in { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repI = new TestIgnoredTrackingReporter
      i.run(None, Args(repI, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repI.testIgnoredReceived)
      assert(!i.theTestThisCalled)
      assert(!i.theTestThatCalled)
      assert(i.theTestTheOtherCalled)

      // No tagsToInclude set, mytags.SlowAsMolasses excluded, TestIgnored should not be received on excluded ones
      val j = new FlatSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        ignore should "test this" taggedAs(mytags.SlowAsMolasses, mytags.FastAsLight) in { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        ignore should "test that" taggedAs(mytags.SlowAsMolasses) in { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        it should "test the other" in { theTestTheOtherCalled = true;/* ASSERTION_SUCCEED */ }
      }
      val repJ = new TestIgnoredTrackingReporter
      j.run(None, Args(repJ, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repI.testIgnoredReceived)
      assert(!j.theTestThisCalled)
      assert(!j.theTestThatCalled)
      assert(j.theTestTheOtherCalled)

      // Same as previous, except Ignore specifically mentioned in excludes set
      val k = new FlatSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        ignore should "test this" taggedAs(mytags.SlowAsMolasses, mytags.FastAsLight) in { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        ignore should "test that" taggedAs(mytags.SlowAsMolasses) in { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        ignore should "test the other" in { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
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
      val a = new FlatSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        registerTest("should test this", mytags.SlowAsMolasses) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("should test that") { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }

      import scala.language.reflectiveCalls

      val repA = new TestIgnoredTrackingReporter
      a.run(None, Args(repA))
      assert(!repA.testIgnoredReceived)
      assert(a.theTestThisCalled)
      assert(a.theTestThatCalled)

      // SlowAsMolasses is included, one test should be excluded
      val b = new FlatSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        registerTest("should test this", mytags.SlowAsMolasses) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("should test that") { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repB = new TestIgnoredTrackingReporter
      b.run(None, Args(repB, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repB.testIgnoredReceived)
      assert(b.theTestThisCalled)
      assert(!b.theTestThatCalled)

      // SlowAsMolasses is included, and both tests should be included
      val c = new FlatSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        registerTest("should test this", mytags.SlowAsMolasses) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("should test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repC = new TestIgnoredTrackingReporter
      c.run(None, Args(repB, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repC.testIgnoredReceived)
      assert(c.theTestThisCalled)
      assert(c.theTestThatCalled)

      // SlowAsMolasses is included. both tests should be included but one ignored
      val d = new FlatSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        registerIgnoredTest("should test this", mytags.SlowAsMolasses) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("should test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repD = new TestIgnoredTrackingReporter
      d.run(None, Args(repD, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.Ignore")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(repD.testIgnoredReceived)
      assert(!d.theTestThisCalled)
      assert(d.theTestThatCalled)

      // SlowAsMolasses included, FastAsLight excluded
      val e = new FlatSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        registerTest("should test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("should test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("should test the other") { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repE = new TestIgnoredTrackingReporter
      e.run(None, Args(repE, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
        ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repE.testIgnoredReceived)
      assert(!e.theTestThisCalled)
      assert(e.theTestThatCalled)
      assert(!e.theTestTheOtherCalled)

      // An Ignored test that was both included and excluded should not generate a TestIgnored event
      val f = new FlatSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        registerIgnoredTest("should test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("should test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("should test the other") { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repF = new TestIgnoredTrackingReporter
      f.run(None, Args(repF, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
        ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repF.testIgnoredReceived)
      assert(!f.theTestThisCalled)
      assert(f.theTestThatCalled)
      assert(!f.theTestTheOtherCalled)

      // An Ignored test that was not included should not generate a TestIgnored event
      val g = new FlatSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        registerTest("should test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("should test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        registerIgnoredTest("should test the other") { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repG = new TestIgnoredTrackingReporter
      g.run(None, Args(repG, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
        ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repG.testIgnoredReceived)
      assert(!g.theTestThisCalled)
      assert(g.theTestThatCalled)
      assert(!g.theTestTheOtherCalled)

      // No tagsToInclude set, FastAsLight excluded
      val h = new FlatSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        registerTest("should test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("should test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("should test the other") { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repH = new TestIgnoredTrackingReporter
      h.run(None, Args(repH, Stopper.default, Filter(None, Set("org.scalatest.FastAsLight")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repH.testIgnoredReceived)
      assert(!h.theTestThisCalled)
      assert(h.theTestThatCalled)
      assert(h.theTestTheOtherCalled)

      // No tagsToInclude set, mytags.SlowAsMolasses excluded
      val i = new FlatSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        registerTest("should test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("should test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("should test the other") { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repI = new TestIgnoredTrackingReporter
      i.run(None, Args(repI, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repI.testIgnoredReceived)
      assert(!i.theTestThisCalled)
      assert(!i.theTestThatCalled)
      assert(i.theTestTheOtherCalled)

      // No tagsToInclude set, mytags.SlowAsMolasses excluded, TestIgnored should not be received on excluded ones
      val j = new FlatSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        registerIgnoredTest("should test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerIgnoredTest("should test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("should test the other") { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repJ = new TestIgnoredTrackingReporter
      j.run(None, Args(repJ, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repI.testIgnoredReceived)
      assert(!j.theTestThisCalled)
      assert(!j.theTestThatCalled)
      assert(j.theTestTheOtherCalled)

      // Same as previous, except Ignore specifically mentioned in excludes set
      val k = new FlatSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        registerIgnoredTest("should test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerIgnoredTest("should test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        registerIgnoredTest("should test the other") { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repK = new TestIgnoredTrackingReporter
      k.run(None, Args(repK, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses", "org.scalatest.Ignore")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(repK.testIgnoredReceived)
      assert(!k.theTestThisCalled)
      assert(!k.theTestThatCalled)
      assert(!k.theTestTheOtherCalled)
    }

    it("should return the correct test count from its expectedTestCount method") {

      val a = new FlatSpec {
        it should "test this" in {/* ASSERTION_SUCCEED */}
        it should "test that" in {/* ASSERTION_SUCCEED */}
      }
      assert(a.expectedTestCount(Filter()) == 2)

      val b = new FlatSpec {
        ignore should "test this" in {/* ASSERTION_SUCCEED */}
        it should "test that" in {/* ASSERTION_SUCCEED */}
      }
      assert(b.expectedTestCount(Filter()) == 1)

      val c = new FlatSpec {
        it should "test this" taggedAs(mytags.FastAsLight) in {/* ASSERTION_SUCCEED */}
        it should "test that" in {/* ASSERTION_SUCCEED */}
      }
      assert(c.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) == 1)
      assert(c.expectedTestCount(Filter(None, Set("org.scalatest.FastAsLight"))) == 1)

      val d = new FlatSpec {
        it should "test this" taggedAs(mytags.FastAsLight, mytags.SlowAsMolasses) in {/* ASSERTION_SUCCEED */}
        it should "test that" taggedAs(mytags.SlowAsMolasses) in {/* ASSERTION_SUCCEED */}
        it should "test the other thing" in {/* ASSERTION_SUCCEED */}
      }
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) == 1)
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight"))) == 1)
      assert(d.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) == 1)
      assert(d.expectedTestCount(Filter()) == 3)

      val e = new FlatSpec {
        it should "test this" taggedAs(mytags.FastAsLight, mytags.SlowAsMolasses) in {/* ASSERTION_SUCCEED */}
        it should "test that" taggedAs(mytags.SlowAsMolasses) in {/* ASSERTION_SUCCEED */}
        ignore should "test the other thing" in {/* ASSERTION_SUCCEED */}
      }
      assert(e.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) == 1)
      assert(e.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight"))) == 1)
      assert(e.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) == 0)
      assert(e.expectedTestCount(Filter()) == 2)

      val f = new Suites(a, b, c, d, e)
      assert(f.expectedTestCount(Filter()) == 10)
    }

    it("should return the correct test count from its expectedTestCount method when uses registerTest and registerIgnoredTest to register tests") {

      val a = new FlatSpec {
        registerTest("should test this") {/* ASSERTION_SUCCEED */}
        registerTest("should test that") {/* ASSERTION_SUCCEED */}
      }
      assert(a.expectedTestCount(Filter()) == 2)

      val b = new FlatSpec {
        registerIgnoredTest("should test this") {/* ASSERTION_SUCCEED */}
        registerTest("should test that") {/* ASSERTION_SUCCEED */}
      }
      assert(b.expectedTestCount(Filter()) == 1)

      val c = new FlatSpec {
        registerTest("should test this", mytags.FastAsLight) {/* ASSERTION_SUCCEED */}
        registerTest("should test that") {/* ASSERTION_SUCCEED */}
      }
      assert(c.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) == 1)
      assert(c.expectedTestCount(Filter(None, Set("org.scalatest.FastAsLight"))) == 1)

      val d = new FlatSpec {
        registerTest("should test this", mytags.FastAsLight, mytags.SlowAsMolasses) {/* ASSERTION_SUCCEED */}
        registerTest("should test that", mytags.SlowAsMolasses) {/* ASSERTION_SUCCEED */}
        registerTest("should test the other thing") {/* ASSERTION_SUCCEED */}
      }
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) == 1)
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight"))) == 1)
      assert(d.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) ==1)
      assert(d.expectedTestCount(Filter()) == 3)

      val e = new FlatSpec {
        registerTest("should test this", mytags.FastAsLight, mytags.SlowAsMolasses) {/* ASSERTION_SUCCEED */}
        registerTest("should test that", mytags.SlowAsMolasses) {/* ASSERTION_SUCCEED */}
        registerIgnoredTest("should test the other thing") {/* ASSERTION_SUCCEED */}
      }
      assert(e.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) == 1)
      assert(e.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight"))) == 1)
      assert(e.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) == 0)
      assert(e.expectedTestCount(Filter()) == 2)

      val f = new Suites(a, b, c, d, e)
      assert(f.expectedTestCount(Filter()) == 10)
    }

    it("should generate a TestPending message when the test body is (pending)") {

      val a = new FlatSpec {

        it should "do this" is (pending)

        it should "do that" in {
          assert(2 + 2 === 4)
        }

        it should "do something else" in {
          assert(2 + 2 === 4)
          pending
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      val tp = rep.testPendingEventsReceived
      assert(tp.size === 2)
    }
    it("should allow is pendingUntilFixed to be used after is") {

      val a = new FlatSpec {

        it should "do this" is pendingUntilFixed {
          fail("i meant to do that")
        }

        it should "do that" in {
          assert(2 + 2 === 4)
        }

        it should "do something else" in {
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
      val a = new FlatSpec {
        it should "throw AssertionError" in { throw new AssertionError }
        it should "throw plain old Error" in { throw new Error }
        it should "throw Throwable" in { throw new Throwable }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      val tf = rep.testFailedEventsReceived
      assert(tf.size === 3)
    }
    // SKIP-SCALATESTJS-START
    it("should propagate out Errors that are direct subtypes of Error in JDK 1.5, other than " +
            "AssertionError, causing Suites and Runs to abort.") {
      val a = new FlatSpec {
        it should "throws AssertionError" in { throw new OutOfMemoryError }
      }
      intercept[OutOfMemoryError] {
        a.run(None, Args(SilentReporter))
      }
    }
    // SKIP-SCALATESTJS-END
/*
    it("should send InfoProvided events with aboutAPendingTest set to true for info " +
            "calls made from a test that is pending") {
      val a = new FlatSpec with GivenWhenThen {
        it should "do something else" in {
          given("two integers")
          when("one is subracted from the other")
          then("the result is the difference between the two numbers")
          pending
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      val testPending = rep.testPendingEventsReceived
      assert(1 === testPending.size)
      val recordedEvents = testPending(0).recordedEvents
      assert(recordedEvents.size === 3)
      for (event <- recordedEvents) {
        val ip = event.asInstanceOf[InfoProvided]
        assert(ip.aboutAPendingTest.isDefined && ip.aboutAPendingTest.get)
      }
    }
    it("should send InfoProvided events with aboutAPendingTest set to false for info " +
            "calls made from a test that is not pending") {
      val a = new FlatSpec with GivenWhenThen {
        it should "do something else" in {
          given("two integers")
          when("one is subracted from the other")
          then("the result is the difference between the two numbers")
          assert(1 + 1 === 2)
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      val testSucceeded = rep.testSucceededEventsReceived
      assert(1 === testSucceeded.size)
      val recordedEvents = testSucceeded(0).recordedEvents
      assert(recordedEvents.size === 3)
      for (event <- recordedEvents) {
        val ip = event.asInstanceOf[InfoProvided]
        assert(ip.aboutAPendingTest.isDefined && !ip.aboutAPendingTest.get)
      }
    }
*/
    it("should generate TestRegistrationClosedException with correct stack depth info when has an it nested inside a test") {
        class ApplicationSpec extends FlatSpec {
          var registrationClosedThrown = false
          "Application" should "send 404 on a bad request" in {
            it should "render an empty form on index" in {/* ASSERTION_SUCCEED */}

            it should "render a feature JSON on feature request" in {/* ASSERTION_SUCCEED */}; /* ASSERTION_SUCCEED */
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
        val a = new ApplicationSpec
        val rep = new EventRecordingReporter
        a.run(None, Args(rep))
        assert(a.registrationClosedThrown == true)
        val testFailedEvents = rep.testFailedEventsReceived
        assertResult(1)(testFailedEvents.size)
        assertResult(classOf[TestRegistrationClosedException])(testFailedEvents(0).throwable.get.getClass())
        val trce = testFailedEvents(0).throwable.get.asInstanceOf[TestRegistrationClosedException]
        assertResult("FlatSpecSpec.scala")(trce.failedCodeFileName.get)
        assertResult(thisLineNumber - 23)(trce.failedCodeLineNumber.get)
    }

    it("should allow test registration with registerTest and registerIgnoredTest") {
      class TestSpec extends FlatSpec {
        val a = 1
        registerTest("test 1") {
          val e = intercept[TestFailedException] {
            assert(a == 2)
          }
          assert(e.message == Some("1 did not equal 2"))
          assert(e.failedCodeFileName == Some("FlatSpecSpec.scala"))
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
      class TestSpec extends FlatSpec with Expectations {
        "a widget" should "do something"  in {
          expect(1 === 2); /* ASSERTION_SUCCEED */
        }
        it should "do something else" in {
          expect(1 === 2); /* ASSERTION_SUCCEED */
        }
      }
      val rep = new EventRecordingReporter
      val s1 = new TestSpec
      s1.run(None, Args(rep))
      assert(rep.testFailedEventsReceived.size === 2)
      assert(rep.testFailedEventsReceived(0).throwable.get.asInstanceOf[TestFailedException].failedCodeFileName.get === "FlatSpecSpec.scala")
      assert(rep.testFailedEventsReceived(0).throwable.get.asInstanceOf[TestFailedException].failedCodeLineNumber.get === thisLineNumber - 11)
      assert(rep.testFailedEventsReceived(1).throwable.get.asInstanceOf[TestFailedException].failedCodeFileName.get === "FlatSpecSpec.scala")
      assert(rep.testFailedEventsReceived(1).throwable.get.asInstanceOf[TestFailedException].failedCodeLineNumber.get === thisLineNumber - 10)
    }
  }
  
  describe("when failure happens") {
    
    it("should fire TestFailed event with correct stack depth info when test failed") {
      class TestSpec extends FlatSpec {
        it should "fail"  in {
          assert(1 === 2)
        }
        behavior of "scenario"
        it should "fail" in {
          assert(1 === 2)
        }
      }
      val rep = new EventRecordingReporter
      val s1 = new TestSpec
      s1.run(None, Args(rep))
      assert(rep.testFailedEventsReceived.size === 2)
      assert(rep.testFailedEventsReceived(0).throwable.get.asInstanceOf[TestFailedException].failedCodeFileName.get === "FlatSpecSpec.scala")
      assert(rep.testFailedEventsReceived(0).throwable.get.asInstanceOf[TestFailedException].failedCodeLineNumber.get === thisLineNumber - 12)
      assert(rep.testFailedEventsReceived(1).throwable.get.asInstanceOf[TestFailedException].failedCodeFileName.get === "FlatSpecSpec.scala")
      assert(rep.testFailedEventsReceived(1).throwable.get.asInstanceOf[TestFailedException].failedCodeLineNumber.get === thisLineNumber - 10)
    }
    
    it("should generate TestRegistrationClosedException with correct stack depth info when has an in nested inside a in") {
      class TestSpec extends FlatSpec {
        var registrationClosedThrown = false
        behavior of "a feature"
        it should "fail" in {
          it should "fail" in {
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
      assert("FlatSpecSpec.scala" === trce.failedCodeFileName.get)
      assert(trce.failedCodeLineNumber.get === thisLineNumber - 23)
      assert(trce.message == Some("An in clause may not appear inside another in or is clause."))
    }

    it("should generate TestRegistrationClosedException with correct stack depth info when has an ignore nested inside a in") {
      class TestSpec extends FlatSpec {
        var registrationClosedThrown = false
        behavior of "a feature"
        it should "fail" in {
          ignore should "fail" in {
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
      assert("FlatSpecSpec.scala" === trce.failedCodeFileName.get)
      assert(trce.failedCodeLineNumber.get === thisLineNumber - 23)
      assert(trce.message == Some("An ignore clause may not appear inside an in or an is clause."))
    }

    it("should generate TestRegistrationClosedException with correct stack depth info when has a registerTest nested inside a registerTest") {
      class TestSpec extends FlatSpec {
        var registrationClosedThrown = false
        behavior of "a feature"
        registerTest("should fail") {
          registerTest("should fail") {
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
      assert("FlatSpecSpec.scala" === trce.failedCodeFileName.get)
      assert(trce.failedCodeLineNumber.get === thisLineNumber - 23)
      assert(trce.message == Some("Test cannot be nested inside another test."))
    }

    it("should generate TestRegistrationClosedException with correct stack depth info when has a registerIgnoredTest nested inside a registerTest") {
      class TestSpec extends FlatSpec {
        var registrationClosedThrown = false
        behavior of "a feature"
        registerTest("should fail") {
          registerIgnoredTest("should fail") {
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
      assert("FlatSpecSpec.scala" === trce.failedCodeFileName.get)
      assert(trce.failedCodeLineNumber.get === thisLineNumber - 23)
      assert(trce.message == Some("Test cannot be nested inside another test."))
    }
  }
}