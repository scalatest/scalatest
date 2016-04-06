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

import org.scalatest._
import SharedHelpers._
import org.scalatest.events.{TestStarting, TestFailed}
import org.scalatest.exceptions.DuplicateTestNameException
import org.scalatest.exceptions.TestFailedException
import org.scalatest.exceptions.TestRegistrationClosedException
import org.scalatest.events.InfoProvided
import org.scalactic.exceptions.NullArgumentException

object SlowTest extends Tag("SlowTest")

class FlatSpecSpec extends org.scalatest.FunSpec {

  describe("A fixture.FlatSpec ") {
    it("A fixture.Spec should return the test names in order of registration from testNames") {
      val a = new FlatSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = Succeeded
        "Something" should "do that" in { fixture =>
          /* ASSERTION_SUCCEED */
        }
        it should "do this" in { fixture =>
          /* ASSERTION_SUCCEED */
        }
      }

      assertResult(List("Something should do that", "Something should do this")) {
        a.testNames.iterator.toList
      }

      val b = new FlatSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = Succeeded
      }

      assertResult(List[String]()) {
        b.testNames.iterator.toList
      }

      val c = new FlatSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = Succeeded
        "Something" should "do this" in { fixture =>
          /* ASSERTION_SUCCEED */
        }
        it should "do that" in { fixture =>
          /* ASSERTION_SUCCEED */
        }
      }

      assertResult(List("Something should do this", "Something should do that")) {
        c.testNames.iterator.toList
      }
    }

    it("should throw DuplicateTestNameException if a duplicate test name registration is attempted") {

      intercept[DuplicateTestNameException] {
        new FlatSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          it should "test this" in { fixture => /* ASSERTION_SUCCEED */ }
          it should "test this" in { fixture => /* ASSERTION_SUCCEED */ }
        }
      }
      intercept[DuplicateTestNameException] {
        new FlatSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          it should "test this" in { fixture => /* ASSERTION_SUCCEED */ }
          it should "test this" taggedAs(SlowTest) ignore { fixture => /* ASSERTION_SUCCEED */ }
        }
      }
      intercept[DuplicateTestNameException] {
        new FlatSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          it should "test this" in { fixture => /* ASSERTION_SUCCEED */ }
          it should "test this" taggedAs(Tag("SlowTest")) ignore { fixture => /* ASSERTION_SUCCEED */ }
        }
      }
      intercept[DuplicateTestNameException] {
        new FlatSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          ignore should "test this" in { fixture => /* ASSERTION_SUCCEED */ }
          it should "test this" ignore { fixture => /* ASSERTION_SUCCEED */ }
        }
      }
      intercept[DuplicateTestNameException] {
        new FlatSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          ignore should "test this" in { fixture => /* ASSERTION_SUCCEED */ }
          it should "test this" in { fixture => /* ASSERTION_SUCCEED */ }
        }
      }

      intercept[DuplicateTestNameException] {
        new FlatSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          registerTest("should test this") { fixture => /* ASSERTION_SUCCEED */ }
          registerTest("should test this") { fixture => /* ASSERTION_SUCCEED */ }
        }
      }
      intercept[DuplicateTestNameException] {
        new FlatSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          registerTest("should test this") { fixture => /* ASSERTION_SUCCEED */ }
          registerIgnoredTest("should test this", SlowTest) { fixture => /* ASSERTION_SUCCEED */ }
        }
      }
      intercept[DuplicateTestNameException] {
        new FlatSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          registerTest("should test this") { fixture => /* ASSERTION_SUCCEED */ }
          registerIgnoredTest("should test this", Tag("SlowTest")) { fixture => /* ASSERTION_SUCCEED */ }
        }
      }
      intercept[DuplicateTestNameException] {
        new FlatSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          registerIgnoredTest("should test this") { fixture => /* ASSERTION_SUCCEED */ }
          registerIgnoredTest("should test this") { fixture => /* ASSERTION_SUCCEED */ }
        }
      }
      intercept[DuplicateTestNameException] {
        new FlatSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          registerIgnoredTest("should test this") { fixture => /* ASSERTION_SUCCEED */ }
          registerTest("should test this") { fixture => /* ASSERTION_SUCCEED */ }
        }
      }
    }

    it("should pass in the fixture to every test method") {
      val a = new FlatSpec {
        type FixtureParam = String
        val hello = "Hello, world!"
        def withFixture(test: OneArgTest): Outcome = {
          test(hello)
        }
        "Something" should "do this" in { fixture =>
          assert(fixture === hello)
        }
        it should "do that" in { fixture =>
          assert(fixture === hello)
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      assert(!rep.eventsReceived.exists(_.isInstanceOf[TestFailed]))
    }

    it("should run tests registered via the 'it can behave like' syntax") {
      trait SharedFlatSpecTests { this: FlatSpec =>
        def nonEmptyStack(s: String)(i: Int) {
          it can "I am shared" in { fixture => /* ASSERTION_SUCCEED */ }
        }
      }
      class MyConfigFlatSpec extends FlatSpec with SharedFlatSpecTests {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = {
          test("hi")
        }
        it can behave like nonEmptyStack("hi")(1)
      }
      val suite = new MyConfigFlatSpec
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
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          it should "hi" taggedAs(null) in { fixture => /* ASSERTION_SUCCEED */ }
        }
      }
      val caught = intercept[NullArgumentException] {
        new FlatSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          it should "hi" taggedAs(mytags.SlowAsMolasses, null) in { fixture => /* ASSERTION_SUCCEED */ }
        }
      }
      assert(caught.getMessage == "a test tag was null")
      intercept[NullArgumentException] {
        new FlatSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          it should "hi" taggedAs(mytags.SlowAsMolasses, null, mytags.WeakAsAKitten) in { fixture => /* ASSERTION_SUCCEED */ }
        }
      }
      // ignore
      intercept[NullArgumentException] {
        new FlatSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          ignore should "hi" taggedAs(null) in { fixture => /* ASSERTION_SUCCEED */ }
        }
      }
      val caught2 = intercept[NullArgumentException] {
        new FlatSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          ignore should "hi" taggedAs(mytags.SlowAsMolasses, null) in { fixture => /* ASSERTION_SUCCEED */ }
        }
      }
      assert(caught2.getMessage == "a test tag was null")
      intercept[NullArgumentException] {
        new FlatSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          ignore should "hi" taggedAs(mytags.SlowAsMolasses, null, mytags.WeakAsAKitten) in { fixture => /* ASSERTION_SUCCEED */ }
        }
      }
      intercept[NullArgumentException] {
        new FlatSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          it should "hi" taggedAs(null) ignore { fixture => /* ASSERTION_SUCCEED */ }
        }
      }
      val caught3 = intercept[NullArgumentException] {
        new FlatSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          it should "hi" taggedAs(mytags.SlowAsMolasses, null) ignore { fixture => /* ASSERTION_SUCCEED */ }
        }
      }
      assert(caught3.getMessage == "a test tag was null")
      intercept[NullArgumentException] {
        new FlatSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          it should "hi" taggedAs(mytags.SlowAsMolasses, null, mytags.WeakAsAKitten) ignore { fixture => /* ASSERTION_SUCCEED */ }
        }
      }

      // registerTest
      intercept[NullArgumentException] {
        new FlatSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          registerTest("should hi", null) { fixture => /* ASSERTION_SUCCEED */ }
        }
      }
      val caught4 = intercept[NullArgumentException] {
        new FlatSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          registerTest("should hi", mytags.SlowAsMolasses, null) { fixture => /* ASSERTION_SUCCEED */ }
        }
      }
      assert(caught4.getMessage == "a test tag was null")
      intercept[NullArgumentException] {
        new FlatSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          registerTest("should hi", mytags.SlowAsMolasses, null, mytags.WeakAsAKitten) { fixture => /* ASSERTION_SUCCEED */ }
        }
      }

      // registerIgnoredTest
      intercept[NullArgumentException] {
        new FlatSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          registerIgnoredTest("should hi", null) { fixture => /* ASSERTION_SUCCEED */ }
        }
      }
      val caught5 = intercept[NullArgumentException] {
        new FlatSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          registerIgnoredTest("should hi", mytags.SlowAsMolasses, null) { fixture => /* ASSERTION_SUCCEED */ }
        }
      }
      assert(caught5.getMessage == "a test tag was null")
      intercept[NullArgumentException] {
        new FlatSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          registerIgnoredTest("should hi", mytags.SlowAsMolasses, null, mytags.WeakAsAKitten) { fixture => /* ASSERTION_SUCCEED */ }
        }
      }
      intercept[NullArgumentException] {
        new FlatSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          registerIgnoredTest("should hi", null) { fixture => /* ASSERTION_SUCCEED */ }
        }
      }
      val caught6 = intercept[NullArgumentException] {
        new FlatSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          registerIgnoredTest("should hi", mytags.SlowAsMolasses, null) { fixture => /* ASSERTION_SUCCEED */ }
        }
      }
      assert(caught6.getMessage == "a test tag was null")
      intercept[NullArgumentException] {
        new FlatSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          registerIgnoredTest("should hi", mytags.SlowAsMolasses, null, mytags.WeakAsAKitten)  { fixture => /* ASSERTION_SUCCEED */ }
        }
      }
    }


    it("should return a correct tags map from the tags method using is (pending), when using regular (not shorthand)" +
            " notation and ignore replacing it") {

      val a = new FlatSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = Succeeded
        ignore should "test this" is (pending)
        it should "test that" is (pending)
      }
      assertResult(Map("should test this" -> Set("org.scalatest.Ignore"))) {
        a.tags
      }

      val b = new FlatSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = Succeeded
        it can "test this" is (pending)
        ignore can "test that" is (pending)
      }
      assertResult(Map("can test that" -> Set("org.scalatest.Ignore"))) {
        b.tags
      }

      val c = new FlatSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = Succeeded
        ignore must "test this" is (pending)
        ignore must "test that" is (pending)
      }
      assertResult(Map("must test this" -> Set("org.scalatest.Ignore"), "must test that" -> Set("org.scalatest.Ignore"))) {
        c.tags
      }

      val d = new FlatSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = Succeeded
        it must "test this" taggedAs(mytags.SlowAsMolasses) is (pending)
        ignore must "test that" taggedAs(mytags.SlowAsMolasses) is (pending)
      }
      assertResult(Map("must test this" -> Set("org.scalatest.SlowAsMolasses"), "must test that" -> Set("org.scalatest.Ignore", "org.scalatest.SlowAsMolasses"))) {
        d.tags
      }

      val e = new FlatSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = Succeeded
        it must "test this" is (pending)
        it must "test that" is (pending)
      }
      assertResult(Map()) {
        e.tags
      }

      val f = new FlatSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = Succeeded
        it can "test this" taggedAs(mytags.SlowAsMolasses, mytags.WeakAsAKitten) is (pending)
        it can "test that" taggedAs(mytags.SlowAsMolasses) in  { fixture => /* ASSERTION_SUCCEED */ }
      }
      assertResult(Map("can test this" -> Set("org.scalatest.SlowAsMolasses", "org.scalatest.WeakAsAKitten"), "can test that" -> Set("org.scalatest.SlowAsMolasses"))) {
        f.tags
      }

      val g = new FlatSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = Succeeded
        it should "test this" taggedAs(mytags.SlowAsMolasses, mytags.WeakAsAKitten) is (pending)
        it should "test that" taggedAs(mytags.SlowAsMolasses) is (pending)
      }
      assertResult(Map("should test this" -> Set("org.scalatest.SlowAsMolasses", "org.scalatest.WeakAsAKitten"), "should test that" -> Set("org.scalatest.SlowAsMolasses"))) {
        g.tags
      }
    }
    it("should return a correct tags map from the tags method using is (pending), when using regular (not shorthand)" +
            " notation and ignore replacing in") {

      val a = new FlatSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = Succeeded
        it should "test this" ignore { fixture => /* ASSERTION_SUCCEED */ }
        it should "test that" is (pending)
      }
      assertResult(Map("should test this" -> Set("org.scalatest.Ignore"))) {
        a.tags
      }

      val b = new FlatSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = Succeeded
        it can "test this" is (pending)
        it can "test that" ignore { fixture => /* ASSERTION_SUCCEED */ }
      }
      assertResult(Map("can test that" -> Set("org.scalatest.Ignore"))) {
        b.tags
      }

      val c = new FlatSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = Succeeded
        it must "test this" ignore { fixture => /* ASSERTION_SUCCEED */ }
        it must "test that" ignore { fixture => /* ASSERTION_SUCCEED */ }
      }
      assertResult(Map("must test this" -> Set("org.scalatest.Ignore"), "must test that" -> Set("org.scalatest.Ignore"))) {
        c.tags
      }

      val d = new FlatSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = Succeeded
        it must "test this" taggedAs(mytags.SlowAsMolasses) is (pending)
        it must "test that" taggedAs(mytags.SlowAsMolasses) ignore { fixture => /* ASSERTION_SUCCEED */ }
      }
      assertResult(Map("must test this" -> Set("org.scalatest.SlowAsMolasses"), "must test that" -> Set("org.scalatest.Ignore", "org.scalatest.SlowAsMolasses"))) {
        d.tags
      }
    }

    it("should return a correct tags map from the tags method using is (pending), when using shorthand notation") {

      val a = new FlatSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = Succeeded
        "A Stack" should "test this" ignore { fixture => /* ASSERTION_SUCCEED */ }
        "A Stack" should "test that" is (pending)
      }
      assertResult(Map("A Stack should test this" -> Set("org.scalatest.Ignore"))) {
        a.tags
      }

      val b = new FlatSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = Succeeded
        "A Stack" can "test this" is (pending)
        "A Stack" can "test that" ignore { fixture => /* ASSERTION_SUCCEED */ }
      }
      assertResult(Map("A Stack can test that" -> Set("org.scalatest.Ignore"))) {
        b.tags
      }

      val c = new FlatSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = Succeeded
        "A Stack" must "test this" ignore { fixture => /* ASSERTION_SUCCEED */ }
        "A Stack" must "test that" ignore { fixture => /* ASSERTION_SUCCEED */ }
      }
      assertResult(Map("A Stack must test this" -> Set("org.scalatest.Ignore"), "A Stack must test that" -> Set("org.scalatest.Ignore"))) {
        c.tags
      }

      val d = new FlatSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = Succeeded
        "A Stack" must "test this" taggedAs(mytags.SlowAsMolasses) is (pending)
        "A Stack" must "test that" taggedAs(mytags.SlowAsMolasses) ignore { fixture => /* ASSERTION_SUCCEED */ }
      }
      assertResult(Map("A Stack must test this" -> Set("org.scalatest.SlowAsMolasses"), "A Stack must test that" -> Set("org.scalatest.Ignore", "org.scalatest.SlowAsMolasses"))) {
        d.tags
      }

      val e = new FlatSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = Succeeded
        "A Stack" must "test this" is (pending)
        "A Stack" must "test that" is (pending)
      }
      assertResult(Map()) {
        e.tags
      }

      val f = new FlatSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = Succeeded
        "A Stack" can "test this" taggedAs(mytags.SlowAsMolasses, mytags.WeakAsAKitten) is (pending)
        "A Stack" can "test that" taggedAs(mytags.SlowAsMolasses) in  { fixture => /* ASSERTION_SUCCEED */ }
      }
      assertResult(Map("A Stack can test this" -> Set("org.scalatest.SlowAsMolasses", "org.scalatest.WeakAsAKitten"), "A Stack can test that" -> Set("org.scalatest.SlowAsMolasses"))) {
        f.tags
      }

      val g = new FlatSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = Succeeded
        "A Stack" should "test this" taggedAs(mytags.SlowAsMolasses, mytags.WeakAsAKitten) is (pending)
        "A Stack" should "test that" taggedAs(mytags.SlowAsMolasses) in  { fixture => /* ASSERTION_SUCCEED */ }
      }
      assertResult(Map("A Stack should test this" -> Set("org.scalatest.SlowAsMolasses", "org.scalatest.WeakAsAKitten"), "A Stack should test that" -> Set("org.scalatest.SlowAsMolasses"))) {
        g.tags
      }
    }

    class TestWasCalledSuite extends FlatSpec {
      type FixtureParam = String
      def withFixture(test: OneArgTest): Outcome = { test("hi") }
      var theTestThisCalled = false
      var theTestThatCalled = false
      it should "run this" in { fixture => theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
      it should "run that, maybe" in { fixture => theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
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
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        it can "test this" in { fixture => theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        it can "test that" in { fixture => theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }

      import scala.language.reflectiveCalls

      val repA = new TestIgnoredTrackingReporter
      a.run(None, Args(repA))
      assert(!repA.testIgnoredReceived)
      assert(a.theTestThisCalled)
      assert(a.theTestThatCalled)

      val b = new FlatSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        ignore must "test this" in { fixture => theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        it must "test that" in { fixture => theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }

      val repB = new TestIgnoredTrackingReporter
      b.run(None, Args(repB))
      assert(repB.testIgnoredReceived)
      assert(repB.lastEvent.isDefined)
      assert(repB.lastEvent.get.testName endsWith "test this")
      assert(!b.theTestThisCalled)
      assert(b.theTestThatCalled)

      val c = new FlatSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        it can "test this" in { fixture => theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        ignore can "test that" in { fixture => theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
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
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        ignore should "test this" in { fixture => theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        ignore should "test that" in { fixture => theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
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
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        ignore must "test this" in { fixture => theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        it must "test that" in { fixture => theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
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
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        it should "test this" taggedAs(mytags.SlowAsMolasses) in { fixture => theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        it should "test that" in { fixture => theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }

      import scala.language.reflectiveCalls

      val repA = new TestIgnoredTrackingReporter
      a.run(None, Args(repA))
      assert(!repA.testIgnoredReceived)
      assert(a.theTestThisCalled)
      assert(a.theTestThatCalled)

      // SlowAsMolasses is included, one test should be excluded
      val b = new FlatSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        it should "test this" taggedAs(mytags.SlowAsMolasses) in { fixture => theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        it should "test that" in { fixture => theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repB = new TestIgnoredTrackingReporter
      b.run(None, Args(repB, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repB.testIgnoredReceived)
      assert(b.theTestThisCalled)
      assert(!b.theTestThatCalled)

      // SlowAsMolasses is included, and both tests should be included
      val c = new FlatSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        it should "test this" taggedAs(mytags.SlowAsMolasses) in { fixture => theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        it should "test that" taggedAs(mytags.SlowAsMolasses) in { fixture => theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repC = new TestIgnoredTrackingReporter
      c.run(None, Args(repB, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repC.testIgnoredReceived)
      assert(c.theTestThisCalled)
      assert(c.theTestThatCalled)

      // SlowAsMolasses is included. both tests should be included but one ignored
      val d = new FlatSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        ignore should "test this" taggedAs(mytags.SlowAsMolasses) in { fixture => theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        it should "test that" taggedAs(mytags.SlowAsMolasses) in { fixture => theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repD = new TestIgnoredTrackingReporter
      d.run(None, Args(repD, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.Ignore")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(repD.testIgnoredReceived)
      assert(!d.theTestThisCalled)
      assert(d.theTestThatCalled)

      // SlowAsMolasses included, FastAsLight excluded
      val e = new FlatSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        it should "test this" taggedAs(mytags.SlowAsMolasses, mytags.FastAsLight) in { fixture => theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        it should "test that" taggedAs(mytags.SlowAsMolasses) in { fixture => theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        it should "test the other" in { fixture => theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
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
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        ignore should "test this" taggedAs(mytags.SlowAsMolasses, mytags.FastAsLight) in { fixture => theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        it should "test that" taggedAs(mytags.SlowAsMolasses) in { fixture => theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        it should "test the other" in { fixture => theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
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
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        it should "test this" taggedAs(mytags.SlowAsMolasses, mytags.FastAsLight) in { fixture => theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        it should "test that" taggedAs(mytags.SlowAsMolasses) in { fixture => theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        ignore should "test the other" in { fixture => theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
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
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        it should "test this" taggedAs(mytags.SlowAsMolasses, mytags.FastAsLight) in { fixture => theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        it should "test that" taggedAs(mytags.SlowAsMolasses) in { fixture => theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        it should "test the other" in { fixture => theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repH = new TestIgnoredTrackingReporter
      h.run(None, Args(repH, Stopper.default, Filter(None, Set("org.scalatest.FastAsLight")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repH.testIgnoredReceived)
      assert(!h.theTestThisCalled)
      assert(h.theTestThatCalled)
      assert(h.theTestTheOtherCalled)

      // No tagsToInclude set, SlowAsMolasses excluded
      val i = new FlatSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        it should "test this" taggedAs(mytags.SlowAsMolasses, mytags.FastAsLight) in { fixture => theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        it should "test that" taggedAs(mytags.SlowAsMolasses) in { fixture => theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        it should "test the other" in { fixture => theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repI = new TestIgnoredTrackingReporter
      i.run(None, Args(repI, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repI.testIgnoredReceived)
      assert(!i.theTestThisCalled)
      assert(!i.theTestThatCalled)
      assert(i.theTestTheOtherCalled)

      // No tagsToInclude set, SlowAsMolasses excluded, TestIgnored should not be received on excluded ones
      val j = new FlatSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        ignore should "test this" taggedAs(mytags.SlowAsMolasses, mytags.FastAsLight) in { fixture => theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        ignore should "test that" taggedAs(mytags.SlowAsMolasses) in { fixture => theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        it should "test the other" in { fixture => theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repJ = new TestIgnoredTrackingReporter
      j.run(None, Args(repJ, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repI.testIgnoredReceived)
      assert(!j.theTestThisCalled)
      assert(!j.theTestThatCalled)
      assert(j.theTestTheOtherCalled)

      // Same as previous, except Ignore specifically mentioned in excludes set
      val k = new FlatSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        ignore should "test this" taggedAs(mytags.SlowAsMolasses, mytags.FastAsLight) in { fixture => theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        ignore should "test that" taggedAs(mytags.SlowAsMolasses) in { fixture => theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        ignore should "test the other" in { fixture => theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
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
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        registerTest("should test this", mytags.SlowAsMolasses) { fixture => theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("should test that") { fixture => theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }

      import scala.language.reflectiveCalls

      val repA = new TestIgnoredTrackingReporter
      a.run(None, Args(repA))
      assert(!repA.testIgnoredReceived)
      assert(a.theTestThisCalled)
      assert(a.theTestThatCalled)

      // SlowAsMolasses is included, one test should be excluded
      val b = new FlatSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        registerTest("should test this", mytags.SlowAsMolasses) { fixture => theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("should test that") { fixture => theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repB = new TestIgnoredTrackingReporter
      b.run(None, Args(repB, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repB.testIgnoredReceived)
      assert(b.theTestThisCalled)
      assert(!b.theTestThatCalled)

      // SlowAsMolasses is included, and both tests should be included
      val c = new FlatSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        registerTest("should test this", mytags.SlowAsMolasses) { fixture => theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("should test that", mytags.SlowAsMolasses) { fixture => theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repC = new TestIgnoredTrackingReporter
      c.run(None, Args(repB, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repC.testIgnoredReceived)
      assert(c.theTestThisCalled)
      assert(c.theTestThatCalled)

      // SlowAsMolasses is included. both tests should be included but one ignored
      val d = new FlatSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        registerIgnoredTest("should test this", mytags.SlowAsMolasses) { fixture => theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("should test that", mytags.SlowAsMolasses) { fixture => theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repD = new TestIgnoredTrackingReporter
      d.run(None, Args(repD, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.Ignore")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(repD.testIgnoredReceived)
      assert(!d.theTestThisCalled)
      assert(d.theTestThatCalled)

      // SlowAsMolasses included, FastAsLight excluded
      val e = new FlatSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        registerTest("should test this", mytags.SlowAsMolasses, mytags.FastAsLight) { fixture => theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("should test that", mytags.SlowAsMolasses) { fixture => theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("should test the other") { fixture => theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
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
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        registerIgnoredTest("should test this", mytags.SlowAsMolasses, mytags.FastAsLight) { fixture => theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("should test that", mytags.SlowAsMolasses) { fixture => theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("should test the other") { fixture => theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
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
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        registerTest("should test this", mytags.SlowAsMolasses, mytags.FastAsLight) { fixture => theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("should test that", mytags.SlowAsMolasses) { fixture => theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        registerIgnoredTest("should test the other") { fixture => theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
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
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        registerTest("should test this", mytags.SlowAsMolasses, mytags.FastAsLight) { fixture => theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("should test that", mytags.SlowAsMolasses) { fixture => theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("should test the other") { fixture => theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repH = new TestIgnoredTrackingReporter
      h.run(None, Args(repH, Stopper.default, Filter(None, Set("org.scalatest.FastAsLight")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repH.testIgnoredReceived)
      assert(!h.theTestThisCalled)
      assert(h.theTestThatCalled)
      assert(h.theTestTheOtherCalled)

      // No tagsToInclude set, SlowAsMolasses excluded
      val i = new FlatSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        registerTest("should test this", mytags.SlowAsMolasses, mytags.FastAsLight) { fixture => theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("should test that", mytags.SlowAsMolasses) { fixture => theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("should test the other") { fixture => theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repI = new TestIgnoredTrackingReporter
      i.run(None, Args(repI, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repI.testIgnoredReceived)
      assert(!i.theTestThisCalled)
      assert(!i.theTestThatCalled)
      assert(i.theTestTheOtherCalled)

      // No tagsToInclude set, SlowAsMolasses excluded, TestIgnored should not be received on excluded ones
      val j = new FlatSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        registerIgnoredTest("should test this", mytags.SlowAsMolasses, mytags.FastAsLight) { fixture => theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerIgnoredTest("should test that", mytags.SlowAsMolasses) { fixture => theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("should test the other") { fixture => theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val repJ = new TestIgnoredTrackingReporter
      j.run(None, Args(repJ, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repI.testIgnoredReceived)
      assert(!j.theTestThisCalled)
      assert(!j.theTestThatCalled)
      assert(j.theTestTheOtherCalled)

      // Same as previous, except Ignore specifically mentioned in excludes set
      val k = new FlatSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        registerIgnoredTest("should test this", mytags.SlowAsMolasses, mytags.FastAsLight) { fixture => theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerIgnoredTest("should test that", mytags.SlowAsMolasses) { fixture => theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        registerIgnoredTest("should test the other") { fixture => theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
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
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        it should "test this" in { fixture => /* ASSERTION_SUCCEED */ }
        it should "test that" in { fixture => /* ASSERTION_SUCCEED */ }
      }
      assert(a.expectedTestCount(Filter()) == 2)

      val b = new FlatSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        ignore should "test this" in { fixture => /* ASSERTION_SUCCEED */ }
        it should "test that" in { fixture => /* ASSERTION_SUCCEED */ }
      }
      assert(b.expectedTestCount(Filter()) == 1)

      val c = new FlatSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        it should "test this" taggedAs(mytags.FastAsLight) in { fixture => /* ASSERTION_SUCCEED */ }
        it should "test that" in { fixture => /* ASSERTION_SUCCEED */ }
      }
      assert(c.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) == 1)
      assert(c.expectedTestCount(Filter(None, Set("org.scalatest.FastAsLight"))) == 1)

      val d = new FlatSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        it should "test this" taggedAs(mytags.FastAsLight, mytags.SlowAsMolasses) in { fixture => /* ASSERTION_SUCCEED */ }
        it should "test that" taggedAs(mytags.SlowAsMolasses) in { fixture => /* ASSERTION_SUCCEED */ }
        it should "test the other thing" in { fixture => /* ASSERTION_SUCCEED */ }
      }
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) == 1)
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight"))) == 1)
      assert(d.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) == 1)
      assert(d.expectedTestCount(Filter()) == 3)

      val e = new FlatSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        it should "test this" taggedAs(mytags.FastAsLight, mytags.SlowAsMolasses) in { fixture => /* ASSERTION_SUCCEED */ }
        it should "test that" taggedAs(mytags.SlowAsMolasses) in { fixture => /* ASSERTION_SUCCEED */ }
        ignore should "test the other thing" in { fixture => /* ASSERTION_SUCCEED */ }
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
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        registerTest("should test this") { fixture => /* ASSERTION_SUCCEED */ }
        registerTest("should test that") { fixture => /* ASSERTION_SUCCEED */ }
      }
      assert(a.expectedTestCount(Filter()) == 2)

      val b = new FlatSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        registerIgnoredTest("should test this") { fixture => /* ASSERTION_SUCCEED */ }
        registerTest("should test that") { fixture => /* ASSERTION_SUCCEED */ }
      }
      assert(b.expectedTestCount(Filter()) == 1)

      val c = new FlatSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        registerTest("should test this", mytags.FastAsLight) { fixture => /* ASSERTION_SUCCEED */ }
        registerTest("should test that") { fixture => /* ASSERTION_SUCCEED */ }
      }
      assert(c.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) == 1)
      assert(c.expectedTestCount(Filter(None, Set("org.scalatest.FastAsLight"))) == 1)

      val d = new FlatSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        registerTest("should test this", mytags.FastAsLight, mytags.SlowAsMolasses) { fixture => /* ASSERTION_SUCCEED */ }
        registerTest("should test that", mytags.SlowAsMolasses) { fixture => /* ASSERTION_SUCCEED */ }
        registerTest("should test the other thing") { fixture => /* ASSERTION_SUCCEED */ }
      }
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) == 1)
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight"))) == 1)
      assert(d.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) == 1)
      assert(d.expectedTestCount(Filter()) == 3)

      val e = new FlatSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        registerTest("should test this", mytags.FastAsLight, mytags.SlowAsMolasses) { fixture => /* ASSERTION_SUCCEED */ }
        registerTest("should test that", mytags.SlowAsMolasses) { fixture => /* ASSERTION_SUCCEED */ }
        registerIgnoredTest("should test the other thing") { fixture => /* ASSERTION_SUCCEED */ }
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
        type FixtureParam = String
        val hello = "Hello, world!"
        def withFixture(test: OneArgTest): Outcome = {
          test(hello)
        }

        it should "do this" is (pending)

        it should "do that" in { fixture =>
          assert(fixture === hello)
        }
        
        it should "do something else" in { fixture =>
          assert(fixture === hello)
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
        type FixtureParam = String
        val hello = "Hello, world!"
        def withFixture(test: OneArgTest): Outcome = {
          test(hello)
        }
        it should "throw AssertionError" in { s => throw new AssertionError }
        it should "throw plain old Error" in { s => throw new Error }
        it should "throw Throwable" in { s =>  throw new Throwable }
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
        type FixtureParam = String
        val hello = "Hello, world!"
        def withFixture(test: OneArgTest): Outcome = {
          test(hello)
        }
        it should "throws AssertionError" in { s => throw new OutOfMemoryError }
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
        type FixtureParam = String
        val hello = "Hello, world!"
        def withFixture(test: OneArgTest): Outcome = {
          test(hello)
        }
        it should "do something else" in { s =>
          given("two integers")
          when("one is subracted from the other")
          then("the result is the difference between the two numbers")
          pending
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
    }
    it("should send InfoProvided events with aboutAPendingTest set to false for info " +
            "calls made from a test that is not pending") {
      val a = new FlatSpec with GivenWhenThen {
        type FixtureParam = String
        val hello = "Hello, world!"
        def withFixture(test: OneArgTest): Outcome = {
          test(hello)
        }
        it should "do something else" in { s =>
          given("two integers")
          when("one is subracted from the other")
          then("the result is the difference between the two numbers")
          assert(1 + 1 === 2)
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
    }
*/
    it("should allow both tests that take fixtures and tests that don't") {
      val a = new FlatSpec {

        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = {
          test("Hello, world!")
        }

        var takesNoArgsInvoked = false
        it should "take no args" in { () => takesNoArgsInvoked = true; /* ASSERTION_SUCCEED */ }

        var takesAFixtureInvoked = false
        it should "take a fixture" in { s => takesAFixtureInvoked = true; /* ASSERTION_SUCCEED */ }
      }

      import scala.language.reflectiveCalls

      a.run(None, Args(SilentReporter))
      assert(a.testNames.size === 2, a.testNames)
      assert(a.takesNoArgsInvoked)
      assert(a.takesAFixtureInvoked)
    }
    it("should work with test functions whose inferred result type is not Unit") {
      val a = new FlatSpec {

        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = {
          test("Hello, world!")
        }

        var takesNoArgsInvoked = false
        it should "take no args" in { () => takesNoArgsInvoked = true; true; /* ASSERTION_SUCCEED */ }

        var takesAFixtureInvoked = false
        it should "take a fixture" in { s => takesAFixtureInvoked = true; true; /* ASSERTION_SUCCEED */ }
      }

      import scala.language.reflectiveCalls

      assert(!a.takesNoArgsInvoked)
      assert(!a.takesAFixtureInvoked)
      a.run(None, Args(SilentReporter))
      assert(a.testNames.size === 2, a.testNames)
      assert(a.takesNoArgsInvoked)
      assert(a.takesAFixtureInvoked)
    }
    it("should work with ignored tests whose inferred result type is not Unit") {
      val a = new FlatSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        ignore should "test this" in { () => theTestThisCalled = true; "hi"; /* ASSERTION_SUCCEED */ }
        ignore should "test that" in { fixture => theTestThatCalled = true; 42; /* ASSERTION_SUCCEED */ }
      }

      import scala.language.reflectiveCalls

      assert(!a.theTestThisCalled)
      assert(!a.theTestThatCalled)
      val reporter = new EventRecordingReporter
      a.run(None, Args(reporter))
      assert(reporter.testIgnoredEventsReceived.size === 2)
      assert(!a.theTestThisCalled)
      assert(!a.theTestThatCalled)
    }
    it("should pass a NoArgTest to withFixture for tests that take no fixture") {
      class MySpec extends FlatSpec {
        type FixtureParam = String
        var aNoArgTestWasPassed = false
        var aOneArgTestWasPassed = false
        override def withFixture(test: NoArgTest): Outcome = {
          aNoArgTestWasPassed = true
          Succeeded
        }
        def withFixture(test: OneArgTest): Outcome = {
          aOneArgTestWasPassed = true
          Succeeded
        }
        it should "do something" in { () =>
          assert(1 + 1 === 2)
        }
      }

      val s = new MySpec
      s.run(None, Args(SilentReporter))
      assert(s.aNoArgTestWasPassed)
      assert(!s.aOneArgTestWasPassed)
    }
    it("should not pass a NoArgTest to withFixture for tests that take a Fixture") {
      class MySpec extends FlatSpec {
        type FixtureParam = String
        var aNoArgTestWasPassed = false
        var aOneArgTestWasPassed = false
        override def withFixture(test: NoArgTest): Outcome = {
          aNoArgTestWasPassed = true
          Succeeded
        }
        def withFixture(test: OneArgTest): Outcome = {
          aOneArgTestWasPassed = true
          Succeeded
        }
        it should "do something" in { fixture =>
          assert(1 + 1 === 2)
        }
      }

      val s = new MySpec
      s.run(None, Args(SilentReporter))
      assert(!s.aNoArgTestWasPassed)
      assert(s.aOneArgTestWasPassed)
    }
    it("should pass a NoArgTest that invokes the no-arg test when the " +
            "NoArgTest's no-arg apply method is invoked") {

      class MySpec extends FlatSpec {
        type FixtureParam = String
        var theNoArgTestWasInvoked = false
        def withFixture(test: OneArgTest): Outcome = {
          // Shouldn't be called, but just in case don't invoke a OneArgTest
          Succeeded
        }
        it should "do something" in { () =>
          theNoArgTestWasInvoked = true
          /* ASSERTION_SUCCEED */
        }
      }

      val s = new MySpec
      s.run(None, Args(SilentReporter))
      assert(s.theNoArgTestWasInvoked)
    }
    it("should pass the correct test name in the OneArgTest passed to withFixture") {
      val a = new FlatSpec {
        type FixtureParam = String
        var correctTestNameWasPassed = false
        def withFixture(test: OneArgTest): Outcome = {
          correctTestNameWasPassed = test.name == "should do something"
          test("hi")
        }
        it should "do something" in { fixture => /* ASSERTION_SUCCEED */ }
      }

      import scala.language.reflectiveCalls

      a.run(None, Args(SilentReporter))
      assert(a.correctTestNameWasPassed)
    }
    it("should pass the correct config map in the OneArgTest passed to withFixture") {
      val a = new FlatSpec {
        type FixtureParam = String
        var correctConfigMapWasPassed = false
        def withFixture(test: OneArgTest): Outcome = {
          correctConfigMapWasPassed = (test.configMap == ConfigMap("hi" -> 7))
          test("hi")
        }
        it should "do something" in { fixture => /* ASSERTION_SUCCEED */ }
      }

      import scala.language.reflectiveCalls

      a.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap("hi" -> 7), None, new Tracker(), Set.empty))
      assert(a.correctConfigMapWasPassed)
    }
    describe("(when a nesting rule has been violated)") {

      it("should, if they call a behavior-of from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends FlatSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = { test("hi") }
          it should "blow up" in { fixture =>
            behavior of "in the wrong place, at the wrong time"
            /* ASSERTION_SUCCEED */
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceivedWithCorrectMessage(spec, "should blow up", "a \"behavior of\" clause may not appear inside an \"in\" clause")
      }
      it("should, if they call a should from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends FlatSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = { test("hi") }
          it should "blow up" in { fixture =>
            "in the wrong place, at the wrong time" should "definitely blow up" in { fixture => /* ASSERTION_SUCCEED */ }
            /* ASSERTION_SUCCEED */
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceivedWithCorrectMessage(spec, "should blow up", "a \"should\" clause may not appear inside an \"in\" clause")
      }
      it("should, if they call a should behave from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends FlatSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = { test("hi") }
          def aTest {}
          it should "blow up" in { fixture =>
            "in the wrong place, at the wrong time" should behave like aTest
            /* ASSERTION_SUCCEED */
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceivedWithCorrectMessage(spec, "should blow up", "a \"should\" clause may not appear inside an \"in\" clause")
      }
      it("should, if they call a behavior-of with a nested it from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends FlatSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = { test("hi") }
          it should "blow up" in { fixture =>
            behavior of "in the wrong place, at the wrong time"
            it should "never run" in { fixture =>
              assert(1 === 1)
            }
            /* ASSERTION_SUCCEED */
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceivedWithCorrectMessage(spec, "should blow up", "a \"behavior of\" clause may not appear inside an \"in\" clause")
      }
      it("should, if they call a should with a nested it from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends FlatSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = { test("hi") }
          it should "blow up" in { fixture =>
            "in the wrong place, at the wrong time" should "definitely blow up" in { fixture =>
              it should "never run" in { fixture =>
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
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = { test("hi") }
          it should "blow up" in { fixture =>
            it should "never run" in { fixture =>
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
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = { test("hi") }
          it should "blow up" in { fixture =>
            it should "never run" taggedAs(mytags.SlowAsMolasses) in { fixture =>
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
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = { test("hi") }
          registerTest("should blow up") { fixture =>
            registerTest("should never run", mytags.SlowAsMolasses) { fixture =>
              assert(1 == 1)
            }
            /* ASSERTION_SUCCEED */
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a behavior-of with a nested ignore from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends FlatSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = { test("hi") }
          it should "blow up" in { fixture =>
            behavior of "in the wrong place, at the wrong time"
            ignore should "never run" in { fixture =>
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
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = { test("hi") }
          it should "blow up" in { fixture =>
            ignore should "never run" in { fixture =>
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
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = { test("hi") }
          it should "blow up" in { fixture =>
            ignore should "never run" taggedAs(mytags.SlowAsMolasses) in { fixture =>
              assert(1 == 1)
            }
            /* ASSERTION_SUCCEED */
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a nested registerIgnoredTest with tags from within a registerTest clause, result in a TestFailedException when running the test") {

        class MySpec extends FlatSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = { test("hi") }
          registerTest("should blow up") { fixture =>
            registerIgnoredTest("should never run", mytags.SlowAsMolasses) { fixture =>
              assert(1 == 1)
            }
            /* ASSERTION_SUCCEED */
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
    }

    describe("when using 'they should' in place of 'it should'") {
      
      it("A fixture.Spec should return the test names in order of registration from testNames") {
        val a = new FlatSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          "Something" should "do that" in { fixture =>
            /* ASSERTION_SUCCEED */
          }
          they should "do this" in { fixture =>
            /* ASSERTION_SUCCEED */
          }
        }

        assertResult(List("Something should do that", "Something should do this")) {
          a.testNames.iterator.toList
        }

        val b = new FlatSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
        }

        assertResult(List[String]()) {
          b.testNames.iterator.toList
        }

        val c = new FlatSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          "Something" should "do this" in { fixture =>
            /* ASSERTION_SUCCEED */
          }
          they should "do that" in { fixture =>
            /* ASSERTION_SUCCEED */
          }
        }

        assertResult(List("Something should do this", "Something should do that")) {
          c.testNames.iterator.toList
        }
      }
      
      it("should throw DuplicateTestNameException if a duplicate test name registration is attempted") {

        intercept[DuplicateTestNameException] {
          new FlatSpec {
            type FixtureParam = String
            def withFixture(test: OneArgTest): Outcome = Succeeded
            they should "test this" in { fixture => /* ASSERTION_SUCCEED */ }
            they should "test this" in { fixture => /* ASSERTION_SUCCEED */ }
          }
        }
        intercept[DuplicateTestNameException] {
          new FlatSpec {
            type FixtureParam = String
            def withFixture(test: OneArgTest): Outcome = Succeeded
            they should "test this" in { fixture => /* ASSERTION_SUCCEED */ }
            they should "test this" taggedAs(SlowTest) ignore { fixture => /* ASSERTION_SUCCEED */ }
          }
        }
        intercept[DuplicateTestNameException] {
          new FlatSpec {
            type FixtureParam = String
            def withFixture(test: OneArgTest): Outcome = Succeeded
            they should "test this" in { fixture => /* ASSERTION_SUCCEED */ }
            they should "test this" taggedAs(Tag("SlowTest")) ignore { fixture => /* ASSERTION_SUCCEED */ }
          }
        }
        intercept[DuplicateTestNameException] {
          new FlatSpec {
            type FixtureParam = String
            def withFixture(test: OneArgTest): Outcome = Succeeded
            ignore should "test this" in { fixture => /* ASSERTION_SUCCEED */ }
            they should "test this" ignore { fixture => /* ASSERTION_SUCCEED */ }
          }
        }
        intercept[DuplicateTestNameException] {
          new FlatSpec {
            type FixtureParam = String
            def withFixture(test: OneArgTest): Outcome = Succeeded
            ignore should "test this" in { fixture => /* ASSERTION_SUCCEED */ }
            they should "test this" in { fixture => /* ASSERTION_SUCCEED */ }
          }
        }
      }
    }

    it("should allow test registration with registerTest and registerIgnoredTest") {
      class TestSpec extends FlatSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        val a = 1
        registerTest("test 1") { fixture =>
          val e = intercept[TestFailedException] {
            assert(a == 2)
          }
          assert(e.message == Some("1 did not equal 2"))
          assert(e.failedCodeFileName == Some("FlatSpecSpec.scala"))
          assert(e.failedCodeLineNumber == Some(thisLineNumber - 4))
        }
        registerTest("test 2") { fixture =>
          assert(a == 2)
        }
        registerTest("test 3") { fixture =>
          pending
        }
        registerTest("test 4") { fixture =>
          cancel
        }
        registerIgnoredTest("test 5") { fixture =>
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
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        "a widget" should "do something"  in { fixture =>
          expect(1 === 2); /* ASSERTION_SUCCEED */
        }
        it should "do something else" in { () =>
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
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        it should "fail"  in { fixture =>
          assert(1 === 2)
        }
        behavior of "scenario"
        it should "fail" in { fixture =>
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
    
    it("should generate TestRegistrationClosedException with correct stack depth info when has an in nested inside an in") {
      class TestSpec extends FlatSpec {
        var registrationClosedThrown = false
        type FixtureParam = String
        behavior of "a feature"
        it should "fail" in { fixture => 
          it should "fail" in { fixture => 
            assert(1 === 2)
          }; /* ASSERTION_SUCCEED */
        }
        override def withFixture(test: OneArgTest): Outcome = {
          val outcome = test.apply("hi")
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

    it("should generate TestRegistrationClosedException with correct stack depth info when has an ignore nested inside an in") {
      class TestSpec extends FlatSpec {
        var registrationClosedThrown = false
        type FixtureParam = String
        behavior of "a feature"
        it should "fail" in { fixture =>
          ignore should "fail" in { fixture =>
            assert(1 === 2)
          }; /* ASSERTION_SUCCEED */
        }
        override def withFixture(test: OneArgTest): Outcome = {
          val outcome = test.apply("hi")
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
        type FixtureParam = String
        behavior of "a feature"
        registerTest("should fail") { fixture =>
          registerTest("should fail") { fixture =>
            assert(1 == 2)
          }; /* ASSERTION_SUCCEED */
        }
        override def withFixture(test: OneArgTest): Outcome = {
          val outcome = test.apply("hi")
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
        type FixtureParam = String
        behavior of "a feature"
        registerTest("should fail") { fixture =>
          registerIgnoredTest("should fail") { fixture =>
            assert(1 == 2)
          }; /* ASSERTION_SUCCEED */
        }
        override def withFixture(test: OneArgTest): Outcome = {
          val outcome = test.apply("hi")
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

    it("should generate a DuplicateTestNameException is detected") {
      class TestSpec extends FlatSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        behavior of "a feature"
        it should "test 1" in { fixture => }
        it should "test 1" in { fixture => }
      }
      val e = intercept[DuplicateTestNameException] {
        new TestSpec
      }
      assert("FlatSpecSpec.scala" == e.failedCodeFileName.get)
      assert(e.failedCodeLineNumber.get == thisLineNumber - 6)
      assert(!e.cause.isDefined)
    }
  }
}