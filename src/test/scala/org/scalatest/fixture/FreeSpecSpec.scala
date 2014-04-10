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
import events.TestFailed
import org.scalatest.exceptions.DuplicateTestNameException
import org.scalatest.exceptions.TestFailedException
import org.scalatest.exceptions.TestRegistrationClosedException
import org.scalatest.events.InfoProvided

class FreeSpecSpec extends org.scalatest.FunSpec with PrivateMethodTester {

  describe("A fixture.FreeSpec") {

    it("should return the test names in order of registration from testNames") {
      val a = new FreeSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = Succeeded
        "Something" - {
          "should do that" in { fixture => ()
          }
          "should do this" in { fixture =>
          }
        }
      }

      assertResult(List("Something should do that", "Something should do this")) {
        a.testNames.iterator.toList
      }

      val b = new FreeSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = Succeeded
      }

      assertResult(List[String]()) {
        b.testNames.iterator.toList
      }

      val c = new FreeSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = Succeeded
        "Something" - {
          "should do this" in { fixture =>
          }
          "should do that" in { fixture =>
          }
        }
      }

      assertResult(List("Something should do this", "Something should do that")) {
        c.testNames.iterator.toList
      }
    }

    it("should throw DuplicateTestNameException if a duplicate test name registration is attempted") {

      intercept[DuplicateTestNameException] {
        new FreeSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          "should test this" in { fixture => }
          "should test this" in { fixture => }
        }
      }
      intercept[DuplicateTestNameException] {
        new FreeSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          "should test this" in { fixture => }
          "should test this" ignore { fixture => }
        }
      }
      intercept[DuplicateTestNameException] {
        new FreeSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          "should test this" ignore { fixture => }
          "should test this" ignore { fixture => }
        }
      }
      intercept[DuplicateTestNameException] {
        new FreeSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          "should test this" ignore { fixture => }
          "should test this" in { fixture => }
        }
      }
    }

    it("should pass in the fixture to every test method") {
      val a = new FreeSpec {
        type FixtureParam = String
        val hello = "Hello, world!"
        def withFixture(test: OneArgTest): Outcome = {
          test(hello)
        }
        "Something" - {
          "should do this" in { fixture =>
            assert(fixture === hello)
          }
          "should do that" in { fixture =>
            assert(fixture === hello)
          }
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      assert(!rep.eventsReceived.exists(_.isInstanceOf[TestFailed]))
    }
    it("should throw NullPointerException if a null test tag is provided") {
      // it
      intercept[NullPointerException] {
        new FreeSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          "hi" taggedAs(null) in { fixture => }
        }
      }
      val caught = intercept[NullPointerException] {
        new FreeSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          "hi" taggedAs(mytags.SlowAsMolasses, null) in { fixture => }
        }
      }
      assert(caught.getMessage === "a test tag was null")
      intercept[NullPointerException] {
        new FreeSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          "hi" taggedAs(mytags.SlowAsMolasses, null, mytags.WeakAsAKitten) in { fixture => }
        }
      }
      // ignore
      intercept[NullPointerException] {
        new FreeSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          "hi" taggedAs(null) ignore { fixture => }
        }
      }
      val caught2 = intercept[NullPointerException] {
        new FreeSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          "hi" taggedAs(mytags.SlowAsMolasses, null) ignore { fixture => }
        }
      }
      assert(caught2.getMessage === "a test tag was null")
      intercept[NullPointerException] {
        new FreeSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          "hi" taggedAs(mytags.SlowAsMolasses, null, mytags.WeakAsAKitten) ignore { fixture => }
        }
      }
    }
    it("should return a correct tags map from the tags method using is (pending)") {

      val a = new FreeSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = Succeeded
        "test this" ignore { fixture => }
        "test that" is (pending)
      }
      assertResult(Map("test this" -> Set("org.scalatest.Ignore"))) {
        a.tags
      }

      val b = new FreeSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = Succeeded
        "test this" is (pending)
        "test that" ignore { fixture => }
      }
      assertResult(Map("test that" -> Set("org.scalatest.Ignore"))) {
        b.tags
      }

      val c = new FreeSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = Succeeded
        "test this" ignore { fixture => }
        "test that" ignore { fixture => }
      }
      assertResult(Map("test this" -> Set("org.scalatest.Ignore"), "test that" -> Set("org.scalatest.Ignore"))) {
        c.tags
      }

      val d = new FreeSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = Succeeded
        "test this" taggedAs(mytags.SlowAsMolasses) is (pending)
        "test that" taggedAs(mytags.SlowAsMolasses) ignore { fixture => }
      }
      assertResult(Map("test this" -> Set("org.scalatest.SlowAsMolasses"), "test that" -> Set("org.scalatest.Ignore", "org.scalatest.SlowAsMolasses"))) {
        d.tags
      }

      val e = new FreeSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = Succeeded
        "test this" is (pending)
        "test that" is (pending)
      }
      assertResult(Map()) {
        e.tags
      }

      val f = new FreeSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = Succeeded
        "test this" taggedAs(mytags.SlowAsMolasses, mytags.WeakAsAKitten) is (pending)
        "test that" taggedAs(mytags.SlowAsMolasses) is (pending)
      }
      assertResult(Map("test this" -> Set("org.scalatest.SlowAsMolasses", "org.scalatest.WeakAsAKitten"), "test that" -> Set("org.scalatest.SlowAsMolasses"))) {
        f.tags
      }

      val g = new FreeSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = Succeeded
        "test this" taggedAs(mytags.SlowAsMolasses, mytags.WeakAsAKitten) is (pending)
        "test that" taggedAs(mytags.SlowAsMolasses) is (pending)
      }
      assertResult(Map("test this" -> Set("org.scalatest.SlowAsMolasses", "org.scalatest.WeakAsAKitten"), "test that" -> Set("org.scalatest.SlowAsMolasses"))) {
        g.tags
      }
    }
    class TestWasCalledSuite extends FreeSpec {
      type FixtureParam = String
      def withFixture(test: OneArgTest): Outcome = { test("hi") }
      var theTestThisCalled = false
      var theTestThatCalled = false
      "run this" in { fixture => theTestThisCalled = true }
      "run that, maybe" in { fixture => theTestThatCalled = true }
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

      val a = new FreeSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        "test this" in { fixture => theTestThisCalled = true }
        "test that" in { fixture => theTestThatCalled = true }
      }

      import scala.language.reflectiveCalls

      val repA = new TestIgnoredTrackingReporter
      a.run(None, Args(repA))
      assert(!repA.testIgnoredReceived)
      assert(a.theTestThisCalled)
      assert(a.theTestThatCalled)

      val b = new FreeSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        "test this" ignore { fixture => theTestThisCalled = true }
        "test that" in { fixture => theTestThatCalled = true }
      }

      val repB = new TestIgnoredTrackingReporter
      b.run(None, Args(repB))
      assert(repB.testIgnoredReceived)
      assert(repB.lastEvent.isDefined)
      assert(repB.lastEvent.get.testName endsWith "test this")
      assert(!b.theTestThisCalled)
      assert(b.theTestThatCalled)

      val c = new FreeSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        "test this" in { fixture => theTestThisCalled = true }
        "test that" ignore { fixture => theTestThatCalled = true }
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
      val d = new FreeSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        "test this" ignore { fixture => theTestThisCalled = true }
        "test that" ignore { fixture => theTestThatCalled = true }
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
      val e = new FreeSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        "test this" ignore { fixture => theTestThisCalled = true }
        "test that" in { fixture => theTestThatCalled = true }
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
      val a = new FreeSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        "test this" taggedAs(mytags.SlowAsMolasses) in { fixture => theTestThisCalled = true }
        "test that" in { fixture => theTestThatCalled = true }
      }

      import scala.language.reflectiveCalls

      val repA = new TestIgnoredTrackingReporter
      a.run(None, Args(repA))
      assert(!repA.testIgnoredReceived)
      assert(a.theTestThisCalled)
      assert(a.theTestThatCalled)

      // SlowAsMolasses is included, one test should be excluded
      val b = new FreeSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        "test this" taggedAs(mytags.SlowAsMolasses) in { fixture => theTestThisCalled = true }
        "test that" in { fixture => theTestThatCalled = true }
      }
      val repB = new TestIgnoredTrackingReporter
      b.run(None, Args(repB, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repB.testIgnoredReceived)
      assert(b.theTestThisCalled)
      assert(!b.theTestThatCalled)

      // SlowAsMolasses is included, and both tests should be included
      val c = new FreeSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        "test this" taggedAs(mytags.SlowAsMolasses) in { fixture => theTestThisCalled = true }
        "test that" taggedAs(mytags.SlowAsMolasses) in { fixture => theTestThatCalled = true }
      }
      val repC = new TestIgnoredTrackingReporter
      c.run(None, Args(repB, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repC.testIgnoredReceived)
      assert(c.theTestThisCalled)
      assert(c.theTestThatCalled)

      // SlowAsMolasses is included. both tests should be included but one ignored
      val d = new FreeSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        "test this" taggedAs(mytags.SlowAsMolasses) ignore { fixture => theTestThisCalled = true }
        "test that" taggedAs(mytags.SlowAsMolasses) in { fixture => theTestThatCalled = true }
      }
      val repD = new TestIgnoredTrackingReporter
      d.run(None, Args(repD, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.Ignore")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(repD.testIgnoredReceived)
      assert(!d.theTestThisCalled)
      assert(d.theTestThatCalled)

      // SlowAsMolasses included, FastAsLight excluded
      val e = new FreeSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        "test this" taggedAs(mytags.SlowAsMolasses, mytags.FastAsLight) in { fixture => theTestThisCalled = true }
        "test that" taggedAs(mytags.SlowAsMolasses) in { fixture => theTestThatCalled = true }
        "test the other" in { fixture => theTestTheOtherCalled = true }
      }
      val repE = new TestIgnoredTrackingReporter
      e.run(None, Args(repE, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
                ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repE.testIgnoredReceived)
      assert(!e.theTestThisCalled)
      assert(e.theTestThatCalled)
      assert(!e.theTestTheOtherCalled)

      // An Ignored test that was both included and excluded should not generate a TestIgnored event
      val f = new FreeSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        "test this" taggedAs(mytags.SlowAsMolasses, mytags.FastAsLight) ignore { fixture => theTestThisCalled = true }
        "test that" taggedAs(mytags.SlowAsMolasses) in { fixture => theTestThatCalled = true }
        "test the other" in { fixture => theTestTheOtherCalled = true }
      }
      val repF = new TestIgnoredTrackingReporter
      f.run(None, Args(repF, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
                ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repF.testIgnoredReceived)
      assert(!f.theTestThisCalled)
      assert(f.theTestThatCalled)
      assert(!f.theTestTheOtherCalled)

      // An Ignored test that was not included should not generate a TestIgnored event
      val g = new FreeSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        "test this" taggedAs(mytags.SlowAsMolasses, mytags.FastAsLight) in { fixture => theTestThisCalled = true }
        "test that" taggedAs(mytags.SlowAsMolasses) in { fixture => theTestThatCalled = true }
        "test the other" ignore { fixture => theTestTheOtherCalled = true }
      }
      val repG = new TestIgnoredTrackingReporter
      g.run(None, Args(repG, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
                ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repG.testIgnoredReceived)
      assert(!g.theTestThisCalled)
      assert(g.theTestThatCalled)
      assert(!g.theTestTheOtherCalled)

      // No tagsToInclude set, FastAsLight excluded
      val h = new FreeSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        "test this" taggedAs(mytags.SlowAsMolasses, mytags.FastAsLight) in { fixture => theTestThisCalled = true }
        "test that" taggedAs(mytags.SlowAsMolasses) in { fixture => theTestThatCalled = true }
        "test the other" in { fixture => theTestTheOtherCalled = true }
      }
      val repH = new TestIgnoredTrackingReporter
      h.run(None, Args(repH, Stopper.default, Filter(None, Set("org.scalatest.FastAsLight")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repH.testIgnoredReceived)
      assert(!h.theTestThisCalled)
      assert(h.theTestThatCalled)
      assert(h.theTestTheOtherCalled)

      // No tagsToInclude set, SlowAsMolasses excluded
      val i = new FreeSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        "test this" taggedAs(mytags.SlowAsMolasses, mytags.FastAsLight) in { fixture => theTestThisCalled = true }
        "test that" taggedAs(mytags.SlowAsMolasses) in { fixture => theTestThatCalled = true }
        "test the other" in { fixture => theTestTheOtherCalled = true }
      }
      val repI = new TestIgnoredTrackingReporter
      i.run(None, Args(repI, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repI.testIgnoredReceived)
      assert(!i.theTestThisCalled)
      assert(!i.theTestThatCalled)
      assert(i.theTestTheOtherCalled)

      // No tagsToInclude set, SlowAsMolasses excluded, TestIgnored should not be received on excluded ones
      val j = new FreeSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        "test this" taggedAs(mytags.SlowAsMolasses, mytags.FastAsLight) ignore { fixture => theTestThisCalled = true }
        "test that" taggedAs(mytags.SlowAsMolasses) ignore { fixture => theTestThatCalled = true }
        "test the other" in { fixture => theTestTheOtherCalled = true }
      }
      val repJ = new TestIgnoredTrackingReporter
      j.run(None, Args(repJ, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repI.testIgnoredReceived)
      assert(!j.theTestThisCalled)
      assert(!j.theTestThatCalled)
      assert(j.theTestTheOtherCalled)

      // Same as previous, except Ignore specifically mentioned in excludes set
      val k = new FreeSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        "test this" taggedAs(mytags.SlowAsMolasses, mytags.FastAsLight) ignore { fixture => theTestThisCalled = true }
        "test that" taggedAs(mytags.SlowAsMolasses) ignore { fixture => theTestThatCalled = true }
        "test the other" ignore { fixture => theTestTheOtherCalled = true }
      }
      val repK = new TestIgnoredTrackingReporter
      k.run(None, Args(repK, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses", "org.scalatest.Ignore")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(repK.testIgnoredReceived)
      assert(!k.theTestThisCalled)
      assert(!k.theTestThatCalled)
      assert(!k.theTestTheOtherCalled)
    }

    it("should return the correct test count from its expectedTestCount method") {

      val a = new FreeSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        "test this" in { fixture => }
        "test that" in { fixture => }
      }
      assert(a.expectedTestCount(Filter()) === 2)

      val b = new FreeSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        "test this" ignore { fixture => }
        "test that" in { fixture => }
      }
      assert(b.expectedTestCount(Filter()) === 1)

      val c = new FreeSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        "test this" taggedAs(mytags.FastAsLight) in { fixture => }
        "test that" in { fixture => }
      }
      assert(c.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 1)
      assert(c.expectedTestCount(Filter(None, Set("org.scalatest.FastAsLight"))) === 1)

      val d = new FreeSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        "test this" taggedAs(mytags.FastAsLight, mytags.SlowAsMolasses) in { fixture => }
        "test that" taggedAs(mytags.SlowAsMolasses) in { fixture => }
        "test the other thing" in { fixture => }
      }
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 1)
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight"))) === 1)
      assert(d.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) === 1)
      assert(d.expectedTestCount(Filter()) === 3)

      val e = new FreeSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        "test this" taggedAs(mytags.FastAsLight, mytags.SlowAsMolasses) in { fixture => }
        "test that" taggedAs(mytags.SlowAsMolasses) in { fixture => }
        "test the other thing" ignore { fixture => }
      }
      assert(e.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 1)
      assert(e.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight"))) === 1)
      assert(e.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) === 0)
      assert(e.expectedTestCount(Filter()) === 2)

      val f = new Suites(a, b, c, d, e)
      assert(f.expectedTestCount(Filter()) === 10)
    }

    it("should generate a TestPending message when the test body is (pending)") {
      val a = new FreeSpec {
        type FixtureParam = String
        val hello = "Hello, world!"
        def withFixture(test: OneArgTest): Outcome = {
          test(hello)
        }

        "should do this" is (pending)

        "should do that" in { fixture =>
          assert(fixture === hello)
        }
        "should do something else" in { fixture =>
          assert(fixture === hello)
          pending
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      val tp = rep.testPendingEventsReceived
      assert(tp.size === 2)
    }
    it("should generate a TestCanceled message when the test body includes a cancel invocation") {
      val a = new FreeSpec {
        type FixtureParam = String
        val hello = "Hello, world!"
        def withFixture(test: OneArgTest): Outcome = {
          test(hello)
        }

        "should do this" in { fixture => cancel() }

        "should do that" in { fixture =>
          assert(fixture === hello)
        }
        "should do something else" in { fixture =>
          assert(fixture === hello)
          cancel("i changed my mind")
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      val tp = rep.testCanceledEventsReceived
      assert(tp.size === 2)
    }
    it("should generate a TestCanceled message when the test body includes an assume invocation") {
      val a = new FreeSpec {
        type FixtureParam = String
        val hello = "Hello, world!"
        def withFixture(test: OneArgTest): Outcome = {
          test(hello)
        }

        "should do this" in { fixture => assume(1 + 1 === 3, "ho") }

        "should do that" in { fixture =>
          assert(fixture === hello)
        }
        "should do something else" in { fixture =>
          assert(fixture === hello)
          assume(3 === 4)
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      val tp = rep.testCanceledEventsReceived
      assert(tp.size === 2)
    }
    it("should generate a test failure if a Throwable, or an Error other than direct Error subtypes " +
            "known in JDK 1.5, excluding AssertionError") {
      val a = new FreeSpec {
        type FixtureParam = String
        val hello = "Hello, world!"
        def withFixture(test: OneArgTest): Outcome = {
          test(hello)
        }
        "This FreeSpec" - {
          "should throw AssertionError" in { s => throw new AssertionError }
          "should throw plain old Error" in { s => throw new Error }
          "should throw Throwable" in { s => throw new Throwable }
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      val tf = rep.testFailedEventsReceived
      assert(tf.size === 3)
    }
    it("should propagate out Errors that are direct subtypes of Error in JDK 1.5, other than " +
            "AssertionError, causing Suites and Runs to abort.") {
      val a = new FreeSpec {
        type FixtureParam = String
        val hello = "Hello, world!"
        def withFixture(test: OneArgTest): Outcome = {
          test(hello)
        }
        "This FreeSpec" - {
          "should throw AssertionError" in { s => throw new OutOfMemoryError }
        }
      }
      intercept[OutOfMemoryError] {
        a.run(None, Args(SilentReporter))
      }
    }
/*
    it("should send InfoProvided events with aboutAPendingTest set to true and aboutACanceledTest set to false for info " +
            "calls made from a test that is pending and not canceled") {
      val a = new FreeSpec with GivenWhenThen {
        type FixtureParam = String
        val hello = "Hello, world!"
        def withFixture(test: OneArgTest): Outcome = {
          test(hello)
        }
        "A FreeSpec" - {
          "should do something" in { s =>
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
        assert(ip.aboutACanceledTest.isDefined && !ip.aboutACanceledTest.get)
      }
      val so = rep.scopeOpenedEventsReceived
      assert(so.size === 1)
      for (event <- so) {
        assert(event.message == "A FreeSpec")
      }
      val sc = rep.scopeClosedEventsReceived
      assert(so.size === 1)
      for (event <- sc) {
        assert(event.message == "A FreeSpec")
      }
    }
    it("should send InfoProvided events with aboutAPendingTest and aboutACanceledTest both set to false for info " +
            "calls made from a test that is neither pending nor canceled") {
      val a = new FreeSpec with GivenWhenThen {
        type FixtureParam = String
        val hello = "Hello, world!"
        def withFixture(test: OneArgTest): Outcome = {
          test(hello)
        }
        "A FreeSpec" - {
          "should do something" in { s =>
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
        assert(ip.aboutACanceledTest.isDefined && !ip.aboutACanceledTest.get)
      }
      val so = rep.scopeOpenedEventsReceived
      assert(so.size === 1)
      for (event <- so) {
        assert(event.message == "A FreeSpec")
      }
      val sc = rep.scopeClosedEventsReceived
      assert(so.size === 1)
      for (event <- sc) {
        assert(event.message == "A FreeSpec")
      }
    }
    it("should send InfoProvided events with aboutAPendingTest set to false and aboutACanceledTest set to true for info " +
            "calls made from a test that is pending and not canceled") {
      val a = new FreeSpec with GivenWhenThen {
        type FixtureParam = String
        val hello = "Hello, world!"
        def withFixture(test: OneArgTest): Outcome = {
          test(hello)
        }
        "A FreeSpec" - {
          "should do something" in { s =>
            given("two integers")
            when("one is subracted from the other")
            then("the result is the difference between the two numbers")
            cancel()
          }
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      val testCanceled = rep.testCanceledEventsReceived
      assert(testCanceled.size === 1)
      val recordedEvents = testCanceled(0).recordedEvents
      assert(recordedEvents.size === 3)
      for (event <- recordedEvents) {
        val ip = event.asInstanceOf[InfoProvided]
        assert(ip.aboutAPendingTest.isDefined && !ip.aboutAPendingTest.get)
        assert(ip.aboutACanceledTest.isDefined && ip.aboutACanceledTest.get)
      }
      val so = rep.scopeOpenedEventsReceived
      assert(so.size === 1)
      for (event <- so) {
        assert(event.message == "A FreeSpec")
      }
      val sc = rep.scopeClosedEventsReceived
      assert(so.size === 1)
      for (event <- sc) {
        assert(event.message == "A FreeSpec")
      }
    }
*/
    it("should allow both tests that take fixtures and tests that don't") {
      val a = new FreeSpec {

        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = {
          test("Hello, world!")
        }

        var takesNoArgsInvoked = false
        var takesAFixtureInvoked = false

        "A FreeSpec" - {
          "should take no args" in { () => takesNoArgsInvoked = true }
          "should take a fixture" in { s => takesAFixtureInvoked = true }
        }
      }

      import scala.language.reflectiveCalls

      a.run(None, Args(SilentReporter))
      assert(a.testNames.size === 2, a.testNames)
      assert(a.takesNoArgsInvoked)
      assert(a.takesAFixtureInvoked)
    }
    it("should work with test functions whose inferred result type is not Unit") {
      val a = new FreeSpec {

        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = {
          test("Hello, world!")
        }

        var takesNoArgsInvoked = false
        var takesAFixtureInvoked = false
        "A FreeSpec" - {
          "should take no args" in { () => takesNoArgsInvoked = true; true }
          "should take a fixture" in { s => takesAFixtureInvoked = true; true }
        }
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
      val a = new FreeSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var takeNoArgsInvoked = false
        var takeAFixtureInvoked = false
        "A FreeSpec" - {
          "should take no args" ignore { () => takeNoArgsInvoked = true; "hi" }
          "should take a fixture" ignore { s => takeAFixtureInvoked = true; 42 }
        }
      }

      import scala.language.reflectiveCalls

      assert(!a.takeNoArgsInvoked)
      assert(!a.takeAFixtureInvoked)
      val reporter = new EventRecordingReporter
      a.run(None, Args(reporter))
      assert(reporter.testIgnoredEventsReceived.size === 2)
      assert(!a.takeNoArgsInvoked)
      assert(!a.takeAFixtureInvoked)
    }
    it("should pass a NoArgTest to withFixture for tests that take no fixture") {
      class MySpec extends FreeSpec {
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
        "do something" in { () =>
          assert(1 + 1 === 2)
        }
      }

      val s = new MySpec
      s.run(None, Args(SilentReporter))
      assert(s.aNoArgTestWasPassed)
      assert(!s.aOneArgTestWasPassed)
    }
    it("should not pass a NoArgTest to withFixture for tests that take a Fixture") {
      class MySpec extends FreeSpec {
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
        "do something" in { fixture =>
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

      class MySpec extends FreeSpec {
        type FixtureParam = String
        var theNoArgTestWasInvoked = false
        def withFixture(test: OneArgTest): Outcome = {
          // Shouldn't be called, but just in case don't invoke a OneArgTest
          Succeeded
        }
        "do something" in { () =>
          theNoArgTestWasInvoked = true
        }
      }

      val s = new MySpec
      s.run(None, Args(SilentReporter))
      assert(s.theNoArgTestWasInvoked)
    }
    it("should pass the correct test name in the OneArgTest passed to withFixture") {
      val a = new FreeSpec {
        type FixtureParam = String
        var correctTestNameWasPassed = false
        def withFixture(test: OneArgTest): Outcome = {
          correctTestNameWasPassed = test.name == "do something"
          test("hi")
        }
        "do something" in { fixture => }
      }

      import scala.language.reflectiveCalls

      a.run(None, Args(SilentReporter))
      assert(a.correctTestNameWasPassed)
    }
    it("should pass the correct config map in the OneArgTest passed to withFixture") {
      val a = new FreeSpec {
        type FixtureParam = String
        var correctConfigMapWasPassed = false
        def withFixture(test: OneArgTest): Outcome = {
          correctConfigMapWasPassed = (test.configMap == ConfigMap("hi" -> 7))
          test("hi")
        }
        "do something" in { fixture => }
      }

      import scala.language.reflectiveCalls

      a.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap("hi" -> 7), None, new Tracker(), Set.empty))
      assert(a.correctConfigMapWasPassed)
    }
    describe("(when a nesting rule has been violated)") {

      it("should, if they call a - from within an in clause, result in a TestFailedException when running the test") {

        class MySpec extends FreeSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = { test("hi") }
          "should blow up" in { fixture =>
            "in the wrong place, at the wrong time" - {
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceivedWithCorrectMessage(spec, "should blow up", "a \"-\" clause may not appear inside an \"in\" clause")
      }
      it("should, if they call a - with a nested in from within an in clause, result in a TestFailedException when running the test") {

        class MySpec extends FreeSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = { test("hi") }
          "should blow up" in { fixture =>
            "in the wrong place, at the wrong time" - {
              "should never run" in { fixture =>
                assert(1 === 1)
              }
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceivedWithCorrectMessage(spec, "should blow up", "a \"-\" clause may not appear inside an \"in\" clause")
      }
      it("should, if they call a nested it from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends FreeSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = { test("hi") }
          "should blow up" in { fixture =>
            "should never run" in { fixture =>
              assert(1 === 1)
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a nested it with tags from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends FreeSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = { test("hi") }
          "should blow up" in { fixture =>
            "should never run" taggedAs(mytags.SlowAsMolasses) in { fixture =>
              assert(1 === 1)
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a describe with a nested ignore from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends FreeSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = { test("hi") }
          "should blow up" in { fixture =>
            "in the wrong place, at the wrong time" - {
              "should never run" ignore { fixture =>
                assert(1 === 1)
              }
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a nested ignore from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends FreeSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = { test("hi") }
          "should blow up" in { fixture =>
            "should never run" ignore { fixture =>
              assert(1 === 1)
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a nested ignore with tags from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends FreeSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = { test("hi") }
          "should blow up" in { fixture =>
            "should never run" taggedAs(mytags.SlowAsMolasses) ignore { fixture =>
              assert(1 === 1)
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
    }

    it("should allow test registration with registerTest and registerIgnoredTest") {
      class TestSpec extends FreeSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        val a = 1
        registerTest("test 1") { fixture =>
          val e = intercept[TestFailedException] {
            assert(a == 2)
          }
          assert(e.message == Some("1 did not equal 2"))
          assert(e.failedCodeFileName == Some("FreeSpecSpec.scala"))
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

  }
  
  describe("when failure happens") {
    
    it("should fire TestFailed event with correct stack depth info when test failed") {
      class TestSpec extends FreeSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        "fail scenario" in { fixture => 
          assert(1 === 2)
        }
        "a feature" - {
          "nested fail scenario" in { fixture => 
            assert(1 === 2)
          }
        }
      }
      val rep = new EventRecordingReporter
      val s1 = new TestSpec
      s1.run(None, Args(rep))
      assert(rep.testFailedEventsReceived.size === 2)
      assert(rep.testFailedEventsReceived(0).throwable.get.asInstanceOf[TestFailedException].failedCodeFileName.get === "FreeSpecSpec.scala")
      assert(rep.testFailedEventsReceived(0).throwable.get.asInstanceOf[TestFailedException].failedCodeLineNumber.get === thisLineNumber - 13)
      assert(rep.testFailedEventsReceived(1).throwable.get.asInstanceOf[TestFailedException].failedCodeFileName.get === "FreeSpecSpec.scala")
      assert(rep.testFailedEventsReceived(1).throwable.get.asInstanceOf[TestFailedException].failedCodeLineNumber.get === thisLineNumber - 11)
    }
    
    it("should generate TestRegistrationClosedException with correct stack depth info when has an in nested inside an in") {
      class TestSpec extends FreeSpec {
        type FixtureParam = String
        var registrationClosedThrown = false
        "a feature" - {
          "a scenario" in { fixture => 
            "nested scenario" in { fixture => 
              
            }
          }
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
      assert("FreeSpecSpec.scala" === trce.failedCodeFileName.get)
      assert(trce.failedCodeLineNumber.get === thisLineNumber - 24)
      assert(trce.message == Some("An in clause may not appear inside another in clause."))
    }

    it("should generate TestRegistrationClosedException with correct stack depth info when has an ignore nested inside an in") {
      class TestSpec extends FreeSpec {
        type FixtureParam = String
        var registrationClosedThrown = false
        "a feature" - {
          "a scenario" in { fixture =>
            "nested scenario" ignore { fixture =>

            }
          }
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
      assert("FreeSpecSpec.scala" === trce.failedCodeFileName.get)
      assert(trce.failedCodeLineNumber.get === thisLineNumber - 24)
      assert(trce.message == Some("An ignore clause may not appear inside an in clause."))
    }

    it("should generate TestRegistrationClosedException with correct stack depth info when has a registerTest nested inside a registerTest") {
      class TestSpec extends FreeSpec {
        type FixtureParam = String
        var registrationClosedThrown = false
        "a feature" - {
          registerTest("a scenario") { fixture =>
            registerTest("nested scenario") { fixture =>

            }
          }
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
      assert("FreeSpecSpec.scala" === trce.failedCodeFileName.get)
      assert(trce.failedCodeLineNumber.get === thisLineNumber - 24)
      assert(trce.message == Some("Test cannot be nested inside another test."))
    }

    it("should generate TestRegistrationClosedException with correct stack depth info when has a registerIgnoredTest nested inside a registerTest") {
      class TestSpec extends FreeSpec {
        type FixtureParam = String
        var registrationClosedThrown = false
        "a feature" - {
          registerTest("a scenario") { fixture =>
            registerIgnoredTest("nested scenario") { fixture =>

            }
          }
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
      assert("FreeSpecSpec.scala" === trce.failedCodeFileName.get)
      assert(trce.failedCodeLineNumber.get === thisLineNumber - 24)
      assert(trce.message == Some("Test cannot be nested inside another test."))
    }
  }
}
