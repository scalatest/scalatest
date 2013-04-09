/*
 * Copyright 2001-2009 Artima, Inc.
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
import events.TestFailed
import org.scalatest.exceptions.DuplicateTestNameException
import org.scalatest.events.InfoProvided

class FixtureSpecSpec extends org.scalatest.FunSpec with PrivateMethodTester with SharedHelpers {

  describe("A FixtureSpec") {

    it("should return the test names in order of registration from testNames") {
      val a = new FixtureSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = Succeeded
        it("should do that") { fixture =>
        }
        it("should do this") { fixture =>
        }
      }

      assertResult(List("should do that", "should do this")) {
        a.testNames.iterator.toList
      }

      val b = new FixtureSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = Succeeded
      }

      assertResult(List[String]()) {
        b.testNames.iterator.toList
      }

      val c = new FixtureSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = Succeeded
        it("should do this") { fixture =>
        }
        it("should do that") { fixture =>
        }
      }

      assertResult(List("should do this", "should do that")) {
        c.testNames.iterator.toList
      }
    }

    it("should throw NotAllowedException if a duplicate test name registration is attempted") {

      intercept[DuplicateTestNameException] {
        new FixtureSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          it("test this") { fixture =>
          }
          it("test this") { fixture =>
          }
        }
      }
      intercept[DuplicateTestNameException] {
        new FixtureSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          it("test this") { fixture =>
          }
          ignore("test this") { fixture =>
          }
        }
      }
      intercept[DuplicateTestNameException] {
        new FixtureSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          ignore("test this") { fixture =>
          }
          ignore("test this") { fixture =>
          }
        }
      }
      intercept[DuplicateTestNameException] {
        new FixtureSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          ignore("test this") { fixture =>
          }
          it("test this") { fixture =>
          }
        }
      }
    }

    it("should pass in the fixture to every test method") {
      val a = new FixtureSpec {
        type FixtureParam = String
        val hello = "Hello, world!"
        def withFixture(test: OneArgTest): Outcome = {
          test(hello)
        }
        it("should do this") { fixture =>
          assert(fixture === hello)
        }
        it("should do that") { fixture =>
          assert(fixture === hello)
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      assert(!rep.eventsReceived.exists(_.isInstanceOf[TestFailed]))
    }
    it("should throw NullPointerException if a null test tag is provided") {
      // it
      intercept[NullPointerException] {
        new FixtureSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          it("hi", null) { fixture => }
        }
      }
      val caught = intercept[NullPointerException] {
        new FixtureSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          it("hi", mytags.SlowAsMolasses, null) { fixture => }
        }
      }
      assert(caught.getMessage === "a test tag was null")
      intercept[NullPointerException] {
        new FixtureSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          it("hi", mytags.SlowAsMolasses, null, mytags.WeakAsAKitten) { fixture => }
        }
      }
      // ignore
      intercept[NullPointerException] {
        new FixtureSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          ignore("hi", null) { fixture => }
        }
      }
      val caught2 = intercept[NullPointerException] {
        new FixtureSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          ignore("hi", mytags.SlowAsMolasses, null) { fixture => }
        }
      }
      assert(caught2.getMessage === "a test tag was null")
      intercept[NullPointerException] {
        new FixtureSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          ignore("hi", mytags.SlowAsMolasses, null, mytags.WeakAsAKitten) { fixture => }
        }
      }
    }
    it("should return a correct tags map from the tags method") {

      val a = new FixtureSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = Succeeded
        ignore("test this") { fixture => }
        it("test that") { fixture => }
      }
      assertResult(Map("test this" -> Set("org.scalatest.Ignore"))) {
        a.tags
      }

      val b = new FixtureSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = Succeeded
        it("test this") { fixture => }
        ignore("test that") { fixture => }
      }
      assertResult(Map("test that" -> Set("org.scalatest.Ignore"))) {
        b.tags
      }

      val c = new FixtureSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = Succeeded
        ignore("test this") { fixture => }
        ignore("test that") { fixture => }
      }
      assertResult(Map("test this" -> Set("org.scalatest.Ignore"), "test that" -> Set("org.scalatest.Ignore"))) {
        c.tags
      }

      val d = new FixtureSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = Succeeded
        it("test this", mytags.SlowAsMolasses) { fixture => }
        ignore("test that", mytags.SlowAsMolasses) { fixture => }
      }
      assertResult(Map("test this" -> Set("org.scalatest.SlowAsMolasses"), "test that" -> Set("org.scalatest.Ignore", "org.scalatest.SlowAsMolasses"))) {
        d.tags
      }

      val e = new FixtureSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = Succeeded
      }
      assertResult(Map()) {
        e.tags
      }

      val f = new FixtureSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = Succeeded
        it("test this", mytags.SlowAsMolasses, mytags.WeakAsAKitten) { fixture => }
        it("test that", mytags.SlowAsMolasses) { fixture => }
      }
      assertResult(Map("test this" -> Set("org.scalatest.SlowAsMolasses", "org.scalatest.WeakAsAKitten"), "test that" -> Set("org.scalatest.SlowAsMolasses"))) {
        f.tags
      }
    }

    class TestWasCalledSuite extends FixtureSpec {
      type FixtureParam = String
      def withFixture(test: OneArgTest): Outcome = { test("hi") }
      var theTestThisCalled = false
      var theTestThatCalled = false
      it("should run this") { fixture => theTestThisCalled = true }
      it("should run that, maybe") { fixture => theTestThatCalled = true }
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

      val a = new FixtureSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        it("test this") { fixture => theTestThisCalled = true }
        it("test that") { fixture => theTestThatCalled = true }
      }

      val repA = new TestIgnoredTrackingReporter
      a.run(None, Args(repA))
      assert(!repA.testIgnoredReceived)
      assert(a.theTestThisCalled)
      assert(a.theTestThatCalled)

      val b = new FixtureSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        ignore("test this") { fixture => theTestThisCalled = true }
        it("test that") { fixture => theTestThatCalled = true }
      }

      val repB = new TestIgnoredTrackingReporter
      b.run(None, Args(repB))
      assert(repB.testIgnoredReceived)
      assert(repB.lastEvent.isDefined)
      assert(repB.lastEvent.get.testName endsWith "test this")
      assert(!b.theTestThisCalled)
      assert(b.theTestThatCalled)

      val c = new FixtureSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        it("test this") { fixture => theTestThisCalled = true }
        ignore("test that") { fixture => theTestThatCalled = true }
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
      val d = new FixtureSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        ignore("test this") { fixture => theTestThisCalled = true }
        ignore("test that") { fixture => theTestThatCalled = true }
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
      val e = new FixtureSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        ignore("test this") { fixture => theTestThisCalled = true }
        it("test that") { fixture => theTestThatCalled = true }
      }

      val repE = new TestIgnoredTrackingReporter
      e.run(Some("test this"), Args(repE))
      assert(repE.testIgnoredReceived)
      assert(!e.theTestThisCalled)
      assert(!e.theTestThatCalled)
    }

    it("should run only those tests selected by the tags to include and exclude sets") {

      // Nothing is excluded
      val a = new FixtureSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        it("test this", mytags.SlowAsMolasses) { fixture => theTestThisCalled = true }
        it("test that") { fixture => theTestThatCalled = true }
      }
      val repA = new TestIgnoredTrackingReporter
      a.run(None, Args(repA))
      assert(!repA.testIgnoredReceived)
      assert(a.theTestThisCalled)
      assert(a.theTestThatCalled)

      // SlowAsMolasses is included, one test should be excluded
      val b = new FixtureSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        it("test this", mytags.SlowAsMolasses) { fixture => theTestThisCalled = true }
        it("test that") { fixture => theTestThatCalled = true }
      }
      val repB = new TestIgnoredTrackingReporter
      b.run(None, Args(repB, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repB.testIgnoredReceived)
      assert(b.theTestThisCalled)
      assert(!b.theTestThatCalled)

      // SlowAsMolasses is included, and both tests should be included
      val c = new FixtureSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        it("test this", mytags.SlowAsMolasses) { fixture => theTestThisCalled = true }
        it("test that", mytags.SlowAsMolasses) { fixture => theTestThatCalled = true }
      }
      val repC = new TestIgnoredTrackingReporter
      c.run(None, Args(repB, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repC.testIgnoredReceived)
      assert(c.theTestThisCalled)
      assert(c.theTestThatCalled)

      // SlowAsMolasses is included. both tests should be included but one ignored
      val d = new FixtureSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        ignore("test this", mytags.SlowAsMolasses) { fixture => theTestThisCalled = true }
        it("test that", mytags.SlowAsMolasses) { fixture => theTestThatCalled = true }
      }
      val repD = new TestIgnoredTrackingReporter
      d.run(None, Args(repD, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.Ignore")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(repD.testIgnoredReceived)
      assert(!d.theTestThisCalled)
      assert(d.theTestThatCalled)

      // SlowAsMolasses included, FastAsLight excluded
      val e = new FixtureSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        it("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { fixture => theTestThisCalled = true }
        it("test that", mytags.SlowAsMolasses) { fixture => theTestThatCalled = true }
        it("test the other") { fixture => theTestTheOtherCalled = true }
      }
      val repE = new TestIgnoredTrackingReporter
      e.run(None, Args(repE, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
                ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repE.testIgnoredReceived)
      assert(!e.theTestThisCalled)
      assert(e.theTestThatCalled)
      assert(!e.theTestTheOtherCalled)

      // An Ignored test that was both included and excluded should not generate a TestIgnored event
      val f = new FixtureSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        ignore("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { fixture => theTestThisCalled = true }
        it("test that", mytags.SlowAsMolasses) { fixture => theTestThatCalled = true }
        it("test the other") { fixture => theTestTheOtherCalled = true }
      }
      val repF = new TestIgnoredTrackingReporter
      f.run(None, Args(repF, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
                ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repF.testIgnoredReceived)
      assert(!f.theTestThisCalled)
      assert(f.theTestThatCalled)
      assert(!f.theTestTheOtherCalled)

      // An Ignored test that was not included should not generate a TestIgnored event
      val g = new FixtureSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        it("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { fixture => theTestThisCalled = true }
        it("test that", mytags.SlowAsMolasses) { fixture => theTestThatCalled = true }
        ignore("test the other") { fixture => theTestTheOtherCalled = true }
      }
      val repG = new TestIgnoredTrackingReporter
      g.run(None, Args(repG, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
                ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repG.testIgnoredReceived)
      assert(!g.theTestThisCalled)
      assert(g.theTestThatCalled)
      assert(!g.theTestTheOtherCalled)

      // No tagsToInclude set, FastAsLight excluded
      val h = new FixtureSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        it("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { fixture => theTestThisCalled = true }
        it("test that", mytags.SlowAsMolasses) { fixture => theTestThatCalled = true }
        it("test the other") { fixture => theTestTheOtherCalled = true }
      }
      val repH = new TestIgnoredTrackingReporter
      h.run(None, Args(repH, Stopper.default, Filter(None, Set("org.scalatest.FastAsLight")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repH.testIgnoredReceived)
      assert(!h.theTestThisCalled)
      assert(h.theTestThatCalled)
      assert(h.theTestTheOtherCalled)

      // No tagsToInclude set, SlowAsMolasses excluded
      val i = new FixtureSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        it("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { fixture => theTestThisCalled = true }
        it("test that", mytags.SlowAsMolasses) { fixture => theTestThatCalled = true }
        it("test the other") { fixture => theTestTheOtherCalled = true }
      }
      val repI = new TestIgnoredTrackingReporter
      i.run(None, Args(repI, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repI.testIgnoredReceived)
      assert(!i.theTestThisCalled)
      assert(!i.theTestThatCalled)
      assert(i.theTestTheOtherCalled)

      // No tagsToInclude set, SlowAsMolasses excluded, TestIgnored should not be received on excluded ones
      val j = new FixtureSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        ignore("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { fixture => theTestThisCalled = true }
        ignore("test that", mytags.SlowAsMolasses) { fixture => theTestThatCalled = true }
        it("test the other") { fixture => theTestTheOtherCalled = true }
      }
      val repJ = new TestIgnoredTrackingReporter
      j.run(None, Args(repJ, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repI.testIgnoredReceived)
      assert(!j.theTestThisCalled)
      assert(!j.theTestThatCalled)
      assert(j.theTestTheOtherCalled)

      // Same as previous, except Ignore specifically mentioned in excludes set
      val k = new FixtureSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        ignore("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { fixture => theTestThisCalled = true }
        ignore("test that", mytags.SlowAsMolasses) { fixture => theTestThatCalled = true }
        ignore("test the other") { fixture => theTestTheOtherCalled = true }
      }
      val repK = new TestIgnoredTrackingReporter
      k.run(None, Args(repK, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses", "org.scalatest.Ignore")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(repK.testIgnoredReceived)
      assert(!k.theTestThisCalled)
      assert(!k.theTestThatCalled)
      assert(!k.theTestTheOtherCalled)
    }

    it("should return the correct test count from its expectedTestCount method") {

      val a = new FixtureSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        it("test this") { fixture => }
        it("test that") { fixture => }
      }
      assert(a.expectedTestCount(Filter()) === 2)

      val b = new FixtureSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        ignore("test this") { fixture => }
        it("test that") { fixture => }
      }
      assert(b.expectedTestCount(Filter()) === 1)

      val c = new FixtureSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        it("test this", mytags.FastAsLight) { fixture => }
        it("test that") { fixture => }
      }
      assert(c.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 1)
      assert(c.expectedTestCount(Filter(None, Set("org.scalatest.FastAsLight"))) === 1)

      val d = new FixtureSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        it("test this", mytags.FastAsLight, mytags.SlowAsMolasses) { fixture => }
        it("test that", mytags.SlowAsMolasses) { fixture => }
        it("test the other thing") { fixture => }
      }
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 1)
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight"))) === 1)
      assert(d.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) === 1)
      assert(d.expectedTestCount(Filter()) === 3)

      val e = new FixtureSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        it("test this", mytags.FastAsLight, mytags.SlowAsMolasses) { fixture => }
        it("test that", mytags.SlowAsMolasses) { fixture => }
        ignore("test the other thing") { fixture => }
      }
      assert(e.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 1)
      assert(e.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight"))) === 1)
      assert(e.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) === 0)
      assert(e.expectedTestCount(Filter()) === 2)

      val f = new Suites(a, b, c, d, e)
      assert(f.expectedTestCount(Filter()) === 10)
    }
    it("should generate a TestPending message when the test body is (pending)") {
      val a = new FixtureSpec {
        type FixtureParam = String
        val hello = "Hello, world!"
        def withFixture(test: OneArgTest): Outcome = {
          test(hello)
        }

        it("should do this") (pending)

        it("should do that") { fixture =>
          assert(fixture === hello)
        }
        it("should do something else") { fixture =>
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
      val a = new FixtureSpec {
        type FixtureParam = String
        val hello = "Hello, world!"
        def withFixture(test: OneArgTest): Outcome = {
          test(hello)
        }
        it("throws AssertionError") { s => throw new AssertionError }
        it("throws plain old Error") { s => throw new Error }
        it("throws Throwable") { s => throw new Throwable }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      val tf = rep.testFailedEventsReceived
      assert(tf.size === 3)
    }
    it("should propagate out Errors that are direct subtypes of Error in JDK 1.5, other than " +
            "AssertionError, causing Suites and Runs to abort.") {
      val a = new FixtureSpec {
        type FixtureParam = String
        val hello = "Hello, world!"
        def withFixture(test: OneArgTest): Outcome = {
          test(hello)
        }
        it("throws AssertionError") { s => throw new OutOfMemoryError }
      }
      intercept[OutOfMemoryError] {
        a.run(None, Args(SilentReporter))
      }
    }
/*
    it("should send InfoProvided events with aboutAPendingTest set to true for info " +
            "calls made from a test that is pending") {
      val a = new FixtureSpec with GivenWhenThen {
        type FixtureParam = String
        val hello = "Hello, world!"
        def withFixture(test: OneArgTest): Outcome = {
          test(hello)
        }
        it("should do something else") { s =>
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
      val a = new FixtureSpec with GivenWhenThen {
        type FixtureParam = String
        val hello = "Hello, world!"
        def withFixture(test: OneArgTest): Outcome = {
          test(hello)
        }
        it("should do something else") { s =>
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
      val a = new FixtureSpec {

        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = {
          test("Hello, world!")
        }

        var takesNoArgsInvoked = false
        it("take no args") { () => takesNoArgsInvoked = true }

        var takesAFixtureInvoked = false
        it("takes a fixture") { s => takesAFixtureInvoked = true }
      }

      a.run(None, Args(SilentReporter))
      assert(a.testNames.size === 2, a.testNames)
      assert(a.takesNoArgsInvoked)
      assert(a.takesAFixtureInvoked)
    }
    it("should work with test functions whose inferred result type is not Unit") {
      val a = new FixtureSpec {

        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = {
          test("Hello, world!")
        }

        var takesNoArgsInvoked = false
        it("should take no args") { () => takesNoArgsInvoked = true; true }

        var takesAFixtureInvoked = false
        it("should take a fixture") { s => takesAFixtureInvoked = true; true }
      }

      assert(!a.takesNoArgsInvoked)
      assert(!a.takesAFixtureInvoked)
      a.run(None, Args(SilentReporter))
      assert(a.testNames.size === 2, a.testNames)
      assert(a.takesNoArgsInvoked)
      assert(a.takesAFixtureInvoked)
    }
    it("should work with ignored tests whose inferred result type is not Unit") {
      val a = new FixtureSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        ignore("should test this") { () => theTestThisCalled = true; "hi" }
        ignore("should test that") { fixture => theTestThatCalled = true; 42 }
      }

      assert(!a.theTestThisCalled)
      assert(!a.theTestThatCalled)
      val reporter = new EventRecordingReporter
      a.run(None, Args(reporter))
      assert(reporter.testIgnoredEventsReceived.size === 2)
      assert(!a.theTestThisCalled)
      assert(!a.theTestThatCalled)
    }
    it("should pass a NoArgTest to withFixture for tests that take no fixture") {
      class MySpec extends FixtureSpec {
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
        it("something") { () =>
          assert(1 + 1 === 2)
        }
      }

      val s = new MySpec
      s.run(None, Args(SilentReporter))
      assert(s.aNoArgTestWasPassed)
      assert(!s.aOneArgTestWasPassed)
    }
    it("should not pass a NoArgTest to withFixture for tests that take a Fixture") {
      class MySpec extends FixtureSpec {
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
        it("something") { fixture =>
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

      class MySpec extends FixtureSpec {
        type FixtureParam = String
        var theNoArgTestWasInvoked = false
        def withFixture(test: OneArgTest): Outcome = {
          // Shouldn't be called, but just in case don't invoke a OneArgTest
          Succeeded
        }
        it("something") { () =>
          theNoArgTestWasInvoked = true
        }
      }

      val s = new MySpec
      s.run(None, Args(SilentReporter))
      assert(s.theNoArgTestWasInvoked)
    }
    it("should pass the correct test name in the OneArgTest passed to withFixture") {
      val a = new FixtureSpec {
        type FixtureParam = String
        var correctTestNameWasPassed = false
        def withFixture(test: OneArgTest): Outcome = {
          correctTestNameWasPassed = test.name == "should do something"
          test("hi")
        }
        it("should do something") { fixture => }
      }
      a.run(None, Args(SilentReporter))
      assert(a.correctTestNameWasPassed)
    }
    it("should pass the correct config map in the OneArgTest passed to withFixture") {
      val a = new FixtureSpec {
        type FixtureParam = String
        var correctConfigMapWasPassed = false
        def withFixture(test: OneArgTest): Outcome = {
          correctConfigMapWasPassed = (test.configMap == ConfigMap("hi" -> 7))
          test("hi")
        }
        it("should do something") { fixture => }
      }
      a.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap("hi" -> 7), None, new Tracker(), Set.empty))
      assert(a.correctConfigMapWasPassed)
    }
    describe("(when a nesting rule has been violated)") {

      it("should, if they call a describe from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends FixtureSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = { test("hi") }
          it("should blow up") { fixture =>
            describe("in the wrong place, at the wrong time") {
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a describe with a nested it from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends FixtureSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = { test("hi") }
          it("should blow up") { fixture =>
            describe("in the wrong place, at the wrong time") {
              it("should never run") { fixture =>
                assert(1 === 1)
              }
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a nested it from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends FixtureSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = { test("hi") }
          it("should blow up") { fixture =>
            it("should never run") { fixture =>
              assert(1 === 1)
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a nested it with tags from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends FixtureSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = { test("hi") }
          it("should blow up") { fixture =>
            it("should never run", mytags.SlowAsMolasses) { fixture =>
              assert(1 === 1)
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a describe with a nested ignore from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends FixtureSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = { test("hi") }
          it("should blow up") { fixture =>
            describe("in the wrong place, at the wrong time") {
              ignore("should never run") { fixture =>
                assert(1 === 1)
              }
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a nested ignore from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends FixtureSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = { test("hi") }
          it("should blow up") { fixture =>
            ignore("should never run") { fixture =>
              assert(1 === 1)
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a nested ignore with tags from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends FixtureSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = { test("hi") }
          it("should blow up") { fixture =>
            ignore("should never run", mytags.SlowAsMolasses) { fixture =>
              assert(1 === 1)
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
    }
  }
}
