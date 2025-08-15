/*
 * Copyright 2001-2025 Artima, Inc.
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
package org.scalatest.propspec

import org.scalatest._
import SharedHelpers._
import events.TestFailed
import org.scalactic.exceptions.NullArgumentException
import org.scalatest.exceptions.DuplicateTestNameException
import org.scalatest.exceptions.TestFailedException
import org.scalatest.exceptions.TestRegistrationClosedException
import org.scalatest
import org.scalatest.propspec

class FixtureAnyPropSpecSpec extends scalatest.funspec.AnyFunSpec {

  describe("A fixture.PropSpec") {
    it("should return the test names in order of registration from testNames") {
      val a = new propspec.FixtureAnyPropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = Succeeded
        property("that") { fixture =>
          /* ASSERTION_SUCCEED */
        }
        property("this") { fixture =>
          /* ASSERTION_SUCCEED */
        }
      }

      assertResult(List("that", "this")) {
        a.testNames.iterator.toList
      }

      val b = new propspec.FixtureAnyPropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = Succeeded
      }

      assertResult(List[String]()) {
        b.testNames.iterator.toList
      }

      val c = new propspec.FixtureAnyPropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = Succeeded
        property("this") { fixture =>
          /* ASSERTION_SUCCEED */
        }
        property("that") { fixture =>
          /* ASSERTION_SUCCEED */
        }
      }

      assertResult(List("this", "that")) {
        c.testNames.iterator.toList
      }
    }

    it("should throw NotAllowedException if a duplicate test name registration is attempted") {

      intercept[DuplicateTestNameException] {
        new propspec.FixtureAnyPropSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          property("test this") { fixture =>
            /* ASSERTION_SUCCEED */
          }
          property("test this") { fixture =>
            /* ASSERTION_SUCCEED */
          }
        }
      }
      intercept[DuplicateTestNameException] {
        new propspec.FixtureAnyPropSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          property("test this") { fixture =>
            /* ASSERTION_SUCCEED */
          }
          ignore("test this") { fixture =>
            /* ASSERTION_SUCCEED */
          }
        }
      }
      intercept[DuplicateTestNameException] {
        new propspec.FixtureAnyPropSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          ignore("test this") { fixture =>
            /* ASSERTION_SUCCEED */
          }
          ignore("test this") { fixture =>
            /* ASSERTION_SUCCEED */
          }
        }
      }
      intercept[DuplicateTestNameException] {
        new propspec.FixtureAnyPropSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          ignore("test this") { fixture =>
            /* ASSERTION_SUCCEED */
          }
          property("test this") { fixture =>
            /* ASSERTION_SUCCEED */
          }
        }
      }
    }

    it("should pass in the fixture to every test method") {
      val a = new propspec.FixtureAnyPropSpec {
        type FixtureParam = String
        val hello = "Hello, world!"
        def withFixture(test: OneArgTest): Outcome = {
          test(hello)
        }
        property("this") { fixture =>
          assert(fixture === hello)
        }
        property("that") { fixture =>
          assert(fixture === hello)
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      assert(!rep.eventsReceived.exists(_.isInstanceOf[TestFailed]))
    }

    it("should throw NullArgumentException if a null test tag is provided") {
      // test
      intercept[NullArgumentException] {
        new propspec.FixtureAnyPropSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          property("hi", null) { fixture => /* ASSERTION_SUCCEED */ }
        }
      }
      val caught = intercept[NullArgumentException] {
        new propspec.FixtureAnyPropSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          property("hi", mytags.SlowAsMolasses, null) { fixture => /* ASSERTION_SUCCEED */ }
        }
      }
      assert(caught.getMessage === "a test tag was null")
      intercept[NullArgumentException] {
        new propspec.FixtureAnyPropSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          property("hi", mytags.SlowAsMolasses, null, mytags.WeakAsAKitten) { fixture => /* ASSERTION_SUCCEED */ }
        }
      }

      // ignore
      intercept[NullArgumentException] {
        new propspec.FixtureAnyPropSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          ignore("hi", null) { fixture => /* ASSERTION_SUCCEED */ }
        }
      }
      val caught2 = intercept[NullArgumentException] {
        new propspec.FixtureAnyPropSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          ignore("hi", mytags.SlowAsMolasses, null) { fixture => /* ASSERTION_SUCCEED */ }
        }
      }
      assert(caught2.getMessage === "a test tag was null")
      intercept[NullArgumentException] {
        new propspec.FixtureAnyPropSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          ignore("hi", mytags.SlowAsMolasses, null, mytags.WeakAsAKitten) { fixture => /* ASSERTION_SUCCEED */ }
        }
      }

      // registerTest
      intercept[NullArgumentException] {
        new propspec.FixtureAnyPropSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          registerTest("hi", null) { fixture => /* ASSERTION_SUCCEED */ }
        }
      }
      val caught3 = intercept[NullArgumentException] {
        new propspec.FixtureAnyPropSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          registerTest("hi", mytags.SlowAsMolasses, null) { fixture => /* ASSERTION_SUCCEED */ }
        }
      }
      assert(caught3.getMessage === "a test tag was null")
      intercept[NullArgumentException] {
        new propspec.FixtureAnyPropSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          registerTest("hi", mytags.SlowAsMolasses, null, mytags.WeakAsAKitten) { fixture => /* ASSERTION_SUCCEED */ }
        }
      }

      // registerIgnoredTest
      intercept[NullArgumentException] {
        new propspec.FixtureAnyPropSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          registerIgnoredTest("hi", null) { fixture => /* ASSERTION_SUCCEED */ }
        }
      }
      val caught4 = intercept[NullArgumentException] {
        new propspec.FixtureAnyPropSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          registerIgnoredTest("hi", mytags.SlowAsMolasses, null) { fixture => /* ASSERTION_SUCCEED */ }
        }
      }
      assert(caught4.getMessage === "a test tag was null")
      intercept[NullArgumentException] {
        new propspec.FixtureAnyPropSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = Succeeded
          registerIgnoredTest("hi", mytags.SlowAsMolasses, null, mytags.WeakAsAKitten) { fixture => /* ASSERTION_SUCCEED */ }
        }
      }
    }
    
    class TestWasCalledSuite extends propspec.FixtureAnyPropSpec {
      type FixtureParam = String
      def withFixture(test: OneArgTest): Outcome = { test("hi") }
      var theTestThisCalled = false
      var theTestThatCalled = false
      property("this") { fixture => theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
      property("that") { fixture => theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
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

      class SpecA extends propspec.FixtureAnyPropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        property("test this") { fixture => theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        property("test that") { fixture => theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val a = new SpecA

      import scala.language.reflectiveCalls

      val repA = new TestIgnoredTrackingReporter
      a.run(None, Args(repA))
      assert(!repA.testIgnoredReceived)
      assert(a.theTestThisCalled)
      assert(a.theTestThatCalled)

      class SpecB extends propspec.FixtureAnyPropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        ignore("test this") { fixture => theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        property("test that") { fixture => theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val b = new SpecB

      val repB = new TestIgnoredTrackingReporter
      b.run(None, Args(repB))
      assert(repB.testIgnoredReceived)
      assert(repB.lastEvent.isDefined)
      assert(repB.lastEvent.get.testName endsWith "test this")
      assert(!b.theTestThisCalled)
      assert(b.theTestThatCalled)

      class SpecC extends propspec.FixtureAnyPropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        property("test this") { fixture => theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        ignore("test that") { fixture => theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val c = new SpecC

      val repC = new TestIgnoredTrackingReporter
      c.run(None, Args(repC))
      assert(repC.testIgnoredReceived)
      assert(repC.lastEvent.isDefined)
      assert(repC.lastEvent.get.testName endsWith "test that", repC.lastEvent.get.testName)
      assert(c.theTestThisCalled)
      assert(!c.theTestThatCalled)

      // The order I want is order of appearance in the file.
      // Will try and implement that tomorrow. Subtypes will be able to change the order.
      class SpecD extends propspec.FixtureAnyPropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        ignore("test this") { fixture => theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        ignore("test that") { fixture => theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val d = new SpecD

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
      class SpecE extends propspec.FixtureAnyPropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        ignore("test this") { fixture => theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        property("test that") { fixture => theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val e = new SpecE

      import scala.language.reflectiveCalls

      val repE = new TestIgnoredTrackingReporter
      e.run(Some("test this"), Args(repE))
      assert(repE.testIgnoredReceived)
      assert(!e.theTestThisCalled)
      assert(!e.theTestThatCalled)
    }

    it("should run only those tests selected by the tags to include and exclude sets") {

      // Nothing is excluded
      class SpecA extends propspec.FixtureAnyPropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        property("test this", mytags.SlowAsMolasses) { fixture => theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        property("test that") { fixture => theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val a = new SpecA

      import scala.language.reflectiveCalls

      val repA = new TestIgnoredTrackingReporter
      a.run(None, Args(repA))
      assert(!repA.testIgnoredReceived)
      assert(a.theTestThisCalled)
      assert(a.theTestThatCalled)

      // SlowAsMolasses is included, one test should be excluded
      class SpecB extends propspec.FixtureAnyPropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        property("test this", mytags.SlowAsMolasses) { fixture => theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        property("test that") { fixture => theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val b = new SpecB
      val repB = new TestIgnoredTrackingReporter
      b.run(None, Args(repB, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), ConfigMap.empty, None, new Tracker))
      assert(!repB.testIgnoredReceived)
      assert(b.theTestThisCalled)
      assert(!b.theTestThatCalled)

      // SlowAsMolasses is included, and both tests should be included
      class SpecC extends propspec.FixtureAnyPropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        property("test this", mytags.SlowAsMolasses) { fixture => theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        property("test that", mytags.SlowAsMolasses) { fixture => theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val c = new SpecC
      val repC = new TestIgnoredTrackingReporter
      c.run(None, Args(repB, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), ConfigMap.empty, None, new Tracker))
      assert(!repC.testIgnoredReceived)
      assert(c.theTestThisCalled)
      assert(c.theTestThatCalled)

      // SlowAsMolasses is included. both tests should be included but one ignored
      class SpecD extends propspec.FixtureAnyPropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        ignore("test this", mytags.SlowAsMolasses) { fixture => theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        property("test that", mytags.SlowAsMolasses) { fixture => theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val d = new SpecD
      val repD = new TestIgnoredTrackingReporter
      d.run(None, Args(repD, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.Ignore")), ConfigMap.empty, None, new Tracker))
      assert(repD.testIgnoredReceived)
      assert(!d.theTestThisCalled)
      assert(d.theTestThatCalled)

      // SlowAsMolasses included, FastAsLight excluded
      class SpecE extends propspec.FixtureAnyPropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        property("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { fixture => theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        property("test that", mytags.SlowAsMolasses) { fixture => theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        property("test the other") { fixture => theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val e = new SpecE
      val repE = new TestIgnoredTrackingReporter
      e.run(None, Args(repE, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
                ConfigMap.empty, None, new Tracker))
      assert(!repE.testIgnoredReceived)
      assert(!e.theTestThisCalled)
      assert(e.theTestThatCalled)
      assert(!e.theTestTheOtherCalled)

      // An Ignored test that was both included and excluded should not generate a TestIgnored event
      class SpecF extends propspec.FixtureAnyPropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        ignore("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { fixture => theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        property("test that", mytags.SlowAsMolasses) { fixture => theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        property("test the other") { fixture => theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val f = new SpecF
      val repF = new TestIgnoredTrackingReporter
      f.run(None, Args(repF, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
                ConfigMap.empty, None, new Tracker))
      assert(!repF.testIgnoredReceived)
      assert(!f.theTestThisCalled)
      assert(f.theTestThatCalled)
      assert(!f.theTestTheOtherCalled)

      // An Ignored test that was not included should not generate a TestIgnored event
      class SpecG extends propspec.FixtureAnyPropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        property("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { fixture => theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        property("test that", mytags.SlowAsMolasses) { fixture => theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        ignore("test the other") { fixture => theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val g = new SpecG
      val repG = new TestIgnoredTrackingReporter
      g.run(None, Args(repG, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
                ConfigMap.empty, None, new Tracker))
      assert(!repG.testIgnoredReceived)
      assert(!g.theTestThisCalled)
      assert(g.theTestThatCalled)
      assert(!g.theTestTheOtherCalled)

      // No tagsToInclude set, FastAsLight excluded
      class SpecH extends propspec.FixtureAnyPropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        property("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { fixture => theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        property("test that", mytags.SlowAsMolasses) { fixture => theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        property("test the other") { fixture => theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val h = new SpecH
      val repH = new TestIgnoredTrackingReporter
      h.run(None, Args(repH, Stopper.default, Filter(None, Set("org.scalatest.FastAsLight")), ConfigMap.empty, None, new Tracker))
      assert(!repH.testIgnoredReceived)
      assert(!h.theTestThisCalled)
      assert(h.theTestThatCalled)
      assert(h.theTestTheOtherCalled)

      // No tagsToInclude set, SlowAsMolasses excluded
      class SpecI extends propspec.FixtureAnyPropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        property("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { fixture => theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        property("test that", mytags.SlowAsMolasses) { fixture => theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        property("test the other") { fixture => theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val i = new SpecI
      val repI = new TestIgnoredTrackingReporter
      i.run(None, Args(repI, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses")), ConfigMap.empty, None, new Tracker))
      assert(!repI.testIgnoredReceived)
      assert(!i.theTestThisCalled)
      assert(!i.theTestThatCalled)
      assert(i.theTestTheOtherCalled)

      // No tagsToInclude set, SlowAsMolasses excluded, TestIgnored should not be received on excluded ones
      class SpecJ extends propspec.FixtureAnyPropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        ignore("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { fixture => theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        ignore("test that", mytags.SlowAsMolasses) { fixture => theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        property("test the other") { fixture => theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val j = new SpecJ
      val repJ = new TestIgnoredTrackingReporter
      j.run(None, Args(repJ, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses")), ConfigMap.empty, None, new Tracker))
      assert(!repI.testIgnoredReceived)
      assert(!j.theTestThisCalled)
      assert(!j.theTestThatCalled)
      assert(j.theTestTheOtherCalled)

      // Same as previous, except Ignore specifically mentioned in excludes set
      class SpecK extends propspec.FixtureAnyPropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        ignore("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { fixture => theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        ignore("test that", mytags.SlowAsMolasses) { fixture => theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        ignore("test the other") { fixture => theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val k = new SpecK
      val repK = new TestIgnoredTrackingReporter
      k.run(None, Args(repK, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses", "org.scalatest.Ignore")), ConfigMap.empty, None, new Tracker))
      assert(repK.testIgnoredReceived)
      assert(!k.theTestThisCalled)
      assert(!k.theTestThatCalled)
      assert(!k.theTestTheOtherCalled)
    }

    it("should run only those registered tests selected by the tags to include and exclude sets") {

      // Nothing is excluded
      class SpecA extends propspec.FixtureAnyPropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        registerTest("test this", mytags.SlowAsMolasses) { fixture => theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test that") { fixture => theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val a = new SpecA

      import scala.language.reflectiveCalls

      val repA = new TestIgnoredTrackingReporter
      a.run(None, Args(repA))
      assert(!repA.testIgnoredReceived)
      assert(a.theTestThisCalled)
      assert(a.theTestThatCalled)

      // SlowAsMolasses is included, one test should be excluded
      class SpecB extends propspec.FixtureAnyPropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        registerTest("test this", mytags.SlowAsMolasses) { fixture => theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test that") { fixture => theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val b = new SpecB
      val repB = new TestIgnoredTrackingReporter
      b.run(None, Args(repB, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), ConfigMap.empty, None, new Tracker))
      assert(!repB.testIgnoredReceived)
      assert(b.theTestThisCalled)
      assert(!b.theTestThatCalled)

      // SlowAsMolasses is included, and both tests should be included
      class SpecC extends propspec.FixtureAnyPropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        registerTest("test this", mytags.SlowAsMolasses) { fixture => theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test that", mytags.SlowAsMolasses) { fixture => theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val c = new SpecC
      val repC = new TestIgnoredTrackingReporter
      c.run(None, Args(repB, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), ConfigMap.empty, None, new Tracker))
      assert(!repC.testIgnoredReceived)
      assert(c.theTestThisCalled)
      assert(c.theTestThatCalled)

      // SlowAsMolasses is included. both tests should be included but one ignored
      class SpecD extends propspec.FixtureAnyPropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        registerIgnoredTest("test this", mytags.SlowAsMolasses) { fixture => theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test that", mytags.SlowAsMolasses) { fixture => theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val d = new SpecD
      val repD = new TestIgnoredTrackingReporter
      d.run(None, Args(repD, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.Ignore")), ConfigMap.empty, None, new Tracker))
      assert(repD.testIgnoredReceived)
      assert(!d.theTestThisCalled)
      assert(d.theTestThatCalled)

      // SlowAsMolasses included, FastAsLight excluded
      class SpecE extends propspec.FixtureAnyPropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        registerTest("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { fixture => theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test that", mytags.SlowAsMolasses) { fixture => theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test the other") { fixture => theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val e = new SpecE
      val repE = new TestIgnoredTrackingReporter
      e.run(None, Args(repE, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
        ConfigMap.empty, None, new Tracker))
      assert(!repE.testIgnoredReceived)
      assert(!e.theTestThisCalled)
      assert(e.theTestThatCalled)
      assert(!e.theTestTheOtherCalled)

      // An Ignored test that was both included and excluded should not generate a TestIgnored event
      class SpecF extends propspec.FixtureAnyPropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        registerIgnoredTest("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { fixture => theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test that", mytags.SlowAsMolasses) { fixture => theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test the other") { fixture => theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val f = new SpecF
      val repF = new TestIgnoredTrackingReporter
      f.run(None, Args(repF, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
        ConfigMap.empty, None, new Tracker))
      assert(!repF.testIgnoredReceived)
      assert(!f.theTestThisCalled)
      assert(f.theTestThatCalled)
      assert(!f.theTestTheOtherCalled)

      // An Ignored test that was not included should not generate a TestIgnored event
      class SpecG extends propspec.FixtureAnyPropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        registerTest("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { fixture => theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test that", mytags.SlowAsMolasses) { fixture => theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        registerIgnoredTest("test the other") { fixture => theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val g = new SpecG
      val repG = new TestIgnoredTrackingReporter
      g.run(None, Args(repG, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
        ConfigMap.empty, None, new Tracker))
      assert(!repG.testIgnoredReceived)
      assert(!g.theTestThisCalled)
      assert(g.theTestThatCalled)
      assert(!g.theTestTheOtherCalled)

      // No tagsToInclude set, FastAsLight excluded
      class SpecH extends propspec.FixtureAnyPropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        registerTest("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { fixture => theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test that", mytags.SlowAsMolasses) { fixture => theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test the other") { fixture => theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val h = new SpecH
      val repH = new TestIgnoredTrackingReporter
      h.run(None, Args(repH, Stopper.default, Filter(None, Set("org.scalatest.FastAsLight")), ConfigMap.empty, None, new Tracker))
      assert(!repH.testIgnoredReceived)
      assert(!h.theTestThisCalled)
      assert(h.theTestThatCalled)
      assert(h.theTestTheOtherCalled)

      // No tagsToInclude set, SlowAsMolasses excluded
      class SpecI extends propspec.FixtureAnyPropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        registerTest("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { fixture => theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test that", mytags.SlowAsMolasses) { fixture => theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test the other") { fixture => theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val i = new SpecI
      val repI = new TestIgnoredTrackingReporter
      i.run(None, Args(repI, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses")), ConfigMap.empty, None, new Tracker))
      assert(!repI.testIgnoredReceived)
      assert(!i.theTestThisCalled)
      assert(!i.theTestThatCalled)
      assert(i.theTestTheOtherCalled)

      // No tagsToInclude set, SlowAsMolasses excluded, TestIgnored should not be received on excluded ones
      class SpecJ extends propspec.FixtureAnyPropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        registerIgnoredTest("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { fixture => theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerIgnoredTest("test that", mytags.SlowAsMolasses) { fixture => theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test the other") { fixture => theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val j = new SpecJ
      val repJ = new TestIgnoredTrackingReporter
      j.run(None, Args(repJ, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses")), ConfigMap.empty, None, new Tracker))
      assert(!repI.testIgnoredReceived)
      assert(!j.theTestThisCalled)
      assert(!j.theTestThatCalled)
      assert(j.theTestTheOtherCalled)

      // Same as previous, except Ignore specifically mentioned in excludes set
      class SpecK extends propspec.FixtureAnyPropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        registerIgnoredTest("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { fixture => theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerIgnoredTest("test that", mytags.SlowAsMolasses) { fixture => theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        registerIgnoredTest("test the other") { fixture => theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val k = new SpecK
      val repK = new TestIgnoredTrackingReporter
      k.run(None, Args(repK, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses", "org.scalatest.Ignore")), ConfigMap.empty, None, new Tracker))
      assert(repK.testIgnoredReceived)
      assert(!k.theTestThisCalled)
      assert(!k.theTestThatCalled)
      assert(!k.theTestTheOtherCalled)
    }

    it("should return the correct test count from its expectedTestCount method") {

      val a = new propspec.FixtureAnyPropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        property("test this") { fixture => /* ASSERTION_SUCCEED */ }
        property("test that") { fixture => /* ASSERTION_SUCCEED */ }
      }
      assert(a.expectedTestCount(Filter()) === 2)

      val b = new propspec.FixtureAnyPropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        ignore("test this") { fixture => /* ASSERTION_SUCCEED */ }
        property("test that") { fixture => /* ASSERTION_SUCCEED */ }
      }
      assert(b.expectedTestCount(Filter()) === 1)

      val c = new propspec.FixtureAnyPropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        property("test this", mytags.FastAsLight) { fixture => /* ASSERTION_SUCCEED */ }
        property("test that") { fixture => /* ASSERTION_SUCCEED */ }
      }
      assert(c.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 1)
      assert(c.expectedTestCount(Filter(None, Set("org.scalatest.FastAsLight"))) === 1)

      val d = new propspec.FixtureAnyPropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        property("test this", mytags.FastAsLight, mytags.SlowAsMolasses) { fixture => /* ASSERTION_SUCCEED */ }
        property("test that", mytags.SlowAsMolasses) { fixture => /* ASSERTION_SUCCEED */ }
        property("test the other thing") { fixture => /* ASSERTION_SUCCEED */ }
      }
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 1)
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight"))) === 1)
      assert(d.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) === 1)
      assert(d.expectedTestCount(Filter()) === 3)

      val e = new propspec.FixtureAnyPropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        property("test this", mytags.FastAsLight, mytags.SlowAsMolasses) { fixture => /* ASSERTION_SUCCEED */ }
        property("test that", mytags.SlowAsMolasses) { fixture => /* ASSERTION_SUCCEED */ }
        ignore("test the other thing") { fixture => /* ASSERTION_SUCCEED */ }
      }
      assert(e.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 1)
      assert(e.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight"))) === 1)
      assert(e.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) === 0)
      assert(e.expectedTestCount(Filter()) === 2)

      val f = new Suites(a, b, c, d, e)
      assert(f.expectedTestCount(Filter()) === 10)
    }
    it("should return the correct test count from its expectedTestCount method when uses registerTest and registerIgnoredTest to register tests") {

      val a = new propspec.FixtureAnyPropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        registerTest("test this") { fixture => /* ASSERTION_SUCCEED */ }
        registerTest("test that") { fixture => /* ASSERTION_SUCCEED */ }
      }
      assert(a.expectedTestCount(Filter()) == 2)

      val b = new propspec.FixtureAnyPropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        registerIgnoredTest("test this") { fixture => /* ASSERTION_SUCCEED */ }
        registerTest("test that") { fixture => /* ASSERTION_SUCCEED */ }
      }
      assert(b.expectedTestCount(Filter()) == 1)

      val c = new propspec.FixtureAnyPropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        registerTest("test this", mytags.FastAsLight) { fixture => /* ASSERTION_SUCCEED */ }
        registerTest("test that") { fixture => /* ASSERTION_SUCCEED */ }
      }
      assert(c.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) == 1)
      assert(c.expectedTestCount(Filter(None, Set("org.scalatest.FastAsLight"))) == 1)

      val d = new propspec.FixtureAnyPropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        registerTest("test this", mytags.FastAsLight, mytags.SlowAsMolasses) { fixture => /* ASSERTION_SUCCEED */ }
        registerTest("test that", mytags.SlowAsMolasses) { fixture => /* ASSERTION_SUCCEED */ }
        registerTest("test the other thing") { fixture => /* ASSERTION_SUCCEED */ }
      }
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) == 1)
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight"))) == 1)
      assert(d.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) == 1)
      assert(d.expectedTestCount(Filter()) == 3)

      val e = new propspec.FixtureAnyPropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        registerTest("test this", mytags.FastAsLight, mytags.SlowAsMolasses) { fixture => /* ASSERTION_SUCCEED */ }
        registerTest("test that", mytags.SlowAsMolasses) { fixture => /* ASSERTION_SUCCEED */ }
        registerIgnoredTest("test the other thing") { fixture => /* ASSERTION_SUCCEED */ }
      }
      assert(e.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) == 1)
      assert(e.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight"))) == 1)
      assert(e.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) == 0)
      assert(e.expectedTestCount(Filter()) == 2)

      val f = new Suites(a, b, c, d, e)
      assert(f.expectedTestCount(Filter()) == 10)
    }
    it("should generate a TestPending message when the test body is (pending)") {
      val a = new propspec.FixtureAnyPropSpec {
        type FixtureParam = String
        val hello = "Hello, world!"
        def withFixture(test: OneArgTest): Outcome = {
          test(hello)
        }

        property("should do this") (pending)

        property("should do that") { fixture =>
          assert(fixture === hello)
        }
        
        property("should do something else") { fixture =>
          assert(fixture === hello)
          pending
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      val tp = rep.testPendingEventsReceived
      assert(tp.size === 2)
    }

    it("should allow tests without fixtures to be combined with tests with fixtures") {

      class SpecA extends propspec.FixtureAnyPropSpec {

        var theTestWithFixtureWasRun = false
        var theTestWithoutFixtureWasRun = false

        type FixtureParam = String
        val hello = "Hello, world!"
        def withFixture(test: OneArgTest): Outcome = {
          test(hello)
        }

        property("should do this") (pending)

        property("should do that") { fixture =>
          assert(fixture === hello)
          theTestWithFixtureWasRun = true
          /* ASSERTION_SUCCEED */
        }

        property("should do something else") { fixture =>
          assert(fixture === hello)
          pending
        }
        
        property("should do that without a fixture") { () =>
          assert(2 + 2 === 4)
          theTestWithoutFixtureWasRun = true
          /* ASSERTION_SUCCEED */
        }
      }
      val a = new SpecA

      import scala.language.reflectiveCalls

      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      val tp = rep.testPendingEventsReceived
      assert(tp.size === 2)
      assert(a.theTestWithFixtureWasRun)
      assert(a.theTestWithoutFixtureWasRun)
    }
    it("should generate a test failure if a Throwable, or an Error other than direct Error subtypes " +
            "known in JDK 1.5, excluding AssertionError") {
      val a = new propspec.FixtureAnyPropSpec {
        type FixtureParam = String
        val hello = "Hello, world!"
        def withFixture(test: OneArgTest): Outcome = {
          test(hello)
        }
        property("throws AssertionError") { s => throw new AssertionError }
        property("throws plain old Error") { s => throw new Error }
        property("throws Throwable") { s => throw new Throwable }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      val tf = rep.testFailedEventsReceived
      assert(tf.size === 3)
    }
    // SKIP-SCALATESTJS,NATIVE-START
    it("should propagate out Errors that are direct subtypes of Error in JDK 1.5, other than " +
            "AssertionError, causing Suites and Runs to abort.") {
      val a = new propspec.FixtureAnyPropSpec {
        type FixtureParam = String
        val hello = "Hello, world!"
        def withFixture(test: OneArgTest): Outcome = {
          test(hello)
        }
        property("throws AssertionError") { s => throw new OutOfMemoryError }
      }
      intercept[OutOfMemoryError] {
        a.run(None, Args(SilentReporter))
      }
    }
    // SKIP-SCALATESTJS,NATIVE-END
    it("should allow both tests that take fixtures and tests that don't") {
      class SpecA extends propspec.FixtureAnyPropSpec {

        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = {
          test("Hello, world!")
        }

        var takesNoArgsInvoked = false
        property("take no args") { () => takesNoArgsInvoked = true;  /* ASSERTION_SUCCEED */ }

        var takesAFixtureInvoked = false
        property("takes a fixture") { s => takesAFixtureInvoked = true;  /* ASSERTION_SUCCEED */ }
      }
      val a = new SpecA

      import scala.language.reflectiveCalls

      a.run(None, Args(SilentReporter))
      assert(a.testNames.size === 2, a.testNames)
      assert(a.takesNoArgsInvoked)
      assert(a.takesAFixtureInvoked)
    }

    it("should work with test functions whose inferred result type is not Unit") {
      class SpecA extends propspec.FixtureAnyPropSpec {

        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = {
          test("Hello, world!")
        }

        var takesNoArgsInvoked = false
        property("take no args") { () => takesNoArgsInvoked = true; true;  /* ASSERTION_SUCCEED */ }

        var takesAFixtureInvoked = false
        property("takes a fixture") { s => takesAFixtureInvoked = true; true;  /* ASSERTION_SUCCEED */ }
      }
      val a = new SpecA

      import scala.language.reflectiveCalls

      assert(!a.takesNoArgsInvoked)
      assert(!a.takesAFixtureInvoked)
      a.run(None, Args(SilentReporter))
      assert(a.testNames.size === 2, a.testNames)
      assert(a.takesNoArgsInvoked)
      assert(a.takesAFixtureInvoked)
    }

    it("should work with ignored tests whose inferred result type is not Unit") {
      class SpecA extends propspec.FixtureAnyPropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        ignore("test this") { () => theTestThisCalled = true; "hi";  /* ASSERTION_SUCCEED */ }
        ignore("test that") { fixture => theTestThatCalled = true; 42;  /* ASSERTION_SUCCEED */ }
      }
      val a = new SpecA

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
      class MySuite extends propspec.FixtureAnyPropSpec {
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
        property("something") { () =>
          assert(1 + 1 === 2)
        }
      }

      val s = new MySuite
      s.run(None, Args(SilentReporter))
      assert(s.aNoArgTestWasPassed)
      assert(!s.aOneArgTestWasPassed)
    }
    it("should not pass a NoArgTest to withFixture for tests that take a Fixture") {
      class MySuite extends propspec.FixtureAnyPropSpec {
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
        property("something") { fixture =>
          assert(1 + 1 === 2)
        }
      }

      val s = new MySuite
      s.run(None, Args(SilentReporter))
      assert(!s.aNoArgTestWasPassed)
      assert(s.aOneArgTestWasPassed)
    }
    it("should pass a NoArgTest that invokes the no-arg test when the " +
            "NoArgTest's no-arg apply method is invoked") {

      class MySuite extends propspec.FixtureAnyPropSpec {
        type FixtureParam = String
        var theNoArgTestWasInvoked = false
        def withFixture(test: OneArgTest): Outcome = {
          // Shouldn't be called, but just in case don't invoke a OneArgTest
          Succeeded
        }
        property("something") { () =>
          theNoArgTestWasInvoked = true
          /* ASSERTION_SUCCEED */
        }
      }

      val s = new MySuite
      s.run(None, Args(SilentReporter))
      assert(s.theNoArgTestWasInvoked)
    }

    it("should pass the correct test name in the OneArgTest passed to withFixture") {
      class SpecA extends propspec.FixtureAnyPropSpec {
        type FixtureParam = String
        var correctTestNameWasPassed = false
        def withFixture(test: OneArgTest): Outcome = {
          correctTestNameWasPassed = test.name == "something"
          test("hi")
        }
        property("something") { fixture => /* ASSERTION_SUCCEED */ }
      }
      val a = new SpecA

      import scala.language.reflectiveCalls

      a.run(None, Args(SilentReporter))
      assert(a.correctTestNameWasPassed)
    }
    it("should pass the correct config map in the OneArgTest passed to withFixture") {
      class SpecA extends propspec.FixtureAnyPropSpec {
        type FixtureParam = String
        var correctConfigMapWasPassed = false
        def withFixture(test: OneArgTest): Outcome = {
          correctConfigMapWasPassed = (test.configMap == ConfigMap("hi" -> 7))
          test("hi")
        }
        property("something") { fixture => /* ASSERTION_SUCCEED */ }
      }
      val a = new SpecA

      import scala.language.reflectiveCalls

      a.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap("hi" -> 7), None, new Tracker()))
      assert(a.correctConfigMapWasPassed)
    }

    describe("(when a nesting rule has been violated)") {

      it("should, if they call a nested it from within an it clause, result in a TestFailedException when running the test") {

        class MySuite extends propspec.FixtureAnyPropSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = { test("hi") }
          property("should blow up") { fixture =>
            property("should never run") { fixture =>
              assert(1 === 1)
            }
            /* ASSERTION_SUCCEED */
          }
        }

        val spec = new MySuite
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a nested it with tags from within an it clause, result in a TestFailedException when running the test") {

        class MySuite extends propspec.FixtureAnyPropSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = { test("hi") }
          property("should blow up") { fixture =>
            property("should never run", mytags.SlowAsMolasses) { fixture =>
              assert(1 == 1)
            }
            /* ASSERTION_SUCCEED */
          }
        }

        val spec = new MySuite
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a nested registerTest with tags from within a registerTest clause, result in a TestFailedException when running the test") {

        class MySuite extends propspec.FixtureAnyPropSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = { test("hi") }
          registerTest("should blow up") { fixture =>
            registerTest("should never run", mytags.SlowAsMolasses) { fixture =>
              assert(1 == 1)
            }
            /* ASSERTION_SUCCEED */
          }
        }

        val spec = new MySuite
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a nested ignore from within an it clause, result in a TestFailedException when running the test") {

        class MySuite extends propspec.FixtureAnyPropSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = { test("hi") }
          property("should blow up") { fixture =>
            ignore("should never run") { fixture =>
              assert(1 === 1)
            }
            /* ASSERTION_SUCCEED */
          }
        }

        val spec = new MySuite
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a nested ignore with tags from within an it clause, result in a TestFailedException when running the test") {

        class MySuite extends propspec.FixtureAnyPropSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = { test("hi") }
          property("should blow up") { fixture =>
            ignore("should never run", mytags.SlowAsMolasses) { fixture =>
              assert(1 == 1)
            }
            /* ASSERTION_SUCCEED */
          }
        }

        val spec = new MySuite
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a nested registerIgnoredTest with tags from within a registerTest clause, result in a TestFailedException when running the test") {

        class MySuite extends propspec.FixtureAnyPropSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest): Outcome = { test("hi") }
          registerTest("should blow up") { fixture =>
            registerIgnoredTest("should never run", mytags.SlowAsMolasses) { fixture =>
              assert(1 == 1)
            }
            /* ASSERTION_SUCCEED */
          }
        }

        val spec = new MySuite
        ensureTestFailedEventReceived(spec, "should blow up")
      }
    }

    it("should throw IllegalArgumentException if passed a testName that doesn't exist") {
      class MySuite extends propspec.FixtureAnyPropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = {
          test("hi")
        }
        property("one") {s => (); /* ASSERTION_SUCCEED */ }
        property("two") {s => (); /* ASSERTION_SUCCEED */ }
      }
      val suite = new MySuite
      intercept[IllegalArgumentException] {
        suite.run(Some("three"), Args(SilentReporter))
      }
    }

    it("should allow test registration with registerTest and registerIgnoredTest") {
      class TestSpec extends propspec.FixtureAnyPropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = {
          test("hi")
        }
        val a = 1
        registerTest("test 1") { fixture =>
          val e = intercept[TestFailedException] {
            assert(a == 2)
          }
          assert(e.message == Some("1 did not equal 2"))
          assert(e.failedCodeFileName == Some("FixtureAnyPropSpecSpec.scala"))
          assert(e.failedCodeLineNumber == Some(thisLineNumber - 4))
        }
        registerTest("test 2") { fixture =>
          assert(a == 2)
        }
        registerTest("test 3") { fixture =>
          pending
        }
        registerTest("test 4") { fixture =>
          cancel()
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
      class TestSpec extends propspec.FixtureAnyPropSpec with expectations.Expectations {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        property("fail scenario") { () =>
          expect(1 === 2); /* ASSERTION_SUCCEED */
        }
        property("nested fail scenario") { fixture =>
          expect(1 === 2); /* ASSERTION_SUCCEED */
        }
      }
      val rep = new EventRecordingReporter
      val s1 = new TestSpec
      s1.run(None, Args(rep))
      assert(rep.testFailedEventsReceived.size === 2)
      assert(rep.testFailedEventsReceived(0).throwable.get.asInstanceOf[TestFailedException].failedCodeFileName.get === "FixtureAnyPropSpecSpec.scala")
      assert(rep.testFailedEventsReceived(0).throwable.get.asInstanceOf[TestFailedException].failedCodeLineNumber.get === thisLineNumber - 11)
      assert(rep.testFailedEventsReceived(1).throwable.get.asInstanceOf[TestFailedException].failedCodeFileName.get === "FixtureAnyPropSpecSpec.scala")
      assert(rep.testFailedEventsReceived(1).throwable.get.asInstanceOf[TestFailedException].failedCodeLineNumber.get === thisLineNumber - 10)
    }
  }
  
  describe("when failure happens") {
    
    it("should fire TestFailed event with correct stack depth info when test failed") {
      class TestSpec extends propspec.FixtureAnyPropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = {
          test("hi")
        }
        property("fail scenario") { fixture =>
          assert(1 === 2)
        }
      }
      val rep = new EventRecordingReporter
      val s1 = new TestSpec
      s1.run(None, Args(rep))
      assert(rep.testFailedEventsReceived.size === 1)
      assert(rep.testFailedEventsReceived(0).throwable.get.asInstanceOf[TestFailedException].failedCodeFileName.get === "FixtureAnyPropSpecSpec.scala")
      assert(rep.testFailedEventsReceived(0).throwable.get.asInstanceOf[TestFailedException].failedCodeLineNumber.get === thisLineNumber - 8)
    }
    
    it("should generate TestRegistrationClosedException with correct stack depth info when has a property nested inside a property") {
      class TestSpec extends propspec.FixtureAnyPropSpec {
        var registrationClosedThrown = false
        type FixtureParam = String
        property("a scenario") { fixture =>
          property("nested scenario") { fixture =>
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
      assert("FixtureAnyPropSpecSpec.scala" === trce.failedCodeFileName.get)
      assert(trce.failedCodeLineNumber.get === thisLineNumber - 23)
      assert(trce.message == Some("A property clause may not appear inside another property clause."))
    }

    it("should generate TestRegistrationClosedException with correct stack depth info when has an ignore nested inside a property") {
      class TestSpec extends propspec.FixtureAnyPropSpec {
        var registrationClosedThrown = false
        type FixtureParam = String
        property("a scenario") { fixture =>
          ignore("nested scenario") { fixture =>
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
      assert("FixtureAnyPropSpecSpec.scala" === trce.failedCodeFileName.get)
      assert(trce.failedCodeLineNumber.get === thisLineNumber - 23)
      assert(trce.message == Some("An ignore clause may not appear inside a property clause."))
    }

    it("should generate TestRegistrationClosedException with correct stack depth info when has a registerTest nested inside a registerTest") {
      class TestSpec extends propspec.FixtureAnyPropSpec {
        var registrationClosedThrown = false
        type FixtureParam = String
        registerTest("a scenario") { fixture =>
          registerTest("nested scenario") { fixture =>
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
      assert("FixtureAnyPropSpecSpec.scala" === trce.failedCodeFileName.get)
      assert(trce.failedCodeLineNumber.get === thisLineNumber - 23)
      assert(trce.message == Some("Test cannot be nested inside another test."))
    }

    it("should generate TestRegistrationClosedException with correct stack depth info when has a registerIgnoredTest nested inside a registerTest") {
      class TestSpec extends propspec.FixtureAnyPropSpec {
        var registrationClosedThrown = false
        type FixtureParam = String
        registerTest("a scenario") { fixture =>
          registerIgnoredTest("nested scenario") { fixture =>
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
      assert("FixtureAnyPropSpecSpec.scala" === trce.failedCodeFileName.get)
      assert(trce.failedCodeLineNumber.get === thisLineNumber - 23)
      assert(trce.message == Some("Test cannot be nested inside another test."))
    }

    it("should generate a DuplicateTestNameException when duplicate test name is detected") {
      class TestSpec extends propspec.FixtureAnyPropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        property("test 1") { fixture => /* ASSERTION_SUCCEED */}
        property("test 1") { fixture => /* ASSERTION_SUCCEED */}
      }
      val e = intercept[DuplicateTestNameException] {
        new TestSpec
      }
      assert("FixtureAnyPropSpecSpec.scala" == e.failedCodeFileName.get)
      assert(e.failedCodeLineNumber.get == thisLineNumber - 6)
      assert(!e.cause.isDefined)
    }

    it("should generate a DuplicateTestNameException when duplicate test name is detected when use ignore") {
      class TestSpec extends propspec.FixtureAnyPropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        property("test 1") { fixture => /* ASSERTION_SUCCEED */}
        ignore("test 1") { fixture => /* ASSERTION_SUCCEED */}
      }
      val e = intercept[DuplicateTestNameException] {
        new TestSpec
      }
      assert("FixtureAnyPropSpecSpec.scala" == e.failedCodeFileName.get)
      assert(e.failedCodeLineNumber.get == thisLineNumber - 6)
      assert(!e.cause.isDefined)
    }
  }
}
