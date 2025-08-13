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
package org.scalatest.featurespec

import org.scalatest._
import SharedHelpers._
import org.scalactic.Prettifier
import java.awt.AWTError
import java.lang.annotation.AnnotationFormatError
import java.nio.charset.CoderMalfunctionError
import javax.xml.parsers.FactoryConfigurationError
import javax.xml.transform.TransformerFactoryConfigurationError
import org.scalactic.exceptions.NullArgumentException
import org.scalatest.events.InfoProvided
import org.scalatest.events.MarkupProvided
import org.scalatest.events.TestStarting
import org.scalatest.exceptions.DuplicateTestNameException
import org.scalatest.exceptions.NotAllowedException
import org.scalatest.exceptions.TestCanceledException
import org.scalatest.exceptions.TestFailedException
import org.scalatest.exceptions.TestRegistrationClosedException
import org.scalatest.featurespec.AnyFeatureSpec
import org.scalatest.funspec.AnyFunSpec

class DeprecatedFeatureSpecSpec extends AnyFunSpec {

  private val prettifier = Prettifier.default

  describe("A FeatureSpec") {

    it("should return the scenario names in registration order from testNames") {

      val a = new AnyFeatureSpec {
        Scenario("test this") {/* ASSERTION_SUCCEED */}
        Scenario("test that") {/* ASSERTION_SUCCEED */}
      }

      assertResult(List("Scenario: test this", "Scenario: test that")) {
        a.testNames.iterator.toList
      }

      val b = new AnyFeatureSpec {}

      assertResult(List[String]()) {
        b.testNames.iterator.toList
      }

      val c = new AnyFeatureSpec {
        Scenario("test that") {/* ASSERTION_SUCCEED */}
        Scenario("test this") {/* ASSERTION_SUCCEED */}
      }

      assertResult(List("Scenario: test that", "Scenario: test this")) {
        c.testNames.iterator.toList
      }
    }

    it("should throw NotAllowedException if a duplicate scenario name registration is attempted") {

      intercept[DuplicateTestNameException] {
        new AnyFeatureSpec {
          Scenario("test this") {/* ASSERTION_SUCCEED */}
          Scenario("test this") {/* ASSERTION_SUCCEED */}
        }
      }
      intercept[DuplicateTestNameException] {
        new AnyFeatureSpec {
          Scenario("test this") {/* ASSERTION_SUCCEED */}
          ignore("test this") {/* ASSERTION_SUCCEED */}
        }
      }
      intercept[DuplicateTestNameException] {
        new AnyFeatureSpec {
          ignore("test this") {/* ASSERTION_SUCCEED */}
          ignore("test this") {/* ASSERTION_SUCCEED */}
        }
      }
      intercept[DuplicateTestNameException] {
        new AnyFeatureSpec {
          ignore("test this") {/* ASSERTION_SUCCEED */}
          Scenario("test this") {/* ASSERTION_SUCCEED */}
        }
      }
    }

    it("should run tests registered via the scenariosFor syntax") {
      trait SharedFeatureSpecTests { this: AnyFeatureSpec =>
        def nonEmptyStack(s: String)(i: Int): Unit = {
          Scenario("I am shared") {/* ASSERTION_SUCCEED */}
        }
      }
      class MySuite extends AnyFeatureSpec with SharedFeatureSpecTests {
        ScenariosFor(nonEmptyStack("hi")(1))
      }
      val suite = new MySuite
      val reporter = new EventRecordingReporter
      suite.run(None, Args(reporter))

      val indexedList = reporter.eventsReceived

      val testStartingOption = indexedList.find(_.isInstanceOf[TestStarting])
      assert(testStartingOption.isDefined)
      assert(testStartingOption.get.asInstanceOf[TestStarting].testName === "Scenario: I am shared")
    }

    it("should throw NullArgumentException if a null test tag is provided") {
      // scenario
      intercept[NullArgumentException] {
        new AnyFeatureSpec {
          Scenario("hi", null) {/* ASSERTION_SUCCEED */}
        }
      }
      val caught = intercept[NullArgumentException] {
        new AnyFeatureSpec {
          Scenario("hi", mytags.SlowAsMolasses, null) {/* ASSERTION_SUCCEED */}
        }
      }
      assert(caught.getMessage === "a test tag was null")
      intercept[NullArgumentException] {
        new AnyFeatureSpec {
          Scenario("hi", mytags.SlowAsMolasses, null, mytags.WeakAsAKitten) {/* ASSERTION_SUCCEED */}
        }
      }

      // ignore
      intercept[NullArgumentException] {
        new AnyFeatureSpec {
          ignore("hi", null) {/* ASSERTION_SUCCEED */}
        }
      }
      val caught2 = intercept[NullArgumentException] {
        new AnyFeatureSpec {
          ignore("hi", mytags.SlowAsMolasses, null) {/* ASSERTION_SUCCEED */}
        }
      }
      assert(caught2.getMessage === "a test tag was null")
      intercept[NullArgumentException] {
        new AnyFeatureSpec {
          ignore("hi", mytags.SlowAsMolasses, null, mytags.WeakAsAKitten) {/* ASSERTION_SUCCEED */}
        }
      }

      // registerTest
      intercept[NullArgumentException] {
        new AnyFeatureSpec {
          registerTest("hi", null) {/* ASSERTION_SUCCEED */}
        }
      }
      val caught3 = intercept[NullArgumentException] {
        new AnyFeatureSpec {
          registerTest("hi", mytags.SlowAsMolasses, null) {/* ASSERTION_SUCCEED */}
        }
      }
      assert(caught3.getMessage === "a test tag was null")
      intercept[NullArgumentException] {
        new AnyFeatureSpec {
          registerTest("hi", mytags.SlowAsMolasses, null, mytags.WeakAsAKitten) {/* ASSERTION_SUCCEED */}
        }
      }

      // registerIgnoredTest
      intercept[NullArgumentException] {
        new AnyFeatureSpec {
          registerIgnoredTest("hi", null) {/* ASSERTION_SUCCEED */}
        }
      }
      val caught4 = intercept[NullArgumentException] {
        new AnyFeatureSpec {
          registerIgnoredTest("hi", mytags.SlowAsMolasses, null) {/* ASSERTION_SUCCEED */}
        }
      }
      assert(caught4.getMessage === "a test tag was null")
      intercept[NullArgumentException] {
        new AnyFeatureSpec {
          registerIgnoredTest("hi", mytags.SlowAsMolasses, null, mytags.WeakAsAKitten) {/* ASSERTION_SUCCEED */}
        }
      }
    }

    class TestWasCalledSuite extends AnyFeatureSpec {
      var theTestThisCalled = false
      var theTestThatCalled = false
      Scenario("this") { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
      Scenario("that") { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
    }

    it("should execute all tests when run is called with testName None") {

      val b = new TestWasCalledSuite
      b.run(None, Args(SilentReporter))
      assert(b.theTestThisCalled)
      assert(b.theTestThatCalled)
    }

    it("should execute one test when run is called with a defined testName") {

      val a = new TestWasCalledSuite
      a.run(Some("Scenario: this"), Args(SilentReporter))
      assert(a.theTestThisCalled)
      assert(!a.theTestThatCalled)
    }

    it("should report as ignored, and not run, tests marked ignored") {

      class SpecA extends AnyFeatureSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        Scenario("test this") { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        Scenario("test that") { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val a = new SpecA

      import scala.language.reflectiveCalls

      val repA = new TestIgnoredTrackingReporter
      a.run(None, Args(repA))
      assert(!repA.testIgnoredReceived)
      assert(a.theTestThisCalled)
      assert(a.theTestThatCalled)

      class SpecB extends AnyFeatureSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        ignore("test this") { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        Scenario("test that") { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val b = new SpecB

      val repB = new TestIgnoredTrackingReporter
      b.run(None, Args(repB))
      assert(repB.testIgnoredReceived)
      assert(repB.lastEvent.isDefined)
      assert(repB.lastEvent.get.testName endsWith "test this")
      assert(!b.theTestThisCalled)
      assert(b.theTestThatCalled)

      class SpecC extends AnyFeatureSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        Scenario("test this") { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        ignore("test that") { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
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
      class SpecD extends AnyFeatureSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        ignore("test this") { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        ignore("test that") { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
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
      class SpecE extends AnyFeatureSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        ignore("test this") { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        Scenario("test that") { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val e = new SpecE

      import scala.language.reflectiveCalls

      val repE = new TestIgnoredTrackingReporter
      e.run(Some("Scenario: test this"), Args(repE))
      assert(repE.testIgnoredReceived)
      assert(!e.theTestThisCalled)
      assert(!e.theTestThatCalled)
    }

    it("should run only those tests selected by the tags to include and exclude sets") {

      // Nothing is excluded
      class SpecA extends AnyFeatureSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        Scenario("test this", mytags.SlowAsMolasses) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        Scenario("test that") { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val a = new SpecA

      import scala.language.reflectiveCalls

      val repA = new TestIgnoredTrackingReporter
      a.run(None, Args(repA))
      assert(!repA.testIgnoredReceived)
      assert(a.theTestThisCalled)
      assert(a.theTestThatCalled)

      // SlowAsMolasses is included, one test should be excluded
      class SpecB extends AnyFeatureSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        Scenario("test this", mytags.SlowAsMolasses) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        Scenario("test that") { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val b = new SpecB
      val repB = new TestIgnoredTrackingReporter
      b.run(None, Args(repB, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), ConfigMap.empty, None, new Tracker))
      assert(!repB.testIgnoredReceived)
      assert(b.theTestThisCalled)
      assert(!b.theTestThatCalled)

      // SlowAsMolasses is included, and both tests should be included
      class SpecC extends AnyFeatureSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        Scenario("test this", mytags.SlowAsMolasses) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        Scenario("test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val c = new SpecC
      val repC = new TestIgnoredTrackingReporter
      c.run(None, Args(repB, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), ConfigMap.empty, None, new Tracker))
      assert(!repC.testIgnoredReceived)
      assert(c.theTestThisCalled)
      assert(c.theTestThatCalled)

      // SlowAsMolasses is included. both tests should be included but one ignored
      class SpecD extends AnyFeatureSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        ignore("test this", mytags.SlowAsMolasses) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        Scenario("test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val d = new SpecD
      val repD = new TestIgnoredTrackingReporter
      d.run(None, Args(repD, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.Ignore")), ConfigMap.empty, None, new Tracker))
      assert(repD.testIgnoredReceived)
      assert(!d.theTestThisCalled)
      assert(d.theTestThatCalled)

      // SlowAsMolasses included, FastAsLight excluded
      class SpecE extends AnyFeatureSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        Scenario("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        Scenario("test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        Scenario("test the other") { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
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
      class SpecF extends AnyFeatureSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        ignore("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        Scenario("test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        Scenario("test the other") { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
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
      class SpecG extends AnyFeatureSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        Scenario("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        Scenario("test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        ignore("test the other") { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
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
      class SpecH extends AnyFeatureSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        Scenario("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        Scenario("test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        Scenario("test the other") { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val h = new SpecH
      val repH = new TestIgnoredTrackingReporter
      h.run(None, Args(repH, Stopper.default, Filter(None, Set("org.scalatest.FastAsLight")), ConfigMap.empty, None, new Tracker))
      assert(!repH.testIgnoredReceived)
      assert(!h.theTestThisCalled)
      assert(h.theTestThatCalled)
      assert(h.theTestTheOtherCalled)

      // No tagsToInclude set, SlowAsMolasses excluded
      class SpecI extends AnyFeatureSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        Scenario("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        Scenario("test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        Scenario("test the other") { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val i = new SpecI
      val repI = new TestIgnoredTrackingReporter
      i.run(None, Args(repI, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses")), ConfigMap.empty, None, new Tracker))
      assert(!repI.testIgnoredReceived)
      assert(!i.theTestThisCalled)
      assert(!i.theTestThatCalled)
      assert(i.theTestTheOtherCalled)

      // No tagsToInclude set, SlowAsMolasses excluded, TestIgnored should not be received on excluded ones
      class SpecJ extends AnyFeatureSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        ignore("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        ignore("test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        Scenario("test the other") { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val j = new SpecJ
      val repJ = new TestIgnoredTrackingReporter
      j.run(None, Args(repJ, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses")), ConfigMap.empty, None, new Tracker))
      assert(!repI.testIgnoredReceived)
      assert(!j.theTestThisCalled)
      assert(!j.theTestThatCalled)
      assert(j.theTestTheOtherCalled)

      // Same as previous, except Ignore specifically mentioned in excludes set
      class SpecK extends AnyFeatureSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        ignore("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        ignore("test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        ignore("test the other") { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
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
      class SpecA extends AnyFeatureSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        registerTest("test this", mytags.SlowAsMolasses) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test that") { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val a = new SpecA

      import scala.language.reflectiveCalls

      val repA = new TestIgnoredTrackingReporter
      a.run(None, Args(repA))
      assert(!repA.testIgnoredReceived)
      assert(a.theTestThisCalled)
      assert(a.theTestThatCalled)

      // SlowAsMolasses is included, one test should be excluded
      class SpecB extends AnyFeatureSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        registerTest("test this", mytags.SlowAsMolasses) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test that") { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val b = new SpecB
      val repB = new TestIgnoredTrackingReporter
      b.run(None, Args(repB, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), ConfigMap.empty, None, new Tracker))
      assert(!repB.testIgnoredReceived)
      assert(b.theTestThisCalled)
      assert(!b.theTestThatCalled)

      // SlowAsMolasses is included, and both tests should be included
      class SpecC extends AnyFeatureSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        registerTest("test this", mytags.SlowAsMolasses) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val c = new SpecC
      val repC = new TestIgnoredTrackingReporter
      c.run(None, Args(repB, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), ConfigMap.empty, None, new Tracker))
      assert(!repC.testIgnoredReceived)
      assert(c.theTestThisCalled)
      assert(c.theTestThatCalled)

      // SlowAsMolasses is included. both tests should be included but one ignored
      class SpecD extends AnyFeatureSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        registerIgnoredTest("test this", mytags.SlowAsMolasses) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val d = new SpecD
      val repD = new TestIgnoredTrackingReporter
      d.run(None, Args(repD, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.Ignore")), ConfigMap.empty, None, new Tracker))
      assert(repD.testIgnoredReceived)
      assert(!d.theTestThisCalled)
      assert(d.theTestThatCalled)

      // SlowAsMolasses included, FastAsLight excluded
      class SpecE extends AnyFeatureSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        registerTest("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test the other") { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
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
      class SpecF extends AnyFeatureSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        registerIgnoredTest("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test the other") { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
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
      class SpecG extends AnyFeatureSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        registerTest("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        registerIgnoredTest("test the other") { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
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
      class SpecH extends AnyFeatureSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        registerTest("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test the other") { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val h = new SpecH
      val repH = new TestIgnoredTrackingReporter
      h.run(None, Args(repH, Stopper.default, Filter(None, Set("org.scalatest.FastAsLight")), ConfigMap.empty, None, new Tracker))
      assert(!repH.testIgnoredReceived)
      assert(!h.theTestThisCalled)
      assert(h.theTestThatCalled)
      assert(h.theTestTheOtherCalled)

      // No tagsToInclude set, SlowAsMolasses excluded
      class SpecI extends AnyFeatureSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        registerTest("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test the other") { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val i = new SpecI
      val repI = new TestIgnoredTrackingReporter
      i.run(None, Args(repI, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses")), ConfigMap.empty, None, new Tracker))
      assert(!repI.testIgnoredReceived)
      assert(!i.theTestThisCalled)
      assert(!i.theTestThatCalled)
      assert(i.theTestTheOtherCalled)

      // No tagsToInclude set, SlowAsMolasses excluded, TestIgnored should not be received on excluded ones
      class SpecJ extends AnyFeatureSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        registerIgnoredTest("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerIgnoredTest("test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        registerTest("test the other") { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
      }
      val j = new SpecJ
      val repJ = new TestIgnoredTrackingReporter
      j.run(None, Args(repJ, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses")), ConfigMap.empty, None, new Tracker))
      assert(!repI.testIgnoredReceived)
      assert(!j.theTestThisCalled)
      assert(!j.theTestThatCalled)
      assert(j.theTestTheOtherCalled)

      // Same as previous, except Ignore specifically mentioned in excludes set
      class SpecK extends AnyFeatureSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        registerIgnoredTest("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true; /* ASSERTION_SUCCEED */ }
        registerIgnoredTest("test that", mytags.SlowAsMolasses) { theTestThatCalled = true; /* ASSERTION_SUCCEED */ }
        registerIgnoredTest("test the other") { theTestTheOtherCalled = true; /* ASSERTION_SUCCEED */ }
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

      val a = new AnyFeatureSpec {
        Scenario("test this") {/* ASSERTION_SUCCEED */}
        Scenario("test that") {/* ASSERTION_SUCCEED */}
      }
      assert(a.expectedTestCount(Filter()) === 2)

      val b = new AnyFeatureSpec {
        ignore("test this") {/* ASSERTION_SUCCEED */}
        Scenario("test that") {/* ASSERTION_SUCCEED */}
      }
      assert(b.expectedTestCount(Filter()) === 1)

      val c = new AnyFeatureSpec {
        Scenario("test this", mytags.FastAsLight) {/* ASSERTION_SUCCEED */}
        Scenario("test that") {/* ASSERTION_SUCCEED */}
      }
      assert(c.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 1)
      assert(c.expectedTestCount(Filter(None, Set("org.scalatest.FastAsLight"))) === 1)

      val d = new AnyFeatureSpec {
        Scenario("test this", mytags.FastAsLight, mytags.SlowAsMolasses) {/* ASSERTION_SUCCEED */}
        Scenario("test that", mytags.SlowAsMolasses) {/* ASSERTION_SUCCEED */}
        Scenario("test the other thing") {/* ASSERTION_SUCCEED */}
      }
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 1)
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight"))) === 1)
      assert(d.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) === 1)
      assert(d.expectedTestCount(Filter()) === 3)

      val e = new AnyFeatureSpec {
        Scenario("test this", mytags.FastAsLight, mytags.SlowAsMolasses) {/* ASSERTION_SUCCEED */}
        Scenario("test that", mytags.SlowAsMolasses) {/* ASSERTION_SUCCEED */}
        ignore("test the other thing") {/* ASSERTION_SUCCEED */}
      }
      assert(e.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 1)
      assert(e.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight"))) === 1)
      assert(e.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) === 0)
      assert(e.expectedTestCount(Filter()) === 2)

      val f = new Suites(a, b, c, d, e)
      assert(f.expectedTestCount(Filter()) === 10)
    }

    it("should return the correct test count from its expectedTestCount method when uses registerTest and registerIgnoredTest to register tests") {

      val a = new AnyFeatureSpec {
        registerTest("test this") {/* ASSERTION_SUCCEED */}
        registerTest("test that") {/* ASSERTION_SUCCEED */}
      }
      assert(a.expectedTestCount(Filter()) === 2)

      val b = new AnyFeatureSpec {
        registerIgnoredTest("test this") {/* ASSERTION_SUCCEED */}
        registerTest("test that") {/* ASSERTION_SUCCEED */}
      }
      assert(b.expectedTestCount(Filter()) === 1)

      val c = new AnyFeatureSpec {
        registerTest("test this", mytags.FastAsLight) {/* ASSERTION_SUCCEED */}
        registerTest("test that") {/* ASSERTION_SUCCEED */}
      }
      assert(c.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 1)
      assert(c.expectedTestCount(Filter(None, Set("org.scalatest.FastAsLight"))) === 1)

      val d = new AnyFeatureSpec {
        registerTest("test this", mytags.FastAsLight, mytags.SlowAsMolasses) {/* ASSERTION_SUCCEED */}
        registerTest("test that", mytags.SlowAsMolasses) {/* ASSERTION_SUCCEED */}
        registerTest("test the other thing") {/* ASSERTION_SUCCEED */}
      }
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 1)
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight"))) === 1)
      assert(d.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) === 1)
      assert(d.expectedTestCount(Filter()) === 3)

      val e = new AnyFeatureSpec {
        registerTest("test this", mytags.FastAsLight, mytags.SlowAsMolasses) {/* ASSERTION_SUCCEED */}
        registerTest("test that", mytags.SlowAsMolasses) {/* ASSERTION_SUCCEED */}
        registerIgnoredTest("test the other thing") {/* ASSERTION_SUCCEED */}
      }
      assert(e.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 1)
      assert(e.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight"))) === 1)
      assert(e.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) === 0)
      assert(e.expectedTestCount(Filter()) === 2)

      val f = new Suites(a, b, c, d, e)
      assert(f.expectedTestCount(Filter()) === 10)
    }
    
    it("should send an InfoProvided event for an info") {
      class MySuite extends AnyFeatureSpec  {
        info(
          "hi there"
        )
      }
      val suite = new MySuite
      val reporter = new EventRecordingReporter
      suite.run(None, Args(reporter))

      val infoList = reporter.infoProvidedEventsReceived

      assert(infoList.size === 1)
      assert(infoList(0).message === "hi there")
    }
    it("should generate a TestPending message when the test body is (pending)") {
      val a = new AnyFeatureSpec {

        Scenario("should do this") (pending)

        Scenario("should do that") {
          assert(2 + 2 === 4)
        }
        
        Scenario("should do something else") {
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
      val a = new AnyFeatureSpec {
        Scenario("throws AssertionError") { throw new AssertionError }
        Scenario("throws plain old Error") { throw new Error }
        Scenario("throws Throwable") { throw new Throwable }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      val tf = rep.testFailedEventsReceived
      assert(tf.size === 3)
    }
    // SKIP-SCALATESTJS,NATIVE-START
    it("should propagate out Errors that are direct subtypes of Error in JDK 1.5, other than " +
            "AssertionError, causing Suites and Runs to abort.") {
      val a = new AnyFeatureSpec {
        Scenario("throws AssertionError") { throw new OutOfMemoryError }
      }
      intercept[OutOfMemoryError] {
        a.run(None, Args(SilentReporter))
      }
    }
    // SKIP-SCALATESTJS,NATIVE-END
/*
    it("should send InfoProvided events with aboutAPendingTest set to true and aboutACanceledTest set to false for info " +
            "calls made from a test that is pending") {
      val a = new FeatureSpec with GivenWhenThen {
        scenario("should do something else") {
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
        assert(ip.aboutACanceledTest.isDefined && !ip.aboutACanceledTest.get)
      }
    }
    it("should send InfoProvided events with aboutAPendingTest and aboutACanceledTest set to false for info " +
            "calls made from a test that is not pending or canceled") {
      val a = new FeatureSpec with GivenWhenThen {
        scenario("should do something else") {
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
        assert(ip.aboutACanceledTest.isDefined && !ip.aboutACanceledTest.get)
      }
    }
    it("should send InfoProvided events with aboutAPendingTest set to false and aboutACanceledTest set to true for info " +
            "calls made from a test that is canceled") {
      val a = new FeatureSpec with GivenWhenThen {
        scenario("should do something else") {
          given("two integers")
          when("one is subracted from the other")
          then("the result is the difference between the two numbers")
          cancel()
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
    }
    it("should send MarkupProvided events with aboutAPendingTest set to true and aboutACanceledTest set to false for markup " +
            "calls made from a test that is pending") {
      val a = new FeatureSpec with GivenWhenThen {
        scenario("should do something else") {
          markup("two strings")
          markup("walked into")
          markup("a bar")
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
        val mp = event.asInstanceOf[MarkupProvided]
        assert(mp.aboutAPendingTest.isDefined && mp.aboutAPendingTest.get)
        assert(mp.aboutACanceledTest.isDefined && !mp.aboutACanceledTest.get)
      }
    }
    it("should send MarkupProvided events with aboutAPendingTest and aboutACanceledTest set to false for markup " +
            "calls made from a test that is not pending or canceled") {
      val a = new FeatureSpec with GivenWhenThen {
        scenario("should do something else") {
          markup("two strings")
          markup("walked into")
          markup("a bar")
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
        val mp = event.asInstanceOf[MarkupProvided]
        assert(mp.aboutAPendingTest.isDefined && !mp.aboutAPendingTest.get)
        assert(mp.aboutACanceledTest.isDefined && !mp.aboutACanceledTest.get)
      }
    }
    it("should send MarkupProvided events with aboutAPendingTest set to false and aboutACanceledTest set to true for markup " +
            "calls made from a test that is canceled") {
      val a = new FeatureSpec with GivenWhenThen {
        scenario("should do something else") {
          markup("two strings")
          markup("walked into")
          markup("a bar")
          cancel()
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      val testCanceled = rep.testCanceledEventsReceived
      assert(testCanceled.size === 1)
      val recordedEvents = testCanceled(0).recordedEvents
      assert(recordedEvents.size === 3)
      for (event <- recordedEvents) {
        val mp = event.asInstanceOf[MarkupProvided]
        assert(mp.aboutAPendingTest.isDefined && !mp.aboutAPendingTest.get)
        assert(mp.aboutACanceledTest.isDefined && mp.aboutACanceledTest.get)
      }
    }
*/
    it("should invoke withFixture from runTest") {
      class SpecA extends AnyFeatureSpec {
        var withFixtureWasInvoked = false
        var testWasInvoked = false
        override def withFixture(test: NoArgTest): Outcome = {
          withFixtureWasInvoked = true
          super.withFixture(test)
        }
        Scenario("something") {
          testWasInvoked = true
          /* ASSERTION_SUCCEED */
        }
      }
      val a = new SpecA

      import scala.language.reflectiveCalls

      a.run(None, Args(SilentReporter))
      assert(a.withFixtureWasInvoked)
      assert(a.testWasInvoked)
    }
    it("should pass the correct test name in the NoArgTest passed to withFixture") {
      class SpecA extends AnyFeatureSpec {
        var correctTestNameWasPassed = false
        override def withFixture(test: NoArgTest): Outcome = {
          correctTestNameWasPassed = test.name == "Scenario: should do something"
          super.withFixture(test)
        }
        Scenario("should do something") {/* ASSERTION_SUCCEED */}
      }
      val a = new SpecA

      import scala.language.reflectiveCalls

      a.run(None, Args(SilentReporter))
      assert(a.correctTestNameWasPassed)
    }
    it("should pass the correct config map in the NoArgTest passed to withFixture") {
      class SpecA extends AnyFeatureSpec {
        var correctConfigMapWasPassed = false
        override def withFixture(test: NoArgTest): Outcome = {
          correctConfigMapWasPassed = (test.configMap == ConfigMap("hi" -> 7))
          super.withFixture(test)
        }
        Scenario("should do something") {/* ASSERTION_SUCCEED */}
      }
      val a = new SpecA

      import scala.language.reflectiveCalls

      a.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap("hi" -> 7), None, new Tracker()))
      assert(a.correctConfigMapWasPassed)
    }
    describe("(when a nesting rule has been violated)") {

      it("should, if they call a feature from within an scenario clause, result in a TestFailedException when running the test") {

        class MySpec extends AnyFeatureSpec {
          Scenario("should blow up") {
            Feature("in the wrong place, at the wrong time") {
            }
            /* ASSERTION_SUCCEED */
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "Scenario: should blow up")
      }
      it("should, if they call a feature with a nested it from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends AnyFeatureSpec {
          Scenario("should blow up") {
            Feature("in the wrong place, at the wrong time") {
              Scenario("should never run") {
                assert(1 === 1)
              }
            }
            /* ASSERTION_SUCCEED */
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "Scenario: should blow up")
      }
      it("should, if they call a nested it from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends AnyFeatureSpec {
          Scenario("should blow up") {
            Scenario("should never run") {
              assert(1 === 1)
            }
            /* ASSERTION_SUCCEED */
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "Scenario: should blow up")
      }
      it("should, if they call a nested it with tags from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends AnyFeatureSpec {
          Scenario("should blow up") {
            Scenario("should never run", mytags.SlowAsMolasses) {
              assert(1 === 1)
            }
            /* ASSERTION_SUCCEED */
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "Scenario: should blow up")
      }
      it("should, if they call a nested registerTest with tags from within a registerTest clause, result in a TestFailedException when running the test") {

        class MySpec extends AnyFeatureSpec {
          registerTest("should blow up") {
            registerTest("should never run", mytags.SlowAsMolasses) {
              assert(1 === 1)
            }
            /* ASSERTION_SUCCEED */
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "Scenario: should blow up")
      }
      it("should, if they call a feature with a nested ignore from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends AnyFeatureSpec {
          Scenario("should blow up") {
            Feature("in the wrong place, at the wrong time") {
              ignore("should never run") {
                assert(1 === 1)
              }
            }
            /* ASSERTION_SUCCEED */
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "Scenario: should blow up")
      }
      it("should, if they call a nested ignore from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends AnyFeatureSpec {
          Scenario("should blow up") {
            ignore("should never run") {
              assert(1 === 1)
            }
            /* ASSERTION_SUCCEED */
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "Scenario: should blow up")
      }
      it("should, if they call a nested ignore with tags from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends AnyFeatureSpec {
          Scenario("should blow up") {
            ignore("should never run", mytags.SlowAsMolasses) {
              assert(1 === 1)
            }
            /* ASSERTION_SUCCEED */
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "Scenario: should blow up")
      }
      it("should, if they call a nested registerIgnoredTest with tags from within a registerTest clause, result in a TestFailedException when running the test") {

        class MySpec extends AnyFeatureSpec {
          registerTest("should blow up") {
            registerIgnoredTest("should never run", mytags.SlowAsMolasses) {
              assert(1 === 1)
            }
            /* ASSERTION_SUCCEED */
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "Scenario: should blow up")
      }
      it("should, if they call a nested feature from within a feature clause, result in a SuiteAborted event when constructing the FeatureSpec") {

        class MySpec extends AnyFeatureSpec {
          Feature("should blow up") {
            Feature("should never run") {
            }
          }
        }

        val caught =
          intercept[NotAllowedException] {
            new MySpec
          }
        assert(caught.getMessage === "Feature clauses cannot be nested.")
      }
    }
    
    class ExamplePrefixSpec extends AnyFeatureSpec {
      Feature("A Feature") {
        Scenario("A Scenario") {
          /* ASSERTION_SUCCEED */
        }
      }
    }
    
    it("should prefix feature text with 'Feature: '") {
      val rep = new EventRecordingReporter
      (new ExamplePrefixSpec).run(None, Args(rep))
      val scopeOpened = rep.scopeOpenedEventsReceived
      assert(scopeOpened.size === 1)
      assert(scopeOpened(0).message === "Feature: A Feature")
      val scopeClosed = rep.scopeClosedEventsReceived
      assert(scopeClosed.size === 1)
      assert(scopeClosed(0).message === "Feature: A Feature")
    }
    
    it("should prefix scenario text with 'Scenario: '") {
      val rep = new EventRecordingReporter
      (new ExamplePrefixSpec).run(None, Args(rep))
      val testStarting = rep.testStartingEventsReceived
      assert(testStarting.size === 1)
      assert(testStarting(0).testText === "Scenario: A Scenario")
      val testSucceeded = rep.testSucceededEventsReceived
      assert(testSucceeded.size === 1)
      assert(testSucceeded(0).testText === "Scenario: A Scenario")
    }

    it("should allow test registration with registerTest and registerIgnoredTest") {
      class TestSpec extends AnyFeatureSpec {
        val a = 1
        registerTest("test 1") {
          val e = intercept[TestFailedException] {
            assert(a == 2)
          }
          assert(e.message == Some("1 did not equal 2"))
          assert(e.failedCodeFileName == Some("DeprecatedFeatureSpecSpec.scala"))
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
      assert(rep.testSucceededEventsReceived(0).testName == "Scenario: test 1")
      assert(rep.testFailedEventsReceived.length == 1)
      assert(rep.testFailedEventsReceived(0).testName == "Scenario: test 2")
      assert(rep.testPendingEventsReceived.length == 1)
      assert(rep.testPendingEventsReceived(0).testName == "Scenario: test 3")
      assert(rep.testCanceledEventsReceived.length == 1)
      assert(rep.testCanceledEventsReceived(0).testName == "Scenario: test 4")
      assert(rep.testIgnoredEventsReceived.length == 1)
      assert(rep.testIgnoredEventsReceived(0).testName == "Scenario: test 5")
    }
    
    describe("when failure happens") {
      
      it("should fire TestFailed event with correct stack depth info when test failed") {
        class TestSpec extends AnyFeatureSpec {
          Scenario("fail scenario") {
            assert(1 === 2)
          }
          Feature("a feature") {
            Scenario("nested fail scenario") {
              assert(1 === 2)
            }
          }
        }
        val rep = new EventRecordingReporter
        val s1 = new TestSpec
        s1.run(None, Args(rep))
        assert(rep.testFailedEventsReceived.size === 2)
        assert(rep.testFailedEventsReceived(0).throwable.get.asInstanceOf[TestFailedException].failedCodeFileName.get === "DeprecatedFeatureSpecSpec.scala")
        assert(rep.testFailedEventsReceived(0).throwable.get.asInstanceOf[TestFailedException].failedCodeLineNumber.get === thisLineNumber - 13)
        assert(rep.testFailedEventsReceived(1).throwable.get.asInstanceOf[TestFailedException].failedCodeFileName.get === "DeprecatedFeatureSpecSpec.scala")
        assert(rep.testFailedEventsReceived(1).throwable.get.asInstanceOf[TestFailedException].failedCodeLineNumber.get === thisLineNumber - 11)
      }
      
      it("should generate NotAllowedException with correct stack depth info when has a feature nested inside a feature") {
        class TestSpec extends AnyFeatureSpec {
          Feature("a feature") {
            Feature("inner feature") {
              ignore("nested fail scenario") {
                assert(1 === 1)
              }
            }
          }
        }
        val rep = new EventRecordingReporter
        val caught = intercept[NotAllowedException] {
          new TestSpec
        }
        assert(caught.failedCodeFileName.get === "DeprecatedFeatureSpecSpec.scala")
        assert(caught.failedCodeLineNumber.get === thisLineNumber - 12)
      }
      
      it("should generate TestRegistrationClosedException with correct stack depth info when has a scenario nested inside a scenario") {
        class TestSpec extends AnyFeatureSpec {
          var registrationClosedThrown = false
          Feature("a feature") {
            Scenario("a scenario") {
              Scenario("nested scenario") {
                assert(1 === 2)
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
        assert(testFailedEvents.size === 1)
        assert(testFailedEvents(0).throwable.get.getClass() === classOf[TestRegistrationClosedException])
        val trce = testFailedEvents(0).throwable.get.asInstanceOf[TestRegistrationClosedException]
        assert("DeprecatedFeatureSpecSpec.scala" === trce.failedCodeFileName.get)
        assert(trce.failedCodeLineNumber.get === thisLineNumber - 24)
        assert(trce.message == Some("A scenario clause may not appear inside another scenario clause."))
      }

      it("should generate TestRegistrationClosedException with correct stack depth info when has a ignore nested inside a scenario") {
        class TestSpec extends AnyFeatureSpec {
          var registrationClosedThrown = false
          Feature("a feature") {
            Scenario("a scenario") {
              ignore("nested scenario") {
                assert(1 === 2)
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
        assert(testFailedEvents.size === 1)
        assert(testFailedEvents(0).throwable.get.getClass() === classOf[TestRegistrationClosedException])
        val trce = testFailedEvents(0).throwable.get.asInstanceOf[TestRegistrationClosedException]
        assert("DeprecatedFeatureSpecSpec.scala" === trce.failedCodeFileName.get)
        assert(trce.failedCodeLineNumber.get === thisLineNumber - 24)
        assert(trce.message == Some("An ignore clause may not appear inside a scenario clause."))
      }

      it("should generate TestRegistrationClosedException with correct stack depth info when has a registerTest nested inside a registerTest") {
        class TestSpec extends AnyFeatureSpec {
          var registrationClosedThrown = false
          Feature("a feature") {
            registerTest("a scenario") {
              registerTest("nested scenario") {
                assert(1 === 2)
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
        assert(testFailedEvents.size === 1)
        assert(testFailedEvents(0).throwable.get.getClass() === classOf[TestRegistrationClosedException])
        val trce = testFailedEvents(0).throwable.get.asInstanceOf[TestRegistrationClosedException]
        assert("DeprecatedFeatureSpecSpec.scala" === trce.failedCodeFileName.get)
        assert(trce.failedCodeLineNumber.get === thisLineNumber - 24)
        assert(trce.message == Some("Test cannot be nested inside another test."))
      }

      it("should generate TestRegistrationClosedException with correct stack depth info when has a registerIgnoredTest nested inside a registerTest") {
        class TestSpec extends AnyFeatureSpec {
          var registrationClosedThrown = false
          Feature("a feature") {
            registerTest("a scenario") {
              registerIgnoredTest("nested scenario") {
                assert(1 === 2)
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
        assert(testFailedEvents.size === 1)
        assert(testFailedEvents(0).throwable.get.getClass() === classOf[TestRegistrationClosedException])
        val trce = testFailedEvents(0).throwable.get.asInstanceOf[TestRegistrationClosedException]
        assert("DeprecatedFeatureSpecSpec.scala" === trce.failedCodeFileName.get)
        assert(trce.failedCodeLineNumber.get === thisLineNumber - 24)
        assert(trce.message == Some("Test cannot be nested inside another test."))
      }

      it("should generate NotAllowedException wrapping a TestFailedException when assert fails in scope") {
        class TestSpec extends AnyFeatureSpec {
          Feature("a feature") {
            val a = 1
            assert(a == 2)
          }
        }
        val e = intercept[NotAllowedException] {
          new TestSpec
        }
        assert("DeprecatedFeatureSpecSpec.scala" == e.failedCodeFileName.get)
        assert(e.failedCodeLineNumber.get == thisLineNumber - 7)
        assert(e.message == Some(FailureMessages.assertionShouldBePutInsideScenarioClauseNotFeatureClause))

        assert(e.cause.isDefined)
        val causeThrowable = e.cause.get
        assert(causeThrowable.isInstanceOf[TestFailedException])
        val cause = causeThrowable.asInstanceOf[TestFailedException]
        assert("DeprecatedFeatureSpecSpec.scala" == cause.failedCodeFileName.get)
        assert(cause.failedCodeLineNumber.get == thisLineNumber - 15)
        assert(cause.message == Some(FailureMessages.didNotEqual(prettifier, 1, 2)))
      }

      it("should generate NotAllowedException wrapping a TestCanceledException when assume fails in scope") {
        class TestSpec extends AnyFeatureSpec {
          Feature("a feature") {
            val a = 1
            assume(a == 2)
          }
        }
        val e = intercept[NotAllowedException] {
          new TestSpec
        }
        assert("DeprecatedFeatureSpecSpec.scala" == e.failedCodeFileName.get)
        assert(e.failedCodeLineNumber.get == thisLineNumber - 7)
        assert(e.message == Some(FailureMessages.assertionShouldBePutInsideScenarioClauseNotFeatureClause))

        assert(e.cause.isDefined)
        val causeThrowable = e.cause.get
        assert(causeThrowable.isInstanceOf[TestCanceledException])
        val cause = causeThrowable.asInstanceOf[TestCanceledException]
        assert("DeprecatedFeatureSpecSpec.scala" == cause.failedCodeFileName.get)
        assert(cause.failedCodeLineNumber.get == thisLineNumber - 15)
        assert(cause.message == Some(FailureMessages.didNotEqual(prettifier, 1, 2)))
      }

      it("should generate NotAllowedException wrapping a non-fatal RuntimeException is thrown inside scope") {
        class TestSpec extends AnyFeatureSpec {
          Feature("a feature") {
            throw new RuntimeException("on purpose")
          }
        }
        val e = intercept[NotAllowedException] {
          new TestSpec
        }
        assert("DeprecatedFeatureSpecSpec.scala" == e.failedCodeFileName.get)
        assert(e.failedCodeLineNumber.get == thisLineNumber - 8)
        assert(e.cause.isDefined)
        val causeThrowable = e.cause.get
        assert(e.message == Some(FailureMessages.exceptionWasThrownInFeatureClause(prettifier, UnquotedString(causeThrowable.getClass.getName), "a feature", "on purpose")))

        assert(causeThrowable.isInstanceOf[RuntimeException])
        val cause = causeThrowable.asInstanceOf[RuntimeException]
        assert(cause.getMessage == "on purpose")
      }

      it("should generate NotAllowedException wrapping a DuplicateTestNameException is thrown inside scope") {
        class TestSpec extends AnyFeatureSpec {
          Feature("a feature") {
            Scenario("test 1") {}
            Scenario("test 1") {}
          }
        }
        val e = intercept[NotAllowedException] {
          new TestSpec
        }
        assert("DeprecatedFeatureSpecSpec.scala" == e.failedCodeFileName.get)
        assert(e.failedCodeLineNumber.get == thisLineNumber - 7)
        assert(e.cause.isDefined)
        val causeThrowable = e.cause.get
        assert(e.message == Some(FailureMessages.exceptionWasThrownInFeatureClause(prettifier, UnquotedString(causeThrowable.getClass.getName), "a feature", FailureMessages.duplicateTestName(prettifier, UnquotedString("Feature: a feature Scenario: test 1")))))

        assert(causeThrowable.isInstanceOf[DuplicateTestNameException])
        val cause = causeThrowable.asInstanceOf[DuplicateTestNameException]
        assert(cause.getMessage == FailureMessages.duplicateTestName(prettifier, UnquotedString("Feature: a feature Scenario: test 1")))
      }

      // SKIP-SCALATESTJS,NATIVE-START
      it("should propagate AnnotationFormatError when it is thrown inside scope") {
        class TestSpec extends AnyFeatureSpec {
          Feature("a feature") {
            throw new AnnotationFormatError("on purpose")
          }
        }
        val e = intercept[AnnotationFormatError] {
          new TestSpec
        }
        assert(e.getMessage == "on purpose")
      }

      it("should propagate AWTError when it is thrown inside scope") {
        class TestSpec extends AnyFeatureSpec {
          Feature("a feature") {
            throw new AWTError("on purpose")
          }
        }
        val e = intercept[AWTError] {
          new TestSpec
        }
        assert(e.getMessage == "on purpose")
      }

      it("should propagate CoderMalfunctionError when it is thrown inside scope") {
        class TestSpec extends AnyFeatureSpec {
          Feature("a feature") {
            throw new CoderMalfunctionError(new RuntimeException("on purpose"))
          }
        }
        val e = intercept[CoderMalfunctionError] {
          new TestSpec
        }
        assert(e.getMessage == "java.lang.RuntimeException: on purpose")
      }

      it("should propagate FactoryConfigurationError when it is thrown inside scope") {
        class TestSpec extends AnyFeatureSpec {
          Feature("a feature") {
            throw new FactoryConfigurationError("on purpose")
          }
        }
        val e = intercept[FactoryConfigurationError] {
          new TestSpec
        }
        assert(e.getMessage == "on purpose")
      }

      it("should propagate LinkageError when it is thrown inside scope") {
        class TestSpec extends AnyFeatureSpec {
          Feature("a feature") {
            throw new LinkageError("on purpose")
          }
        }
        val e = intercept[LinkageError] {
          new TestSpec
        }
        assert(e.getMessage == "on purpose")
      }

      it("should propagate ThreadDeath when it is thrown inside scope") {
        class TestSpec extends AnyFeatureSpec {
          Feature("a feature") {
            throw new ThreadDeath
          }
        }
        val e = intercept[ThreadDeath] {
          new TestSpec
        }
        assert(e.getMessage == null)
      }

      it("should propagate TransformerFactoryConfigurationError when it is thrown inside scope") {
        class TestSpec extends AnyFeatureSpec {
          Feature("a feature") {
            throw new TransformerFactoryConfigurationError("on purpose")
          }
        }
        val e = intercept[TransformerFactoryConfigurationError] {
          new TestSpec
        }
        assert(e.getMessage == "on purpose")
      }

      it("should propagate VirtualMachineError when it is thrown inside scope") {
        class TestSpec extends AnyFeatureSpec {
          Feature("a feature") {
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
    ignore("should support expectations") { // Unignore after we uncomment the expectation implicits in RegistrationPolicy
      class TestSpec extends AnyFeatureSpec with expectations.Expectations {
        Scenario("fail scenario") {
          expect(1 === 2); /* ASSERTION_SUCCEED */
        }
        Feature("a feature") {
          Scenario("nested fail scenario") {
            expect(1 === 2); /* ASSERTION_SUCCEED */
          }
        }
      }
      val rep = new EventRecordingReporter
      val s1 = new TestSpec
      s1.run(None, Args(rep))
      assert(rep.testFailedEventsReceived.size === 2)
      assert(rep.testFailedEventsReceived(0).throwable.get.asInstanceOf[TestFailedException].failedCodeFileName.get === "DeprecatedFeatureSpecSpec.scala")
      assert(rep.testFailedEventsReceived(0).throwable.get.asInstanceOf[TestFailedException].failedCodeLineNumber.get === thisLineNumber - 13)
      assert(rep.testFailedEventsReceived(1).throwable.get.asInstanceOf[TestFailedException].failedCodeFileName.get === "DeprecatedFeatureSpecSpec.scala")
      assert(rep.testFailedEventsReceived(1).throwable.get.asInstanceOf[TestFailedException].failedCodeLineNumber.get === thisLineNumber - 11)
    }
  }
}
