/*
 * Copyright 2001-2008 Artima, Inc.
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
package org.scalatest.path

import org.scalatest._

import org.scalatest.path.{ FreeSpec => PathFreeSpec }
// elements
import org.scalatest.events._
import org.scalatest.exceptions.TestFailedException
import org.scalatest.exceptions.DuplicateTestNameException
import org.scalatest.exceptions.TestRegistrationClosedException

class FreeSpecSpec extends org.scalatest.FunSpec with SharedHelpers with GivenWhenThen {

  describe("A FreeSpec") {

    describe("(when a nesting rule has been violated)") {

      it("should, if they call a describe from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends PathFreeSpec {
          "should blow up" in {
            "in the wrong place, at the wrong time" - {
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a describe with a nested it from within an it clause, result in a TestFailedException when running the test") {
        class MySpec extends PathFreeSpec {
          "should blow up" in {
            "in the wrong place, at the wrong time" - {
              "should never run" in {
                assert(1 === 1)
              }
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }

      it("should, if they call a nested it from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends PathFreeSpec {
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

        class MySpec extends PathFreeSpec {
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

        class MySpec extends PathFreeSpec {
          "should blow up" in {
            "in the wrong place, at the wrong time" - {
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

        class MySpec extends PathFreeSpec {
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

        class MySpec extends PathFreeSpec {
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

      class AFreeSpec extends PathFreeSpec {
        "it should test this" in {}
        "it should test that" in {}
        override def newInstance = new AFreeSpec
      }
      val a = new AFreeSpec

      assertResult(List("it should test this", "it should test that")) {
        a.testNames.iterator.toList
      }

      class BFreeSpec extends PathFreeSpec
      val b = new BFreeSpec

      assertResult(List[String]()) {
        b.testNames.iterator.toList
      }

      class CFreeSpec extends PathFreeSpec {
        "it should test that" in {}
        "it should test this" in {}
        override def newInstance = new CFreeSpec
      }
      val c = new CFreeSpec

      assertResult(List("it should test that", "it should test this")) {
        c.testNames.iterator.toList
      }

      class DFreeSpec extends PathFreeSpec {
        "A Tester" - {
          "should test that" in {}
          "should test this" in {}
        }
        override def newInstance = new DFreeSpec
      }
      val d = new DFreeSpec

      assertResult(List("A Tester should test that", "A Tester should test this")) {
        d.testNames.iterator.toList
      }

      class EFreeSpec extends PathFreeSpec {
        "A Tester" - {
          "should test this" in {}
          "should test that" in {}
        }
        override def newInstance = new EFreeSpec
      }
      val e = new EFreeSpec

      assertResult(List("A Tester should test this", "A Tester should test that")) {
        e.testNames.iterator.toList
      }
    }
    // TODO: put a better message in the instantation exception or probably wrap it in something that 
    // has a better message, explaining the probable solutoin is to add an "override def newInstance"

    it("should throw DuplicateTestNameException if a duplicate test name registration is attempted") {
      
      intercept[DuplicateTestNameException] {
        class AFreeSpec extends  PathFreeSpec {
          "should test this" in {}
          "should test this" in {}
          override def newInstance = new AFreeSpec
        }
        (new AFreeSpec).tags // Must call a method to get it to attempt to register the second test
      }
      intercept[DuplicateTestNameException] {
        class AFreeSpec extends   PathFreeSpec {
          "should test this" in {}
          "should test this" ignore {}
          override def newInstance = new AFreeSpec
        }
        (new AFreeSpec).tags
      }
      intercept[DuplicateTestNameException] {
        class AFreeSpec extends   PathFreeSpec {
          "should test this" ignore {}
          "should test this" ignore {}
          override def newInstance = new AFreeSpec
        }
        (new AFreeSpec).tags
      }
      intercept[DuplicateTestNameException] {
        class AFreeSpec extends   PathFreeSpec {
          "should test this" ignore {}
          "should test this" in {}
          override def newInstance = new AFreeSpec
        }
        (new AFreeSpec).tags
      }
    }

    describe("(with info calls)") {
      class InfoInsideTestSpec extends PathFreeSpec {
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
      class InfoBeforeTestSpec extends PathFreeSpec {
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
        class MySpec extends PathFreeSpec {
          testName in {}
          info(msg)
        }
        val (infoProvidedIndex, testStartingIndex, testSucceededIndex) =
          getIndexesForInformerEventOrderTests(new MySpec, testName, msg)
        assert(testStartingIndex < testSucceededIndex)
        assert(testSucceededIndex < infoProvidedIndex)
      }
      it("should throw an IllegalStateException when info is called by a method invoked after the suite has been executed") {
        class MySpec extends PathFreeSpec {
          callInfo() // This should work fine
          def callInfo() {
            info("howdy")
          }
          "howdy also" in {
            callInfo() // This should work fine
          }
          override def newInstance = new MySpec
        }
        val spec = new MySpec
        val myRep = new EventRecordingReporter
        spec.run(None, Args(myRep))
        intercept[IllegalStateException] {
          spec.callInfo()
        }
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
        new PathFreeSpec {
          "hi" taggedAs(null) in {}
        }
      }
      val caught = intercept[NullPointerException] {
        new PathFreeSpec {
          "hi" taggedAs(mytags.SlowAsMolasses, null) in {}
        }
      }
      assert(caught.getMessage === "a test tag was null")
      intercept[NullPointerException] {
        new PathFreeSpec {
          "hi" taggedAs(mytags.SlowAsMolasses, null, mytags.WeakAsAKitten) in {}
        }
      }
      // ignore
      intercept[NullPointerException] {
        new PathFreeSpec {
          "hi" taggedAs(null) ignore {}
        }
      }
      val caught2 = intercept[NullPointerException] {
        new PathFreeSpec {
          "hi" taggedAs(mytags.SlowAsMolasses, null) ignore {}
        }
      }
      assert(caught2.getMessage === "a test tag was null")
      intercept[NullPointerException] {
        new PathFreeSpec {
          "hi" taggedAs(mytags.SlowAsMolasses, null, mytags.WeakAsAKitten) ignore {}
        }
      }
    }
    it("should return a correct tags map from the tags method using is (pending)") {

      class AFreeSpec extends PathFreeSpec {
        "test this" ignore {}
        "test that" is (pending)
        override def newInstance = new AFreeSpec
      }
      val a = new AFreeSpec
      assertResult(Map("test this" -> Set("org.scalatest.Ignore"))) {
        a.tags
      }

      class BFreeSpec extends PathFreeSpec {
        "test this" is (pending)
        "test that" ignore {}
        override def newInstance = new BFreeSpec
      }
      val b = new BFreeSpec
      assertResult(Map("test that" -> Set("org.scalatest.Ignore"))) {
        b.tags
      }

      class CFreeSpec extends PathFreeSpec {
        "test this" ignore {}
        "test that" ignore {}
        override def newInstance = new CFreeSpec
      }
      val c = new CFreeSpec
      assertResult(Map("test this" -> Set("org.scalatest.Ignore"), "test that" -> Set("org.scalatest.Ignore"))) {
        c.tags
      }

      class DFreeSpec extends PathFreeSpec {
        "test this" taggedAs(mytags.SlowAsMolasses) is (pending)
        "test that" taggedAs(mytags.SlowAsMolasses) ignore {}
        override def newInstance = new DFreeSpec
      }
      val d = new DFreeSpec
      assertResult(Map("test this" -> Set("org.scalatest.SlowAsMolasses"), "test that" -> Set("org.scalatest.Ignore", "org.scalatest.SlowAsMolasses"))) {
        d.tags
      }

      class EFreeSpec extends PathFreeSpec {
        "test this" is (pending)
        "test that" is (pending)
        override def newInstance = new EFreeSpec
      }
      val e = new EFreeSpec
      assertResult(Map()) {
        e.tags
      }

      class FFreeSpec extends PathFreeSpec {
        "test this" taggedAs(mytags.SlowAsMolasses, mytags.WeakAsAKitten) is (pending)
        "test that" taggedAs(mytags.SlowAsMolasses) in  {}
        override def newInstance = new FFreeSpec
      }
      val f = new FFreeSpec
      assertResult(Map("test this" -> Set("org.scalatest.SlowAsMolasses", "org.scalatest.WeakAsAKitten"), "test that" -> Set("org.scalatest.SlowAsMolasses"))) {
        f.tags
      }

      class GFreeSpec extends PathFreeSpec {
        "test this" taggedAs(mytags.SlowAsMolasses, mytags.WeakAsAKitten) is (pending)
        "test that" taggedAs(mytags.SlowAsMolasses) in  {}
        override def newInstance = new GFreeSpec
      }
      val g = new GFreeSpec
      assertResult(Map("test this" -> Set("org.scalatest.SlowAsMolasses", "org.scalatest.WeakAsAKitten"), "test that" -> Set("org.scalatest.SlowAsMolasses"))) {
        g.tags
      }
    }

    case class TestWasCalledCounts(var theTestThisCalled: Boolean, var theTestThatCalled: Boolean)

    class TestWasCalledSuite(val counts: TestWasCalledCounts) extends PathFreeSpec {
      "run this" in { counts.theTestThisCalled = true }
      "run that, maybe" in { counts.theTestThatCalled = true }
      override def newInstance = new TestWasCalledSuite(counts)
    }

    it("should execute all tests when run is called with testName None") {

      val b = new TestWasCalledSuite(TestWasCalledCounts(false, false))
      b.run(None, Args(SilentReporter))
      assert(b.counts.theTestThisCalled)
      assert(b.counts.theTestThatCalled)
    }

    it("should execute one test when run is called with a defined testName") {

      val a = new TestWasCalledSuite(TestWasCalledCounts(false, false))
      val rep = new EventRecordingReporter
      a.run(Some("run this"), Args(rep))
      assert(a.counts.theTestThisCalled)
      assert(a.counts.theTestThatCalled)
      rep.testSucceededEventsReceived
      // val tse = rep.testSucceededEventsReceived
      // assert(tse.size === 1)
      // 99
      val tse = rep.testSucceededEventsReceived
      assert(tse.size === 1)
      assert(tse(0).testName === "run this") 
      val tfe = rep.testFailedEventsReceived
      assert(tfe.size === 0)
      val tste = rep.testStartingEventsReceived
      assert(tste.size === 1)
    }

    it("should report as ignored, and not run, tests marked ignored") {

      class AFreeSpec(val counts: TestWasCalledCounts) extends PathFreeSpec {
        "test this" in { counts.theTestThisCalled = true }
        "test that" in { counts.theTestThatCalled = true }
        override def newInstance = new AFreeSpec(counts)
      }
      val a = new AFreeSpec(TestWasCalledCounts(false, false))

      val repA = new TestIgnoredTrackingReporter
      a.run(None, Args(repA))
      assert(!repA.testIgnoredReceived)
      assert(a.counts.theTestThisCalled)
      assert(a.counts.theTestThatCalled)

      class BFreeSpec(val counts: TestWasCalledCounts) extends PathFreeSpec {
        "test this" ignore { counts.theTestThisCalled = true }
        "test that" in { counts.theTestThatCalled = true }
        override def newInstance = new BFreeSpec(counts)
      }
      val b = new BFreeSpec(TestWasCalledCounts(false, false))

      val repB = new TestIgnoredTrackingReporter
      b.run(None, Args(repB))
      assert(repB.testIgnoredReceived)
      assert(repB.lastEvent.isDefined)
      assert(repB.lastEvent.get.testName endsWith "test this")
      assert(!b.counts.theTestThisCalled)
      assert(b.counts.theTestThatCalled)

      class CFreeSpec(val counts: TestWasCalledCounts) extends PathFreeSpec {
        "test this" in { counts.theTestThisCalled = true }
        "test that" ignore { counts.theTestThatCalled = true }
        override def newInstance = new CFreeSpec(counts)
      }
      val c = new CFreeSpec(TestWasCalledCounts(false, false))

      val repC = new TestIgnoredTrackingReporter
      c.run(None, Args(repC))
      assert(repC.testIgnoredReceived)
      assert(repC.lastEvent.isDefined)
      assert(repC.lastEvent.get.testName endsWith "test that", repC.lastEvent.get.testName)
      assert(c.counts.theTestThisCalled)
      assert(!c.counts.theTestThatCalled)

      // The order I want is order of appearance in the file.
      // Will try and implement that tomorrow. Subtypes will be able to change the order.
      class DFreeSpec(val counts: TestWasCalledCounts) extends PathFreeSpec {
        "test this" ignore { counts.theTestThisCalled = true }
        "test that" ignore { counts.theTestThatCalled = true }
        override def newInstance = new DFreeSpec(counts)
      }
      val d = new DFreeSpec(TestWasCalledCounts(false, false))

      val repD = new TestIgnoredTrackingReporter
      d.run(None, Args(repD))
      assert(repD.testIgnoredReceived)
      assert(repD.lastEvent.isDefined)
      assert(repD.lastEvent.get.testName endsWith "test that") // last because should be in order of appearance
      assert(!d.counts.theTestThisCalled)
      assert(!d.counts.theTestThatCalled)
    }

    it("should ignore a test marked as ignored if run is invoked with that testName") {
      // If I provide a specific testName to run, then it should ignore an Ignore on that test
      // method and actually invoke it.
      class EFreeSpec extends PathFreeSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        "test this" ignore { theTestThisCalled = true }
        "test that" in { theTestThatCalled = true }
        override def newInstance = new EFreeSpec
      }
      val e = new EFreeSpec

      val repE = new TestIgnoredTrackingReporter
      e.run(Some("test this"), Args(repE))
      assert(repE.testIgnoredReceived)
      assert(!e.theTestThisCalled)
      assert(!e.theTestThatCalled)
    }

    it("should run only those tests selected by the tags to include and exclude sets") {

      // Nothing is excluded
      class AFreeSpec(val counts: TestWasCalledCounts) extends PathFreeSpec {
        "test this" taggedAs(mytags.SlowAsMolasses) in { counts.theTestThisCalled = true }
        "test that" in { counts.theTestThatCalled = true }
        override def newInstance = new AFreeSpec(counts)
      }
      val a = new AFreeSpec(TestWasCalledCounts(false, false))
      val repA = new TestIgnoredTrackingReporter
      a.run(None, Args(repA))
      assert(!repA.testIgnoredReceived)
      assert(a.counts.theTestThisCalled)
      assert(a.counts.theTestThatCalled)

      // SlowAsMolasses is included, one test should be excluded
      class BFreeSpec(val counts: TestWasCalledCounts) extends PathFreeSpec {
        "test this" taggedAs(mytags.SlowAsMolasses) in { counts.theTestThisCalled = true }
        "test that" in { counts.theTestThatCalled = true }
        override def newInstance = new BFreeSpec(counts)
      }
      val b = new BFreeSpec(TestWasCalledCounts(false, false))
      val repB = new EventRecordingReporter
      b.run(None, Args(repB, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(repB.testIgnoredEventsReceived.isEmpty)
      assert(b.counts.theTestThisCalled)
      assert(b.counts.theTestThatCalled)
      assert(repB.testStartingEventsReceived.size === 1)
      assert(repB.testStartingEventsReceived(0).testName == "test this")

      // SlowAsMolasses is included, and both tests should be included
      class CFreeSpec(val counts: TestWasCalledCounts) extends PathFreeSpec {
        "test this" taggedAs(mytags.SlowAsMolasses) in { counts.theTestThisCalled = true }
        "test that" taggedAs(mytags.SlowAsMolasses) in { counts.theTestThatCalled = true }
        override def newInstance = new CFreeSpec(counts)
      }
      val c = new CFreeSpec(TestWasCalledCounts(false, false))
      val repC = new EventRecordingReporter
      c.run(None, Args(repC, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(repC.testIgnoredEventsReceived.isEmpty)
      assert(c.counts.theTestThisCalled)
      assert(c.counts.theTestThatCalled)
      assert(repC.testStartingEventsReceived.size === 2)

      // SlowAsMolasses is included. both tests should be included but one ignored
      class DFreeSpec(val counts: TestWasCalledCounts) extends PathFreeSpec {
        "test this" taggedAs(mytags.SlowAsMolasses) ignore { counts.theTestThisCalled = true }
        "test that" taggedAs(mytags.SlowAsMolasses) in { counts.theTestThatCalled = true }
        override def newInstance = new DFreeSpec(counts)
      }
      val d = new DFreeSpec(TestWasCalledCounts(false, false))
      val repD = new EventRecordingReporter
      d.run(None, Args(repD, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.Ignore")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(repD.testIgnoredEventsReceived.size === 1)
      assert(!d.counts.theTestThisCalled)
      assert(d.counts.theTestThatCalled)
      assert(repD.testStartingEventsReceived.size === 1)
      assert(repD.testStartingEventsReceived(0).testName === "test that")

      case class ThreeCounts(var theTestThisCalled: Boolean, var theTestThatCalled: Boolean, var theTestTheOtherCalled: Boolean)
      // SlowAsMolasses included, FastAsLight excluded
      class EFreeSpec(val counts: ThreeCounts) extends PathFreeSpec {
        "test this" taggedAs(mytags.SlowAsMolasses, mytags.FastAsLight) in { counts.theTestThisCalled = true }
        "test that" taggedAs(mytags.SlowAsMolasses) in { counts.theTestThatCalled = true }
        "test the other" in { counts.theTestTheOtherCalled = true }
        override def newInstance = new EFreeSpec(counts)
      }
      val e = new EFreeSpec(ThreeCounts(false, false, false))
      val repE = new EventRecordingReporter
      e.run(None, Args(repE, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
                ConfigMap.empty, None, new Tracker, Set.empty))
      assert(repE.testIgnoredEventsReceived.isEmpty)
      assert(e.counts.theTestThisCalled)
      assert(e.counts.theTestThatCalled)
      assert(e.counts.theTestTheOtherCalled)
      assert(repE.testStartingEventsReceived.size === 1)
      assert(repE.testStartingEventsReceived(0).testName === "test that")

      // An Ignored test that was both included and excluded should not generate a TestIgnored event
      class FFreeSpec(val counts: ThreeCounts) extends PathFreeSpec {
        "test this" taggedAs(mytags.SlowAsMolasses, mytags.FastAsLight) ignore { counts.theTestThisCalled = true }
        "test that" taggedAs(mytags.SlowAsMolasses) in { counts.theTestThatCalled = true }
        "test the other" in { counts.theTestTheOtherCalled = true }
        override def newInstance = new FFreeSpec(counts)
      }
      val f = new FFreeSpec(ThreeCounts(false, false, false))
      val repF = new EventRecordingReporter
      f.run(None, Args(repF, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
                ConfigMap.empty, None, new Tracker, Set.empty))
      assert(repE.testIgnoredEventsReceived.isEmpty)
      assert(!f.counts.theTestThisCalled)
      assert(f.counts.theTestThatCalled)
      assert(f.counts.theTestTheOtherCalled)
      assert(repE.testStartingEventsReceived.size === 1)
      assert(repE.testStartingEventsReceived(0).testName === "test that")

      // An Ignored test that was not included should not generate a TestIgnored event
      class GFreeSpec(val counts: ThreeCounts) extends PathFreeSpec {
        "test this" taggedAs(mytags.SlowAsMolasses, mytags.FastAsLight) in { counts.theTestThisCalled = true }
        "test that" taggedAs(mytags.SlowAsMolasses) in { counts.theTestThatCalled = true }
        "test the other" ignore { counts.theTestTheOtherCalled = true }
        override def newInstance = new GFreeSpec(counts)
      }
      val g = new GFreeSpec(ThreeCounts(false, false, false))
      val repG = new EventRecordingReporter
      g.run(None, Args(repG, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
                ConfigMap.empty, None, new Tracker, Set.empty))
      assert(repG.testIgnoredEventsReceived.isEmpty)
      assert(g.counts.theTestThisCalled)
      assert(g.counts.theTestThatCalled)
      assert(!g.counts.theTestTheOtherCalled)
      assert(repG.testStartingEventsReceived.size === 1)
      assert(repG.testStartingEventsReceived(0).testName === "test that")

      // No tagsToInclude set, FastAsLight excluded
      class HFreeSpec(val counts: ThreeCounts) extends PathFreeSpec {
        "test this" taggedAs(mytags.SlowAsMolasses, mytags.FastAsLight) in { counts.theTestThisCalled = true }
        "test that" taggedAs(mytags.SlowAsMolasses) in { counts.theTestThatCalled = true }
        "test the other" in { counts.theTestTheOtherCalled = true }
        override def newInstance = new HFreeSpec(counts)
      }
      val h = new HFreeSpec(ThreeCounts(false, false, false))
      val repH = new EventRecordingReporter
      h.run(None, Args(repH, Stopper.default, Filter(None, Set("org.scalatest.FastAsLight")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(repH.testIgnoredEventsReceived.isEmpty)
      assert(h.counts.theTestThisCalled)
      assert(h.counts.theTestThatCalled)
      assert(h.counts.theTestTheOtherCalled)
      assert(repH.testStartingEventsReceived.size === 2)
      assert(repH.testStartingEventsReceived.exists(_.testName == "test that"))
      assert(repH.testStartingEventsReceived.exists(_.testName == "test the other"))

      // No tagsToInclude set, mytags.SlowAsMolasses excluded
      class IFreeSpec(val counts: ThreeCounts) extends PathFreeSpec {
        "test this" taggedAs(mytags.SlowAsMolasses, mytags.FastAsLight) in { counts.theTestThisCalled = true }
        "test that" taggedAs(mytags.SlowAsMolasses) in { counts.theTestThatCalled = true }
        "test the other" in { counts.theTestTheOtherCalled = true }
        override def newInstance = new IFreeSpec(counts)
      }
      val i = new IFreeSpec(ThreeCounts(false, false, false))
      val repI = new EventRecordingReporter
      i.run(None, Args(repI, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(repI.testIgnoredEventsReceived.isEmpty)
      assert(i.counts.theTestThisCalled)
      assert(i.counts.theTestThatCalled)
      assert(i.counts.theTestTheOtherCalled)
      assert(repI.testStartingEventsReceived.size === 1)
      assert(repI.testStartingEventsReceived(0).testName === "test the other")

      // No tagsToInclude set, mytags.SlowAsMolasses excluded, TestIgnored should not be received on excluded ones
      class JFreeSpec(val counts: ThreeCounts) extends PathFreeSpec {
        "test this" taggedAs(mytags.SlowAsMolasses, mytags.FastAsLight) ignore { counts.theTestThisCalled = true }
        "test that" taggedAs(mytags.SlowAsMolasses) ignore { counts.theTestThatCalled = true }
        "test the other" in { counts.theTestTheOtherCalled = true }
        override def newInstance = new JFreeSpec(counts)
      }
      val j = new JFreeSpec(ThreeCounts(false, false, false))
      val repJ = new TestIgnoredTrackingReporter
      j.run(None, Args(repJ, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repJ.testIgnoredReceived)
      assert(!j.counts.theTestThisCalled)
      assert(!j.counts.theTestThatCalled)
      assert(j.counts.theTestTheOtherCalled)

      // Same as previous, except Ignore specifically mentioned in excludes set
      class KFreeSpec(val counts: ThreeCounts) extends PathFreeSpec {
        "test this" taggedAs(mytags.SlowAsMolasses, mytags.FastAsLight) ignore { counts.theTestThisCalled = true }
        "test that" taggedAs(mytags.SlowAsMolasses) ignore { counts.theTestThatCalled = true }
        "test the other" ignore { counts.theTestTheOtherCalled = true }
        override def newInstance = new KFreeSpec(counts)
      }
      val k = new KFreeSpec(ThreeCounts(false, false, false))
      val repK = new TestIgnoredTrackingReporter
      k.run(None, Args(repK, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses", "org.scalatest.Ignore")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(repK.testIgnoredReceived)
      assert(!k.counts.theTestThisCalled)
      assert(!k.counts.theTestThatCalled)
      assert(!k.counts.theTestTheOtherCalled)
    }

    it("should return the correct test count from its expectedTestCount method") {

      class AFreeSpec extends PathFreeSpec {
        "test this" in {}
        "test that" in {}
        override def newInstance = new AFreeSpec
      }
      val a = new AFreeSpec
      assert(a.expectedTestCount(Filter()) === 2)

      class BFreeSpec extends PathFreeSpec {
        "test this" ignore {}
        "test that" in {}
        override def newInstance = new BFreeSpec
      }
      val b = new BFreeSpec
      assert(b.expectedTestCount(Filter()) === 1)

      class CFreeSpec extends PathFreeSpec {
        "test this" taggedAs(mytags.FastAsLight) in {}
        "test that" in {}
        override def newInstance = new CFreeSpec
      }
      val c = new CFreeSpec
      assert(c.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 1)
      assert(c.expectedTestCount(Filter(None, Set("org.scalatest.FastAsLight"))) === 1)

      class DFreeSpec extends PathFreeSpec {
        "test this" taggedAs(mytags.FastAsLight, mytags.SlowAsMolasses) in {}
        "test that" taggedAs(mytags.SlowAsMolasses) in {}
        "test the other thing" in {}
        override def newInstance = new DFreeSpec
      }
      val d = new DFreeSpec
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 1)
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight"))) === 1)
      assert(d.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) === 1)
      assert(d.expectedTestCount(Filter()) === 3)

      class EFreeSpec extends PathFreeSpec {
        "test this" taggedAs(mytags.FastAsLight, mytags.SlowAsMolasses) in {}
        "test that" taggedAs(mytags.SlowAsMolasses) in {}
        "test the other thing" ignore {}
        override def newInstance = new EFreeSpec
      }
      val e = new EFreeSpec
      assert(e.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 1)
      assert(e.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight"))) === 1)
      assert(e.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) === 0)
      assert(e.expectedTestCount(Filter()) === 2)

      val f = new Suites(a, b, c, d, e)
      assert(f.expectedTestCount(Filter()) === 10)
    }
    it("should generate a TestPending message when the test body is (pending)") {
      class AFreeSpec extends PathFreeSpec {

        "should do this" is (pending)

        "should do that" in {
          assert(2 + 2 === 4)
        }
        "should do something else" in {
          assert(2 + 2 === 4)
          pending
        }
        override def newInstance = new AFreeSpec
      }
      val a = new AFreeSpec
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      val tp = rep.testPendingEventsReceived
      assert(tp.size === 2)
    }
    it("should generate a test failure if a Throwable, or an Error other than direct Error subtypes " +
            "known in JDK 1.5, excluding AssertionError") {
      class AFreeSpec extends PathFreeSpec {
        "This FreeSpec" - {
          "should throw AssertionError" in { throw new AssertionError }
          "should throw plain old Error" in { throw new Error }
          "should throw Throwable" in { throw new Throwable }
        }
        override def newInstance = new AFreeSpec
      }
      val a = new AFreeSpec
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      val tf = rep.testFailedEventsReceived
      assert(tf.size === 3)
    }
    it("should propagate out Errors that are direct subtypes of Error in JDK 1.5, other than " +
            "AssertionError, causing Suites and Runs to abort.") {
      class AFreeSpec extends PathFreeSpec {
        "This FreeSpec" - {
          "should throw AssertionError" in { throw new OutOfMemoryError }
        }
        override def newInstance = new AFreeSpec
      }
      // val a = new AFreeSpec
      intercept[OutOfMemoryError] {
        new AFreeSpec
        // a.run(None, Args(SilentReporter))
      }
    }
/*
    it("should send InfoProvided events with aboutAPendingTest set to true for info " +
            "calls made from a test that is pending") {
      class AFreeSpec extends PathFreeSpec with GivenWhenThen {
        "A FreeSpec" - {
          "should do something" in {
            given("two integers")
            when("one is subracted from the other")
            then("the result is the difference between the two numbers")
            pending
          }
        }
        override def newInstance = new AFreeSpec
      }
      val a = new AFreeSpec
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      val testPending = rep.testPendingEventsReceived
      assert(testPending.size === 1)
      val recordedEvents = testPending(0).recordedEvents
      val so = rep.scopeOpenedEventsReceived
      val sc = rep.scopeClosedEventsReceived
      assert(recordedEvents.size === 3)
      assert(so.size === 1)
      assert(sc.size === 1)
      for (event <- recordedEvents) {
        val ip = event.asInstanceOf[InfoProvided]
        assert(ip.message == "A FreeSpec" || ip.aboutAPendingTest.isDefined && ip.aboutAPendingTest.get)
      }
    }
    it("should send InfoProvided events with aboutAPendingTest set to false for info " +
            "calls made from a test that is not pending") {
      class AFreeSpec extends PathFreeSpec with GivenWhenThen {
        "A FreeSpec" - {
          "should do something" in {
            given("two integers")
            when("one is subracted from the other")
            then("the result is the difference between the two numbers")
            assert(1 + 1 === 2)
          }
        }
        override def newInstance = new AFreeSpec
      }
      val a = new AFreeSpec
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      val testSucceeded = rep.testSucceededEventsReceived
      assert(testSucceeded.size === 1)
      val recordedEvents = testSucceeded(0).recordedEvents
      val so = rep.scopeOpenedEventsReceived
      val sc = rep.scopeClosedEventsReceived
      assert(recordedEvents.size === 3)
      assert(so.size === 1)
      assert(sc.size === 1)
      for (event <- recordedEvents) {
        val ip = event.asInstanceOf[InfoProvided]
        assert(ip.message == "A FreeSpec" || ip.aboutAPendingTest.isDefined && !ip.aboutAPendingTest.get)
      }
    }
*/
    it("should not put parentheses around should clauses that follow when") {
      class AFreeSpec extends PathFreeSpec {
        "A Stack" - {
          "when empty" - {
            "should chill out" in {
              assert(1 + 1 === 2)
            }
          }
        }
        override def newInstance = new AFreeSpec
      }
      val a = new AFreeSpec
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      val ts = rep.testSucceededEventsReceived
      assert(ts.size === 1)
      assert(ts.head.testName === "A Stack when empty should chill out")
    }
    it("should not put parentheses around should clauses that don't follow when") {
      class AFreeSpec extends PathFreeSpec {
        "A Stack" - {
          "should chill out" in {
            assert(1 + 1 === 2)
          }
        }
        override def newInstance = new AFreeSpec
      }
      val a = new AFreeSpec
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      val ts = rep.testSucceededEventsReceived
      assert(ts.size === 1)
      assert(ts.head.testName === "A Stack should chill out")
    }
    
    it ("should report the duration of the actuall running of the test, not the replaying of the test") {
      class AFreeSpec extends PathFreeSpec {
        "A Stack" - {
          "should chill out" in {
            Thread.sleep(100)
          }
        }
        override def newInstance = new AFreeSpec        
      }
      val a = new AFreeSpec
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      val ts = rep.testSucceededEventsReceived
      assert(ts.size === 1)
      import OptionValues._
      val dura = ts.head.duration.value
      assert(dura > 80, "duration was: " + dura)
    }
  }
  
  describe("when failure happens") {
    
    it("should fire TestFailed event with correct stack depth info when test failed") {
      class TestSpec extends FreeSpec {
        "fail scenario" in {
          assert(1 === 2)
        }
        "a feature" - {
          "nested fail scenario" in {
            assert(1 === 2)
          }
        }
        override def newInstance = new TestSpec
      }
      val rep = new EventRecordingReporter
      val s1 = new TestSpec
      s1.run(None, Args(rep))
      assert(rep.testFailedEventsReceived.size === 2)
      assert(rep.testFailedEventsReceived(0).throwable.get.asInstanceOf[TestFailedException].failedCodeFileName.get === "FreeSpecSpec.scala")
      assert(rep.testFailedEventsReceived(0).throwable.get.asInstanceOf[TestFailedException].failedCodeLineNumber.get === thisLineNumber - 14)
      assert(rep.testFailedEventsReceived(1).throwable.get.asInstanceOf[TestFailedException].failedCodeFileName.get === "FreeSpecSpec.scala")
      assert(rep.testFailedEventsReceived(1).throwable.get.asInstanceOf[TestFailedException].failedCodeLineNumber.get === thisLineNumber - 12)
    }
    
    it("should generate TestRegistrationClosedException with correct stack depth info when has an in nested inside an in") {
      class TestSpec extends FreeSpec {
        //var registrationClosedThrown = false
        "a feature" - {
          "a scenario" in {
            "nested scenario" in {
              
            }
          }
        }
        override def newInstance = new TestSpec
      }
      val rep = new EventRecordingReporter
      val s = new TestSpec
      s.run(None, Args(rep))
      val testFailedEvents = rep.testFailedEventsReceived
      assert(testFailedEvents.size === 1)
      assert(testFailedEvents(0).throwable.get.getClass() === classOf[TestRegistrationClosedException])
      val trce = testFailedEvents(0).throwable.get.asInstanceOf[TestRegistrationClosedException]
      assert("FreeSpecSpec.scala" === trce.failedCodeFileName.get)
      assert(trce.failedCodeLineNumber.get === thisLineNumber - 15)
    }
  }
}
