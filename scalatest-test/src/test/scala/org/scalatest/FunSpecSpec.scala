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
import org.scalatest.exceptions.DuplicateTestNameException
import org.scalatest.exceptions.TestCanceledException
import org.scalatest.exceptions.NotAllowedException
import java.lang.annotation.AnnotationFormatError
import java.awt.AWTError
import java.nio.charset.CoderMalfunctionError
import javax.xml.parsers.FactoryConfigurationError
import javax.xml.transform.TransformerFactoryConfigurationError
import org.scalatest.exceptions.TestFailedException
import org.scalatest.exceptions.TestRegistrationClosedException
import org.scalactic.exceptions.NullArgumentException

class FunSpecSpec extends FunSpec with GivenWhenThen {

  describe("A FunSpec") {

    it("should return the test names in registration order from testNames") {

      val a = new FunSpec {
        it("should test this") {}
        it("should test that") {}
      }

      assertResult(List("should test this", "should test that")) {
        a.testNames.iterator.toList
      }

      val b = new FunSpec {}

      assertResult(List[String]()) {
        b.testNames.iterator.toList
      }

      val c = new FunSpec {
        it("should test that") {}
        it("should test this") {}
      }

      assertResult(List("should test that", "should test this")) {
        c.testNames.iterator.toList
      }

      val d = new FunSpec {
        describe("A Tester") {
          it("should test that") {}
          it("should test this") {}
        }
      }

      assertResult(List("A Tester should test that", "A Tester should test this")) {
        d.testNames.iterator.toList
      }

      val e = new FunSpec {
        describe("A Tester") {
          it("should test this") {}
          it("should test that") {}
        }
      }

      assertResult(List("A Tester should test this", "A Tester should test that")) {
        e.testNames.iterator.toList
      }
    }

    it("should throw DuplicateTestNameException if a duplicate test name registration is attempted") {
      
      intercept[DuplicateTestNameException] {
        new FunSpec {
          it("should test this") {}
          it("should test this") {}
        }
      }
      intercept[DuplicateTestNameException] {
        new FunSpec {
          it("should test this") {}
          ignore("should test this") {}
        }
      }
      intercept[DuplicateTestNameException] {
        new FunSpec {
          ignore("should test this") {}
          ignore("should test this") {}
        }
      }
      intercept[DuplicateTestNameException] {
        new FunSpec {
          ignore("should test this") {}
          it("should test this") {}
        }
      }
    }

    it("should invoke withFixture from runTest") {
      val a = new FunSpec {
        var withFixtureWasInvoked = false
        var testWasInvoked = false
        override def withFixture(test: NoArgTest): Outcome = {
          withFixtureWasInvoked = true
          super.withFixture(test)
        }
        it("should do something") {
          testWasInvoked = true
        }
      }

      import scala.language.reflectiveCalls

      a.run(None, Args(SilentReporter))
      assert(a.withFixtureWasInvoked)
      assert(a.testWasInvoked)
    }
    it("should pass the correct test name in the NoArgTest passed to withFixture") {
      val a = new FunSpec {
        var correctTestNameWasPassed = false
        override def withFixture(test: NoArgTest): Outcome = {
          correctTestNameWasPassed = test.name == "should do something"
          super.withFixture(test)
        }
        it("should do something") {}
      }

      import scala.language.reflectiveCalls

      a.run(None, Args(SilentReporter))
      assert(a.correctTestNameWasPassed)
    }
    it("should pass the correct config map in the NoArgTest passed to withFixture") {
      val a = new FunSpec {
        var correctConfigMapWasPassed = false
        override def withFixture(test: NoArgTest): Outcome = {
          correctConfigMapWasPassed = (test.configMap == ConfigMap("hi" -> 7))
          super.withFixture(test)
        }
        it("should do something") {}
      }

      import scala.language.reflectiveCalls

      a.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap("hi" -> 7), None, new Tracker(), Set.empty))
      assert(a.correctConfigMapWasPassed)
    }
    describe("(with info calls)") {
      class InfoInsideTestSpec extends FunSpec {
        val msg = "hi there, dude"
        val testName = "test name"
        it(testName) {
          info(msg)
        }
      }
      // In a Spec, any InfoProvided's fired during the test should be cached and sent out together with
      // test completed event. This makes the report look nicer, because the info is tucked under the "specifier'
      // text for that test.
      it("should, when the info appears in the code of a successful test, report the info in the TestSucceeded") {
        val spec = new InfoInsideTestSpec
        val (testStartingIndex, testSucceededIndex) =
          getIndexesForTestInformerEventOrderTests(spec, spec.testName, spec.msg)
        assert(testStartingIndex < testSucceededIndex)
      }
      class InfoBeforeTestSpec extends FunSpec {
        val msg = "hi there, dude"
        val testName = "test name"
        info(msg)
        it(testName) {}
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
        class MySpec extends FunSpec {
          it(testName) {}
          info(msg)
        }
        val (infoProvidedIndex, testStartingIndex, testSucceededIndex) =
          getIndexesForInformerEventOrderTests(new MySpec, testName, msg)
        assert(testStartingIndex < testSucceededIndex)
        assert(testSucceededIndex < infoProvidedIndex)
      }
      it("should print to stdout when info is called by a method invoked after the suite has been executed") {
        class MySpec extends FunSpec {
          callInfo() // This should work fine
          def callInfo() {
            info("howdy")
          }
          it("howdy also") {
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

    describe("(when a nesting rule has been violated)") {

      it("should, if they call a describe from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends FunSpec {
          it("should blow up") {
            describe("in the wrong place, at the wrong time") {
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a describe with a nested it from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends FunSpec {
          it("should blow up") {
            describe("in the wrong place, at the wrong time") {
              it("should never run") {
                assert(1 === 1)
              }
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a nested it from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends FunSpec {
          it("should blow up") {
            it("should never run") {
              assert(1 === 1)
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a nested it with tags from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends FunSpec {
          it("should blow up") {
            it("should never run", mytags.SlowAsMolasses) {
              assert(1 == 1)
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a nested registerTest with tags from within an registerTest clause, result in a TestFailedException when running the test") {

        class MySpec extends FunSpec {
          registerTest("should blow up") {
            registerTest("should never run", mytags.SlowAsMolasses) {
              assert(1 == 1)
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a describe with a nested ignore from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends FunSpec {
          it("should blow up") {
            describe("in the wrong place, at the wrong time") {
              ignore("should never run") {
                assert(1 === 1)
              }
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a nested ignore from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends FunSpec {
          it("should blow up") {
            ignore("should never run") {
              assert(1 === 1)
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a nested ignore with tags from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends FunSpec {
          it("should blow up") {
            ignore("should never run", mytags.SlowAsMolasses) {
              assert(1 == 1)
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a nested registerIgnoredTest with tags from within an registerTest clause, result in a TestFailedException when running the test") {

        class MySpec extends FunSpec {
          registerTest("should blow up") {
            registerIgnoredTest("should never run", mytags.SlowAsMolasses) {
              assert(1 == 1)
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "should blow up")
      }
    }
    it("should run tests registered via the 'it should behave like' syntax") {
      trait SharedSpecTests { this: FunSpec =>
        def nonEmptyStack(s: String)(i: Int) {
          it("should be that I am shared") {}
        }
      }
      class MySpec extends FunSpec with SharedSpecTests {
        it should behave like nonEmptyStack("hi")(1)
      }
      val suite = new MySpec
      val reporter = new EventRecordingReporter
      suite.run(None, Args(reporter))

      val indexedList = reporter.eventsReceived

      val testStartingOption = indexedList.find(_.isInstanceOf[TestStarting])
      assert(testStartingOption.isDefined)
      assert(testStartingOption.get.asInstanceOf[TestStarting].testName === "should be that I am shared")
    }

    it("should throw NullArgumentException if a null test tag is provided") {
      // it
      intercept[NullArgumentException] {
        new FunSpec {
          it("hi", null) {}
        }
      }
      val caught = intercept[NullArgumentException] {
        new FunSpec {
          it("hi", mytags.SlowAsMolasses, null) {}
        }
      }
      assert(caught.getMessage == "a test tag was null")
      intercept[NullArgumentException] {
        new FunSpec {
          it("hi", mytags.SlowAsMolasses, null, mytags.WeakAsAKitten) {}
        }
      }

      // ignore
      intercept[NullArgumentException] {
        new FunSpec {
          ignore("hi", null) {}
        }
      }
      val caught2 = intercept[NullArgumentException] {
        new FunSpec {
          ignore("hi", mytags.SlowAsMolasses, null) {}
        }
      }
      assert(caught2.getMessage == "a test tag was null")
      intercept[NullArgumentException] {
        new FunSpec {
          ignore("hi", mytags.SlowAsMolasses, null, mytags.WeakAsAKitten) {}
        }
      }

      // registerTest
      intercept[NullArgumentException] {
        new FunSpec {
          registerTest("hi", null) {}
        }
      }
      val caught3 = intercept[NullArgumentException] {
        new FunSpec {
          registerTest("hi", mytags.SlowAsMolasses, null) {}
        }
      }
      assert(caught3.getMessage === "a test tag was null")
      intercept[NullArgumentException] {
        new FunSpec {
          registerTest("hi", mytags.SlowAsMolasses, null, mytags.WeakAsAKitten) {}
        }
      }

      // registerIgnoredTest
      intercept[NullArgumentException] {
        new FunSpec {
          registerIgnoredTest("hi", null) {}
        }
      }
      val caught4 = intercept[NullArgumentException] {
        new FunSpec {
          registerIgnoredTest("hi", mytags.SlowAsMolasses, null) {}
        }
      }
      assert(caught4.getMessage == "a test tag was null")
      intercept[NullArgumentException] {
        new FunSpec {
          registerIgnoredTest("hi", mytags.SlowAsMolasses, null, mytags.WeakAsAKitten) {}
        }
      }
    }
    class TestWasCalledSuite extends FunSpec {
      var theTestThisCalled = false
      var theTestThatCalled = false
      it("should run this") { theTestThisCalled = true }
      it("should run that, maybe") { theTestThatCalled = true }
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

      val a = new FunSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        it("test this") { theTestThisCalled = true }
        it("test that") { theTestThatCalled = true }
      }

      import scala.language.reflectiveCalls

      val repA = new TestIgnoredTrackingReporter
      a.run(None, Args(repA))
      assert(!repA.testIgnoredReceived)
      assert(a.theTestThisCalled)
      assert(a.theTestThatCalled)

      val b = new FunSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        ignore("test this") { theTestThisCalled = true }
        it("test that") { theTestThatCalled = true }
      }

      val repB = new TestIgnoredTrackingReporter
      b.run(None, Args(repB))
      assert(repB.testIgnoredReceived)
      assert(repB.lastEvent.isDefined)
      assert(repB.lastEvent.get.testName endsWith "test this")
      assert(!b.theTestThisCalled)
      assert(b.theTestThatCalled)

      val c = new FunSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        it("test this") { theTestThisCalled = true }
        ignore("test that") { theTestThatCalled = true }
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
      val d = new FunSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        ignore("test this") { theTestThisCalled = true }
        ignore("test that") { theTestThatCalled = true }
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
      val e = new FunSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        ignore("test this") { theTestThisCalled = true }
        it("test that") { theTestThatCalled = true }
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
      val a = new FunSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        it("test this", mytags.SlowAsMolasses) { theTestThisCalled = true }
        it("test that") { theTestThatCalled = true }
      }

      import scala.language.reflectiveCalls

      val repA = new TestIgnoredTrackingReporter
      a.run(None, Args(repA))
      assert(!repA.testIgnoredReceived)
      assert(a.theTestThisCalled)
      assert(a.theTestThatCalled)

      // SlowAsMolasses is included, one test should be excluded
      val b = new FunSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        it("test this", mytags.SlowAsMolasses) { theTestThisCalled = true }
        it("test that") { theTestThatCalled = true }
      }
      val repB = new TestIgnoredTrackingReporter
      b.run(None, Args(repB, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repB.testIgnoredReceived)
      assert(b.theTestThisCalled)
      assert(!b.theTestThatCalled)

      // SlowAsMolasses is included, and both tests should be included
      val c = new FunSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        it("test this", mytags.SlowAsMolasses) { theTestThisCalled = true }
        it("test that", mytags.SlowAsMolasses) { theTestThatCalled = true }
      }
      val repC = new TestIgnoredTrackingReporter
      c.run(None, Args(repB, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repC.testIgnoredReceived)
      assert(c.theTestThisCalled)
      assert(c.theTestThatCalled)

      // SlowAsMolasses is included. both tests should be included but one ignored
      val d = new FunSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        ignore("test this", mytags.SlowAsMolasses) { theTestThisCalled = true }
        it("test that", mytags.SlowAsMolasses) { theTestThatCalled = true }
      }
      val repD = new TestIgnoredTrackingReporter
      d.run(None, Args(repD, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.Ignore")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(repD.testIgnoredReceived)
      assert(!d.theTestThisCalled)
      assert(d.theTestThatCalled)

      // SlowAsMolasses included, FastAsLight excluded
      val e = new FunSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        it("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true }
        it("test that", mytags.SlowAsMolasses) { theTestThatCalled = true }
        it("test the other") { theTestTheOtherCalled = true }
      }
      val repE = new TestIgnoredTrackingReporter
      e.run(None, Args(repE, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
        ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repE.testIgnoredReceived)
      assert(!e.theTestThisCalled)
      assert(e.theTestThatCalled)
      assert(!e.theTestTheOtherCalled)

      // An Ignored test that was both included and excluded should not generate a TestIgnored event
      val f = new FunSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        ignore("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true }
        it("test that", mytags.SlowAsMolasses) { theTestThatCalled = true }
        it("test the other") { theTestTheOtherCalled = true }
      }
      val repF = new TestIgnoredTrackingReporter
      f.run(None, Args(repF, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
        ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repF.testIgnoredReceived)
      assert(!f.theTestThisCalled)
      assert(f.theTestThatCalled)
      assert(!f.theTestTheOtherCalled)

      // An Ignored test that was not included should not generate a TestIgnored event
      val g = new FunSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        it("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true }
        it("test that", mytags.SlowAsMolasses) { theTestThatCalled = true }
        ignore("test the other") { theTestTheOtherCalled = true }
      }
      val repG = new TestIgnoredTrackingReporter
      g.run(None, Args(repG, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
        ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repG.testIgnoredReceived)
      assert(!g.theTestThisCalled)
      assert(g.theTestThatCalled)
      assert(!g.theTestTheOtherCalled)

      // No tagsToInclude set, FastAsLight excluded
      val h = new FunSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        it("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true }
        it("test that", mytags.SlowAsMolasses) { theTestThatCalled = true }
        it("test the other") { theTestTheOtherCalled = true }
      }
      val repH = new TestIgnoredTrackingReporter
      h.run(None, Args(repH, Stopper.default, Filter(None, Set("org.scalatest.FastAsLight")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repH.testIgnoredReceived)
      assert(!h.theTestThisCalled)
      assert(h.theTestThatCalled)
      assert(h.theTestTheOtherCalled)

      // No tagsToInclude set, mytags.SlowAsMolasses excluded
      val i = new FunSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        it("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true }
        it("test that", mytags.SlowAsMolasses) { theTestThatCalled = true }
        it("test the other") { theTestTheOtherCalled = true }
      }
      val repI = new TestIgnoredTrackingReporter
      i.run(None, Args(repI, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repI.testIgnoredReceived)
      assert(!i.theTestThisCalled)
      assert(!i.theTestThatCalled)
      assert(i.theTestTheOtherCalled)

      // No tagsToInclude set, mytags.SlowAsMolasses excluded, TestIgnored should not be received on excluded ones
      val j = new FunSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        ignore("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true }
        ignore("test that", mytags.SlowAsMolasses) { theTestThatCalled = true }
        it("test the other") { theTestTheOtherCalled = true }
      }
      val repJ = new TestIgnoredTrackingReporter
      j.run(None, Args(repJ, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repI.testIgnoredReceived)
      assert(!j.theTestThisCalled)
      assert(!j.theTestThatCalled)
      assert(j.theTestTheOtherCalled)

      // Same as previous, except Ignore specifically mentioned in excludes set
      val k = new FunSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        ignore("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true }
        ignore("test that", mytags.SlowAsMolasses) { theTestThatCalled = true }
        ignore("test the other") { theTestTheOtherCalled = true }
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
      val a = new FunSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        registerTest("test this", mytags.SlowAsMolasses) { theTestThisCalled = true }
        registerTest("test that") { theTestThatCalled = true }
      }

      import scala.language.reflectiveCalls

      val repA = new TestIgnoredTrackingReporter
      a.run(None, Args(repA))
      assert(!repA.testIgnoredReceived)
      assert(a.theTestThisCalled)
      assert(a.theTestThatCalled)

      // SlowAsMolasses is included, one test should be excluded
      val b = new FunSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        registerTest("test this", mytags.SlowAsMolasses) { theTestThisCalled = true }
        registerTest("test that") { theTestThatCalled = true }
      }
      val repB = new TestIgnoredTrackingReporter
      b.run(None, Args(repB, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repB.testIgnoredReceived)
      assert(b.theTestThisCalled)
      assert(!b.theTestThatCalled)

      // SlowAsMolasses is included, and both tests should be included
      val c = new FunSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        registerTest("test this", mytags.SlowAsMolasses) { theTestThisCalled = true }
        registerTest("test that", mytags.SlowAsMolasses) { theTestThatCalled = true }
      }
      val repC = new TestIgnoredTrackingReporter
      c.run(None, Args(repB, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repC.testIgnoredReceived)
      assert(c.theTestThisCalled)
      assert(c.theTestThatCalled)

      // SlowAsMolasses is included. both tests should be included but one ignored
      val d = new FunSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        registerIgnoredTest("test this", mytags.SlowAsMolasses) { theTestThisCalled = true }
        registerTest("test that", mytags.SlowAsMolasses) { theTestThatCalled = true }
      }
      val repD = new TestIgnoredTrackingReporter
      d.run(None, Args(repD, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.Ignore")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(repD.testIgnoredReceived)
      assert(!d.theTestThisCalled)
      assert(d.theTestThatCalled)

      // SlowAsMolasses included, FastAsLight excluded
      val e = new FunSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        registerTest("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true }
        registerTest("test that", mytags.SlowAsMolasses) { theTestThatCalled = true }
        registerTest("test the other") { theTestTheOtherCalled = true }
      }
      val repE = new TestIgnoredTrackingReporter
      e.run(None, Args(repE, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
        ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repE.testIgnoredReceived)
      assert(!e.theTestThisCalled)
      assert(e.theTestThatCalled)
      assert(!e.theTestTheOtherCalled)

      // An Ignored test that was both included and excluded should not generate a TestIgnored event
      val f = new FunSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        registerIgnoredTest("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true }
        registerTest("test that", mytags.SlowAsMolasses) { theTestThatCalled = true }
        registerTest("test the other") { theTestTheOtherCalled = true }
      }
      val repF = new TestIgnoredTrackingReporter
      f.run(None, Args(repF, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
        ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repF.testIgnoredReceived)
      assert(!f.theTestThisCalled)
      assert(f.theTestThatCalled)
      assert(!f.theTestTheOtherCalled)

      // An Ignored test that was not included should not generate a TestIgnored event
      val g = new FunSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        registerTest("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true }
        registerTest("test that", mytags.SlowAsMolasses) { theTestThatCalled = true }
        registerIgnoredTest("test the other") { theTestTheOtherCalled = true }
      }
      val repG = new TestIgnoredTrackingReporter
      g.run(None, Args(repG, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
        ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repG.testIgnoredReceived)
      assert(!g.theTestThisCalled)
      assert(g.theTestThatCalled)
      assert(!g.theTestTheOtherCalled)

      // No tagsToInclude set, FastAsLight excluded
      val h = new FunSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        registerTest("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true }
        registerTest("test that", mytags.SlowAsMolasses) { theTestThatCalled = true }
        registerTest("test the other") { theTestTheOtherCalled = true }
      }
      val repH = new TestIgnoredTrackingReporter
      h.run(None, Args(repH, Stopper.default, Filter(None, Set("org.scalatest.FastAsLight")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repH.testIgnoredReceived)
      assert(!h.theTestThisCalled)
      assert(h.theTestThatCalled)
      assert(h.theTestTheOtherCalled)

      // No tagsToInclude set, mytags.SlowAsMolasses excluded
      val i = new FunSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        registerTest("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true }
        registerTest("test that", mytags.SlowAsMolasses) { theTestThatCalled = true }
        registerTest("test the other") { theTestTheOtherCalled = true }
      }
      val repI = new TestIgnoredTrackingReporter
      i.run(None, Args(repI, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repI.testIgnoredReceived)
      assert(!i.theTestThisCalled)
      assert(!i.theTestThatCalled)
      assert(i.theTestTheOtherCalled)

      // No tagsToInclude set, mytags.SlowAsMolasses excluded, TestIgnored should not be received on excluded ones
      val j = new FunSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        registerIgnoredTest("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true }
        registerIgnoredTest("test that", mytags.SlowAsMolasses) { theTestThatCalled = true }
        registerTest("test the other") { theTestTheOtherCalled = true }
      }
      val repJ = new TestIgnoredTrackingReporter
      j.run(None, Args(repJ, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repI.testIgnoredReceived)
      assert(!j.theTestThisCalled)
      assert(!j.theTestThatCalled)
      assert(j.theTestTheOtherCalled)

      // Same as previous, except Ignore specifically mentioned in excludes set
      val k = new FunSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        registerIgnoredTest("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true }
        registerIgnoredTest("test that", mytags.SlowAsMolasses) { theTestThatCalled = true }
        registerIgnoredTest("test the other") { theTestTheOtherCalled = true }
      }
      val repK = new TestIgnoredTrackingReporter
      k.run(None, Args(repK, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses", "org.scalatest.Ignore")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(repK.testIgnoredReceived)
      assert(!k.theTestThisCalled)
      assert(!k.theTestThatCalled)
      assert(!k.theTestTheOtherCalled)
    }

    it("should return the correct test count from its expectedTestCount method") {

      val a = new FunSpec {
        it("test this") {}
        it("test that") {}
      }
      assert(a.expectedTestCount(Filter()) === 2)

      val b = new FunSpec {
        ignore("test this") {}
        it("test that") {}
      }
      assert(b.expectedTestCount(Filter()) === 1)

      val c = new FunSpec {
        it("test this", mytags.FastAsLight) {}
        it("test that") {}
      }
      assert(c.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 1)
      assert(c.expectedTestCount(Filter(None, Set("org.scalatest.FastAsLight"))) === 1)

      val d = new FunSpec {
        it("test this", mytags.FastAsLight, mytags.SlowAsMolasses) {}
        it("test that", mytags.SlowAsMolasses) {}
        it("test the other thing") {}
      }
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 1)
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight"))) === 1)
      assert(d.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) === 1)
      assert(d.expectedTestCount(Filter()) === 3)

      val e = new FunSpec {
        it("test this", mytags.FastAsLight, mytags.SlowAsMolasses) {}
        it("test that", mytags.SlowAsMolasses) {}
        ignore("test the other thing") {}
      }
      assert(e.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 1)
      assert(e.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight"))) === 1)
      assert(e.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) === 0)
      assert(e.expectedTestCount(Filter()) === 2)

      val f = new Suites(a, b, c, d, e)
      assert(f.expectedTestCount(Filter()) === 10)
    }

    it("should return the correct test count from its expectedTestCount method when register tests with registerTest and registerIgnoredTest") {

      val a = new FunSpec {
        registerTest("test this") {}
        registerTest("test that") {}
      }
      assert(a.expectedTestCount(Filter()) === 2)

      val b = new FunSpec {
        registerIgnoredTest("test this") {}
        registerTest("test that") {}
      }
      assert(b.expectedTestCount(Filter()) === 1)

      val c = new FunSpec {
        registerTest("test this", mytags.FastAsLight) {}
        registerTest("test that") {}
      }
      assert(c.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 1)
      assert(c.expectedTestCount(Filter(None, Set("org.scalatest.FastAsLight"))) === 1)

      val d = new FunSpec {
        registerTest("test this", mytags.FastAsLight, mytags.SlowAsMolasses) {}
        registerTest("test that", mytags.SlowAsMolasses) {}
        registerTest("test the other thing") {}
      }
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 1)
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight"))) === 1)
      assert(d.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) === 1)
      assert(d.expectedTestCount(Filter()) === 3)

      val e = new FunSpec {
        registerTest("test this", mytags.FastAsLight, mytags.SlowAsMolasses) {}
        registerTest("test that", mytags.SlowAsMolasses) {}
        registerIgnoredTest("test the other thing") {}
      }
      assert(e.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 1)
      assert(e.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight"))) === 1)
      assert(e.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) === 0)
      assert(e.expectedTestCount(Filter()) === 2)

      val f = new Suites(a, b, c, d, e)
      assert(f.expectedTestCount(Filter()) === 10)
    }
    
    it("should generate a TestPending message when the test body is (pending)") {

      val a = new FunSpec {

        it("should do this") (pending)

        it("should do that") {
          assert(2 + 2 === 4)
        }
        
        it("should do something else") {
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
      val a = new FunSpec {
        it("throws AssertionError") { throw new AssertionError }
        it("throws plain old Error") { throw new Error }
        it("throws Throwable") { throw new Throwable }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      val tf = rep.testFailedEventsReceived
      assert(tf.size === 3)
    }
    // SKIP-SCALATESTJS-START
    it("should propagate out Errors that are direct subtypes of Error in JDK 1.5, other than " +
            "AssertionError, causing Suites and Runs to abort.") {
      val a = new FunSpec {
        it("throws AssertionError") { throw new OutOfMemoryError }
      }
      intercept[OutOfMemoryError] {
        a.run(None, Args(SilentReporter))
      }
    }
    // SKIP-SCALATESTJS-END
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
      val a = new FunSpec with GivenWhenThen {
        it("should do something else") {
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
  }
  
  describe("when failure happens") {
    
    it("should fire TestFailed event with correct stack depth info when test failed") {
      class TestSpec extends FunSpec {
        it("fail scenario") {
          assert(1 === 2)
        }
        describe("a feature") {
          it("nested fail scenario") {
            assert(1 === 2)
          }
        }
      }
      val rep = new EventRecordingReporter
      val s1 = new TestSpec
      s1.run(None, Args(rep))
      assert(rep.testFailedEventsReceived.size === 2)
      assert(rep.testFailedEventsReceived(0).throwable.get.asInstanceOf[TestFailedException].failedCodeFileName.get === "FunSpecSpec.scala")
      assert(rep.testFailedEventsReceived(0).throwable.get.asInstanceOf[TestFailedException].failedCodeLineNumber.get === thisLineNumber - 13)
      assert(rep.testFailedEventsReceived(1).throwable.get.asInstanceOf[TestFailedException].failedCodeFileName.get === "FunSpecSpec.scala")
      assert(rep.testFailedEventsReceived(1).throwable.get.asInstanceOf[TestFailedException].failedCodeLineNumber.get === thisLineNumber - 11)
    }
    
    it("should generate TestRegistrationClosedException with correct stack depth info when has a it nested inside a it") {
      class TestSpec extends FunSpec {
        var registrationClosedThrown = false
        describe("a feature") {
          it("a scenario") {
            it("nested scenario") {
              assert(1 === 2)
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
      assert(testFailedEvents.size == 1)
      assert(testFailedEvents(0).throwable.get.getClass() === classOf[TestRegistrationClosedException])
      val trce = testFailedEvents(0).throwable.get.asInstanceOf[TestRegistrationClosedException]
      assert("FunSpecSpec.scala" == trce.failedCodeFileName.get)
      assert(trce.failedCodeLineNumber.get == thisLineNumber - 24)
      assert(trce.message == Some("An it clause may not appear inside another it or they clause."))
    }

    it("should generate TestRegistrationClosedException with correct stack depth info when has a ignore nested inside a it") {
      class TestSpec extends FunSpec {
        var registrationClosedThrown = false
        describe("a feature") {
          it("a scenario") {
            ignore("nested scenario") {
              assert(1 === 2)
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
      assert(testFailedEvents.size == 1)
      assert(testFailedEvents(0).throwable.get.getClass() === classOf[TestRegistrationClosedException])
      val trce = testFailedEvents(0).throwable.get.asInstanceOf[TestRegistrationClosedException]
      assert("FunSpecSpec.scala" == trce.failedCodeFileName.get)
      assert(trce.failedCodeLineNumber.get == thisLineNumber - 24)
      assert(trce.message == Some("An ignore clause may not appear inside an it or a they clause."))
    }

    it("should generate TestRegistrationClosedException with correct stack depth info when has a they nested inside a they") {
      class TestSpec extends FunSpec {
        var registrationClosedThrown = false
        describe("a feature") {
          they("a scenario") {
            they("nested scenario") {
              assert(1 === 2)
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
      assert("FunSpecSpec.scala" === trce.failedCodeFileName.get)
      assert(trce.failedCodeLineNumber.get === thisLineNumber - 24)
      assert(trce.message == Some("A they clause may not appear inside another it or they clause."))
    }

    it("should generate TestRegistrationClosedException with correct stack depth info when has a ignore nested inside a they") {
      class TestSpec extends FunSpec {
        var registrationClosedThrown = false
        describe("a feature") {
          they("a scenario") {
            ignore("nested scenario") {
              assert(1 === 2)
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
      assert("FunSpecSpec.scala" === trce.failedCodeFileName.get)
      assert(trce.failedCodeLineNumber.get === thisLineNumber - 24)
      assert(trce.message == Some("An ignore clause may not appear inside an it or a they clause."))
    }

    it("should allow test registration with registerTest and registerIgnoredTest") {
      class TestSpec extends FunSpec {
        val a = 1
        registerTest("test 1") {
          val e = intercept[TestFailedException] {
            assert(a == 2)
          }
          assert(e.message == Some("1 did not equal 2"))
          assert(e.failedCodeFileName == Some("FunSpecSpec.scala"))
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

    it("should generate TestRegistrationClosedException with correct stack depth info when has a registerTest nested inside a registerTest") {
      class TestSpec extends FunSpec {
        var registrationClosedThrown = false
        describe("a feature") {
          registerTest("a scenario") {
            registerTest("nested scenario") {
              assert(1 === 2)
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
      assert(testFailedEvents.size == 1)
      assert(testFailedEvents(0).throwable.get.getClass() === classOf[TestRegistrationClosedException])
      val trce = testFailedEvents(0).throwable.get.asInstanceOf[TestRegistrationClosedException]
      assert("FunSpecSpec.scala" == trce.failedCodeFileName.get)
      assert(trce.failedCodeLineNumber.get == thisLineNumber - 24)
      assert(trce.message == Some("Test cannot be nested inside another test."))
    }

    it("should generate TestRegistrationClosedException with correct stack depth info when has a registerIgnoredTest nested inside a registerTest") {
      class TestSpec extends FunSpec {
        var registrationClosedThrown = false
        describe("a feature") {
          registerTest("a scenario") {
            registerIgnoredTest("nested scenario") {
              assert(1 === 2)
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
      assert(testFailedEvents.size == 1)
      assert(testFailedEvents(0).throwable.get.getClass() === classOf[TestRegistrationClosedException])
      val trce = testFailedEvents(0).throwable.get.asInstanceOf[TestRegistrationClosedException]
      assert("FunSpecSpec.scala" == trce.failedCodeFileName.get)
      assert(trce.failedCodeLineNumber.get == thisLineNumber - 24)
      assert(trce.message == Some("Test cannot be nested inside another test."))
    }

    it("should generate NotAllowedException wrapping a TestFailedException when assert fails in scope") {
      class TestSpec extends FunSpec {
        describe("a feature") {
          val a = 1
          assert(a == 2)
        }
      }
      val e = intercept[NotAllowedException] {
        new TestSpec
      }
      assert("FunSpecSpec.scala" == e.failedCodeFileName.get)
      assert(e.failedCodeLineNumber.get == thisLineNumber - 3)
      assert(e.message == Some(FailureMessages.assertionShouldBePutInsideItOrTheyClauseNotDescribeClause))

      assert(e.cause.isDefined)
      val causeThrowable = e.cause.get
      assert(causeThrowable.isInstanceOf[TestFailedException])
      val cause = causeThrowable.asInstanceOf[TestFailedException]
      assert("FunSpecSpec.scala" == cause.failedCodeFileName.get)
      assert(cause.failedCodeLineNumber.get == thisLineNumber - 15)
      assert(cause.message == Some(FailureMessages.didNotEqual(1, 2)))
    }

    it("should generate NotAllowedException wrapping a TestCanceledException when assume fails in scope") {
      class TestSpec extends FunSpec {
        describe("a feature") {
          val a = 1
          assume(a == 2)
        }
      }
      val e = intercept[NotAllowedException] {
        new TestSpec
      }
      assert("FunSpecSpec.scala" == e.failedCodeFileName.get)
      assert(e.failedCodeLineNumber.get == thisLineNumber - 3)
      assert(e.message == Some(FailureMessages.assertionShouldBePutInsideItOrTheyClauseNotDescribeClause))

      assert(e.cause.isDefined)
      val causeThrowable = e.cause.get
      assert(causeThrowable.isInstanceOf[TestCanceledException])
      val cause = causeThrowable.asInstanceOf[TestCanceledException]
      assert("FunSpecSpec.scala" == cause.failedCodeFileName.get)
      assert(cause.failedCodeLineNumber.get == thisLineNumber - 15)
      assert(cause.message == Some(FailureMessages.didNotEqual(1, 2)))
    }

    it("should generate NotAllowedException wrapping a non-fatal RuntimeException is thrown inside scope") {
      class TestSpec extends FunSpec {
        describe("a feature") {
          throw new RuntimeException("on purpose")
        }
      }
      val e = intercept[NotAllowedException] {
        new TestSpec
      }
      assert("FunSpecSpec.scala" == e.failedCodeFileName.get)
      assert(e.failedCodeLineNumber.get == thisLineNumber - 3)
      assert(e.cause.isDefined)
      val causeThrowable = e.cause.get
      assert(e.message == Some(FailureMessages.exceptionWasThrownInDescribeClause(UnquotedString(causeThrowable.getClass.getName), "a feature")))

      assert(causeThrowable.isInstanceOf[RuntimeException])
      val cause = causeThrowable.asInstanceOf[RuntimeException]
      assert(cause.getMessage == "on purpose")
    }

    // SKIP-SCALATESTJS-START
    it("should propagate AnnotationFormatError when it is thrown inside scope") {
      class TestSpec extends FunSpec {
        describe("a feature") {
          throw new AnnotationFormatError("on purpose")
        }
      }
      val e = intercept[AnnotationFormatError] {
        new TestSpec
      }
      assert(e.getMessage == "on purpose")
    }

    it("should propagate AWTError when it is thrown inside scope") {
      class TestSpec extends FunSpec {
        describe("a feature") {
          throw new AWTError("on purpose")
        }
      }
      val e = intercept[AWTError] {
        new TestSpec
      }
      assert(e.getMessage == "on purpose")
    }

    it("should propagate CoderMalfunctionError when it is thrown inside scope") {
      class TestSpec extends FunSpec {
        describe("a feature") {
          throw new CoderMalfunctionError(new RuntimeException("on purpose"))
        }
      }
      val e = intercept[CoderMalfunctionError] {
        new TestSpec
      }
      assert(e.getMessage == "java.lang.RuntimeException: on purpose")
    }

    it("should propagate FactoryConfigurationError when it is thrown inside scope") {
      class TestSpec extends FunSpec {
        describe("a feature") {
          throw new FactoryConfigurationError("on purpose")
        }
      }
      val e = intercept[FactoryConfigurationError] {
        new TestSpec
      }
      assert(e.getMessage == "on purpose")
    }

    it("should propagate LinkageError when it is thrown inside scope") {
      class TestSpec extends FunSpec {
        describe("a feature") {
          throw new LinkageError("on purpose")
        }
      }
      val e = intercept[LinkageError] {
        new TestSpec
      }
      assert(e.getMessage == "on purpose")
    }

    it("should propagate ThreadDeath when it is thrown inside scope") {
      class TestSpec extends FunSpec {
        describe("a feature") {
          throw new ThreadDeath
        }
      }
      val e = intercept[ThreadDeath] {
        new TestSpec
      }
      assert(e.getMessage == null)
    }

    it("should propagate TransformerFactoryConfigurationError when it is thrown inside scope") {
      class TestSpec extends FunSpec {
        describe("a feature") {
          throw new TransformerFactoryConfigurationError("on purpose")
        }
      }
      val e = intercept[TransformerFactoryConfigurationError] {
        new TestSpec
      }
      assert(e.getMessage == "on purpose")
    }

    it("should propagate VirtualMachineError when it is thrown inside scope") {
      class TestSpec extends FunSpec {
        describe("a feature") {
          throw new VirtualMachineError("on purpose") {}
        }
      }
      val e = intercept[VirtualMachineError] {
        new TestSpec
      }
      assert(e.getMessage == "on purpose")
    }
    // SKIP-SCALATESTJS-END
  }
}

