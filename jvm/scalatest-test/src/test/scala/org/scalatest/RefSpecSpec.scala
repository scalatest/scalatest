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
package org.scalatest

import SharedHelpers._
import org.scalatest.events._
import org.scalactic.Prettifier
import org.scalatest.refspec.RefSpec
import collection.immutable.TreeSet
import exceptions.TestFailedException
import java.awt.AWTError
import java.lang.annotation.AnnotationFormatError
import java.nio.charset.CoderMalfunctionError
import javax.xml.parsers.FactoryConfigurationError
import javax.xml.transform.TransformerFactoryConfigurationError
import org.scalactic.exceptions.NullArgumentException
import org.scalatest.exceptions.NotAllowedException
import org.scalatest.exceptions.TestCanceledException
import scala.reflect.NameTransformer.encode
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class RefSpecSpec extends AnyFunSpec with PrivateMethodTester {

  private val prettifier = Prettifier.default

  describe("A RefSpec") {
    /*
    it("should send InfoProvided events with aboutAPendingTest set to true and aboutACanceledTest set to false for info " +
            "calls made from a test that is pending") {
      val a = new RefSpec {
        def `test: something`(r: Rep) {
          r.info("two integers")
          r.info("one is subracted from the other")
          r.info("the result is the difference between the two numbers")
          pending
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep, new Stopper {}, Filter(), ConfigMap.empty, None, new Tracker()))
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
      val a = new RefSpec {
        def `test: something`(r: Rep) {
          r.info("two integers")
          r.info("one is subracted from the other")
          r.info("the result is the difference between the two numbers")
          assert(1 + 1 === 2)
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep, new Stopper {}, Filter(), ConfigMap.empty, None, new Tracker()))
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
      val a = new RefSpec {
        def `test: something`(r: Rep) {
          r.info("two integers")
          r.info("one is subracted from the other")
          r.info("the result is the difference between the two numbers")
          cancel()
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep, new Stopper {}, Filter(), ConfigMap.empty, None, new Tracker()))
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
*/

    it("should return the test names in alphabetical order from testNames") {
      val a = new RefSpec {
        def `it should do this`(): Unit = {}
        def `it should do that`(): Unit = {}
      }

      assertResult(List("it should do that", "it should do this")) {
        a.testNames.iterator.toList
      }

      val b = new RefSpec {}

      assertResult(List[String]()) {
        b.testNames.iterator.toList
      }

      val c = new RefSpec {
        def `test: that`(): Unit = {}
        def `test: this`(): Unit = {}
      }

      assertResult(List("test: that", "test: this")) {
        c.testNames.iterator.toList
      }
    }

    it("should return test names nested in scope in alpahbetical order from testNames") {
      val a = new RefSpec {
        object `A Tester` {
          def `should test that`: Unit = {}
          def `should test this`: Unit = {}
        }
      }

      assertResult(List("A Tester should test that", "A Tester should test this")) {
        a.testNames.iterator.toList
      }

      val b = new RefSpec {
        object `A Tester` {
          object `should be able to` {
            def `test this`: Unit = {}
            def `test that`: Unit = {}
          }
          object `must be able to` {
            def `test this`: Unit = {}
            def `test that`: Unit = {}
          }
        }
      }

      assertResult(List("A Tester must be able to test that", "A Tester must be able to test this", "A Tester should be able to test that", "A Tester should be able to test this")) {
        b.testNames.iterator.toList
      }
    }

    it("test names should properly nest scopes in test names") {
      class MySpec extends RefSpec with Matchers {
        object `A Stack` {
          object `(when not empty)` {
            def `must allow me to pop`: Unit = {}
          }
          object `(when not full)` {
            def `must allow me to push`: Unit = {}
          }
        }
      }
      val a = new MySpec
      assert(a.testNames.size === 2)
      assert(a.testNames.iterator.toList(0) === "A Stack (when not empty) must allow me to pop")
      assert(a.testNames.iterator.toList(1) === "A Stack (when not full) must allow me to push")
    }

    it("should be able to mix in BeforeAndAfterEach with BeforeAndAfterAll without any problems") {
      class MySpec extends RefSpec with Matchers with BeforeAndAfterEach with BeforeAndAfterAll {
        object `A Stack` {
          object `(when not empty)` {
            def `should allow me to pop`: Unit = {}
          }
          object `(when not full)` {
            def `should allow me to push`: Unit = {}
          }
        }
      }
      val a = new MySpec
      a.execute()
    }

    it("should register scopes and tests lazily after spec instance variables are created when testNames is invoked") {
      val a = new RefSpec {
        val name = "ScalaTest"
        object `In Scope: ` {
          assert(name === "ScalaTest")
        }
      }
      a.testNames // Should execute assertion in the scope
    }
    it("should register scopes and tests lazily after spec instance variables are created when run is invoked") {
      val a = new RefSpec {
        val name = "ScalaTest"
        object `In Scope: ` {
          assert(name === "ScalaTest")
        }
      }
      a.run(None, Args(SilentReporter)) // Should execute assertion in the scope
    }
    it("should register scopes and tests lazily after spec instance variables are created when expectedTestCount is invoked") {
      val a = new RefSpec {
        val name = "ScalaTest"
        object `In Scope: ` {
          assert(name === "ScalaTest")
        }
      }
      a.expectedTestCount(Filter.default) // Should execute assertion in the scope
    }
    it("should register scopes and tests lazily after spec instance variables are created when tags is invoked") {
      val a = new RefSpec {
        val name = "ScalaTest"
        object `In Scope: ` {
          assert(name === "ScalaTest")
        }
      }
      a.tags // Should execute assertion in the scope
    }

    /*
        it("should register scopes and tests lazily after spec instance variables are created") {
          val a = new RefSpec {
            val name = "ScalaTest"
            object `In Scope: ` {
              info(name)
            }
          }
          val rep = new EventRecordingReporter
          a.run(None, Args(reporter = rep))
          val infoEvents = rep.infoProvidedEventsReceived
          assert(infoEvents.length === 1)
          val info = infoEvents(0)
          assert(info.message === "ScalaTest")
        }
    */

    class TestWasCalledSpec extends RefSpec {
      var theTestThisCalled = false
      var theTestThatCalled = false
      def `test: this`(): Unit = { theTestThisCalled = true }
      def `test: that`(): Unit = { theTestThatCalled = true }
    }

    it("should execute all tests when run is called with testName None") {

      val b = new TestWasCalledSpec
      b.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker))
      assert(b.theTestThisCalled)
      assert(b.theTestThatCalled)
    }

    it("should execute one test when run is called with a defined testName") {

      val a = new TestWasCalledSpec
      a.run(Some("test: this"), Args(SilentReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker))
      assert(a.theTestThisCalled)
      assert(!a.theTestThatCalled)
    }

    it("should report as ignored, and not run, tests marked ignored") {

      class SpecA extends RefSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        def `test: this`(): Unit = { theTestThisCalled = true }
        def `test: that`: Unit = { theTestThatCalled = true }
      }
      val a = new SpecA()

      import scala.language.reflectiveCalls

      val repA = new TestIgnoredTrackingReporter
      a.run(None, Args(repA, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker))
      assert(!repA.testIgnoredReceived)
      assert(a.theTestThisCalled)
      assert(a.theTestThatCalled)

      class SpecB extends RefSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        @Ignore
        def `test: this`(): Unit = { theTestThisCalled = true }
        def `test: that`: Unit = { theTestThatCalled = true }
      }
      val b = new SpecB

      val repB = new TestIgnoredTrackingReporter
      b.run(None, Args(repB, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker))
      assert(repB.testIgnoredReceived)
      assert(repB.lastEvent.isDefined)
      assert(repB.lastEvent.get.testName endsWith "test: this")
      assert(!b.theTestThisCalled)
      assert(b.theTestThatCalled)

      class SpecC extends RefSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        def `test: this`(): Unit = { theTestThisCalled = true }
        @Ignore
        def `test: that`: Unit = { theTestThatCalled = true }
      }
      val c = new SpecC

      val repC = new TestIgnoredTrackingReporter
      c.run(None, Args(repC, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker))
      assert(repC.testIgnoredReceived)
      assert(repC.lastEvent.isDefined)
      assert(repC.lastEvent.get.testName endsWith "test: that", repC.lastEvent.get.testName)
      assert(c.theTestThisCalled)
      assert(!c.theTestThatCalled)

      class SpecD extends RefSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        @Ignore
        def `test: this`(): Unit = { theTestThisCalled = true }
        @Ignore
        def `test: that`: Unit = { theTestThatCalled = true }
      }
      val d = new SpecD

      val repD = new TestIgnoredTrackingReporter
      d.run(None, Args(repD, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker))
      assert(repD.testIgnoredReceived)
      assert(repD.lastEvent.isDefined)
      assert(repD.lastEvent.get.testName === "test: this") // last because run alphabetically
      assert(!d.theTestThisCalled)
      assert(!d.theTestThatCalled)
    }

    it("should ignore a test marked as ignored if run is invoked with that testName") {

      class SpecE extends RefSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        @Ignore
        def `test: this`(): Unit = { theTestThisCalled = true }
        def `test: that`: Unit = { theTestThatCalled = true }
      }
      val e = new SpecE

      import scala.language.reflectiveCalls

      val repE = new TestIgnoredTrackingReporter
      e.run(Some("test: this"), Args(repE, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker))
      assert(repE.testIgnoredReceived)
      assert(!e.theTestThisCalled)
      assert(!e.theTestThatCalled)
    }

    it("should exclude a test with a tag included in the tagsToExclude set even if run is invoked with that testName") {

      class SpecE extends RefSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        @SlowAsMolasses
        def `test: this`(): Unit = { theTestThisCalled = true }
        def `test: that`: Unit = { theTestThatCalled = true }
      }
      val e = new SpecE

      import scala.language.reflectiveCalls

      val repE = new TestIgnoredTrackingReporter
      e.run(Some("test: this"), Args(repE, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses")), ConfigMap.empty, None, new Tracker))
      assert(!repE.testIgnoredReceived)
      assert(!e.theTestThisCalled)
      assert(!e.theTestThatCalled)
    }

    it("should run only those tests selected by the tags to include and exclude sets") {

      // Nothing is excluded
      class SpecA extends RefSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        @SlowAsMolasses
        def `test this`: Unit = { theTestThisCalled = true }
        def `test that`: Unit = { theTestThatCalled = true }
      }
      val a = new SpecA

      import scala.language.reflectiveCalls

      val repA = new TestIgnoredTrackingReporter
      a.run(None, Args(repA, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker))
      assert(!repA.testIgnoredReceived)
      assert(a.theTestThisCalled)
      assert(a.theTestThatCalled)

      // SlowAsMolasses is included, one test should be excluded
      class SpecB extends RefSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        @SlowAsMolasses
        def `test this`: Unit = { theTestThisCalled = true }
        def `test that`: Unit = { theTestThatCalled = true }
      }
      val b = new SpecB
      val repB = new TestIgnoredTrackingReporter
      b.run(None, Args(repB, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), ConfigMap.empty, None, new Tracker))
      assert(!repB.testIgnoredReceived)
      assert(b.theTestThisCalled)
      assert(!b.theTestThatCalled)

      // SlowAsMolasses is included, and both tests should be included
      class SpecC extends RefSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        @SlowAsMolasses
        def `test this`: Unit = { theTestThisCalled = true }
        @SlowAsMolasses
        def `test that`: Unit = { theTestThatCalled = true }
      }
      val c = new SpecC
      val repC = new TestIgnoredTrackingReporter
      c.run(None, Args(repB, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), ConfigMap.empty, None, new Tracker))
      assert(!repC.testIgnoredReceived)
      assert(c.theTestThisCalled)
      assert(c.theTestThatCalled)

      // SlowAsMolasses is included. both tests should be included but one ignored
      class SpecD extends RefSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        @Ignore
        @SlowAsMolasses
        def `test this`: Unit = { theTestThisCalled = true }
        @SlowAsMolasses
        def `test that`: Unit = { theTestThatCalled = true }
      }
      val d = new SpecD
      val repD = new TestIgnoredTrackingReporter
      d.run(None, Args(repD, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.Ignore")), ConfigMap.empty, None, new Tracker))
      assert(repD.testIgnoredReceived)
      assert(!d.theTestThisCalled)
      assert(d.theTestThatCalled)

      // SlowAsMolasses included, FastAsLight excluded
      class SpecE extends RefSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        @SlowAsMolasses
        @FastAsLight
        def `test this`: Unit = { theTestThisCalled = true }
        @SlowAsMolasses
        def `test that`: Unit = { theTestThatCalled = true }
        def `test the other`: Unit = { theTestTheOtherCalled = true }
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
      class SpecF extends RefSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        @Ignore
        @SlowAsMolasses
        @FastAsLight
        def `test this`: Unit = { theTestThisCalled = true }
        @SlowAsMolasses
        def `test that`: Unit = { theTestThatCalled = true }
        def `test the other`: Unit = { theTestTheOtherCalled = true }
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
      class SpecG extends RefSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        @SlowAsMolasses
        @FastAsLight
        def `test this`: Unit = { theTestThisCalled = true }
        @SlowAsMolasses
        def `test that`: Unit = { theTestThatCalled = true }
        @Ignore
        def `test the other`: Unit = { theTestTheOtherCalled = true }
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
      class SpecH extends RefSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        @SlowAsMolasses
        @FastAsLight
        def `test this`: Unit = { theTestThisCalled = true }
        @SlowAsMolasses
        def `test that`: Unit = { theTestThatCalled = true }
        def `test the other`: Unit = { theTestTheOtherCalled = true }
      }
      val h = new SpecH
      val repH = new TestIgnoredTrackingReporter
      h.run(None, Args(repH, Stopper.default, Filter(None, Set("org.scalatest.FastAsLight")), ConfigMap.empty, None, new Tracker))
      assert(!repH.testIgnoredReceived)
      assert(!h.theTestThisCalled)
      assert(h.theTestThatCalled)
      assert(h.theTestTheOtherCalled)

      // No tagsToInclude set, SlowAsMolasses excluded
      class SpecI extends RefSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        @SlowAsMolasses
        @FastAsLight
        def `test this`: Unit = { theTestThisCalled = true }
        @SlowAsMolasses
        def `test that`: Unit = { theTestThatCalled = true }
        def `test the other`: Unit = { theTestTheOtherCalled = true }
      }
      val i = new SpecI
      val repI = new TestIgnoredTrackingReporter
      i.run(None, Args(repI, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses")), ConfigMap.empty, None, new Tracker))
      assert(!repI.testIgnoredReceived)
      assert(!i.theTestThisCalled)
      assert(!i.theTestThatCalled)
      assert(i.theTestTheOtherCalled)

      // No tagsToInclude set, SlowAsMolasses excluded, TestIgnored should not be received on excluded ones
      class SpecJ extends RefSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        @Ignore
        @SlowAsMolasses
        @FastAsLight
        def `test this`: Unit = { theTestThisCalled = true }
        @Ignore
        @SlowAsMolasses
        def `test that`: Unit = { theTestThatCalled = true }
        def `test the other`: Unit = { theTestTheOtherCalled = true }
      }
      val j = new SpecJ
      val repJ = new TestIgnoredTrackingReporter
      j.run(None, Args(repJ, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses")), ConfigMap.empty, None, new Tracker))
      assert(!repI.testIgnoredReceived)
      assert(!j.theTestThisCalled)
      assert(!j.theTestThatCalled)
      assert(j.theTestTheOtherCalled)

      // Same as previous, except Ignore specifically mentioned in excludes set
      class SpecK extends RefSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        @Ignore
        @SlowAsMolasses
        @FastAsLight
        def `test this`: Unit = { theTestThisCalled = true }
        @Ignore
        @SlowAsMolasses
        def `test that`: Unit = { theTestThatCalled = true }
        @Ignore
        def `test the other`: Unit = { theTestTheOtherCalled = true }
      }
      val k = new SpecK
      val repK = new TestIgnoredTrackingReporter
      k.run(None, Args(repK, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses", "org.scalatest.Ignore")), ConfigMap.empty, None, new Tracker))
      assert(repK.testIgnoredReceived)
      assert(!k.theTestThisCalled)
      assert(!k.theTestThatCalled)
      assert(!k.theTestTheOtherCalled)
    }

    it("should return a correct tags map from the tags method") {

      val a = new RefSpec {
        object `This RefSpec should` {
          @Ignore
          def `test this`: Unit = {}
          def `test that`: Unit = { pending }
        }
      }
      assertResult(Map("This RefSpec should test this" -> Set("org.scalatest.Ignore"))) {
        a.tags
      }

      val b = new RefSpec {
        object `This RefSpec should` {
          def `test this`: Unit = { pending }
          @Ignore
          def `test that`: Unit = {}
        }
      }
      assertResult(Map("This RefSpec should test that" -> Set("org.scalatest.Ignore"))) {
        b.tags
      }

      val c = new RefSpec {
        object `This RefSpec should` {
          @Ignore
          def `test this`: Unit = {}
          @Ignore
          def `test that`: Unit = {}
        }
      }
      assertResult(Map("This RefSpec should test this" -> Set("org.scalatest.Ignore"), "This RefSpec should test that" -> Set("org.scalatest.Ignore"))) {
        c.tags
      }

      val d = new RefSpec {
        object `This RefSpec should` {
          @SlowAsMolasses
          def `test this`: Unit = { pending }
          @SlowAsMolasses
          @Ignore
          def `test that`: Unit = {}
        }
      }
      assertResult(Map("This RefSpec should test this" -> Set("org.scalatest.SlowAsMolasses"), "This RefSpec should test that" -> Set("org.scalatest.Ignore", "org.scalatest.SlowAsMolasses"))) {
        d.tags
      }

      val e = new RefSpec {
        object `This RefSpec should` {
          def `test this`: Unit = { pending }
          def `test that`: Unit = { pending }
        }
      }
      assertResult(Map()) {
        e.tags
      }

      val f = new RefSpec {
        object `This RefSpec should` {
          @SlowAsMolasses
          @WeakAsAKitten
          def `test this`: Unit = { pending }
          @SlowAsMolasses
          def `test that`: Unit = {}
        }
      }
      assertResult(Map("This RefSpec should test this" -> Set("org.scalatest.SlowAsMolasses", "org.scalatest.WeakAsAKitten"), "This RefSpec should test that" -> Set("org.scalatest.SlowAsMolasses"))) {
        f.tags
      }

      val g = new RefSpec {
        object `This RefSpec should` {
          @SlowAsMolasses
          @WeakAsAKitten
          def `test this`: Unit = { pending }
          @SlowAsMolasses
          def `test that`: Unit = {}
        }
      }
      assertResult(Map("This RefSpec should test this" -> Set("org.scalatest.SlowAsMolasses", "org.scalatest.WeakAsAKitten"), "This RefSpec should test that" -> Set("org.scalatest.SlowAsMolasses"))) {
        g.tags
      }
    }

    it("should throw IllegalArgumentException if run is passed a testName that does not exist") {
      val spec = new RefSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        def `test: this`(): Unit = { theTestThisCalled = true }
        def `test: that`: Unit = { theTestThatCalled = true }
      }

      intercept[IllegalArgumentException] {
        // Here, they forgot that the name is actually `test: this`(Fixture)
        spec.run(Some(encode("test: misspelled")), Args(SilentReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker))
      }
    }

    it("should return the correct test count from its expectedTestCount method") {

      val a = new RefSpec {
        def `test: this`(): Unit = ()
        def `test: that`: Unit = ()
      }
      assert(a.expectedTestCount(Filter()) === 2)

      val b = new RefSpec {
        @Ignore
        def `test: this`(): Unit = ()
        def `test: that`: Unit = ()
      }
      assert(b.expectedTestCount(Filter()) === 1)

      val c = new RefSpec {
        @FastAsLight
        def `test: this`(): Unit = ()
        def `test: that`: Unit = ()
      }
      assert(c.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 1)
      assert(c.expectedTestCount(Filter(None, Set("org.scalatest.FastAsLight"))) === 1)

      val d = new RefSpec {
        @FastAsLight
        @SlowAsMolasses
        def `test: this`(): Unit = ()
        @SlowAsMolasses
        def `test: that`: Unit = ()
        def `test: the other thing`: Unit = ()
      }
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 1)
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight"))) === 1)
      assert(d.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) === 1)
      assert(d.expectedTestCount(Filter()) === 3)

      val e = new RefSpec {
        @FastAsLight
        @SlowAsMolasses
        def `test: this`(): Unit = ()
        @SlowAsMolasses
        def `test: that`: Unit = ()
        @Ignore
        def `test: the other thing`: Unit = ()
      }
      assert(e.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 1)
      assert(e.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight"))) === 1)
      assert(e.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) === 0)
      assert(e.expectedTestCount(Filter()) === 2)

      val f = new Suites(a, b, c, d, e)
      assert(f.expectedTestCount(Filter()) === 10)
    }

    it("should send an InfoProvided event for an info") {
      class MySuite extends RefSpec  {
        info(
          "hi there"
        )
      }
      val suite = new MySuite
      val reporter = new EventRecordingReporter
      suite.run(None, Args(reporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker))

      val infoList = reporter.infoProvidedEventsReceived

      assert(infoList.size === 1)
      assert(infoList(0).message === "hi there")
    }

    it("should generate a TestPending message when the test body is (pending)") {
      val a = new RefSpec {

        def `test: do this`(): Unit = { pending }

        def `test: do that`(): Unit = {
          assert(2 + 2 === 4)
        }

        def `test: do something else`(): Unit = {
          assert(2 + 2 === 4)
          pending
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker()))
      val tp = rep.testPendingEventsReceived
      assert(tp.size === 2)
    }

    it("should generate a TestCanceled message when the test body includes a cancel call") {
      val a = new RefSpec {

        def `test: do this`(): Unit = { cancel() }

        def `test: do that`(): Unit = {
          assert(2 + 2 === 4)
        }

        def `test: do something else`(): Unit = {
          assert(2 + 2 === 4)
          cancel()
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker()))
      val tp = rep.testCanceledEventsReceived
      assert(tp.size === 2)
    }

    it("should generate a TestCanceled message when the test body includes a failed assume call") {
      val a = new RefSpec {

        def `test: do this`(): Unit = { assume(1 === 2) }

        def `test: do that`(): Unit = {
          assert(2 + 2 === 4)
        }

        def `test: do something else`(): Unit = {
          assert(2 + 2 === 4)
          assume(3 === 4)
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker()))
      val tp = rep.testCanceledEventsReceived
      assert(tp.size === 2)
    }

    it("should generate a test failure if a Throwable, or an Error other than direct Error subtypes " +
      "known in JDK 1.5, excluding AssertionError") {
      val a = new RefSpec {
        def `test: throws AssertionError`(): Unit = { throw new AssertionError }
        def `test: throws plain old Error`(): Unit = { throw new Error }
        def `test: throws Throwable`(): Unit = { throw new Throwable }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker()))
      val tf = rep.testFailedEventsReceived
      assert(tf.size === 3)
    }

    it("should propagate out Errors that are direct subtypes of Error in JDK 1.5, other than " +
      "AssertionError, causing Suites and Runs to abort.") {
      val a = new RefSpec {
        def `test: throws AssertionError`(): Unit = { throw new OutOfMemoryError }
      }
      intercept[OutOfMemoryError] {
        a.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker()))
      }
    }

    it("should invoke withFixture from runTest for no-arg test method") {
      class SpecA extends RefSpec {
        var withFixtureWasInvoked = false
        var theTestWasInvoked = false
        override def withFixture(test: NoArgTest): Outcome = {
          withFixtureWasInvoked = true
          super.withFixture(test)
        }
        def `test: something`(): Unit = {
          theTestWasInvoked = true
        }
      }
      val a = new SpecA

      import scala.language.reflectiveCalls

      a.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker()))
      assert(a.withFixtureWasInvoked)
      assert(a.theTestWasInvoked)
    }

    it("should pass the correct test name in the NoArgTest passed to withFixture") {
      class SpecA extends RefSpec {
        var correctTestNameWasPassed = false
        override def withFixture(test: NoArgTest): Outcome = {
          correctTestNameWasPassed = test.name == "test: something"
          super.withFixture(test)
        }
        def `test: something`: Unit = {}
      }
      val a = new SpecA

      import scala.language.reflectiveCalls

      a.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker()))
      assert(a.correctTestNameWasPassed)
    }

    it("should pass the correct config map in the NoArgTest passed to withFixture") {
      class SpecA extends RefSpec {
        var correctConfigMapWasPassed = false
        override def withFixture(test: NoArgTest): Outcome = {
          correctConfigMapWasPassed = (test.configMap == ConfigMap("hi" -> 7))
          super.withFixture(test)
        }
        def `test: something`: Unit = {}
      }
      val a = new SpecA

      import scala.language.reflectiveCalls

      a.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap("hi" -> 7), None, new Tracker()))
      assert(a.correctConfigMapWasPassed)
    }

    it("should, when a test method writes to the Informer, report the info in test completion event") {
      val msg = "hi there dude"
      class MySpec extends RefSpec {
        def `test: with Informer`: Unit = {
          info(msg)
        }
      }
      val myRep = new EventRecordingReporter
      new MySpec().run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker))
      val testStarting = myRep.testStartingEventsReceived
      assert(testStarting.size === 1)
      val testSucceeded = myRep.testSucceededEventsReceived
      assert(testSucceeded.size === 1)
      assert(testSucceeded(0).recordedEvents.size === 1)
      val ip: InfoProvided = testSucceeded(0).recordedEvents(0).asInstanceOf[InfoProvided]
      assert(msg === ip.message)
    }

    it("Top-level plain-old specifiers should yield good strings in a TestSucceeded report") {
      var reportHadCorrectTestName = false
      var reportHadCorrectSpecText = false
      var reportHadCorrectFormattedSpecText = false
      class MyReporter extends Reporter {
        def apply(event: Event): Unit = {
          event match {
            case TestSucceeded(ordinal, suiteName, suiteId, suiteClassName, testName, testText, testEvents, duration, formatter, location, rerunnable, payload, threadName, timeStamp) =>
              if (testName.indexOf("must start with proper words") != -1)
                reportHadCorrectTestName = true
              formatter match {
                case Some(IndentedText(formattedText, rawText, indentationLevel)) =>
                  if (rawText == "must start with proper words")
                    reportHadCorrectSpecText = true
                  if (formattedText == "- must start with proper words")
                    reportHadCorrectFormattedSpecText = true
                case _ =>
              }
            case _ =>
          }
        }
      }
      class MySpec extends RefSpec with Matchers {
        def `must start with proper words`: Unit = {}
      }
      val a = new MySpec
      a.run(None, Args(new MyReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker))
      assert(reportHadCorrectTestName)
      assert(reportHadCorrectSpecText)
      assert(reportHadCorrectFormattedSpecText)
    }

    it("Top-level plain-old specifiers should yield good strings in a testSucceeded report") {
      var reportHadCorrectTestName = false
      var reportHadCorrectSpecText = false
      var reportHadCorrectFormattedSpecText = false
      class MyReporter extends Reporter {
        def apply(event: Event): Unit = {
          event match {
            case TestSucceeded(ordinal, suiteName, suiteId, suiteClassName, testName, testText, testEvents, duration, formatter, location, rerunnable, payload, threadName, timeStamp) =>
              if (testName.indexOf("must start with proper words") != -1)
                reportHadCorrectTestName = true
              formatter match {
                case Some(IndentedText(formattedText, rawText, indentationLevel)) =>
                  if (rawText == "must start with proper words")
                    reportHadCorrectSpecText = true
                  if (formattedText == "- must start with proper words")
                    reportHadCorrectFormattedSpecText = true
                case _ =>
              }
            case _ =>
          }
        }
      }
      class MySpec extends RefSpec with Matchers {
        def `must start with proper words`: Unit = {}
      }
      val a = new MySpec
      a.run(None, Args(new MyReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker))
      assert(reportHadCorrectTestName)
      assert(reportHadCorrectSpecText)
      assert(reportHadCorrectFormattedSpecText)
    }

    it("Top-level plain-old specifiers should yield good strings in a testFailed report") {
      var reportHadCorrectTestName = false
      var reportHadCorrectSpecText = false
      var reportHadCorrectFormattedSpecText = false
      class MyReporter extends Reporter {
        def apply(event: Event): Unit = {
          event match {
            case event: TestFailed =>
              if (event.testName.indexOf("must start with proper words") != -1)
                reportHadCorrectTestName = true
              event.formatter match {
                case Some(IndentedText(formattedText, rawText, indentationLevel)) =>
                  if (rawText == "must start with proper words")
                    reportHadCorrectSpecText = true
                  if (formattedText == "- must start with proper words")
                    reportHadCorrectFormattedSpecText = true
                case _ =>
              }
            case _ =>
          }
        }
      }
      class MySpec extends RefSpec with Matchers {
        def `must start with proper words`: Unit = { fail() }
      }
      val a = new MySpec
      a.run(None, Args(new MyReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker))
      assert(reportHadCorrectTestName)
      assert(reportHadCorrectSpecText)
      assert(reportHadCorrectFormattedSpecText)
    }

    // Tests for good strings in report for nested-one-level examples
    it("Nested-one-level plain-old specifiers should yield good strings in a TestSucceeded report") {
      var infoReportHadCorrectTestName = false
      var infoReportHadCorrectSpecText = false
      var infoReportHadCorrectFormattedSpecText = false
      var reportHadCorrectTestName = false
      var reportHadCorrectSpecText = false
      var reportHadCorrectFormattedSpecText = false
      var scopeOpenedHasBeenInvoked = false
      var theOtherMethodHasBeenInvoked = false
      class MyReporter extends Reporter {
        def apply(event: Event): Unit = {
          event match {
            case ScopeOpened(ordinal, message, nameInfo, formatter, location, payload, threadName, timeStamp) =>
              // scopeOpened should be invoked before the other method
              assert(!theOtherMethodHasBeenInvoked)
              scopeOpenedHasBeenInvoked = true
              if (message.indexOf("My RefSpec") != -1)
                infoReportHadCorrectTestName = true
              formatter match {
                case Some(IndentedText(formattedText, rawText, indentationLevel)) =>
                  if (rawText == "My RefSpec")
                    infoReportHadCorrectSpecText = true
                  if (formattedText == "My RefSpec")
                    infoReportHadCorrectFormattedSpecText = true
                case _ =>
              }
            case TestSucceeded(ordinal, suiteName, suiteId, suiteClassName, testName, testText, testEvents, duration, formatter, location, rerunnable, payload, threadName, timeStamp) =>
              // scopeOpened should be invoked before the this method
              assert(scopeOpenedHasBeenInvoked)
              theOtherMethodHasBeenInvoked = true
              if (testName.indexOf("My RefSpec must start with proper words") != -1)
                reportHadCorrectTestName = true
              formatter match {
                case Some(IndentedText(formattedText, rawText, indentationLevel)) =>
                  if (rawText == "must start with proper words")
                    reportHadCorrectSpecText = true
                  if (formattedText == "- must start with proper words")
                    reportHadCorrectFormattedSpecText = true
                case _ =>
              }
            case _ =>
          }
        }
      }
      class MySpec extends RefSpec with Matchers {
        object `My RefSpec` {
          def `must start with proper words`: Unit = {}
        }
      }
      val a = new MySpec
      a.run(None, Args(new MyReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker))
      assert(reportHadCorrectTestName)
      assert(reportHadCorrectSpecText)
      assert(reportHadCorrectFormattedSpecText)
      assert(infoReportHadCorrectTestName)
      assert(infoReportHadCorrectSpecText)
      assert(infoReportHadCorrectFormattedSpecText)
    }

    it("Nested-one-level plain-old specifiers should yield good strings in a testSucceeded report") {
      var infoReportHadCorrectTestName = false
      var infoReportHadCorrectSpecText = false
      var infoReportHadCorrectFormattedSpecText = false
      var reportHadCorrectTestName = false
      var reportHadCorrectSpecText = false
      var reportHadCorrectFormattedSpecText = false
      var scopeOpenedHasBeenInvoked = false
      var theOtherMethodHasBeenInvoked = false
      class MyReporter extends Reporter {
        def apply(event: Event): Unit = {
          event match {
            case ScopeOpened(ordinal, message, nameInfo, formatter, location, payload, threadName, timeStamp) =>
              // scopeOpened should be invoked before the other method
              assert(!theOtherMethodHasBeenInvoked)
              scopeOpenedHasBeenInvoked = true
              if (message.indexOf("My RefSpec") != -1)
                infoReportHadCorrectTestName = true
              formatter match {
                case Some(IndentedText(formattedText, rawText, indentationLevel)) =>
                  if (rawText == "My RefSpec")
                    infoReportHadCorrectSpecText = true
                  if (formattedText == "My RefSpec")
                    infoReportHadCorrectFormattedSpecText = true
                case _ =>
              }
            case TestSucceeded(ordinal, suiteName, suiteId, suiteClassName, testName, testText, testEvents, duration, formatter, location, rerunnable, payload, threadName, timeStamp) =>
              // scopeOpened should be invoked before the this method
              assert(scopeOpenedHasBeenInvoked)
              theOtherMethodHasBeenInvoked = true
              if (testName.indexOf("My RefSpec must start with proper words") != -1)
                reportHadCorrectTestName = true
              formatter match {
                case Some(IndentedText(formattedText, rawText, indentationLevel)) =>
                  if (rawText == "must start with proper words")
                    reportHadCorrectSpecText = true
                  if (formattedText == "- must start with proper words")
                    reportHadCorrectFormattedSpecText = true
                case _ =>
              }
            case _ =>
          }
        }
      }
      class MySpec extends RefSpec with Matchers {
        object `My RefSpec` {
          def `must start with proper words`: Unit = {}
        }
      }
      val a = new MySpec
      a.run(None, Args(new MyReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker))
      assert(reportHadCorrectTestName)
      assert(reportHadCorrectSpecText)
      assert(reportHadCorrectFormattedSpecText)
      assert(infoReportHadCorrectTestName)
      assert(infoReportHadCorrectSpecText)
      assert(infoReportHadCorrectFormattedSpecText)
    }

    it("Nested-one-level plain-old specifiers should yield good strings in a TestFailed report") {
      var infoReportHadCorrectTestName = false
      var infoReportHadCorrectSpecText = false
      var infoReportHadCorrectFormattedSpecText = false
      var reportHadCorrectTestName = false
      var reportHadCorrectSpecText = false
      var reportHadCorrectFormattedSpecText = false
      var scopeOpenedHasBeenInvoked = false
      var theOtherMethodHasBeenInvoked = false
      class MyReporter extends Reporter {
        def apply(event: Event): Unit = {
          event match {
            case ScopeOpened(ordinal, message, nameInfo, formatter, location, payload, threadName, timeStamp) =>
              // scopeOpened should be invoked before the other method
              assert(!theOtherMethodHasBeenInvoked)
              scopeOpenedHasBeenInvoked = true
              if (message.indexOf("My RefSpec") != -1)
                infoReportHadCorrectTestName = true
              formatter match {
                case Some(IndentedText(formattedText, rawText, indentationLevel)) =>
                  if (rawText == "My RefSpec")
                    infoReportHadCorrectSpecText = true
                  if (formattedText == "My RefSpec")
                    infoReportHadCorrectFormattedSpecText = true
                case _ =>
              }
            case event: TestFailed =>
              // scopeOpened should be invoked before the this method
              assert(scopeOpenedHasBeenInvoked)
              theOtherMethodHasBeenInvoked = true
              if (event.testName.indexOf("My RefSpec must start with proper words") != -1)
                reportHadCorrectTestName = true
              event.formatter match {
                case Some(IndentedText(formattedText, rawText, indentationLevel)) =>
                  if (rawText == "must start with proper words")
                    reportHadCorrectSpecText = true
                  if (formattedText == "- must start with proper words")
                    reportHadCorrectFormattedSpecText = true
                case _ =>
              }
            case _ =>
          }
        }
      }
      class MySpec extends RefSpec with Matchers {
        object`My RefSpec` {
          def `must start with proper words`: Unit = { fail() }
        }
      }
      val a = new MySpec
      a.run(None, Args(new MyReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker))
      assert(reportHadCorrectTestName)
      assert(reportHadCorrectSpecText)
      assert(reportHadCorrectFormattedSpecText)
      assert(infoReportHadCorrectTestName)
      assert(infoReportHadCorrectSpecText)
      assert(infoReportHadCorrectFormattedSpecText)
    }

    // Tests for good strings in report for nested-two-levels examples
    it("Nested-two-levels plain-old specifiers should yield good strings in a TestSucceeded report") { //ZZZ
    var infoReportHadCorrectTestName = false
      var infoReportHadCorrectSpecText = false
      var infoReportHadCorrectFormattedSpecText = false
      var reportHadCorrectTestName = false
      var reportHadCorrectSpecText = false
      var reportHadCorrectFormattedSpecText = false
      var scopeOpenedHasBeenInvokedOnce = false
      var scopeOpenedHasBeenInvokedTwice = false
      var theOtherMethodHasBeenInvoked = false
      class MyReporter extends Reporter {
        def apply(event: Event): Unit = {
          event match {
            case ScopeOpened(ordinal, message, nameInfo, formatter, location, payload, threadName, timeStamp) =>
              // scopeOpened should be invoked before the other method
              assert(!theOtherMethodHasBeenInvoked)
              if (!scopeOpenedHasBeenInvokedOnce) {
                scopeOpenedHasBeenInvokedOnce = true
                if (message.indexOf("My RefSpec") >= 0)
                  infoReportHadCorrectTestName = true
                formatter match {
                  case Some(IndentedText(formattedText, rawText, indentationLevel)) =>
                    if (rawText == "My RefSpec")
                      infoReportHadCorrectSpecText = true
                    if (formattedText == "My RefSpec")
                      infoReportHadCorrectFormattedSpecText = true
                  case _ =>
                }
              }
              else {
                scopeOpenedHasBeenInvokedTwice = true
                if (message.indexOf("must start") < 0)
                  infoReportHadCorrectTestName = false
                formatter match {
                  case Some(IndentedText(formattedText, rawText, indentationLevel)) =>
                    if (rawText != "must start")
                      infoReportHadCorrectSpecText = false
                    if (formattedText != "  must start")
                      infoReportHadCorrectFormattedSpecText = false
                  case _ =>
                }
              }
            case TestSucceeded(ordinal, suiteName, suiteId, suiteClassName, testName, testText, testEvents, duration, formatter, location, rerunnable, payload, threadName, timeStamp) =>
              // scopeOpened should be invoked before the this method
              assert(scopeOpenedHasBeenInvokedTwice)
              theOtherMethodHasBeenInvoked = true
              if (testName.indexOf("My RefSpec must start with proper words") != -1)
                reportHadCorrectTestName = true
              formatter match {
                case Some(IndentedText(formattedText, rawText, indentationLevel)) =>
                  if (rawText == "with proper words")
                    reportHadCorrectSpecText = true
                  if (formattedText == "  - with proper words")
                    reportHadCorrectFormattedSpecText = true
                case _ =>
              }
            case _ =>
          }
        }
      }
      class MySpec extends RefSpec with Matchers {
        object `My RefSpec` {
          object `must start` {
            def `with proper words`: Unit = {}
          }
        }
      }
      val a = new MySpec
      a.run(None, Args(new MyReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker))
      assert(reportHadCorrectTestName)
      assert(reportHadCorrectSpecText)
      assert(reportHadCorrectFormattedSpecText)
      assert(infoReportHadCorrectTestName)
      assert(infoReportHadCorrectSpecText)
      assert(infoReportHadCorrectFormattedSpecText)
    }

    it("Nested-two-levels plain-old specifiers should yield good strings in a TestFailed report") { //YYY
    var infoReportHadCorrectTestName = false
      var infoReportHadCorrectSpecText = false
      var infoReportHadCorrectFormattedSpecText = false
      var reportHadCorrectTestName = false
      var reportHadCorrectSpecText = false
      var reportHadCorrectFormattedSpecText = false
      var scopeOpenedHasBeenInvokedOnce = false
      var scopeOpenedHasBeenInvokedTwice = false
      var theOtherMethodHasBeenInvoked = false
      class MyReporter extends Reporter {
        def apply(event: Event): Unit = {
          event match {
            case ScopeOpened(ordinal, message, nameInfo, formatter, location, payload, threadName, timeStamp) =>
              // scopeOpened should be invoked before the other method
              assert(!theOtherMethodHasBeenInvoked)
              if (!scopeOpenedHasBeenInvokedOnce) {
                scopeOpenedHasBeenInvokedOnce = true
                if (message.indexOf("My RefSpec") >= 0)
                  infoReportHadCorrectTestName = true
                formatter match {
                  case Some(IndentedText(formattedText, rawText, indentationLevel)) =>
                    if (rawText == "My RefSpec")
                      infoReportHadCorrectSpecText = true
                    if (formattedText == "My RefSpec")
                      infoReportHadCorrectFormattedSpecText = true
                  case _ =>
                }
              }
              else {
                scopeOpenedHasBeenInvokedTwice = true
                if (message.indexOf("must start") < 0)
                  infoReportHadCorrectTestName = false
                formatter match {
                  case Some(IndentedText(formattedText, rawText, indentationLevel)) =>
                    if (rawText != "must start")
                      infoReportHadCorrectSpecText = false
                    if (formattedText != "  must start")
                      infoReportHadCorrectFormattedSpecText = false
                  case _ =>
                }
              }
            case event: TestFailed =>
              // scopeOpened should be invoked before the this method
              assert(scopeOpenedHasBeenInvokedTwice)
              theOtherMethodHasBeenInvoked = true
              if (event.testName.indexOf("My RefSpec must start with proper words") != -1)
                reportHadCorrectTestName = true
              event.formatter match {
                case Some(IndentedText(formattedText, rawText, indentationLevel)) =>
                  if (rawText == "with proper words")
                    reportHadCorrectSpecText = true
                  if (formattedText == "  - with proper words")
                    reportHadCorrectFormattedSpecText = true
                case _ =>
              }
            case _ =>
          }
        }
      }
      class MySpec extends RefSpec with Matchers {
        object `My RefSpec` {
          object `must start` {
            def `with proper words`: Unit = { fail() }
          }
        }
      }
      val a = new MySpec
      a.run(None, Args(new MyReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker))
      assert(reportHadCorrectTestName)
      assert(reportHadCorrectSpecText)
      assert(reportHadCorrectFormattedSpecText)
      assert(infoReportHadCorrectTestName)
      assert(infoReportHadCorrectSpecText)
      assert(infoReportHadCorrectFormattedSpecText)
    }

    // Testing strings sent in reports
    it("In a TestSucceeded report, the test name should be verbatim if it is top level test") {
      var testSucceededReportHadCorrectTestName = false
      class MyReporter extends Reporter {
        def apply(event: Event): Unit = {
          event match {
            case TestSucceeded(ordinal, suiteName, suiteId, suiteClassName, testName, testText, testEvents, duration, formatter, location, rerunnable, payload, threadName, timeStamp) =>
              if (testName.indexOf("this thing must start with proper words") != -1) {
                testSucceededReportHadCorrectTestName = true
              }
            case _ =>
          }
        }
      }
      class MySpec extends RefSpec with Matchers {
        def `this thing must start with proper words`: Unit = {}
      }
      val a = new MySpec
      a.run(None, Args(new MyReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker))
      assert(testSucceededReportHadCorrectTestName)
    }

    it("In a TestFailed report, the test name should be verbatim if it is top level test") {
      var testFailedReportHadCorrectTestName = false
      class MyReporter extends Reporter {
        def apply(event: Event): Unit = {
          event match {
            case event: TestFailed =>
              if (event.testName.indexOf("this thing must start with proper words") != -1)
                testFailedReportHadCorrectTestName = true
            case _ =>
          }
        }
      }
      class MySpec extends RefSpec with Matchers {
        def `this thing must start with proper words`: Unit = { fail() }
      }
      val a = new MySpec
      a.run(None, Args(new MyReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker))
      assert(testFailedReportHadCorrectTestName)
    }

    it("In a TestStarting report, the test name should start with '<scope> ' if nested one level " +
      "inside a object clause and registered with it") {
      var testSucceededReportHadCorrectTestName = false
      class MyReporter extends Reporter {
        def apply(event: Event): Unit = {
          event match {
            case TestStarting(_, _, _, _, testName, _, _, _, _, _, _, _) =>
              if (testName == "A Stack needs to push and pop properly") {
                testSucceededReportHadCorrectTestName = true
              }
            case _ =>
          }
        }
      }
      class MySpec extends RefSpec with Matchers {
        object `A Stack` {
          def `needs to push and pop properly`: Unit = {}
        }
      }
      val a = new MySpec
      a.run(None, Args(new MyReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker))
      assert(testSucceededReportHadCorrectTestName)
    }

    it("Spec should send defined formatters") {
      class MyReporter extends Reporter {

        var gotAnUndefinedFormatter = false
        var lastEventWithUndefinedFormatter: Option[Event] = None

        private def ensureFormatterIsDefined(event: Event): Unit = {
          if (!event.formatter.isDefined) {
            gotAnUndefinedFormatter = true
            lastEventWithUndefinedFormatter = Some(event)
          }
        }

        def apply(event: Event): Unit = {
          event match {
            case event: RunAborted => ensureFormatterIsDefined(event)
            case event: SuiteAborted => ensureFormatterIsDefined(event)
            case event: SuiteStarting => ensureFormatterIsDefined(event)
            case event: SuiteCompleted => ensureFormatterIsDefined(event)
            case event: TestStarting => ensureFormatterIsDefined(event)
            case event: TestSucceeded => ensureFormatterIsDefined(event)
            case event: TestIgnored => ensureFormatterIsDefined(event)
            case event: TestFailed => ensureFormatterIsDefined(event)
            case event: InfoProvided => ensureFormatterIsDefined(event)
            case _ =>
          }
        }
      }

      class MySpec extends RefSpec with Matchers {
        def `it should send defined formatters`: Unit = {
          assert(true)
        }
        def `it should also send defined formatters`: Unit = {
          assert(false)
        }
      }
      val a = new MySpec
      val myRep = new MyReporter
      a.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker))
      assert(!myRep.gotAnUndefinedFormatter, myRep.lastEventWithUndefinedFormatter.toString)
    }

    it("SpecText should come through correctly in a RefSpecReport when registering with def") {
      var testSucceededReportHadCorrectSpecText = false
      var lastSpecText: Option[String] = None
      class MyReporter extends Reporter {
        def apply(event: Event): Unit = {
          event match {
            case TestSucceeded(ordinal, suiteName, suiteId, suiteClassName, testName, testText, testEvents, duration, formatter, location, rerunnable, payload, threadName, timeStamp) =>
              formatter match {
                case Some(IndentedText(formattedText, rawText, indentationLevel)) =>
                  if (rawText == "My spec text must have the proper words")
                    testSucceededReportHadCorrectSpecText = true
                  else
                    lastSpecText = Some(rawText)
                case _ => throw new RuntimeException("Got a non-SpecReport")
              }
            case _ =>
          }
        }
      }
      class MySpec extends RefSpec with Matchers {
        def `My spec text must have the proper words`: Unit = {}
      }
      val a = new MySpec
      a.run(None, Args(new MyReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker))
      assert(testSucceededReportHadCorrectSpecText, lastSpecText match { case Some(s) => s; case None => "No report"})
    }

    it("Spec text should come through correctly in a RefSpecReport when registering with def when nested in one object") {
      var testSucceededReportHadCorrectSpecText = false
      var lastSpecText: Option[String] = None
      class MyReporter extends Reporter {
        def apply(event: Event): Unit = {
          event match {
            case TestSucceeded(ordinal, suiteName, suiteId, suiteClassName, testName, testText, testEvents, duration, formatter, location, rerunnable, payload, threadName, timeStamp) =>
              formatter match {
                case Some(IndentedText(formattedText, rawText, indentationLevel)) =>
                  if (rawText == "My short name must have the proper words")
                    testSucceededReportHadCorrectSpecText = true
                  else
                    lastSpecText = Some(rawText)
                case _ => throw new RuntimeException("Got a non-SpecReport")
              }
            case _ =>
          }
        }
      }
      class MySpec extends RefSpec with Matchers {
        object `A Stack` {
          def `My short name must have the proper words`: Unit = {}
        }
      }
      val a = new MySpec
      a.run(None, Args(new MyReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker))
      assert(testSucceededReportHadCorrectSpecText, lastSpecText match { case Some(s) => s; case None => "No report"})
    }

    it("Spec text should come through correctly in a RefSpecReport when registering with def when nested in two objects") {
      var testSucceededReportHadCorrectSpecText = false
      var lastSpecText: Option[String] = None
      class MyReporter extends Reporter {
        def apply(event: Event): Unit = {
          event match {
            case TestSucceeded(ordinal, suiteName, suiteId, suiteClassName, testName, testText, testEvents, duration, formatter, location, rerunnable, payload, threadName, timeStamp) =>
              formatter match {
                case Some(IndentedText(formattedText, rawText, indentationLevel)) =>
                  if (rawText == "My short name must have the proper words")
                    testSucceededReportHadCorrectSpecText = true
                  else
                    lastSpecText = Some(rawText)
                case _ => throw new RuntimeException("Got a non-SpecReport")
              }
            case _ =>
          }
        }
      }
      class MySpec extends RefSpec with Matchers {
        object `A Stack` {
          object `(when empty)` {
            def `My short name must have the proper words`: Unit = {}
          }
        }
      }
      val a = new MySpec
      a.run(None, Args(new MyReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker))
      assert(testSucceededReportHadCorrectSpecText, lastSpecText match { case Some(s) => s; case None => "No report"})
    }

    it("Should get ScopedOpened with description if one and only one object clause") {

      val expectedSpecText = "A Stack"

      class MyReporter extends Reporter {
        var scopeOpenedCalled = false
        var expectedMessageReceived = false
        def apply(event: Event): Unit = {
          event match {
            case event: ScopeOpened =>
              event.formatter match {
                case Some(IndentedText(formattedText, rawText, indentationLevel)) =>
                  scopeOpenedCalled = true
                  if (!expectedMessageReceived) {
                    expectedMessageReceived = (rawText == expectedSpecText)
                  }
                case _ =>
              }
            case _ =>
          }
        }
      }

      class MySpec extends RefSpec with Matchers {
        object `A Stack` {
          def `should allow me to push`: Unit = {}
        }
      }

      val a = new MySpec
      val myRep = new MyReporter
      a.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker))
      assert(myRep.scopeOpenedCalled)
      assert(myRep.expectedMessageReceived)
    }

    it("should be able to send info to the reporter") {

      val expectedMessage = "this is the expected message"

      class MyReporter extends Reporter {
        var infoProvidedCalled = false
        var expectedMessageReceived = false

        def apply(event: Event): Unit = {
          event match {
            case testSucceeded: TestSucceeded if testSucceeded.testName == "A Stack (when not empty) should allow me to pop" =>
              val recordedEvents = testSucceeded.recordedEvents
              recordedEvents(0) match {
                case event: InfoProvided =>
                  infoProvidedCalled = true
                  if (!expectedMessageReceived) {
                    expectedMessageReceived = event.message.indexOf(expectedMessage) != -1
                  }
                case _ =>
              }
            case _ =>
          }
        }
      }

      class MySpec extends RefSpec with Matchers {
        object `A Stack` {
          object `(when not empty)` {
            def `should allow me to pop`: Unit = {
              info(expectedMessage)
              ()
            }
          }
          object `(when not full)` {
            def `should allow me to push`: Unit = {}
          }
        }
      }
      val a = new MySpec
      val myRep = new MyReporter
      a.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker))
      assert(myRep.infoProvidedCalled)
      assert(myRep.expectedMessageReceived)
    }

    it("test durations are included in TestFailed and TestSucceeded events fired from RefSpec") {

      class MySpec extends RefSpec {
        def `should succeed`: Unit = {}
        def `should fail`: Unit = { fail() }
      }

      val mySpec = new MySpec
      val myReporter = new TestDurationReporter
      mySpec.run(None, Args(myReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker(new Ordinal(99))))
      assert(myReporter.testSucceededWasFiredAndHadADuration)
      assert(myReporter.testFailedWasFiredAndHadADuration)
    }

    it("suite durations are included in SuiteCompleted events fired from RefSpec") {

      class MySpec extends RefSpec {
        override def nestedSuites = Vector(new Suite {})
      }

      val mySuite = new MySpec
      val myReporter = new SuiteDurationReporter
      mySuite.run(None, Args(myReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker(new Ordinal(99))))
      assert(myReporter.suiteCompletedWasFiredAndHadADuration)
    }

    it("suite durations are included in SuiteAborted events fired from RefSpec") {

      class SuiteThatAborts extends Suite {
        override def run(testName: Option[String], args: Args): Status = {
          throw new RuntimeException("Aborting for testing purposes")
        }
      }

      class MySpec extends RefSpec {
        override def nestedSuites = Vector(new SuiteThatAborts {})
      }

      val mySuite = new MySpec
      val myReporter = new SuiteDurationReporter
      mySuite.run(None, Args(myReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker(new Ordinal(99))))
      assert(myReporter.suiteAbortedWasFiredAndHadADuration)
    }

    it("pending in a RefSpec should cause TestPending to be fired") {

      class MySpec extends RefSpec {
        def `should be pending`: Unit = { pending }
      }

      val mySuite = new MySpec
      val myReporter = new PendingReporter
      mySuite.run(None, Args(myReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker(new Ordinal(99))))
      assert(myReporter.testPendingWasFired)
    }

    it("should unwrap InvocationTargetException thrown from scope evaluation") {

      class ExampleSpec extends RefSpec {
        object `A Scope` {
          throw new AnnotationFormatError("boom!")
          def `Test 1`: Unit = {}
          def `Test 2`: Unit = {}
          def `Test 3`: Unit = {}
        }
      }

      val s = new ExampleSpec
      intercept[AnnotationFormatError] {
        s.run(None, Args(reporter = SilentReporter))
      }
    }

    describe("the stopper") {

      it("should stop nested suites from being executed") {
        class SpecA extends RefSpec {
          var executed = false;
          override def run(testName: Option[String], args: Args): Status = {
            executed = true
            super.run(testName, args)
          }
        }
        class SpecB extends RefSpec {
          var executed = false;
          override def run(testName: Option[String], args: Args): Status = {
            executed = true
            super.run(testName, args)
          }
        }
        class SpecC extends RefSpec {
          var executed = false;
          override def run(testName: Option[String], args: Args): Status = {
            executed = true
            super.run(testName, args)
          }
        }
        class SpecD extends RefSpec {
          var executed = false;
          override def run(testName: Option[String], args: Args): Status = {
            executed = true
            val status = super.run(testName, args)
            args.stopper.requestStop()
            status
          }
        }
        class SpecE extends RefSpec {
          var executed = false;
          override def run(testName: Option[String], args: Args): Status = {
            executed = true
            super.run(testName, args)
          }
        }
        class SpecF extends RefSpec {
          var executed = false;
          override def run(testName: Option[String], args: Args): Status = {
            executed = true
            super.run(testName, args)
          }
        }
        class SpecG extends RefSpec {
          var executed = false;
          override def run(testName: Option[String], args: Args): Status = {
            executed = true
            super.run(testName, args)
          }
        }

        val a = new SpecA
        val b = new SpecB
        val c = new SpecC
        val d = new SpecD
        val e = new SpecE
        val f = new SpecF
        val g = new SpecG

        class IgnoreStopRequestStopper extends Stopper {
          def stopRequested: Boolean = false
          def requestStop(): Unit = {}
          def reset(): Unit = {}
        }

        val x = Suites(a, b, c, d, e, f, g)
        x.run(None, Args(SilentReporter, new IgnoreStopRequestStopper, Filter(), ConfigMap.empty, None, new Tracker))

        assert(a.executed)
        assert(b.executed)
        assert(c.executed)
        assert(d.executed)
        assert(e.executed)
        assert(f.executed)
        assert(g.executed)


        val h = new SpecA
        val i = new SpecB
        val j = new SpecC
        val k = new SpecD
        val l = new SpecE
        val m = new SpecF
        val n = new SpecG

        val y = Suites(h, i, j, k, l, m, n)
        y.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker))

        assert(k.executed)
        assert(i.executed)
        assert(j.executed)
        assert(k.executed)
        assert(!l.executed)
        assert(!m.executed)
        assert(!n.executed)
      }

      it("should stop tests from being executed") {

        class MySpec extends RefSpec {
          var theTestsExecutedCount = 0
          def `test: 1`(): Unit = { theTestsExecutedCount += 1 }
          def `test: 2`(): Unit = { theTestsExecutedCount += 1 }
          def `test: 3`(): Unit = { theTestsExecutedCount += 1 }
          def `test: 4`(): Unit = {
            theTestsExecutedCount += 1
          }
          def `test: 5`(): Unit = { theTestsExecutedCount += 1 }
          def `test: 6`(): Unit = { theTestsExecutedCount += 1 }
          def `test: 7`(): Unit = { theTestsExecutedCount += 1 }
        }

        val x = new MySpec
        x.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker))
        assert(x.theTestsExecutedCount === 7)

        val myStopper = Stopper.default

        class MyStoppingSpec extends RefSpec {
          var testsExecutedCount = 0
          def `test: 1`(): Unit = { testsExecutedCount += 1 }
          def `test: 2`(): Unit = { testsExecutedCount += 1 }
          def `test: 3`(): Unit = { testsExecutedCount += 1 }
          def `test: 4`(): Unit = {
            testsExecutedCount += 1
            myStopper.requestStop()
          }
          def `test: 5`(): Unit = { testsExecutedCount += 1 }
          def `test: 6`(): Unit = { testsExecutedCount += 1 }
          def `test: 7`(): Unit = { testsExecutedCount += 1 }
        }

        val y = new MyStoppingSpec
        y.run(None, Args(SilentReporter, myStopper, Filter(), ConfigMap.empty, None, new Tracker))
        assert(y.testsExecutedCount === 4)
      }
    }

    describe("(with info calls)") {
      class InfoInsideTestSpec extends RefSpec {
        val msg = "hi there, dude"
        def `test name`: Unit = {
          info(msg)
        }
      }
      // In a FlatSpec, any InfoProvided's fired during the test should be cached and sent out after the test has
      // suceeded or failed. This makes the report look nicer, because the info is tucked under the "specifier'
      // text for that test.
      it("should, when the info appears in the code of a successful test, report the info in the TestSucceeded") {
        val spec = new InfoInsideTestSpec
        /*val (infoProvidedIndex, testStartingIndex, testSucceededIndex) =
          getIndexesForInformerEventOrderTests(spec, spec.testName, spec.msg)
        assert(testSucceededIndex < infoProvidedIndex)*/
        val myRep = new EventRecordingReporter
        spec.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker))
        val testStarting = myRep.testStartingEventsReceived
        assert(1 === testStarting.size)
        val testSucceeded = myRep.testSucceededEventsReceived
        assert(1 === testSucceeded.size)
        assert(1 === testSucceeded(0).recordedEvents.size)
        val ip: InfoProvided = testSucceeded(0).recordedEvents(0).asInstanceOf[InfoProvided]
        assert(spec.msg === ip.message)
      }
      class InfoBeforeTestSpec extends RefSpec {
        val msg = "hi there, dude"
        val testName = "test name"
        info(msg)
        def `test name`: Unit = {}
      }
      it("should, when the info appears in the body before a test, report the info before the test") {
        val spec = new InfoBeforeTestSpec
        val (infoProvidedIndex, testStartingIndex, testSucceededIndex) =
          getIndexesForInformerEventOrderTests(spec, spec.testName, spec.msg)
        assert(infoProvidedIndex < testStartingIndex)
        assert(testStartingIndex < testSucceededIndex)
      }
      it("should print to stdout when info is called by a method invoked after the suite has been executed") {
        class MySpec extends RefSpec {
          callInfo() // This should work fine
          def callInfo(): Unit = {
            info("howdy")
          }
          def `howdy also`: Unit = {
            callInfo() // This should work fine
          }
        }
        val spec = new MySpec
        val myRep = new EventRecordingReporter
        spec.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker))
        spec.callInfo() // TODO: Actually test that This prints to stdout
      }
      it("should send an InfoProvided with an IndentedText formatter with level 0 when called outside a test") {
        val spec = new InfoBeforeTestSpec
        val indentedText = getIndentedTextFromInfoProvided(spec)
        assert(indentedText === IndentedText("+ " + spec.msg, spec.msg, 0))
      }
      it("should send an InfoProvided with an IndentedText formatter with level 1 when called within a test") {
        val spec = new InfoInsideTestSpec
        val myRep = new EventRecordingReporter
        spec.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker))
        val indentedText = getIndentedTextFromTestInfoProvided(spec)
        assert(indentedText === IndentedText("  + " + spec.msg, spec.msg, 1))
      }
    }
  }

  describe("A Suite's execute method") {
    it("should throw NAE if passed null for configMap") {
      class MySpec extends RefSpec
      intercept[NullArgumentException] {
        (new MySpec).execute(configMap = null)
      }
    }
    it("should throw IAE if a testName is passed that does not exist on the suite") {
      class MySpec extends RefSpec
      intercept[IllegalArgumentException] {
        (new MySpec).execute(testName = "fred")
      }
    }
  }

  it("should discover method names and tags") {

    val a = new RefSpec {
      def `some test name`: Unit = ()
    }
    assert(a.expectedTestCount(Filter()) === 1)
    val tnResult: Set[String] = a.testNames
    val gResult: Map[String, Set[String]] = a.tags
    assert(tnResult.size === 1)
    assert(gResult.keySet.size === 0)
  }

  it("should not return tests with no tags in the tags map") {

    val a = new RefSpec {
      def `test: not tagged`: Unit = ()
    }
    assert(a.tags.keySet.size === 0)
  }

  it("should discover methods that return non-Unit") {
    val a = new RefSpec {
      def `test: this`: Int = 1
      def `test: that`(): String = "hi"
    }
    assert(a.expectedTestCount(Filter()) === 2)
    assert(a.testNames.size === 2)
    assert(a.tags.keySet.size === 0)
  }

  it("should not discover $$outer method generated by scala compiler as test") {
    class SetSpec extends RefSpec {
      object `A Set` {
        object `when empty` {
          def `should have size 0`: Unit = {
            assert(Set.empty.size === 0)
          }

          def `should produce NoSuchElementException when head is invoked`: Unit = {
            intercept[NoSuchElementException] {
              Set.empty.head
            }
          }
        }
      }
    }

    val a = new SetSpec
    assert(a.testNames.size === 2)
  }

  it("should send defined durations") {

    class MySpec extends RefSpec {
      def `test succeeds`: Unit = ()
      def `test fails`: Unit = { fail() }
    }

    val mySpec = new MySpec
    val myReporter = new TestDurationReporter
    mySpec.run(None, Args(myReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker(new Ordinal(99))))
    assert(myReporter.testSucceededWasFiredAndHadADuration)
    assert(myReporter.testFailedWasFiredAndHadADuration)
  }

  class SpecThatAborts extends RefSpec {
    override def run(testName: Option[String], args: Args): Status = {
      throw new RuntimeException("Aborting for testing purposes")
    }
  }

  it("should fire a defined duration in a SuiteCompleted event") {

    // the spec duration is sent by runNestedSuites, so MySpec needs a
    // nested suite
    class MySpec extends RefSpec {
      override def nestedSuites = Vector(new RefSpec {})
      def `test Succeeds`(): Unit = ()
      def `test Fails`(): Unit = { fail() }
    }

    val mySpec = new MySpec
    val myReporter = new SuiteDurationReporter
    mySpec.run(None, Args(myReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker(new Ordinal(99))))
    assert(myReporter.suiteCompletedWasFiredAndHadADuration)

  }

  it("should fire a defined duration in a SuiteAborted event") {

    // the suite duration is sent by runNestedSuites, so MySuite needs a
    // nested suite
    class MyOtherSpec extends RefSpec {
      override def nestedSuites = Vector(new SpecThatAborts)
      def `test Succeeds`(): Unit = ()
      def `test Fails`(): Unit = { fail() }
    }

    val myOtherSpec = new MyOtherSpec
    val myOtherReporter = new SuiteDurationReporter
    myOtherSpec.run(None, Args(myOtherReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker(new Ordinal(99))))
    assert(myOtherReporter.suiteAbortedWasFiredAndHadADuration)
  }

  it("should fire TestPending event for a pending test") {

    class MySpec extends RefSpec {
      def `this is a pending test`: Unit = { pending }
    }

    val mySpec = new MySpec
    val myReporter = new PendingReporter
    mySpec.run(None, Args(myReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker(new Ordinal(99))))
    assert(myReporter.testPendingWasFired)
  }

  class TestWasCalledSpec extends RefSpec {
    var theTestThisCalled = false
    var theTestThatCalled = false
    var theTestTheOtherCalled = false
    var theTestThisConfigMapWasEmpty = true
    var theTestThatConfigMapWasEmpty = true
    var theTestTheOtherConfigMapWasEmpty = true
    override def withFixture(test: NoArgTest): Outcome = {
      if (test.configMap.size > 0)
        test.name match {
          case "test this" => theTestThisConfigMapWasEmpty = false
          case "test that" => theTestThatConfigMapWasEmpty = false
          case "test the other" => theTestTheOtherConfigMapWasEmpty = false
          case _ => throw new Exception("Should never happen")
        }
      test()
    }
    def `test this`(): Unit = { theTestThisCalled = true }
    def `test that`(): Unit = { theTestThatCalled = true }
    def `test the other`(): Unit = { theTestTheOtherCalled = true }
  }

  describe("when its execute method is invoked") {

    it("should run all tests, passing an empty config map, if no arguments are passed") {
      val s1 = new TestWasCalledSpec
      s1.execute()
      assert(s1.theTestThisCalled)
      assert(s1.theTestThatCalled)
      assert(s1.theTestTheOtherCalled)
      assert(s1.theTestThisConfigMapWasEmpty)
      assert(s1.theTestThatConfigMapWasEmpty)
      assert(s1.theTestTheOtherConfigMapWasEmpty)
    }

    it("should run just the specified test, passing an empty config map, if a full test name is passed") {
      val s2 = new TestWasCalledSpec
      s2.execute("test this")
      assert(s2.theTestThisCalled)
      assert(!s2.theTestThatCalled)
      assert(!s2.theTestTheOtherCalled)
      assert(s2.theTestThisConfigMapWasEmpty)
      assert(s2.theTestThatConfigMapWasEmpty)
      assert(s2.theTestTheOtherConfigMapWasEmpty)
    }

    it("should pass a non-empty config map into all tests if a just a config map is specified") {
      val s3 = new TestWasCalledSpec
      s3.execute(configMap = ConfigMap("s" -> "s"))
      assert(s3.theTestThisCalled)
      assert(s3.theTestThatCalled)
      assert(s3.theTestTheOtherCalled)
      assert(!s3.theTestThisConfigMapWasEmpty)
      assert(!s3.theTestThatConfigMapWasEmpty)
      assert(!s3.theTestTheOtherConfigMapWasEmpty)
    }

    it("should pass a non-empty config map into just the specified test if a full test name and a config map is specified") {
      val s4 = new TestWasCalledSpec
      s4.execute("test this", ConfigMap("s" -> "s"))
      assert(s4.theTestThisCalled)
      assert(!s4.theTestThatCalled)
      assert(!s4.theTestTheOtherCalled)
      assert(!s4.theTestThisConfigMapWasEmpty)
      assert(s4.theTestThatConfigMapWasEmpty)
      assert(s4.theTestTheOtherConfigMapWasEmpty)
    }

    it("should run just the specified test, passing an empty config map, if a full test name is passed via a named parameter") {
      val s5 = new TestWasCalledSpec
      s5.execute(testName = "test this")
      assert(s5.theTestThisCalled)
      assert(!s5.theTestThatCalled)
      assert(!s5.theTestTheOtherCalled)
      assert(s5.theTestThisConfigMapWasEmpty)
      assert(s5.theTestThatConfigMapWasEmpty)
      assert(s5.theTestTheOtherConfigMapWasEmpty)
    }

    it("should pass a non-empty config map into just the specified test if a full test name and a config map is specified via named parameters") {
      val s6 = new TestWasCalledSpec
      s6.execute(testName = "test this", configMap = ConfigMap("s" -> "s"))
      assert(s6.theTestThisCalled)
      assert(!s6.theTestThatCalled)
      assert(!s6.theTestTheOtherCalled)
      assert(!s6.theTestThisConfigMapWasEmpty)
      assert(s6.theTestThatConfigMapWasEmpty)
      assert(s6.theTestTheOtherConfigMapWasEmpty)
    }
  }
  describe("when its execute method is invoked and a wildcard is passed for the selected test names") {


    /* Remove if indeed don't need
        class TestWasCalledSpec extends RefSpec {
          var theTestThisCalled = false
          var theTestThatCalled = false
          var theTestTheOtherCalled = false
          var theTestThisConfigMapWasEmpty = true
          var theTestThatConfigMapWasEmpty = true
          var theTestTheOtherConfigMapWasEmpty = true
          override def withFixture(test: NoArgTest): Outcome = {
            if (test.configMap.size > 0)
              test.name match {
                case "test$u0020this" => theTestThisConfigMapWasEmpty = false
                case "test$u0020that" => theTestThatConfigMapWasEmpty = false
                case "test$u0020the$u0020other" => theTestTheOtherConfigMapWasEmpty = false
                case _ => throw new Exception("Should never happen")
              }
            test()
          }
          def `test this` { theTestThisCalled = true }
          def `test that` { theTestThatCalled = true }
          def `test the other` { theTestTheOtherCalled = true }
        }
    */

    it("should run all tests, passing in an empty config map, if a wildcard selecting all tests is passed") {
      val s1 = new TestWasCalledSpec
      s1.execute(" th")
      assert(s1.theTestThisCalled)
      assert(s1.theTestThatCalled)
      assert(s1.theTestTheOtherCalled)
      assert(s1.theTestThisConfigMapWasEmpty)
      assert(s1.theTestThatConfigMapWasEmpty)
      assert(s1.theTestTheOtherConfigMapWasEmpty)
    }

    it("should run just one test, passing in an empty config map, if a wildcard selecting only that test is passed") {
      val s2 = new TestWasCalledSpec
      s2.execute(" this")
      assert(s2.theTestThisCalled)
      assert(!s2.theTestThatCalled)
      assert(!s2.theTestTheOtherCalled)
      assert(s2.theTestThisConfigMapWasEmpty)
      assert(s2.theTestThatConfigMapWasEmpty)
      assert(s2.theTestTheOtherConfigMapWasEmpty)
    }

    it("should run all tests, passing in a non-empty config map, if a wildcard selecting all tests and a config map is passed (with config map specified via a named parameter)") {
      val s3 = new TestWasCalledSpec
      s3.execute(" th", configMap = ConfigMap("s" -> "s"))
      assert(s3.theTestThisCalled)
      assert(s3.theTestThatCalled)
      assert(s3.theTestTheOtherCalled)
      assert(!s3.theTestThisConfigMapWasEmpty)
      assert(!s3.theTestThatConfigMapWasEmpty)
      assert(!s3.theTestTheOtherConfigMapWasEmpty)
    }

    it("should run all tests, passing in a non-empty config map, if a wildcard selecting all tests and a config map is passed") {
      val s4 = new TestWasCalledSpec
      s4.execute(" th", ConfigMap("s" -> "s"))
      assert(s4.theTestThisCalled)
      assert(s4.theTestThatCalled)
      assert(s4.theTestTheOtherCalled)
      assert(!s4.theTestThisConfigMapWasEmpty)
      assert(!s4.theTestThatConfigMapWasEmpty)
      assert(!s4.theTestTheOtherConfigMapWasEmpty)
    }

    it("should run all tests, passing in an empty config map, if a wildcard selecting all tests is passed with a named parameter") {
      val s5 = new TestWasCalledSpec
      s5.execute(testName = " th")
      assert(s5.theTestThisCalled)
      assert(s5.theTestThatCalled)
      assert(s5.theTestTheOtherCalled)
      assert(s5.theTestThisConfigMapWasEmpty)
      assert(s5.theTestThatConfigMapWasEmpty)
      assert(s5.theTestTheOtherConfigMapWasEmpty)
    }

    it("should run all tests, passing in a non-empty config map, if a wildcard selecting all tests and a config map are passed with named parameters") {
      val s6 = new TestWasCalledSpec
      s6.execute(testName = " this", configMap = ConfigMap("s" -> "s"))
      assert(s6.theTestThisCalled)
      assert(!s6.theTestThatCalled)
      assert(!s6.theTestTheOtherCalled)
      assert(!s6.theTestThisConfigMapWasEmpty)
      assert(s6.theTestThatConfigMapWasEmpty)
      assert(s6.theTestTheOtherConfigMapWasEmpty)
    }
  }

  it("should return the test names in alphabetical order from testNames even if the encoded name would sort in the opposite order") {

    // + comes before -
    // but $plus comes after $minus
    class ASpec extends RefSpec {

      def `test: the + operator should add`: Unit = {
        val sum = 1 + 1
        assert(sum === 2)
      }

      def `test: the - operator should subtract`: Unit = {
        val diff = 4 - 1
        assert(diff === 3)
      }
    }

    val a = new ASpec
    val expectedTestNames = List("" +
      "test: the + operator should add",
      "test: the - operator should subtract"
    )
    assert(a.testNames.iterator.toList === expectedTestNames)
  }

  def testTestTags(): Unit = {
    class TagSpec extends RefSpec {
      def testNoTagMethod(): Unit = {}
      @SlowAsMolasses
      def testTagMethod(): Unit = {}
    }
    val testTags = new TagSpec().tags
    assert(testTags.size === 1)
    val tagSet = testTags.getOrElse("testTagMethod", null)
    assert(tagSet != null)
    assert(tagSet.size === 1)
    assert(tagSet.toList(0) === classOf[SlowAsMolasses].getName)
  }

  describe("when annotations are applied at the class level") {
    it("should propate those annotations to all tests in the class") {

      class NoTagSpec extends RefSpec
      @Ignore
      class IgnoreSpec extends RefSpec {
        def `test method 1`: Unit = {}
        def `test method 2`: Unit = {}
        def `test method 3`: Unit = {}
      }
      @SlowAsMolasses
      class SlowAsMolassesSpec extends RefSpec
      @FastAsLight
      class FastAsLightSpec extends RefSpec

      class MasterSpec extends RefSpec {
        override def nestedSuites = Vector(new NoTagSpec(), new IgnoreSpec(), new SlowAsMolassesSpec(), new FastAsLightSpec())
        override def runNestedSuites(args: Args): Status = {
          super.runNestedSuites(args)
        }
      }

      class CounterDistributor extends Distributor {
        var count = 0
        def apply(suite: Suite, args: Args): Status = {
          count += 1
          SucceededStatus
        }
        def apply(suite: Suite, tracker: Tracker): Unit = {
          count += 1
        }
      }

      val masterSpec = new MasterSpec()

      val defaultFilter = Filter(None, Set.empty)
      val defaultReporter = new EventRecordingReporter
      masterSpec.runNestedSuites(Args(defaultReporter, Stopper.default, defaultFilter, ConfigMap.empty, None, new Tracker(new Ordinal(99))))
      assert(defaultReporter.suiteStartingEventsReceived.size === 4)
      assert(defaultReporter.testIgnoredEventsReceived.size === 3)
      val defaultReporterDist = new EventRecordingReporter
      val defaultDistributor = new CounterDistributor
      masterSpec.runNestedSuites(Args(defaultReporterDist, Stopper.default, defaultFilter, ConfigMap.empty, Some(defaultDistributor), new Tracker(new Ordinal(99))))
      assert(defaultDistributor.count === 4)

      val includeFilter = Filter(Some(Set("org.scalatest.FastAsLight")), Set.empty)
      val includeReporter = new EventRecordingReporter
      masterSpec.runNestedSuites(Args(includeReporter, Stopper.default, includeFilter, ConfigMap.empty, None, new Tracker(new Ordinal(99))))
      assert(includeReporter.suiteStartingEventsReceived.size === 4)
      assert(includeReporter.testIgnoredEventsReceived.size === 0)
      val includeReporterDist = new EventRecordingReporter
      val includeDistributor = new CounterDistributor
      masterSpec.runNestedSuites(Args(includeReporterDist, Stopper.default, includeFilter, ConfigMap.empty, Some(includeDistributor), new Tracker(new Ordinal(99))))
      assert(includeDistributor.count === 4)

      val excludeFilter = Filter(None, Set("org.scalatest.SlowAsMolasses"))
      val excludeReporter = new EventRecordingReporter
      masterSpec.runNestedSuites(Args(excludeReporter, Stopper.default, excludeFilter, ConfigMap.empty, None, new Tracker(new Ordinal(99))))
      assert(excludeReporter.suiteStartingEventsReceived.size === 4)
      assert(excludeReporter.testIgnoredEventsReceived.size === 3)
      val excludeReporterDist = new EventRecordingReporter
      val excludeDistributor = new CounterDistributor
      masterSpec.runNestedSuites(Args(excludeReporterDist, Stopper.default, excludeFilter, ConfigMap.empty, Some(excludeDistributor), new Tracker(new Ordinal(99))))
      assert(excludeDistributor.count === 4)
    }
  }

  describe("when its expectedTestCount method is invoked") {
    it("should return a count that takes into 'account' the passed filter") {
      class NoTagSpec extends RefSpec {
        def `test method 1`: Unit = {}
        def `test method 2`: Unit = {}
        def `test method 3`: Unit = {}
      }
      @Ignore
      class IgnoreSpec extends RefSpec {
        def `test method 1`: Unit = {}
        def `test method 2`: Unit = {}
        def `test method 3`: Unit = {}
      }
      @SlowAsMolasses
      class SlowAsMolassesSpec extends RefSpec {
        def `test method 1`: Unit = {}
        def `test method 2`: Unit = {}
        def `test method 3`: Unit = {}
      }
      @FastAsLight
      class FastAsLightSpec extends RefSpec {
        def `test method 1`: Unit = {}
        def `test method 2`: Unit = {}
        def `test method 3`: Unit = {}
      }

      class MasterSpec extends RefSpec {
        override def nestedSuites = Vector(new NoTagSpec(), new IgnoreSpec(), new SlowAsMolassesSpec(), new FastAsLightSpec())
        override def runNestedSuites(args: Args): Status = {
          super.runNestedSuites(args)
        }
      }

      val masterSpec = new MasterSpec()
      assert(masterSpec.expectedTestCount(Filter(None, Set.empty)) === 9)
      assert(masterSpec.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set.empty)) === 3)
      assert(masterSpec.expectedTestCount(Filter(None, Set("org.scalatest.FastAsLight"))) === 6)
      assert(masterSpec.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set.empty)) === 3)
      assert(masterSpec.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) === 6)
    }
  }

  describe("when its rerunner method is invoked") {
    it("should provide the fully qualified name of a class that could be used to rerun the suite, wrapped in a Some, or None, if can't be rerun") {
      assert(new NormalRefSpec().rerunner.get === classOf[NormalRefSpec].getName)
      assert(new WrappedRefSpec(Map.empty).rerunner.get === classOf[WrappedRefSpec].getName)
      assert(new NotAccessibleRefSpec("test").rerunner === None)
    }
  }

  describe("when a test fails") {
    it("should send proper stack depth information") {
      class TestSpec extends RefSpec {
        def `test failure`(): Unit = {
          assert(1 === 2)
        }
      }
      val rep = new EventRecordingReporter
      val s1 = new TestSpec
      s1.run(None, Args(rep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker))
      assert(rep.testFailedEventsReceived.size === 1)
      assert(rep.testFailedEventsReceived(0).throwable.get.asInstanceOf[TestFailedException].failedCodeFileName.get === "RefSpecSpec.scala")
      assert(rep.testFailedEventsReceived(0).throwable.get.asInstanceOf[TestFailedException].failedCodeLineNumber.get === thisLineNumber - 8)
    }

    it("should fire TestFailed event with correct stack depth info when test failed") {
      class TestSpec extends RefSpec {
        def `it should fail`: Unit = {
          assert(1 === 2)
        }
        object `A scenario` {
          def `should fail`: Unit = {
            assert(1 === 2)
          }
        }
      }
      val rep = new EventRecordingReporter
      val s1 = new TestSpec
      s1.run(None, Args(rep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker))
      assert(rep.testFailedEventsReceived.size === 2)
      // The 'A scenario should fail' will be execute first because tests are executed in alphanumerical order.
      assert(rep.testFailedEventsReceived(0).throwable.get.asInstanceOf[TestFailedException].failedCodeFileName.get === "RefSpecSpec.scala")
      assert(rep.testFailedEventsReceived(0).throwable.get.asInstanceOf[TestFailedException].failedCodeLineNumber.get === thisLineNumber - 10)
      assert(rep.testFailedEventsReceived(1).throwable.get.asInstanceOf[TestFailedException].failedCodeFileName.get === "RefSpecSpec.scala")
      assert(rep.testFailedEventsReceived(1).throwable.get.asInstanceOf[TestFailedException].failedCodeLineNumber.get === thisLineNumber - 16)
    }

    it("should generate NotAllowedException wrapping a TestFailedException when assert fails in scope") {
      class TestSpec extends RefSpec {
        object `a feature` {
          val a = 1
          assert(a == 2)
        }
      }
      val e = intercept[NotAllowedException] {
        val spec = new TestSpec
        val rep = new EventRecordingReporter
        spec.run(None, Args(rep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker))
      }
      assert("RefSpecSpec.scala" == e.failedCodeFileName.get)
      assert(e.failedCodeLineNumber.get == thisLineNumber - 9)
      assert(e.message == Some(FailureMessages.assertionShouldBePutInsideDefNotObject))

      assert(e.cause.isDefined)
      val causeThrowable = e.cause.get
      assert(causeThrowable.isInstanceOf[TestFailedException])
      val cause = causeThrowable.asInstanceOf[TestFailedException]
      assert("RefSpecSpec.scala" == cause.failedCodeFileName.get)
      assert(cause.failedCodeLineNumber.get == thisLineNumber - 17)
      assert(cause.message == Some(FailureMessages.didNotEqual(prettifier, 1, 2)))
    }

    it("should generate NotAllowedException wrapping a TestCanceledException when assume fails in scope") {
      class TestSpec extends RefSpec {
        object `a feature` {
          val a = 1
          assume(a == 2)
        }
      }
      val e = intercept[NotAllowedException] {
        val spec = new TestSpec
        val rep = new EventRecordingReporter
        spec.run(None, Args(rep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker))
      }
      assert("RefSpecSpec.scala" == e.failedCodeFileName.get)
      assert(e.failedCodeLineNumber.get == thisLineNumber - 9)
      assert(e.message == Some(FailureMessages.assertionShouldBePutInsideDefNotObject))

      assert(e.cause.isDefined)
      val causeThrowable = e.cause.get
      assert(causeThrowable.isInstanceOf[TestCanceledException])
      val cause = causeThrowable.asInstanceOf[TestCanceledException]
      assert("RefSpecSpec.scala" == cause.failedCodeFileName.get)
      assert(cause.failedCodeLineNumber.get == thisLineNumber - 17)
      assert(cause.message == Some(FailureMessages.didNotEqual(prettifier, 1, 2)))
    }

    it("should generate NotAllowedException wrapping a non-fatal RuntimeException is thrown inside scope") {
      class TestSpec extends RefSpec {
        object `a feature` {
          throw new RuntimeException("on purpose")
        }
      }
      val e = intercept[NotAllowedException] {
        val spec = new TestSpec
        val rep = new EventRecordingReporter
        spec.run(None, Args(rep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker))
      }
      assert("RefSpecSpec.scala" == e.failedCodeFileName.get)
      assert(e.failedCodeLineNumber.get == 2487)
      assert(e.cause.isDefined)
      val causeThrowable = e.cause.get
      assert(e.message == Some(FailureMessages.exceptionWasThrownInObject(prettifier, UnquotedString(causeThrowable.getClass.getName), UnquotedString("a feature"))))

      assert(causeThrowable.isInstanceOf[RuntimeException])
      val cause = causeThrowable.asInstanceOf[RuntimeException]
      assert(cause.getMessage == "on purpose")
    }

    it("should propagate AnnotationFormatError when it is thrown inside scope") {
      class TestSpec extends RefSpec {
        object `a feature` {
          throw new AnnotationFormatError("on purpose")
        }
      }
      val e = intercept[AnnotationFormatError] {
        val spec = new TestSpec
        val rep = new EventRecordingReporter
        spec.run(None, Args(rep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker))
      }
      assert(e.getMessage == "on purpose")
    }

    it("should propagate AWTError when it is thrown inside scope") {
      class TestSpec extends RefSpec {
        object `a feature` {
          throw new AWTError("on purpose")
        }
      }
      val e = intercept[AWTError] {
        val spec = new TestSpec
        val rep = new EventRecordingReporter
        spec.run(None, Args(rep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker))
      }
      assert(e.getMessage == "on purpose")
    }

    it("should propagate CoderMalfunctionError when it is thrown inside scope") {
      class TestSpec extends RefSpec {
        object `a feature` {
          throw new CoderMalfunctionError(new RuntimeException("on purpose"))
        }
      }
      val e = intercept[CoderMalfunctionError] {
        val spec = new TestSpec
        val rep = new EventRecordingReporter
        spec.run(None, Args(rep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker))
      }
      assert(e.getMessage == "java.lang.RuntimeException: on purpose")
    }

    it("should propagate FactoryConfigurationError when it is thrown inside scope") {
      class TestSpec extends RefSpec {
        object `a feature` {
          throw new FactoryConfigurationError("on purpose")
        }
      }
      val e = intercept[FactoryConfigurationError] {
        val spec = new TestSpec
        val rep = new EventRecordingReporter
        spec.run(None, Args(rep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker))
      }
      assert(e.getMessage == "on purpose")
    }

    it("should propagate LinkageError when it is thrown inside scope") {
      class TestSpec extends RefSpec {
        object `a feature` {
          throw new LinkageError("on purpose")
        }
      }
      val e = intercept[LinkageError] {
        val spec = new TestSpec
        val rep = new EventRecordingReporter
        spec.run(None, Args(rep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker))
      }
      assert(e.getMessage == "on purpose")
    }

    it("should propagate ThreadDeath when it is thrown inside scope") {
      class TestSpec extends RefSpec {
        object `a feature` {
          throw new ThreadDeath
        }
      }
      val e = intercept[ThreadDeath] {
        val spec = new TestSpec
        val rep = new EventRecordingReporter
        spec.run(None, Args(rep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker))
      }
      assert(e.getMessage == null)
    }

    it("should propagate TransformerFactoryConfigurationError when it is thrown inside scope") {
      class TestSpec extends RefSpec {
        object `a feature` {
          throw new TransformerFactoryConfigurationError("on purpose")
        }
      }
      val e = intercept[TransformerFactoryConfigurationError] {
        val spec = new TestSpec
        val rep = new EventRecordingReporter
        spec.run(None, Args(rep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker))
      }
      assert(e.getMessage == "on purpose")
    }

    it("should propagate VirtualMachineError when it is thrown inside scope") {
      class TestSpec extends RefSpec {
        object `a feature` {
          throw new VirtualMachineError("on purpose") {}
        }
      }
      val e = intercept[VirtualMachineError] {
        val spec = new TestSpec
        val rep = new EventRecordingReporter
        spec.run(None, Args(rep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker))
      }
      assert(e.getMessage == "on purpose")
    }
  }
}

@DoNotDiscover
class `MyRefSpec` extends RefSpec {}
@DoNotDiscover
class NormalRefSpec extends RefSpec
@DoNotDiscover
@WrapWith(classOf[ConfigMapWrapperSuite])
class WrappedRefSpec(configMap: Map[_, _]) extends RefSpec
@DoNotDiscover
class NotAccessibleRefSpec(name: String) extends RefSpec
