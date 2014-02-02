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

import scala.reflect.NameTransformer.encode
import org.scalatest.events._
import org.scalatest.exceptions._
import collection.immutable.TreeSet
import org.scalatest.Suite._
import org.scalatest.{ PrivateMethodTester, ShouldMatchers, BeforeAndAfterEach, BeforeAndAfterAll, 
                        Filter, Args, Stopper, Tracker, Ignore, SlowAsMolasses, FastAsLight, WeakAsAKitten, Specs, 
                        Reporter, Distributor, OptionValues, NotAllowedException, Resources, DoNotDiscover, WrapWith, 
                        ConfigMapWrapperSuite, StringFixture, Status, SucceededStatus, ConfigMap, Outcome }
import org.scalatest.SharedHelpers._
import org.scalatest.tools.Runner.CHOSEN_STYLES

class SpecSpec extends org.scalatest.FunSpec with PrivateMethodTester {

  describe("A fixture.Spec") {
    /*
    it("should send InfoProvided events with aboutAPendingTest set to true and aboutACanceledTest set to false for info " +
            "calls made from a test that is pending") {
      val a = new Spec {
        def `test: something`(r: Rep) {
          r.info("two integers")
          r.info("one is subracted from the other")
          r.info("the result is the difference between the two numbers")
          pending
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep, new Stopper {}, Filter(), ConfigMap.empty, None, new Tracker(), Set.empty))
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
      val a = new Spec {
        def `test: something`(r: Rep) {
          r.info("two integers")
          r.info("one is subracted from the other")
          r.info("the result is the difference between the two numbers")
          assert(1 + 1 === 2)
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep, new Stopper {}, Filter(), ConfigMap.empty, None, new Tracker(), Set.empty))
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
      val a = new Spec {
        def `test: something`(r: Rep) {
          r.info("two integers")
          r.info("one is subracted from the other")
          r.info("the result is the difference between the two numbers")
          cancel()
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep, new Stopper {}, Filter(), ConfigMap.empty, None, new Tracker(), Set.empty))
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
      val a = new Spec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("A Fixture") }
        def `it should do this`(fixture: String) {}
        def `it should do that`(fixture: String) {}
      }

      assertResult(List("it should do that", "it should do this")) {
        a.testNames.iterator.toList
      }

      val b = new Spec with StringFixture { }

      assertResult(List[String]()) {
        b.testNames.iterator.toList
      }

      val c = new Spec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("A Fixture") }
        def `test: that`(fixture: String) {}
        def `test: this`(fixture: String) {}
      }

      assertResult(List("test: that", "test: this")) {
        c.testNames.iterator.toList
      }
    }
    
    it("should return test names nested in scope in alpahbetical order from testNames") {
      val a = new Spec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("A Fixture") }
        object `A Tester` {
          def `should test that`(fixture: String) {}
          def `should test this`(fixture: String) {}
        }
      }

      assertResult(List("A Tester should test that", "A Tester should test this")) {
        a.testNames.iterator.toList
      }

      val b = new Spec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("A Fixture") }
        object `A Tester` {
          object `should be able to` {
            def `test this`(fixture: String) {}
            def `test that`(fixture: String) {}
          }
          object `must be able to` {
            def `test this`(fixture: String) {}
            def `test that`(fixture: String) {}
          }
        }
      }

      assertResult(List("A Tester must be able to test that", "A Tester must be able to test this", "A Tester should be able to test that", "A Tester should be able to test this")) {
        b.testNames.iterator.toList
      }
    }
    
    it("test names should properly nest scopes in test names") {
      class MySpec extends Spec with ShouldMatchers {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("A Fixture") }
        object `A Stack` {
          object `(when not empty)` {
            def `must allow me to pop`(fixture: String) {}
          }
          object `(when not full)` {
            def `must allow me to push`(fixture: String) {}
          }
        }
      }
      val a = new MySpec
      assert(a.testNames.size === 2)
      assert(a.testNames.iterator.toList(0) === "A Stack (when not empty) must allow me to pop")
      assert(a.testNames.iterator.toList(1) === "A Stack (when not full) must allow me to push")
    }
    
    it("should be able to mix in BeforeAndAfterEach with BeforeAndAfterAll without any problems") {
      class MySpec extends Spec with ShouldMatchers with BeforeAndAfterEach with BeforeAndAfterAll {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("A Fixture") }
        object `A Stack` {
          object `(when not empty)` {
            def `should allow me to pop`(fixture: String) {}
          }
          object `(when not full)` {
            def `should allow me to push`(fixture: String) {}
          }
        }
      }
      val a = new MySpec
      a.execute()
    }
    
    it("should register scopes and tests lazily after spec instance variables are created when testNames is invoked") {
      val a = new Spec with StringFixture {
        val name = "ScalaTest"
        object `In Scope: ` {
          assert(name === "ScalaTest")
        }
      }
      a.testNames // Should execute assertion in the scope
    }
    it("should register scopes and tests lazily after spec instance variables are created when run is invoked") {
      val a = new Spec with StringFixture {
        val name = "ScalaTest"
        object `In Scope: ` {
          assert(name === "ScalaTest")
        }
      }
      a.run(None, Args(SilentReporter)) // Should execute assertion in the scope
    }
    it("should register scopes and tests lazily after spec instance variables are created when expectedTestCount is invoked") {
      val a = new Spec with StringFixture {
        val name = "ScalaTest"
        object `In Scope: ` {
          assert(name === "ScalaTest")
        }
      }
      a.expectedTestCount(Filter.default) // Should execute assertion in the scope
    }
    it("should register scopes and tests lazily after spec instance variables are created when tags is invoked") {
      val a = new Spec with StringFixture {
        val name = "ScalaTest"
        object `In Scope: ` {
          assert(name === "ScalaTest")
        }
      }
      a.tags // Should execute assertion in the scope
    }

/*
    it("should register scopes and tests lazily after spec instance variables are created") {
      val a = new Spec {
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
 
    class TestWasCalledSpec extends Spec with StringFixture {
      var theTestThisCalled = false
      var theTestThatCalled = false
      def `test: this`(fixture: String) { theTestThisCalled = true }
      def `test: that`(fixture: String) { theTestThatCalled = true }
    }
    
    it("should execute all tests when run is called with testName None") {

      val b = new TestWasCalledSpec
      b.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(b.theTestThisCalled)
      assert(b.theTestThatCalled)
    }
    
    it("should execute one test when run is called with a defined testName") {

      val a = new TestWasCalledSpec
      a.run(Some("test: this"), Args(SilentReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(a.theTestThisCalled)
      assert(!a.theTestThatCalled)
    }
    
    it("should report as ignored, and not run, tests marked ignored") {

      val a = new Spec with StringFixture {
        var theTestThisCalled = false
        var theTestThatCalled = false
        def `test: this`(fixture: String) { theTestThisCalled = true }
        def `test: that`(fixture: String) { theTestThatCalled = true }
      }

      import scala.language.reflectiveCalls

      val repA = new TestIgnoredTrackingReporter
      a.run(None, Args(repA, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repA.testIgnoredReceived)
      assert(a.theTestThisCalled)
      assert(a.theTestThatCalled)

      val b = new Spec with StringFixture {
        var theTestThisCalled = false
        var theTestThatCalled = false
        @Ignore
        def `test: this`(fixture: String) { theTestThisCalled = true }
        def `test: that`(fixture: String) { theTestThatCalled = true }
      }

      val repB = new TestIgnoredTrackingReporter
      b.run(None, Args(repB, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(repB.testIgnoredReceived)
      assert(repB.lastEvent.isDefined)
      assert(repB.lastEvent.get.testName endsWith "test: this")
      assert(!b.theTestThisCalled)
      assert(b.theTestThatCalled)

      val c = new Spec with StringFixture {
        var theTestThisCalled = false
        var theTestThatCalled = false
        def `test: this`(fixture: String) { theTestThisCalled = true }
        @Ignore
        def `test: that`(fixture: String) { theTestThatCalled = true }
      }

      val repC = new TestIgnoredTrackingReporter
      c.run(None, Args(repC, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(repC.testIgnoredReceived)
      assert(repC.lastEvent.isDefined)
      assert(repC.lastEvent.get.testName endsWith "test: that", repC.lastEvent.get.testName)
      assert(c.theTestThisCalled)
      assert(!c.theTestThatCalled)

      val d = new Spec with StringFixture {
        var theTestThisCalled = false
        var theTestThatCalled = false
        @Ignore
        def `test: this`(fixture: String) { theTestThisCalled = true }
        @Ignore
        def `test: that`(fixture: String) { theTestThatCalled = true }
      }

      val repD = new TestIgnoredTrackingReporter
      d.run(None, Args(repD, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(repD.testIgnoredReceived)
      assert(repD.lastEvent.isDefined)
      assert(repD.lastEvent.get.testName === "test: this") // last because run alphabetically
      assert(!d.theTestThisCalled)
      assert(!d.theTestThatCalled)
    }
    
    it("should ignore a test marked as ignored if run is invoked with that testName") {

      val e = new Spec with StringFixture {
        var theTestThisCalled = false
        var theTestThatCalled = false
        @Ignore
        def `test: this`(fixture: String) { theTestThisCalled = true }
        def `test: that`(fixture: String) { theTestThatCalled = true }
      }

      import scala.language.reflectiveCalls

      val repE = new TestIgnoredTrackingReporter
      e.run(Some("test: this"), Args(repE, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(repE.testIgnoredReceived)
      assert(!e.theTestThisCalled)
      assert(!e.theTestThatCalled)
    }
    
    it("should exclude a test with a tag included in the tagsToExclude set even if run is invoked with that testName") {

      val e = new Spec with StringFixture {
        var theTestThisCalled = false
        var theTestThatCalled = false
        @SlowAsMolasses
        def `test: this`(fixture: String) { theTestThisCalled = true }
        def `test: that`(fixture: String) { theTestThatCalled = true }
      }

      import scala.language.reflectiveCalls

      val repE = new TestIgnoredTrackingReporter
      e.run(Some("test: this"), Args(repE, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repE.testIgnoredReceived)
      assert(!e.theTestThisCalled)
      assert(!e.theTestThatCalled)
    }
    
    it("should run only those tests selected by the tags to include and exclude sets") {

      // Nothing is excluded
      val a = new Spec with StringFixture {
        var theTestThisCalled = false
        var theTestThatCalled = false
        @SlowAsMolasses
        def `test this`(fixture: String) { theTestThisCalled = true }
        def `test that`(fixture: String) { theTestThatCalled = true }
      }

      import scala.language.reflectiveCalls

      val repA = new TestIgnoredTrackingReporter
      a.run(None, Args(repA, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repA.testIgnoredReceived)
      assert(a.theTestThisCalled)
      assert(a.theTestThatCalled)

      // SlowAsMolasses is included, one test should be excluded
      val b = new Spec with StringFixture {
        var theTestThisCalled = false
        var theTestThatCalled = false
        @SlowAsMolasses
        def `test this`(fixture: String) { theTestThisCalled = true }
        def `test that`(fixture: String) { theTestThatCalled = true }
      }
      val repB = new TestIgnoredTrackingReporter
      b.run(None, Args(repB, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repB.testIgnoredReceived)
      assert(b.theTestThisCalled)
      assert(!b.theTestThatCalled)

      // SlowAsMolasses is included, and both tests should be included
      val c = new Spec with StringFixture {
        var theTestThisCalled = false
        var theTestThatCalled = false
        @SlowAsMolasses
        def `test this`(fixture: String) { theTestThisCalled = true }
        @SlowAsMolasses
        def `test that`(fixture: String) { theTestThatCalled = true }
      }
      val repC = new TestIgnoredTrackingReporter
      c.run(None, Args(repB, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repC.testIgnoredReceived)
      assert(c.theTestThisCalled)
      assert(c.theTestThatCalled)

      // SlowAsMolasses is included. both tests should be included but one ignored
      val d = new Spec with StringFixture {
        var theTestThisCalled = false
        var theTestThatCalled = false
        @Ignore
        @SlowAsMolasses
        def `test this`(fixture: String) { theTestThisCalled = true }
        @SlowAsMolasses
        def `test that`(fixture: String) { theTestThatCalled = true }
      }
      val repD = new TestIgnoredTrackingReporter
      d.run(None, Args(repD, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.Ignore")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(repD.testIgnoredReceived)
      assert(!d.theTestThisCalled)
      assert(d.theTestThatCalled)

      // SlowAsMolasses included, FastAsLight excluded
      val e = new Spec with StringFixture {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        @SlowAsMolasses
        @FastAsLight
        def `test this`(fixture: String) { theTestThisCalled = true }
        @SlowAsMolasses
        def `test that`(fixture: String) { theTestThatCalled = true }
        def `test the other`(fixture: String) { theTestTheOtherCalled = true }
      }
      val repE = new TestIgnoredTrackingReporter
      e.run(None, Args(repE, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
                ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repE.testIgnoredReceived)
      assert(!e.theTestThisCalled)
      assert(e.theTestThatCalled)
      assert(!e.theTestTheOtherCalled)

      // An Ignored test that was both included and excluded should not generate a TestIgnored event
      val f = new Spec with StringFixture {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        @Ignore
        @SlowAsMolasses
        @FastAsLight
        def `test this`(fixture: String) { theTestThisCalled = true }
        @SlowAsMolasses
        def `test that`(fixture: String) { theTestThatCalled = true }
        def `test the other`(fixture: String) { theTestTheOtherCalled = true }
      }
      val repF = new TestIgnoredTrackingReporter
      f.run(None, Args(repF, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
                ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repF.testIgnoredReceived)
      assert(!f.theTestThisCalled)
      assert(f.theTestThatCalled)
      assert(!f.theTestTheOtherCalled)

      // An Ignored test that was not included should not generate a TestIgnored event
      val g = new Spec with StringFixture {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        @SlowAsMolasses
        @FastAsLight
        def `test this`(fixture: String) { theTestThisCalled = true }
        @SlowAsMolasses
        def `test that`(fixture: String) { theTestThatCalled = true }
        @Ignore
        def `test the other`(fixture: String) { theTestTheOtherCalled = true }
      }
      val repG = new TestIgnoredTrackingReporter
      g.run(None, Args(repG, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
                ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repG.testIgnoredReceived)
      assert(!g.theTestThisCalled)
      assert(g.theTestThatCalled)
      assert(!g.theTestTheOtherCalled)

      // No tagsToInclude set, FastAsLight excluded
      val h = new Spec with StringFixture {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        @SlowAsMolasses
        @FastAsLight
        def `test this`(fixture: String) { theTestThisCalled = true }
        @SlowAsMolasses
        def `test that`(fixture: String) { theTestThatCalled = true }
        def `test the other`(fixture: String) { theTestTheOtherCalled = true }
      }
      val repH = new TestIgnoredTrackingReporter
      h.run(None, Args(repH, Stopper.default, Filter(None, Set("org.scalatest.FastAsLight")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repH.testIgnoredReceived)
      assert(!h.theTestThisCalled)
      assert(h.theTestThatCalled)
      assert(h.theTestTheOtherCalled)

      // No tagsToInclude set, SlowAsMolasses excluded
      val i = new Spec with StringFixture {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        @SlowAsMolasses
        @FastAsLight
        def `test this`(fixture: String) { theTestThisCalled = true }
        @SlowAsMolasses
        def `test that`(fixture: String) { theTestThatCalled = true }
        def `test the other`(fixture: String) { theTestTheOtherCalled = true }
      }
      val repI = new TestIgnoredTrackingReporter
      i.run(None, Args(repI, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repI.testIgnoredReceived)
      assert(!i.theTestThisCalled)
      assert(!i.theTestThatCalled)
      assert(i.theTestTheOtherCalled)

      // No tagsToInclude set, SlowAsMolasses excluded, TestIgnored should not be received on excluded ones
      val j = new Spec with StringFixture {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        @Ignore
        @SlowAsMolasses
        @FastAsLight
        def `test this`(fixture: String) { theTestThisCalled = true }
        @Ignore
        @SlowAsMolasses
        def `test that`(fixture: String) { theTestThatCalled = true }
        def `test the other`(fixture: String) { theTestTheOtherCalled = true }
      }
      val repJ = new TestIgnoredTrackingReporter
      j.run(None, Args(repJ, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repI.testIgnoredReceived)
      assert(!j.theTestThisCalled)
      assert(!j.theTestThatCalled)
      assert(j.theTestTheOtherCalled)

      // Same as previous, except Ignore specifically mentioned in excludes set
      val k = new Spec with StringFixture {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        @Ignore
        @SlowAsMolasses
        @FastAsLight
        def `test this`(fixture: String) { theTestThisCalled = true }
        @Ignore
        @SlowAsMolasses
        def `test that`(fixture: String) { theTestThatCalled = true }
        @Ignore
        def `test the other`(fixture: String) { theTestTheOtherCalled = true }
      }
      val repK = new TestIgnoredTrackingReporter
      k.run(None, Args(repK, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses", "org.scalatest.Ignore")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(repK.testIgnoredReceived)
      assert(!k.theTestThisCalled)
      assert(!k.theTestThatCalled)
      assert(!k.theTestTheOtherCalled)
    }
    
    it("should return a correct tags map from the tags method") {

      val a = new Spec with StringFixture {
        object `This Spec should` {
          @Ignore
          def `test this`(fixture: String) {}
          def `test that`(fixture: String) { pending }
        }
      }
      assertResult(Map("This Spec should test this" -> Set("org.scalatest.Ignore"))) {
        a.tags
      }

      val b = new Spec with StringFixture {
        object `This Spec should` {
          def `test this`(fixture: String) { pending }
          @Ignore
          def `test that`(fixture: String) {}
        }
      }
      assertResult(Map("This Spec should test that" -> Set("org.scalatest.Ignore"))) {
        b.tags
      }

      val c = new Spec with StringFixture {
        object `This Spec should` {
          @Ignore
          def `test this`(fixture: String) {}
          @Ignore
          def `test that`(fixture: String) {}
        }
      }
      assertResult(Map("This Spec should test this" -> Set("org.scalatest.Ignore"), "This Spec should test that" -> Set("org.scalatest.Ignore"))) {
        c.tags
      }

      val d = new Spec with StringFixture {
        object `This Spec should` {
          @SlowAsMolasses
          def `test this`(fixture: String) { pending }
          @SlowAsMolasses
          @Ignore
          def `test that`(fixture: String) {}
        }
      }
      assertResult(Map("This Spec should test this" -> Set("org.scalatest.SlowAsMolasses"), "This Spec should test that" -> Set("org.scalatest.Ignore", "org.scalatest.SlowAsMolasses"))) {
        d.tags
      }

      val e = new Spec with StringFixture {
        object `This Spec should` {
          def `test this`(fixture: String) { pending }
          def `test that`(fixture: String) { pending }
        }
      }
      assertResult(Map()) {
        e.tags
      }

      val f = new Spec with StringFixture {
        object `This Spec should` {
          @SlowAsMolasses
          @WeakAsAKitten
          def `test this`(fixture: String) { pending }
          @SlowAsMolasses
          def `test that`(fixture: String) {}
        }
      }
      assertResult(Map("This Spec should test this" -> Set("org.scalatest.SlowAsMolasses", "org.scalatest.WeakAsAKitten"), "This Spec should test that" -> Set("org.scalatest.SlowAsMolasses"))) {
        f.tags
      }

      val g = new Spec with StringFixture {
        object `This Spec should` {
          @SlowAsMolasses
          @WeakAsAKitten
          def `test this`(fixture: String) { pending }
          @SlowAsMolasses
          def `test that`(fixture: String) {}
        }
      }
      assertResult(Map("This Spec should test this" -> Set("org.scalatest.SlowAsMolasses", "org.scalatest.WeakAsAKitten"), "This Spec should test that" -> Set("org.scalatest.SlowAsMolasses"))) {
        g.tags
      }
    }
    
    it("should throw IllegalArgumentException if run is passed a testName that does not exist") {
      val spec = new Spec with StringFixture {
        var theTestThisCalled = false
        var theTestThatCalled = false
        def `test: this`(fixture: String) { theTestThisCalled = true }
        def `test: that`(fixture: String) { theTestThatCalled = true }
      }

      intercept[IllegalArgumentException] {
        // Here, they forgot that the name is actually `test: this`(Fixture)
        spec.run(Some(encode("test: misspelled")), Args(SilentReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
      }
    }
    
    it("should return the correct test count from its expectedTestCount method") {

      val a = new Spec with StringFixture {
        def `test: this`(fixture: String) = ()
        def `test: that`(fixture: String) = ()
      }
      assert(a.expectedTestCount(Filter()) === 2)

      val b = new Spec with StringFixture {
        @Ignore
        def `test: this`(fixture: String) = ()
        def `test: that`(fixture: String) = ()
      }
      assert(b.expectedTestCount(Filter()) === 1)

      val c = new Spec with StringFixture {
        @FastAsLight
        def `test: this`(fixture: String) = ()
        def `test: that`(fixture: String) = ()
      }
      assert(c.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 1)
      assert(c.expectedTestCount(Filter(None, Set("org.scalatest.FastAsLight"))) === 1)

      val d = new Spec with StringFixture {
        @FastAsLight
        @SlowAsMolasses
        def `test: this`(fixture: String) = ()
        @SlowAsMolasses
        def `test: that`(fixture: String) = ()
        def `test: the other thing`(fixture: String) = ()
      }
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 1)
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight"))) === 1)
      assert(d.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) === 1)
      assert(d.expectedTestCount(Filter()) === 3)

      val e = new Spec with StringFixture {
        @FastAsLight
        @SlowAsMolasses
        def `test: this`(fixture: String) = ()
        @SlowAsMolasses
        def `test: that`(fixture: String) = ()
        @Ignore
        def `test: the other thing`(fixture: String) = ()
      }
      assert(e.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 1)
      assert(e.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight"))) === 1)
      assert(e.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) === 0)
      assert(e.expectedTestCount(Filter()) === 2)

      val f = new Specs(a, b, c, d, e)
      assert(f.expectedTestCount(Filter()) === 10)
    }
    
    it("should send an InfoProvided event for an info") {
      class MySuite extends Spec with StringFixture {
        info(
          "hi there"
        )
      }
      val suite = new MySuite
      val reporter = new EventRecordingReporter
      suite.run(None, Args(reporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))

      val infoList = reporter.infoProvidedEventsReceived

      assert(infoList.size === 1)
      assert(infoList(0).message === "hi there")
    }
    
    it("should generate a TestPending message when the test body is (pending)") {
      val a = new Spec with StringFixture {

        def `test: do this`(fixture: String) { pending }

        def `test: do that`(fixture: String) {
          assert(2 + 2 === 4)
        }

        def `test: do something else`(fixture: String) {
          assert(2 + 2 === 4)
          pending
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker(), Set.empty))
      val tp = rep.testPendingEventsReceived
      assert(tp.size === 2)
    }
    
    it("should generate a TestCanceled message when the test body includes a cancel call") {
      val a = new Spec with StringFixture {

        def `test: do this`(fixture: String) { cancel() }

        def `test: do that`(fixture: String) {
          assert(2 + 2 === 4)
        }

        def `test: do something else`(fixture: String) {
          assert(2 + 2 === 4)
          cancel()
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker(), Set.empty))
      val tp = rep.testCanceledEventsReceived
      assert(tp.size === 2)
    }
    
    it("should generate a TestCanceled message when the test body includes a failed assume call") {
      val a = new Spec with StringFixture {

        def `test: do this`(fixture: String) { assume(1 === 2) }

        def `test: do that`(fixture: String) {
          assert(2 + 2 === 4)
        }

        def `test: do something else`(fixture: String) {
          assert(2 + 2 === 4)
          assume(3 === 4)
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker(), Set.empty))
      val tp = rep.testCanceledEventsReceived
      assert(tp.size === 2)
    }
    
    it("should generate a test failure if a Throwable, or an Error other than direct Error subtypes " +
            "known in JDK 1.5, excluding AssertionError") {
      val a = new Spec with StringFixture {
        def `test: throws AssertionError`(fixture: String) { throw new AssertionError }
        def `test: throws plain old Error`(fixture: String) { throw new Error }
        def `test: throws Throwable`(fixture: String) { throw new Throwable }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker(), Set.empty))
      val tf = rep.testFailedEventsReceived
      assert(tf.size === 3)
    }
    
    it("should propagate out Errors that are direct subtypes of Error in JDK 1.5, other than " +
            "AssertionError, causing Suites and Runs to abort.") {
      val a = new Spec with StringFixture {
        def `test: throws AssertionError`(fixture: String) { throw new OutOfMemoryError }
      }
      intercept[OutOfMemoryError] {
        a.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker(), Set.empty))
      }
    }
    
    it("should invoke withFixture from runTest for no-arg test method") {
      val a = new Spec with StringFixture {
        var withFixtureWasInvoked = false
        var theTestWasInvoked = false
        override def withFixture(test: OneArgTest): Outcome = {
          withFixtureWasInvoked = true
          super.withFixture(test)
        }
        def `test: something`(fixture: String) {
          theTestWasInvoked = true
        }
      }

      import scala.language.reflectiveCalls

      a.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker(), Set.empty))
      assert(a.withFixtureWasInvoked)
      assert(a.theTestWasInvoked)
    }
    
    it("should pass the correct test name in the OneArgTest passed to withFixture") {
      val a = new Spec with StringFixture {
        var correctTestNameWasPassed = false
        override def withFixture(test: OneArgTest): Outcome = {
          correctTestNameWasPassed = test.name == "test: something"
          super.withFixture(test)
        }
        def `test: something`(fixture: String) {}
      }

      import scala.language.reflectiveCalls

      a.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker(), Set.empty))
      assert(a.correctTestNameWasPassed)
    }

    it("should pass the correct config map in the OneArgTest passed to withFixture") {
      val a = new Spec with StringFixture {
        var correctConfigMapWasPassed = false
        override def withFixture(test: OneArgTest): Outcome = {
          correctConfigMapWasPassed = (test.configMap == ConfigMap("hi" -> 7))
          super.withFixture(test)
        }
        def `test: something`(fixture: String) {}
      }

      import scala.language.reflectiveCalls

      a.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap("hi" -> 7), None, new Tracker(), Set.empty))
      assert(a.correctConfigMapWasPassed)
    }
    
    it("should, when a test method writes to the Informer, report the info in test completion event") {
      val msg = "hi there dude"
      class MySpec extends Spec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test(msg) }
        def `test: with Informer`(fixture: String) {
          info(fixture)
        }
      }
      val myRep = new EventRecordingReporter
      new MySpec().run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
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
        def apply(event: Event) {
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
      class MySpec extends Spec with ShouldMatchers with StringFixture {
        def `must start with proper words`(fixture: String) {}
      }
      val a = new MySpec
      a.run(None, Args(new MyReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(reportHadCorrectTestName)
      assert(reportHadCorrectSpecText)
      assert(reportHadCorrectFormattedSpecText)
    }
    
    it("Top-level plain-old specifiers should yield good strings in a testSucceeded report") {
      var reportHadCorrectTestName = false
      var reportHadCorrectSpecText = false
      var reportHadCorrectFormattedSpecText = false
      class MyReporter extends Reporter {
        def apply(event: Event) {
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
      class MySpec extends Spec with ShouldMatchers with StringFixture {
        def `must start with proper words`(fixture: String) {}
      }
      val a = new MySpec
      a.run(None, Args(new MyReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(reportHadCorrectTestName)
      assert(reportHadCorrectSpecText)
      assert(reportHadCorrectFormattedSpecText)
    }
    
    it("Top-level plain-old specifiers should yield good strings in a testFailed report") {
      var reportHadCorrectTestName = false
      var reportHadCorrectSpecText = false
      var reportHadCorrectFormattedSpecText = false
      class MyReporter extends Reporter {
        def apply(event: Event) {
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
      class MySpec extends Spec with ShouldMatchers with StringFixture {
        def `must start with proper words`(fixture: String) { fail() }
      }
      val a = new MySpec
      a.run(None, Args(new MyReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
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
        def apply(event: Event) {
          event match {
            case ScopeOpened(ordinal, message, nameInfo, formatter, location, payload, threadName, timeStamp) =>
              // scopeOpened should be invoked before the other method
              assert(!theOtherMethodHasBeenInvoked)
              scopeOpenedHasBeenInvoked = true
              if (message.indexOf("My Spec") != -1)
                infoReportHadCorrectTestName = true
              formatter match {
                case Some(IndentedText(formattedText, rawText, indentationLevel)) =>
                  if (rawText == "My Spec")
                    infoReportHadCorrectSpecText = true
                  if (formattedText == "My Spec")
                    infoReportHadCorrectFormattedSpecText = true
                case _ =>
              }
            case TestSucceeded(ordinal, suiteName, suiteId, suiteClassName, testName, testText, testEvents, duration, formatter, location, rerunnable, payload, threadName, timeStamp) =>
              // scopeOpened should be invoked before the this method
              assert(scopeOpenedHasBeenInvoked)
              theOtherMethodHasBeenInvoked = true
              if (testName.indexOf("My Spec must start with proper words") != -1)
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
      class MySpec extends Spec with ShouldMatchers with StringFixture {
        object `My Spec` {
          def `must start with proper words`(fixture: String) {}
        }
      }
      val a = new MySpec
      a.run(None, Args(new MyReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
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
        def apply(event: Event) {
          event match {
            case ScopeOpened(ordinal, message, nameInfo, formatter, location, payload, threadName, timeStamp) =>
              // scopeOpened should be invoked before the other method
              assert(!theOtherMethodHasBeenInvoked)
              scopeOpenedHasBeenInvoked = true
              if (message.indexOf("My Spec") != -1)
                infoReportHadCorrectTestName = true
              formatter match {
                case Some(IndentedText(formattedText, rawText, indentationLevel)) =>
                  if (rawText == "My Spec")
                    infoReportHadCorrectSpecText = true
                  if (formattedText == "My Spec")
                    infoReportHadCorrectFormattedSpecText = true
                case _ =>
              }
            case TestSucceeded(ordinal, suiteName, suiteId, suiteClassName, testName, testText, testEvents, duration, formatter, location, rerunnable, payload, threadName, timeStamp) =>
              // scopeOpened should be invoked before the this method
              assert(scopeOpenedHasBeenInvoked)
              theOtherMethodHasBeenInvoked = true
              if (testName.indexOf("My Spec must start with proper words") != -1)
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
      class MySpec extends Spec with ShouldMatchers with StringFixture {
        object `My Spec` {
          def `must start with proper words`(fixture: String) {}
        }
      }
      val a = new MySpec
      a.run(None, Args(new MyReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
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
        def apply(event: Event) {
          event match {
            case ScopeOpened(ordinal, message, nameInfo, formatter, location, payload, threadName, timeStamp) =>
              // scopeOpened should be invoked before the other method
              assert(!theOtherMethodHasBeenInvoked)
              scopeOpenedHasBeenInvoked = true
              if (message.indexOf("My Spec") != -1)
                infoReportHadCorrectTestName = true
              formatter match {
                case Some(IndentedText(formattedText, rawText, indentationLevel)) =>
                  if (rawText == "My Spec")
                    infoReportHadCorrectSpecText = true
                  if (formattedText == "My Spec")
                    infoReportHadCorrectFormattedSpecText = true
                case _ =>
              }
            case event: TestFailed =>
              // scopeOpened should be invoked before the this method
              assert(scopeOpenedHasBeenInvoked)
              theOtherMethodHasBeenInvoked = true
              if (event.testName.indexOf("My Spec must start with proper words") != -1)
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
      class MySpec extends Spec with ShouldMatchers with StringFixture {
        object`My Spec` {
          def `must start with proper words`(fixture: String) { fail() }
        }
      }
      val a = new MySpec
      a.run(None, Args(new MyReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
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
        def apply(event: Event) {
          event match {
            case ScopeOpened(ordinal, message, nameInfo, formatter, location, payload, threadName, timeStamp) =>
              // scopeOpened should be invoked before the other method
              assert(!theOtherMethodHasBeenInvoked)
              if (!scopeOpenedHasBeenInvokedOnce) { 
                scopeOpenedHasBeenInvokedOnce = true
                if (message.indexOf("My Spec") >= 0)
                  infoReportHadCorrectTestName = true
                formatter match {
                  case Some(IndentedText(formattedText, rawText, indentationLevel)) =>
                    if (rawText == "My Spec")
                      infoReportHadCorrectSpecText = true
                    if (formattedText == "My Spec")
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
              if (testName.indexOf("My Spec must start with proper words") != -1)
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
      class MySpec extends Spec with ShouldMatchers with StringFixture {
        object `My Spec` {
          object `must start` {
            def `with proper words`(fixture: String) {}
          }
        }
      }
      val a = new MySpec
      a.run(None, Args(new MyReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
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
        def apply(event: Event) {
          event match {
            case ScopeOpened(ordinal, message, nameInfo, formatter, location, payload, threadName, timeStamp) =>
              // scopeOpened should be invoked before the other method
              assert(!theOtherMethodHasBeenInvoked)
              if (!scopeOpenedHasBeenInvokedOnce) { 
                scopeOpenedHasBeenInvokedOnce = true
                if (message.indexOf("My Spec") >= 0)
                  infoReportHadCorrectTestName = true
                formatter match {
                  case Some(IndentedText(formattedText, rawText, indentationLevel)) =>
                    if (rawText == "My Spec")
                      infoReportHadCorrectSpecText = true
                    if (formattedText == "My Spec")
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
              if (event.testName.indexOf("My Spec must start with proper words") != -1)
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
      class MySpec extends Spec with ShouldMatchers with StringFixture {
        object `My Spec` {
          object `must start` {
            def `with proper words`(fixture: String) { fail() }
          }
        }
      }
      val a = new MySpec
      a.run(None, Args(new MyReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
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
        def apply(event: Event) {
          event match {
            case TestSucceeded(ordinal, suiteName, suiteId, suiteClassName, testName, testText, testEvents, duration, formatter, location, rerunnable, payload, threadName, timeStamp) =>
              if (testName.indexOf("this thing must start with proper words") != -1) {
                testSucceededReportHadCorrectTestName = true
              }  
            case _ =>
          }
        }
      }
      class MySpec extends Spec with ShouldMatchers with StringFixture {
        def `this thing must start with proper words`(fixture: String) {}
      }
      val a = new MySpec
      a.run(None, Args(new MyReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(testSucceededReportHadCorrectTestName)
    }
    
    it("In a TestFailed report, the test name should be verbatim if it is top level test") {
      var testFailedReportHadCorrectTestName = false
      class MyReporter extends Reporter {
        def apply(event: Event) {
          event match {
            case event: TestFailed =>
              if (event.testName.indexOf("this thing must start with proper words") != -1)
                testFailedReportHadCorrectTestName = true
            case _ =>
          }
        }
      }
      class MySpec extends Spec with ShouldMatchers with StringFixture {
        def `this thing must start with proper words`(fixture: String) { fail() }
      }
      val a = new MySpec
      a.run(None, Args(new MyReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(testFailedReportHadCorrectTestName)
    }
    
    it("In a TestStarting report, the test name should start with '<scope> ' if nested one level " +
        "inside a object clause and registered with it") {
      var testSucceededReportHadCorrectTestName = false
      class MyReporter extends Reporter {
        def apply(event: Event) {
          event match {
            case TestStarting(_, _, _, _, testName, _, _, _, _, _, _, _) =>
              if (testName == "A Stack needs to push and pop properly") {
                testSucceededReportHadCorrectTestName = true
              }
            case _ => 
          }
        }
      }
      class MySpec extends Spec with ShouldMatchers with StringFixture {
        object `A Stack` {
          def `needs to push and pop properly`(fixture: String) {}
        }
      }
      val a = new MySpec
      a.run(None, Args(new MyReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(testSucceededReportHadCorrectTestName)
    }
    
    it("Spec should send defined formatters") {
      class MyReporter extends Reporter {

        var gotAnUndefinedFormatter = false
        var lastEventWithUndefinedFormatter: Option[Event] = None

        private def ensureFormatterIsDefined(event: Event) {
          if (!event.formatter.isDefined) {
            gotAnUndefinedFormatter = true
            lastEventWithUndefinedFormatter = Some(event)
          }
        }

        def apply(event: Event) {
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

      class MySpec extends Spec with ShouldMatchers with StringFixture {
        def `it should send defined formatters`(fixture: String) {
          assert(true)
        }
        def `it should also send defined formatters`(fixture: String) {
          assert(false)
        }
      }
      val a = new MySpec
      val myRep = new MyReporter
      a.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!myRep.gotAnUndefinedFormatter, myRep.lastEventWithUndefinedFormatter.toString)
    }
    
    it("SpecText should come through correctly in a SpecReport when registering with def") {
      var testSucceededReportHadCorrectSpecText = false
      var lastSpecText: Option[String] = None
      class MyReporter extends Reporter {
        def apply(event: Event) {
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
      class MySpec extends Spec with ShouldMatchers with StringFixture {
        def `My spec text must have the proper words`(fixture: String) {}
      }
      val a = new MySpec
      a.run(None, Args(new MyReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(testSucceededReportHadCorrectSpecText, lastSpecText match { case Some(s) => s; case None => "No report"})
    }
    
    it("Spec text should come through correctly in a SpecReport when registering with def when nested in one object") {
      var testSucceededReportHadCorrectSpecText = false
      var lastSpecText: Option[String] = None
      class MyReporter extends Reporter {
        def apply(event: Event) {
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
      class MySpec extends Spec with ShouldMatchers with StringFixture {
        object `A Stack` {
          def `My short name must have the proper words`(fixture: String) {}
        }
      }
      val a = new MySpec
      a.run(None, Args(new MyReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(testSucceededReportHadCorrectSpecText, lastSpecText match { case Some(s) => s; case None => "No report"})
    }
    
    it("Spec text should come through correctly in a SpecReport when registering with def when nested in two objects") {
      var testSucceededReportHadCorrectSpecText = false
      var lastSpecText: Option[String] = None
      class MyReporter extends Reporter {
        def apply(event: Event) {
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
      class MySpec extends Spec with ShouldMatchers with StringFixture {
        object `A Stack` {
          object `(when empty)` {
            def `My short name must have the proper words`(fixture: String) {}
          }
        }
      }
      val a = new MySpec
      a.run(None, Args(new MyReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(testSucceededReportHadCorrectSpecText, lastSpecText match { case Some(s) => s; case None => "No report"})
    }
    
    it("Should get ScopedOpened with description if one and only one object clause") {

      val expectedSpecText = "A Stack"

      class MyReporter extends Reporter {
        var scopeOpenedCalled = false
        var expectedMessageReceived = false
        def apply(event: Event) {
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

      class MySpec extends Spec with ShouldMatchers with StringFixture {
        object `A Stack` {
          def `should allow me to push`(fixture: String) {}
        }
      }

      val a = new MySpec
      val myRep = new MyReporter
      a.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(myRep.scopeOpenedCalled)
      assert(myRep.expectedMessageReceived)
    }
    
    it("should be able to send info to the reporter") { 

      val expectedMessage = "this is the expected message"

      class MyReporter extends Reporter {
        var infoProvidedCalled = false
        var expectedMessageReceived = false

        def apply(event: Event) {
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

      class MySpec extends Spec with ShouldMatchers with StringFixture {
        object `A Stack` {
          object `(when not empty)` {
            def `should allow me to pop`(fixture: String) {
              info(expectedMessage)
              ()
            }
          }
          object `(when not full)` {
            def `should allow me to push`(fixture: String) {}
          }
        }
      }
      val a = new MySpec
      val myRep = new MyReporter
      a.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(myRep.infoProvidedCalled)
      assert(myRep.expectedMessageReceived)
    }
    
    it("test durations are included in TestFailed and TestSucceeded events fired from Spec") {

      class MySpec extends Spec with StringFixture {
        def `should succeed`(fixture: String) {}
        def `should fail`(fixture: String) { fail() }
      }

      val mySpec = new MySpec
      val myReporter = new TestDurationReporter
      mySpec.run(None, Args(myReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker(new Ordinal(99)), Set.empty))
      assert(myReporter.testSucceededWasFiredAndHadADuration)
      assert(myReporter.testFailedWasFiredAndHadADuration)
    }
    
    it("suite durations are included in SuiteCompleted events fired from Spec") {

      class MySpec extends Spec with StringFixture {
        override def nestedSuites = Vector(new org.scalatest.Suite {})
      }

      val mySuite = new MySpec
      val myReporter = new SuiteDurationReporter
      mySuite.run(None, Args(myReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker(new Ordinal(99)), Set.empty))
      assert(myReporter.suiteCompletedWasFiredAndHadADuration)
    }
    
    it("suite durations are included in SuiteAborted events fired from Spec") {

      class SuiteThatAborts extends Suite with StringFixture {
        override def run(testName: Option[String], args: Args): Status = {
          throw new RuntimeException("Aborting for testing purposes")
        }
      }

      class MySpec extends Spec with StringFixture {
        override def nestedSuites = Vector(new SuiteThatAborts {})
      }

      val mySuite = new MySpec
      val myReporter = new SuiteDurationReporter
      mySuite.run(None, Args(myReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker(new Ordinal(99)), Set.empty))
      assert(myReporter.suiteAbortedWasFiredAndHadADuration)
    }
    
    it("pending in a Spec should cause TestPending to be fired") {

      class MySpec extends Spec with StringFixture {
        def `should be pending`(fixture: String) { pending }
      }

      val mySuite = new MySpec
      val myReporter = new PendingReporter
      mySuite.run(None, Args(myReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker(new Ordinal(99)), Set.empty))
      assert(myReporter.testPendingWasFired)
    }
    
    it("should unwrap InvocationTargetException thrown from scope evaluation") {
      
      class ExampleSpec extends Spec with StringFixture {
        object `A Scope` {
          throw new IllegalArgumentException("boom!")
          def `Test 1` {}
          def `Test 2` {}
          def `Test 3` {}
        }
      }
      
      val s = new ExampleSpec
      intercept[IllegalArgumentException] {
        s.run(None, Args(reporter = SilentReporter))
      }
    }
    
    describe("the stopper") {
      
      it("should stop nested suites from being executed") {
        class SpecA extends Spec with StringFixture {
          var executed = false;
          override def run(testName: Option[String], args: Args): Status = {
            executed = true
            super.run(testName, args)
          }
        }
        class SpecB extends Spec with StringFixture {
          var executed = false;
          override def run(testName: Option[String], args: Args): Status = {
            executed = true
            super.run(testName, args)
          }
        }
        class SpecC extends Spec with StringFixture {
          var executed = false;
          override def run(testName: Option[String], args: Args): Status = {
            executed = true
            super.run(testName, args)
          }
        }
        class SpecD extends Spec with StringFixture {
          var executed = false;
          override def run(testName: Option[String], args: Args): Status = {
            executed = true
            val status = super.run(testName, args)
            args.stopper.requestStop()
            status
          }
        }
        class SpecE extends Spec with StringFixture {
          var executed = false;
          override def run(testName: Option[String], args: Args): Status = {
            executed = true
            super.run(testName, args)
          }
        }
        class SpecF extends Spec with StringFixture {
          var executed = false;
          override def run(testName: Option[String], args: Args): Status = {
            executed = true
            super.run(testName, args)
          }
        }
        class SpecG extends Spec with StringFixture {
          var executed = false;
          override def run(testName: Option[String], args: Args): Status = {
            executed = true
            super.run(testName, args)
          }
        }
        
        class IgnoreStopRequestStopper extends Stopper {
          def stopRequested: Boolean = false
          def requestStop() {}
          def reset() {}
        }

        val a = new SpecA
        val b = new SpecB
        val c = new SpecC
        val d = new SpecD
        val e = new SpecE
        val f = new SpecF
        val g = new SpecG

        val x = Specs(a, b, c, d, e, f, g)
        x.run(None, Args(SilentReporter, new IgnoreStopRequestStopper, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))

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

        val y = Specs(h, i, j, k, l, m, n)
        y.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))

        assert(k.executed)
        assert(i.executed)
        assert(j.executed)
        assert(k.executed)
        assert(!l.executed)
        assert(!m.executed)
        assert(!n.executed)
      }
      
      it("should stop tests from being executed") {

        class MySpec extends Spec with StringFixture {
          var theTestsExecutedCount = 0
          def `test: 1`(fixture: String) { theTestsExecutedCount += 1 }
          def `test: 2`(fixture: String) { theTestsExecutedCount += 1 }
          def `test: 3`(fixture: String) { theTestsExecutedCount += 1 }
          def `test: 4`(fixture: String) {
            theTestsExecutedCount += 1
          }
          def `test: 5`(fixture: String) { theTestsExecutedCount += 1 }
          def `test: 6`(fixture: String) { theTestsExecutedCount += 1 }
          def `test: 7`(fixture: String) { theTestsExecutedCount += 1 }
        }

        val x = new MySpec
        x.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
        assert(x.theTestsExecutedCount === 7)

        val myStopper = Stopper.default

        class MyStoppingSpec extends Spec with StringFixture {
          var testsExecutedCount = 0
          def `test: 1`(fixture: String) { testsExecutedCount += 1 }
          def `test: 2`(fixture: String) { testsExecutedCount += 1 }
          def `test: 3`(fixture: String) { testsExecutedCount += 1 }
          def `test: 4`(fixture: String) {
            testsExecutedCount += 1
            myStopper.requestStop()
          }
          def `test: 5`(fixture: String) { testsExecutedCount += 1 }
          def `test: 6`(fixture: String) { testsExecutedCount += 1 }
          def `test: 7`(fixture: String) { testsExecutedCount += 1 }
        }

        val y = new MyStoppingSpec
        y.run(None, Args(SilentReporter, myStopper, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
        assert(y.testsExecutedCount === 4)
      }
    }
    
    describe("(with info calls)") {
      class InfoInsideTestSpec extends Spec {
        val msg = "hi there, dude"
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test(msg) }
        def `test name`(fixture: String) {
          info(fixture)
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
        spec.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
        val testStarting = myRep.testStartingEventsReceived
        assert(1 === testStarting.size)
        val testSucceeded = myRep.testSucceededEventsReceived
        assert(1 === testSucceeded.size)
        assert(1 === testSucceeded(0).recordedEvents.size)
        val ip: InfoProvided = testSucceeded(0).recordedEvents(0).asInstanceOf[InfoProvided]
        assert(spec.msg === ip.message)
      }
      class InfoBeforeTestSpec extends Spec with StringFixture {
        val msg = "hi there, dude"
        val testName = "test name"
        info(msg)
        def `test name`(fixture: String) {}
      }
      it("should, when the info appears in the body before a test, report the info before the test") {
        val spec = new InfoBeforeTestSpec
        val (infoProvidedIndex, testStartingIndex, testSucceededIndex) =
          getIndexesForInformerEventOrderTests(spec, spec.testName, spec.msg)
        assert(infoProvidedIndex < testStartingIndex)
        assert(testStartingIndex < testSucceededIndex)
      }
      // TODO: This does not work, can't think of a solution yet.
      /*it("should, when the info appears in the body after a test, report the info after the test runs") {
        val msg = "hi there, dude"
        val testName = "test name"
        class MySpec extends Spec {
          def `test name` {}
          info(msg)
        }
        val (infoProvidedIndex, testStartingIndex, testSucceededIndex) =
          getIndexesForInformerEventOrderTests(new MySpec, testName, msg)
        assert(testStartingIndex < testSucceededIndex)
        assert(testSucceededIndex < infoProvidedIndex)
      }*/
      it("should print to stdout when info is called by a method invoked after the suite has been executed") {
        class MySpec extends Spec with StringFixture {
          callInfo() // This should work fine
          def callInfo() {
            info("howdy")
          }
          def `howdy also`(fixture: String) {
            callInfo() // This should work fine
          }
        }
        val spec = new MySpec
        val myRep = new EventRecordingReporter
        spec.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
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
        spec.run(None, Args(myRep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
        val indentedText = getIndentedTextFromTestInfoProvided(spec)
        assert(indentedText === IndentedText("  + " + spec.msg, spec.msg, 1))
      }
    }
  }
  
  describe("A Suite's execute method") {
    it("should throw NPE if passed null for configMap") {
      class MySpec extends Spec with StringFixture
      intercept[NullPointerException] {
        (new MySpec).execute(configMap = null)
      }
    }
    it("should throw IAE if a testName is passed that does not exist on the suite") {
      class MySpec extends Spec with StringFixture
      intercept[IllegalArgumentException] {
        (new MySpec).execute(testName = "fred")
      }
    }
  }

  // TODO: ensure spec aborts if same name is used with and without communicator
  it("should discover method names and tags") {

    val a = new Spec with StringFixture {
      def `some test name`(fixture: String): Unit = ()
    }
    assert(a.expectedTestCount(Filter()) === 1)
    val tnResult: Set[String] = a.testNames
    val gResult: Map[String, Set[String]] = a.tags
    assert(tnResult.size === 1)
    assert(gResult.keySet.size === 0)
  }
  
  it("should not return tests with no tags in the tags map") {
    
    val a = new Spec with StringFixture {
      def `test: not tagged`(fixture: String) = ()
    }
    assert(a.tags.keySet.size === 0)
  }
  
  it("should discover methods that return non-Unit") {
    val a = new Spec with StringFixture {
      def `test: this`(fixture: String): Int = 1
      def `test: that`(fixture: String): String = "hi"
    }
    assert(a.expectedTestCount(Filter()) === 2)
    assert(a.testNames.size === 2)
    assert(a.tags.keySet.size === 0)
  }
  
  it("should not discover $$outer method generated by scala compiler as test") {
    class SetSpec extends Spec with StringFixture {
      object `A Set` {
        object `when empty` {
          def `should have size 0`(fixture: String) {
            assert(Set.empty.size === 0)
          }
    
          def `should produce NoSuchElementException when head is invoked`(fixture: String) {
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

    class MySpec extends Spec with StringFixture {
      def `test succeeds`(fixture: String) = ()
      def `test fails`(fixture: String) { fail() }
    }

    val mySpec = new MySpec
    val myReporter = new TestDurationReporter
    mySpec.run(None, Args(myReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker(new Ordinal(99)), Set.empty))
    assert(myReporter.testSucceededWasFiredAndHadADuration)
    assert(myReporter.testFailedWasFiredAndHadADuration)
  }
  
  class SpecThatAborts extends Spec with StringFixture {
    override def run(testName: Option[String], args: Args): Status = {
      throw new RuntimeException("Aborting for testing purposes")
    }
  }

  it("should fire a defined duration in a SuiteCompleted event") {

    // the spec duration is sent by runNestedSuites, so MySpec needs a
    // nested suite
    class MySpec extends Spec with StringFixture {
      override def nestedSuites = Vector(new Spec with StringFixture {})
      def `test Succeeds`(fixture: String) = ()
      def `test Fails`(fixture: String) { fail() }
    }

    val mySpec = new MySpec
    val myReporter = new SuiteDurationReporter
    mySpec.run(None, Args(myReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker(new Ordinal(99)), Set.empty))
    assert(myReporter.suiteCompletedWasFiredAndHadADuration)

  }

  it("should fire a defined duration in a SuiteAborted event") {

    // the suite duration is sent by runNestedSuites, so MySuite needs a
    // nested suite
    class MyOtherSpec extends Spec with StringFixture {
      override def nestedSuites = Vector(new SpecThatAborts)
      def `test Succeeds`(fixture: String) = ()
      def `test Fails`(fixture: String) { fail() }
    }

    val myOtherSpec = new MyOtherSpec
    val myOtherReporter = new SuiteDurationReporter
    myOtherSpec.run(None, Args(myOtherReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker(new Ordinal(99)), Set.empty))
    assert(myOtherReporter.suiteAbortedWasFiredAndHadADuration)
  }

  it("should fire TestPending event for a pending test") {

    class MySpec extends Spec with StringFixture {
      def `this is a pending test`(fixture: String) { pending }
    }

    val mySpec = new MySpec
    val myReporter = new PendingReporter
    mySpec.run(None, Args(myReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker(new Ordinal(99)), Set.empty))
    assert(myReporter.testPendingWasFired)
  }

  class TestWasCalledSpec extends Spec with StringFixture {
    var theTestThisCalled = false
    var theTestThatCalled = false
    var theTestTheOtherCalled = false
    var theTestThisConfigMapWasEmpty = true
    var theTestThatConfigMapWasEmpty = true
    var theTestTheOtherConfigMapWasEmpty = true
    override def withFixture(test: OneArgTest): Outcome = {
      if (test.configMap.size > 0)
        test.name match {
          case "test this" => theTestThisConfigMapWasEmpty = false
          case "test that" => theTestThatConfigMapWasEmpty = false
          case "test the other" => theTestTheOtherConfigMapWasEmpty = false
          case _ => throw new Exception("Should never happen")
        }
      test("hi")
    }
    def `test this`(fixture: String) { theTestThisCalled = true }
    def `test that`(fixture: String) { theTestThatCalled = true }
    def `test the other`(fixture: String) { theTestTheOtherCalled = true }
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
    class TestWasCalledSpec extends Spec {
      var theTestThisCalled = false
      var theTestThatCalled = false
      var theTestTheOtherCalled = false
      var theTestThisConfigMapWasEmpty = true
      var theTestThatConfigMapWasEmpty = true
      var theTestTheOtherConfigMapWasEmpty = true
      override def withFixture(test: OneArgTest): Outcome = {
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
    class ASpec extends Spec with StringFixture {

      def `test: the + operator should add`(fixture: String) {
        val sum = 1 + 1
        assert(sum === 2)
      }

      def `test: the - operator should subtract`(fixture: String) {
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

  def testTestTags() {
    class TagSpec extends Spec with StringFixture {  
      def testNoTagMethod(fixture: String) {}
      @SlowAsMolasses
      def testTagMethod(fixture: String) {}
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
    
      class NoTagSpec extends Spec with StringFixture
      @Ignore
      class IgnoreSpec extends Spec with StringFixture {
        def `test method 1`(fixture: String) {}
        def `test method 2`(fixture: String) {}
        def `test method 3`(fixture: String) {}
      }
      @SlowAsMolasses
      class SlowAsMolassesSpec extends Spec with StringFixture
      @FastAsLight
      class FastAsLightSpec extends Spec with StringFixture
    
      class MasterSpec extends Spec with StringFixture {
        override def nestedSuites = Vector(new NoTagSpec(), new IgnoreSpec(), new SlowAsMolassesSpec(), new FastAsLightSpec())
        override def runNestedSuites(args: Args): Status = {
          super.runNestedSuites(args)
        }
      }
    
      class CounterDistributor extends Distributor {
        var count = 0
        def apply(suite: org.scalatest.Suite, args: Args): Status = {
          count += 1
          SucceededStatus
        }
        def apply(suite: org.scalatest.Suite, tracker: Tracker) {
          count += 1
        }
      }

      val masterSpec = new MasterSpec()

      val defaultFilter = new Filter(None, Set.empty)
      val defaultReporter = new EventRecordingReporter
      masterSpec.runNestedSuites(Args(defaultReporter, Stopper.default, defaultFilter, ConfigMap.empty, None, new Tracker(new Ordinal(99)), Set.empty))
      assert(defaultReporter.suiteStartingEventsReceived.size === 4)
      assert(defaultReporter.testIgnoredEventsReceived.size === 3)
      val defaultReporterDist = new EventRecordingReporter
      val defaultDistributor = new CounterDistributor
      masterSpec.runNestedSuites(Args(defaultReporterDist, Stopper.default, defaultFilter, ConfigMap.empty, Some(defaultDistributor), new Tracker(new Ordinal(99)), Set.empty))
      assert(defaultDistributor.count === 4)

      val includeFilter = new Filter(Some(Set("org.scalatest.FastAsLight")), Set.empty)
      val includeReporter = new EventRecordingReporter
      masterSpec.runNestedSuites(Args(includeReporter, Stopper.default, includeFilter, ConfigMap.empty, None, new Tracker(new Ordinal(99)), Set.empty))
      assert(includeReporter.suiteStartingEventsReceived.size === 4) 
      assert(includeReporter.testIgnoredEventsReceived.size === 0) 
      val includeReporterDist = new EventRecordingReporter
      val includeDistributor = new CounterDistributor
      masterSpec.runNestedSuites(Args(includeReporterDist, Stopper.default, includeFilter, ConfigMap.empty, Some(includeDistributor), new Tracker(new Ordinal(99)), Set.empty))
      assert(includeDistributor.count === 4) 

      val excludeFilter = new Filter(None, Set("org.scalatest.SlowAsMolasses"))
      val excludeReporter = new EventRecordingReporter
      masterSpec.runNestedSuites(Args(excludeReporter, Stopper.default, excludeFilter, ConfigMap.empty, None, new Tracker(new Ordinal(99)), Set.empty))
      assert(excludeReporter.suiteStartingEventsReceived.size === 4)
      assert(excludeReporter.testIgnoredEventsReceived.size === 3)
      val excludeReporterDist = new EventRecordingReporter
      val excludeDistributor = new CounterDistributor
      masterSpec.runNestedSuites(Args(excludeReporterDist, Stopper.default, excludeFilter, ConfigMap.empty, Some(excludeDistributor), new Tracker(new Ordinal(99)), Set.empty))
      assert(excludeDistributor.count === 4)
    }
  }
  
  describe("when its expectedTestCount method is invoked") {
    it("should return a count that takes into 'account' the passed filter") {
      class NoTagSpec extends Spec with StringFixture {
        def `test method 1`(fixture: String) {}
        def `test method 2`(fixture: String) {}
        def `test method 3`(fixture: String) {}
      }
      @Ignore
      class IgnoreSpec extends Spec with StringFixture {
        def `test method 1`(fixture: String) {}
        def `test method 2`(fixture: String) {}
        def `test method 3`(fixture: String) {}
      }
      @SlowAsMolasses
      class SlowAsMolassesSpec extends Spec with StringFixture {
        def `test method 1`(fixture: String) {}
        def `test method 2`(fixture: String) {}
        def `test method 3`(fixture: String) {}
      }
      @FastAsLight
      class FastAsLightSpec extends Spec with StringFixture {
        def `test method 1`(fixture: String) {}
        def `test method 2`(fixture: String) {}
        def `test method 3`(fixture: String) {}
      }
    
      class MasterSpec extends Spec with StringFixture {
        override def nestedSuites = Vector(new NoTagSpec(), new IgnoreSpec(), new SlowAsMolassesSpec(), new FastAsLightSpec())
        override def runNestedSuites(args: Args): Status = {
          super.runNestedSuites(args)
        }
      }
    
      val masterSpec = new MasterSpec()
      assert(masterSpec.expectedTestCount(new Filter(None, Set.empty)) === 9)
      assert(masterSpec.expectedTestCount(new Filter(Some(Set("org.scalatest.FastAsLight")), Set.empty)) === 3)
      assert(masterSpec.expectedTestCount(new Filter(None, Set("org.scalatest.FastAsLight"))) === 6)
      assert(masterSpec.expectedTestCount(new Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set.empty)) === 3)
      assert(masterSpec.expectedTestCount(new Filter(None, Set("org.scalatest.SlowAsMolasses"))) === 6)
    }
  }
  
  describe("when its rerunner method is invoked") {
    it("should provide the fully qualified name of a class that could be used to rerun the suite, wrapped in a Some, or None, if can't be rerun") {
      assert(new NormalSpec().rerunner.get === classOf[NormalSpec].getName)
      assert(new WrappedSpec(Map.empty).rerunner.get === classOf[WrappedSpec].getName)
      assert(new NotAccessibleSpec("test").rerunner === None)
    }
  }
  
  it("should run only chosen styles, if specified, and throw an exception from run if a non-chosen style is attempted to be run") {

    class SimpleSpec extends Spec with StringFixture {
      def `test method 1`(fixture: String) {}
      def `test method 2`(fixture: String) {}
      def `test method 3`(fixture: String) {}
    }
    
    val simpleSpec = new SimpleSpec()
    simpleSpec.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
    simpleSpec.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap(CHOSEN_STYLES -> Set("org.scalatest.fixture.Spec")), None, new Tracker, Set.empty))
    val caught =
      intercept[NotAllowedException] {
        simpleSpec.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap(CHOSEN_STYLES -> Set("org.scalatest.fixture.FunSpec")), None, new Tracker, Set.empty))
      }
    import OptionValues._
    assert(caught.message.value === Resources("notTheChosenStyle", "org.scalatest.fixture.Spec", "org.scalatest.fixture.FunSpec"))
    val caught2 =
      intercept[NotAllowedException] {
        simpleSpec.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap(CHOSEN_STYLES -> Set("org.scalatest.fixture.FunSpec", "org.scalatest.fixture.FreeSpec")), None, new Tracker, Set.empty))
      }
    assert(caught2.message.value === Resources("notOneOfTheChosenStyles", "org.scalatest.fixture.Spec", makeListForHumans(Vector("org.scalatest.fixture.FunSpec", "org.scalatest.fixture.FreeSpec"))))
    val caught3 =
      intercept[NotAllowedException] {
        simpleSpec.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap(CHOSEN_STYLES -> Set("org.scalatest.fixture.FunSpec", "org.scalatest.fixture.FreeSpec", "org.scalatest.fixture.FlatSpec")), None, new Tracker, Set.empty))
      }
    assert(caught3.message.value === Resources("notOneOfTheChosenStyles", "org.scalatest.fixture.Spec", makeListForHumans(Vector("org.scalatest.fixture.FunSpec", "org.scalatest.fixture.FreeSpec", "org.scalatest.fixture.FlatSpec"))))
  }
  
  describe("when a test fails") {
    it("should send proper stack depth information") {
      class TestSpec extends Spec with StringFixture {
        def `test failure`(fixture: String) {
          assert(1 === 2)
        }
      }
      val rep = new EventRecordingReporter
      val s1 = new TestSpec
      s1.run(None, Args(rep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(rep.testFailedEventsReceived.size === 1)
      assert(rep.testFailedEventsReceived(0).throwable.get.asInstanceOf[TestFailedException].failedCodeFileName.get === "SpecSpec.scala")
      assert(rep.testFailedEventsReceived(0).throwable.get.asInstanceOf[TestFailedException].failedCodeLineNumber.get === thisLineNumber - 8)
    }
    
    it("should fire TestFailed event with correct stack depth info when test failed") {
      class TestSpec extends Spec with StringFixture {
        def `it should fail`(fixture: String) {
          assert(1 === 2)
        }
        object `A scenario` {
          def `should fail`(fixture: String) {
            assert(1 === 2)
          }
        }
      }
      val rep = new EventRecordingReporter
      val s1 = new TestSpec
      s1.run(None, Args(rep, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(rep.testFailedEventsReceived.size === 2)
      // The 'A scenario should fail' will be execute first because tests are executed in alphanumerical order.
      assert(rep.testFailedEventsReceived(0).throwable.get.asInstanceOf[TestFailedException].failedCodeFileName.get === "SpecSpec.scala") 
      assert(rep.testFailedEventsReceived(0).throwable.get.asInstanceOf[TestFailedException].failedCodeLineNumber.get === thisLineNumber - 10)
      assert(rep.testFailedEventsReceived(1).throwable.get.asInstanceOf[TestFailedException].failedCodeFileName.get === "SpecSpec.scala")
      assert(rep.testFailedEventsReceived(1).throwable.get.asInstanceOf[TestFailedException].failedCodeLineNumber.get === thisLineNumber - 16)
    }
    
    it("should throw DuplicateTestNameException when overload test method with and without fixture is defined") {
      class TestSpec extends Spec with StringFixture {
        def `test 1` { }
        def `test 1`(fixture: String) { }
      }
      val s1 = new TestSpec
      val e = intercept[DuplicateTestNameException] {
        s1.run(None, Args(reporter = SilentReporter))
      }
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 2)))
      assert(e.failedCodeFileName === Some("SpecSpec.scala"))
    }
    
    it("should throw DuplicateTestNameException when overload test method with and without fixture is defined in scope") {
      class TestSpec extends Spec with StringFixture {
        object `scope 1` {
          def `test 1` { }
          def `test 1`(fixture: String) { }
        }
      }
      val s1 = new TestSpec
      val e = intercept[DuplicateTestNameException] {
        s1.run(None, Args(reporter = SilentReporter))
      }
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 2)))
      assert(e.failedCodeFileName === Some("SpecSpec.scala"))
    }
  }
}

@DoNotDiscover
class `My Spec` extends Spec with StringFixture {}
@DoNotDiscover
class NormalSpec extends Spec with StringFixture
@DoNotDiscover
@WrapWith(classOf[ConfigMapWrapperSuite]) 
class WrappedSpec(configMap: Map[_, _]) extends Spec with StringFixture
@DoNotDiscover
class NotAccessibleSpec(name: String) extends Spec with StringFixture
