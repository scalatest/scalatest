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
package fixture

import collection.immutable.TreeSet
import events.TestFailed
import events.TestSucceeded
import mock.MockitoSugar
import org.scalatest.exceptions.TestFailedException
import org.scalatest.events.InfoProvided
import SharedHelpers._

class SuiteSpec extends org.scalatest.FunSpec with PrivateMethodTester {

  describe("The private testMethodTakesInformer method") {
    val testMethodTakesAFixtureAndInformer = PrivateMethod[Boolean]('testMethodTakesAFixtureAndInformer)
    val suiteObject = Suite
    it("should return true if passed a string that ends in (FixtureParam, Informer)") {
      assert(suiteObject invokePrivate testMethodTakesAFixtureAndInformer("thisDoes(FixtureParam, Informer)"))
      assert(suiteObject invokePrivate testMethodTakesAFixtureAndInformer("(FixtureParam, Informer)"))
      assert(suiteObject invokePrivate testMethodTakesAFixtureAndInformer("test(FixtureParam, Informer)"))
    }
    it("should return false if passed a string that doesn't end in (FixtureParam, Informer)") {
      assert(!(suiteObject invokePrivate testMethodTakesAFixtureAndInformer("thisDoesNot(FixtureParam)")))
      assert(!(suiteObject invokePrivate testMethodTakesAFixtureAndInformer("test(FixtureParam)")))
    }
  }


/*
  describe("A fixture.Suite without SimpleWithFixture") {

    it("should return the test names in alphabetical order from testNames") {
      val a = new Suite {
        type FixtureParam = String
        def withFixture(fun: String => Unit, config: Map[String, Any]): Outcome = Succeeded
        def testThis(fixture: String) {}
        def testThat(fixture: String) {}
      }

      assertResult(List("testThat(FixtureParam)", "testThis(FixtureParam)")) {
        a.testNames.iterator.toList
      }

      val b = new Suite {
        type FixtureParam = String
        def withFixture(fun: String => Unit, config: Map[String, Any]): Outcome = Succeeded
      }

      assertResult(List[String]()) {
        b.testNames.iterator.toList
      }

      val c = new Suite {
        type FixtureParam = String
        def withFixture(fun: String => Unit, config: Map[String, Any]): Outcome = Succeeded
        def testThat(fixture: String) {}
        def testThis(fixture: String) {}
      }

      assertResult(List("testThat(FixtureParam)", "testThis(FixtureParam)")) {
        c.testNames.iterator.toList
      }
    }

    it("should discover tests with and without Informer parameters") {
      val a = new Suite {
        type FixtureParam = String
        def withFixture(fun: String => Unit, config: Map[String, Any]): Outcome = Succeeded
        def testThis(fixture: String) = ()
        def testThat(fixture: String, info: Informer) = ()
      }
      assert(a.testNames === TreeSet("testThat(FixtureParam, Informer)", "testThis(FixtureParam)"))
    }

    it("should pass in the fixture to every test method") {
      val a = new Suite {
        type FixtureParam = String
        val hello = "Hello, world!"
        def withFixture(fun: String => Unit, config: Map[String, Any]): Outcome = {
          test(hello)
        }
        def testThis(fixture: String) {
          assert(fixture === hello)
        }
        def testThat(fixture: String, info: Informer) {
          assert(fixture === hello)
        }
      }
      a.run(None, SilentReporter, new Stopper {}, Filter(), ConfigMap.empty, None, new Tracker())
    }

    it("can pass in the config map to every test method via the fixture") {
      val key = "greeting"
      val hello = "Hello, world!"
      val a = new Suite {
        type FixtureParam = Map[String, Any]
        def withFixture(fun: FixtureParam => Unit, config: Map[String, Any]): Outcome = {
          test(config)
        }
        def testThis(fixture: FixtureParam) {
          assert(fixture(key) === hello)
        }
        def testThat(fixture: FixtureParam, info: Informer) {
          assert(fixture(key) === hello)
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, rep, new Stopper {}, Filter(), Map(key -> hello), None, new Tracker())
      assert(!rep.eventsReceived.exists(_.isInstanceOf[TestFailed]))
    }
  }
*/

  describe("A fixture.Suite") {
    it("should return the test names in alphabetical order from testNames") {
      val a = new Suite {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = Succeeded
        def testThis(fixture: String) {}
        def testThat(fixture: String) {}
      }

      assertResult(List("testThat(FixtureParam)", "testThis(FixtureParam)")) {
        a.testNames.iterator.toList
      }

      val b = new Suite {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = Succeeded
      }

      assertResult(List[String]()) {
        b.testNames.iterator.toList
      }

      val c = new Suite {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = Succeeded
        def testThat(fixture: String) {}
        def testThis(fixture: String) {}
      }

      assertResult(List("testThat(FixtureParam)", "testThis(FixtureParam)")) {
        c.testNames.iterator.toList
      }
    }

    it("should discover tests with and without Informer parameters") {
      val a = new Suite {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = Succeeded
        def testThis(fixture: String) = ()
        def testThat(fixture: String, info: Informer) = ()
      }
      assert(a.testNames === TreeSet("testThat(FixtureParam, Informer)", "testThis(FixtureParam)"))
    }

    it("should pass in the fixture to every test method") {
      val a = new Suite {
        type FixtureParam = String
        val hello = "Hello, world!"
        def withFixture(test: OneArgTest): Outcome = {
          test(hello)
        }
        def testThis(fixture: String) {
          assert(fixture === hello)
        }
        def testThat(fixture: String, info: Informer) {
          assert(fixture === hello)
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      assert(!rep.eventsReceived.exists(_.isInstanceOf[TestFailed]))
    }
    
    class TestWasCalledSuite extends Suite {
      type FixtureParam = String
      def withFixture(test: OneArgTest): Outcome = { test("hi") }
      var theTestThisCalled = false
      var theTestThatCalled = false
      def testThis(s: String) { theTestThisCalled = true }
      def testThat(s: String) { theTestThatCalled = true }
    }

    it("should execute all tests when run is called with testName None") {

      val b = new TestWasCalledSuite
      b.run(None, Args(SilentReporter))
      assert(b.theTestThisCalled)
      assert(b.theTestThatCalled)
    }

    it("should execute one test when run is called with a defined testName") {

      val a = new TestWasCalledSuite
      a.run(Some("testThis(FixtureParam)"), Args(SilentReporter))
      assert(a.theTestThisCalled)
      assert(!a.theTestThatCalled)
    }

    it("should report as ignored, ant not run, tests marked ignored") {

      val a = new Suite {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        def testThis(fixture: FixtureParam) { theTestThisCalled = true }
        def testThat(fixture: FixtureParam, info: Informer) { theTestThatCalled = true }
      }

      val repA = new TestIgnoredTrackingReporter
      a.run(None, Args(repA))
      assert(!repA.testIgnoredReceived)
      assert(a.theTestThisCalled)
      assert(a.theTestThatCalled)

      val b = new Suite {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        @Ignore
        def testThis(fixture: FixtureParam) { theTestThisCalled = true }
        def testThat(fixture: FixtureParam, info: Informer) { theTestThatCalled = true }
      }

      val repB = new TestIgnoredTrackingReporter
      b.run(None, Args(repB))
      assert(repB.testIgnoredReceived)
      assert(repB.lastEvent.isDefined)
      assert(repB.lastEvent.get.testName endsWith "testThis(FixtureParam)")
      assert(!b.theTestThisCalled)
      assert(b.theTestThatCalled)

      val c = new Suite {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        def testThis(fixture: FixtureParam) { theTestThisCalled = true }
        @Ignore
        def testThat(fixture: FixtureParam, info: Informer) { theTestThatCalled = true }
      }

      val repC = new TestIgnoredTrackingReporter
      c.run(None, Args(repC))
      assert(repC.testIgnoredReceived)
      assert(repC.lastEvent.isDefined)
      assert(repC.lastEvent.get.testName endsWith "testThat(FixtureParam, Informer)", repC.lastEvent.get.testName)
      assert(c.theTestThisCalled)
      assert(!c.theTestThatCalled)

      val d = new Suite {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        @Ignore
        def testThis(fixture: FixtureParam) { theTestThisCalled = true }
        @Ignore
        def testThat(fixture: FixtureParam, info: Informer) { theTestThatCalled = true }
      }

      val repD = new TestIgnoredTrackingReporter
      d.run(None, Args(repD))
      assert(repD.testIgnoredReceived)
      assert(repD.lastEvent.isDefined)
      assert(repD.lastEvent.get.testName endsWith "testThis(FixtureParam)") // last because run alphabetically
      assert(!d.theTestThisCalled)
      assert(!d.theTestThatCalled)
    }

    it("should ignore a test marked as ignored if run is invoked with that testName") {

      val e = new Suite {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        @Ignore
        def testThis(fixture: FixtureParam) { theTestThisCalled = true }
        def testThat(fixture: FixtureParam, info: Informer) { theTestThatCalled = true }
      }

      val repE = new TestIgnoredTrackingReporter
      e.run(Some("testThis(FixtureParam)"), Args(repE))
      assert(repE.testIgnoredReceived)
      assert(!e.theTestThisCalled)
      assert(!e.theTestThatCalled)
    }

    it("should throw IllegalArgumentException if run is passed a testName that does not exist") {

      val suite = new Suite {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        def testThis(fixture: FixtureParam) { theTestThisCalled = true }
        def testThat(fixture: FixtureParam, info: Informer) { theTestThatCalled = true }
      }

      intercept[IllegalArgumentException] {
        // Here, they forgot that the name is actually testThis(FixtureParam)
        suite.run(Some("testThis"), Args(SilentReporter))
      }

      intercept[IllegalArgumentException] {
        // Here, they gave a non-existent test name
        suite.run(Some("doesNotExist(FixtureParam)"), Args(SilentReporter))
      }
    }

    it("should run only those tests selected by the tags to include and exclude sets") {

      // Nothing is excluded
      val a = new Suite {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        @SlowAsMolasses
        def testThis(fixture: FixtureParam) { theTestThisCalled = true }
        def testThat(fixture: FixtureParam, info: Informer) { theTestThatCalled = true }
      }
      val repA = new TestIgnoredTrackingReporter
      a.run(None, Args(repA))
      assert(!repA.testIgnoredReceived)
      assert(a.theTestThisCalled)
      assert(a.theTestThatCalled)

      // SlowAsMolasses is included, one test should be excluded
      val b = new Suite {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        @SlowAsMolasses
        def testThis(fixture: FixtureParam) { theTestThisCalled = true }
        def testThat(fixture: FixtureParam, info: Informer) { theTestThatCalled = true }
      }
      val repB = new TestIgnoredTrackingReporter
      b.run(None, Args(repB, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repB.testIgnoredReceived)
      assert(b.theTestThisCalled)
      assert(!b.theTestThatCalled)

      // SlowAsMolasses is included, and both tests should be included
      val c = new Suite {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        @SlowAsMolasses
        def testThis(fixture: FixtureParam) { theTestThisCalled = true }
        @SlowAsMolasses
        def testThat(fixture: FixtureParam, info: Informer) { theTestThatCalled = true }
      }
      val repC = new TestIgnoredTrackingReporter
      c.run(None, Args(repB, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repC.testIgnoredReceived)
      assert(c.theTestThisCalled)
      assert(c.theTestThatCalled)

      // SlowAsMolasses is included. both tests should be included but one ignored
      val d = new Suite {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        @Ignore
        @SlowAsMolasses
        def testThis(fixture: FixtureParam) { theTestThisCalled = true }
        @SlowAsMolasses
        def testThat(fixture: FixtureParam, info: Informer) { theTestThatCalled = true }
      }
      val repD = new TestIgnoredTrackingReporter
      d.run(None, Args(repD, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.Ignore")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(repD.testIgnoredReceived)
      assert(!d.theTestThisCalled)
      assert(d.theTestThatCalled)

      // SlowAsMolasses included, FastAsLight excluded
      val e = new Suite {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        @FastAsLight
        @SlowAsMolasses
        def testThis(fixture: FixtureParam) { theTestThisCalled = true }
        @SlowAsMolasses
        def testThat(fixture: FixtureParam, info: Informer) { theTestThatCalled = true }
        def testTheOther(fixture: FixtureParam, info: Informer) { theTestTheOtherCalled = true }
      }
      val repE = new TestIgnoredTrackingReporter
      e.run(None, Args(repE, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
                ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repE.testIgnoredReceived)
      assert(!e.theTestThisCalled)
      assert(e.theTestThatCalled)
      assert(!e.theTestTheOtherCalled)

      // An Ignored test that was both included and excluded should not generate a TestIgnored event
      val f = new Suite {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        @Ignore
        @FastAsLight
        @SlowAsMolasses
        def testThis(fixture: FixtureParam) { theTestThisCalled = true }
        @SlowAsMolasses
        def testThat(fixture: FixtureParam, info: Informer) { theTestThatCalled = true }
        def testTheOther(fixture: FixtureParam, info: Informer) { theTestTheOtherCalled = true }
      }
      val repF = new TestIgnoredTrackingReporter
      f.run(None, Args(repF, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
                ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repF.testIgnoredReceived)
      assert(!f.theTestThisCalled)
      assert(f.theTestThatCalled)
      assert(!f.theTestTheOtherCalled)

      // An Ignored test that was not included should not generate a TestIgnored event
      val g = new Suite {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        @FastAsLight
        @SlowAsMolasses
        def testThis(fixture: FixtureParam) { theTestThisCalled = true }
        @SlowAsMolasses
        def testThat(fixture: FixtureParam, info: Informer) { theTestThatCalled = true }
        @Ignore
        def testTheOther(fixture: FixtureParam, info: Informer) { theTestTheOtherCalled = true }
      }
      val repG = new TestIgnoredTrackingReporter
      g.run(None, Args(repG, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
                ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repG.testIgnoredReceived)
      assert(!g.theTestThisCalled)
      assert(g.theTestThatCalled)
      assert(!g.theTestTheOtherCalled)

      // No tagsToInclude set, FastAsLight excluded
      val h = new Suite {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        @FastAsLight
        @SlowAsMolasses
        def testThis(fixture: FixtureParam) { theTestThisCalled = true }
        @SlowAsMolasses
        def testThat(fixture: FixtureParam, info: Informer) { theTestThatCalled = true }
        def testTheOther(fixture: FixtureParam, info: Informer) { theTestTheOtherCalled = true }
      }
      val repH = new TestIgnoredTrackingReporter
      h.run(None, Args(repH, Stopper.default, Filter(None, Set("org.scalatest.FastAsLight")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repH.testIgnoredReceived)
      assert(!h.theTestThisCalled)
      assert(h.theTestThatCalled)
      assert(h.theTestTheOtherCalled)

      // No tagsToInclude set, SlowAsMolasses excluded
      val i = new Suite {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        @FastAsLight
        @SlowAsMolasses
        def testThis(fixture: FixtureParam) { theTestThisCalled = true }
        @SlowAsMolasses
        def testThat(fixture: FixtureParam, info: Informer) { theTestThatCalled = true }
        def testTheOther(fixture: FixtureParam, info: Informer) { theTestTheOtherCalled = true }
      }
      val repI = new TestIgnoredTrackingReporter
      i.run(None, Args(repI, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repI.testIgnoredReceived)
      assert(!i.theTestThisCalled)
      assert(!i.theTestThatCalled)
      assert(i.theTestTheOtherCalled)

      // No tagsToInclude set, SlowAsMolasses excluded, TestIgnored should not be received on excluded ones
      val j = new Suite {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        @Ignore
        @FastAsLight
        @SlowAsMolasses
        def testThis(fixture: FixtureParam) { theTestThisCalled = true }
        @Ignore
        @SlowAsMolasses
        def testThat(fixture: FixtureParam, info: Informer) { theTestThatCalled = true }
        def testTheOther(fixture: FixtureParam, info: Informer) { theTestTheOtherCalled = true }
      }
      val repJ = new TestIgnoredTrackingReporter
      j.run(None, Args(repJ, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repI.testIgnoredReceived)
      assert(!j.theTestThisCalled)
      assert(!j.theTestThatCalled)
      assert(j.theTestTheOtherCalled)

      // Same as previous, except Ignore specifically mentioned in excludes set
      val k = new Suite {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        @Ignore
        @FastAsLight
        @SlowAsMolasses
        def testThis(fixture: FixtureParam) { theTestThisCalled = true }
        @Ignore
        @SlowAsMolasses
        def testThat(fixture: FixtureParam, info: Informer) { theTestThatCalled = true }
        @Ignore
        def testTheOther(fixture: FixtureParam, info: Informer) { theTestTheOtherCalled = true }
      }
      val repK = new TestIgnoredTrackingReporter
      k.run(None, Args(repK, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses", "org.scalatest.Ignore")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(repK.testIgnoredReceived)
      assert(!k.theTestThisCalled)
      assert(!k.theTestThatCalled)
      assert(!k.theTestTheOtherCalled)
    }

    it("should return the correct test count from its expectedTestCount method") {

      val a = new Suite {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        def testThis(fixture: FixtureParam) = ()
        def testThat(fixture: FixtureParam, info: Informer) = ()
      }
      assert(a.expectedTestCount(Filter()) === 2)

      val b = new Suite {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        @Ignore
        def testThis(fixture: FixtureParam) = ()
        def testThat(fixture: FixtureParam, info: Informer) = ()
      }
      assert(b.expectedTestCount(Filter()) === 1)

      val c = new Suite {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        @FastAsLight
        def testThis(fixture: FixtureParam) = ()
        def testThat(fixture: FixtureParam, info: Informer) = ()
      }
      assert(c.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 1)
      assert(c.expectedTestCount(Filter(None, Set("org.scalatest.FastAsLight"))) === 1)

      val d = new Suite {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        @FastAsLight
        @SlowAsMolasses
        def testThis(fixture: FixtureParam) = ()
        @SlowAsMolasses
        def testThat(fixture: FixtureParam, info: Informer) = ()
        def testTheOtherThing(info: Informer) = ()
      }
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 1)
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight"))) === 1)
      assert(d.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) === 1)
      assert(d.expectedTestCount(Filter()) === 3)

      val e = new Suite {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { test("hi") }
        @FastAsLight
        @SlowAsMolasses
        def testThis(fixture: FixtureParam) = ()
        @SlowAsMolasses
        def testThat(fixture: FixtureParam, info: Informer) = ()
        @Ignore
        def testTheOtherThing(info: Informer) = ()
      }
      assert(e.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 1)
      assert(e.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight"))) === 1)
      assert(e.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) === 0)
      assert(e.expectedTestCount(Filter()) === 2)

      val f = Suites(a, b, c, d, e)
      assert(f.expectedTestCount(Filter()) === 10)
    }

    it("should generate a TestPending message when the test body is (pending)") {
      val a = new Suite {
        type FixtureParam = String
        val hello = "Hello, world!"
        def withFixture(test: OneArgTest): Outcome = {
          test(hello)
        }

        def testDoThis(fixture: FixtureParam) { pending }

        def testDoThat(fixture: FixtureParam) {
          assert(fixture === hello)
        }

        def testDoSomethingElse(fixture: FixtureParam) {
          assert(fixture === hello)
          pending
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      val tp = rep.testPendingEventsReceived
      assert(tp.size === 2)
    }
    it("should generate a TestCanceled message when the test body has a cancel invocation") {
      val a = new Suite {
        type FixtureParam = String
        val hello = "Hello, world!"
        def withFixture(test: OneArgTest): Outcome = {
          test(hello)
        }

        def testDoThis(fixture: FixtureParam) { cancel() }

        def testDoThat(fixture: FixtureParam) {
          assert(fixture === hello)
        }

        def testDoSomethingElse(fixture: FixtureParam) {
          assert(fixture === hello)
          cancel("meant to do that")
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      val tp = rep.testCanceledEventsReceived
      assert(tp.size === 2)
    }
    it("should generate a TestCanceled message when the test body has an assume invocation") {
      val a = new Suite {
        type FixtureParam = String
        val hello = "Hello, world!"
        def withFixture(test: OneArgTest): Outcome = {
          test(hello)
        }

        def testDoThis(fixture: FixtureParam) { assume(1 === 2) }

        def testDoThat(fixture: FixtureParam) {
          assert(fixture === hello)
        }

        def testDoSomethingElse(fixture: FixtureParam) {
          assert(fixture === hello)
          assume(1 === 2, "meant to do that")
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      val tp = rep.testCanceledEventsReceived
      assert(tp.size === 2)
    }
    it("should generate a test failure if a Throwable, or an Error other than direct Error subtypes " +
            "known in JDK 1.5, excluding AssertionError") {
      val a = new Suite {
        type FixtureParam = String
        val hello = "Hello, world!"
        def withFixture(test: OneArgTest): Outcome = {
          test(hello)
        }
        def testThrowsAssertionError(s: String) { throw new AssertionError }
        def testThrowsPlainOldError(s: String) { throw new Error }
        def testThrowsThrowable(s: String) { throw new Throwable }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      val tf = rep.testFailedEventsReceived
      assert(tf.size === 3)
    }
    it("should propagate out Errors that are direct subtypes of Error in JDK 1.5, other than " +
            "AssertionError, causing Suites and Runs to abort.") {
      val a = new Suite {
        type FixtureParam = String
        val hello = "Hello, world!"
        def withFixture(test: OneArgTest): Outcome = {
          test(hello)
        }
        def testThrowsAssertionError(s: String) { throw new OutOfMemoryError }
      }
      intercept[OutOfMemoryError] {
        a.run(None, Args(SilentReporter))
      }
    }
    it("should allow both tests that take fixtures and tests that don't") {
      val a = new Suite {

        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = {
          test("Hello, world!")
        }

        var takesNoArgsInvoked = false
        def testTakesNoArgs() { takesNoArgsInvoked = true }

        var takesAnInformerInvoked = false
        def testTakesAnInformer(info: Informer) { takesAnInformerInvoked = true }

        var takesAFixtureInvoked = false
        def testTakesAFixture(s: String) { takesAFixtureInvoked = true }

        var takesAFixtureAndInformerInvoked = false
        def testTakesAFixtureAndInformer(s: String, info: Informer) { takesAFixtureAndInformerInvoked = true }
      }

      a.run(None, Args(SilentReporter))
      assert(a.testNames.size === 4, a.testNames)
      assert(a.takesNoArgsInvoked)
      assert(a.takesAnInformerInvoked)
      assert(a.takesAFixtureInvoked)
      assert(a.takesAFixtureAndInformerInvoked)
    }
    it("should allow primitive type fixtures") {
      val a = new Suite {

        type FixtureParam = Int
        def withFixture(test: OneArgTest): Outcome = {
          test(99)
        }

        var takesNoArgsInvoked = false
        def testTakesNoArgs() { takesNoArgsInvoked = true }

        var takesAnInformerInvoked = false
        def testTakesAnInformer(info: Informer) { takesAnInformerInvoked = true }

        var takesAFixtureInvoked = false
        def testTakesAFixture(i: Int) { takesAFixtureInvoked = true }

        var takesAFixtureAndInformerInvoked = false
        def testTakesAFixtureAndInformer(i: Int, info: Informer) { takesAFixtureAndInformerInvoked = true }
      }

      a.run(None, Args(SilentReporter))
      assert(a.testNames.size === 4, a.testNames)
      assert(a.takesNoArgsInvoked)
      assert(a.takesAnInformerInvoked)
      assert(a.takesAFixtureInvoked)
      assert(a.takesAFixtureAndInformerInvoked)
    }
    it("should pass a NoArgTest to withFixture for test methods that take no arguments") {
      class MySuite extends Suite {
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
        def testSomething() {
          assert(1 + 1 === 2)
        }
      }

      val s = new MySuite
      s.run(None, Args(SilentReporter))
      assert(s.aNoArgTestWasPassed)
      assert(!s.aOneArgTestWasPassed)
    }
    it("should pass a NoArgTest to withFixture for test methods that take only an Informer") {
      class MySuite extends Suite {
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
        def testSomething(info: Informer) {
          assert(1 + 1 === 2)
        }
      }

      val s = new MySuite
      s.run(None, Args(SilentReporter))
      assert(s.aNoArgTestWasPassed)
      assert(!s.aOneArgTestWasPassed)
    }
    it("should not pass a NoArgTest to withFixture for test methods that take a Fixture and an Informer") {
      class MySuite extends Suite {
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
        def testSomething(fixture: FixtureParam, info: Informer) {
          assert(1 + 1 === 2)
        }
      }

      val s = new MySuite
      s.run(None, Args(SilentReporter))
      assert(!s.aNoArgTestWasPassed)
      assert(s.aOneArgTestWasPassed)
    }
    it("should not pass a NoArgTest to withFixture for test methods that take a Fixture") {
      class MySuite extends Suite {
        type FixtureParam = String
        var aNoArgTestWasPassed = false
        def withFixture(test: OneArgTest): Outcome = {
          // Shouldn't be called
          Succeeded
        }
        override def withFixture(test: NoArgTest): Outcome = {
          aNoArgTestWasPassed = true
          Succeeded
        }
        def testSomething(fixture: FixtureParam) {
          assert(1 + 1 === 2)
        }
      }

      val s = new MySuite
      s.run(None, Args(SilentReporter))
      assert(!s.aNoArgTestWasPassed)
    }
    it("should pass a NoArgTest that invokes the no-arg test when the " +
            "NoArgTest's no-arg apply method is invoked") {

      class MySuite extends Suite {
        type FixtureParam = String
        var theNoArgTestWasInvoked = false
        def withFixture(test: OneArgTest): Outcome = {
          // Shouldn't be called, but just in case don't invoke a OneArgTest
          Succeeded
        }
        def testSomething() {
          theNoArgTestWasInvoked = true
        }
      }

      val s = new MySuite
      s.run(None, Args(SilentReporter))
      assert(s.theNoArgTestWasInvoked)
    }
    it("should pass a NoArgTest that invokes a test that takse an Informer when the " +
            "NoArgTest's no-arg apply method is invoked") {

      class MySuite extends Suite {
        type FixtureParam = String
        var theNoArgTestWasInvoked = false
        def withFixture(test: OneArgTest): Outcome = {
          // Shouldn't be called, but just in case don't invoke a OneArgTest
          Succeeded
        }
        def testSomething(info: Informer) {
          theNoArgTestWasInvoked = true
        }
      }

      val s = new MySuite
      s.run(None, Args(SilentReporter))
      assert(s.theNoArgTestWasInvoked)
    }
    it("should pass the correct test name in the OneArgTest passed to withFixture") {
      val a = new Suite {
        type FixtureParam = String
        var correctTestNameWasPassed = false
        def withFixture(test: OneArgTest): Outcome = {
          correctTestNameWasPassed = test.name == "testSomething(FixtureParam, Informer)"
          test("hi")
        }
        def testSomething(fixture: FixtureParam, info: Informer) {}
      }
      a.run(None, Args(SilentReporter))
      assert(a.correctTestNameWasPassed)
    }
    it("should pass the correct config map in the OneArgTest passed to withFixture") {
      val a = new Suite {
        type FixtureParam = String
        var correctConfigMapWasPassed = false
        def withFixture(test: OneArgTest): Outcome = {
          correctConfigMapWasPassed = (test.configMap == ConfigMap("hi" -> 7))
          test("hi")
        }
        def testSomething(fixture: FixtureParam, info: Informer) {}
      }
      a.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap("hi" -> 7), None, new Tracker(), Set.empty))
      assert(a.correctConfigMapWasPassed)
    }
/*
    it("should send InfoProvided events with aboutAPendingTest set to true and aboutACanceledTest set to false for info " +
            "calls made from a test that is pending") {
      val a = new Suite {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = {
          test("hi")
        }
        def testSomething(s: String, info: Informer) {
          info("two integers")
          info("one is subracted from the other")
          info("the result is the difference between the two numbers")
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
      val a = new Suite {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = {
          test("hi")
        }
        def testSomething(s: String, info: Informer) {
          info("two integers")
          info("one is subracted from the other")
          info("the result is the difference between the two numbers")
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
      val a = new Suite {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = {
          test("hi")
        }
        def testSomething(s: String, info: Informer) {
          info("two integers")
          info("one is subracted from the other")
          info("the result is the difference between the two numbers")
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
*/
    it("should, when a test method takes an Informer and writes to it, report the info in the test completion event") {
      val msg = "hi there dude"
      class MySuite extends Suite {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = {
          test("hi")
        }
        def testWithInformer(s: String, info: Informer) {
          info(msg)
        }
      }
      val (testStartingIndex, testSucceededIndex) =
        getIndexesForTestInformerEventOrderTests(new MySuite, "testWithInformer(FixtureParam, Informer)", msg)
      assert(testStartingIndex < testSucceededIndex)
    }
    it("should, when a test method takes an Informer without a fixture, report the info in the test completion event") {
      val msg = "hi there dude"
      class MySuite extends Suite {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = {
          test("hi")
        }
        def testWithInformer(info: Informer) {
          info(msg)
        }
      }
      val (testStartingIndex, testSucceededIndex) =
        getIndexesForTestInformerEventOrderTests(new MySuite, "testWithInformer(FixtureParam, Informer)", msg)
      assert(testStartingIndex < testSucceededIndex)
    }
  }
  describe("A OneArgTest") {
    it("should provide an easy way to invoke a NoArgTest") {

      var noArgWithFixtureWasCalled = false

      val a = new Suite {

        type FixtureParam = String

        override def withFixture(test: NoArgTest): Outcome = {
          noArgWithFixtureWasCalled = true
          test()
        }

        def withFixture(test: OneArgTest): Outcome = {
          withFixture(test.toNoArgTest("hi"))
        }

        def testSomething(fixture: String) { assert(fixture === "hi") }
      }

      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      assert(noArgWithFixtureWasCalled)
      assert(rep.eventsReceived.exists(_.isInstanceOf[TestSucceeded]))
    }
  }
  
  describe("when failure happens") {
    it("should fire TestFailed event with correct stack depth info when test failed") {
      class TestSpec extends Suite {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = {
          withFixture(test.toNoArgTest("hi"))
        }
        def testFailure(fixture: String) {
          assert(1 === 2)
        }
      }
      val rep = new EventRecordingReporter
      val s1 = new TestSpec
      s1.run(None, Args(rep))
      assert(rep.testFailedEventsReceived.size === 1)
      assert(rep.testFailedEventsReceived(0).throwable.get.asInstanceOf[TestFailedException].failedCodeFileName.get === "SuiteSpec.scala")
      assert(rep.testFailedEventsReceived(0).throwable.get.asInstanceOf[TestFailedException].failedCodeLineNumber.get === thisLineNumber - 8)
    }
  }
}

