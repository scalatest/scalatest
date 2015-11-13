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
import mockito.MockitoSugar
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
  }
  
  describe("OneArgTest") {
    it("should offer a factory method that takes another OneArgTest and a function that implements apply") {
      class PassedFixtureWasSpec extends FunSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest): Outcome = { 
          // These will fail the test if the wrapped tests's TestData is not passed through
          assert(test.name == "some test")
          assert(test.configMap == ConfigMap.empty)
          assert(test.scopes == Seq.empty)
          assert(test.text == "some test")
          assert(test.tags == Set.empty)
          test("hi")
        }
        var passedFixtureWas = ""
        it("some test") { s => passedFixtureWas = s }
      }

      val a = new PassedFixtureWasSpec
      a.run(None, Args(SilentReporter))
      assert(a.passedFixtureWas === "hi")

      class WrappedFixtureSpec extends PassedFixtureWasSpec {
        var withFixtureWasCalled = false
        override def withFixture(test: OneArgTest): Outcome = {
          super.withFixture(
            new OneArgTest {
             def apply(s: String): Outcome = {
               withFixtureWasCalled = true
               test(s.toUpperCase)
             }
             val text: String = test.text
             val configMap: ConfigMap = test.configMap
             val scopes: collection.immutable.IndexedSeq[String] = test.scopes
             val name: String = test.name
             val tags: Set[String] = test.tags
            }
          )
        }
      }

      val b = new WrappedFixtureSpec
      b.run(None, Args(SilentReporter))
      assert(b.passedFixtureWas === "HI")

      class ShorthandWrappedFixtureSpec extends PassedFixtureWasSpec {
        var withFixtureWasCalled = false
        override def withFixture(test: OneArgTest): Outcome = {
          super.withFixture(
            OneArgTest(test) { s =>
               withFixtureWasCalled = true
               test(s.toUpperCase)
             }
          )
        }
      }

      val c = new ShorthandWrappedFixtureSpec
      c.run(None, Args(SilentReporter))
      assert(c.passedFixtureWas === "HI")
    }
  }
}

