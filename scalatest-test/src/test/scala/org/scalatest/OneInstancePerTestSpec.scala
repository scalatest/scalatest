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

import java.util.concurrent.atomic.AtomicInteger

import events._
import SharedHelpers._
import org.scalactic.exceptions.NullArgumentException

class TopLevelSpec extends FunSpec with OneInstancePerTest {
  import TopLevelSpec.sideEffectWasNotSeen
  var sideEffectWasIsolated = true
  it("test one") {
    sideEffectWasNotSeen = sideEffectWasNotSeen && sideEffectWasIsolated
    sideEffectWasIsolated = false
    succeed
  }
  it("test two") {
    sideEffectWasNotSeen = sideEffectWasNotSeen && sideEffectWasIsolated
    sideEffectWasIsolated = false
    succeed
  }
  it("test three") {
    sideEffectWasNotSeen = sideEffectWasNotSeen && sideEffectWasIsolated
    sideEffectWasIsolated = false
    succeed
  }
  override def newInstance: Suite with OneInstancePerTest = new TopLevelSpec
}
object TopLevelSpec {
  var sideEffectWasNotSeen = true
}

class OneInstancePerTestSpec extends FunSpec {
  describe("The OneInstancePerTest trait") {
    it("should isolate side effects from one test to the next in a top level Suite class that does not override newInstance") {
      var sideEffectWasNotSeen = true
      class MySpec extends FunSpec with OneInstancePerTest {
        var sideEffectWasIsolated = true
        it("test one") {
          sideEffectWasNotSeen = sideEffectWasNotSeen && sideEffectWasIsolated
          sideEffectWasIsolated = false
          succeed
        }
        it("test two") {
          sideEffectWasNotSeen = sideEffectWasNotSeen && sideEffectWasIsolated
          sideEffectWasIsolated = false
          succeed
        }
        it("test three") {
          sideEffectWasNotSeen = sideEffectWasNotSeen && sideEffectWasIsolated
          sideEffectWasIsolated = false
          succeed
        }
        override def newInstance = new MySpec
      }
      val suite = new MySpec
      suite.run(None, Args(SilentReporter))
      assert(sideEffectWasNotSeen)
    }
    it("should isolate side effects from one test to the next in an inner Suite class that overrides newInstance") {
      val suite = new TopLevelSpec
      suite.run(None, Args(SilentReporter))
      assert(TopLevelSpec.sideEffectWasNotSeen)
    }
    it("should send TestIgnored for an ignored test") {

      var aTheTestThisCalled = false
      var aTheTestThatCalled = false
      class ASpec extends WordSpec with OneInstancePerTest {
        "test this" in { aTheTestThisCalled = true; succeed }
        "test that" in { aTheTestThatCalled = true; succeed }
        override def newInstance = new ASpec
      }
      val a = new ASpec

      val repA = new TestIgnoredTrackingReporter
      a.run(None, Args(repA))
      assert(!repA.testIgnoredReceived)
      assert(aTheTestThisCalled)
      assert(aTheTestThatCalled)

      var bTheTestThisCalled = false
      var bTheTestThatCalled = false
      class BSpec extends WordSpec with OneInstancePerTest {
        "test this" ignore { bTheTestThisCalled = true; succeed }
        "test that" in { bTheTestThatCalled = true; succeed }
        override def newInstance = new BSpec
      }
      val b = new BSpec

      val repB = new TestIgnoredTrackingReporter
      b.run(None, Args(repB))
      assert(repB.testIgnoredReceived)
      assert(repB.lastEvent.isDefined)
      assert(repB.lastEvent.get.testName endsWith "test this")
      assert(!bTheTestThisCalled)
      assert(bTheTestThatCalled)

      var cTheTestThisCalled = false
      var cTheTestThatCalled = false
      class CSpec extends WordSpec with OneInstancePerTest {
        "test this" in { cTheTestThisCalled = true; succeed }
        "test that" ignore { cTheTestThatCalled = true; succeed }
        override def newInstance = new CSpec
      }
      val c = new CSpec

      val repC = new TestIgnoredTrackingReporter
      c.run(None, Args(repC))
      assert(repC.testIgnoredReceived)
      assert(repC.lastEvent.isDefined)
      assert(repC.lastEvent.get.testName endsWith "test that", repC.lastEvent.get.testName)
      assert(cTheTestThisCalled)
      assert(!cTheTestThatCalled)

      // The order I want is order of appearance in the file.
      // Will try and implement that tomorrow. Subtypes will be able to change the order.
      var dTheTestThisCalled = false
      var dTheTestThatCalled = false
      class DSpec extends WordSpec with OneInstancePerTest {
        "test this" ignore { dTheTestThisCalled = true; succeed }
        "test that" ignore { dTheTestThatCalled = true; succeed }
        override def newInstance = new DSpec
      }
      val d = new DSpec

      val repD = new TestIgnoredTrackingReporter
      d.run(None, Args(repD))
      assert(repD.testIgnoredReceived)
      assert(repD.lastEvent.isDefined)
      assert(repD.lastEvent.get.testName endsWith "test that") // last because should be in order of appearance
      assert(!dTheTestThisCalled)
      assert(!dTheTestThatCalled)
    }
    it("should ignore a test marked as ignored if it is passed in a Some as testName") {
      var bTheTestThisCalled = false
      var bTheTestThatCalled = false
      class BSpec extends WordSpec with OneInstancePerTest {
        "test this" ignore { bTheTestThisCalled = true; succeed }
        "test that" in { bTheTestThatCalled = true; succeed }
        override def newInstance = new BSpec
      }
      val b = new BSpec

      val repB = new TestIgnoredTrackingReporter
      b.run(Some("test this"), Args(repB))
      assert(repB.testIgnoredReceived)
      assert(!bTheTestThisCalled)
      assert(!bTheTestThatCalled)
    }

    it("should throw IllegalArgumentException from runTests if runTestInNewInstance is set but testName is empty") {

      class ASpec extends WordSpec with OneInstancePerTest {
        "test this" ignore { succeed }
        "test that" in { succeed }
        override def newInstance = new ASpec
        def invokeRunTests() {
          this.runTests(None, Args(SilentReporter, runTestInNewInstance = true))
        }
      }

      val aSpec = new ASpec
      assertThrows[IllegalArgumentException] {
        aSpec.invokeRunTests()
      }
    }
    
    it("should throw NullArgumentException from runTests if either passed-in argument is null") {
      class ASpec extends WordSpec with OneInstancePerTest {
        "test this" ignore { succeed }
        "test that" in { succeed }
        override def newInstance = new ASpec
        def invokeRunTests(testName: Option[String], args: Args) {
          this.runTests(testName, args)
        }
      }

      val aSpec = new ASpec
      assertThrows[NullArgumentException] {
          aSpec.invokeRunTests(Some("aTestName"), null)
      }
      assertThrows[NullArgumentException] {
          aSpec.invokeRunTests(null, Args(SilentReporter))
      }
    }
    
    it("should only execute nested suites in outer instance") {
      
      class InnerSuite extends FunSuite {
        test("hi") { info("hi info"); succeed }
      }
      
      class OuterSuite extends FunSuite with OneInstancePerTest {
        override def nestedSuites = Vector(new InnerSuite)
        test("outer 1") { info("outer 1 info"); succeed }
        test("outer 2") { info("outer 2 info"); succeed }
        
        override def newInstance = new OuterSuite
      }
      
      val rep = new EventRecordingReporter
      val outer = new OuterSuite
      outer.run(None, Args(rep))
      
      assert(rep.testStartingEventsReceived.size === 3)
      val testSucceededEvents = rep.testSucceededEventsReceived
      assert(testSucceededEvents.size === 3)
      testSucceededEvents.foreach { e => 
        e.testName match {
          case "hi" => 
            assert(e.recordedEvents.size === 1)
            assert(e.recordedEvents(0).asInstanceOf[InfoProvided].message === "hi info")
          case "outer 1" => 
            assert(e.recordedEvents.size === 1)
            assert(e.recordedEvents(0).asInstanceOf[InfoProvided].message === "outer 1 info")
          case "outer 2" => 
            assert(e.recordedEvents.size === 1)
            assert(e.recordedEvents(0).asInstanceOf[InfoProvided].message === "outer 2 info")
          case other => 
            fail("Unexpected TestSucceeded event: " + other)
        }
      }
      succeed
    }

    it("should only run before / after once per test regardless of order of mix-ins") {
      val beforeCountFoo = new AtomicInteger(0)
      val testCountFoo = new AtomicInteger(0)
      val afterCountFoo = new AtomicInteger(0)

      class FooSuite extends FunSuite with BeforeAndAfter with OneInstancePerTest {
        before {
          beforeCountFoo.incrementAndGet()
        }
        test("foo") {
          testCountFoo.incrementAndGet()
          succeed
        }
        after {
          afterCountFoo.incrementAndGet()
        }
        override def newInstance = new FooSuite
      }

      val repFoo = new EventRecordingReporter
      val outerFoo = new FooSuite
      outerFoo.run(None, Args(repFoo))

      assert(repFoo.testSucceededEventsReceived.size === 1)
      assert(beforeCountFoo.get === 1)
      assert(testCountFoo.get === 1)
      assert(afterCountFoo.get === 1)

      // make sure we get the same behavior when the order of the mix-ins is swapped

      val beforeCountBar = new AtomicInteger(0)
      val testCountBar = new AtomicInteger(0)
      val afterCountBar = new AtomicInteger(0)

      class BarSuite extends FunSuite with OneInstancePerTest with BeforeAndAfter {
        before {
          beforeCountBar.incrementAndGet()
        }
        test("bar") {
          testCountBar.incrementAndGet()
          succeed
        }
        after {
          afterCountBar.incrementAndGet()
        }
        override def newInstance = new BarSuite
      }

      val repBar = new EventRecordingReporter
      val outerBar = new BarSuite
      outerBar.run(None, Args(repBar))
      assert(repBar.testSucceededEventsReceived.size === 1)
      assert(beforeCountBar.get === 1)
      assert(testCountBar.get === 1)
      assert(afterCountBar.get === 1)
    }

    it("should only run beforeEach / afterEach once per test regardless of order of mix-ins") {
      val beforeCountFoo = new AtomicInteger(0)
      val testCountFoo = new AtomicInteger(0)
      val afterCountFoo = new AtomicInteger(0)

      class FooSuite extends FunSuite with BeforeAndAfterEach with OneInstancePerTest {
        override def beforeEach() = {
          beforeCountFoo.incrementAndGet()
        }
        test("foo") {
          testCountFoo.incrementAndGet()
          succeed
        }
        override def afterEach() = {
          afterCountFoo.incrementAndGet()
        }
        override def newInstance = new FooSuite
      }

      val repFoo = new EventRecordingReporter
      val outerFoo = new FooSuite
      outerFoo.run(None, Args(repFoo))

      assert(repFoo.testSucceededEventsReceived.size === 1)
      assert(beforeCountFoo.get === 1)
      assert(testCountFoo.get === 1)
      assert(afterCountFoo.get === 1)

      // make sure we get the same behavior when the order of the mix-ins is swapped

      val beforeCountBar = new AtomicInteger(0)
      val testCountBar = new AtomicInteger(0)
      val afterCountBar = new AtomicInteger(0)

      class BarSuite extends FunSuite with OneInstancePerTest with BeforeAndAfterEach {
        override def beforeEach() = {
          beforeCountBar.incrementAndGet()
        }
        test("bar") {
          testCountBar.incrementAndGet()
          succeed
        }
        override def afterEach() = {
          afterCountBar.incrementAndGet()
        }
        override def newInstance = new BarSuite
      }

      val repBar = new EventRecordingReporter
      val outerBar = new BarSuite
      outerBar.run(None, Args(repBar))
      assert(repBar.testSucceededEventsReceived.size === 1)
      assert(beforeCountBar.get === 1)
      assert(testCountBar.get === 1)
      assert(afterCountBar.get === 1)
    }

    it("should only run beforeEach / afterEach (TestData form) once per test regardless of order of mix-ins") {
      val beforeCountFoo = new AtomicInteger(0)
      val testCountFoo = new AtomicInteger(0)
      val afterCountFoo = new AtomicInteger(0)

      class FooSuite extends FunSuite with BeforeAndAfterEachTestData with OneInstancePerTest {
        override def beforeEach(td: TestData) = {
          beforeCountFoo.incrementAndGet()
        }
        test("foo") {
          testCountFoo.incrementAndGet()
          succeed
        }
        override def afterEach(td: TestData) = {
          afterCountFoo.incrementAndGet()
        }
        override def newInstance = new FooSuite
      }

      val repFoo = new EventRecordingReporter
      val outerFoo = new FooSuite
      outerFoo.run(None, Args(repFoo))

      assert(repFoo.testSucceededEventsReceived.size === 1)
      assert(beforeCountFoo.get === 1)
      assert(testCountFoo.get === 1)
      assert(afterCountFoo.get === 1)

      // make sure we get the same behavior when the order of the mix-ins is swapped

      val beforeCountBar = new AtomicInteger(0)
      val testCountBar = new AtomicInteger(0)
      val afterCountBar = new AtomicInteger(0)

      class BarSuite extends FunSuite with OneInstancePerTest with BeforeAndAfterEachTestData {
        override def beforeEach(td: TestData) = {
          beforeCountBar.incrementAndGet()
        }
        test("bar") {
          testCountBar.incrementAndGet()
          succeed
        }
        override def afterEach(td: TestData) = {
          afterCountBar.incrementAndGet()
        }
        override def newInstance = new BarSuite
      }

      val repBar = new EventRecordingReporter
      val outerBar = new BarSuite
      outerBar.run(None, Args(repBar))
      assert(repBar.testSucceededEventsReceived.size === 1)
      assert(beforeCountBar.get === 1)
      assert(testCountBar.get === 1)
      assert(afterCountBar.get === 1)
    }

    it("should only run beforeAll / afterAll once per suite regardless of order of mix-ins") {
      val beforeCountFoo = new AtomicInteger(0)
      val testCountFoo = new AtomicInteger(0)
      val afterCountFoo = new AtomicInteger(0)

      class FooSuite extends FunSuite with BeforeAndAfterAll with OneInstancePerTest {
        override def beforeAll() = {
          beforeCountFoo.incrementAndGet()
        }
        test("foo") {
          testCountFoo.incrementAndGet()
          succeed
        }
        override def afterAll() = {
          afterCountFoo.incrementAndGet()
        }
        override def newInstance = new FooSuite
      }

      val repFoo = new EventRecordingReporter
      val outerFoo = new FooSuite
      outerFoo.run(None, Args(repFoo))

      assert(repFoo.testSucceededEventsReceived.size === 1)
      assert(beforeCountFoo.get === 1)
      assert(testCountFoo.get === 1)
      assert(afterCountFoo.get === 1)

      // make sure we get the same behavior when the order of the mix-ins is swapped

      val beforeCountBar = new AtomicInteger(0)
      val testCountBar = new AtomicInteger(0)
      val afterCountBar = new AtomicInteger(0)

      class BarSuite extends FunSuite with OneInstancePerTest with BeforeAndAfterAll {
        override def beforeAll() = {
          beforeCountBar.incrementAndGet()
        }
        test("bar") {
          testCountBar.incrementAndGet()
          succeed
        }
        override def afterAll() {
          afterCountBar.incrementAndGet()
        }
        override def newInstance = new BarSuite
      }

      val repBar = new EventRecordingReporter
      val outerBar = new BarSuite
      outerBar.run(None, Args(repBar))
      assert(repBar.testSucceededEventsReceived.size === 1)
      assert(beforeCountBar.get === 1)
      assert(testCountBar.get === 1)
      assert(afterCountBar.get === 1)
    }

    it("should only run beforeAll / afterAll (ConfigMap form) once per suite regardless of order of mix-ins") {
      val beforeCountFoo = new AtomicInteger(0)
      val testCountFoo = new AtomicInteger(0)
      val afterCountFoo = new AtomicInteger(0)

      class FooSuite extends FunSuite with BeforeAndAfterAllConfigMap with OneInstancePerTest {
        override def beforeAll(cm: ConfigMap) = {
          beforeCountFoo.incrementAndGet()
        }
        test("foo") {
          testCountFoo.incrementAndGet()
          succeed
        }
        override def afterAll(cm: ConfigMap) = {
          afterCountFoo.incrementAndGet()
        }
        override def newInstance = new FooSuite
      }

      val repFoo = new EventRecordingReporter
      val outerFoo = new FooSuite
      outerFoo.run(None, Args(repFoo))

      assert(repFoo.testSucceededEventsReceived.size === 1)
      assert(beforeCountFoo.get === 1)
      assert(testCountFoo.get === 1)
      assert(afterCountFoo.get === 1)

      // make sure we get the same behavior when the order of the mix-ins is swapped

      val beforeCountBar = new AtomicInteger(0)
      val testCountBar = new AtomicInteger(0)
      val afterCountBar = new AtomicInteger(0)

      class BarSuite extends FunSuite with OneInstancePerTest with BeforeAndAfterAllConfigMap {
        override def beforeAll(cm: ConfigMap) = {
          beforeCountBar.incrementAndGet()
        }
        test("bar") {
          testCountBar.incrementAndGet()
          succeed
        }
        override def afterAll(cm: ConfigMap) {
          afterCountBar.incrementAndGet()
        }
        override def newInstance = new BarSuite
      }

      val repBar = new EventRecordingReporter
      val outerBar = new BarSuite
      outerBar.run(None, Args(repBar))
      assert(repBar.testSucceededEventsReceived.size === 1)
      assert(beforeCountBar.get === 1)
      assert(testCountBar.get === 1)
      assert(afterCountBar.get === 1)
    }
  }
}
