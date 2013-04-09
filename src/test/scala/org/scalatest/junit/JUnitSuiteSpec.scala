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
package org.scalatest.junit

import org.scalatest._
import scala.collection.immutable.TreeSet
import org.scalatest.junit.junit4helpers._
import org.junit.Test
import org.junit.Ignore

class JUnitSuiteSpec extends FunSpec with SharedHelpers {

  describe("A JUnitSuite") {
    it("should return the test names in alphabetical order from testNames") {
      val a = new JUnitSuite {
        @Test def doThis() {}
        @Test def doThat() {}
      }

      assertResult(List("doThat", "doThis")) {
        a.testNames.iterator.toList
      }

      val b = new JUnitSuite {}

      assertResult(List[String]()) {
        b.testNames.iterator.toList
      }

      val c = new JUnitSuite {
        @Test def doThat() {}
        @Test def doThis() {}
      }

      assertResult(List("doThat", "doThis")) {
        c.testNames.iterator.toList
      }
    }

    it("should return the proper testNames for test methods whether or not they take an Informer") {

      val a = new JUnitSuite {
        @Test def doThis() = ()
        @Test def doThat(info: Informer) = ()
      }
      assert(a.testNames === TreeSet("doThis"))

      val b = new JUnitSuite {}
      assert(b.testNames === TreeSet[String]())
    }
 
    it("should return names of methods that are annotated with Test, take no params, but have a return type " +
            "other than Unit from testNames") {

      val a = new TestWithNonUnitMethod
      assert(a.testNames === TreeSet("doThat", "doTheOtherThing", "doThis"))
    }

    it("should return a tags map from the tags method that contains only methods marked with org.junit.Ignore") {

      val a = new JUnitSuite {
        @Ignore
        @Test def testThis() = ()
        @Test def testThat() = ()
      }

      assert(a.tags === Map("testThis" -> Set("org.scalatest.Ignore")))

      val b = new JUnitSuite {
        @Test def testThis() = ()
        @Ignore
        @Test def testThat() = ()
      }

      assert(b.tags === Map("testThat" -> Set("org.scalatest.Ignore")))

      val c = new JUnitSuite {
        @Ignore
        @Test def testThis() = ()
        @Ignore
        @Test def testThat() = ()
      }

      assert(c.tags === Map("testThis" -> Set("org.scalatest.Ignore"), "testThat" -> Set("org.scalatest.Ignore")))

      val d = new JUnitSuite {
        @SlowAsMolasses
        @Test def testThis() = ()
        @SlowAsMolasses
        @Ignore
        @Test def testThat() = ()
      }

      assert(d.tags === Map("testThat" -> Set("org.scalatest.Ignore")))

      val e = new JUnitSuite {}
      assert(e.tags === Map())
    }

    it("should execute all tests when run is called with testName None") {

      TestWasCalledSuite.reinitialize()

      val b = new TestWasCalledSuite
      b.run(None, Args(SilentReporter))
      assert(TestWasCalledSuite.theDoThisCalled)
      assert(TestWasCalledSuite.theDoThatCalled)
    }

    it("should execute one test when run is called with a defined testName") {

      TestWasCalledSuite.reinitialize()

      val a = new TestWasCalledSuite
      a.run(Some("doThis"), Args(SilentReporter))
      assert(TestWasCalledSuite.theDoThisCalled)
      assert(!TestWasCalledSuite.theDoThatCalled)
    }

    it("should throw IllegalArgumentException if run is passed a testName that does not exist") {

      val a = new TestWasCalledSuite
      intercept[IllegalArgumentException] {
        // Here, they forgot that the name is actually doThis(Fixture)
        a.run(Some("misspelled"), Args(SilentReporter))
      }
    }

    it("should run no tests if tags to include is non-empty") {

      TestWasCalledSuite.reinitialize()

      val a = new TestWasCalledSuite
      a.run(None, Args(SilentReporter, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!TestWasCalledSuite.theDoThisCalled)
      assert(!TestWasCalledSuite.theDoThatCalled)
    }
        
    it("should return the correct test count from its expectedTestCount method") {

      val a = new ASuite
      assert(a.expectedTestCount(Filter()) === 1)

      val b = new BSuite
      assert(b.expectedTestCount(Filter()) === 0)

      val c = new org.scalatest.junit.junit4helpers.CSuite
      assert(c.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 0)
      assert(c.expectedTestCount(Filter(None, Set("org.scalatest.FastAsLight"))) === 1)

      val d = new DSuite
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 0)
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight"))) === 0)
      assert(d.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) === 6)
      assert(d.expectedTestCount(Filter()) === 6)

      val e = new ESuite
      assert(e.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 0)
      assert(e.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight"))) === 0)
      assert(e.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) === 1)
      assert(e.expectedTestCount(Filter()) === 1)
    }

    it("should generate a test failure if a Throwable, or an Error other than direct Error subtypes " +
            "known in JDK 1.5, excluding AssertionError") {
      val a = new ShouldFailSuite
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      val tf = rep.testFailedEventsReceived
      assert(tf.size === 3)
    }
  }
}
