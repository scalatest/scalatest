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
import collection.immutable.TreeSet
import helpers._
import org.scalatest.events._

class JUnit3SuiteSpec extends FunSpec with SharedHelpers {

  describe("A JUnit3Suite") {
    it("should return the test names in alphabetical order from testNames") {
      val a = new JUnit3Suite {
        def testThis() {}
        def testThat() {}
      }

      assertResult(List("testThat", "testThis")) {
        a.testNames.iterator.toList
      }

      val b = new JUnit3Suite {}

      assertResult(List[String]()) {
        b.testNames.iterator.toList
      }

      val c = new JUnit3Suite {
        def testThat() {}
        def testThis() {}
      }

      assertResult(List("testThat", "testThis")) {
        c.testNames.iterator.toList
      }
    }

    it("should return the proper testNames for test methods whether or not they take an Informer") {

      val a = new JUnit3Suite {
        def testThis() = ()
        def testThat(info: Informer) = ()
      }
      assert(a.testNames === TreeSet("testThis"))

      val b = new JUnit3Suite {}
      assert(b.testNames === TreeSet[String]())
    }

    it("should not return names of methods that start with test, take no params, but have a return type " +
            "other than Unit from testNames") {
      
      val a = new TestWithNonUnitMethod
      assert(a.testNames === TreeSet("testThat", "testThis"))
    }

    it("should include in testNames a method simply named 'test', that takes no params and has a return type " +
            "of Unit") {

      val a = new TestWithMethodNamedTest
      assert(a.testNames === TreeSet("test", "testThat", "testThis"))
    }

    it("should return an empty tags map from the tags method, because a tag-like concept isn't supported in JUnit 3") {

      val a = new JUnit3Suite {
        @Ignore
        def testThis() = ()
        def testThat(info: Informer) = ()
      }

      assert(a.tags.isEmpty)

      val b = new JUnit3Suite {
        def testThis() = ()
        @Ignore
        def testThat(info: Informer) = ()
      }

      assert(b.tags.isEmpty)

      val c = new JUnit3Suite {
        @Ignore
        def testThis() = ()
        @Ignore
        def testThat(info: Informer) = ()
      }

      assert(c.tags.isEmpty)

      val d = new JUnit3Suite {
        @SlowAsMolasses
        def testThis() = ()
        @SlowAsMolasses
        @Ignore
        def testThat(info: Informer) = ()
      }

      assert(d.tags.isEmpty)

      val e = new JUnit3Suite {}
      assert(e.tags.isEmpty)
    }

    it("should execute all tests when run is called with testName None") {

      TestWasCalledSuite.reinitialize()
      
      val b = new TestWasCalledSuite
      b.run(None, Args(SilentReporter))
      assert(TestWasCalledSuite.theTestThisCalled)
      assert(TestWasCalledSuite.theTestThatCalled)
    }

    it("should execute one test when run is called with a defined testName") {

      TestWasCalledSuite.reinitialize()

      val a = new TestWasCalledSuite
      a.run(Some("testThis"), Args(SilentReporter))
      assert(TestWasCalledSuite.theTestThisCalled)
      assert(!TestWasCalledSuite.theTestThatCalled)
    }

    it("should throw IllegalArgumentException if run is passed a testName that does not exist") {

      val a = new TestWasCalledSuite
      intercept[IllegalArgumentException] {
        // Here, they forgot that the name is actually testThis(Fixture)
        a.run(Some("misspelled"), Args(SilentReporter))
      }
    }

    it("should run no tests if tags to include is non-empty") {

      TestWasCalledSuite.reinitialize()

      val a = new TestWasCalledSuite
      a.run(None, Args(SilentReporter, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!TestWasCalledSuite.theTestThisCalled)
      assert(!TestWasCalledSuite.theTestThatCalled)
    }

    it("should return the correct test count from its expectedTestCount method") {

      val a = new ASuite
      assert(a.expectedTestCount(Filter()) === 1)

      val b = new BSuite
      assert(b.expectedTestCount(Filter()) === 1)

      val c = new CSuite
      assert(c.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 0)
      assert(c.expectedTestCount(Filter(None, Set("org.scalatest.FastAsLight"))) === 1)

      val d = new DSuite
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 0)
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight"))) === 0)
      assert(d.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) === 4)
      assert(d.expectedTestCount(Filter()) === 4)

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
