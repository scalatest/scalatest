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

import collection.immutable.TreeSet
import org.scalatest.events._
import scala.reflect.NameTransformer.encode
import SharedHelpers._
/* Uncomment after remove type aliases in org.scalatest package object
import org.scalatest.exceptions.TestFailedException
import org.scalatest.exceptions.TestPendingException
*/

class MandarinOrangeFunSuite(ns: Suite*) extends FunSuite {
  override def nestedSuites = Vector.empty ++ ns // ns.toVector
}
class MandarinOrangeFunSpec(ns: Suite*) extends FunSpec {
  override def nestedSuites = Vector.empty ++ ns // ns.toVector
}
class MandarinOrangeSpec(ns: Suite*) extends Spec {
  override def nestedSuites = Vector.empty ++ ns // ns.toVector
}
class MandarinOrangeWordSpec(ns: Suite*) extends WordSpec {
  override def nestedSuites = Vector.empty ++ ns // ns.toVector
}
class MandarinOrangeFlatSpec(ns: Suite*) extends FlatSpec {
  override def nestedSuites = Vector.empty ++ ns // ns.toVector
}
class MandarinOrangeFreeSpec(ns: Suite*) extends FreeSpec {
  override def nestedSuites = Vector.empty ++ ns // ns.toVector
}
class MandarinOrangeFeatureSpec(ns: Suite*) extends FeatureSpec {
  override def nestedSuites = Vector.empty ++ ns // ns.toVector
}
class MandarinOrangePropSpec(ns: Suite*) extends PropSpec {
  override def nestedSuites = Vector.empty ++ ns // ns.toVector
}

// Named these with a MandarinOrange prefix so they wouldn't confict
// with anything else in the test suite. These need to be top level
// else they end up with dollar signs in the names.
trait MandarinOrangeFixture { this: fixture.Suite =>
  type FixtureParam = String
  def withFixture(test: OneArgTest): Outcome = { test("hi") }
}

class MandarinOrangeFixtureFunSuite(ns: Suite*) extends fixture.FunSuite with MandarinOrangeFixture {
  override def nestedSuites = Vector.empty ++ ns // ns.toVector
}
class MandarinOrangeFixtureFunSpec(ns: Suite*) extends fixture.FunSpec with MandarinOrangeFixture {
  override def nestedSuites = Vector.empty ++ ns // ns.toVector
}
class MandarinOrangeFixtureSpec(ns: Suite*) extends fixture.Spec with MandarinOrangeFixture {
  override def nestedSuites = Vector.empty ++ ns // ns.toVector
}
class MandarinOrangeFixtureWordSpec(ns: Suite*) extends fixture.WordSpec with MandarinOrangeFixture {
  override def nestedSuites = Vector.empty ++ ns // ns.toVector
}
class MandarinOrangeFixtureFlatSpec(ns: Suite*) extends fixture.FlatSpec with MandarinOrangeFixture {
  override def nestedSuites = Vector.empty ++ ns // ns.toVector
}
class MandarinOrangeFixtureFreeSpec(ns: Suite*) extends fixture.FreeSpec with MandarinOrangeFixture {
  override def nestedSuites = Vector.empty ++ ns // ns.toVector
}
class MandarinOrangeFixtureFeatureSpec(ns: Suite*) extends fixture.FeatureSpec with MandarinOrangeFixture {
  override def nestedSuites = Vector.empty ++ ns // ns.toVector
}
class MandarinOrangeFixturePropSpec(ns: Suite*) extends fixture.PropSpec with MandarinOrangeFixture {
  override def nestedSuites = Vector.empty ++ ns // ns.toVector
}

class MandarinOrangeSuites(suites: Suite*) extends Suites(suites: _*)
class MandarinOrangeSpecs(suites: Suite*) extends Specs(suites: _*)
class MandarinOrangeSequential(suites: Suite*) extends Sequential(suites: _*)
class MandarinOrangeStepwise(suites: Suite*) extends Stepwise(suites: _*)

class SuiteSpec extends FunSpec with PrivateMethodTester {

  describe("the toString method on Suites and SuiteLike traits other than TestNGSuiteLike") {
    describe("when the suite contains no nested suites") {
      it("should return the simple name of the class (and no parens)") {
        import prop.TableDrivenPropertyChecks._
        val examples =
          Table(

            ( "suite", "simple name"),

            ( new FunSuite, "FunSuite"),
            ( new FunSpec, "FunSpec"),
            ( new Spec, "Spec"),
            ( new WordSpec, "WordSpec"),
            ( new FlatSpec, "FlatSpec"),
            ( new FreeSpec, "FreeSpec"),
            ( new FeatureSpec, "FeatureSpec"),
            ( new PropSpec, "PropSpec"),

            ( new MandarinOrangeFunSuite, "MandarinOrangeFunSuite"),
            ( new MandarinOrangeFunSpec, "MandarinOrangeFunSpec"),
            ( new MandarinOrangeSpec, "MandarinOrangeSpec"),
            ( new MandarinOrangeWordSpec, "MandarinOrangeWordSpec"),
            ( new MandarinOrangeFlatSpec, "MandarinOrangeFlatSpec"),
            ( new MandarinOrangeFreeSpec, "MandarinOrangeFreeSpec"),
            ( new MandarinOrangeFeatureSpec, "MandarinOrangeFeatureSpec"),
            ( new MandarinOrangePropSpec, "MandarinOrangePropSpec"),

            ( new MandarinOrangeFixtureFunSuite, "MandarinOrangeFixtureFunSuite"),
            ( new MandarinOrangeFixtureFunSpec, "MandarinOrangeFixtureFunSpec"),
            ( new MandarinOrangeFixtureSpec, "MandarinOrangeFixtureSpec"),
            ( new MandarinOrangeFixtureWordSpec, "MandarinOrangeFixtureWordSpec"),
            ( new MandarinOrangeFixtureFlatSpec, "MandarinOrangeFixtureFlatSpec"),
            ( new MandarinOrangeFixtureFreeSpec, "MandarinOrangeFixtureFreeSpec"),
            ( new MandarinOrangeFixtureFeatureSpec, "MandarinOrangeFixtureFeatureSpec"),
            ( new MandarinOrangeFixturePropSpec, "MandarinOrangeFixturePropSpec"),

            // ( new path.FunSpec, "path.FunSpec"),
            // ( new path.FreeSpec, "path.FreeSpec"),

            ( new Suites, "Suites"),
            ( new Specs, "Specs"), // Will deprecate this one
            ( new Sequential, "Sequential"),
            ( new Stepwise, "Stepwise"),

            ( new MandarinOrangeSuites, "MandarinOrangeSuites"),
            ( new MandarinOrangeSpecs, "MandarinOrangeSpecs"), // Will deprecate this one
            ( new MandarinOrangeSequential, "MandarinOrangeSequential"),
            ( new MandarinOrangeStepwise, "MandarinOrangeStepwise")
          )
        forAll (examples) { (suite, simpleName) =>
          assert(suite.toString === simpleName)
        }
      }
    }
    describe("when the suite contains one nested suite") {
      it("should return the simple name of the class and the nested suite toString wrapped in parens") {
        import prop.TableDrivenPropertyChecks._
        val examples =
          Table(

            ( "suite", "simple name"),

            ( new MandarinOrangeFunSuite(new FunSuite), "MandarinOrangeFunSuite(FunSuite)"),
            ( new MandarinOrangeFunSpec(new FunSuite), "MandarinOrangeFunSpec(FunSuite)"),
            ( new MandarinOrangeSpec(new FunSuite), "MandarinOrangeSpec(FunSuite)"),
            ( new MandarinOrangeWordSpec(new FunSuite), "MandarinOrangeWordSpec(FunSuite)"),
            ( new MandarinOrangeFlatSpec(new FunSuite), "MandarinOrangeFlatSpec(FunSuite)"),
            ( new MandarinOrangeFreeSpec(new FunSuite), "MandarinOrangeFreeSpec(FunSuite)"),
            ( new MandarinOrangeFeatureSpec(new FunSuite), "MandarinOrangeFeatureSpec(FunSuite)"),
            ( new MandarinOrangePropSpec(new FunSuite), "MandarinOrangePropSpec(FunSuite)"),

            ( new MandarinOrangeFixtureFunSuite(new FunSuite), "MandarinOrangeFixtureFunSuite(FunSuite)"),
            ( new MandarinOrangeFixtureFunSpec(new FunSuite), "MandarinOrangeFixtureFunSpec(FunSuite)"),
            ( new MandarinOrangeFixtureSpec(new FunSuite), "MandarinOrangeFixtureSpec(FunSuite)"),
            ( new MandarinOrangeFixtureWordSpec(new FunSuite), "MandarinOrangeFixtureWordSpec(FunSuite)"),
            ( new MandarinOrangeFixtureFlatSpec(new FunSuite), "MandarinOrangeFixtureFlatSpec(FunSuite)"),
            ( new MandarinOrangeFixtureFreeSpec(new FunSuite), "MandarinOrangeFixtureFreeSpec(FunSuite)"),
            ( new MandarinOrangeFixtureFeatureSpec(new FunSuite), "MandarinOrangeFixtureFeatureSpec(FunSuite)"),
            ( new MandarinOrangeFixturePropSpec(new FunSuite), "MandarinOrangeFixturePropSpec(FunSuite)"),

            // ( new path.FunSpec(new FunSuite), "path.FunSpec(FunSuite)"),
            // ( new path.FreeSpec(new FunSuite), "path.FreeSpec(FunSuite)"),

            ( new Suites(new FunSuite), "Suites(FunSuite)"),
            ( new Specs(new FunSuite), "Specs(FunSuite)"), // Will deprecate this one
            ( new Sequential(new FunSuite), "Sequential(FunSuite)"),
            ( new Stepwise(new FunSuite), "Stepwise(FunSuite)"),

            ( new MandarinOrangeSuites(new FunSuite), "MandarinOrangeSuites(FunSuite)"),
            ( new MandarinOrangeSpecs(new FunSuite), "MandarinOrangeSpecs(FunSuite)"), // Will deprecate this one
            ( new MandarinOrangeSequential(new FunSuite), "MandarinOrangeSequential(FunSuite)"),
            ( new MandarinOrangeStepwise(new FunSuite), "MandarinOrangeStepwise(FunSuite)")
          )
        forAll (examples) { (suite, simpleName) =>
          assert(suite.toString === simpleName)
        }
      }
    }
    describe("when the suite contains more than one nested suite") {
      it("should return the simple name of the class and the nested suite toStrings wrapped in parens and separated by commas") {
        import prop.TableDrivenPropertyChecks._
        val examples =
          Table(

            ( "suite", "simple name"),

            ( new MandarinOrangeFunSuite(new PropSpec, new FeatureSpec, new FunSuite), "MandarinOrangeFunSuite(PropSpec, FeatureSpec, FunSuite)"),
            ( new MandarinOrangeFunSpec(new PropSpec, new FeatureSpec, new FunSuite), "MandarinOrangeFunSpec(PropSpec, FeatureSpec, FunSuite)"),
            ( new MandarinOrangeSpec(new PropSpec, new FeatureSpec, new FunSuite), "MandarinOrangeSpec(PropSpec, FeatureSpec, FunSuite)"),
            ( new MandarinOrangeWordSpec(new PropSpec, new FeatureSpec, new FunSuite), "MandarinOrangeWordSpec(PropSpec, FeatureSpec, FunSuite)"),
            ( new MandarinOrangeFlatSpec(new PropSpec, new FeatureSpec, new FunSuite), "MandarinOrangeFlatSpec(PropSpec, FeatureSpec, FunSuite)"),
            ( new MandarinOrangeFreeSpec(new PropSpec, new FeatureSpec, new FunSuite), "MandarinOrangeFreeSpec(PropSpec, FeatureSpec, FunSuite)"),
            ( new MandarinOrangeFeatureSpec(new PropSpec, new FeatureSpec, new FunSuite), "MandarinOrangeFeatureSpec(PropSpec, FeatureSpec, FunSuite)"),
            ( new MandarinOrangePropSpec(new PropSpec, new FeatureSpec, new FunSuite), "MandarinOrangePropSpec(PropSpec, FeatureSpec, FunSuite)"),

            ( new MandarinOrangeFixtureFunSuite(new PropSpec, new FeatureSpec, new FunSuite), "MandarinOrangeFixtureFunSuite(PropSpec, FeatureSpec, FunSuite)"),
            ( new MandarinOrangeFixtureFunSpec(new PropSpec, new FeatureSpec, new FunSuite), "MandarinOrangeFixtureFunSpec(PropSpec, FeatureSpec, FunSuite)"),
            ( new MandarinOrangeFixtureSpec(new PropSpec, new FeatureSpec, new FunSuite), "MandarinOrangeFixtureSpec(PropSpec, FeatureSpec, FunSuite)"),
            ( new MandarinOrangeFixtureWordSpec(new PropSpec, new FeatureSpec, new FunSuite), "MandarinOrangeFixtureWordSpec(PropSpec, FeatureSpec, FunSuite)"),
            ( new MandarinOrangeFixtureFlatSpec(new PropSpec, new FeatureSpec, new FunSuite), "MandarinOrangeFixtureFlatSpec(PropSpec, FeatureSpec, FunSuite)"),
            ( new MandarinOrangeFixtureFreeSpec(new PropSpec, new FeatureSpec, new FunSuite), "MandarinOrangeFixtureFreeSpec(PropSpec, FeatureSpec, FunSuite)"),
            ( new MandarinOrangeFixtureFeatureSpec(new PropSpec, new FeatureSpec, new FunSuite), "MandarinOrangeFixtureFeatureSpec(PropSpec, FeatureSpec, FunSuite)"),
            ( new MandarinOrangeFixturePropSpec(new PropSpec, new FeatureSpec, new FunSuite), "MandarinOrangeFixturePropSpec(PropSpec, FeatureSpec, FunSuite)"),

            // ( new path.FunSpec(new PropSpec, new FeatureSpec, new FunSuite), "path.FunSpec(PropSpec, FeatureSpec, FunSuite)"),
            // ( new path.FreeSpec(new PropSpec, new FeatureSpec, new FunSuite), "path.FreeSpec(PropSpec, FeatureSpec, FunSuite)"),

            ( new Suites(new PropSpec, new FeatureSpec, new FunSuite), "Suites(PropSpec, FeatureSpec, FunSuite)"),
            ( new Specs(new PropSpec, new FeatureSpec, new FunSuite), "Specs(PropSpec, FeatureSpec, FunSuite)"), // Will deprecate this one
            ( new Sequential(new PropSpec, new FeatureSpec, new FunSuite), "Sequential(PropSpec, FeatureSpec, FunSuite)"),
            ( new Stepwise(new PropSpec, new FeatureSpec, new FunSuite), "Stepwise(PropSpec, FeatureSpec, FunSuite)"),

            ( new MandarinOrangeSuites(new PropSpec, new FeatureSpec, new FunSuite), "MandarinOrangeSuites(PropSpec, FeatureSpec, FunSuite)"),
            ( new MandarinOrangeSpecs(new PropSpec, new FeatureSpec, new FunSuite), "MandarinOrangeSpecs(PropSpec, FeatureSpec, FunSuite)"), // Will deprecate this one
            ( new MandarinOrangeSequential(new PropSpec, new FeatureSpec, new FunSuite), "MandarinOrangeSequential(PropSpec, FeatureSpec, FunSuite)"),
            ( new MandarinOrangeStepwise(new PropSpec, new FeatureSpec, new FunSuite), "MandarinOrangeStepwise(PropSpec, FeatureSpec, FunSuite)")
          )
        forAll (examples) { (suite, simpleName) =>
          assert(suite.toString === simpleName)
        }
      }
    }
  }

  describe("The simpleNameForTest method") {
    it("should return the correct test simple name with or without Informer") {
      val simpleNameForTest = PrivateMethod[String]('simpleNameForTest)
      assert((Suite invokePrivate simpleNameForTest("testThis")) === "testThis")
      assert((Suite invokePrivate simpleNameForTest("testThis(Informer)")) === "testThis")
      assert((Suite invokePrivate simpleNameForTest("test(Informer)")) === "test")
      assert((Suite invokePrivate simpleNameForTest("test")) === "test")
    }
  }

  describe("A Suite") {
    it("should return the test names in alphabetical order from testNames") {
      val a = new Suite {
        def `test: this`() {}
        def `test: that`() {}
      }

      assertResult(List(encode("test: that"), encode("test: this"))) {
        a.testNames.iterator.toList
      }

      val b = new Suite {}

      assertResult(List[String]()) {
        b.testNames.iterator.toList
      }

      val c = new Suite {
        def `test: that`() {}
        def `test: this`() {}
      }

      assertResult(List(encode("test: that"), encode("test: this"))) {
        c.testNames.iterator.toList
      }
    }
    
    it("should return the proper testNames for test methods whether or not they take an Informer") {

      val a = new Suite {
        def `test: this`() = ()
        def `test: that`(r: Informer) = ()
      }
      assert(a.testNames === TreeSet(encode("test: that") + "(Informer)", encode("test: this")))

      val b = new Suite {}
      assert(b.testNames === TreeSet[String]())
    }

    class TestWasCalledSuite extends Suite {
      var theTestThisCalled = false
      var theTestThatCalled = false
      def `test: this`() { theTestThisCalled = true }
      def `test: that`() { theTestThatCalled = true }
    }

    it("should execute all tests when run is called with testName None") {

      val b = new TestWasCalledSuite
      b.run(None, Args(SilentReporter))
      assert(b.theTestThisCalled)
      assert(b.theTestThatCalled)
    }

    it("should execute one test when run is called with a defined testName") {

      val a = new TestWasCalledSuite
      a.run(Some(encode("test: this")), Args(SilentReporter))
      assert(a.theTestThisCalled)
      assert(!a.theTestThatCalled)
    }

    it("should report as ignored, ant not run, tests marked ignored") {

      val a = new Suite {
        var theTestThisCalled = false
        var theTestThatCalled = false
        def `test: this`() { theTestThisCalled = true }
        def `test: that`(r: Informer) { theTestThatCalled = true }
      }

      import scala.language.reflectiveCalls

      val repA = new TestIgnoredTrackingReporter
      a.run(None, Args(repA))
      assert(!repA.testIgnoredReceived)
      assert(a.theTestThisCalled)
      assert(a.theTestThatCalled)

      val b = new Suite {
        var theTestThisCalled = false
        var theTestThatCalled = false
        @Ignore
        def `test: this`() { theTestThisCalled = true }
        def `test: that`(r: Informer) { theTestThatCalled = true }
      }

      val repB = new TestIgnoredTrackingReporter
      b.run(None, Args(repB))
      assert(repB.testIgnoredReceived)
      assert(repB.lastEvent.isDefined)
      assert(repB.lastEvent.get.testName endsWith encode("test: this"))
      assert(!b.theTestThisCalled)
      assert(b.theTestThatCalled)

      val c = new Suite {
        var theTestThisCalled = false
        var theTestThatCalled = false
        def `test: this`() { theTestThisCalled = true }
        @Ignore
        def `test: that`(r: Informer) { theTestThatCalled = true }
      }

      val repC = new TestIgnoredTrackingReporter
      c.run(None, Args(repC))
      assert(repC.testIgnoredReceived)
      assert(repC.lastEvent.isDefined)
      assert(repC.lastEvent.get.testName endsWith encode("test: that") + "(Informer)", repC.lastEvent.get.testName)
      assert(c.theTestThisCalled)
      assert(!c.theTestThatCalled)

      val d = new Suite {
        var theTestThisCalled = false
        var theTestThatCalled = false
        @Ignore
        def `test: this`() { theTestThisCalled = true }
        @Ignore
        def `test: that`(r: Informer) { theTestThatCalled = true }
      }

      val repD = new TestIgnoredTrackingReporter
      d.run(None, Args(repD))
      assert(repD.testIgnoredReceived)
      assert(repD.lastEvent.isDefined)
      assert(repD.lastEvent.get.testName endsWith encode("test: this")) // last because run alphabetically
      assert(!d.theTestThisCalled)
      assert(!d.theTestThatCalled)
    }
    
    it("should ignore a test marked as ignored if run is invoked with that testName") {

      val e = new Suite {
        var theTestThisCalled = false
        var theTestThatCalled = false
        @Ignore
        def `test: this`() { theTestThisCalled = true }
        def `test: that`(r: Informer) { theTestThatCalled = true }
      }

      import scala.language.reflectiveCalls

      val repE = new TestIgnoredTrackingReporter
      e.run(Some(encode("test: this")), Args(repE))
      assert(repE.testIgnoredReceived)
      assert(!e.theTestThisCalled)
      assert(!e.theTestThatCalled)
    }

    it("should exclude a test with a tag included in the tagsToExclude set even if run is invoked with that testName") {

      val e = new Suite {
        var theTestThisCalled = false
        var theTestThatCalled = false
        @SlowAsMolasses
        def `test: this`() { theTestThisCalled = true }
        def `test: that`(r: Informer) { theTestThatCalled = true }
      }

      import scala.language.reflectiveCalls

      val repE = new TestIgnoredTrackingReporter
      e.run(Some(encode("test: this")), Args(repE, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repE.testIgnoredReceived)
      assert(!e.theTestThisCalled)
      assert(!e.theTestThatCalled)
    }

    it("should throw IllegalArgumentException if run is passed a testName that does not exist") {
      val suite = new Suite {
        var theTestThisCalled = false
        var theTestThatCalled = false
        def `test: this`() { theTestThisCalled = true }
        def `test: that`(r: Informer) { theTestThatCalled = true }
      }

      intercept[IllegalArgumentException] {
        // Here, they forgot that the name is actually `test: this`(Fixture)
        suite.run(Some(encode("test: misspelled")), Args(SilentReporter))
      }
    }

    it("should run only those tests selected by the tags to include and exclude sets") {

      // Nothing is excluded
      val a = new Suite {
        var theTestThisCalled = false
        var theTestThatCalled = false
        @SlowAsMolasses
        def `test: this`() { theTestThisCalled = true }
        def `test: that`(r: Informer) { theTestThatCalled = true }
      }

      import scala.language.reflectiveCalls

      val repA = new TestIgnoredTrackingReporter
      a.run(None, Args(repA))
      assert(!repA.testIgnoredReceived)
      assert(a.theTestThisCalled)
      assert(a.theTestThatCalled)

      // SlowAsMolasses is included, one test should be excluded
      val b = new Suite {
        var theTestThisCalled = false
        var theTestThatCalled = false
        @SlowAsMolasses
        def `test: this`() { theTestThisCalled = true }
        def `test: that`(r: Informer) { theTestThatCalled = true }
      }
      val repB = new TestIgnoredTrackingReporter
      b.run(None, Args(repB, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repB.testIgnoredReceived)
      assert(b.theTestThisCalled)
      assert(!b.theTestThatCalled)

      // SlowAsMolasses is included, and both tests should be included
      val c = new Suite {
        var theTestThisCalled = false
        var theTestThatCalled = false
        @SlowAsMolasses
        def `test: this`() { theTestThisCalled = true }
        @SlowAsMolasses
        def `test: that`(r: Informer) { theTestThatCalled = true }
      }
      val repC = new TestIgnoredTrackingReporter
      c.run(None, Args(repB, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repC.testIgnoredReceived)
      assert(c.theTestThisCalled)
      assert(c.theTestThatCalled)

      // SlowAsMolasses is included. both tests should be included but one ignored
      val d = new Suite {
        var theTestThisCalled = false
        var theTestThatCalled = false
        @Ignore
        @SlowAsMolasses
        def `test: this`() { theTestThisCalled = true }
        @SlowAsMolasses
        def `test: that`(r: Informer) { theTestThatCalled = true }
      }
      val repD = new TestIgnoredTrackingReporter
      d.run(None, Args(repD, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.Ignore")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(repD.testIgnoredReceived)
      assert(!d.theTestThisCalled)
      assert(d.theTestThatCalled)

      // SlowAsMolasses included, FastAsLight excluded
      val e = new Suite {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        @FastAsLight
        @SlowAsMolasses
        def `test: this`() { theTestThisCalled = true }
        @SlowAsMolasses
        def `test: that`(r: Informer) { theTestThatCalled = true }
        def `test: the other`(r: Informer) { theTestTheOtherCalled = true }
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
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        @Ignore
        @FastAsLight
        @SlowAsMolasses
        def `test: this`() { theTestThisCalled = true }
        @SlowAsMolasses
        def `test: that`(r: Informer) { theTestThatCalled = true }
        def `test: the other`(r: Informer) { theTestTheOtherCalled = true }
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
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        @FastAsLight
        @SlowAsMolasses
        def `test: this`() { theTestThisCalled = true }
        @SlowAsMolasses
        def `test: that`(r: Informer) { theTestThatCalled = true }
        @Ignore
        def `test: the other`(r: Informer) { theTestTheOtherCalled = true }
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
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        @FastAsLight
        @SlowAsMolasses
        def `test: this`() { theTestThisCalled = true }
        @SlowAsMolasses
        def `test: that`(r: Informer) { theTestThatCalled = true }
        def `test: the other`(r: Informer) { theTestTheOtherCalled = true }
      }
      val repH = new TestIgnoredTrackingReporter
      h.run(None, Args(repH, Stopper.default, Filter(None, Set("org.scalatest.FastAsLight")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repH.testIgnoredReceived)
      assert(!h.theTestThisCalled)
      assert(h.theTestThatCalled)
      assert(h.theTestTheOtherCalled)

      // No tagsToInclude set, SlowAsMolasses excluded
      val i = new Suite {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        @FastAsLight
        @SlowAsMolasses
        def `test: this`() { theTestThisCalled = true }
        @SlowAsMolasses
        def `test: that`(r: Informer) { theTestThatCalled = true }
        def `test: the other`(r: Informer) { theTestTheOtherCalled = true }
      }
      val repI = new TestIgnoredTrackingReporter
      i.run(None, Args(repI, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repI.testIgnoredReceived)
      assert(!i.theTestThisCalled)
      assert(!i.theTestThatCalled)
      assert(i.theTestTheOtherCalled)

      // No tagsToInclude set, SlowAsMolasses excluded, TestIgnored should not be received on excluded ones
      val j = new Suite {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        @Ignore
        @FastAsLight
        @SlowAsMolasses
        def `test: this`() { theTestThisCalled = true }
        @Ignore
        @SlowAsMolasses
        def `test: that`(r: Informer) { theTestThatCalled = true }
        def `test: the other`(r: Informer) { theTestTheOtherCalled = true }
      }
      val repJ = new TestIgnoredTrackingReporter
      j.run(None, Args(repJ, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses")), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(!repI.testIgnoredReceived)
      assert(!j.theTestThisCalled)
      assert(!j.theTestThatCalled)
      assert(j.theTestTheOtherCalled)

      // Same as previous, except Ignore specifically mentioned in excludes set
      val k = new Suite {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        @Ignore
        @FastAsLight
        @SlowAsMolasses
        def `test: this`() { theTestThisCalled = true }
        @Ignore
        @SlowAsMolasses
        def `test: that`(r: Informer) { theTestThatCalled = true }
        @Ignore
        def `test: the other`(r: Informer) { theTestTheOtherCalled = true }
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
        def `test: this`() = ()
        def `test: that`(r: Informer) = ()
      }
      assert(a.expectedTestCount(Filter()) === 2)

      val b = new Suite {
        @Ignore
        def `test: this`() = ()
        def `test: that`(r: Informer) = ()
      }
      assert(b.expectedTestCount(Filter()) === 1)

      val c = new Suite {
        @FastAsLight
        def `test: this`() = ()
        def `test: that`(r: Informer) = ()
      }
      assert(c.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 1)
      assert(c.expectedTestCount(Filter(None, Set("org.scalatest.FastAsLight"))) === 1)

      val d = new Suite {
        @FastAsLight
        @SlowAsMolasses
        def `test: this`() = ()
        @SlowAsMolasses
        def `test: that`(r: Informer) = ()
        def `test: the other thing`(r: Informer) = ()
      }
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 1)
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight"))) === 1)
      assert(d.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) === 1)
      assert(d.expectedTestCount(Filter()) === 3)

      val e = new Suite {
        @FastAsLight
        @SlowAsMolasses
        def `test: this`() = ()
        @SlowAsMolasses
        def `test: that`(r: Informer) = ()
        @Ignore
        def `test: the other thing`(r: Informer) = ()
      }
      assert(e.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 1)
      assert(e.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight"))) === 1)
      assert(e.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) === 0)
      assert(e.expectedTestCount(Filter()) === 2)

      val f = new Suites(a, b, c, d, e)
      assert(f.expectedTestCount(Filter()) === 10)
    }

    it("should generate a TestPending message when the test body is (pending)") {
      val a = new Suite {

        def `test: do this`() { pending }

        def `test: do that`() {
          assert(2 + 2 === 4)
        }

        def `test: do something else`() {
          assert(2 + 2 === 4)
          pending
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      val tp = rep.testPendingEventsReceived
      assert(tp.size === 2)
    }
    it("should generate a TestCanceled message when the test body includes a cancel call") {
      val a = new Suite {

        def `test: do this`() { cancel() }

        def `test: do that`() {
          assert(2 + 2 === 4)
        }

        def `test: do something else`() {
          assert(2 + 2 === 4)
          cancel()
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      val tp = rep.testCanceledEventsReceived
      assert(tp.size === 2)
    }
    it("should generate a TestCanceled message when the test body includes a failed assume call") {
      val a = new Suite {

        def `test: do this`() { assume(1 === 2) }

        def `test: do that`() {
          assert(2 + 2 === 4)
        }

        def `test: do something else`() {
          assert(2 + 2 === 4)
          assume(3 === 4)
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
        def `test: throws AssertionError`() { throw new AssertionError }
        def `test: throws plain old Error`() { throw new Error }
        def `test: throws Throwable`() { throw new Throwable }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      val tf = rep.testFailedEventsReceived
      assert(tf.size === 3)
    }
    it("should propagate out Errors that are direct subtypes of Error in JDK 1.5, other than " +
            "AssertionError, causing Suites and Runs to abort.") {
      val a = new Suite {
        def `test: throws AssertionError`() { throw new OutOfMemoryError }
      }
      intercept[OutOfMemoryError] {
        a.run(None, Args(SilentReporter))
      }
    }
    it("should invoke withFixture from runTest for no-arg test method") {
      val a = new Suite {
        var withFixtureWasInvoked = false
        var theTestWasInvoked = false
        override def withFixture(test: NoArgTest): Outcome = {
          withFixtureWasInvoked = true
          super.withFixture(test)
        }
        def `test: something`() {
          theTestWasInvoked = true
        }
      }

      import scala.language.reflectiveCalls

      a.run(None, Args(SilentReporter))
      assert(a.withFixtureWasInvoked)
      assert(a.theTestWasInvoked)
    }
    it("should invoke withFixture from runTest for a test method that takes an Informer") {
      val a = new Suite {
        var withFixtureWasInvoked = false
        var theTestWasInvoked = false
        override def withFixture(test: NoArgTest): Outcome = {
          withFixtureWasInvoked = true
          super.withFixture(test)
        }
        def `test: something`(r: Informer) {
          theTestWasInvoked = true
        }
      }

      import scala.language.reflectiveCalls

      a.run(None, Args(SilentReporter))
      assert(a.withFixtureWasInvoked)
      assert(a.theTestWasInvoked)
    }
    it("should pass the correct test name in the NoArgTest passed to withFixture") {
      val a = new Suite {
        var correctTestNameWasPassed = false
        override def withFixture(test: NoArgTest): Outcome = {
          correctTestNameWasPassed = test.name == (encode("test: something") + "(Informer)")
          super.withFixture(test)
        }
        def `test: something`(r: Informer) {}
      }

      import scala.language.reflectiveCalls

      a.run(None, Args(SilentReporter))
      assert(a.correctTestNameWasPassed)
    }
    it("should pass the correct config map in the NoArgTest passed to withFixture") {
      val a = new Suite {
        var correctConfigMapWasPassed = false
        override def withFixture(test: NoArgTest): Outcome = {
          correctConfigMapWasPassed = (test.configMap == ConfigMap("hi" -> 7))
          super.withFixture(test)
        }
        def `test: something`(r: Informer) {}
      }

      import scala.language.reflectiveCalls

      a.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap("hi" -> 7), None, new Tracker(), Set.empty))
      assert(a.correctConfigMapWasPassed)
    }

    describe("(when its pendingUntilFixed method is invoked)") {
      it("should throw TestPendingException if the code block throws an exception") {
        intercept[TestPendingException] {
          pendingUntilFixed {
            assert(1 + 1 === 3)
          }
        }
      }
      it("should throw TestFailedException if the code block doesn't throw an exception") {
        intercept[TestFailedException] {
          pendingUntilFixed {
            assert(1 + 2 === 3)
          }
        }
      }
    }
    it("should, when a test method takes an Informer and writes to its Informer, report the info in test completion event") {
      val msg = "hi there dude"
      class MySuite extends Suite {
        def `test: with Informer`(info: Informer) {
          info(msg)
        }
      }
      val myRep = new EventRecordingReporter
      new MySuite().run(None, Args(myRep))
      val testStarting = myRep.testStartingEventsReceived
      assert(testStarting.size === 1)
      val testSucceeded = myRep.testSucceededEventsReceived
      assert(testSucceeded.size === 1)
      assert(testSucceeded(0).recordedEvents.size === 1)
      val ip: InfoProvided = testSucceeded(0).recordedEvents(0).asInstanceOf[InfoProvided]
      assert(msg === ip.message)
    }
  }
  describe("the stopper") {
    it("should stop nested suites from being executed") {
      class SuiteA extends Suite {
        var executed = false;
        override def run(testName: Option[String], args: Args): Status = {
          executed = true
          super.run(testName, args)
        }
      }
      class SuiteB extends Suite {
        var executed = false;
        override def run(testName: Option[String], args: Args): Status = {
          executed = true
          super.run(testName, args)
        }
      }
      class SuiteC extends Suite {
        var executed = false;
        override def run(testName: Option[String], args: Args): Status = {
          executed = true
          super.run(testName, args)
        }
      }
      class SuiteD extends Suite {
        var executed = false;
        override def run(testName: Option[String], args: Args): Status = {
          executed = true
          val status = super.run(testName, args)
          args.stopper.requestStop()
          status
        }
      }
      class SuiteE extends Suite {
        var executed = false;
        override def run(testName: Option[String], args: Args): Status = {
          executed = true
          super.run(testName, args)
        }
      }
      class SuiteF extends Suite {
        var executed = false;
        override def run(testName: Option[String], args: Args): Status = {
          executed = true
          super.run(testName, args)
        }
      }
      class SuiteG extends Suite {
        var executed = false;
        override def run(testName: Option[String], args: Args): Status = {
          executed = true
          super.run(testName, args)
        }
      }

      val a = new SuiteA
      val b = new SuiteB
      val c = new SuiteC
      val d = new SuiteD
      val e = new SuiteE
      val f = new SuiteF
      val g = new SuiteG
      
      class IgnoreStopRequestStopper extends Stopper {
        def stopRequested: Boolean = false
        def requestStop() {}
        def reset() {}
      }

      val x = Suites(a, b, c, d, e, f, g)
      x.run(None, Args(SilentReporter, new IgnoreStopRequestStopper))

      assert(a.executed)
      assert(b.executed)
      assert(c.executed)
      assert(d.executed)
      assert(e.executed)
      assert(f.executed)
      assert(g.executed)

      val h = new SuiteA
      val i = new SuiteB
      val j = new SuiteC
      val k = new SuiteD
      val l = new SuiteE
      val m = new SuiteF
      val n = new SuiteG

      val y = Suites(h, i, j, k, l, m, n)
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

      class MySuite extends Suite {
        var theTestsExecutedCount = 0
        def `test: 1`() { theTestsExecutedCount += 1 }
        def `test: 2`() { theTestsExecutedCount += 1 }
        def `test: 3`() { theTestsExecutedCount += 1 }
        def `test: 4`() {
          theTestsExecutedCount += 1
        }
        def `test: 5`() { theTestsExecutedCount += 1 }
        def `test: 6`() { theTestsExecutedCount += 1 }
        def `test: 7`() { theTestsExecutedCount += 1 }
      }

      val x = new MySuite
      x.run(None, Args(SilentReporter))
      assert(x.theTestsExecutedCount === 7)

      val myStopper = Stopper.default

      class MyStoppingSuite extends Suite {
        var testsExecutedCount = 0
        def `test: 1`() { testsExecutedCount += 1 }
        def `test: 2`() { testsExecutedCount += 1 }
        def `test: 3`() { testsExecutedCount += 1 }
        def `test: 4`() {
          testsExecutedCount += 1
          myStopper.requestStop()
        }
        def `test: 5`() { testsExecutedCount += 1 }
        def `test: 6`() { testsExecutedCount += 1 }
        def `test: 7`() { testsExecutedCount += 1 }
      }

      val y = new MyStoppingSuite
      y.run(None, Args(SilentReporter, myStopper, Filter(), ConfigMap.empty, None, new Tracker, Set.empty))
      assert(y.testsExecutedCount === 4)
    }
  }
  describe("A Suite's execute method") {
    it("should throw NPE if passed null for configMap") {
      class MySuite extends Suite
      intercept[NullPointerException] {
        (new MySuite).execute(configMap = null)
      }
    }
    it("should throw IAE if a testName is passed that does not exist on the suite") {
      class MySuite extends Suite
      intercept[IllegalArgumentException] {
        (new MySuite).execute(testName = "fred")
      }
    }
  }
  describe("NoArgTest") {
    it("should offer a factory method that takes another NoArgTest and a function that implements apply") {
      class SideEffectedFixtureWasSpec extends Spec {
        type FixtureParam = String
        var theFixture = ""
        var sideEffectedFixtureWas = ""
        override def withFixture(test: NoArgTest): Outcome = { 
          // These will fail the test if the wrapped tests's TestData is not passed through
          assert(test.name == "some test")
          assert(test.configMap == ConfigMap.empty)
          assert(test.scopes == Seq.empty)
          assert(test.text == "some test")
          assert(test.tags == Set.empty)
          theFixture = "hi"
          test()
        }
        def `some test` = { sideEffectedFixtureWas = theFixture }
      }

      val a = new SideEffectedFixtureWasSpec
      a.run(None, Args(SilentReporter))
      assert(a.sideEffectedFixtureWas === "hi")

      class WrappedFixtureSpec extends SideEffectedFixtureWasSpec {
        var withFixtureWasCalled = false
        override def withFixture(test: NoArgTest): Outcome = {
          super.withFixture(
            new NoArgTest {
             def apply(): Outcome = {
               withFixtureWasCalled = true
               theFixture = theFixture.toUpperCase
               test()
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
      assert(b.sideEffectedFixtureWas === "HI")

      class ShorthandWrappedFixtureSpec extends SideEffectedFixtureWasSpec {
        var withFixtureWasCalled = false
        override def withFixture(test: NoArgTest): Outcome = {
          super.withFixture(
            NoArgTest(test) {
               withFixtureWasCalled = true
               theFixture = theFixture.toUpperCase
               test()
             }
          )
        }
      }

      val c = new ShorthandWrappedFixtureSpec
      c.run(None, Args(SilentReporter))
      assert(c.sideEffectedFixtureWas === "HI")
    }
  }
}

