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
import org.scalatest.exceptions.TestFailedException
import org.scalatest.exceptions.TestPendingException
import org.scalactic.exceptions.NullArgumentException

protected[scalatest] class MandarinOrangeFunSuite(ns: Suite*) extends FunSuite {
  override def nestedSuites = Vector.empty ++ ns // ns.toVector
}
protected[scalatest] class MandarinOrangeFunSpec(ns: Suite*) extends FunSpec {
  override def nestedSuites = Vector.empty ++ ns // ns.toVector
}
// SKIP-SCALATESTJS-START
protected[scalatest] class MandarinOrangeSpec(ns: Suite*) extends RefSpec {
  override def nestedSuites = Vector.empty ++ ns // ns.toVector
}
// SKIP-SCALATESTJS-END
protected[scalatest] class MandarinOrangeWordSpec(ns: Suite*) extends WordSpec {
  override def nestedSuites = Vector.empty ++ ns // ns.toVector
}
protected[scalatest] class MandarinOrangeFlatSpec(ns: Suite*) extends FlatSpec {
  override def nestedSuites = Vector.empty ++ ns // ns.toVector
}
protected[scalatest] class MandarinOrangeFreeSpec(ns: Suite*) extends FreeSpec {
  override def nestedSuites = Vector.empty ++ ns // ns.toVector
}
protected[scalatest] class MandarinOrangeFeatureSpec(ns: Suite*) extends FeatureSpec {
  override def nestedSuites = Vector.empty ++ ns // ns.toVector
}
protected[scalatest] class MandarinOrangePropSpec(ns: Suite*) extends PropSpec {
  override def nestedSuites = Vector.empty ++ ns // ns.toVector
}

// Named these with a MandarinOrange prefix so they wouldn't confict
// with anything else in the test suite. These need to be top level
// else they end up with dollar signs in the names.
trait MandarinOrangeFixture { this: fixture.Suite =>
  type FixtureParam = String
  def withFixture(test: OneArgTest): Outcome = { test("hi") }
}

protected[scalatest] class MandarinOrangeFixtureFunSuite(ns: Suite*) extends fixture.FunSuite with MandarinOrangeFixture {
  override def nestedSuites = Vector.empty ++ ns // ns.toVector
}
protected[scalatest] class MandarinOrangeFixtureFunSpec(ns: Suite*) extends fixture.FunSpec with MandarinOrangeFixture {
  override def nestedSuites = Vector.empty ++ ns // ns.toVector
}
// SKIP-SCALATESTJS-START
protected[scalatest] class MandarinOrangeFixtureSpec(ns: Suite*) extends fixture.RefSpec with MandarinOrangeFixture {
  override def nestedSuites = Vector.empty ++ ns // ns.toVector
}
// SKIP-SCALATESTJS-END
protected[scalatest] class MandarinOrangeFixtureWordSpec(ns: Suite*) extends fixture.WordSpec with MandarinOrangeFixture {
  override def nestedSuites = Vector.empty ++ ns // ns.toVector
}
protected[scalatest] class MandarinOrangeFixtureFlatSpec(ns: Suite*) extends fixture.FlatSpec with MandarinOrangeFixture {
  override def nestedSuites = Vector.empty ++ ns // ns.toVector
}
protected[scalatest] class MandarinOrangeFixtureFreeSpec(ns: Suite*) extends fixture.FreeSpec with MandarinOrangeFixture {
  override def nestedSuites = Vector.empty ++ ns // ns.toVector
}
protected[scalatest] class MandarinOrangeFixtureFeatureSpec(ns: Suite*) extends fixture.FeatureSpec with MandarinOrangeFixture {
  override def nestedSuites = Vector.empty ++ ns // ns.toVector
}
protected[scalatest] class MandarinOrangeFixturePropSpec(ns: Suite*) extends fixture.PropSpec with MandarinOrangeFixture {
  override def nestedSuites = Vector.empty ++ ns // ns.toVector
}

protected[scalatest] class MandarinOrangeSuites(suites: Suite*) extends Suites(suites: _*)
protected[scalatest] class MandarinOrangeSequential(suites: Suite*) extends Sequential(suites: _*)
protected[scalatest] class MandarinOrangeStepwise(suites: Suite*) extends Stepwise(suites: _*)

// SKIP-SCALATESTJS-START
import PrivateMethodTester._
// SKIP-SCALATESTJS-END

class SuiteSpec extends FunSpec {

  describe("the toString method on Suites and SuiteLike traits other than TestNGSuiteLike") {
    describe("when the suite contains no nested suites") {
      it("should return the simple name of the class (and no parens)") {
        import prop.TableDrivenPropertyChecks._
        val examples =
          Table(

            ( "suite", "simple name"),

            ( new FunSuite, "FunSuite"),
            ( new FunSpec, "FunSpec"),
            // SKIP-SCALATESTJS-START
            ( new RefSpec, "RefSpec"),
            // SKIP-SCALATESTJS-END
            ( new WordSpec, "WordSpec"),
            ( new FlatSpec, "FlatSpec"),
            ( new FreeSpec, "FreeSpec"),
            ( new FeatureSpec, "FeatureSpec"),
            ( new PropSpec, "PropSpec"),

            ( new MandarinOrangeFunSuite, "MandarinOrangeFunSuite"),
            ( new MandarinOrangeFunSpec, "MandarinOrangeFunSpec"),
            // SKIP-SCALATESTJS-START
            ( new MandarinOrangeSpec, "MandarinOrangeSpec"),
            // SKIP-SCALATESTJS-END
            ( new MandarinOrangeWordSpec, "MandarinOrangeWordSpec"),
            ( new MandarinOrangeFlatSpec, "MandarinOrangeFlatSpec"),
            ( new MandarinOrangeFreeSpec, "MandarinOrangeFreeSpec"),
            ( new MandarinOrangeFeatureSpec, "MandarinOrangeFeatureSpec"),
            ( new MandarinOrangePropSpec, "MandarinOrangePropSpec"),

            ( new MandarinOrangeFixtureFunSuite, "MandarinOrangeFixtureFunSuite"),
            ( new MandarinOrangeFixtureFunSpec, "MandarinOrangeFixtureFunSpec"),
            // SKIP-SCALATESTJS-START
            ( new MandarinOrangeFixtureSpec, "MandarinOrangeFixtureSpec"),
            // SKIP-SCALATESTJS-END
            ( new MandarinOrangeFixtureWordSpec, "MandarinOrangeFixtureWordSpec"),
            ( new MandarinOrangeFixtureFlatSpec, "MandarinOrangeFixtureFlatSpec"),
            ( new MandarinOrangeFixtureFreeSpec, "MandarinOrangeFixtureFreeSpec"),
            ( new MandarinOrangeFixtureFeatureSpec, "MandarinOrangeFixtureFeatureSpec"),
            ( new MandarinOrangeFixturePropSpec, "MandarinOrangeFixturePropSpec"),

            // ( new path.FunSpec, "path.FunSpec"),
            // ( new path.FreeSpec, "path.FreeSpec"),

            ( new Suites, "Suites"),
            ( new Sequential, "Sequential"),
            ( new Stepwise, "Stepwise"),

            ( new MandarinOrangeSuites, "MandarinOrangeSuites"),
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
            // SKIP-SCALATESTJS-START
            ( new MandarinOrangeSpec(new FunSuite), "MandarinOrangeSpec(FunSuite)"),
            // SKIP-SCALATESTJS-END
            ( new MandarinOrangeWordSpec(new FunSuite), "MandarinOrangeWordSpec(FunSuite)"),
            ( new MandarinOrangeFlatSpec(new FunSuite), "MandarinOrangeFlatSpec(FunSuite)"),
            ( new MandarinOrangeFreeSpec(new FunSuite), "MandarinOrangeFreeSpec(FunSuite)"),
            ( new MandarinOrangeFeatureSpec(new FunSuite), "MandarinOrangeFeatureSpec(FunSuite)"),
            ( new MandarinOrangePropSpec(new FunSuite), "MandarinOrangePropSpec(FunSuite)"),

            ( new MandarinOrangeFixtureFunSuite(new FunSuite), "MandarinOrangeFixtureFunSuite(FunSuite)"),
            ( new MandarinOrangeFixtureFunSpec(new FunSuite), "MandarinOrangeFixtureFunSpec(FunSuite)"),
            // SKIP-SCALATESTJS-START
            ( new MandarinOrangeFixtureSpec(new FunSuite), "MandarinOrangeFixtureSpec(FunSuite)"),
            // SKIP-SCALATESTJS-END
            ( new MandarinOrangeFixtureWordSpec(new FunSuite), "MandarinOrangeFixtureWordSpec(FunSuite)"),
            ( new MandarinOrangeFixtureFlatSpec(new FunSuite), "MandarinOrangeFixtureFlatSpec(FunSuite)"),
            ( new MandarinOrangeFixtureFreeSpec(new FunSuite), "MandarinOrangeFixtureFreeSpec(FunSuite)"),
            ( new MandarinOrangeFixtureFeatureSpec(new FunSuite), "MandarinOrangeFixtureFeatureSpec(FunSuite)"),
            ( new MandarinOrangeFixturePropSpec(new FunSuite), "MandarinOrangeFixturePropSpec(FunSuite)"),

            // ( new path.FunSpec(new FunSuite), "path.FunSpec(FunSuite)"),
            // ( new path.FreeSpec(new FunSuite), "path.FreeSpec(FunSuite)"),

            ( new Suites(new FunSuite), "Suites(FunSuite)"),
            ( new Sequential(new FunSuite), "Sequential(FunSuite)"),
            ( new Stepwise(new FunSuite), "Stepwise(FunSuite)"),

            ( new MandarinOrangeSuites(new FunSuite), "MandarinOrangeSuites(FunSuite)"),
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
            // SKIP-SCALATESTJS-START
            ( new MandarinOrangeSpec(new PropSpec, new FeatureSpec, new FunSuite), "MandarinOrangeSpec(PropSpec, FeatureSpec, FunSuite)"),
            // SKIP-SCALATESTJS-END
            ( new MandarinOrangeWordSpec(new PropSpec, new FeatureSpec, new FunSuite), "MandarinOrangeWordSpec(PropSpec, FeatureSpec, FunSuite)"),
            ( new MandarinOrangeFlatSpec(new PropSpec, new FeatureSpec, new FunSuite), "MandarinOrangeFlatSpec(PropSpec, FeatureSpec, FunSuite)"),
            ( new MandarinOrangeFreeSpec(new PropSpec, new FeatureSpec, new FunSuite), "MandarinOrangeFreeSpec(PropSpec, FeatureSpec, FunSuite)"),
            ( new MandarinOrangeFeatureSpec(new PropSpec, new FeatureSpec, new FunSuite), "MandarinOrangeFeatureSpec(PropSpec, FeatureSpec, FunSuite)"),
            ( new MandarinOrangePropSpec(new PropSpec, new FeatureSpec, new FunSuite), "MandarinOrangePropSpec(PropSpec, FeatureSpec, FunSuite)"),

            ( new MandarinOrangeFixtureFunSuite(new PropSpec, new FeatureSpec, new FunSuite), "MandarinOrangeFixtureFunSuite(PropSpec, FeatureSpec, FunSuite)"),
            ( new MandarinOrangeFixtureFunSpec(new PropSpec, new FeatureSpec, new FunSuite), "MandarinOrangeFixtureFunSpec(PropSpec, FeatureSpec, FunSuite)"),
            // SKIP-SCALATESTJS-START
            ( new MandarinOrangeFixtureSpec(new PropSpec, new FeatureSpec, new FunSuite), "MandarinOrangeFixtureSpec(PropSpec, FeatureSpec, FunSuite)"),
            // SKIP-SCALATESTJS-END
            ( new MandarinOrangeFixtureWordSpec(new PropSpec, new FeatureSpec, new FunSuite), "MandarinOrangeFixtureWordSpec(PropSpec, FeatureSpec, FunSuite)"),
            ( new MandarinOrangeFixtureFlatSpec(new PropSpec, new FeatureSpec, new FunSuite), "MandarinOrangeFixtureFlatSpec(PropSpec, FeatureSpec, FunSuite)"),
            ( new MandarinOrangeFixtureFreeSpec(new PropSpec, new FeatureSpec, new FunSuite), "MandarinOrangeFixtureFreeSpec(PropSpec, FeatureSpec, FunSuite)"),
            ( new MandarinOrangeFixtureFeatureSpec(new PropSpec, new FeatureSpec, new FunSuite), "MandarinOrangeFixtureFeatureSpec(PropSpec, FeatureSpec, FunSuite)"),
            ( new MandarinOrangeFixturePropSpec(new PropSpec, new FeatureSpec, new FunSuite), "MandarinOrangeFixturePropSpec(PropSpec, FeatureSpec, FunSuite)"),

            // ( new path.FunSpec(new PropSpec, new FeatureSpec, new FunSuite), "path.FunSpec(PropSpec, FeatureSpec, FunSuite)"),
            // ( new path.FreeSpec(new PropSpec, new FeatureSpec, new FunSuite), "path.FreeSpec(PropSpec, FeatureSpec, FunSuite)"),

            ( new Suites(new PropSpec, new FeatureSpec, new FunSuite), "Suites(PropSpec, FeatureSpec, FunSuite)"),
            ( new Sequential(new PropSpec, new FeatureSpec, new FunSuite), "Sequential(PropSpec, FeatureSpec, FunSuite)"),
            ( new Stepwise(new PropSpec, new FeatureSpec, new FunSuite), "Stepwise(PropSpec, FeatureSpec, FunSuite)"),

            ( new MandarinOrangeSuites(new PropSpec, new FeatureSpec, new FunSuite), "MandarinOrangeSuites(PropSpec, FeatureSpec, FunSuite)"),
            ( new MandarinOrangeSequential(new PropSpec, new FeatureSpec, new FunSuite), "MandarinOrangeSequential(PropSpec, FeatureSpec, FunSuite)"),
            ( new MandarinOrangeStepwise(new PropSpec, new FeatureSpec, new FunSuite), "MandarinOrangeStepwise(PropSpec, FeatureSpec, FunSuite)")
          )
        forAll (examples) { (suite, simpleName) =>
          assert(suite.toString === simpleName)
        }
      }
    }
  }

  // SKIP-SCALATESTJS-START
  describe("The simpleNameForTest method") {
    it("should return the correct test simple name with or without Informer") {
      val simpleNameForTest = PrivateMethod[String]('simpleNameForTest)
      assert((Suite invokePrivate simpleNameForTest("testThis")) === "testThis")
      assert((Suite invokePrivate simpleNameForTest("testThis(Informer)")) === "testThis")
      assert((Suite invokePrivate simpleNameForTest("test(Informer)")) === "test")
      assert((Suite invokePrivate simpleNameForTest("test")) === "test")
    }
  }
  // SKIP-SCALATESTJS-END

  describe("A Suite") {
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
  }
  // SKIP-SCALATESTJS-START
  describe("A Suite's execute method") {
    it("should throw NAE if passed null for configMap") {
      class MySuite extends Suite
      intercept[NullArgumentException] {
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
  // SKIP-SCALATESTJS-END
  describe("NoArgTest") {
    it("should offer a factory method that takes another NoArgTest and a function that implements apply") {
      class SideEffectedFixtureWasSpec extends FunSpec {
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
        it("some test") { sideEffectedFixtureWas = theFixture }
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

  describe("Suite's runNestedSuites method") {

    it("should fire SuiteAborted event when after function in BeforeAndAfter nested suite throws RuntimeException") {

      class NestedSuite extends FunSuite with BeforeAndAfter {

        test("test 1") {}

        after {
          throw new RuntimeException("oops!")
        }

      }

      class ExampleSuite extends Suite {
        override def nestedSuites = Vector(new NestedSuite)
      }

      val suite = new ExampleSuite
      val rep = new EventRecordingReporter
      suite.run(None, Args(rep))

      assert(rep.suiteStartingEventsReceived.length == 1)
      assert(rep.suiteCompletedEventsReceived.length == 0)
      assert(rep.suiteAbortedEventsReceived.length == 1)
    }

    it("should fire SuiteAborted event when afterAll function in BeforeAndAfterAll nested suite throws RuntimeException") {

      class NestedSuite extends FunSuite with BeforeAndAfterAll {

        test("test 1") {}

        override protected def afterAll() {
          throw new RuntimeException("oops!")
        }

      }

      class ExampleSuite extends Suite {
        override def nestedSuites = Vector(new NestedSuite)
      }

      val suite = new ExampleSuite
      val rep = new EventRecordingReporter
      suite.run(None, Args(rep))

      assert(rep.suiteStartingEventsReceived.length == 1)
      assert(rep.suiteCompletedEventsReceived.length == 0)
      assert(rep.suiteAbortedEventsReceived.length == 1)
    }

    it("should fire SuiteAborted event when afterAll function in BeforeAndAfterAllConfigMap nested suite throws RuntimeException") {

      class NestedSuite extends FunSuite with BeforeAndAfterAllConfigMap {

        test("test 1") {}

        override protected def afterAll(configMap: ConfigMap) {
          throw new RuntimeException("oops!")
        }

      }

      class ExampleSuite extends Suite {
        override def nestedSuites = Vector(new NestedSuite)
      }

      val suite = new ExampleSuite
      val rep = new EventRecordingReporter
      suite.run(None, Args(rep))

      assert(rep.suiteStartingEventsReceived.length == 1)
      assert(rep.suiteCompletedEventsReceived.length == 0)
      assert(rep.suiteAbortedEventsReceived.length == 1)
    }

    it("should fire SuiteAborted event when afterAll function in BeforeAndAfterEach nested suite throws RuntimeException") {

      class NestedSuite extends FunSuite with BeforeAndAfterEach {

        test("test 1") {}

        override protected def afterEach() {
          throw new RuntimeException("oops!")
        }

      }

      class ExampleSuite extends Suite {
        override def nestedSuites = Vector(new NestedSuite)
      }

      val suite = new ExampleSuite
      val rep = new EventRecordingReporter
      suite.run(None, Args(rep))

      assert(rep.suiteStartingEventsReceived.length == 1)
      assert(rep.suiteCompletedEventsReceived.length == 0)
      assert(rep.suiteAbortedEventsReceived.length == 1)
    }

    it("should fire SuiteAborted event when afterAll function in BeforeAndAfterEachTestData nested suite throws RuntimeException") {

      class NestedSuite extends FunSuite with BeforeAndAfterEachTestData {

        test("test 1") {}

        override protected def afterEach(test: TestData) {
          throw new RuntimeException("oops!")
        }

      }

      class ExampleSuite extends Suite {
        override def nestedSuites = Vector(new NestedSuite)
      }

      val suite = new ExampleSuite
      val rep = new EventRecordingReporter
      suite.run(None, Args(rep))

      assert(rep.suiteStartingEventsReceived.length == 1)
      assert(rep.suiteCompletedEventsReceived.length == 0)
      assert(rep.suiteAbortedEventsReceived.length == 1)
    }

  }
}

