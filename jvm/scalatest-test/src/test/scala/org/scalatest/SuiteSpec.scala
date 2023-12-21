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
import org.scalactic.exceptions.NullArgumentException
import org.scalatest.exceptions.TestFailedException
import org.scalatest.exceptions.TestPendingException
import scala.reflect.NameTransformer.encode
import collection.immutable.TreeSet
// SKIP-SCALATESTJS,NATIVE-START
import org.scalatest.refspec.RefSpec
// SKIP-SCALATESTJS,NATIVE-END
import org.scalactic._
import org.scalatest.{ featurespec, flatspec, freespec, funspec, funsuite, propspec, wordspec }
import org.scalatest.featurespec.AnyFeatureSpec
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.propspec.AnyPropSpec
import org.scalatest.wordspec.AnyWordSpec

protected[scalatest] class MandarinOrangeFunSuite(ns: Suite*) extends AnyFunSuite {
  override def nestedSuites = Vector.empty ++ ns // ns.toVector
}
protected[scalatest] class MandarinOrangeFunSpec(ns: Suite*) extends AnyFunSpec {
  override def nestedSuites = Vector.empty ++ ns // ns.toVector
}
// SKIP-SCALATESTJS,NATIVE-START
protected[scalatest] class MandarinOrangeSpec(ns: Suite*) extends RefSpec {
  override def nestedSuites = Vector.empty ++ ns // ns.toVector
}
// SKIP-SCALATESTJS,NATIVE-END
protected[scalatest] class MandarinOrangeWordSpec(ns: Suite*) extends AnyWordSpec {
  override def nestedSuites = Vector.empty ++ ns // ns.toVector
}
protected[scalatest] class MandarinOrangeFlatSpec(ns: Suite*) extends AnyFlatSpec {
  override def nestedSuites = Vector.empty ++ ns // ns.toVector
}
protected[scalatest] class MandarinOrangeFreeSpec(ns: Suite*) extends AnyFreeSpec {
  override def nestedSuites = Vector.empty ++ ns // ns.toVector
}
protected[scalatest] class MandarinOrangeFeatureSpec(ns: Suite*) extends AnyFeatureSpec {
  override def nestedSuites = Vector.empty ++ ns // ns.toVector
}
protected[scalatest] class MandarinOrangePropSpec(ns: Suite*) extends AnyPropSpec {
  override def nestedSuites = Vector.empty ++ ns // ns.toVector
}

// Named these with a MandarinOrange prefix so they wouldn't confict
// with anything else in the test suite. These need to be top level
// else they end up with dollar signs in the names.
trait MandarinOrangeFixture { this: FixtureTestSuite =>
  type FixtureParam = String
  def withFixture(test: OneArgTest): Outcome = { test("hi") }
}

protected[scalatest] class MandarinOrangeFixtureFunSuite(ns: Suite*) extends funsuite.FixtureAnyFunSuite with MandarinOrangeFixture {
  override def nestedSuites = Vector.empty ++ ns // ns.toVector
}
protected[scalatest] class MandarinOrangeFixtureFunSpec(ns: Suite*) extends funspec.FixtureAnyFunSpec with MandarinOrangeFixture {
  override def nestedSuites = Vector.empty ++ ns // ns.toVector
}
protected[scalatest] class MandarinOrangeFixtureWordSpec(ns: Suite*) extends wordspec.FixtureAnyWordSpec with MandarinOrangeFixture {
  override def nestedSuites = Vector.empty ++ ns // ns.toVector
}
protected[scalatest] class MandarinOrangeFixtureFlatSpec(ns: Suite*) extends flatspec.FixtureAnyFlatSpec with MandarinOrangeFixture {
  override def nestedSuites = Vector.empty ++ ns // ns.toVector
}
protected[scalatest] class MandarinOrangeFixtureFreeSpec(ns: Suite*) extends freespec.FixtureAnyFreeSpec with MandarinOrangeFixture {
  override def nestedSuites = Vector.empty ++ ns // ns.toVector
}
protected[scalatest] class MandarinOrangeFixtureFeatureSpec(ns: Suite*) extends featurespec.FixtureAnyFeatureSpec with MandarinOrangeFixture {
  override def nestedSuites = Vector.empty ++ ns // ns.toVector
}
protected[scalatest] class MandarinOrangeFixturePropSpec(ns: Suite*) extends propspec.FixtureAnyPropSpec with MandarinOrangeFixture {
  override def nestedSuites = Vector.empty ++ ns // ns.toVector
}

protected[scalatest] class MandarinOrangeSuites(suites: Suite*) extends Suites(suites: _*)
protected[scalatest] class MandarinOrangeSequential(suites: Suite*) extends Sequential(suites: _*)
protected[scalatest] class MandarinOrangeStepwise(suites: Suite*) extends Stepwise(suites: _*)

// SKIP-SCALATESTJS,NATIVE-START
import PrivateMethodTester._
// SKIP-SCALATESTJS,NATIVE-END

class SuiteSpec extends AnyFunSpec {

  describe("the toString method on Suites and SuiteLike traits other than TestNGSuiteLike") {
    describe("when the suite contains no nested suites") {
      it("should return the simple name of the class (and no parens)") {
        import prop.TableDrivenPropertyChecks._
        val examples =
          Table(

            ( "suite", "simple name"),

            ( new AnyFunSuite, "AnyFunSuite"),
            ( new AnyFunSpec, "AnyFunSpec"),
            // SKIP-SCALATESTJS,NATIVE-START
            ( new RefSpec, "RefSpec"),
            // SKIP-SCALATESTJS,NATIVE-END
            ( new AnyWordSpec, "AnyWordSpec"),
            ( new AnyFlatSpec, "AnyFlatSpec"),
            ( new AnyFreeSpec, "AnyFreeSpec"),
            ( new AnyFeatureSpec, "AnyFeatureSpec"),
            ( new AnyPropSpec, "AnyPropSpec"),

            ( new MandarinOrangeFunSuite, "MandarinOrangeFunSuite"),
            ( new MandarinOrangeFunSpec, "MandarinOrangeFunSpec"),
            // SKIP-SCALATESTJS,NATIVE-START
            ( new MandarinOrangeSpec, "MandarinOrangeSpec"),
            // SKIP-SCALATESTJS,NATIVE-END
            ( new MandarinOrangeWordSpec, "MandarinOrangeWordSpec"),
            ( new MandarinOrangeFlatSpec, "MandarinOrangeFlatSpec"),
            ( new MandarinOrangeFreeSpec, "MandarinOrangeFreeSpec"),
            ( new MandarinOrangeFeatureSpec, "MandarinOrangeFeatureSpec"),
            ( new MandarinOrangePropSpec, "MandarinOrangePropSpec"),

            ( new MandarinOrangeFixtureFunSuite, "MandarinOrangeFixtureFunSuite"),
            ( new MandarinOrangeFixtureFunSpec, "MandarinOrangeFixtureFunSpec"),
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

            ( new MandarinOrangeFunSuite(new AnyFunSuite), "MandarinOrangeFunSuite(AnyFunSuite)"),
            ( new MandarinOrangeFunSpec(new AnyFunSuite), "MandarinOrangeFunSpec(AnyFunSuite)"),
            // SKIP-SCALATESTJS,NATIVE-START
            ( new MandarinOrangeSpec(new AnyFunSuite), "MandarinOrangeSpec(AnyFunSuite)"),
            // SKIP-SCALATESTJS,NATIVE-END
            ( new MandarinOrangeWordSpec(new AnyFunSuite), "MandarinOrangeWordSpec(AnyFunSuite)"),
            ( new MandarinOrangeFlatSpec(new AnyFunSuite), "MandarinOrangeFlatSpec(AnyFunSuite)"),
            ( new MandarinOrangeFreeSpec(new AnyFunSuite), "MandarinOrangeFreeSpec(AnyFunSuite)"),
            ( new MandarinOrangeFeatureSpec(new AnyFunSuite), "MandarinOrangeFeatureSpec(AnyFunSuite)"),
            ( new MandarinOrangePropSpec(new AnyFunSuite), "MandarinOrangePropSpec(AnyFunSuite)"),

            ( new MandarinOrangeFixtureFunSuite(new AnyFunSuite), "MandarinOrangeFixtureFunSuite(AnyFunSuite)"),
            ( new MandarinOrangeFixtureFunSpec(new AnyFunSuite), "MandarinOrangeFixtureFunSpec(AnyFunSuite)"),
            ( new MandarinOrangeFixtureWordSpec(new AnyFunSuite), "MandarinOrangeFixtureWordSpec(AnyFunSuite)"),
            ( new MandarinOrangeFixtureFlatSpec(new AnyFunSuite), "MandarinOrangeFixtureFlatSpec(AnyFunSuite)"),
            ( new MandarinOrangeFixtureFreeSpec(new AnyFunSuite), "MandarinOrangeFixtureFreeSpec(AnyFunSuite)"),
            ( new MandarinOrangeFixtureFeatureSpec(new AnyFunSuite), "MandarinOrangeFixtureFeatureSpec(AnyFunSuite)"),
            ( new MandarinOrangeFixturePropSpec(new AnyFunSuite), "MandarinOrangeFixturePropSpec(AnyFunSuite)"),

            // ( new path.FunSpec(new FunSuite), "path.FunSpec(FunSuite)"),
            // ( new path.FreeSpec(new FunSuite), "path.FreeSpec(FunSuite)"),

            ( new Suites(new AnyFunSuite), "Suites(AnyFunSuite)"),
            ( new Sequential(new AnyFunSuite), "Sequential(AnyFunSuite)"),
            ( new Stepwise(new AnyFunSuite), "Stepwise(AnyFunSuite)"),

            ( new MandarinOrangeSuites(new AnyFunSuite), "MandarinOrangeSuites(AnyFunSuite)"),
            ( new MandarinOrangeSequential(new AnyFunSuite), "MandarinOrangeSequential(AnyFunSuite)"),
            ( new MandarinOrangeStepwise(new AnyFunSuite), "MandarinOrangeStepwise(AnyFunSuite)")
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

            ( new MandarinOrangeFunSuite(new AnyPropSpec, new AnyFeatureSpec, new AnyFunSuite), "MandarinOrangeFunSuite(AnyPropSpec, AnyFeatureSpec, AnyFunSuite)"),
            ( new MandarinOrangeFunSpec(new AnyPropSpec, new AnyFeatureSpec, new AnyFunSuite), "MandarinOrangeFunSpec(AnyPropSpec, AnyFeatureSpec, AnyFunSuite)"),
            // SKIP-SCALATESTJS,NATIVE-START
            ( new MandarinOrangeSpec(new AnyPropSpec, new AnyFeatureSpec, new AnyFunSuite), "MandarinOrangeSpec(AnyPropSpec, AnyFeatureSpec, AnyFunSuite)"),
            // SKIP-SCALATESTJS,NATIVE-END
            ( new MandarinOrangeWordSpec(new AnyPropSpec, new AnyFeatureSpec, new AnyFunSuite), "MandarinOrangeWordSpec(AnyPropSpec, AnyFeatureSpec, AnyFunSuite)"),
            ( new MandarinOrangeFlatSpec(new AnyPropSpec, new AnyFeatureSpec, new AnyFunSuite), "MandarinOrangeFlatSpec(AnyPropSpec, AnyFeatureSpec, AnyFunSuite)"),
            ( new MandarinOrangeFreeSpec(new AnyPropSpec, new AnyFeatureSpec, new AnyFunSuite), "MandarinOrangeFreeSpec(AnyPropSpec, AnyFeatureSpec, AnyFunSuite)"),
            ( new MandarinOrangeFeatureSpec(new AnyPropSpec, new AnyFeatureSpec, new AnyFunSuite), "MandarinOrangeFeatureSpec(AnyPropSpec, AnyFeatureSpec, AnyFunSuite)"),
            ( new MandarinOrangePropSpec(new AnyPropSpec, new AnyFeatureSpec, new AnyFunSuite), "MandarinOrangePropSpec(AnyPropSpec, AnyFeatureSpec, AnyFunSuite)"),

            ( new MandarinOrangeFixtureFunSuite(new AnyPropSpec, new AnyFeatureSpec, new AnyFunSuite), "MandarinOrangeFixtureFunSuite(AnyPropSpec, AnyFeatureSpec, AnyFunSuite)"),
            ( new MandarinOrangeFixtureFunSpec(new AnyPropSpec, new AnyFeatureSpec, new AnyFunSuite), "MandarinOrangeFixtureFunSpec(AnyPropSpec, AnyFeatureSpec, AnyFunSuite)"),
            ( new MandarinOrangeFixtureWordSpec(new AnyPropSpec, new AnyFeatureSpec, new AnyFunSuite), "MandarinOrangeFixtureWordSpec(AnyPropSpec, AnyFeatureSpec, AnyFunSuite)"),
            ( new MandarinOrangeFixtureFlatSpec(new AnyPropSpec, new AnyFeatureSpec, new AnyFunSuite), "MandarinOrangeFixtureFlatSpec(AnyPropSpec, AnyFeatureSpec, AnyFunSuite)"),
            ( new MandarinOrangeFixtureFreeSpec(new AnyPropSpec, new AnyFeatureSpec, new AnyFunSuite), "MandarinOrangeFixtureFreeSpec(AnyPropSpec, AnyFeatureSpec, AnyFunSuite)"),
            ( new MandarinOrangeFixtureFeatureSpec(new AnyPropSpec, new AnyFeatureSpec, new AnyFunSuite), "MandarinOrangeFixtureFeatureSpec(AnyPropSpec, AnyFeatureSpec, AnyFunSuite)"),
            ( new MandarinOrangeFixturePropSpec(new AnyPropSpec, new AnyFeatureSpec, new AnyFunSuite), "MandarinOrangeFixturePropSpec(AnyPropSpec, AnyFeatureSpec, AnyFunSuite)"),

            // ( new path.FunSpec(new PropSpec, new FeatureSpec, new FunSuite), "path.FunSpec(PropSpec, FeatureSpec, FunSuite)"),
            // ( new path.FreeSpec(new PropSpec, new FeatureSpec, new FunSuite), "path.FreeSpec(PropSpec, FeatureSpec, FunSuite)"),

            ( new Suites(new AnyPropSpec, new AnyFeatureSpec, new AnyFunSuite), "Suites(AnyPropSpec, AnyFeatureSpec, AnyFunSuite)"),
            ( new Sequential(new AnyPropSpec, new AnyFeatureSpec, new AnyFunSuite), "Sequential(AnyPropSpec, AnyFeatureSpec, AnyFunSuite)"),
            ( new Stepwise(new AnyPropSpec, new AnyFeatureSpec, new AnyFunSuite), "Stepwise(AnyPropSpec, AnyFeatureSpec, AnyFunSuite)"),

            ( new MandarinOrangeSuites(new AnyPropSpec, new AnyFeatureSpec, new AnyFunSuite), "MandarinOrangeSuites(AnyPropSpec, AnyFeatureSpec, AnyFunSuite)"),
            ( new MandarinOrangeSequential(new AnyPropSpec, new AnyFeatureSpec, new AnyFunSuite), "MandarinOrangeSequential(AnyPropSpec, AnyFeatureSpec, AnyFunSuite)"),
            ( new MandarinOrangeStepwise(new AnyPropSpec, new AnyFeatureSpec, new AnyFunSuite), "MandarinOrangeStepwise(AnyPropSpec, AnyFeatureSpec, AnyFunSuite)")
          )
        forAll (examples) { (suite, simpleName) =>
          assert(suite.toString === simpleName)
        }
      }
    }
  }

  // SKIP-SCALATESTJS,NATIVE-START
  describe("The simpleNameForTest method") {
    it("should return the correct test simple name with or without Informer") {
      val simpleNameForTest = PrivateMethod1[String, String](Symbol("simpleNameForTest"))
      assert((Suite invokePrivate simpleNameForTest("testThis")) === "testThis")
      assert((Suite invokePrivate simpleNameForTest("testThis(Informer)")) === "testThis")
      assert((Suite invokePrivate simpleNameForTest("test(Informer)")) === "test")
      assert((Suite invokePrivate simpleNameForTest("test")) === "test")
    }
  }
  // SKIP-SCALATESTJS,NATIVE-END

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
        def requestStop(): Unit = {}
        def reset(): Unit = {}
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
      y.run(None, Args(SilentReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker))

      assert(k.executed)
      assert(i.executed)
      assert(j.executed)
      assert(k.executed)
      assert(!l.executed)
      assert(!m.executed)
      assert(!n.executed)
    }
  }
  // SKIP-SCALATESTJS,NATIVE-START
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
  // SKIP-SCALATESTJS,NATIVE-END
  describe("NoArgTest") {
    it("should offer a factory method that takes another NoArgTest and a function that implements apply") {
      class SideEffectedFixtureWasSpec extends AnyFunSpec {
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
             val pos: Option[source.Position] = test.pos
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

      class NestedSuite extends AnyFunSuite with BeforeAndAfter {

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

      class NestedSuite extends AnyFunSuite with BeforeAndAfterAll {

        test("test 1") {}

        override protected def afterAll(): Unit = {
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

      class NestedSuite extends AnyFunSuite with BeforeAndAfterAllConfigMap {

        test("test 1") {}

        override protected def afterAll(configMap: ConfigMap): Unit = {
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

      class NestedSuite extends AnyFunSuite with BeforeAndAfterEach {

        test("test 1") {}

        override protected def afterEach(): Unit = {
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

      class NestedSuite extends AnyFunSuite with BeforeAndAfterEachTestData {

        test("test 1") {}

        override protected def afterEach(test: TestData): Unit = {
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

