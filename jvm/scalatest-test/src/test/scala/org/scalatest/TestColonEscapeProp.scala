/*
 * Copyright 2001-2024 Artima, Inc.
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

import org.scalatest.prop.Tables
// SKIP-SCALATESTJS,NATIVE-START
import org.junit.Test
import org.scalatestplus.junit.JUnit3Suite
import org.scalatestplus.junit.JUnitSuite
import org.scalatest.refspec.RefSpec
import org.scalatestplus.testng.TestNGSuite
import org.testng.annotations.{Test => TestNGTest}
// SKIP-SCALATESTJS,NATIVE-END
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.matchers.should.Matchers._
import org.scalatest.events.Ordinal
import org.scalatest.events.IndentedText
import org.scalatest.events.Formatter
import SharedHelpers._
import org.scalatest.{ featurespec, flatspec, freespec, funspec, funsuite, propspec, wordspec }
import org.scalatest.featurespec.AnyFeatureSpec
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatest.wordspec.AnyWordSpec

/*
This test could perhaps just be dropped. The history is that before I had the idea to
do a full on Spec with `names like this`, i had and implemented the idea of having
Suite itself treat names like `test: bla bla bla` specially in that it dropped the test$colon$$space$
from the front of the test name. The suites in this file made sure Suite had that behavior, and none
of the others styles had it. Then I decided to just deprecate Suite as a style trait, but I left in
this behavior during the deprecation period. The deprecation period for Suite as a style triat has
expired, so now it is only testing that no style traits handle test: prefixes specially.
*/
trait NonTestColonEscapeExamples extends Tables {
  // SKIP-SCALATESTJS,NATIVE-START
  def spec: RefSpec
  def junit3Suite: JUnit3Suite
  def junitSuite: JUnitSuite
  def testngSuite: TestNGSuite
  // SKIP-SCALATESTJS,NATIVE-END
  def funSuite: AnyFunSuite
  def fixtureFunSuite: funsuite.FixtureAnyFunSuite
  def funSpec: AnyFunSpec
  def fixtureFunSpec: funspec.FixtureAnyFunSpec
  def featureSpec: AnyFeatureSpec
  def fixtureFeatureSpec: featurespec.FixtureAnyFeatureSpec
  def flatSpec: AnyFlatSpec
  def fixtureFlatSpec: flatspec.FixtureAnyFlatSpec
  def freeSpec: AnyFreeSpec
  def fixtureFreeSpec: freespec.FixtureAnyFreeSpec
  def propSpec: AnyPropSpec
  def fixturePropSpec: propspec.FixtureAnyPropSpec
  def wordSpec: AnyWordSpec
  def fixtureWordSpec: wordspec.FixtureAnyWordSpec
  def pathFreeSpec: freespec.PathAnyFreeSpec
  def pathFunSpec: funspec.PathAnyFunSpec
    
  def examples =
  Table(
    ("suite", "succeeded", "failed", "ignored", "pending", "canceled"),
    // SKIP-SCALATESTJS,NATIVE-START
    (spec, Some("- test: A Succeeded Test"), Some("- test: A Failed Test"), Some("- test: An Ignored Test"), Some("- test: A Pending Test"), Some("- test: A Canceled Test")),
    (junit3Suite, Some("- test: A Succeeded Test(org.scalatest.TestColonEscapeExampleJUnit3Suite)"), Some("- test: A Failed Test(org.scalatest.TestColonEscapeExampleJUnit3Suite)"), None, None, None),
    (junitSuite, Some("- test: A Succeeded Test"), Some("- test: A Failed Test"), Some("- test: An Ignored Test"), None, None),
    (testngSuite, Some("- test: A Succeeded Test"), Some("- test: A Failed Test"), None, None, Some("- test: An Canceled Test")),
    // SKIP-SCALATESTJS,NATIVE-END
    (funSuite, Some("- test: A Succeeded Test"), Some("- test: A Failed Test"), Some("- test: An Ignored Test"), Some("- test: A Pending Test"), Some("- test: A Canceled Test")),
    (fixtureFunSuite, Some("- test: A Succeeded Test"), Some("- test: A Failed Test"), Some("- test: An Ignored Test"), Some("- test: A Pending Test"), Some("- test: A Canceled Test")), 
    (funSpec, Some("- test: A Succeeded Test"), Some("- test: A Failed Test"), Some("- test: An Ignored Test"), Some("- test: A Pending Test"), Some("- test: A Canceled Test")),
    (fixtureFunSpec, Some("- test: A Succeeded Test"), Some("- test: A Failed Test"), Some("- test: An Ignored Test"), Some("- test: A Pending Test"), Some("- test: A Canceled Test")),
    (featureSpec, Some("Scenario: test: A Succeeded Test"), Some("Scenario: test: A Failed Test"), Some("- Scenario: test: An Ignored Test"), Some("Scenario: test: A Pending Test"), Some("Scenario: test: A Canceled Test")),
    (fixtureFeatureSpec, Some("Scenario: test: A Succeeded Test"), Some("Scenario: test: A Failed Test"), Some("- Scenario: test: An Ignored Test"), Some("Scenario: test: A Pending Test"), Some("Scenario: test: A Canceled Test")),
    (flatSpec, Some("- should test: A Succeeded Test"), Some("- should test: A Failed Test"), Some("- should test: An Ignored Test"), Some("- should test: A Pending Test"), Some("- should test: A Canceled Test")),
    (fixtureFlatSpec, Some("- should test: A Succeeded Test"), Some("- should test: A Failed Test"), Some("- should test: An Ignored Test"), Some("- should test: A Pending Test"), Some("- should test: A Canceled Test")),
    (freeSpec, Some("- test: A Succeeded Test"), Some("- test: A Failed Test"), Some("- test: An Ignored Test"), Some("- test: A Pending Test"), Some("- test: A Canceled Test")),
    (fixtureFreeSpec, Some("- test: A Succeeded Test"), Some("- test: A Failed Test"), Some("- test: An Ignored Test"), Some("- test: A Pending Test"), Some("- test: A Canceled Test")),
    (propSpec, Some("- test: A Succeeded Test"), Some("- test: A Failed Test"), Some("- test: An Ignored Test"), Some("- test: A Pending Test"), Some("- test: A Canceled Test")),
    (fixturePropSpec, Some("- test: A Succeeded Test"), Some("- test: A Failed Test"), Some("- test: An Ignored Test"), Some("- test: A Pending Test"), Some("- test: A Canceled Test")),
    (wordSpec, Some("- should test: A Succeeded Test"), Some("- should test: A Failed Test"), Some("- should test: An Ignored Test"), Some("- should test: A Pending Test"), Some("- should test: A Canceled Test")),
    (fixtureWordSpec, Some("- should test: A Succeeded Test"), Some("- should test: A Failed Test"), Some("- should test: An Ignored Test"), Some("- should test: A Pending Test"), Some("- should test: A Canceled Test")),
    (pathFreeSpec, Some("- test: A Succeeded Test"), Some("- test: A Failed Test"), Some("- test: An Ignored Test"), Some("- test: A Pending Test"), Some("- test: A Canceled Test")),
    (pathFunSpec, Some("- test: A Succeeded Test"), Some("- test: A Failed Test"), Some("- test: An Ignored Test"), Some("- test: A Pending Test"), Some("- test: A Canceled Test"))
  )
}

// SKIP-SCALATESTJS,NATIVE-START
@DoNotDiscover
class TestColonEscapeExampleJUnit3Suite extends JUnit3Suite {
  def `test: A Succeeded Test`(): Unit = {}
  def `test: A Failed Test`(): Unit = { _root_.junit.framework.Assert.assertEquals("fail on purpose", 1, 2) }
}

@DoNotDiscover
class  TestColonEscapeExampleJUnitSuite extends JUnitSuite {
  @Test def `test: A Succeeded Test`(): Unit = {}
  @Test def `test: A Failed Test`(): Unit = { _root_.org.junit.Assert.assertEquals(1, 2) }
  @_root_.org.junit.Ignore @Test def `test: An Ignored Test`(): Unit = {}
}

@DoNotDiscover
class TestColonEscapeExampleTestNGSuite extends TestNGSuite {
  @TestNGTest def `test: A Succeeded Test`(): Unit = { }
  @TestNGTest(groups = Array("run")) def `test: A Failed Test`(): Unit = { _root_.org.testng.Assert.assertEquals(1, 2) }
  @TestNGTest(dependsOnGroups = Array("run")) def `test: An Canceled Test`(): Unit = {}
}
// SKIP-SCALATESTJS,NATIVE-END

@DoNotDiscover
protected[scalatest] class TestColonEscapeExamplePathFreeSpec extends freespec.PathAnyFreeSpec {
  "A Scope" - {
    "test: A Succeeded Test" in {}
    "test: A Failed Test" in { fail() }
    "test: An Ignored Test" ignore {}
    "test: A Pending Test" in { pending }
    "test: A Canceled Test" in { cancel() }
  }
  override def newInstance: freespec.PathAnyFreeSpecLike = new TestColonEscapeExamplePathFreeSpec
}

@DoNotDiscover
protected[scalatest] class TestColonEscapeExamplePathFunSpec extends funspec.PathAnyFunSpec {
  describe("A Spec") {
    it("test: A Succeeded Test") { }
    it("test: A Failed Test") { fail() }
    ignore("test: An Ignored Test") { }
    it("test: A Pending Test") { pending }
    it("test: A Canceled Test") { cancel() }
  }
  override def newInstance: funspec.PathAnyFunSpecLike = new TestColonEscapeExamplePathFunSpec
}

class NonTestColonEscapeProp extends AnyFunSuite with NonTestColonEscapeExamples {
  
  def assertFormattedText(formatter: Option[Formatter], expected: Option[String]): Unit = {
    expected match {
      case Some(expected) => 
        formatter match {
          case Some(formatter) =>
            formatter match {
              case IndentedText(formattedText, _, _) =>
                assert(formattedText === expected)
              case _ =>
                fail("Expected Some(IndentedText as formatter, but got: " + formatter)
            }
          case None =>
            fail("Expected Some(IndentedText) as formatter, but got None.")
        }
      case None =>
    }
  }
  
  test("All others style traits besides Suite and fixture.Suite should not escape 'test:' prefix in its IndentedText's formattedText") {
    forAll(examples) { (suite, succeeded, failed, ignored, pending, canceled) =>
      val reporter = new EventRecordingReporter
      suite.run(None, Args(reporter))

      if (succeeded.isDefined) {
        assert(reporter.testSucceededEventsReceived.size === 1)
        val testSucceeded = reporter.testSucceededEventsReceived(0)
        assertFormattedText(testSucceeded.formatter, succeeded)
      }

      if (failed.isDefined) {
        assert(reporter.testFailedEventsReceived.size === 1)
        val testFailed = reporter.testFailedEventsReceived(0)
        assertFormattedText(testFailed.formatter, failed)
      }

      if (ignored.isDefined) {
        assert(reporter.testIgnoredEventsReceived.size === 1)
        val testIgnored = reporter.testIgnoredEventsReceived(0)
        assertFormattedText(testIgnored.formatter, ignored)
      }

      if (pending.isDefined) {
        assert(reporter.testPendingEventsReceived.size === 1)
        val testPending = reporter.testPendingEventsReceived(0)
        assertFormattedText(testPending.formatter, pending)
      }

      if (canceled.isDefined) {
        assert(reporter.testCanceledEventsReceived.size === 1)
        val testCanceled = reporter.testCanceledEventsReceived(0)
        assertFormattedText(testCanceled.formatter, canceled)
      }
    }
  }

  // SKIP-SCALATESTJS,NATIVE-START
  def spec = new ExampleSpec()
  class ExampleSpec extends RefSpec {
    def `test: A Succeeded Test`: Unit = {}
    def `test: A Failed Test` = { fail() }
    @Ignore def `test: An Ignored Test`: Unit = {}
    def `test: A Pending Test` = { pending }
    def `test: A Canceled Test` = { cancel() }
  }
  // SKIP-SCALATESTJS,NATIVE-END

  def funSuite = new ExampleFunSuite()
  class ExampleFunSuite extends AnyFunSuite {
    test("test: A Succeeded Test") {}
    test("test: A Failed Test") { fail() }
    ignore("test: An Ignored Test") {}
    test("test: A Pending Test") { pending }
    test("test: A Canceled Test") { cancel() }
  }
  
  def fixtureFunSuite = new ExampleFixtureFunSuite
  class ExampleFixtureFunSuite extends funsuite.FixtureAnyFunSuite with StringFixture {
    test("test: A Succeeded Test") { fixture => }
    test("test: A Failed Test") { fixture => fail() }
    ignore("test: An Ignored Test") { fixture => }
    test("test: A Pending Test") { fixture => pending }
    test("test: A Canceled Test") { fixture => cancel() }
  }
  
  def funSpec = new ExampleFunSpec
  class ExampleFunSpec extends AnyFunSpec {
    describe("A Spec") {
      it("test: A Succeeded Test") {}
      it("test: A Failed Test") { fail() }
      ignore("test: An Ignored Test") {}
      it("test: A Pending Test") { pending }
      it("test: A Canceled Test") { cancel() }
    }
  }
  
  def fixtureFunSpec = new ExampleFixtureFunSpec
  class ExampleFixtureFunSpec extends funspec.FixtureAnyFunSpec with StringFixture {
    describe("A Spec") {
      it("test: A Succeeded Test") { fixture => }
      it("test: A Failed Test") { fixture => fail() }
      ignore("test: An Ignored Test") { fixture => }
      it("test: A Pending Test") { fixture => pending }
      it("test: A Canceled Test") { fixture => cancel() }
    }
  }
  
  def featureSpec = new ExampleFeatureSpec
  class ExampleFeatureSpec extends AnyFeatureSpec {
    Scenario("test: A Succeeded Test") {}
    Scenario("test: A Failed Test") { fail() }
    ignore("test: An Ignored Test") {}
    Scenario("test: A Pending Test") { pending }
    Scenario("test: A Canceled Test") { cancel() }
  }
  
  def fixtureFeatureSpec = new ExampleFixtureFeatureSpec
  class ExampleFixtureFeatureSpec extends featurespec.FixtureAnyFeatureSpec with StringFixture {
    Scenario("test: A Succeeded Test") { fixture => }
    Scenario("test: A Failed Test") { fixture => fail() }
    ignore("test: An Ignored Test") { fixture => }
    Scenario("test: A Pending Test") { fixture => pending }
    Scenario("test: A Canceled Test") { fixture => cancel() }
  }
  
  def flatSpec = new ExampleFlatSpec
  class ExampleFlatSpec extends AnyFlatSpec {
    "A Scope" should "test: A Succeeded Test" in {}
    "A Scope" should "test: A Failed Test" in { fail() }
    "A Scope" should "test: An Ignored Test" ignore {}
    "A Scope" should "test: A Pending Test" in { pending }
    "A Scope" should "test: A Canceled Test" in { cancel() }
  }
  
  def fixtureFlatSpec = new ExampleFixtureFlatSpec
  class ExampleFixtureFlatSpec extends flatspec.FixtureAnyFlatSpec with StringFixture {
    "A Scope" should "test: A Succeeded Test" in { fixture => }
    "A Scope" should "test: A Failed Test" in { fixture => fail() }
    "A Scope" should "test: An Ignored Test" ignore { fixture => }
    "A Scope" should "test: A Pending Test" in { fixture => pending }
    "A Scope" should "test: A Canceled Test" in { fixture =>  cancel() }
  }
  
  def freeSpec = new ExampleFreeSpec
  class ExampleFreeSpec extends AnyFreeSpec {
    "A Scope" - {
      "test: A Succeeded Test" in {}
      "test: A Failed Test" in { fail() }
      "test: An Ignored Test" ignore {}
      "test: A Pending Test" in { pending }
      "test: A Canceled Test" in { cancel() }
    }
  }
  
  def fixtureFreeSpec = new ExampleFixtureFreeSpec
  class ExampleFixtureFreeSpec extends freespec.FixtureAnyFreeSpec with StringFixture {
    "A Scope" - {
      "test: A Succeeded Test" in { fixture => }
      "test: A Failed Test" in { fixture => fail() }
      "test: An Ignored Test" ignore { fixture => }
      "test: A Pending Test" in { fixture => pending }
      "test: A Canceled Test" in { fixture => cancel() }
    }
  }
  
  def propSpec = new ExamplePropSpec
  class ExamplePropSpec extends AnyPropSpec {
    property("test: A Succeeded Test") {}
    property("test: A Failed Test") { fail() }
    ignore("test: An Ignored Test") {}
    property("test: A Pending Test") { pending }
    property("test: A Canceled Test") { cancel() }
  }
  
  def fixturePropSpec = new ExampleFixturePropSpec
  class ExampleFixturePropSpec extends propspec.FixtureAnyPropSpec with StringFixture {
    property("test: A Succeeded Test") { fixture => }
    property("test: A Failed Test") { fixture =>  fail() }
    ignore("test: An Ignored Test") { fixture => }
    property("test: A Pending Test") { fixture =>  pending }
    property("test: A Canceled Test") { fixture =>  cancel() }
  }
  
  def wordSpec = new ExampleWordSpec
  class ExampleWordSpec extends AnyWordSpec {
    "A Scope" should {
      "test: A Succeeded Test" in {}
      "test: A Failed Test" in { fail() }
      "test: An Ignored Test" ignore {}
      "test: A Pending Test" in { pending }
      "test: A Canceled Test" in { cancel() }
    }
  }
  
  def fixtureWordSpec = new ExampleFixtureWordSpec
  class ExampleFixtureWordSpec extends wordspec.FixtureAnyWordSpec with StringFixture {
    "A Scope" should { 
      "test: A Succeeded Test" in { fixture => }
      "test: A Failed Test" in { fixture =>  fail() }
      "test: An Ignored Test" ignore { fixture => }
      "test: A Pending Test" in { fixture => pending }
      "test: A Canceled Test" in { fixture => cancel() }
    }
  }
  
  def pathFreeSpec = new TestColonEscapeExamplePathFreeSpec
  def pathFunSpec = new TestColonEscapeExamplePathFunSpec
  // SKIP-SCALATESTJS,NATIVE-START
  def junit3Suite = new TestColonEscapeExampleJUnit3Suite
  def junitSuite = new TestColonEscapeExampleJUnitSuite
  def testngSuite = new TestColonEscapeExampleTestNGSuite
  // SKIP-SCALATESTJS,NATIVE-END
}
