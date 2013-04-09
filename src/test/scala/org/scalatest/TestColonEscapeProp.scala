package org.scalatest

import org.scalatest.prop.Tables
import org.scalatest.junit.JUnit3Suite
import org.scalatest.junit.JUnitSuite
import org.scalatest.testng.TestNGSuite
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.events.Ordinal
import org.scalatest.events.IndentedText
import org.junit.Test
import org.testng.annotations.{Test => TestNGTest}
import org.scalatest.events.Formatter

trait TestColonEscapeExamples extends Tables {
  def suite: Suite
  def fixtureSuite: fixture.Suite
    
  def examples =
  Table(
    ("suite", "succeeded", "failed", "ignored", "pending", "canceled"),
    (suite, "- A Succeeded Test", "- A Failed Test", "- An Ignored Test", "- A Pending Test", "- A Canceled Test"), 
    (fixtureSuite, "- A Succeeded Test", "- A Failed Test", "- An Ignored Test", "- A Pending Test", "- A Canceled Test"))
}
  
trait NonTestColonEscapeExamples extends Tables {
  def funSuite: FunSuite
  def fixtureFunSuite: fixture.FunSuite
  def funSpec: FunSpec
  def fixtureFunSpec: fixture.FunSpec
  def featureSpec: FeatureSpec
  def fixtureFeatureSpec: fixture.FeatureSpec
  def flatSpec: FlatSpec
  def fixtureFlatSpec: fixture.FlatSpec
  def freeSpec: FreeSpec
  def fixtureFreeSpec: fixture.FreeSpec
  def propSpec: PropSpec
  def fixturePropSpec: fixture.PropSpec
  def wordSpec: WordSpec
  def fixtureWordSpec: fixture.WordSpec
  def pathFreeSpec: path.FreeSpec
  def pathFunSpec: path.FunSpec
  def junit3Suite: JUnit3Suite
  def junitSuite: JUnitSuite
  def testngSuite: TestNGSuite
    
  def examples =
  Table(
    ("suite", "succeeded", "failed", "ignored", "pending", "canceled"),
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
    (pathFunSpec, Some("- test: A Succeeded Test"), Some("- test: A Failed Test"), Some("- test: An Ignored Test"), Some("- test: A Pending Test"), Some("- test: A Canceled Test")),
    (junit3Suite, Some("- test: A Succeeded Test(org.scalatest.TestColonEscapeExampleJUnit3Suite)"), Some("- test: A Failed Test(org.scalatest.TestColonEscapeExampleJUnit3Suite)"), None, None, None),
    (junitSuite, Some("- test: A Succeeded Test"), Some("- test: A Failed Test"), Some("- test: An Ignored Test"), None, None),
    (testngSuite, Some("- test: A Succeeded Test"), Some("- test: A Failed Test"), Some("- test: An Ignored Test"), None, None))
}

class TestColonEscapeProp extends FunSuite with TestColonEscapeExamples with TableDrivenPropertyChecks with ShouldMatchers with SharedHelpers {

  def assertFormattedText(formatter: Option[Formatter], expected: String) {
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
  }
  
  test("Suite and fixture.Suite should escape 'test:' prefix in its IndentedText's formattedText") {
    forAll(examples) { (suite, succeeded, failed, ignored, pending, canceled) =>
      val reporter = new EventRecordingReporter
      suite.run(None, Args(reporter))
      
      assert(reporter.testSucceededEventsReceived.size === 1)
      val testSucceeded = reporter.testSucceededEventsReceived(0)
      assertFormattedText(testSucceeded.formatter, succeeded)
      
      assert(reporter.testFailedEventsReceived.size === 1)
      val testFailed = reporter.testFailedEventsReceived(0)
      assertFormattedText(testFailed.formatter, failed)
      
      assert(reporter.testIgnoredEventsReceived.size === 1)
      val testIgnored = reporter.testIgnoredEventsReceived(0)
      assertFormattedText(testIgnored.formatter, ignored)
      
      assert(reporter.testPendingEventsReceived.size === 1)
      val testPending = reporter.testPendingEventsReceived(0)
      assertFormattedText(testPending.formatter, pending)
      
      assert(reporter.testCanceledEventsReceived.size === 1)
      val testCanceled = reporter.testCanceledEventsReceived(0)
      assertFormattedText(testCanceled.formatter, canceled)
    }
  }
  
  def suite = new ExampleSuite()
  class ExampleSuite extends Suite {
    def `test: A Succeeded Test`() {}
    def `test: A Failed Test`() { fail }
    @Ignore def `test: An Ignored Test`() {}
    def `test: A Pending Test`() { pending }
    def `test: A Canceled Test`() { cancel }
  }
  
  def fixtureSuite = new ExampleFixtureSuite
  class ExampleFixtureSuite extends fixture.Suite with StringFixture {
    def `test: A Succeeded Test`() {}
    def `test: A Failed Test`() { fail }
    @Ignore def `test: An Ignored Test`() {}
    def `test: A Pending Test`() { pending }
    def `test: A Canceled Test`() { cancel }
  }
}

@DoNotDiscover
class TestColonEscapeExampleJUnit3Suite extends JUnit3Suite {
  def `test: A Succeeded Test`() {}
  def `test: A Failed Test`() { _root_.junit.framework.Assert.assertEquals("fail on purpose", 1, 2) }
}

@DoNotDiscover
class  TestColonEscapeExampleJUnitSuite extends JUnitSuite {
  @Test def `test: A Succeeded Test`() {}
  @Test def `test: A Failed Test`() { _root_.org.junit.Assert.assertEquals(1, 2) }
  @_root_.org.junit.Ignore @Test def `test: An Ignored Test`() {}
}

@DoNotDiscover
class TestColonEscapeExampleTestNGSuite extends TestNGSuite {
  @TestNGTest def `test: A Succeeded Test`() { }
  @TestNGTest(groups = Array("run")) def `test: A Failed Test`() { _root_.org.testng.Assert.assertEquals(1, 2) }
  @TestNGTest(dependsOnGroups = Array("run")) def `test: An Ignored Test`() {}
}

@DoNotDiscover
class TestColonEscapeExamplePathFreeSpec extends path.FreeSpec {
  "A Scope" - {
    "test: A Succeeded Test" in {}
    "test: A Failed Test" in { fail }
    "test: An Ignored Test" ignore {}
    "test: A Pending Test" in { pending }
    "test: A Canceled Test" in { cancel }
  }
}

@DoNotDiscover
class TestColonEscapeExamplePathFunSpec extends path.FunSpec {
  describe("A Spec") {
    it("test: A Succeeded Test") { }
    it("test: A Failed Test") { fail }
    ignore("test: An Ignored Test") { }
    it("test: A Pending Test") { pending }
    it("test: A Canceled Test") { cancel }
  }
}

class NonTestColonEscapeProp extends FunSuite with NonTestColonEscapeExamples with TableDrivenPropertyChecks with ShouldMatchers with SharedHelpers {
  
  def assertFormattedText(formatter: Option[Formatter], expected: Option[String]) {
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
  
  def funSuite = new ExampleFunSuite()
  class ExampleFunSuite extends FunSuite {
    test("test: A Succeeded Test") {}
    test("test: A Failed Test") { fail }
    ignore("test: An Ignored Test") {}
    test("test: A Pending Test") { pending }
    test("test: A Canceled Test") { cancel }
  }
  
  def fixtureFunSuite = new ExampleFixtureFunSuite
  class ExampleFixtureFunSuite extends fixture.FunSuite with StringFixture {
    test("test: A Succeeded Test") { fixture => }
    test("test: A Failed Test") { fixture => fail }
    ignore("test: An Ignored Test") { fixture => }
    test("test: A Pending Test") { fixture => pending }
    test("test: A Canceled Test") { fixture => cancel }
  }
  
  def funSpec = new ExampleFunSpec
  class ExampleFunSpec extends FunSpec {
    describe("A Spec") {
      it("test: A Succeeded Test") {}
      it("test: A Failed Test") { fail }
      ignore("test: An Ignored Test") {}
      it("test: A Pending Test") { pending }
      it("test: A Canceled Test") { cancel }
    }
  }
  
  def fixtureFunSpec = new ExampleFixtureFunSpec
  class ExampleFixtureFunSpec extends fixture.FunSpec with StringFixture {
    describe("A Spec") {
      it("test: A Succeeded Test") { fixture => }
      it("test: A Failed Test") { fixture => fail }
      ignore("test: An Ignored Test") { fixture => }
      it("test: A Pending Test") { fixture => pending }
      it("test: A Canceled Test") { fixture => cancel }
    }
  }
  
  def featureSpec = new ExampleFeatureSpec
  class ExampleFeatureSpec extends FeatureSpec {
    scenario("test: A Succeeded Test") {}
    scenario("test: A Failed Test") { fail }
    ignore("test: An Ignored Test") {}
    scenario("test: A Pending Test") { pending }
    scenario("test: A Canceled Test") { cancel }
  }
  
  def fixtureFeatureSpec = new ExampleFixtureFeatureSpec
  class ExampleFixtureFeatureSpec extends fixture.FeatureSpec with StringFixture {
    scenario("test: A Succeeded Test") { fixture => }
    scenario("test: A Failed Test") { fixture => fail }
    ignore("test: An Ignored Test") { fixture => }
    scenario("test: A Pending Test") { fixture => pending }
    scenario("test: A Canceled Test") { fixture => cancel }
  }
  
  def flatSpec = new ExampleFlatSpec
  class ExampleFlatSpec extends FlatSpec {
    "A Scope" should "test: A Succeeded Test" in {}
    "A Scope" should "test: A Failed Test" in { fail }
    "A Scope" should "test: An Ignored Test" ignore {}
    "A Scope" should "test: A Pending Test" in { pending }
    "A Scope" should "test: A Canceled Test" in { cancel }
  }
  
  def fixtureFlatSpec = new ExampleFixtureFlatSpec
  class ExampleFixtureFlatSpec extends fixture.FlatSpec with StringFixture {
    "A Scope" should "test: A Succeeded Test" in { fixture => }
    "A Scope" should "test: A Failed Test" in { fixture => fail }
    "A Scope" should "test: An Ignored Test" ignore { fixture => }
    "A Scope" should "test: A Pending Test" in { fixture => pending }
    "A Scope" should "test: A Canceled Test" in { fixture =>  cancel }
  }
  
  def freeSpec = new ExampleFreeSpec
  class ExampleFreeSpec extends FreeSpec {
    "A Scope" - {
      "test: A Succeeded Test" in {}
      "test: A Failed Test" in { fail }
      "test: An Ignored Test" ignore {}
      "test: A Pending Test" in { pending }
      "test: A Canceled Test" in { cancel }
    }
  }
  
  def fixtureFreeSpec = new ExampleFixtureFreeSpec
  class ExampleFixtureFreeSpec extends fixture.FreeSpec with StringFixture {
    "A Scope" - {
      "test: A Succeeded Test" in { fixture => }
      "test: A Failed Test" in { fixture => fail }
      "test: An Ignored Test" ignore { fixture => }
      "test: A Pending Test" in { fixture => pending }
      "test: A Canceled Test" in { fixture => cancel }
    }
  }
  
  def propSpec = new ExamplePropSpec
  class ExamplePropSpec extends PropSpec {
    property("test: A Succeeded Test") {}
    property("test: A Failed Test") { fail }
    ignore("test: An Ignored Test") {}
    property("test: A Pending Test") { pending }
    property("test: A Canceled Test") { cancel }
  }
  
  def fixturePropSpec = new ExampleFixturePropSpec
  class ExampleFixturePropSpec extends fixture.PropSpec with StringFixture {
    property("test: A Succeeded Test") { fixture => }
    property("test: A Failed Test") { fixture =>  fail }
    ignore("test: An Ignored Test") { fixture => }
    property("test: A Pending Test") { fixture =>  pending }
    property("test: A Canceled Test") { fixture =>  cancel }
  }
  
  def wordSpec = new ExampleWordSpec
  class ExampleWordSpec extends WordSpec {
    "A Scope" should {
      "test: A Succeeded Test" in {}
      "test: A Failed Test" in { fail }
      "test: An Ignored Test" ignore {}
      "test: A Pending Test" in { pending }
      "test: A Canceled Test" in { cancel }
    }
  }
  
  def fixtureWordSpec = new ExampleFixtureWordSpec
  class ExampleFixtureWordSpec extends fixture.WordSpec with StringFixture {
    "A Scope" should { 
      "test: A Succeeded Test" in { fixture => }
      "test: A Failed Test" in { fixture =>  fail }
      "test: An Ignored Test" ignore { fixture => }
      "test: A Pending Test" in { fixture => pending }
      "test: A Canceled Test" in { fixture => cancel }
    }
  }
  
  def pathFreeSpec = new TestColonEscapeExamplePathFreeSpec
  def pathFunSpec = new TestColonEscapeExamplePathFunSpec
  def junit3Suite = new TestColonEscapeExampleJUnit3Suite
  def junitSuite = new TestColonEscapeExampleJUnitSuite
  def testngSuite = new TestColonEscapeExampleTestNGSuite
}