package org.scalatest

import org.scalatest.events.Event
import org.scalatest.prop.Tables
import org.scalatest.events.TestStarting
import org.scalatest.events.TestSucceeded
import scala.collection.mutable.ListBuffer
import org.scalatest.events.ScopeClosed
import org.scalatest.time.Span
import org.scalatest.time.Millis

trait SuiteTimeoutSetting { s: ParallelTestExecution =>
  override abstract def sortingTimeout: Span = Span(300, Millis)
}

trait SuiteTimeoutSuites extends EventHelpers {
  def suite1: Suite with SuiteTimeoutSetting
  def suite2: Suite with SuiteTimeoutSetting
  val holdingSuiteId: String
  val holdingTestName: String
  val holdingScopeClosedName: Option[String]
  val holdUntilEventCount: Int
  def assertSuiteTimeoutTest(events: List[Event])
}

class SuiteHoldingReporter(dispatch: Reporter, holdingSuiteId: String, holdingTestName: String, holdingScopeClosedName: Option[String]) extends CatchReporter {
  val out = System.err
  private val holdEvents = new ListBuffer[Event]()
  override protected def doApply(event: Event) {
    event match {
      case testStarting: TestStarting if testStarting.suiteId == holdingSuiteId && testStarting.testName == holdingTestName => 
        holdEvents += testStarting
      case testSucceeded: TestSucceeded if testSucceeded.suiteId == holdingSuiteId && testSucceeded.testName == holdingTestName =>
        holdEvents += testSucceeded
      case scopeClosed: ScopeClosed if holdingScopeClosedName.isDefined && scopeClosed.message == holdingScopeClosedName.get => 
        holdEvents += scopeClosed
      case _ => dispatch(event)
    }
  }
  protected def doDispose() {}
  def fireHoldEvents() {
    holdEvents.foreach(dispatch(_))
  }
}

trait ParallelTestExecutionSuiteTimeoutExamples extends Tables {
  
  def suiteTimeoutExamples = 
    Table(
      "pair", 
      new ExampleParallelTestExecutionSuiteTimeoutSuitePair, 
      new ExampleParallelTestExecutionSuiteTimeoutSpecPair, 
      new ExampleParallelTestExecutionSuiteTimeoutFunSuitePair, 
      new ExampleParallelTestExecutionSuiteTimeoutFunSpecPair, 
      new ExampleParallelTestExecutionSuiteTimeoutFeatureSpecPair,
      new ExampleParallelTestExecutionSuiteTimeoutFlatSpecPair,
      new ExampleParallelTestExecutionSuiteTimeoutFreeSpecPair,
      new ExampleParallelTestExecutionSuiteTimeoutPropSpecPair,
      new ExampleParallelTestExecutionSuiteTimeoutWordSpecPair
    )
}

class ExampleParallelTestExecutionSuiteTimeoutSuitePair extends SuiteTimeoutSuites {
  def suite1 = new ExampleParallelTestExecutionSuiteTimeoutSuite
  def suite2 = new ExampleParallelTestExecutionSuiteTimeoutFixtureSuite
  val holdingSuiteId = suite1.suiteId
  val holdingTestName = "testMethod3"
  val holdingScopeClosedName = None
  val holdUntilEventCount = 13
  def assertSuiteTimeoutTest(events: List[Event]) {
    assert(events.size === 16)
    
    checkSuiteStarting(events(0), suite1.suiteId)
    checkTestStarting(events(1), "testMethod1")
    checkTestSucceeded(events(2), "testMethod1")
    checkTestStarting(events(3), "testMethod2")
    checkTestSucceeded(events(4), "testMethod2")
    
    checkSuiteStarting(events(5), suite2.suiteId)
    checkTestStarting(events(6), "testFixtureMethod1")
    checkTestSucceeded(events(7), "testFixtureMethod1")
    checkTestStarting(events(8), "testFixtureMethod2")
    checkTestSucceeded(events(9), "testFixtureMethod2")
    checkTestStarting(events(10), "testFixtureMethod3")
    checkTestSucceeded(events(11), "testFixtureMethod3")
    checkSuiteCompleted(events(12), suite2.suiteId)
    
    checkTestStarting(events(13), "testMethod3")
    checkTestSucceeded(events(14), "testMethod3")
    checkSuiteCompleted(events(15), suite1.suiteId)
  }
}

class ExampleParallelTestExecutionSuiteTimeoutSpecPair extends SuiteTimeoutSuites {
  def suite1 = new ExampleParallelTestExecutionSuiteTimeoutSpec
  def suite2 = new ExampleParallelTestExecutionSuiteTimeoutFixtureSpec
  val holdingSuiteId = suite1.suiteId
  val holdingTestName = "test 3"
  val holdingScopeClosedName = None
  val holdUntilEventCount = 13
  def assertSuiteTimeoutTest(events: List[Event]) {
    assert(events.size === 16)
    
    checkSuiteStarting(events(0), suite1.suiteId)
    checkTestStarting(events(1), "test 1")
    checkTestSucceeded(events(2), "test 1")
    checkTestStarting(events(3), "test 2")
    checkTestSucceeded(events(4), "test 2")
    
    checkSuiteStarting(events(5), suite2.suiteId)
    checkTestStarting(events(6), "test 1")
    checkTestSucceeded(events(7), "test 1")
    checkTestStarting(events(8), "test 2")
    checkTestSucceeded(events(9), "test 2")
    checkTestStarting(events(10), "test 3")
    checkTestSucceeded(events(11), "test 3")
    checkSuiteCompleted(events(12), suite2.suiteId)
    
    checkTestStarting(events(13), "test 3")
    checkTestSucceeded(events(14), "test 3")
    checkSuiteCompleted(events(15), suite1.suiteId)
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionSuiteTimeoutSuite extends Suite with ParallelTestExecution with SuiteTimeoutSetting {
  def testMethod1() {}
  def testMethod2() {}
  def testMethod3() {}
}

@DoNotDiscover
class ExampleParallelTestExecutionSuiteTimeoutFixtureSuite extends fixture.Suite with ParallelTestExecution with SuiteTimeoutSetting with StringFixture {
  def testFixtureMethod1() {}
  def testFixtureMethod2() {}
  def testFixtureMethod3() {}
}

@DoNotDiscover
class ExampleParallelTestExecutionSuiteTimeoutSpec extends Spec with ParallelTestExecution with SuiteTimeoutSetting {
  def `test 1` {}
  def `test 2` {}
  def `test 3` {}
}

@DoNotDiscover
class ExampleParallelTestExecutionSuiteTimeoutFixtureSpec extends fixture.Spec with ParallelTestExecution with SuiteTimeoutSetting with StringFixture {
  def `test 1`(fixture: String) {}
  def `test 2`(fixture: String) {}
  def `test 3`(fixture: String) {}
}

class ExampleParallelTestExecutionSuiteTimeoutFunSuitePair extends SuiteTimeoutSuites {
  def suite1 = new ExampleParallelTestExecutionSuiteTimeoutFunSuite
  def suite2 = new ExampleParallelTestExecutionSuiteTimeoutFixtureFunSuite
  val holdingSuiteId = suite1.suiteId
  val holdingTestName = "Test 3"
  val holdingScopeClosedName = None
  val holdUntilEventCount = 13
  def assertSuiteTimeoutTest(events: List[Event]) {
    assert(events.size === 16)
    
    checkSuiteStarting(events(0), suite1.suiteId)
    checkTestStarting(events(1), "Test 1")
    checkTestSucceeded(events(2), "Test 1")
    checkTestStarting(events(3), "Test 2")
    checkTestSucceeded(events(4), "Test 2")
    
    checkSuiteStarting(events(5), suite2.suiteId)
    checkTestStarting(events(6), "Fixture Test 1")
    checkTestSucceeded(events(7), "Fixture Test 1")
    checkTestStarting(events(8), "Fixture Test 2")
    checkTestSucceeded(events(9), "Fixture Test 2")
    checkTestStarting(events(10), "Fixture Test 3")
    checkTestSucceeded(events(11), "Fixture Test 3")
    checkSuiteCompleted(events(12), suite2.suiteId)
    
    checkTestStarting(events(13), "Test 3")
    checkTestSucceeded(events(14), "Test 3")
    checkSuiteCompleted(events(15), suite1.suiteId)
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionSuiteTimeoutFunSuite extends FunSuite with ParallelTestExecution with SuiteTimeoutSetting {
  test("Test 1") {}
  test("Test 2") {}
  test("Test 3") {}  
}

@DoNotDiscover
class ExampleParallelTestExecutionSuiteTimeoutFixtureFunSuite extends fixture.FunSuite with ParallelTestExecution with SuiteTimeoutSetting with StringFixture {
  test("Fixture Test 1") { fixture => }
  test("Fixture Test 2") { fixture => }
  test("Fixture Test 3") { fixture => }
}

class ExampleParallelTestExecutionSuiteTimeoutFunSpecPair extends SuiteTimeoutSuites {
  def suite1 = new ExampleParallelTestExecutionSuiteTimeoutFunSpec
  def suite2 = new ExampleParallelTestExecutionSuiteTimeoutFixtureFunSpec
  val holdingSuiteId = suite1.suiteId
  val holdingTestName = "Scope 2 Test 4"
  val holdingScopeClosedName = Some("Scope 2")
  val holdUntilEventCount = 24
  def assertSuiteTimeoutTest(events: List[Event]) {
    assert(events.size === 28)
    
    checkSuiteStarting(events(0), suite1.suiteId)
    checkScopeOpened(events(1), "Scope 1")
    checkTestStarting(events(2), "Scope 1 Test 1")
    checkTestSucceeded(events(3), "Scope 1 Test 1")
    checkTestStarting(events(4), "Scope 1 Test 2")
    checkTestSucceeded(events(5), "Scope 1 Test 2")
    checkScopeClosed(events(6), "Scope 1")
    checkScopeOpened(events(7), "Scope 2")
    checkTestStarting(events(8), "Scope 2 Test 3")
    checkTestSucceeded(events(9), "Scope 2 Test 3")
    
    checkSuiteStarting(events(10), suite2.suiteId)
    checkScopeOpened(events(11), "Fixture Scope 1")
    checkTestStarting(events(12), "Fixture Scope 1 Fixture Test 1")
    checkTestSucceeded(events(13), "Fixture Scope 1 Fixture Test 1")
    checkTestStarting(events(14), "Fixture Scope 1 Fixture Test 2")
    checkTestSucceeded(events(15), "Fixture Scope 1 Fixture Test 2")
    checkScopeClosed(events(16), "Fixture Scope 1")
    checkScopeOpened(events(17), "Fixture Scope 2")
    checkTestStarting(events(18), "Fixture Scope 2 Fixture Test 3")
    checkTestSucceeded(events(19), "Fixture Scope 2 Fixture Test 3")
    checkTestStarting(events(20), "Fixture Scope 2 Fixture Test 4")
    checkTestSucceeded(events(21), "Fixture Scope 2 Fixture Test 4")
    checkScopeClosed(events(22), "Fixture Scope 2")
    checkSuiteCompleted(events(23), suite2.suiteId)
    
    checkTestStarting(events(24), "Scope 2 Test 4")
    checkTestSucceeded(events(25), "Scope 2 Test 4")
    checkScopeClosed(events(26), "Scope 2")
    checkSuiteCompleted(events(27), suite1.suiteId)
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionSuiteTimeoutFunSpec extends FunSpec with ParallelTestExecution with SuiteTimeoutSetting {
  describe("Scope 1") {
    it("Test 1") {}
    it("Test 2") {}
  }
  describe("Scope 2") {
    it("Test 3") {}
    it("Test 4") {}
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionSuiteTimeoutFixtureFunSpec extends fixture.FunSpec with ParallelTestExecution with SuiteTimeoutSetting with StringFixture {
  describe("Fixture Scope 1") {
    it("Fixture Test 1") { fixture => }
    it("Fixture Test 2") { fixture => }
  }
  describe("Fixture Scope 2") {
    it("Fixture Test 3") { fixture => }
    it("Fixture Test 4") { fixture => }
  }
}

class ExampleParallelTestExecutionSuiteTimeoutFeatureSpecPair extends SuiteTimeoutSuites {
  def suite1 = new ExampleParallelTestExecutionSuiteTimeoutFeatureSpec
  def suite2 = new ExampleParallelTestExecutionSuiteTimeoutFixtureFeatureSpec
  val holdingSuiteId = suite1.suiteId
  val holdingTestName = "Feature: Scope 2 Scenario: Test 4"
  val holdingScopeClosedName = Some("Feature: Scope 2")
  val holdUntilEventCount = 24
  def assertSuiteTimeoutTest(events: List[Event]) {
    assert(events.size === 28)
    
    checkSuiteStarting(events(0), suite1.suiteId)
    checkScopeOpened(events(1), "Feature: Scope 1")
    checkTestStarting(events(2), "Feature: Scope 1 Scenario: Test 1")
    checkTestSucceeded(events(3), "Feature: Scope 1 Scenario: Test 1")
    checkTestStarting(events(4), "Feature: Scope 1 Scenario: Test 2")
    checkTestSucceeded(events(5), "Feature: Scope 1 Scenario: Test 2")
    checkScopeClosed(events(6), "Feature: Scope 1")
    checkScopeOpened(events(7), "Feature: Scope 2")
    checkTestStarting(events(8), "Feature: Scope 2 Scenario: Test 3")
    checkTestSucceeded(events(9), "Feature: Scope 2 Scenario: Test 3")
    
    checkSuiteStarting(events(10), suite2.suiteId)
    checkScopeOpened(events(11), "Feature: Fixture Scope 1")
    checkTestStarting(events(12), "Feature: Fixture Scope 1 Scenario: Fixture Test 1")
    checkTestSucceeded(events(13), "Feature: Fixture Scope 1 Scenario: Fixture Test 1")
    checkTestStarting(events(14), "Feature: Fixture Scope 1 Scenario: Fixture Test 2")
    checkTestSucceeded(events(15), "Feature: Fixture Scope 1 Scenario: Fixture Test 2")
    checkScopeClosed(events(16), "Feature: Fixture Scope 1")
    checkScopeOpened(events(17), "Feature: Fixture Scope 2")
    checkTestStarting(events(18), "Feature: Fixture Scope 2 Scenario: Fixture Test 3")
    checkTestSucceeded(events(19), "Feature: Fixture Scope 2 Scenario: Fixture Test 3")
    checkTestStarting(events(20), "Feature: Fixture Scope 2 Scenario: Fixture Test 4")
    checkTestSucceeded(events(21), "Feature: Fixture Scope 2 Scenario: Fixture Test 4")
    checkScopeClosed(events(22), "Feature: Fixture Scope 2")
    checkSuiteCompleted(events(23), suite2.suiteId)
    
    checkTestStarting(events(24), "Feature: Scope 2 Scenario: Test 4")
    checkTestSucceeded(events(25), "Feature: Scope 2 Scenario: Test 4")
    checkScopeClosed(events(26), "Feature: Scope 2")
    checkSuiteCompleted(events(27), suite1.suiteId)
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionSuiteTimeoutFeatureSpec extends FeatureSpec with ParallelTestExecution with SuiteTimeoutSetting {
  feature("Scope 1") {
    scenario("Test 1") {}
    scenario("Test 2") {}
  }
  feature("Scope 2") {
    scenario("Test 3") {}
    scenario("Test 4") {}
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionSuiteTimeoutFixtureFeatureSpec extends fixture.FeatureSpec with ParallelTestExecution with SuiteTimeoutSetting with StringFixture {
  feature("Fixture Scope 1") {
    scenario("Fixture Test 1") { fixture => }
    scenario("Fixture Test 2") { fixture =>}
  }
  feature("Fixture Scope 2") {
    scenario("Fixture Test 3") { fixture => }
    scenario("Fixture Test 4") { fixture => }
  }
}

class ExampleParallelTestExecutionSuiteTimeoutFlatSpecPair extends SuiteTimeoutSuites {
  def suite1 = new ExampleParallelTestExecutionSuiteTimeoutFlatSpec
  def suite2 = new ExampleParallelTestExecutionSuiteTimeoutFixtureFlatSpec
  val holdingSuiteId = suite1.suiteId
  val holdingTestName = "Scope 2 should Test 4"
  val holdingScopeClosedName = Some("Scope 2")
  val holdUntilEventCount = 24
  def assertSuiteTimeoutTest(events: List[Event]) {
    assert(events.size === 28)
    
    checkSuiteStarting(events(0), suite1.suiteId)
    checkScopeOpened(events(1), "Scope 1")
    checkTestStarting(events(2), "Scope 1 should Test 1")
    checkTestSucceeded(events(3), "Scope 1 should Test 1")
    checkTestStarting(events(4), "Scope 1 should Test 2")
    checkTestSucceeded(events(5), "Scope 1 should Test 2")
    checkScopeClosed(events(6), "Scope 1")
    checkScopeOpened(events(7), "Scope 2")
    checkTestStarting(events(8), "Scope 2 should Test 3")
    checkTestSucceeded(events(9), "Scope 2 should Test 3")    
    
    checkSuiteStarting(events(10), suite2.suiteId)
    checkScopeOpened(events(11), "Fixture Scope 1")
    checkTestStarting(events(12), "Fixture Scope 1 should Fixture Test 1")
    checkTestSucceeded(events(13), "Fixture Scope 1 should Fixture Test 1")
    checkTestStarting(events(14), "Fixture Scope 1 should Fixture Test 2")
    checkTestSucceeded(events(15), "Fixture Scope 1 should Fixture Test 2")
    checkScopeClosed(events(16), "Fixture Scope 1")
    checkScopeOpened(events(17), "Fixture Scope 2")
    checkTestStarting(events(18), "Fixture Scope 2 should Fixture Test 3")
    checkTestSucceeded(events(19), "Fixture Scope 2 should Fixture Test 3")
    checkTestStarting(events(20), "Fixture Scope 2 should Fixture Test 4")
    checkTestSucceeded(events(21), "Fixture Scope 2 should Fixture Test 4")
    checkScopeClosed(events(22), "Fixture Scope 2")
    checkSuiteCompleted(events(23), suite2.suiteId)
    
    checkTestStarting(events(24), "Scope 2 should Test 4")
    checkTestSucceeded(events(25), "Scope 2 should Test 4")
    checkScopeClosed(events(26), "Scope 2")
    checkSuiteCompleted(events(27), suite1.suiteId)
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionSuiteTimeoutFlatSpec extends FlatSpec with ParallelTestExecution with SuiteTimeoutSetting {
  behavior of "Scope 1"
  it should "Test 1" in {}
  it should "Test 2" in {}
  
  behavior of "Scope 2"
  it should "Test 3" in {}
  it should "Test 4" in {}
}

@DoNotDiscover
class ExampleParallelTestExecutionSuiteTimeoutFixtureFlatSpec extends fixture.FlatSpec with ParallelTestExecution with SuiteTimeoutSetting with StringFixture {
  behavior of "Fixture Scope 1"
  it should "Fixture Test 1" in { fixture => }
  it should "Fixture Test 2" in { fixture => }
  
  behavior of "Fixture Scope 2"
  it should "Fixture Test 3" in { fixture => }
  it should "Fixture Test 4" in { fixture => }
}

class ExampleParallelTestExecutionSuiteTimeoutFreeSpecPair extends SuiteTimeoutSuites {
  def suite1 = new ExampleParallelTestExecutionSuiteTimeoutFreeSpec
  def suite2 = new ExampleParallelTestExecutionSuiteTimeoutFixtureFreeSpec
  val holdingSuiteId = suite1.suiteId
  val holdingTestName = "Scope 2 Test 4"
  val holdingScopeClosedName = Some("Scope 2")
  val holdUntilEventCount = 24
  def assertSuiteTimeoutTest(events: List[Event]) {
    assert(events.size === 28)
    
    checkSuiteStarting(events(0), suite1.suiteId)
    checkScopeOpened(events(1), "Scope 1")
    checkTestStarting(events(2), "Scope 1 Test 1")
    checkTestSucceeded(events(3), "Scope 1 Test 1")
    checkTestStarting(events(4), "Scope 1 Test 2")
    checkTestSucceeded(events(5), "Scope 1 Test 2")
    checkScopeClosed(events(6), "Scope 1")
    checkScopeOpened(events(7), "Scope 2")
    checkTestStarting(events(8), "Scope 2 Test 3")
    checkTestSucceeded(events(9), "Scope 2 Test 3")
    
    checkSuiteStarting(events(10), suite2.suiteId)
    checkScopeOpened(events(11), "Fixture Scope 1")
    checkTestStarting(events(12), "Fixture Scope 1 Fixture Test 1")
    checkTestSucceeded(events(13), "Fixture Scope 1 Fixture Test 1")
    checkTestStarting(events(14), "Fixture Scope 1 Fixture Test 2")
    checkTestSucceeded(events(15), "Fixture Scope 1 Fixture Test 2")
    checkScopeClosed(events(16), "Fixture Scope 1")
    checkScopeOpened(events(17), "Fixture Scope 2")
    checkTestStarting(events(18), "Fixture Scope 2 Fixture Test 3")
    checkTestSucceeded(events(19), "Fixture Scope 2 Fixture Test 3")
    checkTestStarting(events(20), "Fixture Scope 2 Fixture Test 4")
    checkTestSucceeded(events(21), "Fixture Scope 2 Fixture Test 4")
    checkScopeClosed(events(22), "Fixture Scope 2")
    checkSuiteCompleted(events(23), suite2.suiteId)
    
    checkTestStarting(events(24), "Scope 2 Test 4")
    checkTestSucceeded(events(25), "Scope 2 Test 4")
    checkScopeClosed(events(26), "Scope 2")
    checkSuiteCompleted(events(27), suite1.suiteId)
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionSuiteTimeoutFreeSpec extends FreeSpec with ParallelTestExecution with SuiteTimeoutSetting {
  "Scope 1" - {
    "Test 1" in {}
    "Test 2" in {}
  }
  
  "Scope 2" - {
    "Test 3" in {}
    "Test 4" in {}
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionSuiteTimeoutFixtureFreeSpec extends fixture.FreeSpec with ParallelTestExecution with SuiteTimeoutSetting with StringFixture {
  "Fixture Scope 1" - {
    "Fixture Test 1" in { fixture => }
    "Fixture Test 2" in { fixture => }
  }
  
  "Fixture Scope 2" - {
    "Fixture Test 3" in { fixture => }
    "Fixture Test 4" in { fixture => }
  }
}

class ExampleParallelTestExecutionSuiteTimeoutPropSpecPair extends SuiteTimeoutSuites {
  def suite1 = new ExampleParallelTestExecutionSuiteTimeoutPropSpec
  def suite2 = new ExampleParallelTestExecutionSuiteTimeoutFixturePropSpec
  val holdingSuiteId = suite1.suiteId
  val holdingTestName = "Test 3"
  val holdingScopeClosedName = None
  val holdUntilEventCount = 13
  def assertSuiteTimeoutTest(events: List[Event]) {
    assert(events.size === 16)
    
    checkSuiteStarting(events(0), suite1.suiteId)
    checkTestStarting(events(1), "Test 1")
    checkTestSucceeded(events(2), "Test 1")
    checkTestStarting(events(3), "Test 2")
    checkTestSucceeded(events(4), "Test 2")
    
    checkSuiteStarting(events(5), suite2.suiteId)
    checkTestStarting(events(6), "Fixture Test 1")
    checkTestSucceeded(events(7), "Fixture Test 1")
    checkTestStarting(events(8), "Fixture Test 2")
    checkTestSucceeded(events(9), "Fixture Test 2")
    checkTestStarting(events(10), "Fixture Test 3")
    checkTestSucceeded(events(11), "Fixture Test 3")
    checkSuiteCompleted(events(12), suite2.suiteId)
    
    checkTestStarting(events(13), "Test 3")
    checkTestSucceeded(events(14), "Test 3")
    checkSuiteCompleted(events(15), suite1.suiteId)
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionSuiteTimeoutPropSpec extends PropSpec with ParallelTestExecution with SuiteTimeoutSetting {
  property("Test 1") {}
  property("Test 2") {}
  property("Test 3") {}
}

@DoNotDiscover
class ExampleParallelTestExecutionSuiteTimeoutFixturePropSpec extends fixture.PropSpec with ParallelTestExecution with SuiteTimeoutSetting with StringFixture {
  property("Fixture Test 1") { fixture => }
  property("Fixture Test 2") { fixture => }
  property("Fixture Test 3") { fixture => }
}

class ExampleParallelTestExecutionSuiteTimeoutWordSpecPair extends SuiteTimeoutSuites {
  def suite1 = new ExampleParallelTestExecutionSuiteTimeoutWordSpec
  def suite2 = new ExampleParallelTestExecutionSuiteTimeoutFixtureWordSpec
  val holdingSuiteId = suite1.suiteId
  val holdingTestName = "Scope 2 should Test 4"
  val holdingScopeClosedName = Some("Scope 2")
  val holdUntilEventCount = 24
  def assertSuiteTimeoutTest(events: List[Event]) {
    assert(events.size === 28)
    
    checkSuiteStarting(events(0), suite1.suiteId)
    checkScopeOpened(events(1), "Scope 1")
    checkTestStarting(events(2), "Scope 1 should Test 1")
    checkTestSucceeded(events(3), "Scope 1 should Test 1")
    checkTestStarting(events(4), "Scope 1 should Test 2")
    checkTestSucceeded(events(5), "Scope 1 should Test 2")
    checkScopeClosed(events(6), "Scope 1")
    checkScopeOpened(events(7), "Scope 2")
    checkTestStarting(events(8), "Scope 2 should Test 3")
    checkTestSucceeded(events(9), "Scope 2 should Test 3")
    
    checkSuiteStarting(events(10), suite2.suiteId)
    checkScopeOpened(events(11), "Fixture Scope 1")
    checkTestStarting(events(12), "Fixture Scope 1 should Fixture Test 1")
    checkTestSucceeded(events(13), "Fixture Scope 1 should Fixture Test 1")
    checkTestStarting(events(14), "Fixture Scope 1 should Fixture Test 2")
    checkTestSucceeded(events(15), "Fixture Scope 1 should Fixture Test 2")
    checkScopeClosed(events(16), "Fixture Scope 1")
    checkScopeOpened(events(17), "Fixture Scope 2")
    checkTestStarting(events(18), "Fixture Scope 2 should Fixture Test 3")
    checkTestSucceeded(events(19), "Fixture Scope 2 should Fixture Test 3")
    checkTestStarting(events(20), "Fixture Scope 2 should Fixture Test 4")
    checkTestSucceeded(events(21), "Fixture Scope 2 should Fixture Test 4")
    checkScopeClosed(events(22), "Fixture Scope 2")
    checkSuiteCompleted(events(23), suite2.suiteId)
    
    checkTestStarting(events(24), "Scope 2 should Test 4")
    checkTestSucceeded(events(25), "Scope 2 should Test 4")
    checkScopeClosed(events(26), "Scope 2")
    checkSuiteCompleted(events(27), suite1.suiteId)
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionSuiteTimeoutWordSpec extends WordSpec with ParallelTestExecution with SuiteTimeoutSetting {
  "Scope 1" should {
    "Test 1" in {}
    "Test 2" in {}
  }
  
  "Scope 2" should {
    "Test 3" in {}
    "Test 4" in {}
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionSuiteTimeoutFixtureWordSpec extends fixture.WordSpec with ParallelTestExecution with SuiteTimeoutSetting with StringFixture {
  "Fixture Scope 1" should {
    "Fixture Test 1" in { fixture => }
    "Fixture Test 2" in { fixture => }
  }
  
  "Fixture Scope 2" should {
    "Fixture Test 3" in { fixture => }
    "Fixture Test 4" in { fixture => }
  }
}
