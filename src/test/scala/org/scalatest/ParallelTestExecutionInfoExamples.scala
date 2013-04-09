package org.scalatest

import org.scalatest.events.Event
import org.scalatest.prop.Tables

trait InfoExpectedResults extends EventHelpers {
  def assertBeforeAfterInfo(events: List[Event])
}

trait ParallelTestExecutionInfoExamples extends Tables {

  def infoSuite = new ExampleParallelTestExecutionInfoSuite()
  def infoFixtureSuite = new ExampleParallelTestExecutionInfoFixtureSuite()
  def infoSpec = new ExampleParallelTestExecutionInfoSpec()
  def infoFixtureSpec = new ExampleParallelTestExecutionInfoFixtureSpec()
  def infoFunSuite = new ExampleParallelTestExecutionInfoFunSuite()
  def infoFixtureFunSuite = new ExampleParallelTestExecutionInfoFixtureFunSuite()
  def infoFunSpec = new ExampleParallelTestExecutionInfoFunSpec()
  def infoFixtureFunSpec = new ExampleParallelTestExecutionInfoFixtureFunSpec()
  def infoFeatureSpec = new ExampleParallelTestExecutionInfoFeatureSpec()
  def infoFixtureFeatureSpec = new ExampleParallelTestExecutionInfoFixtureFeatureSpec()
  def infoFlatSpec = new ExampleParallelTestExecutionInfoFlatSpec()
  def infoFixtureFlatSpec = new ExampleParallelTestExecutionInfoFixtureFlatSpec()
  def infoFreeSpec = new ExampleParallelTestExecutionInfoFreeSpec()
  def infoFixtureFreeSpec = new ExampleParallelTestExecutionInfoFixtureFreeSpec()
  def infoPropSpec = new ExampleParallelTestExecutionInfoPropSpec()
  def infoFixturePropSpec = new ExampleParallelTestExecutionInfoFixturePropSpec()
  def infoWordSpec = new ExampleParallelTestExecutionInfoWordSpec()
  def infoFixtureWordSpec = new ExampleParallelTestExecutionInfoFixtureWordSpec()
  
  def infoExamples =
    Table(
      "suite1",
      infoSuite, 
      infoFixtureSuite, 
      infoSpec, 
      infoFixtureSpec, 
      infoFunSuite, 
      infoFixtureFunSuite, 
      infoFunSpec, 
      infoFixtureFunSpec, 
      infoFeatureSpec, 
      infoFixtureFeatureSpec, 
      infoFlatSpec, 
      infoFixtureFlatSpec, 
      infoFreeSpec, 
      infoFixtureFreeSpec,
      infoPropSpec, 
      infoFixturePropSpec, 
      infoWordSpec, 
      infoFixtureWordSpec
    )
}

@DoNotDiscover
class ExampleParallelTestExecutionInfoSuite extends Suite with InfoExpectedResults with BeforeAndAfter with ParallelTestExecution {
  before {}  // how to fire info here?
  def testMethod1() {}
  def testMethod2() {}
  def testMethod3() {}
  after {} // how to fire info here?
  
  def assertBeforeAfterInfo(events: List[Event]) {
    assert(events.size === 6)
    checkTestStarting(events(0), "testMethod1")
    checkTestSucceeded(events(1), "testMethod1")
    checkTestStarting(events(2), "testMethod2")
    checkTestSucceeded(events(3), "testMethod2")
    checkTestStarting(events(4), "testMethod3")
    checkTestSucceeded(events(5), "testMethod3")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionInfoFixtureSuite extends fixture.Suite with InfoExpectedResults with BeforeAndAfter with ParallelTestExecution with StringFixture {
  before {}  // how to fire info here?
  def testMethod1() {}
  def testMethod2() {}
  def testMethod3() {}
  after {}  // how to fire info here?
  
  def assertBeforeAfterInfo(events: List[Event]) {
    assert(events.size === 6)
    checkTestStarting(events(0), "testMethod1")
    checkTestSucceeded(events(1), "testMethod1")
    checkTestStarting(events(2), "testMethod2")
    checkTestSucceeded(events(3), "testMethod2")
    checkTestStarting(events(4), "testMethod3")
    checkTestSucceeded(events(5), "testMethod3")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionInfoSpec extends Spec with InfoExpectedResults with BeforeAndAfter with ParallelTestExecution {
  before {}  // how to fire info here?
  def `test 1` {}
  def `test 2` {}
  def `test 3` {}
  after {} // how to fire info here?
  
  def assertBeforeAfterInfo(events: List[Event]) {
    assert(events.size === 6)
    checkTestStarting(events(0), "test 1")
    checkTestSucceeded(events(1), "test 1")
    checkTestStarting(events(2), "test 2")
    checkTestSucceeded(events(3), "test 2")
    checkTestStarting(events(4), "test 3")
    checkTestSucceeded(events(5), "test 3")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionInfoFixtureSpec extends fixture.Spec with InfoExpectedResults with BeforeAndAfter with ParallelTestExecution with StringFixture {
  before {}  // how to fire info here?
  def `test 1`(fixture: String) {}
  def `test 2`(fixture: String) {}
  def `test 3`(fixture: String) {}
  after {} // how to fire info here?
  
  def assertBeforeAfterInfo(events: List[Event]) {
    assert(events.size === 6)
    checkTestStarting(events(0), "test 1")
    checkTestSucceeded(events(1), "test 1")
    checkTestStarting(events(2), "test 2")
    checkTestSucceeded(events(3), "test 2")
    checkTestStarting(events(4), "test 3")
    checkTestSucceeded(events(5), "test 3")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionInfoFunSuite extends FunSuite with InfoExpectedResults with BeforeAndAfter with ParallelTestExecution {
  before { info("In Before") }
  after { info("In After") }
  test("Test 1") {}
  test("Test 2") {}
  test("Test 3") {}
  
  def assertBeforeAfterInfo(events: List[Event]) {
    assert(events.size === 12)
    checkInfoProvided(events(0), "In Before")
    checkTestStarting(events(1), "Test 1")
    checkTestSucceeded(events(2), "Test 1")
    checkInfoProvided(events(3), "In After")
    checkInfoProvided(events(4), "In Before")
    checkTestStarting(events(5), "Test 2")
    checkTestSucceeded(events(6), "Test 2")
    checkInfoProvided(events(7), "In After")
    checkInfoProvided(events(8), "In Before")
    checkTestStarting(events(9), "Test 3")
    checkTestSucceeded(events(10), "Test 3")
    checkInfoProvided(events(11), "In After")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionInfoFixtureFunSuite extends fixture.FunSuite with InfoExpectedResults with BeforeAndAfter with ParallelTestExecution with StringFixture {
  before { info("In Before") }
  after { info("In After") }
  test("Test 1") { fixture => }
  test("Test 2") { fixture => }
  test("Test 3") { fixture => }
  
  def assertBeforeAfterInfo(events: List[Event]) {
    assert(events.size === 12)
    checkInfoProvided(events(0), "In Before")
    checkTestStarting(events(1), "Test 1")
    checkTestSucceeded(events(2), "Test 1")
    checkInfoProvided(events(3), "In After")
    checkInfoProvided(events(4), "In Before")
    checkTestStarting(events(5), "Test 2")
    checkTestSucceeded(events(6), "Test 2")
    checkInfoProvided(events(7), "In After")
    checkInfoProvided(events(8), "In Before")
    checkTestStarting(events(9), "Test 3")
    checkTestSucceeded(events(10), "Test 3")
    checkInfoProvided(events(11), "In After")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionInfoFunSpec extends FunSpec with InfoExpectedResults with BeforeAndAfter with ParallelTestExecution {
  before { info("In Before") }
  after { info("In After") }
  describe("Scope 1") {
    it("Test 1") {}
    it("Test 2") {}
  }
  describe("Scope 2") {
    it("Test 3") {}
    it("Test 4") {}
  }
  
  def assertBeforeAfterInfo(events: List[Event]) {
    assert(events.size === 20)
    checkScopeOpened(events(0), "Scope 1")
    checkInfoProvided(events(1), "In Before")
    checkTestStarting(events(2), "Scope 1 Test 1")
    checkTestSucceeded(events(3), "Scope 1 Test 1")
    checkInfoProvided(events(4), "In After")
    checkInfoProvided(events(5), "In Before")
    checkTestStarting(events(6), "Scope 1 Test 2")
    checkTestSucceeded(events(7), "Scope 1 Test 2")
    checkInfoProvided(events(8), "In After")
    checkScopeClosed(events(9), "Scope 1")
    checkScopeOpened(events(10), "Scope 2")
    checkInfoProvided(events(11), "In Before")
    checkTestStarting(events(12), "Scope 2 Test 3")
    checkTestSucceeded(events(13), "Scope 2 Test 3")
    checkInfoProvided(events(14), "In After")
    checkInfoProvided(events(15), "In Before")
    checkTestStarting(events(16), "Scope 2 Test 4")
    checkTestSucceeded(events(17), "Scope 2 Test 4")
    checkInfoProvided(events(18), "In After")
    checkScopeClosed(events(19), "Scope 2")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionInfoFixtureFunSpec extends fixture.FunSpec with InfoExpectedResults with BeforeAndAfter with ParallelTestExecution with StringFixture {
  before { info("In Before") }
  after { info("In After") }
  describe("Scope 1") {
    it("Test 1") { fixture => }
    it("Test 2") { fixture =>}
  }
  describe("Scope 2") {
    it("Test 3") { fixture => }
    it("Test 4") { fixture =>}
  }
  
  def assertBeforeAfterInfo(events: List[Event]) {
    assert(events.size === 20)
    checkScopeOpened(events(0), "Scope 1")
    checkInfoProvided(events(1), "In Before")
    checkTestStarting(events(2), "Scope 1 Test 1")
    checkTestSucceeded(events(3), "Scope 1 Test 1")
    checkInfoProvided(events(4), "In After")
    checkInfoProvided(events(5), "In Before")
    checkTestStarting(events(6), "Scope 1 Test 2")
    checkTestSucceeded(events(7), "Scope 1 Test 2")
    checkInfoProvided(events(8), "In After")
    checkScopeClosed(events(9), "Scope 1")
    checkScopeOpened(events(10), "Scope 2")
    checkInfoProvided(events(11), "In Before")
    checkTestStarting(events(12), "Scope 2 Test 3")
    checkTestSucceeded(events(13), "Scope 2 Test 3")
    checkInfoProvided(events(14), "In After")
    checkInfoProvided(events(15), "In Before")
    checkTestStarting(events(16), "Scope 2 Test 4")
    checkTestSucceeded(events(17), "Scope 2 Test 4")
    checkInfoProvided(events(18), "In After")
    checkScopeClosed(events(19), "Scope 2")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionInfoFeatureSpec extends FeatureSpec with InfoExpectedResults with BeforeAndAfter with ParallelTestExecution {
  before { info("In Before") }
  after { info("In After") }
  feature("Scope 1") {
    scenario("Test 1") {}
    scenario("Test 2") {}
  }
  feature("Scope 2") {
    scenario("Test 3") {}
    scenario("Test 4") {}
  }
  
  def assertBeforeAfterInfo(events: List[Event]) {
    assert(events.size === 20)
    checkScopeOpened(events(0), "Feature: Scope 1")
    checkInfoProvided(events(1), "In Before")
    checkTestStarting(events(2), "Feature: Scope 1 Scenario: Test 1")
    checkTestSucceeded(events(3), "Feature: Scope 1 Scenario: Test 1")
    checkInfoProvided(events(4), "In After")
    checkInfoProvided(events(5), "In Before")
    checkTestStarting(events(6), "Feature: Scope 1 Scenario: Test 2")
    checkTestSucceeded(events(7), "Feature: Scope 1 Scenario: Test 2")
    checkInfoProvided(events(8), "In After")
    checkScopeClosed(events(9), "Feature: Scope 1")
    checkScopeOpened(events(10), "Feature: Scope 2")
    checkInfoProvided(events(11), "In Before")
    checkTestStarting(events(12), "Feature: Scope 2 Scenario: Test 3")
    checkTestSucceeded(events(13), "Feature: Scope 2 Scenario: Test 3")
    checkInfoProvided(events(14), "In After")
    checkInfoProvided(events(15), "In Before")
    checkTestStarting(events(16), "Feature: Scope 2 Scenario: Test 4")
    checkTestSucceeded(events(17), "Feature: Scope 2 Scenario: Test 4")
    checkInfoProvided(events(18), "In After")
    checkScopeClosed(events(19), "Feature: Scope 2")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionInfoFixtureFeatureSpec extends fixture.FeatureSpec with InfoExpectedResults with BeforeAndAfter with ParallelTestExecution with StringFixture {
  before { info("In Before") }
  after { info("In After") }
  feature("Scope 1") {
    scenario("Test 1") { fixture => }
    scenario("Test 2") { fixture =>}
  }
  feature("Scope 2") {
    scenario("Test 3") { fixture => }
    scenario("Test 4") { fixture =>}
  }
  
  def assertBeforeAfterInfo(events: List[Event]) {
    assert(events.size === 20)
    checkScopeOpened(events(0), "Feature: Scope 1")
    checkInfoProvided(events(1), "In Before")
    checkTestStarting(events(2), "Feature: Scope 1 Scenario: Test 1")
    checkTestSucceeded(events(3), "Feature: Scope 1 Scenario: Test 1")
    checkInfoProvided(events(4), "In After")
    checkInfoProvided(events(5), "In Before")
    checkTestStarting(events(6), "Feature: Scope 1 Scenario: Test 2")
    checkTestSucceeded(events(7), "Feature: Scope 1 Scenario: Test 2")
    checkInfoProvided(events(8), "In After")
    checkScopeClosed(events(9), "Feature: Scope 1")
    checkScopeOpened(events(10), "Feature: Scope 2")
    checkInfoProvided(events(11), "In Before")
    checkTestStarting(events(12), "Feature: Scope 2 Scenario: Test 3")
    checkTestSucceeded(events(13), "Feature: Scope 2 Scenario: Test 3")
    checkInfoProvided(events(14), "In After")
    checkInfoProvided(events(15), "In Before")
    checkTestStarting(events(16), "Feature: Scope 2 Scenario: Test 4")
    checkTestSucceeded(events(17), "Feature: Scope 2 Scenario: Test 4")
    checkInfoProvided(events(18), "In After")
    checkScopeClosed(events(19), "Feature: Scope 2")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionInfoFlatSpec extends FlatSpec with InfoExpectedResults with BeforeAndAfter with ParallelTestExecution {
  before { info("In Before") }
  after { info("In After") }
  behavior of "Scope 1"
  it should "Test 1" in {}
  it should "Test 2" in {}
  
  behavior of "Scope 2"
  it should "Test 3" in {}
  it should "Test 4" in {}
  
  def assertBeforeAfterInfo(events: List[Event]) {
    assert(events.size === 20)
    checkScopeOpened(events(0), "Scope 1")
    checkInfoProvided(events(1), "In Before")
    checkTestStarting(events(2), "Scope 1 should Test 1")
    checkTestSucceeded(events(3), "Scope 1 should Test 1")
    checkInfoProvided(events(4), "In After")
    checkInfoProvided(events(5), "In Before")
    checkTestStarting(events(6), "Scope 1 should Test 2")
    checkTestSucceeded(events(7), "Scope 1 should Test 2")
    checkInfoProvided(events(8), "In After")
    checkScopeClosed(events(9), "Scope 1")
    checkScopeOpened(events(10), "Scope 2")
    checkInfoProvided(events(11), "In Before")
    checkTestStarting(events(12), "Scope 2 should Test 3")
    checkTestSucceeded(events(13), "Scope 2 should Test 3")
    checkInfoProvided(events(14), "In After")
    checkInfoProvided(events(15), "In Before")
    checkTestStarting(events(16), "Scope 2 should Test 4")
    checkTestSucceeded(events(17), "Scope 2 should Test 4")
    checkInfoProvided(events(18), "In After")
    checkScopeClosed(events(19), "Scope 2")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionInfoFixtureFlatSpec extends fixture.FlatSpec with InfoExpectedResults with BeforeAndAfter with ParallelTestExecution with StringFixture {
  before { info("In Before") }
  after { info("In After") }
  behavior of "Scope 1"
  it should "Test 1" in { fixture => }
  it should "Test 2" in { fixture => }
  
  behavior of "Scope 2"
  it should "Test 3" in { fixture => }
  it should "Test 4" in { fixture => }
  
  def assertBeforeAfterInfo(events: List[Event]) {
    assert(events.size === 20)
    checkScopeOpened(events(0), "Scope 1")
    checkInfoProvided(events(1), "In Before")
    checkTestStarting(events(2), "Scope 1 should Test 1")
    checkTestSucceeded(events(3), "Scope 1 should Test 1")
    checkInfoProvided(events(4), "In After")
    checkInfoProvided(events(5), "In Before")
    checkTestStarting(events(6), "Scope 1 should Test 2")
    checkTestSucceeded(events(7), "Scope 1 should Test 2")
    checkInfoProvided(events(8), "In After")
    checkScopeClosed(events(9), "Scope 1")
    checkScopeOpened(events(10), "Scope 2")
    checkInfoProvided(events(11), "In Before")
    checkTestStarting(events(12), "Scope 2 should Test 3")
    checkTestSucceeded(events(13), "Scope 2 should Test 3")
    checkInfoProvided(events(14), "In After")
    checkInfoProvided(events(15), "In Before")
    checkTestStarting(events(16), "Scope 2 should Test 4")
    checkTestSucceeded(events(17), "Scope 2 should Test 4")
    checkInfoProvided(events(18), "In After")
    checkScopeClosed(events(19), "Scope 2")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionInfoFreeSpec extends FreeSpec with InfoExpectedResults with BeforeAndAfter with ParallelTestExecution {
  before { info("In Before") }
  after { info("In After") }
  "Scope 1" - {
    "Test 1" in {}
    "Test 2" in {}
  }
  
  "Scope 2" - {
    "Test 3" in {}
    "Test 4" in {}
  }
  
  def assertBeforeAfterInfo(events: List[Event]) {
    assert(events.size === 20)
    checkScopeOpened(events(0), "Scope 1")
    checkInfoProvided(events(1), "In Before")
    checkTestStarting(events(2), "Scope 1 Test 1")
    checkTestSucceeded(events(3), "Scope 1 Test 1")
    checkInfoProvided(events(4), "In After")
    checkInfoProvided(events(5), "In Before")
    checkTestStarting(events(6), "Scope 1 Test 2")
    checkTestSucceeded(events(7), "Scope 1 Test 2")
    checkInfoProvided(events(8), "In After")
    checkScopeClosed(events(9), "Scope 1")
    checkScopeOpened(events(10), "Scope 2")
    checkInfoProvided(events(11), "In Before")
    checkTestStarting(events(12), "Scope 2 Test 3")
    checkTestSucceeded(events(13), "Scope 2 Test 3")
    checkInfoProvided(events(14), "In After")
    checkInfoProvided(events(15), "In Before")
    checkTestStarting(events(16), "Scope 2 Test 4")
    checkTestSucceeded(events(17), "Scope 2 Test 4")
    checkInfoProvided(events(18), "In After")
    checkScopeClosed(events(19), "Scope 2")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionInfoFixtureFreeSpec extends fixture.FreeSpec with InfoExpectedResults with BeforeAndAfter with ParallelTestExecution with StringFixture {
  before { info("In Before") }
  after { info("In After") }
  "Scope 1" - {
    "Test 1" in { fixture => }
    "Test 2" in { fixture => }
  }
  
  "Scope 2" - {
    "Test 3" in { fixture => }
    "Test 4" in { fixture => }
  }
  
  def assertBeforeAfterInfo(events: List[Event]) {
    assert(events.size === 20)
    checkScopeOpened(events(0), "Scope 1")
    checkInfoProvided(events(1), "In Before")
    checkTestStarting(events(2), "Scope 1 Test 1")
    checkTestSucceeded(events(3), "Scope 1 Test 1")
    checkInfoProvided(events(4), "In After")
    checkInfoProvided(events(5), "In Before")
    checkTestStarting(events(6), "Scope 1 Test 2")
    checkTestSucceeded(events(7), "Scope 1 Test 2")
    checkInfoProvided(events(8), "In After")
    checkScopeClosed(events(9), "Scope 1")
    checkScopeOpened(events(10), "Scope 2")
    checkInfoProvided(events(11), "In Before")
    checkTestStarting(events(12), "Scope 2 Test 3")
    checkTestSucceeded(events(13), "Scope 2 Test 3")
    checkInfoProvided(events(14), "In After")
    checkInfoProvided(events(15), "In Before")
    checkTestStarting(events(16), "Scope 2 Test 4")
    checkTestSucceeded(events(17), "Scope 2 Test 4")
    checkInfoProvided(events(18), "In After")
    checkScopeClosed(events(19), "Scope 2")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionInfoPropSpec extends PropSpec with InfoExpectedResults with BeforeAndAfter with ParallelTestExecution {
  before { info("In Before") }
  after { info("In After") }
  property("Test 1") {}
  property("Test 2") {}
  property("Test 3") {}
  
  def assertBeforeAfterInfo(events: List[Event]) {
    assert(events.size === 12)
    checkInfoProvided(events(0), "In Before")
    checkTestStarting(events(1), "Test 1")
    checkTestSucceeded(events(2), "Test 1")
    checkInfoProvided(events(3), "In After")
    checkInfoProvided(events(4), "In Before")
    checkTestStarting(events(5), "Test 2")
    checkTestSucceeded(events(6), "Test 2")
    checkInfoProvided(events(7), "In After")
    checkInfoProvided(events(8), "In Before")
    checkTestStarting(events(9), "Test 3")
    checkTestSucceeded(events(10), "Test 3")
    checkInfoProvided(events(11), "In After")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionInfoFixturePropSpec extends fixture.PropSpec with InfoExpectedResults with BeforeAndAfter with ParallelTestExecution with StringFixture {
  before { info("In Before") }
  after { info("In After") }
  property("Test 1") { fixture => }
  property("Test 2") { fixture => }
  property("Test 3") { fixture => }
  
  def assertBeforeAfterInfo(events: List[Event]) {
    assert(events.size === 12)
    checkInfoProvided(events(0), "In Before")
    checkTestStarting(events(1), "Test 1")
    checkTestSucceeded(events(2), "Test 1")
    checkInfoProvided(events(3), "In After")
    checkInfoProvided(events(4), "In Before")
    checkTestStarting(events(5), "Test 2")
    checkTestSucceeded(events(6), "Test 2")
    checkInfoProvided(events(7), "In After")
    checkInfoProvided(events(8), "In Before")
    checkTestStarting(events(9), "Test 3")
    checkTestSucceeded(events(10), "Test 3")
    checkInfoProvided(events(11), "In After")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionInfoWordSpec extends WordSpec with InfoExpectedResults with BeforeAndAfter with ParallelTestExecution {
  before { info("In Before") }
  after { info("In After") }
  "Scope 1" should {
    "Test 1" in {}
    "Test 2" in {}
  }
  
  "Scope 2" should {
    "Test 3" in {}
    "Test 4" in {}
  }
  
  def assertBeforeAfterInfo(events: List[Event]) {
    assert(events.size === 20)
    checkScopeOpened(events(0), "Scope 1")
    checkInfoProvided(events(1), "In Before")
    checkTestStarting(events(2), "Scope 1 should Test 1")
    checkTestSucceeded(events(3), "Scope 1 should Test 1")
    checkInfoProvided(events(4), "In After")
    checkInfoProvided(events(5), "In Before")
    checkTestStarting(events(6), "Scope 1 should Test 2")
    checkTestSucceeded(events(7), "Scope 1 should Test 2")
    checkInfoProvided(events(8), "In After")
    checkScopeClosed(events(9), "Scope 1")
    checkScopeOpened(events(10), "Scope 2")
    checkInfoProvided(events(11), "In Before")
    checkTestStarting(events(12), "Scope 2 should Test 3")
    checkTestSucceeded(events(13), "Scope 2 should Test 3")
    checkInfoProvided(events(14), "In After")
    checkInfoProvided(events(15), "In Before")
    checkTestStarting(events(16), "Scope 2 should Test 4")
    checkTestSucceeded(events(17), "Scope 2 should Test 4")
    checkInfoProvided(events(18), "In After")
    checkScopeClosed(events(19), "Scope 2")
  }
}
@DoNotDiscover
class ExampleParallelTestExecutionInfoFixtureWordSpec extends fixture.WordSpec with InfoExpectedResults with BeforeAndAfter with ParallelTestExecution with StringFixture {
  before { info("In Before") }
  after { info("In After") }
  "Scope 1" should {
    "Test 1" in { fixture => }
    "Test 2" in { fixture => }
  }
  
  "Scope 2" should {
    "Test 3" in { fixture => }
    "Test 4" in { fixture => }
  }
  
  def assertBeforeAfterInfo(events: List[Event]) {
    assert(events.size === 20)
    checkScopeOpened(events(0), "Scope 1")
    checkInfoProvided(events(1), "In Before")
    checkTestStarting(events(2), "Scope 1 should Test 1")
    checkTestSucceeded(events(3), "Scope 1 should Test 1")
    checkInfoProvided(events(4), "In After")
    checkInfoProvided(events(5), "In Before")
    checkTestStarting(events(6), "Scope 1 should Test 2")
    checkTestSucceeded(events(7), "Scope 1 should Test 2")
    checkInfoProvided(events(8), "In After")
    checkScopeClosed(events(9), "Scope 1")
    checkScopeOpened(events(10), "Scope 2")
    checkInfoProvided(events(11), "In Before")
    checkTestStarting(events(12), "Scope 2 should Test 3")
    checkTestSucceeded(events(13), "Scope 2 should Test 3")
    checkInfoProvided(events(14), "In After")
    checkInfoProvided(events(15), "In Before")
    checkTestStarting(events(16), "Scope 2 should Test 4")
    checkTestSucceeded(events(17), "Scope 2 should Test 4")
    checkInfoProvided(events(18), "In After")
    checkScopeClosed(events(19), "Scope 2")
  }
}
