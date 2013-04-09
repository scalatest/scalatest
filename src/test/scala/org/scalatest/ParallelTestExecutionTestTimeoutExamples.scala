package org.scalatest

import org.scalatest.events.Event
import org.scalatest.time.Span
import org.scalatest.time.Millis
import org.scalatest.prop.Tables
import org.scalatest.events.TestSucceeded
import org.scalatest.tools.TestSortingReporter

trait TestTimeoutExpectedResults extends EventHelpers { s: ParallelTestExecution =>
  def assertTestTimeoutTest(events: List[Event])
  val holdTestSucceededName: String
  val holdUntilEventCount: Int
  override def sortingTimeout: Span = Span(300, Millis)
  var holdingReporter: TestHoldingReporter = null
}

trait ParallelTestExecutionTestTimeoutExamples extends Tables {

  def testTimeoutSuite = new ExampleParallelTestExecutionTestTimeoutSuite()
  def testTimeoutFixtureSuite = new ExampleParallelTestExecutionTestTimeoutFixtureSuite()
  def testTimeoutSpec = new ExampleParallelTestExecutionTestTimeoutSpec()
  def testTimeoutFixtureSpec = new ExampleParallelTestExecutionTestTimeoutFixtureSpec()
  def testTimeoutFunSuite = new ExampleParallelTestExecutionTestTimeoutFunSuite()
  def testTimeoutFixtureFunSuite = new ExampleParallelTestExecutionTestTimeoutFixtureFunSuite()
  def testTimeoutFunSpec = new ExampleParallelTestExecutionTestTimeoutFunSpec()
  def testTimeoutFixtureFunSpec = new ExampleParallelTestExecutionTestTimeoutFixtureFunSpec()
  def testTimeoutFeatureSpec = new ExampleParallelTestExecutionTestTimeoutFeatureSpec()
  def testTimeoutFixtureFeatureSpec = new ExampleParallelTestExecutionTestTimeoutFixtureFeatureSpec()
  def testTimeoutFlatSpec = new ExampleParallelTestExecutionTestTimeoutFlatSpec()
  def testTimeoutFixtureFlatSpec = new ExampleParallelTestExecutionTestTimeoutFixtureFlatSpec()
  def testTimeoutFreeSpec = new ExampleParallelTestExecutionTestTimeoutFreeSpec()
  def testTimeoutFixtureFreeSpec = new ExampleParallelTestExecutionTestTimeoutFixtureFreeSpec()
  def testTimeoutPropSpec = new ExampleParallelTestExecutionTestTimeoutPropSpec()
  def testTimeoutFixturePropSpec = new ExampleParallelTestExecutionTestTimeoutFixturePropSpec()
  def testTimeoutWordSpec = new ExampleParallelTestExecutionTestTimeoutWordSpec()
  def testTimeoutFixtureWordSpec = new ExampleParallelTestExecutionTestTimeoutFixtureWordSpec()
  
  def testTimeoutExamples =
    Table(
      "suite1",
      testTimeoutSuite, 
      testTimeoutFixtureSuite, 
      testTimeoutSpec, 
      testTimeoutFixtureSpec, 
      testTimeoutFunSuite, 
      testTimeoutFixtureFunSuite, 
      testTimeoutFunSpec, 
      testTimeoutFixtureFunSpec, 
      testTimeoutFeatureSpec, 
      testTimeoutFixtureFeatureSpec, 
      testTimeoutFlatSpec, 
      testTimeoutFixtureFlatSpec, 
      testTimeoutFreeSpec, 
      testTimeoutFixtureFreeSpec,
      testTimeoutPropSpec, 
      testTimeoutFixturePropSpec, 
      testTimeoutWordSpec, 
      testTimeoutFixtureWordSpec
    )
}

class TestHoldingReporter(dispatch: Reporter, holdingTestSucceededName: String) extends CatchReporter {
  val out = System.err
  private var holdEvent: Option[Event] = None
  override protected def doApply(event: Event) {
    event match {
      case testSucceeded: TestSucceeded if testSucceeded.testName == holdingTestSucceededName =>
        holdEvent = Some(testSucceeded)
      case _ => dispatch(event)
    }
  }
  protected def doDispose() {}
  def fireHoldEvent() {
    holdEvent match {
      case Some(event) => dispatch(event)
      case None =>
    }
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionTestTimeoutSuite extends Suite with ParallelTestExecution with TestTimeoutExpectedResults {
  def testMethod1() {}
  def testMethod2() {}
  def testMethod3() {}
  
  val holdTestSucceededName = "testMethod2"
  val holdUntilEventCount = 5
  
  override protected[scalatest] def createTestSpecificReporter(testSorter: DistributedTestSorter, testName: String): Reporter = {
    holdingReporter = new TestHoldingReporter(super.createTestSpecificReporter(testSorter, testName), holdTestSucceededName)
    holdingReporter
  }
  
  def assertTestTimeoutTest(events: List[Event]) {
    assert(events.size === 6)
    checkTestStarting(events(0), "testMethod1")
    checkTestSucceeded(events(1), "testMethod1")
    checkTestStarting(events(2), "testMethod2")
    checkTestStarting(events(3), "testMethod3")
    checkTestSucceeded(events(4), "testMethod3")
    // The missing one
    checkTestSucceeded(events(5), "testMethod2")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionTestTimeoutFixtureSuite extends fixture.Suite with ParallelTestExecution with TestTimeoutExpectedResults with StringFixture {
  def testMethod1() {}
  def testMethod2() {}
  def testMethod3() {}
  
  val holdTestSucceededName = "testMethod2"
  val holdUntilEventCount = 5
  
  override protected[scalatest] def createTestSpecificReporter(testSorter: DistributedTestSorter, testName: String): Reporter = {
    holdingReporter = new TestHoldingReporter(super.createTestSpecificReporter(testSorter, testName), holdTestSucceededName)
    holdingReporter
  }
  
  def assertTestTimeoutTest(events: List[Event]) {
    assert(events.size === 6)
    checkTestStarting(events(0), "testMethod1")
    checkTestSucceeded(events(1), "testMethod1")
    checkTestStarting(events(2), "testMethod2")
    checkTestStarting(events(3), "testMethod3")
    checkTestSucceeded(events(4), "testMethod3")
    // The missing one
    checkTestSucceeded(events(5), "testMethod2")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionTestTimeoutSpec extends Spec with ParallelTestExecution with TestTimeoutExpectedResults {
  def `test 1` {}
  def `test 2` {}
  def `test 3` {}
  
  val holdTestSucceededName = "test 2"
  val holdUntilEventCount = 5
  
  override protected[scalatest] def createTestSpecificReporter(testSorter: DistributedTestSorter, testName: String): Reporter = {
    holdingReporter = new TestHoldingReporter(super.createTestSpecificReporter(testSorter, testName), holdTestSucceededName)
    holdingReporter
  }
  
  def assertTestTimeoutTest(events: List[Event]) {
    assert(events.size === 6)
    checkTestStarting(events(0), "test 1")
    checkTestSucceeded(events(1), "test 1")
    checkTestStarting(events(2), "test 2")
    checkTestStarting(events(3), "test 3")
    checkTestSucceeded(events(4), "test 3")
    // The missing one
    checkTestSucceeded(events(5), "test 2")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionTestTimeoutFixtureSpec extends fixture.Spec with ParallelTestExecution with TestTimeoutExpectedResults with StringFixture {
  def `test 1`(fixture: String) {}
  def `test 2`(fixture: String) {}
  def `test 3`(fixture: String) {}
  
  val holdTestSucceededName = "test 2"
  val holdUntilEventCount = 5
  
  override protected[scalatest] def createTestSpecificReporter(testSorter: DistributedTestSorter, testName: String): Reporter = {
    holdingReporter = new TestHoldingReporter(super.createTestSpecificReporter(testSorter, testName), holdTestSucceededName)
    holdingReporter
  }
  
  def assertTestTimeoutTest(events: List[Event]) {
    assert(events.size === 6)
    checkTestStarting(events(0), "test 1")
    checkTestSucceeded(events(1), "test 1")
    checkTestStarting(events(2), "test 2")
    checkTestStarting(events(3), "test 3")
    checkTestSucceeded(events(4), "test 3")
    // The missing one
    checkTestSucceeded(events(5), "test 2")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionTestTimeoutFunSuite extends FunSuite with ParallelTestExecution with TestTimeoutExpectedResults {
  test("Test 1") {}
  test("Test 2") {}
  test("Test 3") {}
  
  val holdTestSucceededName = "Test 2"
  val holdUntilEventCount = 5
  
  override protected[scalatest] def createTestSpecificReporter(testSorter: DistributedTestSorter, testName: String): Reporter = {
    holdingReporter = new TestHoldingReporter(super.createTestSpecificReporter(testSorter, testName), holdTestSucceededName)
    holdingReporter
  }
  
  def assertTestTimeoutTest(events: List[Event]) {
    assert(events.size === 6)
    checkTestStarting(events(0), "Test 1")
    checkTestSucceeded(events(1), "Test 1")
    checkTestStarting(events(2), "Test 2")
    checkTestStarting(events(3), "Test 3")
    checkTestSucceeded(events(4), "Test 3")
    // The missing one
    checkTestSucceeded(events(5), "Test 2")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionTestTimeoutFixtureFunSuite extends fixture.FunSuite with ParallelTestExecution with TestTimeoutExpectedResults with StringFixture {
  test("Test 1") { fixture => }
  test("Test 2") { fixture => }
  test("Test 3") { fixture => }
  
  val holdTestSucceededName = "Test 2"
  val holdUntilEventCount = 5
  
  override protected[scalatest] def createTestSpecificReporter(testSorter: DistributedTestSorter, testName: String): Reporter = {
    holdingReporter = new TestHoldingReporter(super.createTestSpecificReporter(testSorter, testName), holdTestSucceededName)
    holdingReporter
  }
  
  def assertTestTimeoutTest(events: List[Event]) {
    assert(events.size === 6)
    checkTestStarting(events(0), "Test 1")
    checkTestSucceeded(events(1), "Test 1")
    checkTestStarting(events(2), "Test 2")
    checkTestStarting(events(3), "Test 3")
    checkTestSucceeded(events(4), "Test 3")
    // The missing one
    checkTestSucceeded(events(5), "Test 2")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionTestTimeoutFunSpec extends FunSpec with ParallelTestExecution with TestTimeoutExpectedResults {
  describe("Scope 1") {
    it("Test 1") {}
    it("Test 2") {}
  }
  describe("Scope 2") {
    it("Test 3") {}
    it("Test 4") {}
  }
  
  val holdTestSucceededName = "Scope 2 Test 3"
  val holdUntilEventCount = 11
  
  override protected[scalatest] def createTestSpecificReporter(testSorter: DistributedTestSorter, testName: String): Reporter = {
    holdingReporter = new TestHoldingReporter(super.createTestSpecificReporter(testSorter, testName), holdTestSucceededName)
    holdingReporter
  }
  
  def assertTestTimeoutTest(events: List[Event]) {
    assert(events.size === 12)
    checkScopeOpened(events(0), "Scope 1")
    checkTestStarting(events(1), "Scope 1 Test 1")
    checkTestSucceeded(events(2), "Scope 1 Test 1")
    checkTestStarting(events(3), "Scope 1 Test 2")
    checkTestSucceeded(events(4), "Scope 1 Test 2")
    checkScopeClosed(events(5), "Scope 1")
    checkScopeOpened(events(6), "Scope 2")
    checkTestStarting(events(7), "Scope 2 Test 3")
    checkTestStarting(events(8), "Scope 2 Test 4")
    checkTestSucceeded(events(9), "Scope 2 Test 4")
    checkScopeClosed(events(10), "Scope 2")
    // The missing one
    checkTestSucceeded(events(11), "Scope 2 Test 3")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionTestTimeoutFixtureFunSpec extends fixture.FunSpec with ParallelTestExecution with TestTimeoutExpectedResults with StringFixture {
  describe("Scope 1") {
    it("Test 1") { fixture => }
    it("Test 2") { fixture =>}
  }
  describe("Scope 2") {
    it("Test 3") { fixture => }
    it("Test 4") { fixture => }
  }
  
  val holdTestSucceededName = "Scope 2 Test 3"
  val holdUntilEventCount = 11
  
  override protected[scalatest] def createTestSpecificReporter(testSorter: DistributedTestSorter, testName: String): Reporter = {
    holdingReporter = new TestHoldingReporter(super.createTestSpecificReporter(testSorter, testName), holdTestSucceededName)
    holdingReporter
  }
  
  def assertTestTimeoutTest(events: List[Event]) {
    assert(events.size === 12)
    checkScopeOpened(events(0), "Scope 1")
    checkTestStarting(events(1), "Scope 1 Test 1")
    checkTestSucceeded(events(2), "Scope 1 Test 1")
    checkTestStarting(events(3), "Scope 1 Test 2")
    checkTestSucceeded(events(4), "Scope 1 Test 2")
    checkScopeClosed(events(5), "Scope 1")
    checkScopeOpened(events(6), "Scope 2")
    checkTestStarting(events(7), "Scope 2 Test 3")
    checkTestStarting(events(8), "Scope 2 Test 4")
    checkTestSucceeded(events(9), "Scope 2 Test 4")
    checkScopeClosed(events(10), "Scope 2")
    // The missing one
    checkTestSucceeded(events(11), "Scope 2 Test 3")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionTestTimeoutFeatureSpec extends FeatureSpec with ParallelTestExecution with TestTimeoutExpectedResults {
  feature("Scope 1") {
    scenario("Test 1") {}
    scenario("Test 2") {}
  }
  feature("Scope 2") {
    scenario("Test 3") {}
    scenario("Test 4") {}
  }
  
  val holdTestSucceededName = "Feature: Scope 2 Scenario: Test 3"
  val holdUntilEventCount = 11
  
  override protected[scalatest] def createTestSpecificReporter(testSorter: DistributedTestSorter, testName: String): Reporter = {
    holdingReporter = new TestHoldingReporter(super.createTestSpecificReporter(testSorter, testName), holdTestSucceededName)
    holdingReporter
  }
  
  def assertTestTimeoutTest(events: List[Event]) {
    assert(events.size === 12)
    checkScopeOpened(events(0), "Feature: Scope 1")
    checkTestStarting(events(1), "Feature: Scope 1 Scenario: Test 1")
    checkTestSucceeded(events(2), "Feature: Scope 1 Scenario: Test 1")
    checkTestStarting(events(3), "Feature: Scope 1 Scenario: Test 2")
    checkTestSucceeded(events(4), "Feature: Scope 1 Scenario: Test 2")
    checkScopeClosed(events(5), "Feature: Scope 1")
    checkScopeOpened(events(6), "Feature: Scope 2")
    checkTestStarting(events(7), "Feature: Scope 2 Scenario: Test 3")
    checkTestStarting(events(8), "Feature: Scope 2 Scenario: Test 4")
    checkTestSucceeded(events(9), "Feature: Scope 2 Scenario: Test 4")
    checkScopeClosed(events(10), "Feature: Scope 2")
    // The missing one
    checkTestSucceeded(events(11), "Feature: Scope 2 Scenario: Test 3")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionTestTimeoutFixtureFeatureSpec extends fixture.FeatureSpec with ParallelTestExecution with TestTimeoutExpectedResults with StringFixture {
  feature("Scope 1") {
    scenario("Test 1") { fixture => }
    scenario("Test 2") { fixture =>}
  }
  feature("Scope 2") {
    scenario("Test 3") { fixture => }
    scenario("Test 4") { fixture => }
  }
  
  val holdTestSucceededName = "Feature: Scope 2 Scenario: Test 3"
  val holdUntilEventCount = 11
  
  override protected[scalatest] def createTestSpecificReporter(testSorter: DistributedTestSorter, testName: String): Reporter = {
    holdingReporter = new TestHoldingReporter(super.createTestSpecificReporter(testSorter, testName), holdTestSucceededName)
    holdingReporter
  }
  
  def assertTestTimeoutTest(events: List[Event]) {
    assert(events.size === 12)
    checkScopeOpened(events(0), "Feature: Scope 1")
    checkTestStarting(events(1), "Feature: Scope 1 Scenario: Test 1")
    checkTestSucceeded(events(2), "Feature: Scope 1 Scenario: Test 1")
    checkTestStarting(events(3), "Feature: Scope 1 Scenario: Test 2")
    checkTestSucceeded(events(4), "Feature: Scope 1 Scenario: Test 2")
    checkScopeClosed(events(5), "Feature: Scope 1")
    checkScopeOpened(events(6), "Feature: Scope 2")
    checkTestStarting(events(7), "Feature: Scope 2 Scenario: Test 3")
    checkTestStarting(events(8), "Feature: Scope 2 Scenario: Test 4")
    checkTestSucceeded(events(9), "Feature: Scope 2 Scenario: Test 4")
    checkScopeClosed(events(10), "Feature: Scope 2")
    // The missing one
    checkTestSucceeded(events(11), "Feature: Scope 2 Scenario: Test 3")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionTestTimeoutFlatSpec extends FlatSpec with ParallelTestExecution with TestTimeoutExpectedResults {
  behavior of "Scope 1"
  it should "Test 1" in {}
  it should "Test 2" in {}
  
  behavior of "Scope 2"
  it should "Test 3" in {}
  it should "Test 4" in {}
  
  val holdTestSucceededName = "Scope 2 should Test 3"
  val holdUntilEventCount = 11
  
  override protected[scalatest] def createTestSpecificReporter(testSorter: DistributedTestSorter, testName: String): Reporter = {
    holdingReporter = new TestHoldingReporter(super.createTestSpecificReporter(testSorter, testName), holdTestSucceededName)
    holdingReporter
  }
  
  def assertTestTimeoutTest(events: List[Event]) {
    assert(events.size === 12)
    checkScopeOpened(events(0), "Scope 1")
    checkTestStarting(events(1), "Scope 1 should Test 1")
    checkTestSucceeded(events(2), "Scope 1 should Test 1")
    checkTestStarting(events(3), "Scope 1 should Test 2")
    checkTestSucceeded(events(4), "Scope 1 should Test 2")
    checkScopeClosed(events(5), "Scope 1")
    checkScopeOpened(events(6), "Scope 2")
    checkTestStarting(events(7), "Scope 2 should Test 3")
    checkTestStarting(events(8), "Scope 2 should Test 4")
    checkTestSucceeded(events(9), "Scope 2 should Test 4")
    checkScopeClosed(events(10), "Scope 2")
    // The missing one
    checkTestSucceeded(events(11), "Scope 2 should Test 3")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionTestTimeoutFixtureFlatSpec extends fixture.FlatSpec with ParallelTestExecution with TestTimeoutExpectedResults with StringFixture {
  behavior of "Scope 1"
  it should "Test 1" in { fixture => }
  it should "Test 2" in { fixture => }
  
  behavior of "Scope 2"
  it should "Test 3" in { fixture => }
  it should "Test 4" in { fixture => }
  
  val holdTestSucceededName = "Scope 2 should Test 3"
  val holdUntilEventCount = 11
  
  override protected[scalatest] def createTestSpecificReporter(testSorter: DistributedTestSorter, testName: String): Reporter = {
    holdingReporter = new TestHoldingReporter(super.createTestSpecificReporter(testSorter, testName), holdTestSucceededName)
    holdingReporter
  }
  
  def assertTestTimeoutTest(events: List[Event]) {
    assert(events.size === 12)
    checkScopeOpened(events(0), "Scope 1")
    checkTestStarting(events(1), "Scope 1 should Test 1")
    checkTestSucceeded(events(2), "Scope 1 should Test 1")
    checkTestStarting(events(3), "Scope 1 should Test 2")
    checkTestSucceeded(events(4), "Scope 1 should Test 2")
    checkScopeClosed(events(5), "Scope 1")
    checkScopeOpened(events(6), "Scope 2")
    checkTestStarting(events(7), "Scope 2 should Test 3")
    checkTestStarting(events(8), "Scope 2 should Test 4")
    checkTestSucceeded(events(9), "Scope 2 should Test 4")
    checkScopeClosed(events(10), "Scope 2")
    // The missing one
    checkTestSucceeded(events(11), "Scope 2 should Test 3")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionTestTimeoutFreeSpec extends FreeSpec with ParallelTestExecution with TestTimeoutExpectedResults {
  "Scope 1" - {
    "Test 1" in {}
    "Test 2" in {}
  }
  
  "Scope 2" - {
    "Test 3" in {}
    "Test 4" in {}
  }
  
  val holdTestSucceededName = "Scope 2 Test 3"
  val holdUntilEventCount = 11
  
  override protected[scalatest] def createTestSpecificReporter(testSorter: DistributedTestSorter, testName: String): Reporter = {
    holdingReporter = new TestHoldingReporter(super.createTestSpecificReporter(testSorter, testName), holdTestSucceededName)
    holdingReporter
  }
  
  def assertTestTimeoutTest(events: List[Event]) {
    assert(events.size === 12)
    checkScopeOpened(events(0), "Scope 1")
    checkTestStarting(events(1), "Scope 1 Test 1")
    checkTestSucceeded(events(2), "Scope 1 Test 1")
    checkTestStarting(events(3), "Scope 1 Test 2")
    checkTestSucceeded(events(4), "Scope 1 Test 2")
    checkScopeClosed(events(5), "Scope 1")
    checkScopeOpened(events(6), "Scope 2")
    checkTestStarting(events(7), "Scope 2 Test 3")
    checkTestStarting(events(8), "Scope 2 Test 4")
    checkTestSucceeded(events(9), "Scope 2 Test 4")
    checkScopeClosed(events(10), "Scope 2")
    // The missing one
    checkTestSucceeded(events(11), "Scope 2 Test 3")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionTestTimeoutFixtureFreeSpec extends fixture.FreeSpec with ParallelTestExecution with TestTimeoutExpectedResults with StringFixture {
  "Scope 1" - {
    "Test 1" in { fixture => }
    "Test 2" in { fixture => }
  }
  
  "Scope 2" - {
    "Test 3" in { fixture => }
    "Test 4" in { fixture => }
  }
  
  val holdTestSucceededName = "Scope 2 Test 3"
  val holdUntilEventCount = 11
  
  override protected[scalatest] def createTestSpecificReporter(testSorter: DistributedTestSorter, testName: String): Reporter = {
    holdingReporter = new TestHoldingReporter(super.createTestSpecificReporter(testSorter, testName), holdTestSucceededName)
    holdingReporter
  }
  
  def assertTestTimeoutTest(events: List[Event]) {
    assert(events.size === 12)
    checkScopeOpened(events(0), "Scope 1")
    checkTestStarting(events(1), "Scope 1 Test 1")
    checkTestSucceeded(events(2), "Scope 1 Test 1")
    checkTestStarting(events(3), "Scope 1 Test 2")
    checkTestSucceeded(events(4), "Scope 1 Test 2")
    checkScopeClosed(events(5), "Scope 1")
    checkScopeOpened(events(6), "Scope 2")
    checkTestStarting(events(7), "Scope 2 Test 3")
    checkTestStarting(events(8), "Scope 2 Test 4")
    checkTestSucceeded(events(9), "Scope 2 Test 4")
    checkScopeClosed(events(10), "Scope 2")
    // The missing one
    checkTestSucceeded(events(11), "Scope 2 Test 3")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionTestTimeoutPropSpec extends PropSpec with ParallelTestExecution with TestTimeoutExpectedResults {
  property("Test 1") {}
  property("Test 2") {}
  property("Test 3") {}
  
  val holdTestSucceededName = "Test 2"
  val holdUntilEventCount = 5
  
  override protected[scalatest] def createTestSpecificReporter(testSorter: DistributedTestSorter, testName: String): Reporter = {
    holdingReporter = new TestHoldingReporter(super.createTestSpecificReporter(testSorter, testName), holdTestSucceededName)
    holdingReporter
  }
  
  def assertTestTimeoutTest(events: List[Event]) {
    assert(events.size === 6)
    checkTestStarting(events(0), "Test 1")
    checkTestSucceeded(events(1), "Test 1")
    checkTestStarting(events(2), "Test 2")
    checkTestStarting(events(3), "Test 3")
    checkTestSucceeded(events(4), "Test 3")
    // The missing one
    checkTestSucceeded(events(5), "Test 2")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionTestTimeoutFixturePropSpec extends fixture.PropSpec with ParallelTestExecution with TestTimeoutExpectedResults with StringFixture {
  property("Test 1") { fixture => }
  property("Test 2") { fixture => }
  property("Test 3") { fixture => }
  
  val holdTestSucceededName = "Test 2"
  val holdUntilEventCount = 5
  
  override protected[scalatest] def createTestSpecificReporter(testSorter: DistributedTestSorter, testName: String): Reporter = {
    holdingReporter = new TestHoldingReporter(super.createTestSpecificReporter(testSorter, testName), holdTestSucceededName)
    holdingReporter
  }
  
  def assertTestTimeoutTest(events: List[Event]) {
    assert(events.size === 6)
    checkTestStarting(events(0), "Test 1")
    checkTestSucceeded(events(1), "Test 1")
    checkTestStarting(events(2), "Test 2")
    checkTestStarting(events(3), "Test 3")
    checkTestSucceeded(events(4), "Test 3")
    // The missing one
    checkTestSucceeded(events(5), "Test 2")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionTestTimeoutWordSpec extends WordSpec with ParallelTestExecution with TestTimeoutExpectedResults {
  "Scope 1" should {
    "Test 1" in {}
    "Test 2" in {}
  }
  
  "Scope 2" should {
    "Test 3" in {}
    "Test 4" in {}
  }
  
  val holdTestSucceededName = "Scope 2 should Test 3"
  val holdUntilEventCount = 11
  
  override protected[scalatest] def createTestSpecificReporter(testSorter: DistributedTestSorter, testName: String): Reporter = {
    holdingReporter = new TestHoldingReporter(super.createTestSpecificReporter(testSorter, testName), holdTestSucceededName)
    holdingReporter
  }
  
  def assertTestTimeoutTest(events: List[Event]) {
    assert(events.size === 12)
    checkScopeOpened(events(0), "Scope 1")
    checkTestStarting(events(1), "Scope 1 should Test 1")
    checkTestSucceeded(events(2), "Scope 1 should Test 1")
    checkTestStarting(events(3), "Scope 1 should Test 2")
    checkTestSucceeded(events(4), "Scope 1 should Test 2")
    checkScopeClosed(events(5), "Scope 1")
    checkScopeOpened(events(6), "Scope 2")
    checkTestStarting(events(7), "Scope 2 should Test 3")
    checkTestStarting(events(8), "Scope 2 should Test 4")
    checkTestSucceeded(events(9), "Scope 2 should Test 4")
    checkScopeClosed(events(10), "Scope 2")
    // The missing one
    checkTestSucceeded(events(11), "Scope 2 should Test 3")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionTestTimeoutFixtureWordSpec extends fixture.WordSpec with ParallelTestExecution with TestTimeoutExpectedResults with StringFixture {
  "Scope 1" should {
    "Test 1" in { fixture => }
    "Test 2" in { fixture => }
  }
  
  "Scope 2" should {
    "Test 3" in { fixture => }
    "Test 4" in { fixture => }
  }
  
  val holdTestSucceededName = "Scope 2 should Test 3"
  val holdUntilEventCount = 11
  
  override protected[scalatest] def createTestSpecificReporter(testSorter: DistributedTestSorter, testName: String): Reporter = {
    holdingReporter = new TestHoldingReporter(super.createTestSpecificReporter(testSorter, testName), holdTestSucceededName)
    holdingReporter
  }
  
  def assertTestTimeoutTest(events: List[Event]) {
    assert(events.size === 12)
    checkScopeOpened(events(0), "Scope 1")
    checkTestStarting(events(1), "Scope 1 should Test 1")
    checkTestSucceeded(events(2), "Scope 1 should Test 1")
    checkTestStarting(events(3), "Scope 1 should Test 2")
    checkTestSucceeded(events(4), "Scope 1 should Test 2")
    checkScopeClosed(events(5), "Scope 1")
    checkScopeOpened(events(6), "Scope 2")
    checkTestStarting(events(7), "Scope 2 should Test 3")
    checkTestStarting(events(8), "Scope 2 should Test 4")
    checkTestSucceeded(events(9), "Scope 2 should Test 4")
    checkScopeClosed(events(10), "Scope 2")
    // The missing one
    checkTestSucceeded(events(11), "Scope 2 should Test 3")
  }
}
