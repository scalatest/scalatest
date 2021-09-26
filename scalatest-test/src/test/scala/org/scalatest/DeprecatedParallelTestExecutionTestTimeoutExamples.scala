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
package org.scalatest.deprecated

import org.scalatest._
import org.scalatest.events.Event
import org.scalatest.prop.Tables
import org.scalatest.time.Span
import org.scalatest.events.TestSucceeded
import org.scalatest.time.Millis
import org.scalatest.tools.TestSortingReporter
// SKIP-SCALATESTJS,NATIVE-START
import org.scalatest.refspec.RefSpec
// SKIP-SCALATESTJS,NATIVE-END

trait TestTimeoutExpectedResults extends EventHelpers { s: ParallelTestExecution =>
  def assertTestTimeoutTest(events: List[Event])
  val holdTestSucceededName: String
  val holdUntilEventCount: Int
  override def sortingTimeout: Span = Span(300, Millis)
  var holdingReporter: TestHoldingReporter = null
}

object DeprecatedParallelTestExecutionTestTimeoutExamples extends Tables {

  // SKIP-SCALATESTJS,NATIVE-START
  def testTimeoutSpec = new ExampleParallelTestExecutionTestTimeoutSpec()
  // SKIP-SCALATESTJS,NATIVE-END
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
      // SKIP-SCALATESTJS,NATIVE-START
      testTimeoutSpec, 
      // SKIP-SCALATESTJS,NATIVE-END
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
  override protected def doApply(event: Event): Unit = {
    event match {
      case testSucceeded: TestSucceeded if testSucceeded.testName == holdingTestSucceededName =>
        holdEvent = Some(testSucceeded)
      case _ => dispatch(event)
    }
  }
  protected def doDispose(): Unit = {}
  def fireHoldEvent(): Unit = {
    holdEvent match {
      case Some(event) => dispatch(event)
      case None =>
    }
  }
}

// SKIP-SCALATESTJS,NATIVE-START
@DoNotDiscover
class ExampleParallelTestExecutionTestTimeoutSpec extends RefSpec with ParallelTestExecution with TestTimeoutExpectedResults {
  def `test 1`: Unit = {}
  def `test 2`: Unit = {}
  def `test 3`: Unit = {}
  
  val holdTestSucceededName = "test 2"
  val holdUntilEventCount = 5
  
  override protected[scalatest] def createTestSpecificReporter(testSorter: DistributedTestSorter, testName: String): Reporter = {
    holdingReporter = new TestHoldingReporter(super.createTestSpecificReporter(testSorter, testName), holdTestSucceededName)
    holdingReporter
  }
  
  def assertTestTimeoutTest(events: List[Event]): Unit = {
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
// SKIP-SCALATESTJS,NATIVE-END

@DoNotDiscover
protected[scalatest] class ExampleParallelTestExecutionTestTimeoutFunSuite extends FunSuite with ParallelTestExecution with TestTimeoutExpectedResults {
  test("Test 1") {}
  test("Test 2") {}
  test("Test 3") {}
  
  val holdTestSucceededName = "Test 2"
  val holdUntilEventCount = 5
  
  override protected[scalatest] def createTestSpecificReporter(testSorter: DistributedTestSorter, testName: String): Reporter = {
    holdingReporter = new TestHoldingReporter(super.createTestSpecificReporter(testSorter, testName), holdTestSucceededName)
    holdingReporter
  }
  
  def assertTestTimeoutTest(events: List[Event]): Unit = {
    assert(events.size === 6)
    checkTestStarting(events(0), "Test 1")
    checkTestSucceeded(events(1), "Test 1")
    checkTestStarting(events(2), "Test 2")
    checkTestStarting(events(3), "Test 3")
    checkTestSucceeded(events(4), "Test 3")
    // The missing one
    checkTestSucceeded(events(5), "Test 2")
  }
  //SCALATESTJS,NATIVE-ONLY override def newInstance: Suite with ParallelTestExecution = new ExampleParallelTestExecutionTestTimeoutFunSuite
}

@DoNotDiscover
protected[scalatest] class ExampleParallelTestExecutionTestTimeoutFixtureFunSuite extends fixture.FunSuite with ParallelTestExecution with TestTimeoutExpectedResults with StringFixture {
  test("Test 1") { fixture => }
  test("Test 2") { fixture => }
  test("Test 3") { fixture => }
  
  val holdTestSucceededName = "Test 2"
  val holdUntilEventCount = 5
  
  override protected[scalatest] def createTestSpecificReporter(testSorter: DistributedTestSorter, testName: String): Reporter = {
    holdingReporter = new TestHoldingReporter(super.createTestSpecificReporter(testSorter, testName), holdTestSucceededName)
    holdingReporter
  }
  
  def assertTestTimeoutTest(events: List[Event]): Unit = {
    assert(events.size === 6)
    checkTestStarting(events(0), "Test 1")
    checkTestSucceeded(events(1), "Test 1")
    checkTestStarting(events(2), "Test 2")
    checkTestStarting(events(3), "Test 3")
    checkTestSucceeded(events(4), "Test 3")
    // The missing one
    checkTestSucceeded(events(5), "Test 2")
  }
  //SCALATESTJS,NATIVE-ONLY override def newInstance: Suite with ParallelTestExecution = new ExampleParallelTestExecutionTestTimeoutFixtureFunSuite
}

@DoNotDiscover
protected[scalatest] class ExampleParallelTestExecutionTestTimeoutFunSpec extends FunSpec with ParallelTestExecution with TestTimeoutExpectedResults {
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
  
  def assertTestTimeoutTest(events: List[Event]): Unit = {
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
  //SCALATESTJS,NATIVE-ONLY override def newInstance: Suite with ParallelTestExecution = new ExampleParallelTestExecutionTestTimeoutFunSpec
}

@DoNotDiscover
protected[scalatest] class ExampleParallelTestExecutionTestTimeoutFixtureFunSpec extends fixture.FunSpec with ParallelTestExecution with TestTimeoutExpectedResults with StringFixture {
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
  
  def assertTestTimeoutTest(events: List[Event]): Unit = {
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
  //SCALATESTJS,NATIVE-ONLY override def newInstance: Suite with ParallelTestExecution = new ExampleParallelTestExecutionTestTimeoutFixtureFunSpec
}

@DoNotDiscover
protected[scalatest] class ExampleParallelTestExecutionTestTimeoutFeatureSpec extends FeatureSpec with ParallelTestExecution with TestTimeoutExpectedResults {
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
  
  def assertTestTimeoutTest(events: List[Event]): Unit = {
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
  //SCALATESTJS,NATIVE-ONLY override def newInstance: Suite with ParallelTestExecution = new ExampleParallelTestExecutionTestTimeoutFeatureSpec
}

@DoNotDiscover
protected[scalatest] class ExampleParallelTestExecutionTestTimeoutFixtureFeatureSpec extends fixture.FeatureSpec with ParallelTestExecution with TestTimeoutExpectedResults with StringFixture {
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
  
  def assertTestTimeoutTest(events: List[Event]): Unit = {
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
  //SCALATESTJS,NATIVE-ONLY override def newInstance: Suite with ParallelTestExecution = new ExampleParallelTestExecutionTestTimeoutFixtureFeatureSpec
}

@DoNotDiscover
protected[scalatest] class ExampleParallelTestExecutionTestTimeoutFlatSpec extends FlatSpec with ParallelTestExecution with TestTimeoutExpectedResults {
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
  
  def assertTestTimeoutTest(events: List[Event]): Unit = {
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
  //SCALATESTJS,NATIVE-ONLY override def newInstance: Suite with ParallelTestExecution = new ExampleParallelTestExecutionTestTimeoutFlatSpec
}

@DoNotDiscover
protected[scalatest] class ExampleParallelTestExecutionTestTimeoutFixtureFlatSpec extends fixture.FlatSpec with ParallelTestExecution with TestTimeoutExpectedResults with StringFixture {
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
  
  def assertTestTimeoutTest(events: List[Event]): Unit = {
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
  //SCALATESTJS,NATIVE-ONLY override def newInstance: Suite with ParallelTestExecution = new ExampleParallelTestExecutionTestTimeoutFixtureFlatSpec
}

@DoNotDiscover
protected[scalatest] class ExampleParallelTestExecutionTestTimeoutFreeSpec extends FreeSpec with ParallelTestExecution with TestTimeoutExpectedResults {
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
  
  def assertTestTimeoutTest(events: List[Event]): Unit = {
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
  //SCALATESTJS,NATIVE-ONLY override def newInstance: Suite with ParallelTestExecution = new ExampleParallelTestExecutionTestTimeoutFreeSpec
}

@DoNotDiscover
protected[scalatest] class ExampleParallelTestExecutionTestTimeoutFixtureFreeSpec extends fixture.FreeSpec with ParallelTestExecution with TestTimeoutExpectedResults with StringFixture {
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
  
  def assertTestTimeoutTest(events: List[Event]): Unit = {
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
  //SCALATESTJS,NATIVE-ONLY override def newInstance: Suite with ParallelTestExecution = new ExampleParallelTestExecutionTestTimeoutFixtureFreeSpec
}

@DoNotDiscover
protected[scalatest] class ExampleParallelTestExecutionTestTimeoutPropSpec extends PropSpec with ParallelTestExecution with TestTimeoutExpectedResults {
  property("Test 1") {}
  property("Test 2") {}
  property("Test 3") {}
  
  val holdTestSucceededName = "Test 2"
  val holdUntilEventCount = 5
  
  override protected[scalatest] def createTestSpecificReporter(testSorter: DistributedTestSorter, testName: String): Reporter = {
    holdingReporter = new TestHoldingReporter(super.createTestSpecificReporter(testSorter, testName), holdTestSucceededName)
    holdingReporter
  }
  
  def assertTestTimeoutTest(events: List[Event]): Unit = {
    assert(events.size === 6)
    checkTestStarting(events(0), "Test 1")
    checkTestSucceeded(events(1), "Test 1")
    checkTestStarting(events(2), "Test 2")
    checkTestStarting(events(3), "Test 3")
    checkTestSucceeded(events(4), "Test 3")
    // The missing one
    checkTestSucceeded(events(5), "Test 2")
  }
  //SCALATESTJS,NATIVE-ONLY override def newInstance: Suite with ParallelTestExecution = new ExampleParallelTestExecutionTestTimeoutPropSpec
}

@DoNotDiscover
protected[scalatest] class ExampleParallelTestExecutionTestTimeoutFixturePropSpec extends fixture.PropSpec with ParallelTestExecution with TestTimeoutExpectedResults with StringFixture {
  property("Test 1") { fixture => }
  property("Test 2") { fixture => }
  property("Test 3") { fixture => }
  
  val holdTestSucceededName = "Test 2"
  val holdUntilEventCount = 5
  
  override protected[scalatest] def createTestSpecificReporter(testSorter: DistributedTestSorter, testName: String): Reporter = {
    holdingReporter = new TestHoldingReporter(super.createTestSpecificReporter(testSorter, testName), holdTestSucceededName)
    holdingReporter
  }
  
  def assertTestTimeoutTest(events: List[Event]): Unit = {
    assert(events.size === 6)
    checkTestStarting(events(0), "Test 1")
    checkTestSucceeded(events(1), "Test 1")
    checkTestStarting(events(2), "Test 2")
    checkTestStarting(events(3), "Test 3")
    checkTestSucceeded(events(4), "Test 3")
    // The missing one
    checkTestSucceeded(events(5), "Test 2")
  }
  //SCALATESTJS,NATIVE-ONLY override def newInstance: Suite with ParallelTestExecution = new ExampleParallelTestExecutionTestTimeoutFixturePropSpec
}

@DoNotDiscover
protected[scalatest] class ExampleParallelTestExecutionTestTimeoutWordSpec extends WordSpec with ParallelTestExecution with TestTimeoutExpectedResults {
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
  
  def assertTestTimeoutTest(events: List[Event]): Unit = {
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
  //SCALATESTJS,NATIVE-ONLY override def newInstance: Suite with ParallelTestExecution = new ExampleParallelTestExecutionTestTimeoutWordSpec
}

@DoNotDiscover
protected[scalatest] class ExampleParallelTestExecutionTestTimeoutFixtureWordSpec extends fixture.WordSpec with ParallelTestExecution with TestTimeoutExpectedResults with StringFixture {
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
  
  def assertTestTimeoutTest(events: List[Event]): Unit = {
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
  //SCALATESTJS,NATIVE-ONLY override def newInstance: Suite with ParallelTestExecution = new ExampleParallelTestExecutionTestTimeoutFixtureWordSpec
}
