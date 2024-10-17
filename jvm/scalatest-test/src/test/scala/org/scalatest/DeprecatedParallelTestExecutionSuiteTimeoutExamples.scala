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

import org.scalatest.events.Event
import org.scalatest.prop.Tables
import org.scalatest.time.Span
import scala.collection.mutable.ListBuffer
import org.scalatest.events.ScopeClosed
import org.scalatest.events.TestStarting
import org.scalatest.events.TestSucceeded
import org.scalatest.time.Millis
// SKIP-SCALATESTJS,NATIVE-START
import org.scalatest.refspec.RefSpec
// SKIP-SCALATESTJS,NATIVE-END
import org.scalatest.{ featurespec, flatspec, freespec, funspec, funsuite, propspec, wordspec }
import org.scalatest.featurespec.AnyFeatureSpec
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.propspec.AnyPropSpec
import org.scalatest.wordspec.AnyWordSpec

trait DeprecatedSuiteTimeoutSetting { s: ParallelTestExecution with Suite =>
  override abstract def sortingTimeout: Span = Span(300, Millis)
}

trait DeprecatedSuiteTimeoutSuites extends EventHelpers {
  def suite1: Suite with DeprecatedSuiteTimeoutSetting
  def suite2: Suite with DeprecatedSuiteTimeoutSetting
  val holdingSuiteId: String
  val holdingTestName: String
  val holdingScopeClosedName: Option[String]
  val holdUntilEventCount: Int
  def assertSuiteTimeoutTest(events: List[Event]): Unit
}

class DeprecatedSuiteHoldingReporter(dispatch: Reporter, holdingSuiteId: String, holdingTestName: String, holdingScopeClosedName: Option[String]) extends CatchReporter {
  val out = System.err
  private val holdEvents = new ListBuffer[Event]()
  override protected def doApply(event: Event): Unit = {
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
  protected def doDispose(): Unit = {}
  def fireHoldEvents(): Unit = {
    holdEvents.foreach(dispatch(_))
  }
}

object DeprecatedParallelTestExecutionSuiteTimeoutExamples extends Tables {
  
  def suiteTimeoutExamples = 
    Table(
      "pair",
      // SKIP-SCALATESTJS,NATIVE-START
      new DeprecatedExampleParallelTestExecutionSuiteTimeoutSpecPair,
      // SKIP-SCALATESTJS,NATIVE-END
      new DeprecatedExampleParallelTestExecutionSuiteTimeoutFunSuitePair, 
      new DeprecatedExampleParallelTestExecutionSuiteTimeoutFunSpecPair, 
      new DeprecatedExampleParallelTestExecutionSuiteTimeoutFeatureSpecPair,
      new DeprecatedExampleParallelTestExecutionSuiteTimeoutFlatSpecPair,
      new DeprecatedExampleParallelTestExecutionSuiteTimeoutFreeSpecPair,
      new DeprecatedExampleParallelTestExecutionSuiteTimeoutPropSpecPair,
      new DeprecatedExampleParallelTestExecutionSuiteTimeoutWordSpecPair
    )
}

// SKIP-SCALATESTJS,NATIVE-START
class DeprecatedExampleParallelTestExecutionSuiteTimeoutSpecPair extends DeprecatedSuiteTimeoutSuites {
  def suite1 = new DeprecatedExampleParallelTestExecutionSuiteTimeoutSpec
  def suite2 = new DeprecatedExampleParallelTestExecutionSuiteTimeoutSpec
  val holdingSuiteId = suite1.suiteId
  val holdingTestName = "test 3"
  val holdingScopeClosedName = None
  val holdUntilEventCount = 13
  def assertSuiteTimeoutTest(events: List[Event]): Unit = {
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
protected[scalatest] class DeprecatedExampleParallelTestExecutionSuiteTimeoutSpec extends RefSpec with ParallelTestExecution with DeprecatedSuiteTimeoutSetting {
  def `test 1`: Unit = {}
  def `test 2`: Unit = {}
  def `test 3`: Unit = {}
}

// SKIP-SCALATESTJS,NATIVE-END

class DeprecatedExampleParallelTestExecutionSuiteTimeoutFunSuitePair extends DeprecatedSuiteTimeoutSuites {
  def suite1 = new DeprecatedExampleParallelTestExecutionSuiteTimeoutFunSuite
  def suite2 = new DeprecatedExampleParallelTestExecutionSuiteTimeoutFixtureFunSuite
  val holdingSuiteId = suite1.suiteId
  val holdingTestName = "Test 3"
  val holdingScopeClosedName = None
  val holdUntilEventCount = 13
  def assertSuiteTimeoutTest(events: List[Event]): Unit = {
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
protected[scalatest] class DeprecatedExampleParallelTestExecutionSuiteTimeoutFunSuite extends AnyFunSuite with ParallelTestExecution with DeprecatedSuiteTimeoutSetting {
  test("Test 1") {}
  test("Test 2") {}
  test("Test 3") {}
  //SCALATESTJS,NATIVE-ONLY override def newInstance: Suite with ParallelTestExecution = new DeprecatedExampleParallelTestExecutionSuiteTimeoutFunSuite
}

@DoNotDiscover
protected[scalatest] class DeprecatedExampleParallelTestExecutionSuiteTimeoutFixtureFunSuite extends funsuite.FixtureAnyFunSuite with ParallelTestExecution with DeprecatedSuiteTimeoutSetting with StringFixture {
  test("Fixture Test 1") { fixture => }
  test("Fixture Test 2") { fixture => }
  test("Fixture Test 3") { fixture => }
  //SCALATESTJS,NATIVE-ONLY override def newInstance: Suite with ParallelTestExecution = new DeprecatedExampleParallelTestExecutionSuiteTimeoutFixtureFunSuite
}

class DeprecatedExampleParallelTestExecutionSuiteTimeoutFunSpecPair extends DeprecatedSuiteTimeoutSuites {
  def suite1 = new DeprecatedExampleParallelTestExecutionSuiteTimeoutFunSpec
  def suite2 = new DeprecatedExampleParallelTestExecutionSuiteTimeoutFixtureFunSpec
  val holdingSuiteId = suite1.suiteId
  val holdingTestName = "Scope 2 Test 4"
  val holdingScopeClosedName = Some("Scope 2")
  val holdUntilEventCount = 24
  def assertSuiteTimeoutTest(events: List[Event]): Unit = {
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
protected[scalatest] class DeprecatedExampleParallelTestExecutionSuiteTimeoutFunSpec extends AnyFunSpec with ParallelTestExecution with DeprecatedSuiteTimeoutSetting {
  describe("Scope 1") {
    it("Test 1") {}
    it("Test 2") {}
  }
  describe("Scope 2") {
    it("Test 3") {}
    it("Test 4") {}
  }
  //SCALATESTJS,NATIVE-ONLY override def newInstance: Suite with ParallelTestExecution = new DeprecatedExampleParallelTestExecutionSuiteTimeoutFunSpec
}

@DoNotDiscover
protected[scalatest] class DeprecatedExampleParallelTestExecutionSuiteTimeoutFixtureFunSpec extends funspec.FixtureAnyFunSpec with ParallelTestExecution with DeprecatedSuiteTimeoutSetting with StringFixture {
  describe("Fixture Scope 1") {
    it("Fixture Test 1") { fixture => }
    it("Fixture Test 2") { fixture => }
  }
  describe("Fixture Scope 2") {
    it("Fixture Test 3") { fixture => }
    it("Fixture Test 4") { fixture => }
  }
  //SCALATESTJS,NATIVE-ONLY override def newInstance: Suite with ParallelTestExecution = new DeprecatedExampleParallelTestExecutionSuiteTimeoutFixtureFunSpec
}

class DeprecatedExampleParallelTestExecutionSuiteTimeoutFeatureSpecPair extends DeprecatedSuiteTimeoutSuites {
  def suite1 = new DeprecatedExampleParallelTestExecutionSuiteTimeoutFeatureSpec
  def suite2 = new DeprecatedExampleParallelTestExecutionSuiteTimeoutFixtureFeatureSpec
  val holdingSuiteId = suite1.suiteId
  val holdingTestName = "Feature: Scope 2 Scenario: Test 4"
  val holdingScopeClosedName = Some("Feature: Scope 2")
  val holdUntilEventCount = 24
  def assertSuiteTimeoutTest(events: List[Event]): Unit = {
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
protected[scalatest] class DeprecatedExampleParallelTestExecutionSuiteTimeoutFeatureSpec extends AnyFeatureSpec with ParallelTestExecution with DeprecatedSuiteTimeoutSetting {
  Feature("Scope 1") {
    Scenario("Test 1") {}
    Scenario("Test 2") {}
  }
  Feature("Scope 2") {
    Scenario("Test 3") {}
    Scenario("Test 4") {}
  }
  //SCALATESTJS,NATIVE-ONLY override def newInstance: Suite with ParallelTestExecution = new DeprecatedExampleParallelTestExecutionSuiteTimeoutFeatureSpec
}

@DoNotDiscover
protected[scalatest] class DeprecatedExampleParallelTestExecutionSuiteTimeoutFixtureFeatureSpec extends featurespec.FixtureAnyFeatureSpec with ParallelTestExecution with DeprecatedSuiteTimeoutSetting with StringFixture {
  Feature("Fixture Scope 1") {
    Scenario("Fixture Test 1") { fixture => }
    Scenario("Fixture Test 2") { fixture =>}
  }
  Feature("Fixture Scope 2") {
    Scenario("Fixture Test 3") { fixture => }
    Scenario("Fixture Test 4") { fixture => }
  }
  //SCALATESTJS,NATIVE-ONLY override def newInstance: Suite with ParallelTestExecution = new DeprecatedExampleParallelTestExecutionSuiteTimeoutFixtureFeatureSpec
}

class DeprecatedExampleParallelTestExecutionSuiteTimeoutFlatSpecPair extends DeprecatedSuiteTimeoutSuites {
  def suite1 = new DeprecatedExampleParallelTestExecutionSuiteTimeoutFlatSpec
  def suite2 = new DeprecatedExampleParallelTestExecutionSuiteTimeoutFixtureFlatSpec
  val holdingSuiteId = suite1.suiteId
  val holdingTestName = "Scope 2 should Test 4"
  val holdingScopeClosedName = Some("Scope 2")
  val holdUntilEventCount = 24
  def assertSuiteTimeoutTest(events: List[Event]): Unit = {
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
protected[scalatest] class DeprecatedExampleParallelTestExecutionSuiteTimeoutFlatSpec extends AnyFlatSpec with ParallelTestExecution with DeprecatedSuiteTimeoutSetting {
  behavior of "Scope 1"
  it should "Test 1" in {}
  it should "Test 2" in {}
  
  behavior of "Scope 2"
  it should "Test 3" in {}
  it should "Test 4" in {}
  //SCALATESTJS,NATIVE-ONLY override def newInstance: Suite with ParallelTestExecution = new DeprecatedExampleParallelTestExecutionSuiteTimeoutFlatSpec
}

@DoNotDiscover
protected[scalatest] class DeprecatedExampleParallelTestExecutionSuiteTimeoutFixtureFlatSpec extends flatspec.FixtureAnyFlatSpec with ParallelTestExecution with DeprecatedSuiteTimeoutSetting with StringFixture {
  behavior of "Fixture Scope 1"
  it should "Fixture Test 1" in { fixture => }
  it should "Fixture Test 2" in { fixture => }
  
  behavior of "Fixture Scope 2"
  it should "Fixture Test 3" in { fixture => }
  it should "Fixture Test 4" in { fixture => }
  //SCALATESTJS,NATIVE-ONLY override def newInstance: Suite with ParallelTestExecution = new DeprecatedExampleParallelTestExecutionSuiteTimeoutFlatSpec
}

class DeprecatedExampleParallelTestExecutionSuiteTimeoutFreeSpecPair extends DeprecatedSuiteTimeoutSuites {
  def suite1 = new DeprecatedExampleParallelTestExecutionSuiteTimeoutFreeSpec
  def suite2 = new DeprecatedExampleParallelTestExecutionSuiteTimeoutFixtureFreeSpec
  val holdingSuiteId = suite1.suiteId
  val holdingTestName = "Scope 2 Test 4"
  val holdingScopeClosedName = Some("Scope 2")
  val holdUntilEventCount = 24
  def assertSuiteTimeoutTest(events: List[Event]): Unit = {
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
protected[scalatest] class DeprecatedExampleParallelTestExecutionSuiteTimeoutFreeSpec extends AnyFreeSpec with ParallelTestExecution with DeprecatedSuiteTimeoutSetting {
  "Scope 1" - {
    "Test 1" in {}
    "Test 2" in {}
  }
  
  "Scope 2" - {
    "Test 3" in {}
    "Test 4" in {}
  }
  //SCALATESTJS,NATIVE-ONLY override def newInstance: Suite with ParallelTestExecution = new DeprecatedExampleParallelTestExecutionSuiteTimeoutFreeSpec
}

@DoNotDiscover
protected[scalatest] class DeprecatedExampleParallelTestExecutionSuiteTimeoutFixtureFreeSpec extends freespec.FixtureAnyFreeSpec with ParallelTestExecution with DeprecatedSuiteTimeoutSetting with StringFixture {
  "Fixture Scope 1" - {
    "Fixture Test 1" in { fixture => }
    "Fixture Test 2" in { fixture => }
  }
  
  "Fixture Scope 2" - {
    "Fixture Test 3" in { fixture => }
    "Fixture Test 4" in { fixture => }
  }
  //SCALATESTJS,NATIVE-ONLY override def newInstance: Suite with ParallelTestExecution = new DeprecatedExampleParallelTestExecutionSuiteTimeoutFixtureFreeSpec
}

class DeprecatedExampleParallelTestExecutionSuiteTimeoutPropSpecPair extends DeprecatedSuiteTimeoutSuites {
  def suite1 = new DeprecatedExampleParallelTestExecutionSuiteTimeoutPropSpec
  def suite2 = new DeprecatedExampleParallelTestExecutionSuiteTimeoutFixturePropSpec
  val holdingSuiteId = suite1.suiteId
  val holdingTestName = "Test 3"
  val holdingScopeClosedName = None
  val holdUntilEventCount = 13
  def assertSuiteTimeoutTest(events: List[Event]): Unit = {
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
protected[scalatest] class DeprecatedExampleParallelTestExecutionSuiteTimeoutPropSpec extends AnyPropSpec with ParallelTestExecution with DeprecatedSuiteTimeoutSetting {
  property("Test 1") {}
  property("Test 2") {}
  property("Test 3") {}
  //SCALATESTJS,NATIVE-ONLY override def newInstance: Suite with ParallelTestExecution = new DeprecatedExampleParallelTestExecutionSuiteTimeoutPropSpec
}

@DoNotDiscover
protected[scalatest] class DeprecatedExampleParallelTestExecutionSuiteTimeoutFixturePropSpec extends propspec.FixtureAnyPropSpec with ParallelTestExecution with DeprecatedSuiteTimeoutSetting with StringFixture {
  property("Fixture Test 1") { fixture => }
  property("Fixture Test 2") { fixture => }
  property("Fixture Test 3") { fixture => }
  //SCALATESTJS,NATIVE-ONLY override def newInstance: Suite with ParallelTestExecution = new DeprecatedExampleParallelTestExecutionSuiteTimeoutFixturePropSpec
}

class DeprecatedExampleParallelTestExecutionSuiteTimeoutWordSpecPair extends DeprecatedSuiteTimeoutSuites {
  def suite1 = new DeprecatedExampleParallelTestExecutionSuiteTimeoutWordSpec
  def suite2 = new DeprecatedExampleParallelTestExecutionSuiteTimeoutFixtureWordSpec
  val holdingSuiteId = suite1.suiteId
  val holdingTestName = "Scope 2 should Test 4"
  val holdingScopeClosedName = Some("Scope 2")
  val holdUntilEventCount = 24
  def assertSuiteTimeoutTest(events: List[Event]): Unit = {
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
protected[scalatest] class DeprecatedExampleParallelTestExecutionSuiteTimeoutWordSpec extends AnyWordSpec with ParallelTestExecution with DeprecatedSuiteTimeoutSetting {
  "Scope 1" should {
    "Test 1" in {}
    "Test 2" in {}
  }
  
  "Scope 2" should {
    "Test 3" in {}
    "Test 4" in {}
  }
  //SCALATESTJS,NATIVE-ONLY override def newInstance: Suite with ParallelTestExecution = new DeprecatedExampleParallelTestExecutionSuiteTimeoutWordSpec
}

@DoNotDiscover
protected[scalatest] class DeprecatedExampleParallelTestExecutionSuiteTimeoutFixtureWordSpec extends wordspec.FixtureAnyWordSpec with ParallelTestExecution with DeprecatedSuiteTimeoutSetting with StringFixture {
  "Fixture Scope 1" should {
    "Fixture Test 1" in { fixture => }
    "Fixture Test 2" in { fixture => }
  }
  
  "Fixture Scope 2" should {
    "Fixture Test 3" in { fixture => }
    "Fixture Test 4" in { fixture => }
  }
  //SCALATESTJS,NATIVE-ONLY override def newInstance: Suite with ParallelTestExecution = new DeprecatedExampleParallelTestExecutionSuiteTimeoutFixtureWordSpec
}
