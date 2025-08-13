/*
 * Copyright 2001-2025 Artima, Inc.
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

trait DeprecatedInfoExpectedResults extends EventHelpers {
  def assertBeforeAfterInfo(events: List[Event]): Unit
}

object DeprecatedParallelTestExecutionInfoExamples extends Tables {

  // SKIP-SCALATESTJS,NATIVE-START
  def infoSpec = new DeprecatedExampleParallelTestExecutionInfoSpec()
  // SKIP-SCALATESTJS,NATIVE-END
  def infoFunSuite = new DeprecatedExampleParallelTestExecutionInfoFunSuite()
  def infoFixtureFunSuite = new DeprecatedExampleParallelTestExecutionInfoFixtureFunSuite()
  def infoFunSpec = new DeprecatedExampleParallelTestExecutionInfoFunSpec()
  def infoFixtureFunSpec = new DeprecatedExampleParallelTestExecutionInfoFixtureFunSpec()
  def infoFeatureSpec = new DeprecatedExampleParallelTestExecutionInfoFeatureSpec()
  def infoFixtureFeatureSpec = new DeprecatedExampleParallelTestExecutionInfoFixtureFeatureSpec()
  def infoFlatSpec = new DeprecatedExampleParallelTestExecutionInfoFlatSpec()
  def infoFixtureFlatSpec = new DeprecatedExampleParallelTestExecutionInfoFixtureFlatSpec()
  def infoFreeSpec = new DeprecatedExampleParallelTestExecutionInfoFreeSpec()
  def infoFixtureFreeSpec = new DeprecatedExampleParallelTestExecutionInfoFixtureFreeSpec()
  def infoPropSpec = new DeprecatedExampleParallelTestExecutionInfoPropSpec()
  def infoFixturePropSpec = new DeprecatedExampleParallelTestExecutionInfoFixturePropSpec()
  def infoWordSpec = new DeprecatedExampleParallelTestExecutionInfoWordSpec()
  def infoFixtureWordSpec = new DeprecatedExampleParallelTestExecutionInfoFixtureWordSpec()
  
  def infoExamples =
    Table(
      "suite1",
      // SKIP-SCALATESTJS,NATIVE-START
      infoSpec, 
      // SKIP-SCALATESTJS,NATIVE-END
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

// SKIP-SCALATESTJS,NATIVE-START
@DoNotDiscover
protected[scalatest] class DeprecatedExampleParallelTestExecutionInfoSpec extends RefSpec with DeprecatedInfoExpectedResults with BeforeAndAfter with ParallelTestExecution {
  before {}  // how to fire info here?
  def `test 1`: Unit = {}
  def `test 2`: Unit = {}
  def `test 3`: Unit = {}
  after {} // how to fire info here?
  
  def assertBeforeAfterInfo(events: List[Event]): Unit = {
    assert(events.size === 6)
    checkTestStarting(events(0), "test 1")
    checkTestSucceeded(events(1), "test 1")
    checkTestStarting(events(2), "test 2")
    checkTestSucceeded(events(3), "test 2")
    checkTestStarting(events(4), "test 3")
    checkTestSucceeded(events(5), "test 3")
  }
}
// SKIP-SCALATESTJS,NATIVE-END

@DoNotDiscover
protected[scalatest] class DeprecatedExampleParallelTestExecutionInfoFunSuite extends AnyFunSuite with DeprecatedInfoExpectedResults with BeforeAndAfter with ParallelTestExecution {
  before { info("In Before") }
  after { info("In After") }
  test("Test 1") {}
  test("Test 2") {}
  test("Test 3") {}
  
  def assertBeforeAfterInfo(events: List[Event]): Unit = {
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
  //SCALATESTJS,NATIVE-ONLY override def newInstance: Suite with ParallelTestExecution = new DeprecatedExampleParallelTestExecutionInfoFunSuite
}

@DoNotDiscover
protected[scalatest] class DeprecatedExampleParallelTestExecutionInfoFixtureFunSuite extends funsuite.FixtureAnyFunSuite with DeprecatedInfoExpectedResults with BeforeAndAfter with ParallelTestExecution with StringFixture {
  before { info("In Before") }
  after { info("In After") }
  test("Test 1") { fixture => }
  test("Test 2") { fixture => }
  test("Test 3") { fixture => }
  
  def assertBeforeAfterInfo(events: List[Event]): Unit = {
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
  //SCALATESTJS,NATIVE-ONLY override def newInstance: Suite with ParallelTestExecution = new DeprecatedExampleParallelTestExecutionInfoFixtureFunSuite
}

@DoNotDiscover
protected[scalatest] class DeprecatedExampleParallelTestExecutionInfoFunSpec extends AnyFunSpec with DeprecatedInfoExpectedResults with BeforeAndAfter with ParallelTestExecution {
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
  
  def assertBeforeAfterInfo(events: List[Event]): Unit = {
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
  //SCALATESTJS,NATIVE-ONLY override def newInstance: Suite with ParallelTestExecution = new DeprecatedExampleParallelTestExecutionInfoFunSpec
}

@DoNotDiscover
protected[scalatest] class DeprecatedExampleParallelTestExecutionInfoFixtureFunSpec extends funspec.FixtureAnyFunSpec with DeprecatedInfoExpectedResults with BeforeAndAfter with ParallelTestExecution with StringFixture {
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
  
  def assertBeforeAfterInfo(events: List[Event]): Unit = {
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
  //SCALATESTJS,NATIVE-ONLY override def newInstance: Suite with ParallelTestExecution = new DeprecatedExampleParallelTestExecutionInfoFixtureFunSpec
}

@DoNotDiscover
protected[scalatest] class DeprecatedExampleParallelTestExecutionInfoFeatureSpec extends AnyFeatureSpec with DeprecatedInfoExpectedResults with BeforeAndAfter with ParallelTestExecution {
  before { info("In Before") }
  after { info("In After") }
  Feature("Scope 1") {
    Scenario("Test 1") {}
    Scenario("Test 2") {}
  }
  Feature("Scope 2") {
    Scenario("Test 3") {}
    Scenario("Test 4") {}
  }
  
  def assertBeforeAfterInfo(events: List[Event]): Unit = {
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
  //SCALATESTJS,NATIVE-ONLY override def newInstance: Suite with ParallelTestExecution = new DeprecatedExampleParallelTestExecutionInfoFeatureSpec
}

@DoNotDiscover
protected[scalatest] class DeprecatedExampleParallelTestExecutionInfoFixtureFeatureSpec extends featurespec.FixtureAnyFeatureSpec with DeprecatedInfoExpectedResults with BeforeAndAfter with ParallelTestExecution with StringFixture {
  before { info("In Before") }
  after { info("In After") }
  Feature("Scope 1") {
    Scenario("Test 1") { fixture => }
    Scenario("Test 2") { fixture =>}
  }
  Feature("Scope 2") {
    Scenario("Test 3") { fixture => }
    Scenario("Test 4") { fixture =>}
  }
  
  def assertBeforeAfterInfo(events: List[Event]): Unit = {
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
  //SCALATESTJS,NATIVE-ONLY override def newInstance: Suite with ParallelTestExecution = new DeprecatedExampleParallelTestExecutionInfoFixtureFeatureSpec
}

@DoNotDiscover
protected[scalatest] class DeprecatedExampleParallelTestExecutionInfoFlatSpec extends AnyFlatSpec with DeprecatedInfoExpectedResults with BeforeAndAfter with ParallelTestExecution {
  before { info("In Before") }
  after { info("In After") }
  behavior of "Scope 1"
  it should "Test 1" in {}
  it should "Test 2" in {}
  
  behavior of "Scope 2"
  it should "Test 3" in {}
  it should "Test 4" in {}
  
  def assertBeforeAfterInfo(events: List[Event]): Unit = {
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
  //SCALATESTJS,NATIVE-ONLY override def newInstance: Suite with ParallelTestExecution = new DeprecatedExampleParallelTestExecutionInfoFlatSpec
}

@DoNotDiscover
protected[scalatest] class DeprecatedExampleParallelTestExecutionInfoFixtureFlatSpec extends flatspec.FixtureAnyFlatSpec with DeprecatedInfoExpectedResults with BeforeAndAfter with ParallelTestExecution with StringFixture {
  before { info("In Before") }
  after { info("In After") }
  behavior of "Scope 1"
  it should "Test 1" in { fixture => }
  it should "Test 2" in { fixture => }
  
  behavior of "Scope 2"
  it should "Test 3" in { fixture => }
  it should "Test 4" in { fixture => }
  
  def assertBeforeAfterInfo(events: List[Event]): Unit = {
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
  //SCALATESTJS,NATIVE-ONLY override def newInstance: Suite with ParallelTestExecution = new DeprecatedExampleParallelTestExecutionInfoFixtureFlatSpec
}

@DoNotDiscover
protected[scalatest] class DeprecatedExampleParallelTestExecutionInfoFreeSpec extends AnyFreeSpec with DeprecatedInfoExpectedResults with BeforeAndAfter with ParallelTestExecution {
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
  
  def assertBeforeAfterInfo(events: List[Event]): Unit = {
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
  //SCALATESTJS,NATIVE-ONLY override def newInstance: Suite with ParallelTestExecution = new DeprecatedExampleParallelTestExecutionInfoFreeSpec
}

@DoNotDiscover
protected[scalatest] class DeprecatedExampleParallelTestExecutionInfoFixtureFreeSpec extends freespec.FixtureAnyFreeSpec with DeprecatedInfoExpectedResults with BeforeAndAfter with ParallelTestExecution with StringFixture {
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
  
  def assertBeforeAfterInfo(events: List[Event]): Unit = {
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
  //SCALATESTJS,NATIVE-ONLY override def newInstance: Suite with ParallelTestExecution = new DeprecatedExampleParallelTestExecutionInfoFixtureFreeSpec
}

@DoNotDiscover
protected[scalatest] class DeprecatedExampleParallelTestExecutionInfoPropSpec extends AnyPropSpec with DeprecatedInfoExpectedResults with BeforeAndAfter with ParallelTestExecution {
  before { info("In Before") }
  after { info("In After") }
  property("Test 1") {}
  property("Test 2") {}
  property("Test 3") {}
  
  def assertBeforeAfterInfo(events: List[Event]): Unit = {
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
  //SCALATESTJS,NATIVE-ONLY override def newInstance: Suite with ParallelTestExecution = new DeprecatedExampleParallelTestExecutionInfoPropSpec
}

@DoNotDiscover
protected[scalatest] class DeprecatedExampleParallelTestExecutionInfoFixturePropSpec extends propspec.FixtureAnyPropSpec with DeprecatedInfoExpectedResults with BeforeAndAfter with ParallelTestExecution with StringFixture {
  before { info("In Before") }
  after { info("In After") }
  property("Test 1") { fixture => }
  property("Test 2") { fixture => }
  property("Test 3") { fixture => }
  
  def assertBeforeAfterInfo(events: List[Event]): Unit = {
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
  //SCALATESTJS,NATIVE-ONLY override def newInstance: Suite with ParallelTestExecution = new DeprecatedExampleParallelTestExecutionInfoFixturePropSpec
}

@DoNotDiscover
protected[scalatest] class DeprecatedExampleParallelTestExecutionInfoWordSpec extends AnyWordSpec with DeprecatedInfoExpectedResults with BeforeAndAfter with ParallelTestExecution {
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
  
  def assertBeforeAfterInfo(events: List[Event]): Unit = {
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
  //SCALATESTJS,NATIVE-ONLY override def newInstance: Suite with ParallelTestExecution = new DeprecatedExampleParallelTestExecutionInfoWordSpec
}
@DoNotDiscover
protected[scalatest] class DeprecatedExampleParallelTestExecutionInfoFixtureWordSpec extends wordspec.FixtureAnyWordSpec with DeprecatedInfoExpectedResults with BeforeAndAfter with ParallelTestExecution with StringFixture {
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
  
  def assertBeforeAfterInfo(events: List[Event]): Unit = {
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
  //SCALATESTJS,NATIVE-ONLY override def newInstance: Suite with ParallelTestExecution = new DeprecatedExampleParallelTestExecutionInfoFixtureWordSpec
}
