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

trait OrderExpectedResults extends EventHelpers {
  def assertOrderTest(events: List[Event]): Unit
}

object ParallelTestExecutionOrderExamples extends Tables {

  // SKIP-SCALATESTJS,NATIVE-START
  def orderSpec = new ExampleParallelTestExecutionOrderSpec
  def orderOtherSpec = new ExampleParallelTestExecutionOrderOtherSpec
  // SKIP-SCALATESTJS,NATIVE-END
  def orderFunSuite = new ExampleParallelTestExecutionOrderFunSuite
  def orderFixtureFunSuite = new ExampleParallelTestExecutionOrderFixtureFunSuite
  def orderFunSpec = new ExampleParallelTestExecutionOrderFunSpec
  def orderFixtureFunSpec = new ExampleParallelTestExecutionOrderFixtureFunSpec
  def orderFeatureSpec = new ExampleParallelTestExecutionOrderFeatureSpec
  def orderFixtureFeatureSpec = new ExampleParallelTestExecutionOrderFixtureFeatureSpec
  def orderFlatSpec = new ExampleParallelTestExecutionOrderFlatSpec
  def orderFixtureFlatSpec = new ExampleParallelTestExecutionOrderFixtureFlatSpec
  def orderFreeSpec = new ExampleParallelTestExecutionOrderFreeSpec
  def orderFixtureFreeSpec = new ExampleParallelTestExecutionOrderFixtureFreeSpec
  def orderPropSpec = new ExampleParallelTestExecutionOrderPropSpec
  def orderFixturePropSpec = new ExampleParallelTestExecutionOrderFixturePropSpec
  def orderWordSpec = new ExampleParallelTestExecutionOrderWordSpec
  def orderFixtureWordSpec = new ExampleParallelTestExecutionOrderFixtureWordSpec
  
  def orderExamples =
    Table(
      "suite1",
      // SKIP-SCALATESTJS,NATIVE-START
      orderSpec,
      orderOtherSpec,
      // SKIP-SCALATESTJS,NATIVE-END
      orderFunSuite, 
      orderFixtureFunSuite, 
      orderFunSpec, 
      orderFixtureFunSpec, 
      orderFeatureSpec, 
      orderFixtureFeatureSpec, 
      orderFlatSpec, 
      orderFixtureFlatSpec, 
      orderFreeSpec, 
      orderFixtureFreeSpec,
      orderPropSpec, 
      orderFixturePropSpec, 
      orderWordSpec, 
      orderFixtureWordSpec
    )
}

// SKIP-SCALATESTJS,NATIVE-START
@DoNotDiscover
protected[scalatest] class ExampleParallelTestExecutionOrderSpec extends RefSpec with OrderExpectedResults with ParallelTestExecution {
  def `test 1`: Unit = {}
  def `test 2`: Unit = {}
  def `test 3`: Unit = {}
  
  def assertOrderTest(events: List[Event]): Unit = {
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
protected[scalatest] class ExampleParallelTestExecutionOrderOtherSpec extends RefSpec with OrderExpectedResults with ParallelTestExecution {
  def `test 1`: Unit = {}
  def `test 2`: Unit = {}
  def `test 3`: Unit = {}
  
  def assertOrderTest(events: List[Event]): Unit = {
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
protected[scalatest] class ExampleParallelTestExecutionOrderFunSuite extends AnyFunSuite with OrderExpectedResults with ParallelTestExecution {
  test("Test 1") {}
  test("Test 2") {}
  test("Test 3") {}
  def assertOrderTest(events: List[Event]): Unit = {
    assert(events.size === 6)
    checkTestStarting(events(0), "Test 1")
    checkTestSucceeded(events(1), "Test 1")
    checkTestStarting(events(2), "Test 2")
    checkTestSucceeded(events(3), "Test 2")
    checkTestStarting(events(4), "Test 3")
    checkTestSucceeded(events(5), "Test 3")
  }
  //SCALATESTJS,NATIVE-ONLY override def newInstance: Suite with ParallelTestExecution = new ExampleParallelTestExecutionOrderFunSuite
}

@DoNotDiscover
protected[scalatest] class ExampleParallelTestExecutionOrderFixtureFunSuite extends funsuite.FixtureAnyFunSuite with OrderExpectedResults with ParallelTestExecution with StringFixture {
  test("Fixture Test 1") { fixture => }
  test("Fixture Test 2") { fixture => }
  test("Fixture Test 3") { fixture => }
  def assertOrderTest(events: List[Event]): Unit = {
    assert(events.size === 6)
    checkTestStarting(events(0), "Fixture Test 1")
    checkTestSucceeded(events(1), "Fixture Test 1")
    checkTestStarting(events(2), "Fixture Test 2")
    checkTestSucceeded(events(3), "Fixture Test 2")
    checkTestStarting(events(4), "Fixture Test 3")
    checkTestSucceeded(events(5), "Fixture Test 3")
  }
  //SCALATESTJS,NATIVE-ONLY override def newInstance: Suite with ParallelTestExecution = new ExampleParallelTestExecutionOrderFixtureFunSuite
}

@DoNotDiscover
protected[scalatest] class ExampleParallelTestExecutionOrderFunSpec extends AnyFunSpec with OrderExpectedResults with ParallelTestExecution {
  describe("Scope 1") {
    it("Test 1") {}
    it("Test 2") {}
  }
  describe("Scope 2") {
    it("Test 3") {}
    it("Test 4") {}
  }
  def assertOrderTest(events: List[Event]): Unit = {
    assert(events.size === 12)
    checkScopeOpened(events(0), "Scope 1")
    checkTestStarting(events(1), "Scope 1 Test 1")
    checkTestSucceeded(events(2), "Scope 1 Test 1")
    checkTestStarting(events(3), "Scope 1 Test 2")
    checkTestSucceeded(events(4), "Scope 1 Test 2")
    checkScopeClosed(events(5), "Scope 1")
    checkScopeOpened(events(6), "Scope 2")
    checkTestStarting(events(7), "Scope 2 Test 3")
    checkTestSucceeded(events(8), "Scope 2 Test 3")
    checkTestStarting(events(9), "Scope 2 Test 4")
    checkTestSucceeded(events(10), "Scope 2 Test 4")
    checkScopeClosed(events(11), "Scope 2")
  }
  //SCALATESTJS,NATIVE-ONLY override def newInstance: Suite with ParallelTestExecution = new ExampleParallelTestExecutionOrderFunSpec
}

@DoNotDiscover
protected[scalatest] class ExampleParallelTestExecutionOrderFixtureFunSpec extends funspec.FixtureAnyFunSpec with OrderExpectedResults with ParallelTestExecution with StringFixture {
  describe("Fixture Scope 1") {
    it("Fixture Test 1") { fixture => }
    it("Fixture Test 2") { fixture =>}
  }
  describe("Fixture Scope 2") {
    it("Fixture Test 3") { fixture => }
    it("Fixture Test 4") { fixture =>}
  }
  def assertOrderTest(events: List[Event]): Unit = {
    assert(events.size === 12)
    checkScopeOpened(events(0), "Fixture Scope 1")
    checkTestStarting(events(1), "Fixture Scope 1 Fixture Test 1")
    checkTestSucceeded(events(2), "Fixture Scope 1 Fixture Test 1")
    checkTestStarting(events(3), "Fixture Scope 1 Fixture Test 2")
    checkTestSucceeded(events(4), "Fixture Scope 1 Fixture Test 2")
    checkScopeClosed(events(5), "Fixture Scope 1")
    checkScopeOpened(events(6), "Fixture Scope 2")
    checkTestStarting(events(7), "Fixture Scope 2 Fixture Test 3")
    checkTestSucceeded(events(8), "Fixture Scope 2 Fixture Test 3")
    checkTestStarting(events(9), "Fixture Scope 2 Fixture Test 4")
    checkTestSucceeded(events(10), "Fixture Scope 2 Fixture Test 4")
    checkScopeClosed(events(11), "Fixture Scope 2")
  }
  //SCALATESTJS,NATIVE-ONLY override def newInstance: Suite with ParallelTestExecution = new ExampleParallelTestExecutionOrderFixtureFunSpec
}

@DoNotDiscover
protected[scalatest] class ExampleParallelTestExecutionOrderFeatureSpec extends AnyFeatureSpec with OrderExpectedResults with ParallelTestExecution {
  Feature("Scope 1") {
    Scenario("Test 1") {}
    Scenario("Test 2") {}
  }
  Feature("Scope 2") {
    Scenario("Test 3") {}
    Scenario("Test 4") {}
  }
  def assertOrderTest(events: List[Event]): Unit = {
    assert(events.size === 12)
    checkScopeOpened(events(0), "Feature: Scope 1")
    checkTestStarting(events(1), "Feature: Scope 1 Scenario: Test 1")
    checkTestSucceeded(events(2), "Feature: Scope 1 Scenario: Test 1")
    checkTestStarting(events(3), "Feature: Scope 1 Scenario: Test 2")
    checkTestSucceeded(events(4), "Feature: Scope 1 Scenario: Test 2")
    checkScopeClosed(events(5), "Feature: Scope 1")
    checkScopeOpened(events(6), "Feature: Scope 2")
    checkTestStarting(events(7), "Feature: Scope 2 Scenario: Test 3")
    checkTestSucceeded(events(8), "Feature: Scope 2 Scenario: Test 3")
    checkTestStarting(events(9), "Feature: Scope 2 Scenario: Test 4")
    checkTestSucceeded(events(10), "Feature: Scope 2 Scenario: Test 4")
    checkScopeClosed(events(11), "Feature: Scope 2")
  }
  //SCALATESTJS,NATIVE-ONLY override def newInstance: Suite with ParallelTestExecution = new ExampleParallelTestExecutionOrderFeatureSpec
}

@DoNotDiscover
protected[scalatest] class ExampleParallelTestExecutionOrderFixtureFeatureSpec extends featurespec.FixtureAnyFeatureSpec with OrderExpectedResults with ParallelTestExecution with StringFixture {
  Feature("Fixture Scope 1") {
    Scenario("Fixture Test 1") { fixture => }
    Scenario("Fixture Test 2") { fixture =>}
  }
  Feature("Fixture Scope 2") {
    Scenario("Fixture Test 3") { fixture => }
    Scenario("Fixture Test 4") { fixture =>}
  }
  def assertOrderTest(events: List[Event]): Unit = {
    assert(events.size === 12)
    checkScopeOpened(events(0), "Feature: Fixture Scope 1")
    checkTestStarting(events(1), "Feature: Fixture Scope 1 Scenario: Fixture Test 1")
    checkTestSucceeded(events(2), "Feature: Fixture Scope 1 Scenario: Fixture Test 1")
    checkTestStarting(events(3), "Feature: Fixture Scope 1 Scenario: Fixture Test 2")
    checkTestSucceeded(events(4), "Feature: Fixture Scope 1 Scenario: Fixture Test 2")
    checkScopeClosed(events(5), "Feature: Fixture Scope 1")
    checkScopeOpened(events(6), "Feature: Fixture Scope 2")
    checkTestStarting(events(7), "Feature: Fixture Scope 2 Scenario: Fixture Test 3")
    checkTestSucceeded(events(8), "Feature: Fixture Scope 2 Scenario: Fixture Test 3")
    checkTestStarting(events(9), "Feature: Fixture Scope 2 Scenario: Fixture Test 4")
    checkTestSucceeded(events(10), "Feature: Fixture Scope 2 Scenario: Fixture Test 4")
    checkScopeClosed(events(11), "Feature: Fixture Scope 2")
  }
  //SCALATESTJS,NATIVE-ONLY override def newInstance: Suite with ParallelTestExecution = new ExampleParallelTestExecutionOrderFixtureFeatureSpec
}

@DoNotDiscover
protected[scalatest] class ExampleParallelTestExecutionOrderFlatSpec extends AnyFlatSpec with OrderExpectedResults with ParallelTestExecution {
  behavior of "Scope 1"
  it should "Test 1" in {}
  it should "Test 2" in {}
  
  behavior of "Scope 2"
  it should "Test 3" in {}
  it should "Test 4" in {}
  
  def assertOrderTest(events: List[Event]): Unit = {
    assert(events.size === 12)
    checkScopeOpened(events(0), "Scope 1")
    checkTestStarting(events(1), "Scope 1 should Test 1")
    checkTestSucceeded(events(2), "Scope 1 should Test 1")
    checkTestStarting(events(3), "Scope 1 should Test 2")
    checkTestSucceeded(events(4), "Scope 1 should Test 2")
    checkScopeClosed(events(5), "Scope 1")
    checkScopeOpened(events(6), "Scope 2")
    checkTestStarting(events(7), "Scope 2 should Test 3")
    checkTestSucceeded(events(8), "Scope 2 should Test 3")
    checkTestStarting(events(9), "Scope 2 should Test 4")
    checkTestSucceeded(events(10), "Scope 2 should Test 4")
    checkScopeClosed(events(11), "Scope 2")
  }
  //SCALATESTJS,NATIVE-ONLY override def newInstance: Suite with ParallelTestExecution = new ExampleParallelTestExecutionOrderFlatSpec
}

@DoNotDiscover
protected[scalatest] class ExampleParallelTestExecutionOrderFixtureFlatSpec extends flatspec.FixtureAnyFlatSpec with OrderExpectedResults with ParallelTestExecution with StringFixture {
  behavior of "Fixture Scope 1"
  it should "Fixture Test 1" in { fixture => }
  it should "Fixture Test 2" in { fixture => }
  
  behavior of "Fixture Scope 2"
  it should "Fixture Test 3" in { fixture => }
  it should "Fixture Test 4" in { fixture => }
  
  def assertOrderTest(events: List[Event]): Unit = {
    assert(events.size === 12)
    checkScopeOpened(events(0), "Fixture Scope 1")
    checkTestStarting(events(1), "Fixture Scope 1 should Fixture Test 1")
    checkTestSucceeded(events(2), "Fixture Scope 1 should Fixture Test 1")
    checkTestStarting(events(3), "Fixture Scope 1 should Fixture Test 2")
    checkTestSucceeded(events(4), "Fixture Scope 1 should Fixture Test 2")
    checkScopeClosed(events(5), "Fixture Scope 1")
    checkScopeOpened(events(6), "Fixture Scope 2")
    checkTestStarting(events(7), "Fixture Scope 2 should Fixture Test 3")
    checkTestSucceeded(events(8), "Fixture Scope 2 should Fixture Test 3")
    checkTestStarting(events(9), "Fixture Scope 2 should Fixture Test 4")
    checkTestSucceeded(events(10), "Fixture Scope 2 should Fixture Test 4")
    checkScopeClosed(events(11), "Fixture Scope 2")
  }
  //SCALATESTJS,NATIVE-ONLY override def newInstance: Suite with ParallelTestExecution = new ExampleParallelTestExecutionOrderFixtureFlatSpec
}

@DoNotDiscover
protected[scalatest] class ExampleParallelTestExecutionOrderFreeSpec extends AnyFreeSpec with OrderExpectedResults with ParallelTestExecution {
  "Scope 1" - {
    "Test 1" in {}
    "Test 2" in {}
  }
  
  "Scope 2" - {
    "Test 3" in {}
    "Test 4" in {}
  }
  
  def assertOrderTest(events: List[Event]): Unit = {
    assert(events.size === 12)
    checkScopeOpened(events(0), "Scope 1")
    checkTestStarting(events(1), "Scope 1 Test 1")
    checkTestSucceeded(events(2), "Scope 1 Test 1")
    checkTestStarting(events(3), "Scope 1 Test 2")
    checkTestSucceeded(events(4), "Scope 1 Test 2")
    checkScopeClosed(events(5), "Scope 1")
    checkScopeOpened(events(6), "Scope 2")
    checkTestStarting(events(7), "Scope 2 Test 3")
    checkTestSucceeded(events(8), "Scope 2 Test 3")
    checkTestStarting(events(9), "Scope 2 Test 4")
    checkTestSucceeded(events(10), "Scope 2 Test 4")
    checkScopeClosed(events(11), "Scope 2")
  }
  //SCALATESTJS,NATIVE-ONLY override def newInstance: Suite with ParallelTestExecution = new ExampleParallelTestExecutionOrderFreeSpec
}

@DoNotDiscover
protected[scalatest] class ExampleParallelTestExecutionOrderFixtureFreeSpec extends freespec.FixtureAnyFreeSpec with OrderExpectedResults with ParallelTestExecution with StringFixture {
  "Fixture Scope 1" - {
    "Fixture Test 1" in { fixture => }
    "Fixture Test 2" in { fixture => }
  }
  
  "Fixture Scope 2" - {
    "Fixture Test 3" in { fixture => }
    "Fixture Test 4" in { fixture => }
  }
  
  def assertOrderTest(events: List[Event]): Unit = {
    assert(events.size === 12)
    checkScopeOpened(events(0), "Fixture Scope 1")
    checkTestStarting(events(1), "Fixture Scope 1 Fixture Test 1")
    checkTestSucceeded(events(2), "Fixture Scope 1 Fixture Test 1")
    checkTestStarting(events(3), "Fixture Scope 1 Fixture Test 2")
    checkTestSucceeded(events(4), "Fixture Scope 1 Fixture Test 2")
    checkScopeClosed(events(5), "Fixture Scope 1")
    checkScopeOpened(events(6), "Fixture Scope 2")
    checkTestStarting(events(7), "Fixture Scope 2 Fixture Test 3")
    checkTestSucceeded(events(8), "Fixture Scope 2 Fixture Test 3")
    checkTestStarting(events(9), "Fixture Scope 2 Fixture Test 4")
    checkTestSucceeded(events(10), "Fixture Scope 2 Fixture Test 4")
    checkScopeClosed(events(11), "Fixture Scope 2")
  }
  //SCALATESTJS,NATIVE-ONLY override def newInstance: Suite with ParallelTestExecution = new ExampleParallelTestExecutionOrderFixtureFreeSpec
}

@DoNotDiscover
protected[scalatest] class ExampleParallelTestExecutionOrderPropSpec extends AnyPropSpec with OrderExpectedResults with ParallelTestExecution {
  property("Test 1") {}
  property("Test 2") {}
  property("Test 3") {}
  
  def assertOrderTest(events: List[Event]): Unit = {
    assert(events.size === 6)
    checkTestStarting(events(0), "Test 1")
    checkTestSucceeded(events(1), "Test 1")
    checkTestStarting(events(2), "Test 2")
    checkTestSucceeded(events(3), "Test 2")
    checkTestStarting(events(4), "Test 3")
    checkTestSucceeded(events(5), "Test 3")
  }
  //SCALATESTJS,NATIVE-ONLY override def newInstance: Suite with ParallelTestExecution = new ExampleParallelTestExecutionOrderPropSpec
}

@DoNotDiscover
protected[scalatest] class ExampleParallelTestExecutionOrderFixturePropSpec extends propspec.FixtureAnyPropSpec with OrderExpectedResults with ParallelTestExecution with StringFixture {
  property("Fixture Test 1") { fixture => }
  property("Fixture Test 2") { fixture => }
  property("Fixture Test 3") { fixture => }
  
  def assertOrderTest(events: List[Event]): Unit = {
    assert(events.size === 6)
    checkTestStarting(events(0), "Fixture Test 1")
    checkTestSucceeded(events(1), "Fixture Test 1")
    checkTestStarting(events(2), "Fixture Test 2")
    checkTestSucceeded(events(3), "Fixture Test 2")
    checkTestStarting(events(4), "Fixture Test 3")
    checkTestSucceeded(events(5), "Fixture Test 3")
  }
  //SCALATESTJS,NATIVE-ONLY override def newInstance: Suite with ParallelTestExecution = new ExampleParallelTestExecutionOrderFixturePropSpec
}

@DoNotDiscover
protected[scalatest] class ExampleParallelTestExecutionOrderWordSpec extends AnyWordSpec with OrderExpectedResults with ParallelTestExecution {
  "Scope 1" should {
    "Test 1" in {}
    "Test 2" in {}
  }
  
  "Scope 2" should {
    "Test 3" in {}
    "Test 4" in {}
  }
  
  def assertOrderTest(events: List[Event]): Unit = {
    assert(events.size === 12)
    checkScopeOpened(events(0), "Scope 1")
    checkTestStarting(events(1), "Scope 1 should Test 1")
    checkTestSucceeded(events(2), "Scope 1 should Test 1")
    checkTestStarting(events(3), "Scope 1 should Test 2")
    checkTestSucceeded(events(4), "Scope 1 should Test 2")
    checkScopeClosed(events(5), "Scope 1")
    checkScopeOpened(events(6), "Scope 2")
    checkTestStarting(events(7), "Scope 2 should Test 3")
    checkTestSucceeded(events(8), "Scope 2 should Test 3")
    checkTestStarting(events(9), "Scope 2 should Test 4")
    checkTestSucceeded(events(10), "Scope 2 should Test 4")
    checkScopeClosed(events(11), "Scope 2")
  }
  //SCALATESTJS,NATIVE-ONLY override def newInstance: Suite with ParallelTestExecution = new ExampleParallelTestExecutionOrderWordSpec
}

@DoNotDiscover
protected[scalatest] class ExampleParallelTestExecutionOrderFixtureWordSpec extends wordspec.FixtureAnyWordSpec with OrderExpectedResults with ParallelTestExecution with StringFixture {
  "Fixture Scope 1" should {
    "Fixture Test 1" in { fixture => }
    "Fixture Test 2" in { fixture => }
  }
  
  "Fixture Scope 2" should {
    "Fixture Test 3" in { fixture => }
    "Fixture Test 4" in { fixture => }
  }
  
  def assertOrderTest(events: List[Event]): Unit = {
    assert(events.size === 12)
    checkScopeOpened(events(0), "Fixture Scope 1")
    checkTestStarting(events(1), "Fixture Scope 1 should Fixture Test 1")
    checkTestSucceeded(events(2), "Fixture Scope 1 should Fixture Test 1")
    checkTestStarting(events(3), "Fixture Scope 1 should Fixture Test 2")
    checkTestSucceeded(events(4), "Fixture Scope 1 should Fixture Test 2")
    checkScopeClosed(events(5), "Fixture Scope 1")
    checkScopeOpened(events(6), "Fixture Scope 2")
    checkTestStarting(events(7), "Fixture Scope 2 should Fixture Test 3")
    checkTestSucceeded(events(8), "Fixture Scope 2 should Fixture Test 3")
    checkTestStarting(events(9), "Fixture Scope 2 should Fixture Test 4")
    checkTestSucceeded(events(10), "Fixture Scope 2 should Fixture Test 4")
    checkScopeClosed(events(11), "Fixture Scope 2")
  }
  //SCALATESTJS,NATIVE-ONLY override def newInstance: Suite with ParallelTestExecution = new ExampleParallelTestExecutionOrderFixtureWordSpec
}
