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

import org.scalatest.prop.Tables
import org.scalatest.events.Event

trait OrderExpectedResults extends EventHelpers {
  def assertOrderTest(events: List[Event])
}

object ParallelTestExecutionOrderExamples extends Tables {

  // SKIP-SCALATESTJS-START
  def orderSpec = new ExampleParallelTestExecutionOrderSpec
  def orderFixtureSpec = new ExampleParallelTestExecutionOrderFixtureSpec
  // SKIP-SCALATESTJS-END
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
      // SKIP-SCALATESTJS-START
      orderSpec,
      orderFixtureSpec,
      // SKIP-SCALATESTJS-END
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

// SKIP-SCALATESTJS-START
@DoNotDiscover
protected[scalatest] class ExampleParallelTestExecutionOrderSpec extends Spec with OrderExpectedResults with ParallelTestExecution {
  def `test 1` {}
  def `test 2` {}
  def `test 3` {}
  
  def assertOrderTest(events: List[Event]) {
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
protected[scalatest] class ExampleParallelTestExecutionOrderFixtureSpec extends fixture.Spec with OrderExpectedResults with ParallelTestExecution with StringFixture {
  def `test 1`(fixture: String) {}
  def `test 2`(fixture: String) {}
  def `test 3`(fixture: String) {}
  
  def assertOrderTest(events: List[Event]) {
    assert(events.size === 6)
    checkTestStarting(events(0), "test 1")
    checkTestSucceeded(events(1), "test 1")
    checkTestStarting(events(2), "test 2")
    checkTestSucceeded(events(3), "test 2")
    checkTestStarting(events(4), "test 3")
    checkTestSucceeded(events(5), "test 3")
  }
}
// SKIP-SCALATESTJS-END

@DoNotDiscover
protected[scalatest] class ExampleParallelTestExecutionOrderFunSuite extends FunSuite with OrderExpectedResults with ParallelTestExecution {
  test("Test 1") { succeed }
  test("Test 2") { succeed }
  test("Test 3") { succeed }
  def assertOrderTest(events: List[Event]) {
    assert(events.size === 6)
    checkTestStarting(events(0), "Test 1")
    checkTestSucceeded(events(1), "Test 1")
    checkTestStarting(events(2), "Test 2")
    checkTestSucceeded(events(3), "Test 2")
    checkTestStarting(events(4), "Test 3")
    checkTestSucceeded(events(5), "Test 3")
  }
  //SCALATESTJS-ONLY override def newInstance: Suite with ParallelTestExecution = new ExampleParallelTestExecutionOrderFunSuite
}

@DoNotDiscover
protected[scalatest] class ExampleParallelTestExecutionOrderFixtureFunSuite extends fixture.FunSuite with OrderExpectedResults with ParallelTestExecution with StringFixture {
  test("Fixture Test 1") { fixture => succeed }
  test("Fixture Test 2") { fixture => succeed }
  test("Fixture Test 3") { fixture => succeed }
  def assertOrderTest(events: List[Event]) {
    assert(events.size === 6)
    checkTestStarting(events(0), "Fixture Test 1")
    checkTestSucceeded(events(1), "Fixture Test 1")
    checkTestStarting(events(2), "Fixture Test 2")
    checkTestSucceeded(events(3), "Fixture Test 2")
    checkTestStarting(events(4), "Fixture Test 3")
    checkTestSucceeded(events(5), "Fixture Test 3")
  }
  //SCALATESTJS-ONLY override def newInstance: Suite with ParallelTestExecution = new ExampleParallelTestExecutionOrderFixtureFunSuite
}

@DoNotDiscover
protected[scalatest] class ExampleParallelTestExecutionOrderFunSpec extends FunSpec with OrderExpectedResults with ParallelTestExecution {
  describe("Scope 1") {
    it("Test 1") { succeed }
    it("Test 2") { succeed }
  }
  describe("Scope 2") {
    it("Test 3") { succeed }
    it("Test 4") { succeed }
  }
  def assertOrderTest(events: List[Event]) {
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
  //SCALATESTJS-ONLY override def newInstance: Suite with ParallelTestExecution = new ExampleParallelTestExecutionOrderFunSpec
}

@DoNotDiscover
protected[scalatest] class ExampleParallelTestExecutionOrderFixtureFunSpec extends fixture.FunSpec with OrderExpectedResults with ParallelTestExecution with StringFixture {
  describe("Fixture Scope 1") {
    it("Fixture Test 1") { fixture => succeed }
    it("Fixture Test 2") { fixture => succeed }
  }
  describe("Fixture Scope 2") {
    it("Fixture Test 3") { fixture => succeed }
    it("Fixture Test 4") { fixture => succeed }
  }
  def assertOrderTest(events: List[Event]) {
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
  //SCALATESTJS-ONLY override def newInstance: Suite with ParallelTestExecution = new ExampleParallelTestExecutionOrderFixtureFunSpec
}

@DoNotDiscover
protected[scalatest] class ExampleParallelTestExecutionOrderFeatureSpec extends FeatureSpec with OrderExpectedResults with ParallelTestExecution {
  feature("Scope 1") {
    scenario("Test 1") { succeed }
    scenario("Test 2") { succeed }
  }
  feature("Scope 2") {
    scenario("Test 3") { succeed }
    scenario("Test 4") { succeed }
  }
  def assertOrderTest(events: List[Event]) {
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
  //SCALATESTJS-ONLY override def newInstance: Suite with ParallelTestExecution = new ExampleParallelTestExecutionOrderFeatureSpec
}

@DoNotDiscover
protected[scalatest] class ExampleParallelTestExecutionOrderFixtureFeatureSpec extends fixture.FeatureSpec with OrderExpectedResults with ParallelTestExecution with StringFixture {
  feature("Fixture Scope 1") {
    scenario("Fixture Test 1") { fixture => succeed }
    scenario("Fixture Test 2") { fixture => succeed }
  }
  feature("Fixture Scope 2") {
    scenario("Fixture Test 3") { fixture => succeed }
    scenario("Fixture Test 4") { fixture => succeed }
  }
  def assertOrderTest(events: List[Event]) {
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
  //SCALATESTJS-ONLY override def newInstance: Suite with ParallelTestExecution = new ExampleParallelTestExecutionOrderFixtureFeatureSpec
}

@DoNotDiscover
protected[scalatest] class ExampleParallelTestExecutionOrderFlatSpec extends FlatSpec with OrderExpectedResults with ParallelTestExecution {
  behavior of "Scope 1"
  it should "Test 1" in { succeed }
  it should "Test 2" in { succeed }
  
  behavior of "Scope 2"
  it should "Test 3" in { succeed }
  it should "Test 4" in { succeed }
  
  def assertOrderTest(events: List[Event]) {
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
  //SCALATESTJS-ONLY override def newInstance: Suite with ParallelTestExecution = new ExampleParallelTestExecutionOrderFlatSpec
}

@DoNotDiscover
protected[scalatest] class ExampleParallelTestExecutionOrderFixtureFlatSpec extends fixture.FlatSpec with OrderExpectedResults with ParallelTestExecution with StringFixture {
  behavior of "Fixture Scope 1"
  it should "Fixture Test 1" in { fixture => succeed }
  it should "Fixture Test 2" in { fixture => succeed }
  
  behavior of "Fixture Scope 2"
  it should "Fixture Test 3" in { fixture => succeed }
  it should "Fixture Test 4" in { fixture => succeed }
  
  def assertOrderTest(events: List[Event]) {
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
  //SCALATESTJS-ONLY override def newInstance: Suite with ParallelTestExecution = new ExampleParallelTestExecutionOrderFixtureFlatSpec
}

@DoNotDiscover
protected[scalatest] class ExampleParallelTestExecutionOrderFreeSpec extends FreeSpec with OrderExpectedResults with ParallelTestExecution {
  "Scope 1" - {
    "Test 1" in { succeed }
    "Test 2" in { succeed }
  }
  
  "Scope 2" - {
    "Test 3" in { succeed }
    "Test 4" in { succeed }
  }
  
  def assertOrderTest(events: List[Event]) {
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
  //SCALATESTJS-ONLY override def newInstance: Suite with ParallelTestExecution = new ExampleParallelTestExecutionOrderFreeSpec
}

@DoNotDiscover
protected[scalatest] class ExampleParallelTestExecutionOrderFixtureFreeSpec extends fixture.FreeSpec with OrderExpectedResults with ParallelTestExecution with StringFixture {
  "Fixture Scope 1" - {
    "Fixture Test 1" in { fixture => succeed }
    "Fixture Test 2" in { fixture => succeed }
  }
  
  "Fixture Scope 2" - {
    "Fixture Test 3" in { fixture => succeed }
    "Fixture Test 4" in { fixture => succeed }
  }
  
  def assertOrderTest(events: List[Event]) {
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
  //SCALATESTJS-ONLY override def newInstance: Suite with ParallelTestExecution = new ExampleParallelTestExecutionOrderFixtureFreeSpec
}

@DoNotDiscover
protected[scalatest] class ExampleParallelTestExecutionOrderPropSpec extends PropSpec with OrderExpectedResults with ParallelTestExecution {
  property("Test 1") { succeed }
  property("Test 2") { succeed }
  property("Test 3") { succeed }
  
  def assertOrderTest(events: List[Event]) {
    assert(events.size === 6)
    checkTestStarting(events(0), "Test 1")
    checkTestSucceeded(events(1), "Test 1")
    checkTestStarting(events(2), "Test 2")
    checkTestSucceeded(events(3), "Test 2")
    checkTestStarting(events(4), "Test 3")
    checkTestSucceeded(events(5), "Test 3")
  }
  //SCALATESTJS-ONLY override def newInstance: Suite with ParallelTestExecution = new ExampleParallelTestExecutionOrderPropSpec
}

@DoNotDiscover
protected[scalatest] class ExampleParallelTestExecutionOrderFixturePropSpec extends fixture.PropSpec with OrderExpectedResults with ParallelTestExecution with StringFixture {
  property("Fixture Test 1") { fixture => succeed }
  property("Fixture Test 2") { fixture => succeed }
  property("Fixture Test 3") { fixture => succeed }
  
  def assertOrderTest(events: List[Event]) {
    assert(events.size === 6)
    checkTestStarting(events(0), "Fixture Test 1")
    checkTestSucceeded(events(1), "Fixture Test 1")
    checkTestStarting(events(2), "Fixture Test 2")
    checkTestSucceeded(events(3), "Fixture Test 2")
    checkTestStarting(events(4), "Fixture Test 3")
    checkTestSucceeded(events(5), "Fixture Test 3")
  }
  //SCALATESTJS-ONLY override def newInstance: Suite with ParallelTestExecution = new ExampleParallelTestExecutionOrderFixturePropSpec
}

@DoNotDiscover
protected[scalatest] class ExampleParallelTestExecutionOrderWordSpec extends WordSpec with OrderExpectedResults with ParallelTestExecution {
  "Scope 1" should {
    "Test 1" in { succeed }
    "Test 2" in { succeed }
  }
  
  "Scope 2" should {
    "Test 3" in { succeed }
    "Test 4" in { succeed }
  }
  
  def assertOrderTest(events: List[Event]) {
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
  //SCALATESTJS-ONLY override def newInstance: Suite with ParallelTestExecution = new ExampleParallelTestExecutionOrderWordSpec
}

@DoNotDiscover
protected[scalatest] class ExampleParallelTestExecutionOrderFixtureWordSpec extends fixture.WordSpec with OrderExpectedResults with ParallelTestExecution with StringFixture {
  "Fixture Scope 1" should {
    "Fixture Test 1" in { fixture => succeed }
    "Fixture Test 2" in { fixture => succeed }
  }
  
  "Fixture Scope 2" should {
    "Fixture Test 3" in { fixture => succeed }
    "Fixture Test 4" in { fixture => succeed }
  }
  
  def assertOrderTest(events: List[Event]) {
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
  //SCALATESTJS-ONLY override def newInstance: Suite with ParallelTestExecution = new ExampleParallelTestExecutionOrderFixtureWordSpec
}
