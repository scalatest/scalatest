package org.scalatest

import org.scalatest.prop.Tables
import org.scalatest.events.Event

trait ParallelSuites extends EventHelpers {
  def suite1: Suite
  def suite2: Suite
  def assertParallelSuites(events: List[Event])
}

trait ParallelTestExecutionParallelSuiteExamples extends Tables {
  
  def parallelExamples = 
    Table(
      "pair", 
      new ExampleParallelTestExecutionParallelSuitePair, 
      new ExampleParallelTestExecutionParallelSpecPair, 
      new ExampleParallelTestExecutionParallelFunSuitePair, 
      new ExampleParallelTestExecutionParallelFunSpecPair, 
      new ExampleParallelTestExecutionParallelFeatureSpecPair,
      new ExampleParallelTestExecutionParallelFlatSpecPair,
      new ExampleParallelTestExecutionParallelFreeSpecPair,
      new ExampleParallelTestExecutionParallelPropSpecPair,
      new ExampleParallelTestExecutionParallelWordSpecPair
    )
}

class ExampleParallelTestExecutionParallelSuitePair extends ParallelSuites {
  def suite1 = new ExampleParallelTestExecutionOrderSuite
  def suite2 = new ExampleParallelTestExecutionOrderFixtureSuite
  
  def assertParallelSuites(events: List[Event]) {
    assert(events.size === 16)
    
    checkSuiteStarting(events(0), suite1.suiteId)
    checkTestStarting(events(1), "testMethod1")
    checkTestSucceeded(events(2), "testMethod1")
    checkTestStarting(events(3), "testMethod2")
    checkTestSucceeded(events(4), "testMethod2")
    checkTestStarting(events(5), "testMethod3")
    checkTestSucceeded(events(6), "testMethod3")
    checkSuiteCompleted(events(7), suite1.suiteId)
    
    checkSuiteStarting(events(8), suite2.suiteId)
    checkTestStarting(events(9), "testFixtureMethod1")
    checkTestSucceeded(events(10), "testFixtureMethod1")
    checkTestStarting(events(11), "testFixtureMethod2")
    checkTestSucceeded(events(12), "testFixtureMethod2")
    checkTestStarting(events(13), "testFixtureMethod3")
    checkTestSucceeded(events(14), "testFixtureMethod3")
    checkSuiteCompleted(events(15), suite2.suiteId)
  }
}

class ExampleParallelTestExecutionParallelSpecPair extends ParallelSuites {
  def suite1 = new ExampleParallelTestExecutionOrderSpec
  def suite2 = new ExampleParallelTestExecutionOrderFixtureSpec
  
  def assertParallelSuites(events: List[Event]) {
    assert(events.size === 16)
    
    checkSuiteStarting(events(0), suite1.suiteId)
    checkTestStarting(events(1), "test 1")
    checkTestSucceeded(events(2), "test 1")
    checkTestStarting(events(3), "test 2")
    checkTestSucceeded(events(4), "test 2")
    checkTestStarting(events(5), "test 3")
    checkTestSucceeded(events(6), "test 3")
    checkSuiteCompleted(events(7), suite1.suiteId)
    
    checkSuiteStarting(events(8), suite2.suiteId)
    checkTestStarting(events(9), "test 1")
    checkTestSucceeded(events(10), "test 1")
    checkTestStarting(events(11), "test 2")
    checkTestSucceeded(events(12), "test 2")
    checkTestStarting(events(13), "test 3")
    checkTestSucceeded(events(14), "test 3")
    checkSuiteCompleted(events(15), suite2.suiteId)
  }
}

class ExampleParallelTestExecutionParallelFunSuitePair extends ParallelSuites {
  def suite1 = new ExampleParallelTestExecutionOrderFunSuite
  def suite2 = new ExampleParallelTestExecutionOrderFixtureFunSuite
  
  def assertParallelSuites(events: List[Event]) {
    assert(events.size === 16)
    
    checkSuiteStarting(events(0), suite1.suiteId)
    checkTestStarting(events(1), "Test 1")
    checkTestSucceeded(events(2), "Test 1")
    checkTestStarting(events(3), "Test 2")
    checkTestSucceeded(events(4), "Test 2")
    checkTestStarting(events(5), "Test 3")
    checkTestSucceeded(events(6), "Test 3")
    checkSuiteCompleted(events(7), suite1.suiteId)
    
    checkSuiteStarting(events(8), suite2.suiteId)
    checkTestStarting(events(9), "Fixture Test 1")
    checkTestSucceeded(events(10), "Fixture Test 1")
    checkTestStarting(events(11), "Fixture Test 2")
    checkTestSucceeded(events(12), "Fixture Test 2")
    checkTestStarting(events(13), "Fixture Test 3")
    checkTestSucceeded(events(14), "Fixture Test 3")
    checkSuiteCompleted(events(15), suite2.suiteId)
  }
}

class ExampleParallelTestExecutionParallelFunSpecPair extends ParallelSuites {
  def suite1 = new ExampleParallelTestExecutionOrderFunSpec
  def suite2 = new ExampleParallelTestExecutionOrderFixtureFunSpec
  
  def assertParallelSuites(events: List[Event]) {
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
    checkTestStarting(events(10), "Scope 2 Test 4")
    checkTestSucceeded(events(11), "Scope 2 Test 4")
    checkScopeClosed(events(12), "Scope 2")
    checkSuiteCompleted(events(13), suite1.suiteId)
    
    checkSuiteStarting(events(14), suite2.suiteId)
    checkScopeOpened(events(15), "Fixture Scope 1")
    checkTestStarting(events(16), "Fixture Scope 1 Fixture Test 1")
    checkTestSucceeded(events(17), "Fixture Scope 1 Fixture Test 1")
    checkTestStarting(events(18), "Fixture Scope 1 Fixture Test 2")
    checkTestSucceeded(events(19), "Fixture Scope 1 Fixture Test 2")
    checkScopeClosed(events(20), "Fixture Scope 1")
    checkScopeOpened(events(21), "Fixture Scope 2")
    checkTestStarting(events(22), "Fixture Scope 2 Fixture Test 3")
    checkTestSucceeded(events(23), "Fixture Scope 2 Fixture Test 3")
    checkTestStarting(events(24), "Fixture Scope 2 Fixture Test 4")
    checkTestSucceeded(events(25), "Fixture Scope 2 Fixture Test 4")
    checkScopeClosed(events(26), "Fixture Scope 2")
    checkSuiteCompleted(events(27), suite2.suiteId)
  }
}

class ExampleParallelTestExecutionParallelFeatureSpecPair extends ParallelSuites {
  def suite1 = new ExampleParallelTestExecutionOrderFeatureSpec
  def suite2 = new ExampleParallelTestExecutionOrderFixtureFeatureSpec
  
  def assertParallelSuites(events: List[Event]) {
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
    checkTestStarting(events(10), "Feature: Scope 2 Scenario: Test 4")
    checkTestSucceeded(events(11), "Feature: Scope 2 Scenario: Test 4")
    checkScopeClosed(events(12), "Feature: Scope 2")
    checkSuiteCompleted(events(13), suite1.suiteId)
    
    checkSuiteStarting(events(14), suite2.suiteId)
    checkScopeOpened(events(15), "Feature: Fixture Scope 1")
    checkTestStarting(events(16), "Feature: Fixture Scope 1 Scenario: Fixture Test 1")
    checkTestSucceeded(events(17), "Feature: Fixture Scope 1 Scenario: Fixture Test 1")
    checkTestStarting(events(18), "Feature: Fixture Scope 1 Scenario: Fixture Test 2")
    checkTestSucceeded(events(19), "Feature: Fixture Scope 1 Scenario: Fixture Test 2")
    checkScopeClosed(events(20), "Feature: Fixture Scope 1")
    checkScopeOpened(events(21), "Feature: Fixture Scope 2")
    checkTestStarting(events(22), "Feature: Fixture Scope 2 Scenario: Fixture Test 3")
    checkTestSucceeded(events(23), "Feature: Fixture Scope 2 Scenario: Fixture Test 3")
    checkTestStarting(events(24), "Feature: Fixture Scope 2 Scenario: Fixture Test 4")
    checkTestSucceeded(events(25), "Feature: Fixture Scope 2 Scenario: Fixture Test 4")
    checkScopeClosed(events(26), "Feature: Fixture Scope 2")
    checkSuiteCompleted(events(27), suite2.suiteId)
  }
}

class ExampleParallelTestExecutionParallelFlatSpecPair extends ParallelSuites {
  def suite1 = new ExampleParallelTestExecutionOrderFlatSpec
  def suite2 = new ExampleParallelTestExecutionOrderFixtureFlatSpec
  
  def assertParallelSuites(events: List[Event]) {
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
    checkTestStarting(events(10), "Scope 2 should Test 4")
    checkTestSucceeded(events(11), "Scope 2 should Test 4")
    checkScopeClosed(events(12), "Scope 2")
    checkSuiteCompleted(events(13), suite1.suiteId)
    
    checkSuiteStarting(events(14), suite2.suiteId)
    checkScopeOpened(events(15), "Fixture Scope 1")
    checkTestStarting(events(16), "Fixture Scope 1 should Fixture Test 1")
    checkTestSucceeded(events(17), "Fixture Scope 1 should Fixture Test 1")
    checkTestStarting(events(18), "Fixture Scope 1 should Fixture Test 2")
    checkTestSucceeded(events(19), "Fixture Scope 1 should Fixture Test 2")
    checkScopeClosed(events(20), "Fixture Scope 1")
    checkScopeOpened(events(21), "Fixture Scope 2")
    checkTestStarting(events(22), "Fixture Scope 2 should Fixture Test 3")
    checkTestSucceeded(events(23), "Fixture Scope 2 should Fixture Test 3")
    checkTestStarting(events(24), "Fixture Scope 2 should Fixture Test 4")
    checkTestSucceeded(events(25), "Fixture Scope 2 should Fixture Test 4")
    checkScopeClosed(events(26), "Fixture Scope 2")
    checkSuiteCompleted(events(27), suite2.suiteId)
  }
}

class ExampleParallelTestExecutionParallelFreeSpecPair extends ParallelSuites {
  def suite1 = new ExampleParallelTestExecutionOrderFreeSpec
  def suite2 = new ExampleParallelTestExecutionOrderFixtureFreeSpec
  
  def assertParallelSuites(events: List[Event]) {
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
    checkTestStarting(events(10), "Scope 2 Test 4")
    checkTestSucceeded(events(11), "Scope 2 Test 4")
    checkScopeClosed(events(12), "Scope 2")
    checkSuiteCompleted(events(13), suite1.suiteId)
    
    checkSuiteStarting(events(14), suite2.suiteId)
    checkScopeOpened(events(15), "Fixture Scope 1")
    checkTestStarting(events(16), "Fixture Scope 1 Fixture Test 1")
    checkTestSucceeded(events(17), "Fixture Scope 1 Fixture Test 1")
    checkTestStarting(events(18), "Fixture Scope 1 Fixture Test 2")
    checkTestSucceeded(events(19), "Fixture Scope 1 Fixture Test 2")
    checkScopeClosed(events(20), "Fixture Scope 1")
    checkScopeOpened(events(21), "Fixture Scope 2")
    checkTestStarting(events(22), "Fixture Scope 2 Fixture Test 3")
    checkTestSucceeded(events(23), "Fixture Scope 2 Fixture Test 3")
    checkTestStarting(events(24), "Fixture Scope 2 Fixture Test 4")
    checkTestSucceeded(events(25), "Fixture Scope 2 Fixture Test 4")
    checkScopeClosed(events(26), "Fixture Scope 2")
    checkSuiteCompleted(events(27), suite2.suiteId)
  }
}

class ExampleParallelTestExecutionParallelPropSpecPair extends ParallelSuites {
  def suite1 = new ExampleParallelTestExecutionOrderPropSpec
  def suite2 = new ExampleParallelTestExecutionOrderFixturePropSpec
  
  def assertParallelSuites(events: List[Event]) {
    assert(events.size === 16)
    
    checkSuiteStarting(events(0), suite1.suiteId)
    checkTestStarting(events(1), "Test 1")
    checkTestSucceeded(events(2), "Test 1")
    checkTestStarting(events(3), "Test 2")
    checkTestSucceeded(events(4), "Test 2")
    checkTestStarting(events(5), "Test 3")
    checkTestSucceeded(events(6), "Test 3")
    checkSuiteCompleted(events(7), suite1.suiteId)
    
    checkSuiteStarting(events(8), suite2.suiteId)
    checkTestStarting(events(9), "Fixture Test 1")
    checkTestSucceeded(events(10), "Fixture Test 1")
    checkTestStarting(events(11), "Fixture Test 2")
    checkTestSucceeded(events(12), "Fixture Test 2")
    checkTestStarting(events(13), "Fixture Test 3")
    checkTestSucceeded(events(14), "Fixture Test 3")
    checkSuiteCompleted(events(15), suite2.suiteId)
  }
}

class ExampleParallelTestExecutionParallelWordSpecPair extends ParallelSuites {
  def suite1 = new ExampleParallelTestExecutionOrderWordSpec
  def suite2 = new ExampleParallelTestExecutionOrderFixtureWordSpec
  
  def assertParallelSuites(events: List[Event]) {
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
    checkTestStarting(events(10), "Scope 2 should Test 4")
    checkTestSucceeded(events(11), "Scope 2 should Test 4")
    checkScopeClosed(events(12), "Scope 2")
    checkSuiteCompleted(events(13), suite1.suiteId)
    
    checkSuiteStarting(events(14), suite2.suiteId)
    checkScopeOpened(events(15), "Fixture Scope 1")
    checkTestStarting(events(16), "Fixture Scope 1 should Fixture Test 1")
    checkTestSucceeded(events(17), "Fixture Scope 1 should Fixture Test 1")
    checkTestStarting(events(18), "Fixture Scope 1 should Fixture Test 2")
    checkTestSucceeded(events(19), "Fixture Scope 1 should Fixture Test 2")
    checkScopeClosed(events(20), "Fixture Scope 1")
    checkScopeOpened(events(21), "Fixture Scope 2")
    checkTestStarting(events(22), "Fixture Scope 2 should Fixture Test 3")
    checkTestSucceeded(events(23), "Fixture Scope 2 should Fixture Test 3")
    checkTestStarting(events(24), "Fixture Scope 2 should Fixture Test 4")
    checkTestSucceeded(events(25), "Fixture Scope 2 should Fixture Test 4")
    checkScopeClosed(events(26), "Fixture Scope 2")
    checkSuiteCompleted(events(27), suite2.suiteId)
  }
}
