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
package org.scalatest.events
import org.scalatest._
import SharedHelpers._

class LocationFunctionSuiteProp extends FunctionSuiteProp {
  
  test("Function suites should have correct LineInFile location in test events.") {
    forAll(examples) { suite =>
      val reporter = new EventRecordingReporter
      suite.run(None, Args(reporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker(new Ordinal(99)), Set.empty))
      val eventList = reporter.eventsReceived
      eventList.foreach { event => suite.checkFun(event) }
      suite.allChecked
      succeed
    }
  }
  
  type FixtureServices = TestLocationFunctionServices
  
  val expectedSourceFileName: String = "LocationFunctionSuiteProp.scala"
  
  def funSuite = new FunSuite with FixtureServices {
    test("succeed") {
      succeed
    }
    test("pending") {
      pending
    }
    test("cancel") {
      cancel
    }
    ignore("ignore") {
      succeed
    }
    val suiteTypeName: String = "FunSuite"
    val expectedStartingList = List(TestStartingPair("succeed", expectedSourceFileName, thisLineNumber - 13), 
                                   TestStartingPair("pending", expectedSourceFileName, thisLineNumber - 11),
                                   TestStartingPair("cancel", expectedSourceFileName, thisLineNumber - 9))
    val expectedResultList = List(TestResultPair(classOf[TestSucceeded], expectedSourceFileName, thisLineNumber - 16), 
                                 TestResultPair(classOf[TestPending], expectedSourceFileName, thisLineNumber - 14),
                                 TestResultPair(classOf[TestCanceled], expectedSourceFileName, thisLineNumber - 12),
                                 TestResultPair(classOf[TestIgnored], expectedSourceFileName, thisLineNumber - 10))
    val expectedScopeOpenedList = Nil
    val expectedScopeClosedList = Nil
  }
  
  def fixtureFunSuite = new fixture.FunSuite with FixtureServices with StringFixture {
    test("succeed") { param =>
      succeed
    }
    test("pending") { param =>
      pending
    }
    test("cancel") { param =>
      cancel
    }
    ignore("ignore") { param =>
      succeed
    }
    val suiteTypeName: String = "FunSuite"
    val expectedStartingList = List(TestStartingPair("succeed", expectedSourceFileName, thisLineNumber - 13), 
                                   TestStartingPair("pending", expectedSourceFileName, thisLineNumber - 11),
                                   TestStartingPair("cancel", expectedSourceFileName, thisLineNumber - 9))
    val expectedResultList = List(TestResultPair(classOf[TestSucceeded], expectedSourceFileName, thisLineNumber - 16), 
                                 TestResultPair(classOf[TestPending], expectedSourceFileName, thisLineNumber - 14),
                                 TestResultPair(classOf[TestCanceled], expectedSourceFileName, thisLineNumber - 12),
                                 TestResultPair(classOf[TestIgnored], expectedSourceFileName, thisLineNumber - 10))
    val expectedScopeOpenedList = Nil
    val expectedScopeClosedList = Nil
  }
  
  def funSpec = new FunSpec with FixtureServices {
    describe("A Spec") {
      it("succeed") {
        succeed
      }
      it("pending") {
        pending
      }
      it("cancel") {
        cancel
      }
      ignore("ignore") {
        succeed
      }
    }
    val suiteTypeName: String = "FunSpec"
    val expectedStartingList = List(TestStartingPair("A Spec succeed", expectedSourceFileName, thisLineNumber - 14), 
                                   TestStartingPair("A Spec pending", expectedSourceFileName, thisLineNumber - 12),
                                   TestStartingPair("A Spec cancel", expectedSourceFileName, thisLineNumber - 10))
    val expectedResultList = List(TestResultPair(classOf[TestSucceeded], expectedSourceFileName, thisLineNumber - 17), 
                                 TestResultPair(classOf[TestPending], expectedSourceFileName, thisLineNumber - 15),
                                 TestResultPair(classOf[TestCanceled], expectedSourceFileName, thisLineNumber - 13),
                                 TestResultPair(classOf[TestIgnored], expectedSourceFileName, thisLineNumber - 11))
    val expectedScopeOpenedList = List(ScopeOpenedPair("A Spec", expectedSourceFileName, thisLineNumber - 22))
    val expectedScopeClosedList = List(ScopeClosedPair("A Spec", expectedSourceFileName, thisLineNumber - 23))
  }
  
  def fixtureFunSpec = new fixture.FunSpec with FixtureServices with StringFixture {
    describe("A Spec") {
      it("succeed") { param =>
        succeed
      }
      it("pending") { param =>
        pending
      }
      it("cancel") { param =>
        cancel
      }
      ignore("ignore") { param =>
        succeed
      }
    }
    val suiteTypeName: String = "FixtureFunSpec"
    val expectedStartingList = List(TestStartingPair("A Spec succeed", expectedSourceFileName, thisLineNumber - 14), 
                                   TestStartingPair("A Spec pending", expectedSourceFileName, thisLineNumber - 12),
                                   TestStartingPair("A Spec cancel", expectedSourceFileName, thisLineNumber - 10))
    val expectedResultList = List(TestResultPair(classOf[TestSucceeded], expectedSourceFileName, thisLineNumber - 17), 
                                 TestResultPair(classOf[TestPending], expectedSourceFileName, thisLineNumber - 15),
                                 TestResultPair(classOf[TestCanceled], expectedSourceFileName, thisLineNumber - 13),
                                 TestResultPair(classOf[TestIgnored], expectedSourceFileName, thisLineNumber - 11))
    val expectedScopeOpenedList = List(ScopeOpenedPair("A Spec", expectedSourceFileName, thisLineNumber - 22))
    val expectedScopeClosedList = List(ScopeClosedPair("A Spec", expectedSourceFileName, thisLineNumber - 23))
  }
  
  def featureSpec = new FeatureSpec with FixtureServices {
    feature("Test") {
      scenario("succeed") {
        succeed
      }
      scenario("pending") {
        pending
      }
      scenario("cancel") {
        cancel
      }
      ignore("ignore") {
        succeed
      }
    }
    val suiteTypeName: String = "FeatureSpec"
    val expectedStartingList = List(TestStartingPair("Feature: Test Scenario: succeed", expectedSourceFileName, thisLineNumber - 14), 
                                   TestStartingPair("Feature: Test Scenario: pending", expectedSourceFileName, thisLineNumber - 12),
                                   TestStartingPair("Feature: Test Scenario: cancel", expectedSourceFileName, thisLineNumber - 10))
    val expectedResultList = List(TestResultPair(classOf[TestSucceeded], expectedSourceFileName, thisLineNumber - 17), 
                                 TestResultPair(classOf[TestPending], expectedSourceFileName, thisLineNumber - 15),
                                 TestResultPair(classOf[TestCanceled], expectedSourceFileName, thisLineNumber - 13),
                                 TestResultPair(classOf[TestIgnored], expectedSourceFileName, thisLineNumber - 11))
    val expectedScopeOpenedList = List(ScopeOpenedPair("Feature: Test", expectedSourceFileName, thisLineNumber - 22))
    val expectedScopeClosedList = List(ScopeClosedPair("Feature: Test", expectedSourceFileName, thisLineNumber - 23))
  }
  
  def fixtureFeatureSpec = new fixture.FeatureSpec with FixtureServices with StringFixture {
    feature("Test") {
      scenario("succeed") { param =>
        succeed
      }
      scenario("pending") { param =>
        pending
      }
      scenario("cancel") { param =>
        cancel
      }
      ignore("ignore") { param =>
        succeed
      }
    }
    val suiteTypeName: String = "FixtureFeatureSpec"
    val expectedStartingList = List(TestStartingPair("Feature: Test Scenario: succeed", expectedSourceFileName, thisLineNumber - 14), 
                                   TestStartingPair("Feature: Test Scenario: pending", expectedSourceFileName, thisLineNumber - 12),
                                   TestStartingPair("Feature: Test Scenario: cancel", expectedSourceFileName, thisLineNumber - 10))
    val expectedResultList = List(TestResultPair(classOf[TestSucceeded], expectedSourceFileName, thisLineNumber - 17), 
                                 TestResultPair(classOf[TestPending], expectedSourceFileName, thisLineNumber - 15),
                                 TestResultPair(classOf[TestCanceled], expectedSourceFileName, thisLineNumber - 13),
                                 TestResultPair(classOf[TestIgnored], expectedSourceFileName, thisLineNumber - 11))
    val expectedScopeOpenedList = List(ScopeOpenedPair("Feature: Test", expectedSourceFileName, thisLineNumber - 22))
    val expectedScopeClosedList = List(ScopeClosedPair("Feature: Test", expectedSourceFileName, thisLineNumber - 23))
  }
  
  def flatSpec = new FlatSpec with FixtureServices {
    "Test 1" should "succeed" in {
      succeed
    }
    "Test 2" should "pending" in {
      pending
    }
    "Test 3" should "cancel" in {
      cancel
    }
    behavior of "Test 4"
    it should "be ignored" ignore {
      succeed
    }
    val suiteTypeName: String = "FlatSpec"
    val expectedStartingList = List(TestStartingPair("Test 1 should succeed", expectedSourceFileName, thisLineNumber - 14), 
                                   TestStartingPair("Test 2 should pending", expectedSourceFileName, thisLineNumber - 12),
                                   TestStartingPair("Test 3 should cancel", expectedSourceFileName, thisLineNumber - 10))
    val expectedResultList = List(TestResultPair(classOf[TestSucceeded], expectedSourceFileName, thisLineNumber - 17), 
                                 TestResultPair(classOf[TestPending], expectedSourceFileName, thisLineNumber - 15),
                                 TestResultPair(classOf[TestCanceled], expectedSourceFileName, thisLineNumber - 13),
                                 TestResultPair(classOf[TestIgnored], expectedSourceFileName, thisLineNumber - 10))
    val expectedScopeOpenedList = List(ScopeOpenedPair("Test 1", expectedSourceFileName, thisLineNumber - 21), 
                                       ScopeOpenedPair("Test 2", expectedSourceFileName, thisLineNumber - 19),
                                       ScopeOpenedPair("Test 3", expectedSourceFileName, thisLineNumber - 17),
                                       ScopeOpenedPair("Test 4", expectedSourceFileName, thisLineNumber - 15))
    val expectedScopeClosedList = List(ScopeClosedPair("Test 1", expectedSourceFileName, thisLineNumber - 25),
                                       ScopeClosedPair("Test 2", expectedSourceFileName, thisLineNumber - 23),
                                       ScopeClosedPair("Test 3", expectedSourceFileName, thisLineNumber - 21),
                                       ScopeClosedPair("Test 4", expectedSourceFileName, thisLineNumber - 19))
  }
  
  def fixtureFlatSpec = new fixture.FlatSpec with FixtureServices with StringFixture {
    "Test 1" should "succeed" in { param =>
      succeed
    }
    "Test 2" should "pending" in { param =>
      pending
    }
    "Test 3" should "cancel" in { param =>
      cancel
    }
    behavior of "Test 4"
    it should "be ignored" ignore { param =>
      succeed
    }
    val suiteTypeName: String = "FixtureFlatSpec"
    val expectedStartingList = List(TestStartingPair("Test 1 should succeed", expectedSourceFileName, thisLineNumber - 14), 
                                   TestStartingPair("Test 2 should pending", expectedSourceFileName, thisLineNumber - 12),
                                   TestStartingPair("Test 3 should cancel", expectedSourceFileName, thisLineNumber - 10))
    val expectedResultList = List(TestResultPair(classOf[TestSucceeded], expectedSourceFileName, thisLineNumber - 17), 
                                 TestResultPair(classOf[TestPending], expectedSourceFileName, thisLineNumber - 15),
                                 TestResultPair(classOf[TestCanceled], expectedSourceFileName, thisLineNumber - 13),
                                 TestResultPair(classOf[TestIgnored], expectedSourceFileName, thisLineNumber - 10))
    val expectedScopeOpenedList = List(ScopeOpenedPair("Test 1", expectedSourceFileName, thisLineNumber - 21), 
                                       ScopeOpenedPair("Test 2", expectedSourceFileName, thisLineNumber - 19),
                                       ScopeOpenedPair("Test 3", expectedSourceFileName, thisLineNumber - 17),
                                       ScopeOpenedPair("Test 4", expectedSourceFileName, thisLineNumber - 15))
    val expectedScopeClosedList = List(ScopeClosedPair("Test 1", expectedSourceFileName, thisLineNumber - 25),
                                       ScopeClosedPair("Test 2", expectedSourceFileName, thisLineNumber - 23),
                                       ScopeClosedPair("Test 3", expectedSourceFileName, thisLineNumber - 21),
                                       ScopeClosedPair("Test 4", expectedSourceFileName, thisLineNumber - 19))
  }
  
  def freeSpec = new FreeSpec with FixtureServices {
    "Test" - {
      "should succeed" in {
        succeed
      }
      "should pending" in {
        pending
      }
      "should cancel" in {
        cancel
      }
      "should ignore" ignore {
        succeed
      }
    }
    val suiteTypeName: String = "FreeSpec"
    val expectedStartingList = List(TestStartingPair("Test should succeed", expectedSourceFileName, thisLineNumber - 14), 
                                   TestStartingPair("Test should pending", expectedSourceFileName, thisLineNumber - 12),
                                   TestStartingPair("Test should cancel", expectedSourceFileName, thisLineNumber - 10))
    val expectedResultList = List(TestResultPair(classOf[TestSucceeded], expectedSourceFileName, thisLineNumber - 17), 
                                 TestResultPair(classOf[TestPending], expectedSourceFileName, thisLineNumber - 15),
                                 TestResultPair(classOf[TestCanceled], expectedSourceFileName, thisLineNumber - 13),
                                 TestResultPair(classOf[TestIgnored], expectedSourceFileName, thisLineNumber - 11))
    val expectedScopeOpenedList = List(ScopeOpenedPair("Test", expectedSourceFileName, thisLineNumber - 22))
    val expectedScopeClosedList = List(ScopeClosedPair("Test", expectedSourceFileName, thisLineNumber - 23))
  }
  
  def fixtureFreeSpec = new fixture.FreeSpec with FixtureServices with StringFixture {
    "Test" - {
      "should succeed" in { param =>
        succeed
      }
      "should pending" in { param =>
        pending
      }
      "should cancel" in { param =>
        cancel
      }
      "should ignore" ignore { param =>
        succeed
      }
    }
    val suiteTypeName: String = "FixtureFreeSpec"
    val expectedStartingList = List(TestStartingPair("Test should succeed", expectedSourceFileName, thisLineNumber - 14), 
                                   TestStartingPair("Test should pending", expectedSourceFileName, thisLineNumber - 12),
                                   TestStartingPair("Test should cancel", expectedSourceFileName, thisLineNumber - 10))
    val expectedResultList = List(TestResultPair(classOf[TestSucceeded], expectedSourceFileName, thisLineNumber - 17), 
                                 TestResultPair(classOf[TestPending], expectedSourceFileName, thisLineNumber - 15),
                                 TestResultPair(classOf[TestCanceled], expectedSourceFileName, thisLineNumber - 13),
                                 TestResultPair(classOf[TestIgnored], expectedSourceFileName, thisLineNumber - 11))
    val expectedScopeOpenedList = List(ScopeOpenedPair("Test", expectedSourceFileName, thisLineNumber - 22))
    val expectedScopeClosedList = List(ScopeClosedPair("Test", expectedSourceFileName, thisLineNumber - 23))
  }
  
  def propSpec = new PropSpec with FixtureServices {
    property("Test should succeed") {
      succeed
    }
    property("Test should pending") {
      pending
    }
    property("Test should cancel") {
      cancel
    }
    ignore("Test should ignore") {
      succeed
    }
    val suiteTypeName: String = "PropSpec"
    val expectedStartingList = List(TestStartingPair("Test should succeed", expectedSourceFileName, thisLineNumber - 13), 
                                   TestStartingPair("Test should pending", expectedSourceFileName, thisLineNumber - 11),
                                   TestStartingPair("Test should cancel", expectedSourceFileName, thisLineNumber - 9))
    val expectedResultList = List(TestResultPair(classOf[TestSucceeded], expectedSourceFileName, thisLineNumber - 16), 
                                 TestResultPair(classOf[TestPending], expectedSourceFileName, thisLineNumber - 14),
                                 TestResultPair(classOf[TestCanceled], expectedSourceFileName, thisLineNumber - 12),
                                 TestResultPair(classOf[TestIgnored], expectedSourceFileName, thisLineNumber - 10))
    val expectedScopeOpenedList = Nil
    val expectedScopeClosedList = Nil
  }
  
  def fixturePropSpec = new fixture.PropSpec with FixtureServices with StringFixture {
    property("Test should succeed") { param =>
      succeed
    }
    property("Test should pending") { param =>
      pending
    }
    property("Test should cancel") { param =>
      cancel
    }
    ignore("Test should ignore") { param =>
      succeed
    }
    val suiteTypeName: String = "FixturePropSpec"
    val expectedStartingList = List(TestStartingPair("Test should succeed", expectedSourceFileName, thisLineNumber - 13), 
                                   TestStartingPair("Test should pending", expectedSourceFileName, thisLineNumber - 11),
                                   TestStartingPair("Test should cancel", expectedSourceFileName, thisLineNumber - 9))
    val expectedResultList = List(TestResultPair(classOf[TestSucceeded], expectedSourceFileName, thisLineNumber - 16), 
                                 TestResultPair(classOf[TestPending], expectedSourceFileName, thisLineNumber - 14),
                                 TestResultPair(classOf[TestCanceled], expectedSourceFileName, thisLineNumber - 12),
                                 TestResultPair(classOf[TestIgnored], expectedSourceFileName, thisLineNumber - 10))
    val expectedScopeOpenedList = Nil
    val expectedScopeClosedList = Nil
  }
  
  def wordSpec = new WordSpec with FixtureServices {
    def provide = afterWord("provide")
    "Test 1" should provide {
      "succeed" in {
        succeed
      }
    }
    "Test 2" when {
      "pending" in {
        pending
      }
    }
    "Test 3" which {
      "cancel" in {
        cancel
      }
    }
    "Test 4" that {
      "ignore " ignore {
        succeed
      }
    }
    
    val suiteTypeName: String = "WordSpec"
    val expectedStartingList = List(TestStartingPair("Test 1 should provide succeed", expectedSourceFileName, thisLineNumber - 21), 
                                   TestStartingPair("Test 2 when pending", expectedSourceFileName, thisLineNumber - 17),
                                   TestStartingPair("Test 3 which cancel", expectedSourceFileName, thisLineNumber - 13))
    val expectedResultList = List(TestResultPair(classOf[TestSucceeded], expectedSourceFileName, thisLineNumber - 24), 
                                 TestResultPair(classOf[TestPending], expectedSourceFileName, thisLineNumber - 20),
                                 TestResultPair(classOf[TestCanceled], expectedSourceFileName, thisLineNumber - 16),
                                 TestResultPair(classOf[TestIgnored], expectedSourceFileName, thisLineNumber - 12))
    val expectedScopeOpenedList = List(ScopeOpenedPair("Test 1", expectedSourceFileName, thisLineNumber - 29), 
                                       ScopeOpenedPair("should provide", expectedSourceFileName, thisLineNumber - 30), 
                                       ScopeOpenedPair("Test 2", expectedSourceFileName, thisLineNumber - 26),
                                       ScopeOpenedPair("Test 3 which", expectedSourceFileName, thisLineNumber - 22),
                                       ScopeOpenedPair("Test 4 that", expectedSourceFileName, thisLineNumber - 18))
    val expectedScopeClosedList = List(ScopeClosedPair("Test 1", expectedSourceFileName, thisLineNumber - 34), 
                                       ScopeClosedPair("should provide", expectedSourceFileName, thisLineNumber - 35), 
                                       ScopeClosedPair("Test 2", expectedSourceFileName, thisLineNumber - 31),
                                       ScopeClosedPair("Test 3 which", expectedSourceFileName, thisLineNumber - 27),
                                       ScopeClosedPair("Test 4 that", expectedSourceFileName, thisLineNumber - 23))
  }
  
  def fixtureWordSpec = new fixture.WordSpec with FixtureServices with StringFixture {
    def provide = afterWord("provide")
    "Test 1" should provide {
      "succeed" in { param =>
        succeed
      }
    }
    "Test 2" when {
      "pending" in { param =>
        pending
      }
    }
    "Test 3" which {
      "cancel" in { param =>
        cancel
      }
    }
    "Test 4" that {
      "ignore " ignore { param =>
        succeed
      }
    }
    
    val suiteTypeName: String = "FixtureWordSpec"
    val expectedStartingList = List(TestStartingPair("Test 1 should provide succeed", expectedSourceFileName, thisLineNumber - 21), 
                                   TestStartingPair("Test 2 when pending", expectedSourceFileName, thisLineNumber - 17),
                                   TestStartingPair("Test 3 which cancel", expectedSourceFileName, thisLineNumber - 13))
    val expectedResultList = List(TestResultPair(classOf[TestSucceeded], expectedSourceFileName, thisLineNumber - 24), 
                                 TestResultPair(classOf[TestPending], expectedSourceFileName, thisLineNumber - 20),
                                 TestResultPair(classOf[TestCanceled], expectedSourceFileName, thisLineNumber - 16),
                                 TestResultPair(classOf[TestIgnored], expectedSourceFileName, thisLineNumber - 12))
    val expectedScopeOpenedList = List(ScopeOpenedPair("Test 1", expectedSourceFileName, thisLineNumber - 29), 
                                       ScopeOpenedPair("should provide", expectedSourceFileName, thisLineNumber - 30), 
                                       ScopeOpenedPair("Test 2", expectedSourceFileName, thisLineNumber - 26),
                                       ScopeOpenedPair("Test 3 which", expectedSourceFileName, thisLineNumber - 22),
                                       ScopeOpenedPair("Test 4 that", expectedSourceFileName, thisLineNumber - 18))
    val expectedScopeClosedList = List(ScopeClosedPair("Test 1", expectedSourceFileName, thisLineNumber - 34), 
                                       ScopeClosedPair("should provide", expectedSourceFileName, thisLineNumber - 35), 
                                       ScopeClosedPair("Test 2", expectedSourceFileName, thisLineNumber - 31),
                                       ScopeClosedPair("Test 3 which", expectedSourceFileName, thisLineNumber - 27),
                                       ScopeClosedPair("Test 4 that", expectedSourceFileName, thisLineNumber - 23))
  }
  
  def pathFreeSpec = new TestLocationFunctionPathFreeSpec
  
  def pathFunSpec = new TestLocationFunctionPathFunSpec
}

class TestLocationFunctionPathFreeSpec extends path.FreeSpec with TestLocationFunctionServices {
  val expectedSourceFileName = "LocationFunctionSuiteProp.scala"
  //SCALATESTJS-ONLY override def newInstance = new TestLocationFunctionPathFreeSpec
  "Test" - {
    "should succeed" in {
      succeed
    }
    "should pending" in {
      pending
    }
    "should cancel" in {
      cancel
    }
    "should ignore" ignore {
      succeed
    }
  }
  val suiteTypeName: String = "path.FreeSpec"
  val expectedStartingList = List(TestStartingPair("Test should succeed", expectedSourceFileName, thisLineNumber - 14), 
                                  TestStartingPair("Test should pending", expectedSourceFileName, thisLineNumber - 12),
                                  TestStartingPair("Test should cancel", expectedSourceFileName, thisLineNumber - 10))
  val expectedResultList = List(TestResultPair(classOf[TestSucceeded], expectedSourceFileName, thisLineNumber - 17), 
                                TestResultPair(classOf[TestPending], expectedSourceFileName, thisLineNumber - 15),
                                TestResultPair(classOf[TestCanceled], expectedSourceFileName, thisLineNumber - 13),
                                TestResultPair(classOf[TestIgnored], expectedSourceFileName, thisLineNumber - 11))
  val expectedScopeOpenedList = List(ScopeOpenedPair("Test", expectedSourceFileName, thisLineNumber - 22))
  val expectedScopeClosedList = List(ScopeClosedPair("Test", expectedSourceFileName, thisLineNumber - 23))
}

class TestLocationFunctionPathFunSpec extends path.FunSpec with TestLocationFunctionServices {
  val expectedSourceFileName = "LocationFunctionSuiteProp.scala"
  //SCALATESTJS-ONLY override def newInstance = new TestLocationFunctionPathFunSpec
  describe("A Spec") {
    it("succeed") {
      succeed
    }
    it("pending") {
      pending
    }
    it("cancel") {
      cancel
    }
    ignore("ignore") {
      succeed
    }
  }
  val suiteTypeName: String = "path.FunSpec"
  val expectedStartingList = List(TestStartingPair("A Spec succeed", expectedSourceFileName, thisLineNumber - 14), 
                                  TestStartingPair("A Spec pending", expectedSourceFileName, thisLineNumber - 12),
                                  TestStartingPair("A Spec cancel", expectedSourceFileName, thisLineNumber - 10))
  val expectedResultList = List(TestResultPair(classOf[TestSucceeded], expectedSourceFileName, thisLineNumber - 17), 
                                TestResultPair(classOf[TestPending], expectedSourceFileName, thisLineNumber - 15),
                                TestResultPair(classOf[TestCanceled], expectedSourceFileName, thisLineNumber - 13),
                                TestResultPair(classOf[TestIgnored], expectedSourceFileName, thisLineNumber - 11))
  val expectedScopeOpenedList = List(ScopeOpenedPair("A Spec", expectedSourceFileName, thisLineNumber - 22))
  val expectedScopeClosedList = List(ScopeClosedPair("A Spec", expectedSourceFileName, thisLineNumber - 23))
}
