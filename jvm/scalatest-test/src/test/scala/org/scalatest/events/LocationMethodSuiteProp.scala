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
package org.scalatest.events

import org.scalatest._
import SharedHelpers._
import refspec.RefSpec

class LocationMethodSuiteProp extends MethodSuiteProp {

  test("Method suites should have correct TopOfMethod location in test events.") {
    forAll(examples) { suite =>
      val reporter = new EventRecordingReporter
      suite.run(None, Args(reporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker(new Ordinal(99))))
      val eventList = reporter.eventsReceived
      eventList.foreach { event => suite.checkFun(event) }
      suite.allChecked
    }
  }
  
  type FixtureServices = TestLocationMethodServices
  
  def spec = new TestLocationSpec
  class TestLocationSpec extends RefSpec with FixtureServices {
    val suiteTypeName = "org.scalatest.events.LocationMethodSuiteProp$TestLocationSpec"
    val expectedStartingList = List(TestStartingPair("A Spec test succeed", "org.scalatest.events.LocationMethodSuiteProp$TestLocationSpec$A$u0020Spec$", "test$u0020succeed()"), 
                                TestStartingPair("A Spec test pending", "org.scalatest.events.LocationMethodSuiteProp$TestLocationSpec$A$u0020Spec$", "test$u0020pending()"), 
                                TestStartingPair("A Spec test cancel", "org.scalatest.events.LocationMethodSuiteProp$TestLocationSpec$A$u0020Spec$", "test$u0020cancel()"))
    val expectedResultList = List(TestResultPair(classOf[TestSucceeded], "org.scalatest.events.LocationMethodSuiteProp$TestLocationSpec$A$u0020Spec$", "test$u0020succeed()"), 
                              TestResultPair(classOf[TestPending], "org.scalatest.events.LocationMethodSuiteProp$TestLocationSpec$A$u0020Spec$", "test$u0020pending()"),
                              TestResultPair(classOf[TestCanceled], "org.scalatest.events.LocationMethodSuiteProp$TestLocationSpec$A$u0020Spec$", "test$u0020cancel()"),
                              TestResultPair(classOf[TestIgnored], "org.scalatest.events.LocationMethodSuiteProp$TestLocationSpec$A$u0020Spec$", "test$u0020ignore()"))
    val expectedScopeOpenedList = List(ScopeOpenedPair("A Spec", "org.scalatest.events.LocationMethodSuiteProp$TestLocationSpec$A$u0020Spec$"))
    val expectedScopeClosedList = List(ScopeClosedPair("A Spec", "org.scalatest.events.LocationMethodSuiteProp$TestLocationSpec$A$u0020Spec$"))
    
    object `A Spec` {
      def `test succeed`: Unit = {
        
      }
      def `test pending`: Unit = {
        pending
      }
      def `test cancel`: Unit = {
        cancel()
      }
      @Ignore
      def `test ignore`: Unit = {
        
      }
    }
  }
  
  def junit3Suite = new TestLocationMethodJUnit3Suite
  
  def junitSuite = new TestLocationMethodJUnitSuite
  
  def testngSuite = new TestLocationMethodTestNGSuite
}
