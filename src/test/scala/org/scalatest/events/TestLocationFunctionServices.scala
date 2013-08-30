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

import org.scalatest.Assertions._

trait TestLocationFunctionServices {
  
  private[events] case class TestStartingPair(testName: String, fileName: String, lineNumber: Int, var checked: Boolean = false)
  private[events] case class TestResultPair(clazz: Class[_], fileName: String, lineNumber: Int, var checked: Boolean = false)
  private[events] case class ScopeOpenedPair(testName: String, fileName: String, lineNumber: Int, var checked: Boolean = false)
  private[events] case class ScopeClosedPair(testName: String, fileName: String, lineNumber: Int, var checked: Boolean = false)
  
  val suiteTypeName: String
  val expectedStartingList: List[TestStartingPair]
  val expectedResultList: List[TestResultPair]
  val expectedScopeOpenedList: List[ScopeOpenedPair]
  val expectedScopeClosedList: List[ScopeClosedPair]
  
  def checkFileNameLineNumber(suiteName:String, expectedFileName: String, expectedLineNumber: Int, event: Event):Boolean = {
    event.location match {
      case Some(evt) =>
        val lineInFile = event.location.get.asInstanceOf[LineInFile]
        assert(expectedFileName == lineInFile.fileName, "Suite " + suiteName + " - " + event + " expected LocationFunctionSuiteProp.scala, got " + lineInFile.fileName)
        assert(expectedLineNumber == lineInFile.lineNumber, "Suite " + suiteName + " - " + event + " expected " + expectedLineNumber + ", got " + lineInFile.lineNumber)
        true
      case None => 
        fail("Suite " + suiteName + " - Event " + event.getClass.getName + " does not have location.")
    }
  }
  
  def checkFun(event: Event) {
    event match {
      case testStarting: TestStarting => 
        val expectedStartingPairOpt = expectedStartingList.find { pair => pair.testName == testStarting.testName }
        expectedStartingPairOpt match {
          case Some(expectedStartingPair) => expectedStartingPair.checked = checkFileNameLineNumber(suiteTypeName, expectedStartingPair.fileName,  expectedStartingPair.lineNumber, event)
          case None => fail("Unknown TestStarting for testName=" + testStarting.testName + " in " + suiteTypeName)
        }
      case scopeOpened: ScopeOpened =>
        val expectedScopeOpenedPairOpt = expectedScopeOpenedList.find { pair => pair.testName == scopeOpened.message }
        expectedScopeOpenedPairOpt match {
          case Some(expectedScopeOpenedPair) => expectedScopeOpenedPair.checked = checkFileNameLineNumber(suiteTypeName, expectedScopeOpenedPair.fileName, expectedScopeOpenedPair.lineNumber, event)
          case None => fail("Unknown ScopeOpened for testName=" + scopeOpened.message + " in " + suiteTypeName)
        }
      case scopeClosed: ScopeClosed =>
        val expectedScopeClosedPairOpt = expectedScopeClosedList.find { pair => pair.testName == scopeClosed.message }
        expectedScopeClosedPairOpt match {
          case Some(expectedScopeClosedPair) => expectedScopeClosedPair.checked = checkFileNameLineNumber(suiteTypeName, expectedScopeClosedPair.fileName, expectedScopeClosedPair.lineNumber, event)
          case None => fail("Unknown ScopeClosed for testName=" + scopeClosed.message + " in " + suiteTypeName)
        }
      case _ =>
        val expectedResultPairOpt = expectedResultList.find { pair => pair.clazz == event.getClass() }
        expectedResultPairOpt match {
          case Some(expectedResultPair) =>
            expectedResultPair.checked = checkFileNameLineNumber(suiteTypeName, expectedResultPair.fileName,  expectedResultPair.lineNumber, event)
          case None => fail("Unexpected event:" + event.getClass.getName + " in " + suiteTypeName)
        }
    }
  }
  def allChecked = {
    expectedStartingList.foreach { pair => assert(pair.checked, suiteTypeName + ": TestStarting for " + pair.testName + " not fired.") }
    expectedResultList.foreach { pair => assert(pair.checked, suiteTypeName + ": " + pair.clazz.getName() + " event not fired.") }
    expectedScopeOpenedList.foreach { pair => assert(pair.checked, suiteTypeName + ": ScopedOpened for " + pair.testName + " not fired.") }
    expectedScopeClosedList.foreach { pair => assert(pair.checked, suiteTypeName + ": ScopedClosed for " + pair.testName + " not fired.") }
  }
}
