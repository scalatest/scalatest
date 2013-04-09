package org.scalatest.events

import org.scalatest.Assertions._

trait TestLocationMethodServices {
  
  private[events] case class TestStartingPair(testName: String, className: String, methodName: String, var checked: Boolean = false)
  private[events] case class TestResultPair(clazz: Class[_], className: String, methodName: String, var checked: Boolean = false)
  private[events] case class ScopeOpenedPair(testName: String, className: String, var checked: Boolean = false)
  private[events] case class ScopeClosedPair(testName: String, className: String, var checked: Boolean = false)
  
  val suiteTypeName: String
  val expectedStartingList: List[TestStartingPair]
  val expectedResultList: List[TestResultPair]
  val expectedScopeOpenedList: List[ScopeOpenedPair]
  val expectedScopeClosedList: List[ScopeClosedPair]
  
  private def checkTopOfMethod(className: String, methodName: String, event: Event): Boolean = {
    event.location match {
      case Some(location) => 
        location match {
          case topOfMethod:TopOfMethod => 
            val expectedClassName = className
            val expectedMethodId = "public void " + expectedClassName + "." + methodName
            assert(expectedClassName == topOfMethod.className, "Suite " + suiteTypeName + "'s " + event.getClass.getName + " event's TopOfMethod.className expected to be " + expectedClassName + ", but got " + topOfMethod.className)
            assert(expectedMethodId == topOfMethod.methodId, "Suite " + suiteTypeName + "'s " + event.getClass.getName + " event's TopOfMethod.methodId expected to be " + expectedMethodId + ", but got " + topOfMethod.methodId)
            true
          case _ => fail("Suite " + suiteTypeName + "'s " + event.getClass.getName + " event expect to have TopOfMethod location, but got " + location.getClass.getName)
        }
      case None => fail("Suite " + suiteTypeName + "'s " + event.getClass.getName + " does not have location (None)")
    }
  }
  
  private def checkTopOfClass(className: String, event: Event): Boolean = {
    event.location match {
      case Some(location) => 
        location match {
          case topOfClass: TopOfClass => 
            assert(className == topOfClass.className, "Suite " + suiteTypeName + "'s " + event.getClass.getName + " event's TopOfClass.className expected to be " + className + ", but got " + topOfClass.className)
            true
          case _ => fail("Suite " + suiteTypeName + "'s " + event.getClass.getName + " event expect to have TopOfClass location, but got " + location.getClass.getName)
        }
      case None => fail("Suite " + suiteTypeName + "'s " + event.getClass.getName + " does not have location (None)")
    }
  }
  
  def checkFun(event: Event) {
    event match {
      case testStarting: TestStarting => 
        val expectedStartingPairOpt = expectedStartingList.find { pair => pair.testName == testStarting.testName }
        expectedStartingPairOpt match {
          case Some(expectedStartingPair) => expectedStartingPair.checked = checkTopOfMethod(expectedStartingPair.className, expectedStartingPair.methodName, event)
          case None => fail("Unknown TestStarting for testName=" + testStarting.testName + " in " + suiteTypeName)
        }
      case scopeOpened: ScopeOpened =>
        val expectedScopeOpenedPairOpt = expectedScopeOpenedList.find { pair => pair.testName == scopeOpened.message }
        expectedScopeOpenedPairOpt match {
          case Some(expectedScopeOpenedPair) => expectedScopeOpenedPair.checked = checkTopOfClass(expectedScopeOpenedPair.className, event)
          case None => fail("Unknown ScopeOpened for testName=" + scopeOpened.message + " in " + suiteTypeName)
        }
      case scopeClosed: ScopeClosed =>
        val expectedScopeClosedPairOpt = expectedScopeClosedList.find { pair => pair.testName == scopeClosed.message }
        expectedScopeClosedPairOpt match {
          case Some(expectedScopeClosedPair) => expectedScopeClosedPair.checked = checkTopOfClass(expectedScopeClosedPair.className, event)
          case None => fail("Unknown ScopeClosed for testName=" + scopeClosed.message + " in " + suiteTypeName)
        }
      case suiteStarting: SuiteStarting => // Tested in LocationSuiteProp
      case suiteCompleted: SuiteCompleted => // Tested in LocationSuiteProp
      case _ => 
        val expectedResultPairOpt = expectedResultList.find { pair => pair.clazz == event.getClass() }
        expectedResultPairOpt match {
          case Some(expectedResultPair) => expectedResultPair.checked = checkTopOfMethod(expectedResultPair.className, expectedResultPair.methodName, event)
          case None => fail("Unexpected event:" + event.getClass.getName + " in " + suiteTypeName)
        }
    }
  }
  
  def allChecked = {
    expectedStartingList.foreach { pair => assert(pair.checked, suiteTypeName + ": TestStarting for " + pair.testName + " not fired.") }
    expectedResultList.foreach { pair => assert(pair.checked, suiteTypeName + ": " + pair.clazz.getName() + " event not fired.") }
  }
}