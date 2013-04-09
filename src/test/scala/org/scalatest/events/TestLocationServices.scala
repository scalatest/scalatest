package org.scalatest.events

import org.scalatest.Assertions._

trait TestLocationServices {
  private[events] abstract class LocationPair {
    var checked: Boolean
  }
  private[events] case class TopOfClassPair(className: String, var checked: Boolean = false) extends LocationPair
  private[events] case class TopOfMethodPair(className: String, methodId: String, var checked: Boolean = false) extends LocationPair
  private[events] case class SeeStackDepthExceptionPair(name: String, var checked: Boolean = false) extends LocationPair
  private[events] case class LineInFilePair(message: String, fileName: String, lineNumber: Int, var checked: Boolean = false) extends LocationPair
  
  val suiteTypeName: String
  val expectedSuiteStartingList: List[LocationPair]
  val expectedSuiteCompletedList: List[LocationPair]
  val expectedSuiteAbortedList: List[SeeStackDepthExceptionPair]
  val expectedTestFailedList: List[SeeStackDepthExceptionPair]
  val expectedTestSucceededList: List[LocationPair]
  val expectedInfoProvidedList: List[LineInFilePair]
  
  private def checkLocation(expectedList: List[LocationPair], event: Event) {
    event.location match {
      case Some(location) =>
        location match {
          case topOfClass: TopOfClass => 
            expectedList.find { pair =>
              pair match {
                case topOfClassPair: TopOfClassPair => 
                  topOfClassPair.className == topOfClass.className
                case _ => false
              }
            } match {
              case Some(pair) => 
                pair.checked = true
              case None => 
                fail("Suite " + suiteTypeName + " got unexpected TopOfClass(className=" + topOfClass.className + ") location in " + event.getClass.getName)
            }
          case topOfMethod: TopOfMethod => 
            expectedList.find { pair => 
              pair match {
                case topOfMethodPair: TopOfMethodPair => 
                  topOfMethodPair.className == topOfMethod.className && topOfMethodPair.methodId == topOfMethod.methodId
                case _ => false
              }
            } match {
              case Some(pair) =>
                pair.checked = true
              case None => 
                fail("Suite " + suiteTypeName + " got unexpected TopOfMethod(className=" + topOfMethod.className + ", methodId=" + topOfMethod.methodId + ") location in " + event.getClass.getName)
            }
          case lineInFile: LineInFile =>
            expectedList.find { pair => 
              pair match {
                case lineInFilePair: LineInFilePair => 
                  lineInFilePair.fileName == lineInFile.fileName && lineInFilePair.lineNumber == lineInFile.lineNumber
                case _ => false
              } 
            } match {
              case Some(pair) => 
                pair.checked = true
              case None => 
                fail("Suite " + suiteTypeName + " got unexpected LineInFile(fileName=" + lineInFile.fileName + ", lineNumber=" + lineInFile.lineNumber + ") location in " + event.getClass.getName)
            }
          case _ =>
            fail("Suite " + suiteTypeName + " got unexpected " + event.getClass.getName + " with location=" + location)
        }
      case None =>
        fail("Suite " + suiteTypeName + " got unexpected " + event.getClass.getName + ".")
    }
  }
  
  private def checkSeeStackDepthExceptionPair(expectedList: List[SeeStackDepthExceptionPair], expectedName: String, event: Event) {
    val expectedPairOpt: Option[SeeStackDepthExceptionPair] = expectedList.find { pair => pair.name == expectedName }
    expectedPairOpt match {
      case Some(expectedPair) =>
        event.location match {
          case Some(location) =>
            assert(location == SeeStackDepthException, "Suite " + suiteTypeName + "'s " + event.getClass.getName + " event expect to have SeeStackDepthException location, but got " + location.getClass.getName)
            expectedPair.checked = true
          case None => fail("Suite " + suiteTypeName + "'s " + event.getClass.getName + " does not have location (None)")
        }
      case None => fail("Suite " + suiteTypeName + " got unexpected " + expectedName + " for event " + event.getClass.getName)
    }
  }
  
  def checkFun(event: Event) {
    event match {
      case suiteStarting: SuiteStarting => 
        checkLocation(expectedSuiteStartingList, suiteStarting)
      case suiteCompleted: SuiteCompleted => 
        checkLocation(expectedSuiteCompletedList, suiteCompleted)
      case suiteAborted: SuiteAborted => 
        checkSeeStackDepthExceptionPair(expectedSuiteAbortedList, suiteAborted.suiteId, event)
      case testSucceeded: TestSucceeded => 
        checkLocation(expectedTestSucceededList, testSucceeded)
        testSucceeded.recordedEvents.foreach { e => 
          checkLocation(expectedInfoProvidedList, e)
        }
      case testFailed: TestFailed => 
        checkSeeStackDepthExceptionPair(expectedTestFailedList, testFailed.testName, event)
      //case infoProvided: InfoProvided => checkLineInFile(expectedInfoProvidedList, infoProvided.message, event)
      case _ => // Tested in LocationMethodSuiteProp or LocationFunctionSuiteProp
    }
  }
  
  def allChecked = {
    expectedSuiteStartingList.foreach { pair => assert(pair.checked, suiteTypeName + ": SuiteStarting with location " + pair + " not fired.") }
    expectedSuiteCompletedList.foreach { pair => assert(pair.checked, suiteTypeName + ": SuiteCompleted with location " + pair + " not fired.") }
    expectedSuiteAbortedList.foreach { pair => assert(pair.checked, suiteTypeName + ": SuiteAborted for " + pair.name + " not fired.") }
    expectedTestSucceededList.foreach { pair => assert(pair.checked, suiteTypeName + ": TestSucceeded with location " + pair + " not fired.") }
    expectedTestFailedList.foreach { pair => assert(pair.checked, suiteTypeName + ": TestFailed for " + pair.name + " not fired.") }
    expectedInfoProvidedList.foreach { pair => assert(pair.checked, suiteTypeName + ": InfoProvided for " + pair.message + " not fired.") }
  }
}