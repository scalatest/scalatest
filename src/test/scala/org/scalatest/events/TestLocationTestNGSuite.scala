package org.scalatest.events

import org.testng.annotations.Test
import org.scalatest.testng.TestNGSuite
import org.scalatest.DoNotDiscover

@DoNotDiscover
class TestLocationTestNGSuite extends TestNGSuite with TestLocationServices {
  val suiteTypeName = "org.scalatest.events.TestLocationTestNGSuite"
  val expectedSuiteStartingList = Nil
  val expectedSuiteCompletedList = Nil
  val expectedSuiteAbortedList = Nil
  val expectedTestSucceededList = Nil
  val expectedTestFailedList = List(SeeStackDepthExceptionPair("testFail"))
  val expectedInfoProvidedList = Nil
  
  @Test
  def testFail() { 
    fail
  }
}
