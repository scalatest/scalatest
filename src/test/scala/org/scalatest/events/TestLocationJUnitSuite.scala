package org.scalatest.events

import org.scalatest.junit.JUnitSuite
import org.junit.Test
import org.junit.Ignore
import org.scalatest.DoNotDiscover

@DoNotDiscover
class TestLocationJUnitSuite extends JUnitSuite with TestLocationServices {
  val suiteTypeName = "org.scalatest.events.TestLocationJUnitSuite"
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