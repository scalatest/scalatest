package org.scalatest.events

import org.scalatest.junit.JUnit3Suite
import org.scalatest.DoNotDiscover

@DoNotDiscover
class TestLocationJUnit3Suite extends JUnit3Suite with TestLocationServices {
  val suiteTypeName = "org.scalatest.events.TestLocationJUnit3Suite"
  val expectedSuiteStartingList = Nil
  val expectedSuiteCompletedList = Nil
  val expectedSuiteAbortedList = Nil
  val expectedTestSucceededList = Nil
  val expectedTestFailedList = List(SeeStackDepthExceptionPair("testFail(org.scalatest.events.TestLocationJUnit3Suite)"))
  val expectedInfoProvidedList = Nil
  
  def testFail() { fail }
}