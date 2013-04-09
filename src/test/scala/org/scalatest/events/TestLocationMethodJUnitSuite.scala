package org.scalatest.events

import org.scalatest.junit.JUnitSuite
import org.junit.Test
import org.junit.Ignore
import org.scalatest.DoNotDiscover

@DoNotDiscover
class TestLocationMethodJUnitSuite extends JUnitSuite with TestLocationMethodServices {
  val suiteTypeName = "org.scalatest.events.TestLocationMethodJUnitSuite"
  val expectedStartingList = List(TestStartingPair("succeed", "org.scalatest.events.TestLocationMethodJUnitSuite", "succeed()"))
  val expectedResultList = List(TestResultPair(classOf[TestSucceeded], "org.scalatest.events.TestLocationMethodJUnitSuite", "succeed()"))
  val expectedScopeOpenedList = Nil
  val expectedScopeClosedList = Nil
  
  @Test
  def succeed() { 
      
  }
  @Ignore 
  def ignore() {
      
  }
}