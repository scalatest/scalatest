package org.scalatest

import org.scalatest.events.Event
import org.scalatest.events.ScopeOpened
import org.scalatest.events.TestStarting
import org.scalatest.events.TestSucceeded
import org.scalatest.events.ScopeClosed
import org.scalatest.events.InfoProvided
import org.scalatest.events.SuiteStarting
import org.scalatest.events.SuiteCompleted

trait EventHelpers extends Assertions {

  def checkScopeOpened(event: Event, message: String) {
    event match {
      case scopeOpened: ScopeOpened => assert(scopeOpened.message === message)
      case _ => fail("Expected ScopedOpened '" + message + "', but got " + event.getClass.getName)
    }
  }

  def checkScopeClosed(event: Event, message: String) {
    event match {
      case scopeClosed: ScopeClosed => assert(scopeClosed.message === message)
      case _ => fail("Expected ScopedOpened '" + message + "', but got " + event.getClass.getName)
    }
  }

  def checkTestStarting(event: Event, testName: String) {
    event match {
      case testStarting: TestStarting => assert(testStarting.testName === testName)
      case _ => fail("Expected TestStarting '" + testName + "', but got " + event.getClass.getName)
    }
  }

  def checkTestSucceeded(event: Event, testName: String) {
    event match {
      case testSucceeded: TestSucceeded => assert(testSucceeded.testName === testName)
      case _ => fail("Expected TestStarting '" + testName + "', but got " + event.getClass.getName)
    }
  }

  def checkInfoProvided(event: Event, message: String) {
    event match {
      case infoProvided: InfoProvided => assert(infoProvided.message === message)
      case _ => fail("Expected InfoProvided '" + message + "', but got " + event.getClass.getName)
    }
  }

  def checkSuiteStarting(event: Event, suiteId: String) {
    event match {
      case suiteStarting: SuiteStarting => assert(suiteStarting.suiteId === suiteId)
      case _ => fail("Expected SuiteStarting '" + suiteId + "', but got " + event.getClass.getName)
    }
  }

  def checkSuiteCompleted(event: Event, suiteId: String) {
    event match {
      case suiteCompleted: SuiteCompleted => assert(suiteCompleted.suiteId === suiteId)
      case _ => fail("Expected SuiteCompleted '" + suiteId + "', but got " + event.getClass.getName)
    }
  }
}
