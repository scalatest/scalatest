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

  def checkScopeOpened(event: Event, message: String): Assertion = {
    event match {
      case scopeOpened: ScopeOpened => assert(scopeOpened.message === message)
      case _ => fail("Expected ScopedOpened '" + message + "', but got " + event.getClass.getName)
    }
  }

  def checkScopeClosed(event: Event, message: String): Assertion = {
    event match {
      case scopeClosed: ScopeClosed => assert(scopeClosed.message === message)
      case _ => fail("Expected ScopedOpened '" + message + "', but got " + event.getClass.getName)
    }
  }

  def checkTestStarting(event: Event, testName: String): Assertion = {
    event match {
      case testStarting: TestStarting => assert(testStarting.testName === testName)
      case _ => fail("Expected TestStarting '" + testName + "', but got " + event.getClass.getName)
    }
  }

  def checkTestSucceeded(event: Event, testName: String): Assertion = {
    event match {
      case testSucceeded: TestSucceeded => assert(testSucceeded.testName === testName)
      case _ => fail("Expected TestStarting '" + testName + "', but got " + event.getClass.getName)
    }
  }

  def checkInfoProvided(event: Event, message: String): Assertion = {
    event match {
      case infoProvided: InfoProvided => assert(infoProvided.message === message)
      case _ => fail("Expected InfoProvided '" + message + "', but got " + event.getClass.getName)
    }
  }

  def checkSuiteStarting(event: Event, suiteId: String): Assertion = {
    event match {
      case suiteStarting: SuiteStarting => assert(suiteStarting.suiteId === suiteId)
      case _ => fail("Expected SuiteStarting '" + suiteId + "', but got " + event.getClass.getName)
    }
  }

  def checkSuiteCompleted(event: Event, suiteId: String): Assertion = {
    event match {
      case suiteCompleted: SuiteCompleted => assert(suiteCompleted.suiteId === suiteId)
      case _ => fail("Expected SuiteCompleted '" + suiteId + "', but got " + event.getClass.getName)
    }
  }
}
