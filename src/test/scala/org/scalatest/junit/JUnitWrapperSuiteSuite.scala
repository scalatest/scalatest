/*
 * Copyright 2001-2008 Artima, Inc.
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
package org.scalatest.junit {

  import org.scalatest._
  import org.scalatest.events._

  class JUnitWrapperSuiteSuite extends FunSuite {

    class MyReporter extends Reporter {

      def apply(event: Event) {
        event match {
          case event: TestStarting =>
            testStartingEvents += event
          case event: TestIgnored =>
            testIgnoredEvent = Some(event)
          case event: TestSucceeded =>
            testSucceededEvents += event
          case event: TestFailed =>
            testFailedEvent = Some(event)
          case _ => 
        }
      }

      var testStartingEvents = Set[TestStarting]()
      var testSucceededEvents = Set[TestSucceeded]()

      var testFailedEvent: Option[TestFailed] = None

      var testIgnoredEvent: Option[TestIgnored] = None
    }

    test("A JUnitWrapperSuite runs a JUnit3 TestCase class successfully") {
      val jRap =
        new JUnitWrapperSuite("org.scalatest.junit.JUnit3TestCase",
                              this.getClass.getClassLoader)
      val repA = new MyReporter

      jRap.run(None, Args(repA))

      //
      // verify one of the TestStarting events
      //
      val startingEventsTestA =
        repA.testSucceededEvents.filter(_.testName == "testA")

      assert(startingEventsTestA.size === 1)

      val startingEventTestA = startingEventsTestA.toArray.apply(0) // For 2.8

      assert(startingEventTestA.suiteName === "JUnit3TestCase")
      assert(startingEventTestA.suiteClassName.get ===
             "org.scalatest.junit.JUnit3TestCase")
      assert(repA.testStartingEvents.size === 3)

      //
      // verify one of the TestSucceeded events
      //
      val successEventsTestA =
        repA.testSucceededEvents.filter(_.testName == "testA")

      assert(successEventsTestA.size === 1)

      val successEventTestA = successEventsTestA.toArray.apply(0) // For 2.8

      assert(successEventTestA.suiteName === "JUnit3TestCase")
      assert(successEventTestA.suiteClassName.get ===
             "org.scalatest.junit.JUnit3TestCase")
      assert(repA.testSucceededEvents.size === 2)

      //
      // verify a TestFailed event
      //
      assert(repA.testFailedEvent.isDefined)
      assert(repA.testFailedEvent.get.testName === "testC")
      assert(repA.testFailedEvent.get.suiteName === "JUnit3TestCase")
      assert(repA.testFailedEvent.get.suiteClassName.get ===
             "org.scalatest.junit.JUnit3TestCase")
    }

    test("A JUnitWrapperSuite runs a JUnit4 class successfully") {
      val jRap =
        new JUnitWrapperSuite("org.scalatest.junit.JHappySuite",
                              this.getClass.getClassLoader)
      val repA = new MyReporter

      jRap.run(None, Args(repA))

      //
      // verify the TestStarting event
      //
      assert(repA.testStartingEvents.size === 1)

      val startingEvent = repA.testStartingEvents.toArray.apply(0) // For 2.8

      assert(startingEvent.testName === "verifySomething")
      assert(startingEvent.suiteName === "JHappySuite")
      assert(startingEvent.suiteClassName.get ===
             "org.scalatest.junit.JHappySuite")

      //
      // verify the TestSucceeded event
      //
      assert(repA.testSucceededEvents.size === 1)

      val succeededEvent = repA.testSucceededEvents.toArray.apply(0) // For 2.8

      assert(succeededEvent.testName === "verifySomething")
      assert(succeededEvent.suiteName === "JHappySuite")
      assert(succeededEvent.suiteClassName.get ===
             "org.scalatest.junit.JHappySuite")

    }

    test("A JUnitWrapperSuite runs a failing JUnit4 class successfully") {
      val jRap =
        new JUnitWrapperSuite("org.scalatest.junit.JBitterSuite",
                              this.getClass.getClassLoader)
      val repA = new MyReporter

      jRap.run(None, Args(repA))

      //
      // verify the TestStarting event
      //
      assert(repA.testStartingEvents.size === 1)

      val startingEvent = repA.testStartingEvents.toArray.apply(0) // For 2.8

      assert(startingEvent.testName === "verifySomething")
      assert(startingEvent.suiteName === "JBitterSuite")
      assert(startingEvent.suiteClassName.get ===
             "org.scalatest.junit.JBitterSuite")

      //
      // verify a TestFailed event
      //
      assert(repA.testFailedEvent.isDefined)
      assert(repA.testFailedEvent.get.testName === "verifySomething")
      assert(repA.testFailedEvent.get.suiteName === "JBitterSuite")
      assert(repA.testFailedEvent.get.suiteClassName.get ===
             "org.scalatest.junit.JBitterSuite")
      assert(repA.testSucceededEvents.size === 0)
    }

    test("A JUnitWrapperSuite runs a JUnit3 TestSuite class successfully") {
      val jRap = new JUnitWrapperSuite("org.scalatest.junit.JUnit3TestSuite",
                                       this.getClass.getClassLoader)
      val repA = new MyReporter

      jRap.run(None, Args(repA))

      //
      // verify one of the TestStarting events
      //
      val startingEventsTestB =
        repA.testStartingEvents.filter(_.testName == "testB")

      assert(startingEventsTestB.size === 1)

      val startingEventTestB = startingEventsTestB.toArray.apply(0) // For 2.8

      assert(startingEventTestB.testName === "testB")
      assert(startingEventTestB.suiteName === "JUnit3TestCase")
      assert(startingEventTestB.suiteClassName.get ===
             "org.scalatest.junit.JUnit3TestCase")
      assert(repA.testStartingEvents.size === 2)

      //
      // verify one of the TestSucceeded events
      //
      val successEventsTestB =
        repA.testSucceededEvents.filter(_.testName == "testB")

      assert(successEventsTestB.size === 1)

      val successEventTestB = successEventsTestB.toArray.apply(0) // For 2.8

      assert(successEventTestB.suiteName === "JUnit3TestCase")
      assert(successEventTestB.suiteClassName.get ===
             "org.scalatest.junit.JUnit3TestCase")
      assert(repA.testSucceededEvents.size === 2)
    }

  }
}
