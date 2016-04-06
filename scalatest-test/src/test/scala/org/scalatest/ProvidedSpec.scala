/*
 * Copyright 2001-2016 Artima, Inc.
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

import matchers._
import SharedHelpers._
import FailureMessages.decorateToStringValue
import Matchers._
import events._
import scala.concurrent.Future

class ProvidedSpec extends FunSuite {

  test("info should be allowed as the last statement in Async styles in Assertion-result tests") {
    class MyAsyncSuite extends AsyncFunSuite with GivenWhenThen {
      test("An info should work in an Async style") {
        info("info provided")
      }
      test("A markup should work in an Async style") {
        markup("markup provided")
      }
      test("A Given should work in an Async style") {
        Given("info provided")
      }
      test("A When should work in an Async style") {
        When("info provided")
      }
      test("A Then should work in an Async style") {
        Then("info provided")
      }
      test("An And should work in an Async style") {
        And("info provided")
      }
      test("An alert should work in an Async style") {
        alert("alert provided")
      }
      test("A note should work in an Async style") {
        note("note provided")
      }
    }
    val suite = new MyAsyncSuite()
    val rep = new EventRecordingReporter()
    val status = suite.run(None, Args(rep))
    // SKIP-SCALATESTJS-START
    status.waitUntilCompleted()
    // SKIP-SCALATESTJS-END
    val testSucceededEvents = rep.testSucceededEventsReceived
    val testFailedEvents = rep.testFailedEventsReceived
    val infoProvidedEvents = rep.infoProvidedEventsReceived
    val markupProvidedEvents = rep.markupProvidedEventsReceived
    val alertProvidedEvents = rep.alertProvidedEventsReceived
    val noteProvidedEvents = rep.noteProvidedEventsReceived

    assert(testSucceededEvents.length === 8)
    assert(testSucceededEvents(0).recordedEvents(0).asInstanceOf[InfoProvided].message === "info provided")
    assert(testSucceededEvents(1).recordedEvents(0).asInstanceOf[MarkupProvided].text === "markup provided")
    assert(testSucceededEvents(2).recordedEvents(0).asInstanceOf[InfoProvided].message === "Given info provided")
    assert(testSucceededEvents(3).recordedEvents(0).asInstanceOf[InfoProvided].message === "When info provided")
    assert(testSucceededEvents(4).recordedEvents(0).asInstanceOf[InfoProvided].message === "Then info provided")
    assert(testSucceededEvents(5).recordedEvents(0).asInstanceOf[InfoProvided].message === "And info provided")

    assert(alertProvidedEvents.length === 1)
    assert(alertProvidedEvents(0).message === "alert provided")

    assert(noteProvidedEvents.length === 1)
    assert(noteProvidedEvents(0).message === "note provided")
  }

  test("info should be allowed as the last statement in Async styles in Future[Assertion]-result tests") {
    class MyAsyncSuite extends AsyncFunSuite with GivenWhenThen {
      test("An info should work in an Async style") {
        Future { 1 + 1 } map { sum => assert(sum == 2); info("info provided") }
      }
      test("A markup should work in an Async style") {
        Future { 1 + 1 } map { sum => assert(sum == 2); markup("markup provided") }
      }
      test("A Given should work in an Async style") {
        Future { 1 + 1 } map { sum => assert(sum == 2); Given("info provided") }
      }
      test("A When should work in an Async style") {
        Future { 1 + 1 } map { sum => assert(sum == 2); When("info provided") }
      }
      test("A Then should work in an Async style") {
        Future { 1 + 1 } map { sum => assert(sum == 2); Then("info provided") }
      }
      test("An And should work in an Async style") {
        Future { 1 + 1 } map { sum => assert(sum == 2); And("info provided") }
      }
      test("An alert should work in an Async style") {
        Future { 1 + 1 } map { sum => assert(sum == 2); alert("alert provided") }
      }
      test("A note should work in an Async style") {
        Future { 1 + 1 } map { sum => assert(sum == 2); note("note provided") }
      }
    }
    val suite = new MyAsyncSuite()
    val rep = new EventRecordingReporter()
    val status = suite.run(None, Args(rep))
    // SKIP-SCALATESTJS-START
    status.waitUntilCompleted()
    // SKIP-SCALATESTJS-END
    val testSucceededEvents = rep.testSucceededEventsReceived
    val testFailedEvents = rep.testFailedEventsReceived
    val infoProvidedEvents = rep.infoProvidedEventsReceived
    val markupProvidedEvents = rep.markupProvidedEventsReceived
    val alertProvidedEvents = rep.alertProvidedEventsReceived
    val noteProvidedEvents = rep.noteProvidedEventsReceived

    assert(testSucceededEvents.length === 8)
    assert(testSucceededEvents(0).recordedEvents(0).asInstanceOf[InfoProvided].message === "info provided")
    assert(testSucceededEvents(1).recordedEvents(0).asInstanceOf[MarkupProvided].text === "markup provided")
    assert(testSucceededEvents(2).recordedEvents(0).asInstanceOf[InfoProvided].message === "Given info provided")
    assert(testSucceededEvents(3).recordedEvents(0).asInstanceOf[InfoProvided].message === "When info provided")
    assert(testSucceededEvents(4).recordedEvents(0).asInstanceOf[InfoProvided].message === "Then info provided")
    assert(testSucceededEvents(5).recordedEvents(0).asInstanceOf[InfoProvided].message === "And info provided")

    assert(alertProvidedEvents.length === 1)
    assert(alertProvidedEvents(0).message === "alert provided")

    assert(noteProvidedEvents.length === 1)
    assert(noteProvidedEvents(0).message === "note provided")
  }
}


