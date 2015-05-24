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

import SharedHelpers._
import Matchers._

class AlerterSpec extends FunSpec {
  describe("An Alerter") {
    it("should fire AlertProvided event with correct message and None in payload when using apply(message)") {
      class MySuite extends FunSuite {
        alert("alert message")
      }
      val suite = new MySuite()
      val rep = new EventRecordingReporter()
      suite.run(None, Args(rep))
      val alertProvidedEvents = rep.alertProvidedEventsReceived
      assert(alertProvidedEvents.length === 1)
      assert(alertProvidedEvents(0).message === "alert message")
      assert(alertProvidedEvents(0).payload === None)
    }
    it("should fire AlertProvided event with correct message and payload when using apply(message, payload)") {
      class MySuite extends FunSuite {
        alert("alert message", Some("a payload"))
      }
      val suite = new MySuite()
      val rep = new EventRecordingReporter()
      suite.run(None, Args(rep))
      val alertProvidedEvents = rep.alertProvidedEventsReceived
      assert(alertProvidedEvents.length === 1)
      assert(alertProvidedEvents(0).message === "alert message")
      assert(alertProvidedEvents(0).payload === Some("a payload"))
    }
  }
}

