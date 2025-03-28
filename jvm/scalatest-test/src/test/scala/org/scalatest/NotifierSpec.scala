/*
 * Copyright 2001-2024 Artima, Inc.
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
import org.scalatest.funsuite.AnyFunSuite

class NotifierSpec extends funspec.AnyFunSpec {
  describe("A Notifier") {
    it("should fire NoteProvided event with correct message and None in payload when using apply(message)") {
      class MySuite extends AnyFunSuite {
        note("update message")
      }
      val suite = new MySuite()
      val rep = new EventRecordingReporter()
      suite.run(None, Args(rep))
      val noteProvidedEvents = rep.noteProvidedEventsReceived
      assert(noteProvidedEvents.length === 1)
      assert(noteProvidedEvents(0).message === "update message")
      assert(noteProvidedEvents(0).payload === None)
    }
    it("should fire NoteProvided event with correct message and payload when using apply(message, payload)") {
      class MySuite extends AnyFunSuite {
        note("update message", Some("a payload"))
      }
      val suite = new MySuite()
      val rep = new EventRecordingReporter()
      suite.run(None, Args(rep))
      val noteProvidedEvents = rep.noteProvidedEventsReceived
      assert(noteProvidedEvents.length === 1)
      assert(noteProvidedEvents(0).message === "update message")
      assert(noteProvidedEvents(0).payload === Some("a payload"))
    }
  }
}

