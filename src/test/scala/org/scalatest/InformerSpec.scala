/*
 * Copyright 2001-2011 Artima, Inc.
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

class InformerSpec extends FlatSpec {
  "An Informer" should "fire InfoProvided event with correct message and None in payload when using apply(message)" in {
    class MySuite extends FunSuite {
      info("info message")
    }
    val suite = new MySuite()
    val rep = new EventRecordingReporter()
    suite.run(None, Args(rep))
    val infoProvidedEvents = rep.infoProvidedEventsReceived
    assert(infoProvidedEvents.length === 1)
    assert(infoProvidedEvents(0).message === "info message")
    assert(infoProvidedEvents(0).payload === None)
  }
  
  it should "fire InfoProvided event with correct message and payload when using apply(message, payload)" in {
    class MySuite extends FunSuite {
      info("info message", Some("a payload"))
    }
    val suite = new MySuite()
    val rep = new EventRecordingReporter()
    suite.run(None, Args(rep))
    val infoProvidedEvents = rep.infoProvidedEventsReceived
    assert(infoProvidedEvents.length === 1)
    assert(infoProvidedEvents(0).message === "info message")
    assert(infoProvidedEvents(0).payload === Some("a payload"))
  }
}
