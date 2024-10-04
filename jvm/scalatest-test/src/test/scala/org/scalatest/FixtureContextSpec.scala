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
import FailureMessages.decorateToStringValue
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

class FixtureContextSpec extends AnyFunSuite {

  class MyFixtureContext extends FixtureContext

  test("Fixture context objects should work in Async styles in Assertion-result tests") {
    class MyAsyncSpec extends AsyncFlatSpec {
      "A Fixture Context" should "work in an Async style" in new MyFixtureContext {
        assert(1 + 1 == 2)
      }
      it should "work when it fails" in new MyFixtureContext {
        assert(1 + 1 == 3)
      }
    }
    val suite = new MyAsyncSpec()
    val rep = new EventRecordingReporter()
    suite.run(None, Args(rep))
    val testSucceededEvents = rep.testSucceededEventsReceived
    val testFailedEvents = rep.testFailedEventsReceived
    assert(testSucceededEvents.length === 1)
    assert(testSucceededEvents(0).testName.endsWith("work in an Async style"))
    assert(testFailedEvents.length === 1)
    assert(testFailedEvents(0).testName.endsWith("work when it fails"))
  }
}

