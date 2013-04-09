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
package org.scalatest

import events.InfoProvided

class DeprecatedGivenWhenThenSpec extends FunSpec with SharedHelpers {
  describe("The GivenWhenThen trait") {

    val theGiven = "an invalid zip code"
    val theAnd = "the zip code validator has been initialized"
    val theWhen = "validate is invoked with the invalid zip code"
    val theThen = "the validator should return false"
    class GivenWhenThenInsideTestSpec extends FunSpec with GivenWhenThen {
      it("should do something") {
        given(theGiven)
        and(theAnd)
        when(theWhen)
        then(theThen)
      }
    }
    val spec = new GivenWhenThenInsideTestSpec
    val myRep = new EventRecordingReporter
    spec.run(None, Args(myRep))
    val testSucceeded = myRep.testSucceededEventsReceived
    assert(testSucceeded.size === 1)
    val recordedEvents = testSucceeded(0).recordedEvents
    assert(recordedEvents.size === 4)
    val infoProvidedList = recordedEvents.map(_.asInstanceOf[InfoProvided])

    it("should pass given through to the reporter") {
      assert(infoProvidedList.exists(_.message == "Given " + theGiven))
    }
    it("should pass and through to the reporter") {
      assert(infoProvidedList.exists(_.message == "And " + theAnd))
    }
    it("should pass when through to the reporter") {
      assert(infoProvidedList.exists(_.message == "When " + theWhen))
    }
    it("should pass then through to the reporter") {
      assert(infoProvidedList.exists(_.message == "Then " + theThen))
    }
  }
}
