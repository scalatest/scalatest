/*
 * Copyright 2001-2015 Artima, Inc.
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

import org.scalactic.PrettyMethods
import org.scalatest.exceptions.TestFailedException
import SharedHelpers.thisLineNumber
import Fact._

class FactSpec extends FreeSpec with Matchers with PrettyMethods with ExpectationHavePropertyMatchers {
  "A Fact" - {
    val falseFact = False("1 did not equal 2", "1 equaled 2", "1 did not equal 2", "1 equaled 2")
    val trueFact = True("1 did not equal 2", "1 equaled 2", "1 did not equal 2", "1 equaled 2")
    "should have isTrue and isFalse methods" in {
      falseFact.isTrue shouldBe false
      falseFact.isFalse shouldBe true
      trueFact.isTrue shouldBe true
      trueFact.isFalse shouldBe false
    }
    "should have a toAssertion method that either returns Succeeded or throws TestFailedException with the correct error message and stack depth" in {
      trueFact.toAssertion shouldBe Succeeded
      val caught = the [TestFailedException] thrownBy falseFact.toAssertion
      caught should have message "1 did not equal 2"
      caught.failedCodeLineNumber shouldEqual Some(thisLineNumber - 2)
      caught.failedCodeFileName shouldBe Some("FactSpec.scala")
    }
    "should offer a toBoolean method, even though it is redundant with isTrue" in {
      falseFact.toBoolean shouldBe false
      trueFact.toBoolean shouldBe true
    }
    "should construct localized strings from the raw strings and args" in {
      val fact = False("{0} did not equal {1}", "{0} equaled {1}", "{0} did not equal {1}", "{0} equaled {1}", Vector(1, 2), Vector(1, 2))
      fact should have (
        failureMessage ("1 did not equal 2"),
        negatedFailureMessage ("1 equaled 2"),
        midSentenceFailureMessage ("1 did not equal 2"),
        midSentenceNegatedFailureMessage ("1 equaled 2"),
        rawFailureMessage ("{0} did not equal {1}"),
        rawNegatedFailureMessage ("{0} equaled {1}"),
        rawMidSentenceFailureMessage ("{0} did not equal {1}"),
        rawMidSentenceNegatedFailureMessage ("{0} equaled {1}"),
        failureMessageArgs(Vector(1, 2)),
        negatedFailureMessageArgs(Vector(1, 2)),
        midSentenceFailureMessageArgs(Vector(1, 2)),
        midSentenceNegatedFailureMessageArgs(Vector(1, 2)),
        composite(false)
      )
    }

    "should use midSentenceFailureMessageArgs to construct midSentenceFailureMessage" in {
      val fact = False("{0} did not equal {1}", "{0} equaled {1}", "{0} did not equal {1}", "{0} equaled {1}", Vector.empty, Vector.empty, Vector(1, 2), Vector.empty)
      fact.midSentenceFailureMessage should be ("1 did not equal 2")
    }

    "should use midSentenceNegatedFailureMessageArgs to construct midSentenceNegatedFailureMessage" in {
      val fact = False("{0} did not equal {1}", "{0} equaled {1}", "{0} did not equal {1}", "{0} equaled {1}", Vector.empty, Vector.empty, Vector.empty, Vector(1, 2))
      fact.midSentenceNegatedFailureMessage should be ("1 equaled 2")
    }
  }
}
