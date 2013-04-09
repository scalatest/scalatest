/*
 * Copyright 2001-2009 Artima, Inc.
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

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.words.ShouldVerb

private class VariousWordSpec {} // prevents unnecessary recompilation

class AmpersandWordSpec extends WordSpec with ShouldMatchers {

  "The Scala language" should {
    "provide an && operator" which {
      "returns true for true && true" in { true && true should be (true) }
      "returns false for true && false" in { true && false should be (false) }
      "returns false for false && true" in { true && false should be (false) }
      "returns false for false && false" in { false && false should be (false) }
    }
  }
  
  /**
   * The following test should be removed once the 'that' word is removed.
   */
  "The Scala language" should {
    "provide an && operator" that {
      "returns true for true && true" in { true && true should be (true) }
      "returns false for true && false" in { true && false should be (false) }
      "returns false for false && true" in { true && false should be (false) }
      "returns false for false && false" in { false && false should be (false) }
    }
  }
}

class LoginUiWordSpec extends WordSpec {

  def theUser = afterWord("the user")
  def have = afterWord("have")
  def is = afterWord("is")

  "The login screen" when theUser {
    "first enters it" should have {
      "an empty username field" is (pending)
      "a password field" which is {
        "empty" is (pending)
        "disabled" is (pending)
      }
      "a password field" that is {
        "empty" is (pending)
        "disabled" is (pending)
      }
      "a login button that is disabled" is (pending)
    }
    "enters his or her username" should have {
      "a username field which contains the entered username" is (pending)
      "a password field" which is {
        "empty" is (pending)
        "enabled" is (pending)
      }
      "a password field" that is {
        "empty" is (pending)
        "enabled" is (pending)
      }
      "a login button that is disabled" is (pending)
    }
    "enters his or her password" should have {
      "an username field that contains the entered username" is (pending)
      "a password field that contains the entered password" is (pending)
      "a login button that is enabled" is (pending)
    }
    "presses the login button" should {
      "attempt to log the user in" is (pending)
    }
  }
  
  /**
   * The 'that' test should be removed once the 'that' word is removed.
   */
}

