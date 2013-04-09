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
package org.scalatest.junit

import org.scalatest._
import _root_.junit.framework.AssertionFailedError
import org.scalatest.WordSpec

class ShouldMatchersForJUnitWordSpec extends WordSpec with ShouldMatchersForJUnit
  with AssertionsForJUnit3SharedTests {

  val throwAssertionFailedError = afterWord("throw AssertionFailedError")

  "the ShouldMatchersForJUnit3 trait" should throwAssertionFailedError {

    // ShouldMatchersForJUnit3 mixes in AssertionsForJUnit3, so ShouldMatchersForJUnit3
    // should behave like AsssertionsForJUnit3. (I don't want to use the 'behave like'
    // sugar here, because I want to use the throwAssertionFailedError "after word," and
    // it doesn't read well with the sugar in that case
    fromAssertExpectInterceptAndFail()

    "from failed ShouldMatcher expressions" in {
      intercept[AssertionFailedError] {
       7 should equal (3)
      }
      intercept[AssertionFailedError] {
       "hi" should have length 3
      }
      intercept[AssertionFailedError] {
       Set(1, 2, 3) should have size (4)
      }
      intercept[AssertionFailedError] {
        "hello, world" should startWith ("world")
      }
      intercept[AssertionFailedError] {
        "hello, world" should endWith ("hello")
      }
      intercept[AssertionFailedError] {
        "hello, world" should include ("seven")
      }
      intercept[AssertionFailedError] {
        "hello, world" should startWith regex ("wor*")
      }
      intercept[AssertionFailedError] {
        "hello, world" should endWith regex ("hel*")
      }
      intercept[AssertionFailedError] {
        "hello, world" should include regex ("sev.n")
      }
      intercept[AssertionFailedError] {
       "hi" should fullyMatch regex ("""(-)?(\d+)(\.\d*)?""")
      }
      intercept[AssertionFailedError] {
        7 should be < (7)
      }
      intercept[AssertionFailedError] {
        -1 should be > (0)
      }
      intercept[AssertionFailedError] {
        8 should be <= (7)
      }
      intercept[AssertionFailedError] {
        6 should be >= (7)
      }
      intercept[AssertionFailedError] {
       Set(1, 2, 3) should be ('empty)
      }
      intercept[AssertionFailedError] {
       (new Object) should be theSameInstanceAs (new Object)
      }
      intercept[AssertionFailedError] {
       21.0 should be (6.9 plusOrMinus 0.2)
      }
      intercept[AssertionFailedError] {
       17 should be (6 plusOrMinus 2)
      }
      intercept[AssertionFailedError] {
       List(1, 2, 3) should contain (5)
      }
      intercept[AssertionFailedError] {
       Map("hi" -> 1) should contain key ("ho")
      }
      intercept[AssertionFailedError] {
        Map("hi" -> 1) should contain value (9)
      }
      intercept[AssertionFailedError] {
       "hi" should be ("ho")
      }
      intercept[AssertionFailedError] {
       "hi" should not be ("hi")
      }
      intercept[AssertionFailedError] {
       10 should not be <= (11)
      }
      intercept[AssertionFailedError] {
       Map("hi" -> 5) should (contain key ("hi") and not contain value (5))
      }
      intercept[AssertionFailedError] {
       "ho ho ho" should (
         equal ("fee") or
         equal ("fie") or
         equal ("foe") or
         equal ("fum")
        )
      }
      intercept[AssertionFailedError] {
       11 should (be > (0) and be <= (10))
      }
    }

    "including as its cause an unexpected exception" in {
      val wrongException = new RuntimeException("oops!")
      val caught =
        intercept[AssertionFailedError] {
          evaluating { throw wrongException } should produce [IllegalArgumentException]
        }
      assert(caught.getCause eq wrongException)
    }
  }
}

