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
package org.scalatest.matchers

import org.scalatest._
import org.scalatest.exceptions.TestFailedException

class ShouldSameInstanceAsSpec extends Spec with ShouldMatchers {

  object `The be a ('symbol) syntax` {

    val string = "Hi"
    val obj: AnyRef = string
    val otherString = new String("Hi")

    def `should do nothing if the object is the same instance as another object` {
      string should be theSameInstanceAs (string)
      obj should be theSameInstanceAs (string)
      string should be theSameInstanceAs (obj)
    }

    def `should do nothing if the object is not the same instance as another object, when used with not` {
      otherString should not { be theSameInstanceAs (string) }
      otherString should not be theSameInstanceAs (string)
    }

    def `should do nothing if the object is the same instnace as another object, when used in a logical-and expression` {
      obj should ((be theSameInstanceAs (string)) and (be theSameInstanceAs (string)))
      obj should (be theSameInstanceAs (string) and (be theSameInstanceAs (string)))
      obj should (be theSameInstanceAs (string) and be theSameInstanceAs (string))
    }

    def `should do nothing if the object is the same instance as another object, when used in a logical-or expression` {

      obj should ((be theSameInstanceAs (otherString)) or (be theSameInstanceAs (string)))
      obj should (be theSameInstanceAs (otherString) or (be theSameInstanceAs (string)))
      obj should (be theSameInstanceAs (otherString) or be theSameInstanceAs (string))

      obj should ((be theSameInstanceAs (string)) or (be theSameInstanceAs (otherString)))
      obj should (be theSameInstanceAs (string) or (be theSameInstanceAs (otherString)))
      obj should (be theSameInstanceAs (string) or be theSameInstanceAs (otherString))
    }

    def `should do nothing if the object is not the same instance as another object, when used in a logical-and expression with not` {

      obj should (not (be theSameInstanceAs (otherString)) and not (be theSameInstanceAs (otherString)))
      obj should ((not be theSameInstanceAs (otherString)) and (not be theSameInstanceAs (otherString)))
      obj should (not be theSameInstanceAs (otherString) and not be theSameInstanceAs (otherString))
    }

    def `should do nothing if the object is not the same instance as another object, when used in a logical-or expression with not` {

      obj should (not (be theSameInstanceAs (string)) or not (be theSameInstanceAs (otherString)))
      obj should ((not be theSameInstanceAs (string)) or (not be theSameInstanceAs (otherString)))
      obj should (not be theSameInstanceAs (string) or not be theSameInstanceAs (otherString))

      obj should (not (be theSameInstanceAs (otherString)) or not (be theSameInstanceAs (string)))
      obj should ((not be theSameInstanceAs (otherString)) or (not be theSameInstanceAs (string)))
      obj should (not be theSameInstanceAs (otherString) or not be theSameInstanceAs (string))
    }

    def `should throw TestFailedException if the object is not the same instance as another object` {
      val caught1 = intercept[TestFailedException] {
        otherString should be theSameInstanceAs (string)
      }
      assert(caught1.getMessage === "\"Hi\" was not the same instance as \"Hi\"")
    }

    def `should throw TestFailedException if the object is the same instance as another object, when used with not` {
      val caught1 = intercept[TestFailedException] {
        obj should not { be theSameInstanceAs (string) }
      }
      assert(caught1.getMessage === "\"Hi\" was the same instance as \"Hi\"")
      val caught2 = intercept[TestFailedException] {
        obj should not be theSameInstanceAs (string)
      }
      assert(caught2.getMessage === "\"Hi\" was the same instance as \"Hi\"")
    }

    def `should throw TestFailedException if the object is not the same instance as another object, when used in a logical-and expression` {
      val caught1 = intercept[TestFailedException] {
        obj should ((be theSameInstanceAs (string)) and (be theSameInstanceAs (otherString)))
      }
      assert(caught1.getMessage === "\"Hi\" was the same instance as \"Hi\", but \"Hi\" was not the same instance as \"Hi\"")
      val caught2 = intercept[TestFailedException] {
        obj should (be theSameInstanceAs (string) and (be theSameInstanceAs (otherString)))
      }
      assert(caught2.getMessage === "\"Hi\" was the same instance as \"Hi\", but \"Hi\" was not the same instance as \"Hi\"")
      val caught3 = intercept[TestFailedException] {
        obj should (be theSameInstanceAs (string) and be theSameInstanceAs (otherString))
      }
      assert(caught3.getMessage === "\"Hi\" was the same instance as \"Hi\", but \"Hi\" was not the same instance as \"Hi\"")
    }

    def `should throw TestFailedException if the object is not the same instance as another object, when used in a logical-or expression` {

      val caught1 = intercept[TestFailedException] {
        obj should ((be theSameInstanceAs (otherString)) or (be theSameInstanceAs (otherString)))
      }
      assert(caught1.getMessage === "\"Hi\" was not the same instance as \"Hi\", and \"Hi\" was not the same instance as \"Hi\"")
      val caught2 = intercept[TestFailedException] {
        obj should (be theSameInstanceAs (otherString) or (be theSameInstanceAs (otherString)))
      }
      assert(caught2.getMessage === "\"Hi\" was not the same instance as \"Hi\", and \"Hi\" was not the same instance as \"Hi\"")
      val caught3 = intercept[TestFailedException] {
        obj should (be theSameInstanceAs (otherString) or be theSameInstanceAs (otherString))
      }
      assert(caught3.getMessage === "\"Hi\" was not the same instance as \"Hi\", and \"Hi\" was not the same instance as \"Hi\"")
    }

    def `should throw TestFailedException if the object is the same instance as another object, when used in a logical-and expression with not` {

      val caught1 = intercept[TestFailedException] {
        obj should (not (be theSameInstanceAs (otherString)) and not (be theSameInstanceAs (string)))
      }
      assert(caught1.getMessage === "\"Hi\" was not the same instance as \"Hi\", but \"Hi\" was the same instance as \"Hi\"")
      val caught2 = intercept[TestFailedException] {
        obj should ((not be theSameInstanceAs (otherString)) and (not be theSameInstanceAs (string)))
      }
      assert(caught2.getMessage === "\"Hi\" was not the same instance as \"Hi\", but \"Hi\" was the same instance as \"Hi\"")
      val caught3 = intercept[TestFailedException] {
        obj should (not be theSameInstanceAs (otherString) and not be theSameInstanceAs (string))
      }
      assert(caught3.getMessage === "\"Hi\" was not the same instance as \"Hi\", but \"Hi\" was the same instance as \"Hi\"")
      // Check that the error message "short circuits"
      val caught7 = intercept[TestFailedException] {
        obj should (not (be theSameInstanceAs (string)) and not (be theSameInstanceAs (otherString)))
      }
      assert(caught7.getMessage === "\"Hi\" was the same instance as \"Hi\"")
    }

    def `should throw TestFailedException if the object has an appropriately named method, which returns true, when used in a logical-or expression with not` {

      val caught1 = intercept[TestFailedException] {
        obj should (not (be theSameInstanceAs (string)) or not (be theSameInstanceAs (string)))
      }
      assert(caught1.getMessage === "\"Hi\" was the same instance as \"Hi\", and \"Hi\" was the same instance as \"Hi\"")
      val caught2 = intercept[TestFailedException] {
        obj should ((not be theSameInstanceAs (string)) or (not be theSameInstanceAs (string)))
      }
      assert(caught2.getMessage === "\"Hi\" was the same instance as \"Hi\", and \"Hi\" was the same instance as \"Hi\"")
      val caught3 = intercept[TestFailedException] {
        obj should (not be theSameInstanceAs (string) or not be theSameInstanceAs (string))
      }
      assert(caught3.getMessage === "\"Hi\" was the same instance as \"Hi\", and \"Hi\" was the same instance as \"Hi\"")
    }
  }
}
