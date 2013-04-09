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

import org.scalatest.events._

class TimesOnIntSpec extends FunSpec with SharedHelpers with TimesOnInt {

  describe("The TimesOnInt trait") {

    it("should allow people to repeat side effects a specified number of times") {

      // Need to support this one, because someone may invoke times on an integer variable.
      // Probably need to support 0 times as well, but should throw IAE if negative is passed.
      var i = 0
      0 times { i += 1 }
      assert(i === 0)
      1 times { i += 1 }
      assert(i === 1)
      2 times { i += 1 }
      assert(i === 3)
      3 times { i += 1 }
      assert(i === 6)
      4 times { i += 1 }
      assert(i === 10)
      90 times { i += 1 }
      assert(i === 100)
    }

    it("should throw IllegalArgumentException if times is invoked on a negative integer") {
      var i = 0
      intercept[IllegalArgumentException] {
        -1 times { i += 1 }
      }
      assert(i === 0)
    }
  }
}
