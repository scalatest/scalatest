/*
 * Copyright 2001-2014 Artima, Inc.
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

import Matchers._
import OptionValues._
import org.scalactic.Prettifier

class ShouldMatchPatternSpec extends FunSpec with OptionValues {

  case class Person(name: String, age: Int)
  case class Ball(weight: Int)

  describe("should matchPattern syntax") {

    val result = Person("Bob", 30)

    it("should do nothing when checking the right pattern") {
      result should matchPattern { case Person("Bob", _) => }
    }

    it("should throw TestFailedException with correct error message when checking wrong pattern") {
      val e = intercept[TestFailedException] {
        result should matchPattern { case Ball(weight) => }
      }
      e.message.value should startWith (Prettifier.default(result) + " did not pattern match ")
    }

  }

}