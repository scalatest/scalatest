/*
 * Copyright 2001-2013 Artima, Inc.
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
package org.scalautils

import java.text._
import org.scalatest._
import scala.util.Try
import scala.util.Success
import scala.util.Failure

class ValidationSpec extends UnitSpec with TypeCheckedTripleEquals {
  "A Validation" should "not execute the expression passed to && if already a Fail" in {
    var wasExecuted = false
    def isRound(i: Int): Validation[ErrorMessage] = {
      wasExecuted = true
      if (i % 10 != 0) Fail(i + " was not a round number") else Pass
    }
    (for (i <- Good(2) if Fail("already bad!") && isRound(i)) yield i) shouldBe Bad("already bad!")
    wasExecuted shouldBe false
  }
}
