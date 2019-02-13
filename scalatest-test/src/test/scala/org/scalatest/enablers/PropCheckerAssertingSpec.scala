/*
 * Copyright 2001-2019 Artima, Inc.
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
package org.scalatest.enablers

import org.scalatest._
import org.scalactic.Equality
import scala.collection.immutable
import prop.GeneratorDrivenPropertyChecks
import org.scalactic.anyvals.PosZInt
import exceptions.TestFailedException
import OptionValues._

class PropCheckerAssertingSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks with LineNumberHelper {

  describe("PropCheckerAsserting") {
    it("The exception thrown by forAll should include the exception thrown by the property function evaluation as its cause") {
      var thrownInnerEx: Option[TestFailedException] = None
      val tfe =
        intercept[TestFailedException] {
          forAll { (i: Int) =>
            val innerTfe =
              intercept[TestFailedException] {
                i should equal (i + 1)
              }
              thrownInnerEx = Some(innerTfe)
              throw innerTfe
          }
        }
      tfe.cause.value should be theSameInstanceAs (thrownInnerEx.value)
    }
  }
}

