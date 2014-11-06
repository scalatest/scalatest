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
package org.scalactic.numbers

import org.scalatest._
import scala.collection.mutable.WrappedArray
import OptionValues._
import org.scalactic.CheckedEquality._

class PozFloatSpec extends Spec with Matchers {
  object `An PozFloat` {
    object `should offer a from factory method that` {
      def `returns Some[PozFloat] if the passed Float is greater than or equal to 0`
      {
        PozFloat.from(0.0f).value.value shouldBe 0.0f
        PozFloat.from(50.23f).value.value shouldBe 50.23f
        PozFloat.from(100.0f).value.value shouldBe 100.0f
      }
      def `returns None if the passed Float is NOT greater than or equal to 0` {
        PozFloat.from(-0.00001f) shouldBe None
        PozFloat.from(-99.9f) shouldBe None
      }
    } 
    def `should have a pretty toString` {
      PozFloat.from(42.0f).value.toString shouldBe "PozFloat(42.0)"
    }
  }
}

