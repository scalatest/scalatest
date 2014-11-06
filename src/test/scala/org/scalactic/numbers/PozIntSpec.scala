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

class PozIntSpec extends Spec with Matchers {
  object `A PozInt` {
    object `should offer a from factory method that` {
      def `returns Some[PozInt] if the passed Int is greater than or equal to 0`
      {
        PozInt.from(0).value.value shouldBe 0
        PozInt.from(50).value.value shouldBe 50
        PozInt.from(100).value.value shouldBe 100
      }
      def `returns None if the passed Int is NOT greater than or equal to 0` {
        PozInt.from(-1) shouldBe None
        PozInt.from(-99) shouldBe None
      }
    } 
    def `should have a pretty toString` {
      PozInt.from(42).value.toString shouldBe "PozInt(42)"
    }
  }
}

