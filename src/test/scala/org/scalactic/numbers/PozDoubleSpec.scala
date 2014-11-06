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

class PozDoubleSpec extends Spec with Matchers {
  object `A PozDouble` {
    object `should offer a from factory method that` {
      def `returns Some[PozDouble] if the passed Double is greater than or equal to 0`
      {
        PozDouble.from(0.0).value.value shouldBe 0.0
        PozDouble.from(50.23).value.value shouldBe 50.23
        PozDouble.from(100.0).value.value shouldBe 100.0
      }
      def `returns None if the passed Double is NOT greater than or equal to 0`
      {
        PozDouble.from(-0.00001) shouldBe None
        PozDouble.from(-99.9) shouldBe None
      }
    } 
    def `should have a pretty toString` {
      PozDouble.from(42.0).value.toString shouldBe "PozDouble(42.0)"
    }
  }
}

