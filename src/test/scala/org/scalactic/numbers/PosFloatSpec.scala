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
import org.scalactic.StrictCheckedEquality._

class PosFloatSpec extends Spec with Matchers {
  object `A PosFloat` {
    object `should offer a from factory method that` {
      def `returns Some[PosFloat] if the passed Float is greater than 0`
      {
        PosFloat.from(50.23F).value.value shouldBe 50.23F
        PosFloat.from(100.0F).value.value shouldBe 100.0F
      }
      def `returns None if the passed Float is NOT greater than 0` {
        PosFloat.from(0.0F) shouldBe None
        PosFloat.from(-0.00001F) shouldBe None
        PosFloat.from(-99.9F) shouldBe None
      }
    } 
    def `should have a pretty toString` {
      PosFloat.from(42.0F).value.toString shouldBe "PosFloat(42.0)"
    }
  }
}

