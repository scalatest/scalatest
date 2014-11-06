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

class PozFSpec extends Spec with Matchers {
  object `An PozF` {
    object `should offer a from factory method that` {
      def `returns Some[PozF] if the passed Float is greater than or equal to 0`
      {
        PozF.from(0.0f).value.value shouldBe 0.0f
        PozF.from(50.23f).value.value shouldBe 50.23f
        PozF.from(100.0f).value.value shouldBe 100.0f
      }
      def `returns None if the passed Float is NOT greater than or equal to 0` {
        PozF.from(-0.00001f) shouldBe None
        PozF.from(-99.9f) shouldBe None
      }
    } 
    def `should have a pretty toString` {
      PozF.from(42.0f).value.toString shouldBe "PozF(42.0)"
    }
  }
}

