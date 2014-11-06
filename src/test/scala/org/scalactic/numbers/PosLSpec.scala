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

class PosLSpec extends Spec with Matchers {

  object `A PosL` {
    object `should offer a from factory method that` {
      def `returns Some[PosL] if the passed Long is greater than 0`
      {
        PosL.from(50L).value.value shouldBe 50
        PosL.from(100L).value.value shouldBe 100
      }
      def `returns None if the passed Long is NOT greater than 0` {
        PosL.from(0L) shouldBe None
        PosL.from(-1L) shouldBe None
        PosL.from(-99L) shouldBe None
      }
    } 
    def `should have a pretty toString` {
      PosL.from(42L).value.toString shouldBe "PosL(42)"
    }
  }
}

