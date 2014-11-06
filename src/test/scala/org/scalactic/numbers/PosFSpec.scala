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

class PosFSpec extends Spec with Matchers {
  object `A PosF` {
    object `should offer a from factory method that` {
      def `returns Some[PosF] if the passed Float is greater than 0`
      {
        PosF.from(50.23F).value.value shouldBe 50.23F
        PosF.from(100.0F).value.value shouldBe 100.0F
      }
      def `returns None if the passed Float is NOT greater than 0` {
        PosF.from(0.0F) shouldBe None
        PosF.from(-0.00001F) shouldBe None
        PosF.from(-99.9F) shouldBe None
      }
    } 
    def `should have a pretty toString` {
      PosF.from(42.0F).value.toString shouldBe "PosF(42.0)"
    }
  }
}

