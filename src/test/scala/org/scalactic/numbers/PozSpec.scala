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

class PozSpec extends Spec with Matchers {
  object `A Poz` {
    object `should offer a from factory method that` {
      def `returns Some[Poz] if the passed Int is greater than or equal to 0`
      {
        Poz.from(0).value.value shouldBe 0
        Poz.from(50).value.value shouldBe 50
        Poz.from(100).value.value shouldBe 100
      }
      def `returns None if the passed Int is NOT greater than or equal to 0` {
        Poz.from(-1) shouldBe None
        Poz.from(-99) shouldBe None
      }
    } 
    def `should have a pretty toString` {
      Poz.from(42).value.toString shouldBe "Poz(42)"
    }
  }

  object `An LPoz` {
    object `should offer a from factory method that` {
      def `returns Some[LPoz] if the passed Long is greater than or equal to 0`
      {
        LPoz.from(0L).value.value shouldBe 0
        LPoz.from(50L).value.value shouldBe 50
        LPoz.from(100L).value.value shouldBe 100
      }
      def `returns None if the passed Long is NOT greater than or equal to 0` {
        LPoz.from(-1L) shouldBe None
        LPoz.from(-99L) shouldBe None
      }
    } 
    def `should have a pretty toString` {
      LPoz.from(42L).value.toString shouldBe "LPoz(42)"
    }
  }

  object `A DPoz` {
    object `should offer a from factory method that` {
      def `returns Some[DPoz] if the passed Double is greater than or equal to 0`
      {
        DPoz.from(0.0).value.value shouldBe 0.0
        DPoz.from(50.23).value.value shouldBe 50.23
        DPoz.from(100.0).value.value shouldBe 100.0
      }
      def `returns None if the passed Double is NOT greater than or equal to 0`
      {
        DPoz.from(-0.00001) shouldBe None
        DPoz.from(-99.9) shouldBe None
      }
    } 
    def `should have a pretty toString` {
      DPoz.from(42.0).value.toString shouldBe "DPoz(42.0)"
    }
  }

  object `An FPoz` {
    object `should offer a from factory method that` {
      def `returns Some[FPoz] if the passed Float is greater than or equal to 0`
      {
        FPoz.from(0.0f).value.value shouldBe 0.0f
        FPoz.from(50.23f).value.value shouldBe 50.23f
        FPoz.from(100.0f).value.value shouldBe 100.0f
      }
      def `returns None if the passed Float is NOT greater than or equal to 0` {
        FPoz.from(-0.00001f) shouldBe None
        FPoz.from(-99.9f) shouldBe None
      }
    } 
    def `should have a pretty toString` {
      FPoz.from(42.0f).value.toString shouldBe "FPoz(42.0)"
    }
  }
}

