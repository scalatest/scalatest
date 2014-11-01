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

class PosSpec extends Spec with Matchers {
  object `A Pos` {
    object `should offer a from factory method that` {
      def `returns Some[Pos] if the passed Int is greater than 0`
      {
        Pos.from(50).value.value shouldBe 50
        Pos.from(100).value.value shouldBe 100
      }
      def `returns None if the passed Int is NOT greater than 0` {
        Pos.from(0) shouldBe None
        Pos.from(-1) shouldBe None
        Pos.from(-99) shouldBe None
      }
    } 
    def `should have a pretty toString` {
      Pos.from(42).value.toString shouldBe "Pos(42)"
    }
    def `should be automatically widened to compatible AnyVal targets` {
      (Pos.from(3).get: Int) shouldEqual 3
      (Pos.from(3).get: Long) shouldEqual 3L
      (Pos.from(3).get: Float) shouldEqual 3.0F
      (Pos.from(3).get: Double) shouldEqual 3.0
      (Pos.from(3).get: Poz) shouldEqual Poz.from(3).get
      (Pos.from(3).get: LPoz) shouldEqual LPoz.from(3L).get
      (Pos.from(3).get: FPoz) shouldEqual FPoz.from(3.0F).get
      (Pos.from(3).get: DPoz) shouldEqual DPoz.from(3.0).get
    }
    object `when a compatible AnyVal is passed to a + method invoked on it` {
      def `should give the same AnyVal type back at compile time, and correct value at runtime` {
        // When adding a "primitive"
        val posInt = Pos.from(3).get + 3
        posInt shouldEqual 6

        val posLong = Pos.from(3).get + 3L
        posLong shouldEqual 6L

        val posFloat = Pos.from(3).get + 3.0F
        posFloat shouldEqual 6.0F

        val posDouble = Pos.from(3).get + 3.0
        posDouble shouldEqual 6.0

        // When adding a *Pos
        val posPos = Pos.from(3).get + Pos.from(3).get
        posPos shouldEqual 6

        val posLPos = Pos.from(3).get + LPos.from(3L).get
        posLPos shouldEqual 6L

        val posFPos = Pos.from(3).get + FPos.from(3.0F).get
        posFPos shouldEqual 6.0F

        val posDPos = Pos.from(3).get + DPos.from(3.0).get
        posDPos shouldEqual 6.0

        // When adding a *Poz
        val posPoz = Pos.from(3).get + Poz.from(3).get
        posPoz shouldEqual Poz.from(6).get.value

        val posLPoz = Pos.from(3).get + LPoz.from(3L).get
        posLPoz shouldEqual LPoz.from(6L).get.value

        val posFPoz = Pos.from(3).get + FPoz.from(3.0F).get
        posFPoz shouldEqual FPoz.from(6.0F).get.value

        val posDPoz = Pos.from(3).get + DPoz.from(3.0).get
        posDPoz shouldEqual DPoz.from(6.0).get.value
      }
    }
  }

  object `An LPos` {
    object `should offer a from factory method that` {
      def `returns Some[LPos] if the passed Long is greater than 0`
      {
        LPos.from(50L).value.value shouldBe 50
        LPos.from(100L).value.value shouldBe 100
      }
      def `returns None if the passed Long is NOT greater than 0` {
        LPos.from(0L) shouldBe None
        LPos.from(-1L) shouldBe None
        LPos.from(-99L) shouldBe None
      }
    } 
    def `should have a pretty toString` {
      LPos.from(42L).value.toString shouldBe "LPos(42)"
    }
  }

  object `A DPos` {
    object `should offer a from factory method that` {
      def `returns Some[DPos] if the passed Double is greater than 0`
      {
        DPos.from(50.23).value.value shouldBe 50.23
        DPos.from(100.0).value.value shouldBe 100.0
      }
      def `returns None if the passed Double is NOT greater than 0`
      {
        DPos.from(0.0) shouldBe None
        DPos.from(-0.00001) shouldBe None
        DPos.from(-99.9) shouldBe None
      }
    } 
    def `should have a pretty toString` {
      DPos.from(42.0).value.toString shouldBe "DPos(42.0)"
    }
  }

  object `An FPos` {
    object `should offer a from factory method that` {
      def `returns Some[FPos] if the passed Float is greater than 0`
      {
        FPos.from(50.23F).value.value shouldBe 50.23F
        FPos.from(100.0F).value.value shouldBe 100.0F
      }
      def `returns None if the passed Float is NOT greater than 0` {
        FPos.from(0.0F) shouldBe None
        FPos.from(-0.00001F) shouldBe None
        FPos.from(-99.9F) shouldBe None
      }
    } 
    def `should have a pretty toString` {
      FPos.from(42.0F).value.toString shouldBe "FPos(42.0)"
    }
  }
}

