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
      (Pos.from(3).get: PozL) shouldEqual PozL.from(3L).get
      (Pos.from(3).get: PozF) shouldEqual PozF.from(3.0F).get
      (Pos.from(3).get: PozD) shouldEqual PozD.from(3.0).get
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

        val posPosL = Pos.from(3).get + PosL.from(3L).get
        posPosL shouldEqual 6L

        val posPosF = Pos.from(3).get + PosF.from(3.0F).get
        posPosF shouldEqual 6.0F

        val posPosD = Pos.from(3).get + PosD.from(3.0).get
        posPosD shouldEqual 6.0

        // When adding a *Poz
        val posPoz = Pos.from(3).get + Poz.from(3).get
        posPoz shouldEqual Poz.from(6).get.value

        val posPozL = Pos.from(3).get + PozL.from(3L).get
        posPozL shouldEqual PozL.from(6L).get.value

        val posPozF = Pos.from(3).get + PozF.from(3.0F).get
        posPozF shouldEqual PozF.from(6.0F).get.value

        val posPozD = Pos.from(3).get + PozD.from(3.0).get
        posPozD shouldEqual PozD.from(6.0).get.value
      }
    }

    object `when created with apply method` {

      def `should compile when 8 is passed in`: Unit = {
        "Pos(8)" should compile
        Pos(8).value shouldEqual 8
      }

      def `should not compile when 0 is passed in`: Unit = {
        "Pos(0)" shouldNot compile
      }

      def `should not compile when -8 is passed in`: Unit = {
        "Pos(-8)" shouldNot compile
      }

      def `should not compile when x is passed in`: Unit = {
        val x: Int = -8
        "Pos(x)" shouldNot compile
      }
    }
    object `when specified as a plain-old Int` {

      def takesPos(pos: Pos): Int = pos.value

      def `should compile when 8 is passed in`: Unit = {
        "takesPos(8)" should compile
        takesPos(8) shouldEqual 8
      }

      def `should not compile when 0 is passed in`: Unit = {
        "takesPos(0)" shouldNot compile
      }

      def `should not compile when -8 is passed in`: Unit = {
        "takesPos(-8)" shouldNot compile
      }

      def `should not compile when x is passed in`: Unit = {
        val x: Int = -8
        "takesPos(x)" shouldNot compile
      }
    }
  }

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

