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
    object `when created with apply method` {

      def `should compile when 8 is passed in`: Unit = {
        "PosL(8)" should compile
        PosL(8).value shouldEqual 8
        "PosL(8L)" should compile
        PosL(8L).value shouldEqual 8
      }

      def `should not compile when 0 is passed in`: Unit = {
        "PosL(0)" shouldNot compile
        "PosL(0L)" shouldNot compile
      }

      def `should not compile when -8 is passed in`: Unit = {
        "PosL(-8)" shouldNot compile
        "PosL(-8L)" shouldNot compile
      }
      def `should not compile when x is passed in`: Unit = {
        val a: Int = -8
        "Pos(a)" shouldNot compile
        val b: Long = -8L
        "Pos(b)" shouldNot compile
      }
    }
/*
    object `when specified as a plain-old Int` {

      def takesPosL(posL: PosL): Long = posL.value

      def `should compile when 8 is passed in`: Unit = {
        "takesPosL(8)" should compile
        takesPosL(8) shouldEqual 8L
        "takesPosL(8L)" should compile
        takesPosL(8L) shouldEqual 8L
      }

      def `should not compile when 0 is passed in`: Unit = {
        "takesPosL(0)" shouldNot compile
        "takesPosL(0L)" shouldNot compile
      }

      def `should not compile when -8 is passed in`: Unit = {
        "takesPosL(-8)" shouldNot compile
        "takesPosL(-8L)" shouldNot compile
      }

      def `should not compile when x is passed in`: Unit = {
        val a: Int = -8
        "takesPosL(a)" shouldNot compile
        val b: Long = -8L
        "takesPosL(b)" shouldNot compile
      }
    }
*/
/*
    def `should be automatically widened to compatible AnyVal targets` {
      (PosL.from(3).get: Int) shouldEqual 3 // shouldNot typeCheck
      (PosL.from(3).get: Long) shouldEqual 3L
      (PosL.from(3).get: Float) shouldEqual 3.0F
      (PosL.from(3).get: Double) shouldEqual 3.0
      (PosL.from(3).get: Poz) shouldEqual Poz.from(3).get
      (PosL.from(3).get: PozL) shouldEqual PozL.from(3L).get
      (PosL.from(3).get: PozF) shouldEqual PozF.from(3.0F).get
      (PosL.from(3).get: PozD) shouldEqual PozD.from(3.0).get
    }
    object `when a compatible AnyVal is passed to a + method invoked on it` {
      def `should give the same AnyVal type back at compile time, and correct value at runtime` {
        // When adding a "primitive"
        val opInt = PosL.from(3).get + 3 // should be type Long
        opInt shouldEqual 6L

        val opLong = PosL.from(3).get + 3L
        opLong shouldEqual 6L

        val opFloat = PosL.from(3).get + 3.0F
        opFloat shouldEqual 6.0F

        val opDouble = PosL.from(3).get + 3.0
        opDouble shouldEqual 6.0

        // When adding a *Pos
        val opPos = Pos.from(3).get + Pos.from(3).get
        opPos shouldEqual 6L

        val opPosL = Pos.from(3).get + PosL.from(3L).get
        opPosL shouldEqual 6L

        val opPosF = Pos.from(3).get + PosF.from(3.0F).get
        opPosF shouldEqual 6.0F

        val opPosD = Pos.from(3).get + PosD.from(3.0).get
        opPosD shouldEqual 6.0

        // When adding a *Poz
        val opPoz = Pos.from(3).get + Poz.from(3).get
        opPoz shouldEqual Poz.from(6).get.value

        val opPozL = Pos.from(3).get + PozL.from(3L).get
        opPozL shouldEqual PozL.from(6L).get.value

        val opPozF = Pos.from(3).get + PozF.from(3.0F).get
        opPozF shouldEqual PozF.from(6.0F).get.value

        val opPozD = Pos.from(3).get + PozD.from(3.0).get
        opPozD shouldEqual PozD.from(6.0).get.value
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
*/
  }
}

