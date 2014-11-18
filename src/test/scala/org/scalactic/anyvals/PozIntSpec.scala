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
package org.scalactic.anyvals

import org.scalatest._
import scala.collection.mutable.WrappedArray
import OptionValues._
import org.scalactic.StrictCheckedEquality

class PozIntSpec extends Spec with Matchers with StrictCheckedEquality {
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
    def `should return the same type from its unary_+ method` {
      +PozInt(3) shouldEqual PozInt(3)
    } 
    def `should be automatically widened to compatible AnyVal targets` {
      (PozInt(3): Int) shouldEqual 3
      (PozInt(3): Long) shouldEqual 3L
      (PozInt(3): Float) shouldEqual 3.0F
      (PozInt(3): Double) shouldEqual 3.0
      (PozInt(3): PozLong) shouldEqual PozLong(3L)
      (PozInt(3): PozFloat) shouldEqual PozFloat(3.0F)
      (PozInt(3): PozDouble) shouldEqual PozDouble(3.0)
    }
    object `when a compatible AnyVal is passed to a + method invoked on it` {
      def `should give the same AnyVal type back at compile time, and correct value at runtime` {
        // When adding a "primitive"
        val opInt = PozInt(3) + 3
        opInt shouldEqual 6

        val opLong = PozInt(3) + 3L
        opLong shouldEqual 6L

        val opFloat = PozInt(3) + 3.0F
        opFloat shouldEqual 6.0F

        val opDouble = PozInt(3) + 3.0
        opDouble shouldEqual 6.0

        // When adding a Poz*
        val opPosInt = PozInt(3) + PosInt(3)
        opPosInt shouldEqual 6

        val opPosLong = PozInt(3) + PosLong(3L)
        opPosLong shouldEqual 6L

        val opPosFloat = PozInt(3) + PosFloat(3.0F)
        opPosFloat shouldEqual 6.0F

        val opPosDouble = PozInt(3) + PosDouble(3.0)
        opPosDouble shouldEqual 6.0

        // When adding a *Poz
        val opPoz = PozInt(3) + PozInt(3)
        opPoz shouldEqual 6

        val opPozLong = PozInt(3) + PozLong(3L)
        opPozLong shouldEqual 6L

        val opPozFloat = PozInt(3) + PozFloat(3.0F)
        opPozFloat shouldEqual 6.0F

        val opPozDouble = PozInt(3) + PozDouble(3.0)
        opPozDouble shouldEqual 6.0
      }
    }

    object `when created with apply method` {

      def `should compile when 8 is passed in`: Unit = {
        "PozInt(8)" should compile
        PozInt(8).value shouldEqual 8
      }

      def `should compile when 0 is passed in`: Unit = {
        "PozInt(0)" should compile
        PozInt(0).value shouldEqual 0
      }

      def `should not compile when -8 is passed in`: Unit = {
        "PozInt(-8)" shouldNot compile
      }

      def `should not compile when x is passed in`: Unit = {
        val x: Int = -8
        "PozInt(x)" shouldNot compile
      }
    }
    object `when specified as a plain-old Int` {

      def takesPozInt(pos: PozInt): Int = pos.value

      def `should compile when 8 is passed in`: Unit = {
        "takesPozInt(8)" should compile
        takesPozInt(8) shouldEqual 8
      }

      def `should compile when 0 is passed in`: Unit = {
        "takesPozInt(0)" should compile
      }

      def `should not compile when -8 is passed in`: Unit = {
        "takesPozInt(-8)" shouldNot compile
      }

      def `should not compile when x is passed in`: Unit = {
        val x: Int = -8
        "takesPozInt(x)" shouldNot compile
      }
    }
  }
}

