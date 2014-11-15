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

class PozLongSpec extends Spec with Matchers {
  object `A PozLong` {
    object `should offer a from factory method that` {
      def `returns Some[PozLong] if the passed Long is greater than or equal to 0` {
        PozLong.from(0L).value.value shouldBe 0
        PozLong.from(50L).value.value shouldBe 50
        PozLong.from(100L).value.value shouldBe 100
      }
      def `returns None if the passed Long is NOT greater than or equal to 0` {
        PozLong.from(-1L) shouldBe None
        PozLong.from(-99L) shouldBe None
      }
    }
    def `should have a pretty toString` {
      PozLong.from(42L).value.toString shouldBe "PozLong(42)"
    }
    def `should be automatically widened to compatible AnyVal targets` {
      (PozLong(3L): Long) shouldEqual 3L
      (PozLong(3L): Float) shouldEqual 3.0F
      (PozLong(3L): Double) shouldEqual 3.0
      (PozLong(3L): PozFloat) shouldEqual PozFloat(3.0F)
      (PozLong(3L): PozDouble) shouldEqual PozDouble(3.0)
    }
    object `when a compatible AnyVal is passed to a + method invoked on it` {
      def `should give the same AnyVal type back at compile time, and correct value at runtime` {
        // When adding a "primitive"
        val opInt = PozLong(3L) + 3
        opInt shouldEqual 6L

        val opLong = PozLong(3L) + 3L
        opLong shouldEqual 6L

        val opFloat = PozLong(3L) + 3.0F
        opFloat shouldEqual 6.0F

        val opDouble = PozLong(3L) + 3.0
        opDouble shouldEqual 6.0

        // When adding a Pos*
        val opPosInt = PozLong(3L) + PosInt(3)
        opPosInt shouldEqual 6L

        val opPosLong = PozLong(3L) + PosLong(3L)
        opPosLong shouldEqual 6L

        val opPosFloat = PozLong(3L) + PosFloat(3.0F)
        opPosFloat shouldEqual 6.0F

        val opPosDouble = PozLong(3L) + PosDouble(3.0)
        opPosDouble shouldEqual 6.0

        // When adding a *Poz
        val opPoz = PozLong(3L) + PozInt(3)
        opPoz shouldEqual 6L

        val opPozLong = PozLong(3L) + PozLong(3L)
        opPozLong shouldEqual 6L

        val opPozFloat = PozLong(3L) + PozFloat(3.0F)
        opPozFloat shouldEqual 6.0F

        val opPozDouble = PozLong(3L) + PozDouble(3.0)
        opPozDouble shouldEqual 6.0
      }
    }

    object `when created with apply method` {

      def `should compile when 8 is passed in`: Unit = {
        "PozLong(8)" should compile
        PozLong(8).value shouldEqual 8L
        "PozLong(8L)" should compile
        PozLong(8L).value shouldEqual 8L
      }

      def `should compile when 0 is passed in`: Unit = {
        "PozLong(0)" should compile
        PozLong(0).value shouldEqual 0L
        "PozLong(0L)" should compile
        PozLong(0L).value shouldEqual 0L
      }

      def `should not compile when -8 is passed in`: Unit = {
        "PozLong(-8)" shouldNot compile
        "PozLong(-8L)" shouldNot compile
      }

      def `should not compile when x is passed in`: Unit = {
        val a: Int = -8
        "PozLong(a)" shouldNot compile
        val b: Long = -8L
        "PozLong(b)" shouldNot compile
      }
    }
    object `when specified as a plain-old Long` {

      def takesPozLong(pos: PozLong): Long = pos.value

      def `should compile when 8 is passed in`: Unit = {
        "takesPozLong(8)" should compile
        takesPozLong(8) shouldEqual 8
        "takesPozLong(8L)" should compile
        takesPozLong(8L) shouldEqual 8L
      }

      def `should compile when 0 is passed in`: Unit = {
        "takesPozLong(0)" should compile
        takesPozLong(0) shouldEqual 0
        "takesPozLong(0L)" should compile
        takesPozLong(0L) shouldEqual 0L
      }

      def `should not compile when -8 is passed in`: Unit = {
        "takesPozLong(-8)" shouldNot compile
        "takesPozLong(-8L)" shouldNot compile
      }

      def `should not compile when x is passed in`: Unit = {
        val x: Long = -8
        "takesPozLong(x)" shouldNot compile
        val b: Long = -8L
        "takesPozLong(b)" shouldNot compile
      }
    }
  }
}

