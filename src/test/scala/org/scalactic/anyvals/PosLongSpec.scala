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
import org.scalactic.StrictCheckedEquality._

class PosLongSpec extends Spec with Matchers {

  object `A PosLong` {
    object `should offer a from factory method that` {
      def `returns Some[PosLong] if the passed Long is greater than 0`
      {
        PosLong.from(50L).value.value shouldBe 50
        PosLong.from(100L).value.value shouldBe 100
      }
      def `returns None if the passed Long is NOT greater than 0` {
        PosLong.from(0L) shouldBe None
        PosLong.from(-1L) shouldBe None
        PosLong.from(-99L) shouldBe None
      }
    } 
    def `should have a pretty toString` {
      PosLong.from(42L).value.toString shouldBe "PosLong(42)"
    }
    def `should return the same type from its unary_+ method` {
      +PosLong(3L) shouldEqual PosLong(3L)
    } 
    def `should be automatically widened to compatible AnyVal targets` {
      (PosLong(3L): Long) shouldEqual 3L
      (PosLong(3L): Float) shouldEqual 3.0F
      (PosLong(3L): Double) shouldEqual 3.0
      (PosLong(3L): PozLong) shouldEqual PozLong(3L)
      (PosLong(3L): PozFloat) shouldEqual PozFloat(3.0F)
      (PosLong(3L): PozDouble) shouldEqual PozDouble(3.0)
    }
    object `when a compatible AnyVal is passed to a + method invoked on it` {
      def `should give the same AnyVal type back at compile time, and correct value at runtime` {
        // When adding a "primitive"
        val opInt = PosLong(3L) + 3
        opInt shouldEqual 6L

        val opLong = PosLong(3L) + 3L
        opLong shouldEqual 6L

        val opFloat = PosLong(3L) + 3.0F
        opFloat shouldEqual 6.0F

        val opDouble = PosLong(3L) + 3.0
        opDouble shouldEqual 6.0

        // When adding a Pos*
        val opPosInt = PosLong(3L) + PosInt(3)
        opPosInt shouldEqual 6L

        val opPosLong = PosLong(3L) + PosLong(3L)
        opPosLong shouldEqual 6L

        val opPosFloat = PosLong(3L) + PosFloat(3.0F)
        opPosFloat shouldEqual 6.0F

        val opPosDouble = PosLong(3L) + PosDouble(3.0)
        opPosDouble shouldEqual 6.0

        // When adding a *Poz
        val opPoz = PosLong(3L) + PozInt(3)
        opPoz shouldEqual 6L

        val opPozLong = PosLong(3L) + PozLong(3L)
        opPozLong shouldEqual 6L

        val opPozFloat = PosLong(3L) + PozFloat(3.0F)
        opPozFloat shouldEqual 6.0F

        val opPozDouble = PosLong(3L) + PozDouble(3.0)
        opPozDouble shouldEqual 6.0
      }
    }

    object `when created with apply method` {

      def `should compile when 8 is passed in`: Unit = {
        "PosLong(8)" should compile
        PosLong(8).value shouldEqual 8
        "PosLong(8L)" should compile
        PosLong(8L).value shouldEqual 8
      }

      def `should not compile when 0 is passed in`: Unit = {
        "PosLong(0)" shouldNot compile
        "PosLong(0L)" shouldNot compile
      }

      def `should not compile when -8 is passed in`: Unit = {
        "PosLong(-8)" shouldNot compile
        "PosLong(-8L)" shouldNot compile
      }

      def `should not compile when x is passed in`: Unit = {
        val a: Int = -8
        "PosLong(a)" shouldNot compile
        val b: Long = -8L
        "PosLong(b)" shouldNot compile
      }
    }
    object `when specified as a plain-old Long` {

      def takesPosLong(pos: PosLong): Long = pos.value

      def `should compile when 8 is passed in`: Unit = {
        "takesPosLong(8)" should compile
        takesPosLong(8) shouldEqual 8
        "takesPosLong(8L)" should compile
        takesPosLong(8L) shouldEqual 8
      }

      def `should not compile when 0 is passed in`: Unit = {
        "takesPosLong(0)" shouldNot compile
        "takesPosLong(0L)" shouldNot compile
      }

      def `should not compile when -8 is passed in`: Unit = {
        "takesPosLong(-8)" shouldNot compile
        "takesPosLong(-8L)" shouldNot compile
      }

      def `should not compile when x is passed in`: Unit = {
        val x: Long = -8
        "takesPosLong(x)" shouldNot compile
        val b: Long = -8L
        "takesPosLong(b)" shouldNot compile
      }
    }
  }
}

