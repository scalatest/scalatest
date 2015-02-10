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
//import org.scalactic.StrictCheckedEquality

class PosZLongSpec extends Spec with Matchers/* with StrictCheckedEquality*/ {
  object `A PosZLong` {
    object `should offer a from factory method that` {
      def `returns Some[PosZLong] if the passed Long is greater than or equal to 0` {
        PosZLong.from(0L).value.value shouldBe 0L
        PosZLong.from(50L).value.value shouldBe 50L
        PosZLong.from(100L).value.value shouldBe 100L
      }
      def `returns None if the passed Long is NOT greater than or equal to 0` {
        PosZLong.from(-1L) shouldBe None
        PosZLong.from(-99L) shouldBe None
      }
    }
    def `should have a pretty toString` {
      PosZLong.from(42L).value.toString shouldBe "PosZLong(42)"
    }
    def `should return the same type from its unary_+ method` {
      +PosZLong(3L) shouldEqual PosZLong(3L)
    } 
    def `should be automatically widened to compatible AnyVal targets` {
      "(PosZLong(3L): Int)" shouldNot typeCheck
      (PosZLong(3L): Long) shouldEqual 3L
      (PosZLong(3L): Float) shouldEqual 3.0F
      (PosZLong(3L): Double) shouldEqual 3.0

      "(PosZLong(3L): PosInt)" shouldNot typeCheck
      "(PosZLong(3L): PosLong)" shouldNot typeCheck
      "(PosZLong(3L): PosFloat)" shouldNot typeCheck
      "(PosZLong(3L): PosDouble)" shouldNot typeCheck

      "(PosZLong(3L): PosZInt)" shouldNot typeCheck
      (PosZLong(3L): PosZLong) shouldEqual PosZLong(3L)
      (PosZLong(3L): PosZFloat) shouldEqual PosZFloat(3.0F)
      (PosZLong(3L): PosZDouble) shouldEqual PosZDouble(3.0)
    }
    object `when a compatible AnyVal is passed to a + method invoked on it` {
      def `should give the same AnyVal type back at compile time, and correct value at runtime` {
        // When adding a "primitive"
        val opInt = PosZLong(3L) + 3
        opInt shouldEqual 6L

        val opLong = PosZLong(3L) + 3L
        opLong shouldEqual 6L

        val opFloat = PosZLong(3L) + 3.0F
        opFloat shouldEqual 6.0F

        val opDouble = PosZLong(3L) + 3.0
        opDouble shouldEqual 6.0

        // When adding a Pos*
        val opPosInt = PosZLong(3L) + PosInt(3)
        opPosInt shouldEqual 6L

        val opPosLong = PosZLong(3L) + PosLong(3L)
        opPosLong shouldEqual 6L

        val opPosFloat = PosZLong(3L) + PosFloat(3.0F)
        opPosFloat shouldEqual 6.0F

        val opPosDouble = PosZLong(3L) + PosDouble(3.0)
        opPosDouble shouldEqual 6.0

        // When adding a *PosZ
        val opPosZ = PosZLong(3L) + PosZInt(3)
        opPosZ shouldEqual 6L

        val opPosZLong = PosZLong(3L) + PosZLong(3L)
        opPosZLong shouldEqual 6L

        val opPosZFloat = PosZLong(3L) + PosZFloat(3.0F)
        opPosZFloat shouldEqual 6.0F

        val opPosZDouble = PosZLong(3L) + PosZDouble(3.0)
        opPosZDouble shouldEqual 6.0
      }
    }

    object `when created with apply method` {

      def `should compile when 8 is passed in`: Unit = {
        "PosZLong(8)" should compile
        PosZLong(8).value shouldEqual 8L
        "PosZLong(8L)" should compile
        PosZLong(8L).value shouldEqual 8L
      }

      def `should compile when 0 is passed in`: Unit = {
        "PosZLong(0)" should compile
        PosZLong(0).value shouldEqual 0L
        "PosZLong(0L)" should compile
        PosZLong(0L).value shouldEqual 0L
      }

      def `should not compile when -8 is passed in`: Unit = {
        "PosZLong(-8)" shouldNot compile
        "PosZLong(-8L)" shouldNot compile
      }

      def `should not compile when x is passed in`: Unit = {
        val a: Int = -8
        "PosZLong(a)" shouldNot compile
        val b: Long = -8L
        "PosZLong(b)" shouldNot compile
      }
    }
    object `when specified as a plain-old Long` {

      def takesPosZLong(pos: PosZLong): Long = pos.value

      def `should compile when 8 is passed in`: Unit = {
        "takesPosZLong(8)" should compile
        takesPosZLong(8) shouldEqual 8L
        "takesPosZLong(8L)" should compile
        takesPosZLong(8L) shouldEqual 8L
      }

      def `should compile when 0 is passed in`: Unit = {
        "takesPosZLong(0)" should compile
        takesPosZLong(0) shouldEqual 0L
        "takesPosZLong(0L)" should compile
        takesPosZLong(0L) shouldEqual 0L
      }

      def `should not compile when -8 is passed in`: Unit = {
        "takesPosZLong(-8)" shouldNot compile
        "takesPosZLong(-8L)" shouldNot compile
      }

      def `should not compile when x is passed in`: Unit = {
        val x: Int = -8
        "takesPosZLong(x)" shouldNot compile
        val b: Long = -8L
        "takesPosZLong(b)" shouldNot compile
      }
    }
  }
}

