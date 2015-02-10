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

class PosZDoubleSpec extends Spec with Matchers with StrictCheckedEquality {
  object `A PosZDouble` {
    object `should offer a from factory method that` {
      def `returns Some[PosZDouble] if the passed Double is greater than or equal to 0`
      {
        PosZDouble.from(0.0).value.value shouldBe 0.0
        PosZDouble.from(50.23).value.value shouldBe 50.23
        PosZDouble.from(100.0).value.value shouldBe 100.0
      }
      def `returns None if the passed Double is NOT greater than or equal to 0`
      {
        PosZDouble.from(-0.00001) shouldBe None
        PosZDouble.from(-99.9) shouldBe None
      }
    } 
    def `should have a pretty toString` {
      PosZDouble.from(42.0).value.toString shouldBe "PosZDouble(42.0)"
    }
    def `should return the same type from its unary_+ method` {
      +PosZDouble(3.0) shouldEqual PosZDouble(3.0)
    } 
    def `should be automatically widened to compatible AnyVal targets` {
      "PosZDouble(3.0): Int" shouldNot typeCheck
      "PosZDouble(3.0): Long" shouldNot typeCheck
      "PosZDouble(3.0): Float" shouldNot typeCheck
      (PosZDouble(3.0): Double) shouldEqual 3.0

      "PosZDouble(3.0): PosInt" shouldNot typeCheck
      "PosZDouble(3.0): PosLong" shouldNot typeCheck
      "PosZDouble(3.0): PosFloat" shouldNot typeCheck
      "PosZDouble(3.0): PosDouble" shouldNot typeCheck

      "PosZDouble(3.0): PosZInt" shouldNot typeCheck
      "PosZDouble(3.0): PosZLong" shouldNot typeCheck
      "PosZDouble(3.0): PosZFloat" shouldNot typeCheck
      (PosZDouble(3.0): PosZDouble) shouldEqual PosZDouble(3.0)
    }
    object `when a compatible AnyVal is passed to a + method invoked on it` {
      def `should give the same AnyVal type back at compile time, and correct value at runtime` {
        // When adding a "primitive"
        val opInt = PosZDouble(3.0) + 3
        opInt shouldEqual 6.0

        val opLong = PosZDouble(3.0) + 3L
        opLong shouldEqual 6.0

        val opFloat = PosZDouble(3.0) + 3.0F
        opFloat shouldEqual 6.0

        val opDouble = PosZDouble(3.0) + 3.0
        opDouble shouldEqual 6.0

        // When adding a Pos*
        val opPosInt = PosZDouble(3.0) + PosInt(3)
        opPosInt shouldEqual 6.0

        val opPosLong = PosZDouble(3.0) + PosLong(3L)
        opPosLong shouldEqual 6.0

        val opPosFloat = PosZDouble(3.0) + PosFloat(3.0F)
        opPosFloat shouldEqual 6.0

        val opPosDouble = PosZDouble(3.0) + PosDouble(3.0)
        opPosDouble shouldEqual 6.0

        // When adding a *PosZ
        val opPosZ = PosZDouble(3.0) + PosZInt(3)
        opPosZ shouldEqual 6.0

        val opPosZLong = PosZDouble(3.0) + PosZLong(3L)
        opPosZLong shouldEqual 6.0

        val opPosZFloat = PosZDouble(3.0) + PosZFloat(3.0F)
        opPosZFloat shouldEqual 6.0

        val opPosZDouble = PosZDouble(3.0) + PosZDouble(3.0)
        opPosZDouble shouldEqual 6.0
      }
    }

    object `when created with apply method` {

      def `should compile when 8 is passed in`: Unit = {
        "PosZDouble(8)" should compile
        PosZDouble(8).value shouldEqual 8.0
        "PosZDouble(8L)" should compile
        PosZDouble(8L).value shouldEqual 8.0
        "PosZDouble(8.0F)" should compile
        PosZDouble(8.0F).value shouldEqual 8.0
        "PosZDouble(8.0)" should compile
        PosZDouble(8.0).value shouldEqual 8.0
      }

      def `should compile when 0 is passed in`: Unit = {
        "PosZDouble(0)" should compile
        PosZDouble(0).value shouldEqual 0.0
        "PosZDouble(0L)" should compile
        PosZDouble(0L).value shouldEqual 0.0
        "PosZDouble(0.0F)" should compile
        PosZDouble(0.0F).value shouldEqual 0.0
        "PosZDouble(0.0)" should compile
        PosZDouble(0.0).value shouldEqual 0.0
      }

      def `should not compile when -8 is passed in`: Unit = {
        "PosZDouble(-8)" shouldNot compile
        "PosZDouble(-8L)" shouldNot compile
        "PosZDouble(-8.0F)" shouldNot compile
        "PosZDouble(-8.0)" shouldNot compile
      }
      def `should not compile when x is passed in`: Unit = {
        val a: Int = -8
        "PosZDouble(a)" shouldNot compile
        val b: Long = -8L
        "PosZDouble(b)" shouldNot compile
        val c: Float = -8.0F
        "PosZDouble(c)" shouldNot compile
        val d: Double = -8.0
        "PosZDouble(d)" shouldNot compile
      }
    }
    object `when specified as a plain-old Double` {

      def takesPosZDouble(poz: PosZDouble): Double = poz.value

      def `should compile when 8 is passed in`: Unit = {
        "takesPosZDouble(8)" should compile
        takesPosZDouble(8) shouldEqual 8.0
        "takesPosZDouble(8L)" should compile
        takesPosZDouble(8L) shouldEqual 8.0
        "takesPosZDouble(8.0F)" should compile
        takesPosZDouble(8.0F) shouldEqual 8.0
        "takesPosZDouble(8.0)" should compile
        takesPosZDouble(8.0) shouldEqual 8.0
      }

      def `should compile when 0 is passed in`: Unit = {
        "takesPosZDouble(0)" should compile
        takesPosZDouble(0) shouldEqual 0.0
        "takesPosZDouble(0L)" should compile
        takesPosZDouble(0L) shouldEqual 0.0
        "takesPosZDouble(0.0F)" should compile
        takesPosZDouble(0.0F) shouldEqual 0.0
        "takesPosZDouble(0.0)" should compile
        takesPosZDouble(0.0) shouldEqual 0.0
      }

      def `should not compile when -8 is passed in`: Unit = {
        "takesPosZDouble(-8)" shouldNot compile
        "takesPosZDouble(-8L)" shouldNot compile
        "takesPosZDouble(-8.0F)" shouldNot compile
        "takesPosZDouble(-8.0)" shouldNot compile
      }

      def `should not compile when x is passed in`: Unit = {
        val x: Int = -8
        "takesPosZDouble(x)" shouldNot compile
        val b: Long = -8L
        "takesPosZDouble(b)" shouldNot compile
        val c: Float = -8.0F
        "takesPosZDouble(c)" shouldNot compile
        val d: Double = -8.0
        "takesPosZDouble(d)" shouldNot compile
      }
    }
  }
}

