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

class PosDoubleSpec extends Spec with Matchers/* with StrictCheckedEquality*/ {

  object `A PosDouble` {
    object `should offer a from factory method that` {
      def `returns Some[PosDouble] if the passed Double is greater than 0`
      {
        PosDouble.from(50.23).value.value shouldBe 50.23
        PosDouble.from(100.0).value.value shouldBe 100.0
      }
      def `returns None if the passed Double is NOT greater than 0`
      {
        PosDouble.from(0.0) shouldBe None
        PosDouble.from(-0.00001) shouldBe None
        PosDouble.from(-99.9) shouldBe None
      }
    } 
    def `should have a pretty toString` {
      PosDouble.from(42.0).value.toString shouldBe "PosDouble(42.0)"
    }
    def `should return the same type from its unary_+ method` {
      +PosDouble(3.0) shouldEqual PosDouble(3.0)
    } 
    def `should be automatically widened to compatible AnyVal targets` {
      "PosDouble(3.0): Int" shouldNot typeCheck
      "PosDouble(3.0): Long" shouldNot typeCheck
      "PosDouble(3.0): Float" shouldNot typeCheck
      (PosDouble(3.0): Double) shouldEqual 3.0

      "PosDouble(3.0): PosInt" shouldNot typeCheck
      "PosDouble(3.0): PosLong" shouldNot typeCheck
      "PosDouble(3.0): PosFloat" shouldNot typeCheck
      (PosDouble(3.0): PosDouble) shouldEqual PosDouble(3.0F)

      "PosDouble(3.0): PosZInt" shouldNot typeCheck
      "PosDouble(3.0): PosZLong" shouldNot typeCheck
      "PosDouble(3.0): PosZFloat" shouldNot typeCheck
      (PosDouble(3.0): PosZDouble) shouldEqual PosZDouble(3.0)
    }
    object `when a compatible AnyVal is passed to a + method invoked on it` {
      def `should give the same AnyVal type back at compile time, and correct value at runtime` {
        // When adding a "primitive"
        val opInt = PosDouble(3.0) + 3
        opInt shouldEqual 6.0

        val opLong = PosDouble(3.0) + 3L
        opLong shouldEqual 6.0

        val opFloat = PosDouble(3.0) + 3.0F
        opFloat shouldEqual 6.0

        val opDouble = PosDouble(3.0) + 3.0
        opDouble shouldEqual 6.0

        // When adding a Pos*
        val opPosInt = PosDouble(3.0) + PosInt(3)
        opPosInt shouldEqual 6.0

        val opPosLong = PosDouble(3.0) + PosLong(3L)
        opPosLong shouldEqual 6.0

        val opPosFloat = PosDouble(3.0) + PosFloat(3.0F)
        opPosFloat shouldEqual 6.0

        val opPosDouble = PosDouble(3.0) + PosDouble(3.0)
        opPosDouble shouldEqual 6.0

        // When adding a *PosZ
        val opPosZ = PosDouble(3.0) + PosZInt(3)
        opPosZ shouldEqual 6.0

        val opPosZLong = PosDouble(3.0) + PosZLong(3L)
        opPosZLong shouldEqual 6.0

        val opPosZFloat = PosDouble(3.0) + PosZFloat(3.0F)
        opPosZFloat shouldEqual 6.0

        val opPosZDouble = PosDouble(3.0) + PosZDouble(3.0)
        opPosZDouble shouldEqual 6.0
      }
    }

    object `when created with apply method` {

      def `should compile when 8 is passed in`: Unit = {
        "PosDouble(8)" should compile
        PosDouble(8).value shouldEqual 8.0
        "PosDouble(8L)" should compile
        PosDouble(8L).value shouldEqual 8.0
        "PosDouble(8.0F)" should compile
        PosDouble(8.0F).value shouldEqual 8.0
        "PosDouble(8.0)" should compile
        PosDouble(8.0).value shouldEqual 8.0
      }

      def `should not compile when 0 is passed in`: Unit = {
        "PosDouble(0)" shouldNot compile
        "PosDouble(0L)" shouldNot compile
        "PosDouble(0.0F)" shouldNot compile
        "PosDouble(0.0)" shouldNot compile
      }

      def `should not compile when -8 is passed in`: Unit = {
        "PosDouble(-8)" shouldNot compile
        "PosDouble(-8L)" shouldNot compile
        "PosDouble(-8.0F)" shouldNot compile
        "PosDouble(-8.0)" shouldNot compile
      }
      def `should not compile when x is passed in`: Unit = {
        val a: Int = -8
        "PosDouble(a)" shouldNot compile
        val b: Long = -8L
        "PosDouble(b)" shouldNot compile
        val c: Float = -8.0F
        "PosDouble(c)" shouldNot compile
        val d: Double = -8.0
        "PosDouble(d)" shouldNot compile
      }
    }
    object `when specified as a plain-old Double` {

      def takesPosDouble(pos: PosDouble): Double = pos.value

      def `should compile when 8 is passed in`: Unit = {
        "takesPosDouble(8)" should compile
        takesPosDouble(8) shouldEqual 8.0
        "takesPosDouble(8L)" should compile
        takesPosDouble(8L) shouldEqual 8.0
        "takesPosDouble(8.0F)" should compile
        takesPosDouble(8.0F) shouldEqual 8.0
        "takesPosDouble(8.0)" should compile
        takesPosDouble(8.0) shouldEqual 8.0
      }

      def `should not compile when 0 is passed in`: Unit = {
        "takesPosDouble(0)" shouldNot compile
        "takesPosDouble(0L)" shouldNot compile
        "takesPosDouble(0.0F)" shouldNot compile
        "takesPosDouble(0.0)" shouldNot compile
      }

      def `should not compile when -8 is passed in`: Unit = {
        "takesPosDouble(-8)" shouldNot compile
        "takesPosDouble(-8L)" shouldNot compile
        "takesPosDouble(-8.0F)" shouldNot compile
        "takesPosDouble(-8.0)" shouldNot compile
      }

      def `should not compile when x is passed in`: Unit = {
        val x: Int = -8
        "takesPosDouble(x)" shouldNot compile
        val b: Long = -8L
        "takesPosDouble(b)" shouldNot compile
        val c: Float = -8.0F
        "takesPosDouble(c)" shouldNot compile
        val d: Double = -8.0
        "takesPosDouble(d)" shouldNot compile
      }
    }
  }
}

