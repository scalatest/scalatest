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

class PosZFloatSpec extends Spec with Matchers/* with StrictCheckedEquality*/ {
  object `An PosZFloat` {
    object `should offer a from factory method that` {
      def `returns Some[PosZFloat] if the passed Float is greater than or equal to 0`
      {
        PosZFloat.from(0.0f).value.value shouldBe 0.0f
        PosZFloat.from(50.23f).value.value shouldBe 50.23f
        PosZFloat.from(100.0f).value.value shouldBe 100.0f
      }
      def `returns None if the passed Float is NOT greater than or equal to 0` {
        PosZFloat.from(-0.00001f) shouldBe None
        PosZFloat.from(-99.9f) shouldBe None
      }
    } 
    def `should have a pretty toString` {
      PosZFloat.from(42.0f).value.toString shouldBe "PosZFloat(42.0)"
    }
    def `should return the same type from its unary_+ method` {
      +PosZFloat(3.0F) shouldEqual PosZFloat(3.0F)
    } 
    def `should be automatically widened to compatible AnyVal targets` {
      "PosZFloat(3.0F): Int" shouldNot typeCheck
      "PosZFloat(3.0F): Long" shouldNot typeCheck
      (PosZFloat(3.0F): Float) shouldEqual 3.0F
      (PosZFloat(3.0F): Double) shouldEqual 3.0

      "PosZFloat(3.0F): PosInt" shouldNot typeCheck
      "PosZFloat(3.0F): PosLong" shouldNot typeCheck
      "PosZFloat(3.0F): PosFloat" shouldNot typeCheck
      "PosZFloat(3.0F): PosDouble" shouldNot typeCheck

      "PosZFloat(3.0F): PosZInt" shouldNot typeCheck
      "PosZFloat(3.0F): PosZLong" shouldNot typeCheck
      (PosZFloat(3.0F): PosZFloat) shouldEqual PosZFloat(3.0F)
      (PosZFloat(3.0F): PosZDouble) shouldEqual PosZDouble(3.0)
    }
    object `when a compatible AnyVal is passed to a + method invoked on it` {
      def `should give the same AnyVal type back at compile time, and correct value at runtime` {
        // When adding a "primitive"
        val opInt = PosZFloat(3.0F) + 3
        opInt shouldEqual 6.0F

        val opLong = PosZFloat(3.0F) + 3L
        opLong shouldEqual 6.0F

        val opFloat = PosZFloat(3.0F) + 3.0F
        opFloat shouldEqual 6.0F

        val opDouble = PosZFloat(3.0F) + 3.0
        opDouble shouldEqual 6.0

        // When adding a Pos*
        val opPosInt = PosZFloat(3.0F) + PosInt(3)
        opPosInt shouldEqual 6.0F

        val opPosLong = PosZFloat(3.0F) + PosLong(3L)
        opPosLong shouldEqual 6.0F

        val opPosFloat = PosZFloat(3.0F) + PosFloat(3.0F)
        opPosFloat shouldEqual 6.0F

        val opPosDouble = PosZFloat(3.0F) + PosDouble(3.0)
        opPosDouble shouldEqual 6.0

        // When adding a *PosZ
        val opPosZ = PosZFloat(3.0F) + PosZInt(3)
        opPosZ shouldEqual 6.0F

        val opPosZLong = PosZFloat(3.0F) + PosZLong(3L)
        opPosZLong shouldEqual 6.0F

        val opPosZFloat = PosZFloat(3.0F) + PosZFloat(3.0F)
        opPosZFloat shouldEqual 6.0F

        val opPosZDouble = PosZFloat(3.0F) + PosZDouble(3.0)
        opPosZDouble shouldEqual 6.0
      }
    }

    object `when created with apply method` {
  
      def `should compile when 8 is passed in`: Unit = {
        "PosZFloat(8)" should compile
        PosZFloat(8).value shouldEqual 8.0F
        "PosZFloat(8L)" should compile
        PosZFloat(8L).value shouldEqual 8.0F
        "PosZFloat(8.0F)" should compile
        PosZFloat(8.0F).value shouldEqual 8.0F
      }
  
      def `should compile when 0 is passed in`: Unit = {
        "PosZFloat(0)" should compile
        PosZFloat(0).value shouldEqual 0.0F
        "PosZFloat(0L)" should compile
        PosZFloat(0L).value shouldEqual 0.0F
        "PosZFloat(0.0F)" should compile
        PosZFloat(0.0F).value shouldEqual 0.0F
      }

      def `should not compile when -8 is passed in`: Unit = {
        "PosZFloat(-8)" shouldNot compile
        "PosZFloat(-8L)" shouldNot compile
        "PosZFloat(-8.0F)" shouldNot compile
      }

      def `should not compile when x is passed in`: Unit = {
        val a: Int = -8
        "PosZFloat(a)" shouldNot compile
        val b: Long = -8L
        "PosZFloat(b)" shouldNot compile
        val c: Float = -8.0F
        "PosZFloat(c)" shouldNot compile
      }
    }
    object `when specified as a plain-old Float` {

      def takesPosZFloat(pos: PosZFloat): Float = pos.value

      def `should compile when 8 is passed in`: Unit = {
        "takesPosZFloat(8)" should compile
        takesPosZFloat(8) shouldEqual 8.0F
        "takesPosZFloat(8L)" should compile
        takesPosZFloat(8L) shouldEqual 8.0F
        "takesPosZFloat(8.0F)" should compile
        takesPosZFloat(8.0F) shouldEqual 8.0F
      }

      def `should compile when 0 is passed in`: Unit = {
        "takesPosZFloat(0)" should compile
        takesPosZFloat(0) shouldEqual 0.0F
        "takesPosZFloat(0L)" should compile
        takesPosZFloat(0L) shouldEqual 0.0F
        "takesPosZFloat(0.0F)" should compile
        takesPosZFloat(0.0F) shouldEqual 0.0F
      }

      def `should not compile when -8 is passed in`: Unit = {
        "takesPosZFloat(-8)" shouldNot compile
        "takesPosZFloat(-8L)" shouldNot compile
        "takesPosZFloat(-8.0F)" shouldNot compile
      }

      def `should not compile when x is passed in`: Unit = {
        val x: Int = -8
        "takesPosZFloat(x)" shouldNot compile
        val b: Long = -8L
        "takesPosZFloat(b)" shouldNot compile
        val c: Float = -8.0F
        "takesPosZFloat(c)" shouldNot compile
      }
    }
  }
}

