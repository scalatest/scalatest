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

class PozDoubleSpec extends Spec with Matchers {
  object `A PozDouble` {
    object `should offer a from factory method that` {
      def `returns Some[PozDouble] if the passed Double is greater than or equal to 0`
      {
        PozDouble.from(0.0).value.value shouldBe 0.0
        PozDouble.from(50.23).value.value shouldBe 50.23
        PozDouble.from(100.0).value.value shouldBe 100.0
      }
      def `returns None if the passed Double is NOT greater than or equal to 0`
      {
        PozDouble.from(-0.00001) shouldBe None
        PozDouble.from(-99.9) shouldBe None
      }
    } 
    def `should have a pretty toString` {
      PozDouble.from(42.0).value.toString shouldBe "PozDouble(42.0)"
    }
    def `should be automatically widened to compatible AnyVal targets` {
      (PozDouble(3.0): Double) shouldEqual 3.0
    }
    object `when a compatible AnyVal is passed to a + method invoked on it` {
      def `should give the same AnyVal type back at compile time, and correct value at runtime` {
        // When adding a "primitive"
        val opInt = PozDouble(3.0) + 3
        opInt shouldEqual 6.0

        val opLong = PozDouble(3.0) + 3L
        opLong shouldEqual 6.0

        val opFloat = PozDouble(3.0) + 3.0F
        opFloat shouldEqual 6.0

        val opDouble = PozDouble(3.0) + 3.0
        opDouble shouldEqual 6.0

        // When adding a Pos*
        val opPosInt = PozDouble(3.0) + PosInt(3)
        opPosInt shouldEqual 6.0

        val opPosLong = PozDouble(3.0) + PosLong(3L)
        opPosLong shouldEqual 6.0

        val opPosFloat = PozDouble(3.0) + PosFloat(3.0F)
        opPosFloat shouldEqual 6.0

        val opPosDouble = PozDouble(3.0) + PosDouble(3.0)
        opPosDouble shouldEqual 6.0

        // When adding a *Poz
        val opPoz = PozDouble(3.0) + PozInt(3)
        opPoz shouldEqual 6.0

        val opPozLong = PozDouble(3.0) + PozLong(3L)
        opPozLong shouldEqual 6.0

        val opPozFloat = PozDouble(3.0) + PozFloat(3.0F)
        opPozFloat shouldEqual 6.0

        val opPozDouble = PozDouble(3.0) + PozDouble(3.0)
        opPozDouble shouldEqual 6.0
      }
    }

    object `when created with apply method` {

      def `should compile when 8 is passed in`: Unit = {
        "PozDouble(8)" should compile
        PozDouble(8).value shouldEqual 8
        "PozDouble(8L)" should compile
        PozDouble(8L).value shouldEqual 8
        "PozDouble(8.0F)" should compile
        PozDouble(8.0F).value shouldEqual 8.0F
        "PozDouble(8.0)" should compile
        PozDouble(8.0).value shouldEqual 8.0
      }

      def `should compile when 0 is passed in`: Unit = {
        "PozDouble(0)" should compile
        PozDouble(0).value shouldEqual 0
        "PozDouble(0L)" should compile
        PozDouble(0L).value shouldEqual 0
        "PozDouble(0.0F)" should compile
        PozDouble(0.0F).value shouldEqual 0.0F
        "PozDouble(0.0)" should compile
        PozDouble(0.0).value shouldEqual 0.0
      }

      def `should not compile when -8 is passed in`: Unit = {
        "PozDouble(-8)" shouldNot compile
        "PozDouble(-8L)" shouldNot compile
        "PozDouble(-8.0F)" shouldNot compile
        "PozDouble(-8.0)" shouldNot compile
      }
      def `should not compile when x is passed in`: Unit = {
        val a: Int = -8
        "PozDouble(a)" shouldNot compile
        val b: Long = -8L
        "PozDouble(b)" shouldNot compile
        val c: Float = -8.0F
        "PozDouble(c)" shouldNot compile
        val d: Double = -8.0
        "PozDouble(d)" shouldNot compile
      }
    }
  }
}

