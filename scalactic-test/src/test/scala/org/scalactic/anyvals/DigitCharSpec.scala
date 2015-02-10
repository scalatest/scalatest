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

class DigitCharSpec extends Spec with Matchers/* with StrictCheckedEquality*/ {
  object `A DigitChar` {
    object `should offer a from factory method that` {
      def `returns Some[DigitChar] if the passed Char is between '0' and '9'` {
        DigitChar.from('0').value.value shouldBe '0'
        DigitChar.from('5').value.value shouldBe '5'
        DigitChar.from('9').value.value shouldBe '9'
      }
      def `returns None if the passed Char is NOT between '0' and '9'` {
        DigitChar.from('a') shouldBe None
        DigitChar.from('z') shouldBe None
        DigitChar.from('A') shouldBe None
        DigitChar.from(0) shouldBe None
        DigitChar.from(-1.toChar) shouldBe None
      }
    } 
    def `should define min and max values` {
      DigitChar.MinValue shouldBe '0'
      DigitChar.MaxValue shouldBe '9'
    } 
    def `should define min and max methods` {
      DigitChar('0') min DigitChar('1') shouldBe DigitChar('0')
      DigitChar('0') max DigitChar('1') shouldBe DigitChar('1')
      DigitChar('8') min DigitChar('9') shouldBe DigitChar('8')
      DigitChar('8') max DigitChar('9') shouldBe DigitChar('9')
    } 
    def `should define methods to convert to the numeric value the character represents` {
      DigitChar('0').asDigit shouldBe 0
      DigitChar('9').asDigit shouldBe 9
      DigitChar('1').asDigitPosInt shouldBe PosInt(1)
      DigitChar('9').asDigitPosInt shouldBe PosInt(9)
      DigitChar('0').asDigitPosZInt shouldBe PosZInt(0)
      DigitChar('9').asDigitPosZInt shouldBe PosZInt(9)
    } 
    def `should have a pretty toString` {
      DigitChar.from('0').value.toString shouldBe "DigitChar(0)"
      DigitChar.from('9').value.toString shouldBe "DigitChar(9)"
    }
    def `should return the same type from its unary_+ method` {
      +DigitChar('3') shouldEqual DigitChar('3')
    } 
    def `should be automatically widened to compatible AnyVal targets` {
      (DigitChar('3'): Int) shouldEqual '3'.toInt
      (DigitChar('3'): Long) shouldEqual '3'.toLong
      (DigitChar('3'): Float) shouldEqual '3'.toFloat
      (DigitChar('3'): Double) shouldEqual '3'.toDouble

      (DigitChar('3'): PosInt) shouldEqual PosInt.from('3'.toInt).get
      (DigitChar('3'): PosLong) shouldEqual PosLong.from('3'.toLong).get
      (DigitChar('3'): PosFloat) shouldEqual PosFloat.from('3'.toFloat).get
      (DigitChar('3'): PosDouble) shouldEqual PosDouble.from('3'.toDouble).get

      (DigitChar('3'): PosZInt) shouldEqual PosZInt.from('3'.toInt).get
      (DigitChar('3'): PosZLong) shouldEqual PosZLong.from('3'.toLong).get
      (DigitChar('3'): PosZFloat) shouldEqual PosZFloat.from('3'.toFloat).get
      (DigitChar('3'): PosZDouble) shouldEqual PosZDouble.from('3'.toDouble).get
    }
    object `when a compatible AnyVal is passed to a + method invoked on it` {
      def `should give the same AnyVal type back at compile time, and correct value at runtime` {
        // When adding a "primitive"
        val opInt = DigitChar('3') + 3
        opInt shouldEqual '3'.toInt + 3

        val opLong = DigitChar('3') + 3L
        opLong shouldEqual '3'.toLong + 3L

        val opFloat = DigitChar('3') + 3.0F
        opFloat shouldEqual '3'.toFloat + 3.0F

        val opDouble = DigitChar('3') + 3.0
        opDouble shouldEqual '3'.toDouble + 3.0

        // When adding a Pos*
        val opPosInt = DigitChar('3') + PosInt(3)
        opPosInt shouldEqual '3'.toInt + 3

        val opPosLong = DigitChar('3') + PosLong(3L)
        opPosLong shouldEqual '3'.toInt + 3L

        val opPosFloat = DigitChar('3') + PosFloat(3.0F)
        opPosFloat shouldEqual '3'.toInt + 3.0F

        val opPosDouble = DigitChar('3') + PosDouble(3.0)
        opPosDouble shouldEqual '3'.toInt + 3.0

        // When adding a *PosZ
        val opPosZ = DigitChar('3') + PosZInt(3)
        opPosZ shouldEqual '3'.toInt + 3

        val opPosZLong = DigitChar('3') + PosZLong(3L)
        opPosZLong shouldEqual '3'.toInt + 3L

        val opPosZFloat = DigitChar('3') + PosZFloat(3.0F)
        opPosZFloat shouldEqual '3'.toInt + 3.0F

        val opPosZDouble = DigitChar('3') + PosZDouble(3.0)
        opPosZDouble shouldEqual '3'.toInt + 3.0
      }
    }

    object `when created with apply method` {

      def `should compile when '8' is passed in`: Unit = {
        "DigitChar('8')" should compile
        DigitChar('8').value shouldEqual '8'
      }

      def `should not compile when 'A' is passed in`: Unit = {
        "DigitChar('A')" shouldNot compile
      }

      def `should not compile when -8 is passed in`: Unit = {
        "DigitChar(-8.toChar)" shouldNot compile
      }

      def `should not compile when x is passed in`: Unit = {
        val x: Char = 'A'
        "DigitChar(x)" shouldNot compile
      }
    }
    object `when specified as a plain-old Char` {

      def takesDigitChar(dig: DigitChar): Char = dig.value

      def `should compile when '8' is passed in`: Unit = {
        "takesDigitChar('8')" should compile
        takesDigitChar('8') shouldEqual '8'
      }

      def `should not compile when 'x' is passed in`: Unit = {
        "takesDigitChar('x')" shouldNot compile
      }

      def `should not compile when -8 is passed in`: Unit = {
        "takesDigitChar(-8.toChar)" shouldNot compile
      }

      def `should not compile when x is passed in`: Unit = {
        val x: Int = 'x'
        "takesDigitChar(x)" shouldNot compile
      }
    }
  }
}

