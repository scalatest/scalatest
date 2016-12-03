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

class NumCharSpec extends FunSpec with Matchers/* with StrictCheckedEquality*/ {
  describe("A NumChar") {
    describe("should offer a from factory method that") {
      it("returns Some[NumChar] if the passed Char is between '0' and '9'") {
        NumChar.from('0').value.value shouldBe '0'
        NumChar.from('5').value.value shouldBe '5'
        NumChar.from('9').value.value shouldBe '9'
      }
      it("returns None if the passed Char is NOT between '0' and '9'") {
        NumChar.from('a') shouldBe None
        NumChar.from('z') shouldBe None
        NumChar.from('A') shouldBe None
        NumChar.from(0) shouldBe None
        NumChar.from(-1.toChar) shouldBe None
      }
    } 
    describe("should offer an ensuringValid factory method that") {
      it("returns NumChar if the passed Char is between '0' and '9'") {
        NumChar.ensuringValid('0').value shouldBe '0'
        NumChar.ensuringValid('5').value shouldBe '5'
        NumChar.ensuringValid('9').value shouldBe '9'
      }
      it("throws AssertionError if the passed Char is NOT between '0' and '9'") {
        an [AssertionError] should be thrownBy NumChar.ensuringValid('a')
        an [AssertionError] should be thrownBy NumChar.ensuringValid('z')
        an [AssertionError] should be thrownBy NumChar.ensuringValid('A')
        an [AssertionError] should be thrownBy NumChar.ensuringValid(0)
        an [AssertionError] should be thrownBy NumChar.ensuringValid(-1.toChar)
      }
    } 
    it("should define min and max values") {
      NumChar.MinValue shouldBe '0'
      NumChar.MaxValue shouldBe '9'
    } 
    it("should define min and max methods") {
      NumChar('0') min NumChar('1') shouldBe NumChar('0')
      NumChar('0') max NumChar('1') shouldBe NumChar('1')
      NumChar('8') min NumChar('9') shouldBe NumChar('8')
      NumChar('8') max NumChar('9') shouldBe NumChar('9')
    } 
    it("should define methods to convert to the numeric value the character represents") {
      NumChar('0').asDigit shouldBe 0
      NumChar('9').asDigit shouldBe 9
      NumChar('1').asDigitPosInt shouldBe PosInt(1)
      NumChar('9').asDigitPosInt shouldBe PosInt(9)
      NumChar('0').asDigitPosZInt shouldBe PosZInt(0)
      NumChar('9').asDigitPosZInt shouldBe PosZInt(9)
    } 
    it("should have a pretty toString") {
      NumChar.from('0').value.toString shouldBe "NumChar(0)"
      NumChar.from('9').value.toString shouldBe "NumChar(9)"
    }
    it("should return the same type from its unary_+ method") {
      +NumChar('3') shouldEqual NumChar('3')
    } 
    it("should be automatically widened to compatible AnyVal targets") {
      (NumChar('3'): Int) shouldEqual '3'.toInt
      (NumChar('3'): Long) shouldEqual '3'.toLong
      (NumChar('3'): Float) shouldEqual '3'.toFloat
      (NumChar('3'): Double) shouldEqual '3'.toDouble

      (NumChar('3'): PosInt) shouldEqual PosInt.from('3'.toInt).get
      (NumChar('3'): PosLong) shouldEqual PosLong.from('3'.toLong).get
      (NumChar('3'): PosFloat) shouldEqual PosFloat.from('3'.toFloat).get
      (NumChar('3'): PosDouble) shouldEqual PosDouble.from('3'.toDouble).get

      (NumChar('3'): PosZInt) shouldEqual PosZInt.from('3'.toInt).get
      (NumChar('3'): PosZLong) shouldEqual PosZLong.from('3'.toLong).get
      (NumChar('3'): PosZFloat) shouldEqual PosZFloat.from('3'.toFloat).get
      (NumChar('3'): PosZDouble) shouldEqual PosZDouble.from('3'.toDouble).get
    }
    describe("when a compatible AnyVal is passed to a + method invoked on it") {
      it("should give the same AnyVal type back at compile time, and correct value at runtime") {
        // When adding a "primitive"
        val opInt = NumChar('3') + 3
        opInt shouldEqual '3'.toInt + 3

        val opLong = NumChar('3') + 3L
        opLong shouldEqual '3'.toLong + 3L

        val opFloat = NumChar('3') + 3.0F
        opFloat shouldEqual '3'.toFloat + 3.0F

        val opDouble = NumChar('3') + 3.0
        opDouble shouldEqual '3'.toDouble + 3.0

        // When adding a Pos*
        val opPosInt = NumChar('3') + PosInt(3)
        opPosInt shouldEqual '3'.toInt + 3

        val opPosLong = NumChar('3') + PosLong(3L)
        opPosLong shouldEqual '3'.toInt + 3L

        val opPosFloat = NumChar('3') + PosFloat(3.0F)
        opPosFloat shouldEqual '3'.toInt + 3.0F

        val opPosDouble = NumChar('3') + PosDouble(3.0)
        opPosDouble shouldEqual '3'.toInt + 3.0

        // When adding a *PosZ
        val opPosZ = NumChar('3') + PosZInt(3)
        opPosZ shouldEqual '3'.toInt + 3

        val opPosZLong = NumChar('3') + PosZLong(3L)
        opPosZLong shouldEqual '3'.toInt + 3L

        val opPosZFloat = NumChar('3') + PosZFloat(3.0F)
        opPosZFloat shouldEqual '3'.toInt + 3.0F

        val opPosZDouble = NumChar('3') + PosZDouble(3.0)
        opPosZDouble shouldEqual '3'.toInt + 3.0
      }
    }

    describe("when created with apply method") {

      it("should compile when '8' is passed in") {
        "NumChar('8')" should compile
        NumChar('8').value shouldEqual '8'
      }

      it("should not compile when 'A' is passed in") {
        "NumChar('A')" shouldNot compile
      }

      it("should not compile when -8 is passed in") {
        "NumChar(-8.toChar)" shouldNot compile
      }

      it("should not compile when x is passed in") {
        val x: Char = 'A'
        "NumChar(x)" shouldNot compile
      }
    }
    describe("when specified as a plain-old Char") {

      def takesNumChar(dig: NumChar): Char = dig.value

      it("should compile when '8' is passed in") {
        "takesNumChar('8')" should compile
        takesNumChar('8') shouldEqual '8'
      }

      it("should not compile when 'x' is passed in") {
        "takesNumChar('x')" shouldNot compile
      }

      it("should not compile when -8 is passed in") {
        "takesNumChar(-8.toChar)" shouldNot compile
      }

      it("should not compile when x is passed in") {
        val x: Int = 'x'
        "takesNumChar(x)" shouldNot compile
      }
    }
  }
}

