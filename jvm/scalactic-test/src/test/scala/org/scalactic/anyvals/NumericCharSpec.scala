/*
 * Copyright 2001-2024 Artima, Inc.
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
import org.scalactic.Equality
import org.scalatest.prop.GeneratorDrivenPropertyChecks
//import org.scalactic.StrictCheckedEquality

import scala.util.{Failure, Success, Try}
import TryValues._
import org.scalactic.{Pass, Fail}
import org.scalactic.{Good, Bad}

trait NumericCharSpecSupport {

  implicit def tryEquality[T]: Equality[Try[T]] = new Equality[Try[T]] {
    override def areEqual(a: Try[T], b: Any): Boolean = a match {
      case Success(double: Double) if double.isNaN =>  // This is because in scala.js x/0 results to NaN not ArithmetricException like in jvm, and we need to make sure Success(NaN) == Success(NaN) is true to pass the test.
        b match {
          case Success(bDouble: Double) if bDouble.isNaN => true
          case _ => false
        }
        // I needed this because with GenDrivenPropertyChecks, got:
        // [info] - should offer a '%' method that is consistent with Int *** FAILED ***
        // [info]   Success(NaN) did not equal Success(NaN) (PosIntExperiment.scala:498)
      case Success(float: Float) if float.isNaN =>
        b match {
          case Success(bFloat: Float) if bFloat.isNaN => true
          case _ => false
        }
      case _: Success[_] => a == b
      case Failure(ex) => b match {
        case _: Success[_] => false
        case Failure(otherEx) => ex.getClass == otherEx.getClass && ex.getMessage == otherEx.getMessage
        case _ => false
      }
    }
  }

}


class NumericCharSpec extends funspec.AnyFunSpec with matchers.should.Matchers with GeneratorDrivenPropertyChecks with NumericCharSpecSupport/* with StrictCheckedEquality*/ {
  describe("A NumericChar") {
    describe("should offer a from factory method that") {
      it("returns Some[NumericChar] if the passed Char is between '0' and '9'") {
        NumericChar.from('0').value.value shouldBe '0'
        NumericChar.from('5').value.value shouldBe '5'
        NumericChar.from('9').value.value shouldBe '9'
      }
      it("returns None if the passed Char is NOT between '0' and '9'") {
        NumericChar.from('a') shouldBe None
        NumericChar.from('z') shouldBe None
        NumericChar.from('A') shouldBe None
        NumericChar.from(0) shouldBe None
        NumericChar.from(-1.toChar) shouldBe None
      }
    } 
    describe("should offer an ensuringValid factory method that") {
      it("returns NumericChar if the passed Char is between '0' and '9'") {
        NumericChar.ensuringValid('0').value shouldBe '0'
        NumericChar.ensuringValid('5').value shouldBe '5'
        NumericChar.ensuringValid('9').value shouldBe '9'
      }
      it("throws AssertionError if the passed Char is NOT between '0' and '9'") {
        an [AssertionError] should be thrownBy NumericChar.ensuringValid('a')
        an [AssertionError] should be thrownBy NumericChar.ensuringValid('z')
        an [AssertionError] should be thrownBy NumericChar.ensuringValid('A')
        an [AssertionError] should be thrownBy NumericChar.ensuringValid(0)
        an [AssertionError] should be thrownBy NumericChar.ensuringValid(-1.toChar)
      }
    } 
    it("should define min and max values") {
      NumericChar.MinValue shouldBe NumericChar.ensuringValid('0')
      NumericChar.MaxValue shouldBe NumericChar.ensuringValid('9')
    } 
    it("should define min and max methods") {
      NumericChar('0') min NumericChar('1') shouldBe NumericChar('0')
      NumericChar('0') max NumericChar('1') shouldBe NumericChar('1')
      NumericChar('8') min NumericChar('9') shouldBe NumericChar('8')
      NumericChar('8') max NumericChar('9') shouldBe NumericChar('9')
    } 
    it("should define methods to convert to the numeric value the character represents") {
      NumericChar('0').asDigit shouldBe 0
      NumericChar('9').asDigit shouldBe 9
      NumericChar('0').asDigitPosZInt shouldBe PosZInt(0)
      NumericChar('9').asDigitPosZInt shouldBe PosZInt(9)
    } 
    it("should have a pretty toString") {
      NumericChar.from('0').value.toString shouldBe "NumericChar('0')"
      NumericChar.from('9').value.toString shouldBe "NumericChar('9')"
    }
    it("should return the same type from its unary_+ method") {
      +NumericChar('3') shouldEqual NumericChar('3')
    }

    it("should be automatically widened to compatible AnyVal targets") {
      (NumericChar('3'): Int) shouldEqual '3'.toInt
      (NumericChar('3'): Long) shouldEqual '3'.toLong
      (NumericChar('3'): Float) shouldEqual '3'.toFloat
      (NumericChar('3'): Double) shouldEqual '3'.toDouble

      (NumericChar('3'): PosInt) shouldEqual PosInt.from('3'.toInt).get
      (NumericChar('3'): PosLong) shouldEqual PosLong.from('3'.toLong).get
      (NumericChar('3'): PosFloat) shouldEqual PosFloat.from('3'.toFloat).get
      (NumericChar('3'): PosDouble) shouldEqual PosDouble.from('3'.toDouble).get

      (NumericChar('3'): PosZInt) shouldEqual PosZInt.from('3'.toInt).get
      (NumericChar('3'): PosZLong) shouldEqual PosZLong.from('3'.toLong).get
      (NumericChar('3'): PosZFloat) shouldEqual PosZFloat.from('3'.toFloat).get
      (NumericChar('3'): PosZDouble) shouldEqual PosZDouble.from('3'.toDouble).get
    }
    describe("when a compatible AnyVal is passed to a + method invoked on it") {
      it("should give the same AnyVal type back at compile time, and correct value at runtime") {
        // When adding a "primitive"
        val opInt = NumericChar('3') + 3
        opInt shouldEqual '3'.toInt + 3

        val opLong = NumericChar('3') + 3L
        opLong shouldEqual '3'.toLong + 3L

        val opFloat = NumericChar('3') + 3.0F
        opFloat shouldEqual '3'.toFloat + 3.0F

        val opDouble = NumericChar('3') + 3.0
        opDouble shouldEqual '3'.toDouble + 3.0

        // When adding a Pos*
        val opPosInt = NumericChar('3') + PosInt(3)
        opPosInt shouldEqual '3'.toInt + 3

        val opPosLong = NumericChar('3') + PosLong(3L)
        opPosLong shouldEqual '3'.toInt + 3L

        val opPosFloat = NumericChar('3') + PosFloat(3.0F)
        opPosFloat shouldEqual '3'.toInt + 3.0F

        val opPosDouble = NumericChar('3') + PosDouble(3.0)
        opPosDouble shouldEqual '3'.toInt + 3.0

        // When adding a *PosZ
        val opPosZ = NumericChar('3') + PosZInt(3)
        opPosZ shouldEqual '3'.toInt + 3

        val opPosZLong = NumericChar('3') + PosZLong(3L)
        opPosZLong shouldEqual '3'.toInt + 3L

        val opPosZFloat = NumericChar('3') + PosZFloat(3.0F)
        opPosZFloat shouldEqual '3'.toInt + 3.0F

        val opPosZDouble = NumericChar('3') + PosZDouble(3.0)
        opPosZDouble shouldEqual '3'.toInt + 3.0
      }
    }

    describe("when created with apply method") {

      it("should compile when '8' is passed in") {
        "NumericChar('8')" should compile
        NumericChar('8').value shouldEqual '8'
      }

      it("should not compile when 'A' is passed in") {
        "NumericChar('A')" shouldNot compile
      }

      it("should not compile when -8 is passed in") {
        "NumericChar(-8.toChar)" shouldNot compile
      }

      it("should not compile when x is passed in") {
        val x: Char = 'A'
        "NumericChar(x)" shouldNot compile
      }
    }
    describe("when specified as a plain-old Char") {

      def takesNumericChar(dig: NumericChar): Char = dig.value

      it("should compile when '8' is passed in") {
        "takesNumericChar('8')" should compile
        takesNumericChar('8') shouldEqual '8'
      }

      it("should not compile when 'x' is passed in") {
        "takesNumericChar('x')" shouldNot compile
      }

      it("should not compile when -8 is passed in") {
        "takesNumericChar(-8.toChar)" shouldNot compile
      }

      it("should not compile when x is passed in") {
        val x: Int = 'x'
        "takesNumericChar(x)" shouldNot compile
      }
    }
    describe("should offer a goodOrElse factory method that") {
      it ("returns a NumericChar wrapped in a Good if the given Char "+
          "is between '0' and '9'") {
        NumericChar.goodOrElse('0')(c => c) shouldBe
          Good(NumericChar('0'))
        NumericChar.goodOrElse('5')(c => c) shouldBe 
          Good(NumericChar('5'))
        NumericChar.goodOrElse('9')(c => c) shouldBe 
          Good(NumericChar('9'))
      }
      it ("returns an error value produced by passing the given Char to "+
          "the given function if the passed Char is not between '0' and '9'") {
        NumericChar.goodOrElse('a')(c => s"'$c' did not taste good") shouldBe
          Bad("'a' did not taste good")
        NumericChar.goodOrElse('?')(c => s"'$c' did not taste good") shouldBe 
          Bad("'?' did not taste good")
      }
    }
    describe("should offer a passOrElse factory method that") {
      it ("returns a Pass if the given Char is between '0' and '9'") {
        NumericChar.passOrElse('0')(i => i) shouldBe Pass
        NumericChar.passOrElse('1')(i => i) shouldBe Pass
        NumericChar.passOrElse('8')(i => i) shouldBe Pass
        NumericChar.passOrElse('9')(i => i) shouldBe Pass
      }
      it (" returns an error value produced by passing the given Char to "+
          "the given function if the passed Char is NOT between '0' and '9',"+
          "wrapped in a Fail") {
        NumericChar.passOrElse('a')(i => s"'$i' is not so good") shouldBe
          Fail("'a' is not so good")
        NumericChar.passOrElse('?')(i => s"'$i' is not so good") shouldBe 
          Fail("'?' is not so good")
        NumericChar.passOrElse('.')(i => s"'$i' is not so good") shouldBe 
          Fail("'.' is not so good")
        NumericChar.passOrElse('X')(i => s"'$i' is not so good") shouldBe 
          Fail("'X' is not so good")
      }
    }
    describe("should offer a rightOrElse factory method that") {
      it("returns a NumericChar wrapped in a Right if the given Char is "+
         "between '0' and '9'") {
        NumericChar.rightOrElse('0')(i => i) shouldBe
          Right(NumericChar('0'))
        NumericChar.rightOrElse('1')(i => i) shouldBe 
          Right(NumericChar('1'))
        NumericChar.rightOrElse('8')(i => i) shouldBe 
          Right(NumericChar('8'))
        NumericChar.rightOrElse('9')(i => i) shouldBe 
          Right(NumericChar('9'))
      }
      it ("returns an error value produced by passing the given Char to "+
          "the given function if the passed Char does not contain only "+
          "numeric characters, wrapped in a Left") {
        NumericChar.rightOrElse('a')(i => s"'$i' is not so good") shouldBe
          Left("'a' is not so good")
        NumericChar.rightOrElse('*')(i => s"'$i' is not so good") shouldBe 
          Left("'*' is not so good")
        NumericChar.rightOrElse('!')(i => s"'$i' is not so good") shouldBe 
          Left("'!' is not so good")
      }
    }
    describe("should offer a tryingValid factory method that") {
      it ("returns a NumericChar wrapped in a Success if the passed Char "+
          "is between '0' and '9'") {
        NumericChar.tryingValid('0').success.value.value shouldBe '0'
        NumericChar.tryingValid('2').success.value.value shouldBe '2'
        NumericChar.tryingValid('7').success.value.value shouldBe '7'
        NumericChar.tryingValid('9').success.value.value shouldBe '9'
      }
      it (" returns an AssertionError wrapped in a Failure if the passed "+
          "Char does not contain only numeric characters") {
        NumericChar.tryingValid('a').failure.exception shouldBe
          an [AssertionError]
        NumericChar.tryingValid('X').failure.exception shouldBe 
          an [AssertionError]
        NumericChar.tryingValid('^').failure.exception shouldBe 
          an [AssertionError]
        NumericChar.tryingValid('o').failure.exception shouldBe 
          an [AssertionError]
      }
    }
    describe("should offer a fromOrElse factory method that") {
      it("returns a NumericChar if the passed Char is numeric") {
        NumericChar.fromOrElse('0', NumericChar('1')).value shouldBe '0'
        NumericChar.fromOrElse('3', NumericChar('1')).value shouldBe '3'
        NumericChar.fromOrElse('6', NumericChar('1')).value shouldBe '6'
        NumericChar.fromOrElse('9', NumericChar('1')).value shouldBe '9'
      }
      it("returns a given default if the passed Char is NOT numeric") {
        NumericChar.fromOrElse('a', NumericChar('1')).value shouldBe '1'
        NumericChar.fromOrElse('&', NumericChar('1')).value shouldBe '1'
        NumericChar.fromOrElse('(', NumericChar('1')).value shouldBe '1'
      }
    } 
    describe("should offer an isValid predicate method that") {
      it("returns true if the passed Char is between '0' and '9'") {
        NumericChar.isValid('0') shouldBe true
        NumericChar.isValid('5') shouldBe true
        NumericChar.isValid('6') shouldBe true
        NumericChar.isValid('9') shouldBe true
        NumericChar.isValid('a') shouldBe false
        NumericChar.isValid('A') shouldBe false
        NumericChar.isValid('@') shouldBe false
      }
    }
    describe("should offer a toByte method that") {
      it("returns correct Byte value") {
        NumericChar('0').toByte shouldBe 48
        NumericChar('1').toByte shouldBe 49
        NumericChar('2').toByte shouldBe 50
        NumericChar('3').toByte shouldBe 51
        NumericChar('4').toByte shouldBe 52
        NumericChar('5').toByte shouldBe 53
        NumericChar('6').toByte shouldBe 54
        NumericChar('7').toByte shouldBe 55
        NumericChar('8').toByte shouldBe 56
        NumericChar('9').toByte shouldBe 57
      }
    }
    describe("should offer a toShort method that") {
      it("returns correct Short value") {
        NumericChar('0').toShort shouldBe 48
        NumericChar('1').toShort shouldBe 49
        NumericChar('2').toShort shouldBe 50
        NumericChar('3').toShort shouldBe 51
        NumericChar('4').toShort shouldBe 52
        NumericChar('5').toShort shouldBe 53
        NumericChar('6').toShort shouldBe 54
        NumericChar('7').toShort shouldBe 55
        NumericChar('8').toShort shouldBe 56
        NumericChar('9').toShort shouldBe 57
      }
    }
    describe("should offer a toChar method that") {
      it("returns correct Char value") {
        NumericChar('0').toChar shouldBe '0'
        NumericChar('1').toChar shouldBe '1'
        NumericChar('2').toChar shouldBe '2'
        NumericChar('3').toChar shouldBe '3'
        NumericChar('4').toChar shouldBe '4'
        NumericChar('5').toChar shouldBe '5'
        NumericChar('6').toChar shouldBe '6'
        NumericChar('7').toChar shouldBe '7'
        NumericChar('8').toChar shouldBe '8'
        NumericChar('9').toChar shouldBe '9'
      }
    }
    describe("should offer a toInt method that") {
      it("returns correct Int value") {
        NumericChar('0').toInt shouldBe 48
        NumericChar('1').toInt shouldBe 49
        NumericChar('2').toInt shouldBe 50
        NumericChar('3').toInt shouldBe 51
        NumericChar('4').toInt shouldBe 52
        NumericChar('5').toInt shouldBe 53
        NumericChar('6').toInt shouldBe 54
        NumericChar('7').toInt shouldBe 55
        NumericChar('8').toInt shouldBe 56
        NumericChar('9').toInt shouldBe 57
      }
    }
    describe("should offer a toLong method that") {
      it("returns correct Long value") {
        NumericChar('0').toLong shouldBe 48L
        NumericChar('1').toLong shouldBe 49L
        NumericChar('2').toLong shouldBe 50L
        NumericChar('3').toLong shouldBe 51L
        NumericChar('4').toLong shouldBe 52L
        NumericChar('5').toLong shouldBe 53L
        NumericChar('6').toLong shouldBe 54L
        NumericChar('7').toLong shouldBe 55L
        NumericChar('8').toLong shouldBe 56L
        NumericChar('9').toLong shouldBe 57L
      }
    }

    describe("should offer a toFloat method that") {
      it("returns correct Float value") {
        NumericChar('0').toFloat shouldBe 48.0f
        NumericChar('1').toFloat shouldBe 49.0f
        NumericChar('2').toFloat shouldBe 50.0f
        NumericChar('3').toFloat shouldBe 51.0f
        NumericChar('4').toFloat shouldBe 52.0f
        NumericChar('5').toFloat shouldBe 53.0f
        NumericChar('6').toFloat shouldBe 54.0f
        NumericChar('7').toFloat shouldBe 55.0f
        NumericChar('8').toFloat shouldBe 56.0f
        NumericChar('9').toFloat shouldBe 57.0f
      }
    }

    describe("should offer a toDouble method that") {
      it("returns correct Double value") {
        NumericChar('0').toFloat shouldBe 48.0
        NumericChar('1').toFloat shouldBe 49.0
        NumericChar('2').toFloat shouldBe 50.0
        NumericChar('3').toFloat shouldBe 51.0
        NumericChar('4').toFloat shouldBe 52.0
        NumericChar('5').toFloat shouldBe 53.0
        NumericChar('6').toFloat shouldBe 54.0
        NumericChar('7').toFloat shouldBe 55.0
        NumericChar('8').toFloat shouldBe 56.0
        NumericChar('9').toFloat shouldBe 57.0
      }
    }

    describe("should offer 'min' and 'max' methods that") {
      it("are consistent with Char") {
        forAll { (p1: NumericChar, p2: NumericChar) =>
          p1.max(p2).toChar shouldEqual p1.toChar.max(p2.toChar)
          p1.min(p2).toChar shouldEqual p1.toChar.min(p2.toChar)
        }
      }
    }

    describe("should offer asDigit method that") {
      it("is consistent with Char's asDigit") {
        forAll { (p1: NumericChar) =>
          p1.asDigit shouldEqual p1.value.asDigit
        }
      }
    }

    describe("should offer asDigitPosZInt method that") {
      it("is consistent with Char's asDigit") {
        forAll { (p1: NumericChar) =>
          p1.asDigitPosZInt.value shouldEqual p1.value.asDigit
        }
      }
    }

    describe("should offer a unary ~ method that") {
      it("is consistent with Char") {
        forAll { (p: NumericChar) =>
          (~p) shouldEqual (~(p.toChar))
        }
      }
    }

    describe("should offer a + method that") {
      it("takes a String which is consistent with Char") {
        forAll { (p: NumericChar) =>
          p + "test" shouldEqual p.value + "test"
        }
      }
    }

    describe("should offer << methods that") {
      it("are consistent with Char") {
        forAll { (p: NumericChar, shift: Int) =>
          p << shift shouldEqual p.value << shift
        }
        forAll { (p: NumericChar, shift: Long) =>
          p << shift shouldEqual p.value << shift
        }
      }
    }

    describe("should offer >>> methods that") {
      it("are consistent with Char") {
        forAll { (p: NumericChar, shift: Int) =>
          p >>> shift shouldEqual p.value >>> shift
        }
        forAll { (p: NumericChar, shift: Long) =>
          p >>> shift shouldEqual p.value >>> shift
        }
      }
    }

    describe("should offer >> methods that") {
      it("are consistent with Char") {
        forAll { (p: NumericChar, shift: Int) =>
          p >> shift shouldEqual p.value >> shift
        }
        forAll { (p: NumericChar, shift: Long) =>
          p >> shift shouldEqual p.value >> shift
        }
      }
    }

    describe("should offer a '|' method that") {
      it("is consistent with Char") {
        forAll { (p: NumericChar, byte: Byte) =>
          (p | byte) shouldEqual (p.value | byte)
        }
        forAll { (p: NumericChar, short: Short) =>
          (p | short) shouldEqual (p.value | short)
        }
        forAll { (p: NumericChar, char: Char) =>
          (p | char) shouldEqual (p.value | char)
        }
        forAll { (p: NumericChar, int: Int) =>
          (p | int) shouldEqual (p.value | int)
        }
        forAll { (p: NumericChar, long: Long) =>
          (p | long) shouldEqual (p.value | long)
        }
      }
    }

    describe("should offer an '&' method that") {
      it("is consistent with Char") {
        forAll { (p: NumericChar, byte: Byte) =>
          (p & byte) shouldEqual (p.value & byte)
        }
        forAll { (p: NumericChar, short: Short) =>
          (p & short) shouldEqual (p.value & short)
        }
        forAll { (p: NumericChar, char: Char) =>
          (p & char) shouldEqual (p.value & char)
        }
        forAll { (p: NumericChar, int: Int) =>
          (p & int) shouldEqual (p.value & int)
        }
        forAll { (p: NumericChar, long: Long) =>
          (p & long) shouldEqual (p.value & long)
        }
      }
    }

    describe("should offer an '^' method that") {
      it("is consistent with Char") {
        forAll { (p: NumericChar, byte: Byte) =>
          (p ^ byte) shouldEqual (p.value ^ byte)
        }
        forAll { (p: NumericChar, char: Char) =>
          (p ^ char) shouldEqual (p.value ^ char)
        }
        forAll { (p: NumericChar, short: Short) =>
          (p ^ short) shouldEqual (p.value ^ short)
        }
        forAll { (p: NumericChar, int: Int) =>
          (p ^ int) shouldEqual (p.value ^ int)
        }
        forAll { (p: NumericChar, long: Long) =>
          (p ^ long) shouldEqual (p.value ^ long)
        }
      }
    }
  }
}

