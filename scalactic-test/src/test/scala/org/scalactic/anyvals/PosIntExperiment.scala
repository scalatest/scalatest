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

import org.scalactic.Equality
import org.scalatest._
import org.scalatest.prop.GenDrivenPropertyChecks
import org.scalatest.prop.Gen
import OptionValues._

import scala.util.{Failure, Success, Try}

class PosIntExperiment extends FunSpec with Matchers with GenDrivenPropertyChecks {

/*
  implicit val posIntGen: Gen[PosInt] =
    for {i <- Gen.chooseInt(1, Int.MaxValue)} yield PosInt.from(i).get
*/

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

  describe("A PosInt") {

    describe("should offer a from factory method that") {
      it("returns Some[PosInt] if the passed Int is greater than 0") {
        PosInt.from(50).value.value shouldBe 50
        PosInt.from(100).value.value shouldBe 100
      }

      it("returns None if the passed Int is NOT greater than 0") {
        PosInt.from(0) shouldBe None
        PosInt.from(-1) shouldBe None
        PosInt.from(-99) shouldBe None
      }
    }

    it("should offer a MaxValue factor method") {
      PosInt.MaxValue shouldEqual PosInt.from(Int.MaxValue).get
    }

    it("should have a pretty toString") {
      PosInt.from(42).value.toString shouldBe "PosInt(42)"
    }

    it("should return the same type from its unary_+ method") {
      +PosInt(3) shouldEqual PosInt(3)
    }

    it("should be automatically widened to compatible AnyVal targets") {
      (PosInt(3): Int) shouldEqual 3
      (PosInt(3): Long) shouldEqual 3L
      (PosInt(3): Float) shouldEqual 3.0F
      (PosInt(3): Double) shouldEqual 3.0

      (PosInt(3): PosInt) shouldEqual PosInt(3)
      (PosInt(3): PosLong) shouldEqual PosLong(3L)
      (PosInt(3): PosFloat) shouldEqual PosFloat(3.0F)
      (PosInt(3): PosDouble) shouldEqual PosDouble(3.0)

      (PosInt(3): PosZInt) shouldEqual PosZInt(3)
      (PosInt(3): PosZLong) shouldEqual PosZLong(3L)
      (PosInt(3): PosZFloat) shouldEqual PosZFloat(3.0F)
      (PosInt(3): PosZDouble) shouldEqual PosZDouble(3.0)
    }

    describe("when a compatible AnyVal is passed to a + method invoked on it") {
      it("should give the same AnyVal type back at compile time, and correct value at runtime") {
        // When adding a "primitive"
        val opInt = PosInt(3) + 3
        opInt shouldEqual 6

        val opLong = PosInt(3) + 3L
        opLong shouldEqual 6L

        val opFloat = PosInt(3) + 3.0F
        opFloat shouldEqual 6.0F

        val opDouble = PosInt(3) + 3.0
        opDouble shouldEqual 6.0

        // When adding a Pos*
        val opPosInt = PosInt(3) + PosInt(3)
        opPosInt shouldEqual 6

        val opPosLong = PosInt(3) + PosLong(3L)
        opPosLong shouldEqual 6L

        val opPosFloat = PosInt(3) + PosFloat(3.0F)
        opPosFloat shouldEqual 6.0F

        val opPosDouble = PosInt(3) + PosDouble(3.0)
        opPosDouble shouldEqual 6.0

        // When adding a *PosZ
        val opPosZ = PosInt(3) + PosZInt(3)
        opPosZ shouldEqual 6

        val opPosZLong = PosInt(3) + PosZLong(3L)
        opPosZLong shouldEqual 6L

        val opPosZFloat = PosInt(3) + PosZFloat(3.0F)
        opPosZFloat shouldEqual 6.0F

        val opPosZDouble = PosInt(3) + PosZDouble(3.0)
        opPosZDouble shouldEqual 6.0
      }
    }

    describe("when created with apply method") {

      it("should compile when 8 is passed in") {
        "PosInt(8)" should compile
        PosInt(8).value shouldEqual 8
      }

      it("should not compile when 0 is passed in") {
        "PosInt(0)" shouldNot compile
      }

      it("should not compile when -8 is passed in") {
        "PosInt(-8)" shouldNot compile
      }

      it("should not compile when x is passed in") {
        val x: Int = -8
        "PosInt(x)" shouldNot compile
      }
    }

    describe("when specified as a plain-old Int") {

      def takesPosInt(pos: PosInt): Int = pos.value

      it("should compile when 8 is passed in") {
        "takesPosInt(8)" should compile
        takesPosInt(8) shouldEqual 8
      }

      it("should not compile when 0 is passed in") {
        "takesPosInt(0)" shouldNot compile
      }

      it("should not compile when -8 is passed in") {
        "takesPosInt(-8)" shouldNot compile
      }

      it("should not compile when x is passed in") {
        val x: Int = -8
        "takesPosInt(x)" shouldNot compile
      }
    }

    it("should offer a unary ~ method that is consistent with Int") {
      forAll { (pint: PosInt) =>
        (~pint) shouldEqual (~(pint.toInt))
      }
    }

    it("should offer a unary + method that is consistent with Int") {
      forAll { (pint: PosInt) =>
        (+pint).toInt shouldEqual (+(pint.toInt))
      }
    }

    it("should offer a unary - method that is consistent with Int") {
      forAll { (pint: PosInt) =>
        (-pint) shouldEqual (-(pint.toInt))
      }
    }

    it("should offer << methods that are consistent with Int") {
      forAll { (pint: PosInt, shift: Int) =>
        pint << shift shouldEqual pint.toInt << shift
      }
      forAll { (pint: PosInt, shift: Long) =>
        pint << shift shouldEqual pint.toInt << shift
      }
    }

    it("should offer >>> methods that are consistent with Int") {
      forAll { (pint: PosInt, shift: Int) =>
        pint >>> shift shouldEqual pint.toInt >>> shift
      }
      forAll { (pint: PosInt, shift: Long) =>
        pint >>> shift shouldEqual pint.toInt >>> shift
      }
    }

    it("should offer >> methods that are consistent with Int") {
      forAll { (pint: PosInt, shift: Int) =>
        pint >> shift shouldEqual pint.toInt >> shift
      }
      forAll { (pint: PosInt, shift: Long) =>
        pint >> shift shouldEqual pint.toInt >> shift
      }
    }

    it("should offer '<' comparison that is consistent with Int") {
      forAll { (pint: PosInt, byte: Byte) =>
        (pint < byte) shouldEqual (pint.toInt < byte)
      }
      forAll { (pint: PosInt, short: Short) =>
        (pint < short) shouldEqual (pint.toInt < short)
      }
      forAll { (pint: PosInt, char: Char) =>
        (pint < char) shouldEqual (pint.toInt < char)
      }
      forAll { (pint: PosInt, int: Int) =>
        (pint < int) shouldEqual (pint.toInt < int)
      }
      forAll { (pint: PosInt, long: Long) =>
        (pint < long) shouldEqual (pint.toInt < long)
      }
      forAll { (pint: PosInt, float: Float) =>
        (pint < float) shouldEqual (pint.toInt < float)
      }
      forAll { (pint: PosInt, double: Double) =>
        (pint < double) shouldEqual (pint.toInt < double)
      }
    }

    it("should offer '<=' comparison that is consistent with Int") {
      forAll { (pint: PosInt, byte: Byte) =>
        (pint <= byte) shouldEqual (pint.toInt <= byte)
      }
      forAll { (pint: PosInt, short: Short) =>
        (pint <= short) shouldEqual (pint.toInt <= short)
      }
      forAll { (pint: PosInt, char: Char) =>
        (pint <= char) shouldEqual (pint.toInt <= char)
      }
      forAll { (pint: PosInt, int: Int) =>
        (pint <= int) shouldEqual (pint.toInt <= int)
      }
      forAll { (pint: PosInt, long: Long) =>
        (pint <= long) shouldEqual (pint.toInt <= long)
      }
      forAll { (pint: PosInt, float: Float) =>
        (pint <= float) shouldEqual (pint.toInt <= float)
      }
      forAll { (pint: PosInt, double: Double) =>
        (pint <= double) shouldEqual (pint.toInt <= double)
      }
    }

    it("should offer '>' comparison that is consistent with Int") {
      forAll { (pint: PosInt, byte: Byte) =>
        (pint > byte) shouldEqual (pint.toInt > byte)
      }
      forAll { (pint: PosInt, short: Short) =>
        (pint > short) shouldEqual (pint.toInt > short)
      }
      forAll { (pint: PosInt, char: Char) =>
        (pint > char) shouldEqual (pint.toInt > char)
      }
      forAll { (pint: PosInt, int: Int) =>
        (pint > int) shouldEqual (pint.toInt > int)
      }
      forAll { (pint: PosInt, long: Long) =>
        (pint > long) shouldEqual (pint.toInt > long)
      }
      forAll { (pint: PosInt, float: Float) =>
        (pint > float) shouldEqual (pint.toInt > float)
      }
      forAll { (pint: PosInt, double: Double) =>
        (pint > double) shouldEqual (pint.toInt > double)
      }
    }

    it("should offer '>=' comparison that is consistent with Int") {
      forAll { (pint: PosInt, byte: Byte) =>
        (pint >= byte) shouldEqual (pint.toInt >= byte)
      }
      forAll { (pint: PosInt, short: Short) =>
        (pint >= short) shouldEqual (pint.toInt >= short)
      }
      forAll { (pint: PosInt, char: Char) =>
        (pint >= char) shouldEqual (pint.toInt >= char)
      }
      forAll { (pint: PosInt, int: Int) =>
        (pint >= int) shouldEqual (pint.toInt >= int)
      }
      forAll { (pint: PosInt, long: Long) =>
        (pint >= long) shouldEqual (pint.toInt >= long)
      }
      forAll { (pint: PosInt, float: Float) =>
        (pint >= float) shouldEqual (pint.toInt >= float)
      }
      forAll { (pint: PosInt, double: Double) =>
        (pint >= double) shouldEqual (pint.toInt >= double)
      }
    }

    it("should offer a '|' method that is consistent with Int") {
      forAll { (pint: PosInt, byte: Byte) =>
        (pint | byte) shouldEqual (pint.toInt | byte)
      }
      forAll { (pint: PosInt, short: Short) =>
        (pint | short) shouldEqual (pint.toInt | short)
      }
      forAll { (pint: PosInt, char: Char) =>
        (pint | char) shouldEqual (pint.toInt | char)
      }
      forAll { (pint: PosInt, int: Int) =>
        (pint | int) shouldEqual (pint.toInt | int)
      }
      forAll { (pint: PosInt, long: Long) =>
        (pint | long) shouldEqual (pint.toInt | long)
      }
    }

    it("should offer an '&' method that is consistent with Int") {
      forAll { (pint: PosInt, byte: Byte) =>
        (pint & byte) shouldEqual (pint.toInt & byte)
      }
      forAll { (pint: PosInt, short: Short) =>
        (pint & short) shouldEqual (pint.toInt & short)
      }
      forAll { (pint: PosInt, char: Char) =>
        (pint & char) shouldEqual (pint.toInt & char)
      }
      forAll { (pint: PosInt, int: Int) =>
        (pint & int) shouldEqual (pint.toInt & int)
      }
      forAll { (pint: PosInt, long: Long) =>
        (pint & long) shouldEqual (pint.toInt & long)
      }
    }

    it("should offer an '^' method that is consistent with Int") {
      forAll { (pint: PosInt, byte: Byte) =>
        (pint ^ byte) shouldEqual (pint.toInt ^ byte)
      }
      forAll { (pint: PosInt, char: Char) =>
        (pint ^ char) shouldEqual (pint.toInt ^ char)
      }
      forAll { (pint: PosInt, short: Short) =>
        (pint ^ short) shouldEqual (pint.toInt ^ short)
      }
      forAll { (pint: PosInt, int: Int) =>
        (pint ^ int) shouldEqual (pint.toInt ^ int)
      }
      forAll { (pint: PosInt, long: Long) =>
        (pint ^ long) shouldEqual (pint.toInt ^ long)
      }
    }

    it("should offer a '+' method that is consistent with Int") {
      forAll { (pint: PosInt, byte: Byte) =>
        (pint + byte) shouldEqual (pint.toInt + byte)
      }
      forAll { (pint: PosInt, char: Char) =>
        (pint + char) shouldEqual (pint.toInt + char)
      }
      forAll { (pint: PosInt, short: Short) =>
        (pint + short) shouldEqual (pint.toInt + short)
      }
      forAll { (pint: PosInt, int: Int) =>
        (pint + int) shouldEqual (pint.toInt + int)
      }
      forAll { (pint: PosInt, long: Long) =>
        (pint + long) shouldEqual (pint.toInt + long)
      }
      forAll { (pint: PosInt, float: Float) =>
        (pint + float) shouldEqual (pint.toInt + float)
      }
      forAll { (pint: PosInt, double: Double) =>
        (pint + double) shouldEqual (pint.toInt + double)
      }
    }

    it("should offer a '-' method that is consistent with Int") {
      forAll { (pint: PosInt, byte: Byte) =>
        (pint - byte) shouldEqual (pint.toInt - byte)
      }
      forAll { (pint: PosInt, short: Short) =>
        (pint - short) shouldEqual (pint.toInt - short)
      }
      forAll { (pint: PosInt, byte: Char) =>
        (pint - byte) shouldEqual (pint.toInt - byte)
      }
      forAll { (pint: PosInt, int: Int) =>
        (pint - int) shouldEqual (pint.toInt - int)
      }
      forAll { (pint: PosInt, long: Long) =>
        (pint - long) shouldEqual (pint.toInt - long)
      }
      forAll { (pint: PosInt, float: Float) =>
        (pint - float) shouldEqual (pint.toInt - float)
      }
      forAll { (pint: PosInt, double: Double) =>
        (pint - double) shouldEqual (pint.toInt - double)
      }
    }

    it("should offer a '*' method that is consistent with Int") {
      forAll { (pint: PosInt, byte: Byte) =>
        (pint * byte) shouldEqual (pint.toInt * byte)
      }
      forAll { (pint: PosInt, short: Short) =>
        (pint * short) shouldEqual (pint.toInt * short)
      }
      forAll { (pint: PosInt, byte: Char) =>
        (pint * byte) shouldEqual (pint.toInt * byte)
      }
      forAll { (pint: PosInt, int: Int) =>
        (pint * int) shouldEqual (pint.toInt * int)
      }
      forAll { (pint: PosInt, long: Long) =>
        (pint * long) shouldEqual (pint.toInt * long)
      }
      forAll { (pint: PosInt, float: Float) =>
        (pint * float) shouldEqual (pint.toInt * float)
      }
      forAll { (pint: PosInt, double: Double) =>
        (pint * double) shouldEqual (pint.toInt * double)
      }
    }

    it("should offer a '/' method that is consistent with Int") {
      // Note that Try (and associated Equality[Try]) are used since some values
      // will legitimately throw an exception

      forAll { (pint: PosInt, byte: Byte) =>
        Try(pint / byte) shouldEqual Try(pint.toInt / byte)
      }
      forAll { (pint: PosInt, short: Short) =>
        Try(pint / short) shouldEqual Try(pint.toInt / short)
      }
      forAll { (pint: PosInt, char: Char) =>
        Try(pint / char) shouldEqual Try(pint.toInt / char)
      }
      forAll { (pint: PosInt, int: Int) =>
        Try(pint / int) shouldEqual Try(pint.toInt / int)
      }
      forAll { (pint: PosInt, long: Long) =>
        Try(pint / long) shouldEqual Try(pint.toInt / long)
      }
      forAll { (pint: PosInt, float: Float) =>
        Try(pint / float) shouldEqual Try(pint.toInt / float)
      }
      forAll { (pint: PosInt, double: Double) =>
        Try(pint / double) shouldEqual Try(pint.toInt / double)
      }
    }

    it("should offer a '%' method that is consistent with Int") {
      // Note that Try (and associated Equality[Try]) are used since some values
      // will legitimately throw an exception

      forAll { (pint: PosInt, byte: Byte) =>
        Try(pint % byte) shouldEqual Try(pint.toInt % byte)
      }
      forAll { (pint: PosInt, short: Short) =>
        Try(pint % short) shouldEqual Try(pint.toInt % short)
      }
      forAll { (pint: PosInt, char: Char) =>
        Try(pint % char) shouldEqual Try(pint.toInt % char)
      }
      forAll { (pint: PosInt, int: Int) =>
        Try(pint % int) shouldEqual Try(pint.toInt % int)
      }
      forAll { (pint: PosInt, long: Long) =>
        Try(pint % long) shouldEqual Try(pint.toInt % long)
      }
      forAll { (pint: PosInt, float: Float) =>
        Try(pint % float) shouldEqual Try(pint.toInt % float)
      }
      forAll { (pint: PosInt, double: Double) =>
        Try(pint % double) shouldEqual Try(pint.toInt % double)
      }
    }

    it("should offer 'min' and 'max' methods that are consistent with Int") {
      forAll { (pint1: PosInt, pint2: PosInt) =>
        pint1.max(pint2).toInt shouldEqual pint1.toInt.max(pint2.toInt)
        pint1.min(pint2).toInt shouldEqual pint1.toInt.min(pint2.toInt)
      }
    }

    it("should offer a 'toBinaryString' method that is consistent with Int") {
      forAll { (pint: PosInt) =>
        pint.toBinaryString shouldEqual pint.toInt.toBinaryString
      }
    }

    it("should offer a 'toHexString' method that is consistent with Int") {
      forAll { (pint: PosInt) =>
        pint.toHexString shouldEqual pint.toInt.toHexString
      }
    }

    it("should offer a 'toOctalString' method that is consistent with Int") {
      forAll { (pint: PosInt) =>
        pint.toOctalString shouldEqual pint.toInt.toOctalString
      }
    }

    it("should offer 'to' and 'until' methods that are consistent with Int") {
      forAll { (pint: PosInt, end: Int, step: Int) =>
        Try(pint.to(end)) shouldEqual Try(pint.toInt.to(end))
        Try(pint.to(end, step)) shouldEqual Try(pint.toInt.to(end, step))
        Try(pint.until(end)) shouldEqual Try(pint.toInt.until(end))
        Try(pint.until(end, step)) shouldEqual Try(pint.toInt.until(end, step))
      }
    }

    it("should offer widening methods for basic types that are consistent with Int") {
      forAll { (pint: PosInt) =>
        def widen(value: Int): Int = value
        widen(pint) shouldEqual widen(pint.toInt)
      }
      forAll { (pint: PosInt) =>
        def widen(value: Long): Long = value
        widen(pint) shouldEqual widen(pint.toInt)
      }
      forAll { (pint: PosInt) =>
        def widen(value: Float): Float = value
        widen(pint) shouldEqual widen(pint.toInt)
      }
      forAll { (pint: PosInt) =>
        def widen(value: Double): Double = value
        widen(pint) shouldEqual widen(pint.toInt)
      }
      forAll { (pint: PosInt) =>
        def widen(value: PosLong): PosLong = value
        widen(pint) shouldEqual widen(PosLong.from(pint.toInt).get)
      }
      forAll { (pint: PosInt) =>
        def widen(value: PosFloat): PosFloat = value
        widen(pint) shouldEqual widen(PosFloat.from(pint.toInt).get)
      }
      forAll { (pint: PosInt) =>
        def widen(value: PosDouble): PosDouble = value
        widen(pint) shouldEqual widen(PosDouble.from(pint.toInt).get)
      }
      forAll { (pint: PosInt) =>
        def widen(value: PosZLong): PosZLong = value
        widen(pint) shouldEqual widen(PosZLong.from(pint.toInt).get)
      }
      forAll { (pint: PosInt) =>
        def widen(value: PosZFloat): PosZFloat = value
        widen(pint) shouldEqual widen(PosZFloat.from(pint.toInt).get)
      }
      forAll { (pint: PosInt) =>
        def widen(value: PosZDouble): PosZDouble = value
        widen(pint) shouldEqual widen(PosZDouble.from(pint.toInt).get)
      }
    }
  }
}

