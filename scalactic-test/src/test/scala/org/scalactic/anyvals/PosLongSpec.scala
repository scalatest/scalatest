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
import prop.NyayaGeneratorDrivenPropertyChecks._
import japgolly.nyaya.test.Gen
import OptionValues._
import org.scalactic.StrictCheckedEquality

// SKIP-SCALATESTJS-START
import scala.collection.immutable.NumericRange
// SKIP-SCALATESTJS-END
import scala.util.{Failure, Success, Try}

class PosLongSpec extends FunSpec with Matchers with StrictCheckedEquality {

  implicit val posLongGen: Gen[PosLong] =
    for {i <- Gen.chooselong(1, Long.MaxValue)} yield PosLong.from(i).get

  implicit val intGen: Gen[Int] = Gen.int
  implicit val longGen: Gen[Long] = Gen.long
  implicit val shortGen: Gen[Short] = Gen.short
  implicit val charGen: Gen[Char] = Gen.char
  implicit val floatGen: Gen[Float] = Gen.float
  implicit val doubleGen: Gen[Double] = Gen.double
  implicit val byteGen: Gen[Byte] = Gen.byte

  implicit def tryEquality[T]: Equality[Try[T]] = new Equality[Try[T]] {
    override def areEqual(a: Try[T], b: Any): Boolean = a match {
      case _: Success[_] => a == b
      case Failure(ex) => b match {
        case _: Success[_] => false
        case Failure(otherEx) => ex.getClass == otherEx.getClass && ex.getMessage == otherEx.getMessage
        case _ => false
      }
    }
  }

  describe("A PosLong") {
    describe("should offer a from factory method that") {
      it("returns Some[PosLong] if the passed Long is greater than 0") {
        PosLong.from(50L).value.value shouldBe 50L
        PosLong.from(100L).value.value shouldBe 100L
      }
      it("returns None if the passed Long is NOT greater than 0") {
        PosLong.from(0L) shouldBe None
        PosLong.from(-1L) shouldBe None
        PosLong.from(-99L) shouldBe None
      }
    } 
    it("should have a pretty toString") {
      PosLong.from(42L).value.toString shouldBe "PosLong(42)"
    }
    it("should return the same type from its unary_+ method") {
      +PosLong(3L) shouldEqual PosLong(3L)
    } 
    it("should be automatically widened to compatible AnyVal targets") {
      "PosLong(3L): Int" shouldNot typeCheck
      (PosLong(3L): Long) shouldEqual 3L
      (PosLong(3L): Float) shouldEqual 3.0F
      (PosLong(3L): Double) shouldEqual 3.0

      "PosLong(3L): PosInt" shouldNot typeCheck
      (PosLong(3L): PosLong) shouldEqual PosLong(3L)
      (PosLong(3L): PosFloat) shouldEqual PosFloat(3.0F)
      (PosLong(3L): PosDouble) shouldEqual PosDouble(3.0)

      "PosLong(3L): PosZInt" shouldNot typeCheck
      (PosLong(3L): PosZLong) shouldEqual PosZLong(3L)
      (PosLong(3L): PosZFloat) shouldEqual PosZFloat(3.0F)
      (PosLong(3L): PosZDouble) shouldEqual PosZDouble(3.0)
    }
    describe("when a compatible AnyVal is passed to a + method invoked on it") {
      it("should give the same AnyVal type back at compile time, and correct value at runtime") {
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

        // When adding a *PosZ
        val opPosZ = PosLong(3L) + PosZInt(3)
        opPosZ shouldEqual 6L

        val opPosZLong = PosLong(3L) + PosZLong(3L)
        opPosZLong shouldEqual 6L

        val opPosZFloat = PosLong(3L) + PosZFloat(3.0F)
        opPosZFloat shouldEqual 6.0F

        val opPosZDouble = PosLong(3L) + PosZDouble(3.0)
        opPosZDouble shouldEqual 6.0
      }
    }

    describe("when created with apply method") {

      it("should compile when 8 is passed in") {
        "PosLong(8)" should compile
        PosLong(8).value shouldEqual 8L
        "PosLong(8L)" should compile
        PosLong(8L).value shouldEqual 8L
      }

      it("should not compile when 0 is passed in") {
        "PosLong(0)" shouldNot compile
        "PosLong(0L)" shouldNot compile
      }

      it("should not compile when -8 is passed in") {
        "PosLong(-8)" shouldNot compile
        "PosLong(-8L)" shouldNot compile
      }

      it("should not compile when x is passed in") {
        val a: Int = -8
        "PosLong(a)" shouldNot compile
        val b: Long = -8L
        "PosLong(b)" shouldNot compile
      }
    }
    describe("when specified as a plain-old Long") {

      def takesPosLong(pos: PosLong): Long = pos.value

      it("should compile when 8 is passed in") {
        "takesPosLong(8)" should compile
        takesPosLong(8) shouldEqual 8L
        "takesPosLong(8L)" should compile
        takesPosLong(8L) shouldEqual 8L
      }

      it("should not compile when 0 is passed in") {
        "takesPosLong(0)" shouldNot compile
        "takesPosLong(0L)" shouldNot compile
      }

      it("should not compile when -8 is passed in") {
        "takesPosLong(-8)" shouldNot compile
        "takesPosLong(-8L)" shouldNot compile
      }

      it("should not compile when x is passed in") {
        val x: Int = -8
        "takesPosLong(x)" shouldNot compile
        val b: Long = -8L
        "takesPosLong(b)" shouldNot compile
      }

      it("should offer a unary ~ method that is consistent with Long") {
        forAll { (plong: PosLong) =>
          (~plong) shouldEqual (~(plong.toLong))
        }
      }

      it("should offer a unary + method that is consistent with Long") {
        forAll { (plong: PosLong) =>
          (+plong).toLong shouldEqual (+(plong.toLong))
        }
      }

      it("should offer a unary - method that is consistent with Long") {
        forAll { (plong: PosLong) =>
          (-plong) shouldEqual (-(plong.toLong))
        }
      }

      it("should offer << methods that are consistent with Long") {
        forAll { (plong: PosLong, shift: Int) =>
          plong << shift shouldEqual plong.toLong << shift
        }
        forAll { (plong: PosLong, shift: Long) =>
          plong << shift shouldEqual plong.toLong << shift
        }
      }

      it("should offer >>> methods that are consistent with Long") {
        forAll { (plong: PosLong, shift: Int) =>
          plong >>> shift shouldEqual plong.toLong >>> shift
        }
        forAll { (plong: PosLong, shift: Long) =>
          plong >>> shift shouldEqual plong.toLong >>> shift
        }
      }

      it("should offer >> methods that are consistent with Long") {
        forAll { (plong: PosLong, shift: Int) =>
          plong >> shift shouldEqual plong.toLong >> shift
        }
        forAll { (plong: PosLong, shift: Long) =>
          plong >> shift shouldEqual plong.toLong >> shift
        }
      }

      it("should offer '<' comparison that is consistent with Long") {
        forAll { (plong: PosLong, byte: Byte) =>
          (plong < byte) shouldEqual (plong.toLong < byte)
        }
        forAll { (plong: PosLong, short: Short) =>
          (plong < short) shouldEqual (plong.toLong < short)
        }
        forAll { (plong: PosLong, char: Char) =>
          (plong < char) shouldEqual (plong.toLong < char)
        }
        forAll { (plong: PosLong, int: Int) =>
          (plong < int) shouldEqual (plong.toLong < int)
        }
        forAll { (plong: PosLong, long: Long) =>
          (plong < long) shouldEqual (plong.toLong < long)
        }
        forAll { (plong: PosLong, float: Float) =>
          (plong < float) shouldEqual (plong.toLong < float)
        }
        forAll { (plong: PosLong, double: Double) =>
          (plong < double) shouldEqual (plong.toLong < double)
        }
      }

      it("should offer '<=' comparison that is consistent with Long") {
        forAll { (plong: PosLong, byte: Byte) =>
          (plong <= byte) shouldEqual (plong.toLong <= byte)
        }
        forAll { (plong: PosLong, char: Char) =>
          (plong <= char) shouldEqual (plong.toLong <= char)
        }
        forAll { (plong: PosLong, short: Short) =>
          (plong <= short) shouldEqual (plong.toLong <= short)
        }
        forAll { (plong: PosLong, int: Int) =>
          (plong <= int) shouldEqual (plong.toLong <= int)
        }
        forAll { (plong: PosLong, long: Long) =>
          (plong <= long) shouldEqual (plong.toLong <= long)
        }
        forAll { (plong: PosLong, float: Float) =>
          (plong <= float) shouldEqual (plong.toLong <= float)
        }
        forAll { (plong: PosLong, double: Double) =>
          (plong <= double) shouldEqual (plong.toLong <= double)
        }
      }

      it("should offer '>' comparison that is consistent with Long") {
        forAll { (plong: PosLong, byte: Byte) =>
          (plong > byte) shouldEqual (plong.toLong > byte)
        }
        forAll { (plong: PosLong, short: Short) =>
          (plong > short) shouldEqual (plong.toLong > short)
        }
        forAll { (plong: PosLong, char: Char) =>
          (plong > char) shouldEqual (plong.toLong > char)
        }
        forAll { (plong: PosLong, int: Int) =>
          (plong > int) shouldEqual (plong.toLong > int)
        }
        forAll { (plong: PosLong, long: Long) =>
          (plong > long) shouldEqual (plong.toLong > long)
        }
        forAll { (plong: PosLong, float: Float) =>
          (plong > float) shouldEqual (plong.toLong > float)
        }
        forAll { (plong: PosLong, double: Double) =>
          (plong > double) shouldEqual (plong.toLong > double)
        }
      }

      it("should offer '>=' comparison that is consistent with Long") {
        forAll { (plong: PosLong, byte: Byte) =>
          (plong >= byte) shouldEqual (plong.toLong >= byte)
        }
        forAll { (plong: PosLong, short: Short) =>
          (plong >= short) shouldEqual (plong.toLong >= short)
        }
        forAll { (plong: PosLong, char: Char) =>
          (plong >= char) shouldEqual (plong.toLong >= char)
        }
        forAll { (plong: PosLong, int: Int) =>
          (plong >= int) shouldEqual (plong.toLong >= int)
        }
        forAll { (plong: PosLong, long: Long) =>
          (plong >= long) shouldEqual (plong.toLong >= long)
        }
        forAll { (plong: PosLong, float: Float) =>
          (plong >= float) shouldEqual (plong.toLong >= float)
        }
        forAll { (plong: PosLong, double: Double) =>
          (plong >= double) shouldEqual (plong.toLong >= double)
        }
      }

      it("should offer a '|' method that is consistent with Long") {
        forAll { (plong: PosLong, byte: Byte) =>
          (plong | byte) shouldEqual (plong.toLong | byte)
        }
        forAll { (plong: PosLong, short: Short) =>
          (plong | short) shouldEqual (plong.toLong | short)
        }
        forAll { (plong: PosLong, char: Char) =>
          (plong | char) shouldEqual (plong.toLong | char)
        }
        forAll { (plong: PosLong, int: Int) =>
          (plong | int) shouldEqual (plong.toLong | int)
        }
        forAll { (plong: PosLong, long: Long) =>
          (plong | long) shouldEqual (plong.toLong | long)
        }
      }

      it("should offer an '&' method that is consistent with Long") {
        forAll { (plong: PosLong, byte: Byte) =>
          (plong & byte) shouldEqual (plong.toLong & byte)
        }
        forAll { (plong: PosLong, short: Short) =>
          (plong & short) shouldEqual (plong.toLong & short)
        }
        forAll { (plong: PosLong, char: Char) =>
          (plong & char) shouldEqual (plong.toLong & char)
        }
        forAll { (plong: PosLong, int: Int) =>
          (plong & int) shouldEqual (plong.toLong & int)
        }
        forAll { (plong: PosLong, long: Long) =>
          (plong & long) shouldEqual (plong.toLong & long)
        }
      }

      it("should offer an '^' method that is consistent with Long") {
        forAll { (plong: PosLong, byte: Byte) =>
          (plong ^ byte) shouldEqual (plong.toLong ^ byte)
        }
        forAll { (plong: PosLong, short: Short) =>
          (plong ^ short) shouldEqual (plong.toLong ^ short)
        }
        forAll { (plong: PosLong, char: Char) =>
          (plong ^ char) shouldEqual (plong.toLong ^ char)
        }
        forAll { (plong: PosLong, int: Int) =>
          (plong ^ int) shouldEqual (plong.toLong ^ int)
        }
        forAll { (plong: PosLong, long: Long) =>
          (plong ^ long) shouldEqual (plong.toLong ^ long)
        }
      }

      it("should offer a '+' method that is consistent with Long") {
        forAll { (plong: PosLong, byte: Byte) =>
          (plong + byte) shouldEqual (plong.toLong + byte)
        }
        forAll { (plong: PosLong, short: Short) =>
          (plong + short) shouldEqual (plong.toLong + short)
        }
        forAll { (plong: PosLong, char: Char) =>
          (plong + char) shouldEqual (plong.toLong + char)
        }
        forAll { (plong: PosLong, int: Int) =>
          (plong + int) shouldEqual (plong.toLong + int)
        }
        forAll { (plong: PosLong, long: Long) =>
          (plong + long) shouldEqual (plong.toLong + long)
        }
        forAll { (plong: PosLong, float: Float) =>
          (plong + float) shouldEqual (plong.toLong + float)
        }
        forAll { (plong: PosLong, double: Double) =>
          (plong + double) shouldEqual (plong.toLong + double)
        }
      }

      it("should offer a '-' method that is consistent with Long") {
        forAll { (plong: PosLong, byte: Byte) =>
          (plong - byte) shouldEqual (plong.toLong - byte)
        }
        forAll { (plong: PosLong, short: Short) =>
          (plong - short) shouldEqual (plong.toLong - short)
        }
        forAll { (plong: PosLong, char: Char) =>
          (plong - char) shouldEqual (plong.toLong - char)
        }
        forAll { (plong: PosLong, int: Int) =>
          (plong - int) shouldEqual (plong.toLong - int)
        }
        forAll { (plong: PosLong, long: Long) =>
          (plong - long) shouldEqual (plong.toLong - long)
        }
        forAll { (plong: PosLong, float: Float) =>
          (plong - float) shouldEqual (plong.toLong - float)
        }
        forAll { (plong: PosLong, double: Double) =>
          (plong - double) shouldEqual (plong.toLong - double)
        }
      }

      it("should offer a '*' method that is consistent with Long") {
        forAll { (plong: PosLong, byte: Byte) =>
          (plong * byte) shouldEqual (plong.toLong * byte)
        }
        forAll { (plong: PosLong, short: Short) =>
          (plong * short) shouldEqual (plong.toLong * short)
        }
        forAll { (plong: PosLong, char: Char) =>
          (plong * char) shouldEqual (plong.toLong * char)
        }
        forAll { (plong: PosLong, int: Int) =>
          (plong * int) shouldEqual (plong.toLong * int)
        }
        forAll { (plong: PosLong, long: Long) =>
          (plong * long) shouldEqual (plong.toLong * long)
        }
        forAll { (plong: PosLong, float: Float) =>
          (plong * float) shouldEqual (plong.toLong * float)
        }
        forAll { (plong: PosLong, double: Double) =>
          (plong * double) shouldEqual (plong.toLong * double)
        }
      }

      it("should offer a '/' method that is consistent with Long") {
        forAll { (plong: PosLong, byte: Byte) =>
          Try(plong / byte) shouldEqual Try(plong.toLong / byte)
        }
        forAll { (plong: PosLong, short: Short) =>
          Try(plong / short) shouldEqual Try(plong.toLong / short)
        }
        forAll { (plong: PosLong, char: Char) =>
          Try(plong / char) shouldEqual Try(plong.toLong / char)
        }
        forAll { (plong: PosLong, int: Int) =>
          Try(plong / int) shouldEqual Try(plong.toLong / int)
        }
        forAll { (plong: PosLong, long: Long) =>
          Try(plong / long) shouldEqual Try(plong.toLong / long)
        }
        forAll { (plong: PosLong, float: Float) =>
          Try(plong / float) shouldEqual Try(plong.toLong / float)
        }
        forAll { (plong: PosLong, double: Double) =>
          Try(plong / double) shouldEqual Try(plong.toLong / double)
        }
      }

      it("should offer a '%' method that is consistent with Long") {
        forAll { (plong: PosLong, byte: Byte) =>
          Try(plong % byte) shouldEqual Try(plong.toLong % byte)
        }
        forAll { (plong: PosLong, short: Short) =>
          Try(plong % short) shouldEqual Try(plong.toLong % short)
        }
        forAll { (plong: PosLong, char: Char) =>
          Try(plong % char) shouldEqual Try(plong.toLong % char)
        }
        forAll { (plong: PosLong, int: Int) =>
          Try(plong % int) shouldEqual Try(plong.toLong % int)
        }
        forAll { (plong: PosLong, long: Long) =>
          Try(plong % long) shouldEqual Try(plong.toLong % long)
        }
        forAll { (plong: PosLong, float: Float) =>
          Try(plong % float) shouldEqual Try(plong.toLong % float)
        }
        forAll { (plong: PosLong, double: Double) =>
          Try(plong % double) shouldEqual Try(plong.toLong % double)
        }
      }

      it("should offer 'min' and 'max' methods that are consistent with Long") {
        forAll { (plong1: PosLong, plong2: PosLong) =>
          plong1.max(plong2).toLong shouldEqual plong1.toLong.max(plong2.toLong)
          plong1.min(plong2).toLong shouldEqual plong1.toLong.min(plong2.toLong)
        }
      }

      it("should offer a 'toBinaryString' method that is consistent with Long") {
        forAll { (plong: PosLong) =>
          plong.toBinaryString shouldEqual plong.toLong.toBinaryString
        }
      }

      it("should offer a 'toHexString' method that is consistent with Long") {
        forAll { (plong: PosLong) =>
          plong.toHexString shouldEqual plong.toLong.toHexString
        }
      }

      it("should offer a 'toOctalString' method that is consistent with Long") {
        forAll { (plong: PosLong) =>
          plong.toOctalString shouldEqual plong.toLong.toOctalString
        }
      }

      // SKIP-SCALATESTJS-START
      it("should offer 'to' and 'until' method that is consistent with Long") {
        def rangeEqual[T](a: NumericRange[T], b: NumericRange[T]): Boolean =
          a.start == b.start && a.end == b.end && a.step == b.step

        forAll { (plong: PosLong, end: Long, step: Long) =>
          rangeEqual(plong.until(end), plong.toLong.until(end)) shouldBe true
          rangeEqual(plong.until(end, step), plong.toLong.until(end, step)) shouldBe true
          rangeEqual(plong.to(end), plong.toLong.to(end)) shouldBe true
          rangeEqual(plong.to(end, step), plong.toLong.to(end, step)) shouldBe true
        }
      }
      // SKIP-SCALATESTJS-END

      it("should offer widening methods for basic types that are consistent with Long") {
        forAll { (plong: PosLong) =>
          def widen(value: Long): Long = value
          widen(plong) shouldEqual widen(plong.toLong)
        }
        forAll { (plong: PosLong) =>
          def widen(value: Float): Float = value
          widen(plong) shouldEqual widen(plong.toLong)
        }
        forAll { (plong: PosLong) =>
          def widen(value: Double): Double = value
          widen(plong) shouldEqual widen(plong.toLong)
        }
        forAll { (plong: PosLong) =>
          def widen(value: PosFloat): PosFloat = value
          widen(plong) shouldEqual widen(PosFloat.from(plong.toLong).get)
        }
        forAll { (plong: PosLong) =>
          def widen(value: PosDouble): PosDouble = value
          widen(plong) shouldEqual widen(PosDouble.from(plong.toLong).get)
        }
        forAll { (plong: PosLong) =>
          def widen(value: PosZLong): PosZLong = value
          widen(plong) shouldEqual widen(PosZLong.from(plong.toLong).get)
        }
        forAll { (plong: PosLong) =>
          def widen(value: PosZFloat): PosZFloat = value
          widen(plong) shouldEqual widen(PosZFloat.from(plong.toLong).get)
        }
        forAll { (plong: PosLong) =>
          def widen(value: PosZDouble): PosZDouble = value
          widen(plong) shouldEqual widen(PosZDouble.from(plong.toLong).get)
        }
      }
    }
  }
}

