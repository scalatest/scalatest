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

import org.scalactic._
import org.scalatest._
import prop.NyayaGeneratorDrivenPropertyChecks._
import japgolly.nyaya.test.Gen
import OptionValues._
// SKIP-SCALATESTJS-START
import scala.collection.immutable.NumericRange
// SKIP-SCALATESTJS-END
import scala.util.{Failure, Success, Try}

class PosFloatSpec extends FunSpec with Matchers {

  implicit val posFloatGen: Gen[PosFloat] =
    for {i <- Gen.choosefloat(1, Float.MaxValue)} yield PosFloat.from(i).get

  implicit val intGen: Gen[Int] = Gen.int
  implicit val longGen: Gen[Long] = Gen.long
  implicit val shortGen: Gen[Short] = Gen.short
  implicit val charGen: Gen[Char] = Gen.char
  implicit val floatGen: Gen[Float] = Gen.float
  implicit val doubleGen: Gen[Double] = Gen.double
  implicit val byteGen: Gen[Byte] = Gen.byte

  describe("A PosFloat") {
    describe("should offer a from factory method that") {
      it("returns Some[PosFloat] if the passed Float is greater than 0") {
        PosFloat.from(50.23F).value.value shouldBe 50.23F
        PosFloat.from(100.0F).value.value shouldBe 100.0F
      }
      it("returns None if the passed Float is NOT greater than 0") {
        PosFloat.from(0.0F) shouldBe None
        PosFloat.from(-0.00001F) shouldBe None
        PosFloat.from(-99.9F) shouldBe None
      }
    } 
    it("should have a pretty toString") {
      // SKIP-SCALATESTJS-START
      PosFloat.from(42.0F).value.toString shouldBe "PosFloat(42.0)"
      // SKIP-SCALATESTJS-END
      //SCALATESTJS-ONLY PosFloat.from(42.0F).value.toString shouldBe "PosFloat(42)"
    }
    it("should return the same type from its unary_+ method") {
      +PosFloat(3.0F) shouldEqual PosFloat(3.0F)
    } 
    it("should be automatically widened to compatible AnyVal targets") {
      "PosFloat(3.0F): Int" shouldNot typeCheck
      "PosFloat(3.0F): Long" shouldNot typeCheck
      (PosFloat(3.0F): Float) shouldEqual 3.0F
      (PosFloat(3.0F): Double) shouldEqual 3.0

      "PosFloat(3.0F): PosInt" shouldNot typeCheck
      "PosFloat(3.0F): PosLong" shouldNot typeCheck
      (PosFloat(3.0F): PosFloat) shouldEqual PosFloat(3.0F)
      (PosFloat(3.0F): PosDouble) shouldEqual PosDouble(3.0)

      "PosFloat(3.0F): PosZInt" shouldNot typeCheck
      "PosFloat(3.0F): PosZLong" shouldNot typeCheck
      (PosFloat(3.0F): PosZFloat) shouldEqual PosZFloat(3.0F)
      (PosFloat(3.0F): PosZDouble) shouldEqual PosZDouble(3.0)
    }
    describe("when a compatible AnyVal is passed to a + method invoked on it") {
      it("should give the same AnyVal type back at compile time, and correct value at runtime") {
        // When adding a "primitive"
        val opInt = PosFloat(3.0F) + 3
        opInt shouldEqual 6.0F

        val opLong = PosFloat(3.0F) + 3L
        opLong shouldEqual 6.0F

        val opFloat = PosFloat(3.0F) + 3.0F
        opFloat shouldEqual 6.0F

        val opDouble = PosFloat(3.0F) + 3.0
        opDouble shouldEqual 6.0

        // When adding a Pos*
        val opPosInt = PosFloat(3.0F) + PosInt(3)
        opPosInt shouldEqual 6.0F

        val opPosLong = PosFloat(3.0F) + PosLong(3L)
        opPosLong shouldEqual 6.0F

        val opPosFloat = PosFloat(3.0F) + PosFloat(3.0F)
        opPosFloat shouldEqual 6.0F

        val opPosDouble = PosFloat(3.0F) + PosDouble(3.0)
        opPosDouble shouldEqual 6.0

        // When adding a *PosZ
        val opPosZ = PosFloat(3.0F) + PosZInt(3)
        opPosZ shouldEqual 6.0F

        val opPosZLong = PosFloat(3.0F) + PosZLong(3L)
        opPosZLong shouldEqual 6.0F

        val opPosZFloat = PosFloat(3.0F) + PosZFloat(3.0F)
        opPosZFloat shouldEqual 6.0F

        val opPosZDouble = PosFloat(3.0F) + PosZDouble(3.0)
        opPosZDouble shouldEqual 6.0
      }
    }

    describe("when created with apply method") {
  
      it("should compile when 8 is passed in") {
        "PosFloat(8)" should compile
        PosFloat(8).value shouldEqual 8.0F
        "PosFloat(8L)" should compile
        PosFloat(8L).value shouldEqual 8.0F
        "PosFloat(8.0F)" should compile
        PosFloat(8.0F).value shouldEqual 8.0F
      }
  
      it("should not compile when 0 is passed in") {
        "PosFloat(0)" shouldNot compile
        "PosFloat(0L)" shouldNot compile
        "PosFloat(0.0F)" shouldNot compile
      }
  
      it("should not compile when -8 is passed in") {
        "PosFloat(-8)" shouldNot compile
        "PosFloat(-8L)" shouldNot compile
        "PosFloat(-8.0F)" shouldNot compile
      }
      it("should not compile when x is passed in") {
        val a: Int = -8
        "PosFloat(a)" shouldNot compile
        val b: Long = -8L
        "PosFloat(b)" shouldNot compile
        val c: Float = -8.0F
        "PosFloat(c)" shouldNot compile
      }
    }
    describe("when specified as a plain-old Float") {

      def takesPosFloat(pos: PosFloat): Float = pos.value

      it("should compile when 8 is passed in") {
        "takesPosFloat(8)" should compile
        takesPosFloat(8) shouldEqual 8.0F
        "takesPosFloat(8L)" should compile
        takesPosFloat(8L) shouldEqual 8.0F
        "takesPosFloat(8.0F)" should compile
        takesPosFloat(8.0F) shouldEqual 8.0F
      }

      it("should not compile when 0 is passed in") {
        "takesPosFloat(0)" shouldNot compile
        "takesPosFloat(0L)" shouldNot compile
        "takesPosFloat(0.0F)" shouldNot compile
      }

      it("should not compile when -8 is passed in") {
        "takesPosFloat(-8)" shouldNot compile
        "takesPosFloat(-8L)" shouldNot compile
        "takesPosFloat(-8.0F)" shouldNot compile
      }

      it("should not compile when x is passed in") {
        val x: Int = -8
        "takesPosFloat(x)" shouldNot compile
        val b: Long = -8L
        "takesPosFloat(b)" shouldNot compile
        val c: Float = -8.0F
        "takesPosFloat(c)" shouldNot compile
      }

      it("should offer a unary + method that is consistent with Float") {
        forAll { (pfloat: PosFloat) =>
          (+pfloat).toFloat shouldEqual (+(pfloat.toFloat))
        }
      }

      it("should offer a unary - method that is consistent with Float") {
        forAll { (pfloat: PosFloat) =>
          (-pfloat) shouldEqual (-(pfloat.toFloat))
        }
      }

      it("should offer '<' comparison that is consistent with Float") {
        forAll { (pfloat: PosFloat, byte: Byte) =>
          (pfloat < byte) shouldEqual (pfloat.toFloat < byte)
        }
        forAll { (pfloat: PosFloat, short: Short) =>
          (pfloat < short) shouldEqual (pfloat.toFloat < short)
        }
        forAll { (pfloat: PosFloat, char: Char) =>
          (pfloat < char) shouldEqual (pfloat.toFloat < char)
        }
        forAll { (pfloat: PosFloat, int: Int) =>
          (pfloat < int) shouldEqual (pfloat.toFloat < int)
        }
        forAll { (pfloat: PosFloat, long: Long) =>
          (pfloat < long) shouldEqual (pfloat.toFloat < long)
        }
        forAll { (pfloat: PosFloat, float: Float) =>
          (pfloat < float) shouldEqual (pfloat.toFloat < float)
        }
        forAll { (pfloat: PosFloat, double: Double) =>
          (pfloat < double) shouldEqual (pfloat.toFloat < double)
        }
      }

      it("should offer '<=' comparison that is consistent with Float") {
        forAll { (pfloat: PosFloat, byte: Byte) =>
          (pfloat <= byte) shouldEqual (pfloat.toFloat <= byte)
        }
        forAll { (pfloat: PosFloat, char: Char) =>
          (pfloat <= char) shouldEqual (pfloat.toFloat <= char)
        }
        forAll { (pfloat: PosFloat, short: Short) =>
          (pfloat <= short) shouldEqual (pfloat.toFloat <= short)
        }
        forAll { (pfloat: PosFloat, int: Int) =>
          (pfloat <= int) shouldEqual (pfloat.toFloat <= int)
        }
        forAll { (pfloat: PosFloat, long: Long) =>
          (pfloat <= long) shouldEqual (pfloat.toFloat <= long)
        }
        forAll { (pfloat: PosFloat, float: Float) =>
          (pfloat <= float) shouldEqual (pfloat.toFloat <= float)
        }
        forAll { (pfloat: PosFloat, double: Double) =>
          (pfloat <= double) shouldEqual (pfloat.toFloat <= double)
        }
      }

      it("should offer '>' comparison that is consistent with Float") {
        forAll { (pfloat: PosFloat, byte: Byte) =>
          (pfloat > byte) shouldEqual (pfloat.toFloat > byte)
        }
        forAll { (pfloat: PosFloat, short: Short) =>
          (pfloat > short) shouldEqual (pfloat.toFloat > short)
        }
        forAll { (pfloat: PosFloat, char: Char) =>
          (pfloat > char) shouldEqual (pfloat.toFloat > char)
        }
        forAll { (pfloat: PosFloat, int: Int) =>
          (pfloat > int) shouldEqual (pfloat.toFloat > int)
        }
        forAll { (pfloat: PosFloat, long: Long) =>
          (pfloat > long) shouldEqual (pfloat.toFloat > long)
        }
        forAll { (pfloat: PosFloat, float: Float) =>
          (pfloat > float) shouldEqual (pfloat.toFloat > float)
        }
        forAll { (pfloat: PosFloat, double: Double) =>
          (pfloat > double) shouldEqual (pfloat.toFloat > double)
        }
      }

      it("should offer '>=' comparison that is consistent with Float") {
        forAll { (pfloat: PosFloat, byte: Byte) =>
          (pfloat >= byte) shouldEqual (pfloat.toFloat >= byte)
        }
        forAll { (pfloat: PosFloat, short: Short) =>
          (pfloat >= short) shouldEqual (pfloat.toFloat >= short)
        }
        forAll { (pfloat: PosFloat, char: Char) =>
          (pfloat >= char) shouldEqual (pfloat.toFloat >= char)
        }
        forAll { (pfloat: PosFloat, int: Int) =>
          (pfloat >= int) shouldEqual (pfloat.toFloat >= int)
        }
        forAll { (pfloat: PosFloat, long: Long) =>
          (pfloat >= long) shouldEqual (pfloat.toFloat >= long)
        }
        forAll { (pfloat: PosFloat, float: Float) =>
          (pfloat >= float) shouldEqual (pfloat.toFloat >= float)
        }
        forAll { (pfloat: PosFloat, double: Double) =>
          (pfloat >= double) shouldEqual (pfloat.toFloat >= double)
        }
      }

      it("should offer a '+' method that is consistent with Float") {
        forAll { (pfloat: PosFloat, byte: Byte) =>
          (pfloat + byte) shouldEqual (pfloat.toFloat + byte)
        }
        forAll { (pfloat: PosFloat, short: Short) =>
          (pfloat + short) shouldEqual (pfloat.toFloat + short)
        }
        forAll { (pfloat: PosFloat, char: Char) =>
          (pfloat + char) shouldEqual (pfloat.toFloat + char)
        }
        forAll { (pfloat: PosFloat, int: Int) =>
          (pfloat + int) shouldEqual (pfloat.toFloat + int)
        }
        forAll { (pfloat: PosFloat, long: Long) =>
          (pfloat + long) shouldEqual (pfloat.toFloat + long)
        }
        forAll { (pfloat: PosFloat, float: Float) =>
          (pfloat + float) shouldEqual (pfloat.toFloat + float)
        }
        forAll { (pfloat: PosFloat, double: Double) =>
          (pfloat + double) shouldEqual (pfloat.toFloat + double)
        }
      }

      it("should offer a '-' method that is consistent with Float") {
        forAll { (pfloat: PosFloat, byte: Byte) =>
          (pfloat - byte) shouldEqual (pfloat.toFloat - byte)
        }
        forAll { (pfloat: PosFloat, short: Short) =>
          (pfloat - short) shouldEqual (pfloat.toFloat - short)
        }
        forAll { (pfloat: PosFloat, char: Char) =>
          (pfloat - char) shouldEqual (pfloat.toFloat - char)
        }
        forAll { (pfloat: PosFloat, int: Int) =>
          (pfloat - int) shouldEqual (pfloat.toFloat - int)
        }
        forAll { (pfloat: PosFloat, long: Long) =>
          (pfloat - long) shouldEqual (pfloat.toFloat - long)
        }
        forAll { (pfloat: PosFloat, float: Float) =>
          (pfloat - float) shouldEqual (pfloat.toFloat - float)
        }
        forAll { (pfloat: PosFloat, double: Double) =>
          (pfloat - double) shouldEqual (pfloat.toFloat - double)
        }
      }

      it("should offer a '*' method that is consistent with Float") {
        forAll { (pfloat: PosFloat, byte: Byte) =>
          (pfloat * byte) shouldEqual (pfloat.toFloat * byte)
        }
        forAll { (pfloat: PosFloat, short: Short) =>
          (pfloat * short) shouldEqual (pfloat.toFloat * short)
        }
        forAll { (pfloat: PosFloat, char: Char) =>
          (pfloat * char) shouldEqual (pfloat.toFloat * char)
        }
        forAll { (pfloat: PosFloat, int: Int) =>
          (pfloat * int) shouldEqual (pfloat.toFloat * int)
        }
        forAll { (pfloat: PosFloat, long: Long) =>
          (pfloat * long) shouldEqual (pfloat.toFloat * long)
        }
        forAll { (pfloat: PosFloat, float: Float) =>
          (pfloat * float) shouldEqual (pfloat.toFloat * float)
        }
        forAll { (pfloat: PosFloat, double: Double) =>
          (pfloat * double) shouldEqual (pfloat.toFloat * double)
        }
      }

      it("should offer a '/' method that is consistent with Float") {
        forAll { (pfloat: PosFloat, byte: Byte) =>
          pfloat / byte shouldEqual pfloat.toFloat / byte
        }
        forAll { (pfloat: PosFloat, short: Short) =>
          pfloat / short shouldEqual pfloat.toFloat / short
        }
        forAll { (pfloat: PosFloat, char: Char) =>
          pfloat / char shouldEqual pfloat.toFloat / char
        }
        forAll { (pfloat: PosFloat, int: Int) =>
          pfloat / int shouldEqual pfloat.toFloat / int
        }
        forAll { (pfloat: PosFloat, long: Long) =>
          pfloat / long shouldEqual pfloat.toFloat / long
        }
        forAll { (pfloat: PosFloat, float: Float) =>
          pfloat / float shouldEqual pfloat.toFloat / float
        }
        forAll { (pfloat: PosFloat, double: Double) =>
          pfloat / double shouldEqual pfloat.toFloat / double
        }
      }

      // note: since a PosInt % 0 is NaN (as opposed to PosInt / 0, which is Infinity)
      // extra logic is needed to convert to a comparable type (boolean, in this case)
      it("should offer a '%' method that is consistent with Float") {
        forAll { (pfloat: PosFloat, byte: Byte) =>
          val res = pfloat % byte
          if (res.isNaN)
            (pfloat.toFloat % byte).isNaN shouldBe true
          else
            res shouldEqual pfloat.toFloat % byte
        }
        forAll { (pfloat: PosFloat, short: Short) =>
          val res = pfloat % short
          if (res.isNaN)
            (pfloat.toFloat % short).isNaN shouldBe true
          else
            res shouldEqual pfloat.toFloat % short
        }
        forAll { (pfloat: PosFloat, char: Char) =>
          val res = pfloat % char
          if (res.isNaN)
            (pfloat.toFloat % char).isNaN shouldBe true
          else
            res shouldEqual pfloat.toFloat % char
        }
        forAll { (pfloat: PosFloat, int: Int) =>
          val res = pfloat % int
          if (res.isNaN)
            (pfloat.toFloat % int).isNaN shouldBe true
          else
            res shouldEqual pfloat.toFloat % int
        }
        forAll { (pfloat: PosFloat, long: Long) =>
          val res = pfloat % long
          if (res.isNaN)
            (pfloat.toFloat % long).isNaN shouldBe true
          else
            res shouldEqual pfloat.toFloat % long
        }
        forAll { (pfloat: PosFloat, float: Float) =>
          val res = pfloat % float
          if (res.isNaN)
            (pfloat.toFloat % float).isNaN shouldBe true
          else
            res shouldEqual pfloat.toFloat % float
        }
        forAll { (pfloat: PosFloat, double: Double) =>
          val res = pfloat % double
          if (res.isNaN)
            (pfloat.toFloat % double).isNaN shouldBe true
          else
            res shouldEqual pfloat.toFloat % double
        }
      }

      it("should offer 'min' and 'max' methods that are consistent with Float") {
        forAll { (pfloat1: PosFloat, pfloat2: PosFloat) =>
          pfloat1.max(pfloat2).toFloat shouldEqual pfloat1.toFloat.max(pfloat2.toFloat)
          pfloat1.min(pfloat2).toFloat shouldEqual pfloat1.toFloat.min(pfloat2.toFloat)
        }
      }

      it("should offer an 'isWhole' method that is consistent with Float") {
        forAll { (pfloat: PosFloat) =>
          pfloat.isWhole shouldEqual pfloat.toFloat.isWhole
        }
      }

      it("should offer 'round', 'ceil', and 'floor' methods that are consistent with Float") {
        forAll { (pfloat: PosFloat) =>
          pfloat.round.toFloat shouldEqual pfloat.toFloat.round
          pfloat.ceil.toFloat shouldEqual pfloat.toFloat.ceil
          pfloat.floor.toFloat shouldEqual pfloat.toFloat.floor
        }
      }

      it("should offer 'toRadians' and 'toDegrees' methods that are consistent with Float") {
        forAll { (pfloat: PosFloat) =>
          pfloat.toRadians shouldEqual pfloat.toFloat.toRadians
        }
      }

      // SKIP-SCALATESTJS-START
      it("should offer 'to' and 'until' method that is consistent with Float") {
        def rangeEqual[T](a: NumericRange[T], b: NumericRange[T]): Boolean =
          a.start == b.start && a.end == b.end && a.step == b.step

        forAll { (pfloat: PosFloat, end: Float, step: Float) =>
          rangeEqual(pfloat.until(end).by(1f), pfloat.toFloat.until(end).by(1f)) shouldBe true
          rangeEqual(pfloat.until(end, step), pfloat.toFloat.until(end, step)) shouldBe true
          rangeEqual(pfloat.to(end).by(1f), pfloat.toFloat.to(end).by(1f)) shouldBe true
          rangeEqual(pfloat.to(end, step), pfloat.toFloat.to(end, step)) shouldBe true
        }
      }
      // SKIP-SCALATESTJS-END

      it("should offer widening methods for basic types that are consistent with Float") {
        forAll { (pfloat: PosFloat) =>
          def widen(value: Float): Float = value
          widen(pfloat) shouldEqual widen(pfloat.toFloat)
        }
        forAll { (pfloat: PosFloat) =>
          def widen(value: Double): Double = value
          widen(pfloat) shouldEqual widen(pfloat.toFloat)
        }
        forAll { (pfloat: PosFloat) =>
          def widen(value: PosDouble): PosDouble = value
          widen(pfloat) shouldEqual widen(PosDouble.from(pfloat.toFloat).get)
        }
        forAll { (pfloat: PosFloat) =>
          def widen(value: PosZFloat): PosZFloat = value
          widen(pfloat) shouldEqual widen(PosZFloat.from(pfloat.toFloat).get)
        }
        forAll { (pfloat: PosFloat) =>
          def widen(value: PosZDouble): PosZDouble = value
          widen(pfloat) shouldEqual widen(PosZDouble.from(pfloat.toFloat).get)
        }
      }
    }
  }
}
  
