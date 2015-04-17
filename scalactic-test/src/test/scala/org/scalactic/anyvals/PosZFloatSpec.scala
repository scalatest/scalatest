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
import org.scalatest.prop.NyayaGeneratorDrivenPropertyChecks._
import japgolly.nyaya.test.Gen
// SKIP-SCALATESTJS-START
import scala.collection.immutable.NumericRange
// SKIP-SCALATESTJS-END
import scala.collection.mutable.WrappedArray
import OptionValues._
//import org.scalactic.StrictCheckedEquality

class PosZFloatSpec extends FunSpec with Matchers/* with StrictCheckedEquality*/ {

  implicit val posZFloatGen: Gen[PosZFloat] =
    for {i <- Gen.choosefloat(1, Float.MaxValue)} yield PosZFloat.from(i).get

  implicit val intGen: Gen[Int] = Gen.int
  implicit val longGen: Gen[Long] = Gen.long
  implicit val shortGen: Gen[Short] = Gen.short
  implicit val charGen: Gen[Char] = Gen.char
  implicit val floatGen: Gen[Float] = Gen.float
  implicit val doubleGen: Gen[Double] = Gen.double
  implicit val byteGen: Gen[Byte] = Gen.byte

  describe("A PosZFloat") {
    describe("should offer a from factory method that") {
      it("returns Some[PosZFloat] if the passed Float is greater than or equal to 0") {
        PosZFloat.from(0.0f).value.value shouldBe 0.0f
        PosZFloat.from(50.23f).value.value shouldBe 50.23f
        PosZFloat.from(100.0f).value.value shouldBe 100.0f
      }
      it("returns None if the passed Float is NOT greater than or equal to 0") {
        PosZFloat.from(-0.00001f) shouldBe None
        PosZFloat.from(-99.9f) shouldBe None
      }
    } 
    it("should have a pretty toString") {
      // SKIP-SCALATESTJS-START
      PosZFloat.from(42.0f).value.toString shouldBe "PosZFloat(42.0)"
      // SKIP-SCALATESTJS-END
      //SCALATESTJS-ONLY PosZFloat.from(42.0f).value.toString shouldBe "PosZFloat(42)"
    }
    it("should return the same type from its unary_+ method") {
      +PosZFloat(3.0F) shouldEqual PosZFloat(3.0F)
    } 
    it("should be automatically widened to compatible AnyVal targets") {
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
    describe("when a compatible AnyVal is passed to a + method invoked on it") {
      it("should give the same AnyVal type back at compile time, and correct value at runtime") {
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

    describe("when created with apply method") {
  
      it("should compile when 8 is passed in") {
        "PosZFloat(8)" should compile
        PosZFloat(8).value shouldEqual 8.0F
        "PosZFloat(8L)" should compile
        PosZFloat(8L).value shouldEqual 8.0F
        "PosZFloat(8.0F)" should compile
        PosZFloat(8.0F).value shouldEqual 8.0F
      }
  
      it("should compile when 0 is passed in") {
        "PosZFloat(0)" should compile
        PosZFloat(0).value shouldEqual 0.0F
        "PosZFloat(0L)" should compile
        PosZFloat(0L).value shouldEqual 0.0F
        "PosZFloat(0.0F)" should compile
        PosZFloat(0.0F).value shouldEqual 0.0F
      }

      it("should not compile when -8 is passed in") {
        "PosZFloat(-8)" shouldNot compile
        "PosZFloat(-8L)" shouldNot compile
        "PosZFloat(-8.0F)" shouldNot compile
      }

      it("should not compile when x is passed in") {
        val a: Int = -8
        "PosZFloat(a)" shouldNot compile
        val b: Long = -8L
        "PosZFloat(b)" shouldNot compile
        val c: Float = -8.0F
        "PosZFloat(c)" shouldNot compile
      }
    }
    describe("when specified as a plain-old Float") {

      def takesPosZFloat(pos: PosZFloat): Float = pos.value

      it("should compile when 8 is passed in") {
        "takesPosZFloat(8)" should compile
        takesPosZFloat(8) shouldEqual 8.0F
        "takesPosZFloat(8L)" should compile
        takesPosZFloat(8L) shouldEqual 8.0F
        "takesPosZFloat(8.0F)" should compile
        takesPosZFloat(8.0F) shouldEqual 8.0F
      }

      it("should compile when 0 is passed in") {
        "takesPosZFloat(0)" should compile
        takesPosZFloat(0) shouldEqual 0.0F
        "takesPosZFloat(0L)" should compile
        takesPosZFloat(0L) shouldEqual 0.0F
        "takesPosZFloat(0.0F)" should compile
        takesPosZFloat(0.0F) shouldEqual 0.0F
      }

      it("should not compile when -8 is passed in") {
        "takesPosZFloat(-8)" shouldNot compile
        "takesPosZFloat(-8L)" shouldNot compile
        "takesPosZFloat(-8.0F)" shouldNot compile
      }

      it("should not compile when x is passed in") {
        val x: Int = -8
        "takesPosZFloat(x)" shouldNot compile
        val b: Long = -8L
        "takesPosZFloat(b)" shouldNot compile
        val c: Float = -8.0F
        "takesPosZFloat(c)" shouldNot compile
      }
    }

    it("should offer a unary + method that is consistent with Float") {
      forAll { (pzfloat: PosZFloat) =>
        (+pzfloat).toFloat shouldEqual (+(pzfloat.toFloat))
      }
    }

    it("should offer a unary - method that is consistent with Float") {
      forAll { (pzfloat: PosZFloat) =>
        (-pzfloat) shouldEqual (-(pzfloat.toFloat))
      }
    }

    it("should offer '<' comparison that is consistent with Float") {
      forAll { (pzfloat: PosZFloat, byte: Byte) =>
        (pzfloat < byte) shouldEqual (pzfloat.toFloat < byte)
      }
      forAll { (pzfloat: PosZFloat, short: Short) =>
        (pzfloat < short) shouldEqual (pzfloat.toFloat < short)
      }
      forAll { (pzfloat: PosZFloat, char: Char) =>
        (pzfloat < char) shouldEqual (pzfloat.toFloat < char)
      }
      forAll { (pzfloat: PosZFloat, int: Int) =>
        (pzfloat < int) shouldEqual (pzfloat.toFloat < int)
      }
      forAll { (pzfloat: PosZFloat, long: Long) =>
        (pzfloat < long) shouldEqual (pzfloat.toFloat < long)
      }
      forAll { (pzfloat: PosZFloat, float: Float) =>
        (pzfloat < float) shouldEqual (pzfloat.toFloat < float)
      }
      forAll { (pzfloat: PosZFloat, double: Double) =>
        (pzfloat < double) shouldEqual (pzfloat.toFloat < double)
      }
    }

    it("should offer '<=' comparison that is consistent with Float") {
      forAll { (pzfloat: PosZFloat, byte: Byte) =>
        (pzfloat <= byte) shouldEqual (pzfloat.toFloat <= byte)
      }
      forAll { (pzfloat: PosZFloat, char: Char) =>
        (pzfloat <= char) shouldEqual (pzfloat.toFloat <= char)
      }
      forAll { (pzfloat: PosZFloat, short: Short) =>
        (pzfloat <= short) shouldEqual (pzfloat.toFloat <= short)
      }
      forAll { (pzfloat: PosZFloat, int: Int) =>
        (pzfloat <= int) shouldEqual (pzfloat.toFloat <= int)
      }
      forAll { (pzfloat: PosZFloat, long: Long) =>
        (pzfloat <= long) shouldEqual (pzfloat.toFloat <= long)
      }
      forAll { (pzfloat: PosZFloat, float: Float) =>
        (pzfloat <= float) shouldEqual (pzfloat.toFloat <= float)
      }
      forAll { (pzfloat: PosZFloat, double: Double) =>
        (pzfloat <= double) shouldEqual (pzfloat.toFloat <= double)
      }
    }

    it("should offer '>' comparison that is consistent with Float") {
      forAll { (pzfloat: PosZFloat, byte: Byte) =>
        (pzfloat > byte) shouldEqual (pzfloat.toFloat > byte)
      }
      forAll { (pzfloat: PosZFloat, short: Short) =>
        (pzfloat > short) shouldEqual (pzfloat.toFloat > short)
      }
      forAll { (pzfloat: PosZFloat, char: Char) =>
        (pzfloat > char) shouldEqual (pzfloat.toFloat > char)
      }
      forAll { (pzfloat: PosZFloat, int: Int) =>
        (pzfloat > int) shouldEqual (pzfloat.toFloat > int)
      }
      forAll { (pzfloat: PosZFloat, long: Long) =>
        (pzfloat > long) shouldEqual (pzfloat.toFloat > long)
      }
      forAll { (pzfloat: PosZFloat, float: Float) =>
        (pzfloat > float) shouldEqual (pzfloat.toFloat > float)
      }
      forAll { (pzfloat: PosZFloat, double: Double) =>
        (pzfloat > double) shouldEqual (pzfloat.toFloat > double)
      }
    }

    it("should offer '>=' comparison that is consistent with Float") {
      forAll { (pzfloat: PosZFloat, byte: Byte) =>
        (pzfloat >= byte) shouldEqual (pzfloat.toFloat >= byte)
      }
      forAll { (pzfloat: PosZFloat, short: Short) =>
        (pzfloat >= short) shouldEqual (pzfloat.toFloat >= short)
      }
      forAll { (pzfloat: PosZFloat, char: Char) =>
        (pzfloat >= char) shouldEqual (pzfloat.toFloat >= char)
      }
      forAll { (pzfloat: PosZFloat, int: Int) =>
        (pzfloat >= int) shouldEqual (pzfloat.toFloat >= int)
      }
      forAll { (pzfloat: PosZFloat, long: Long) =>
        (pzfloat >= long) shouldEqual (pzfloat.toFloat >= long)
      }
      forAll { (pzfloat: PosZFloat, float: Float) =>
        (pzfloat >= float) shouldEqual (pzfloat.toFloat >= float)
      }
      forAll { (pzfloat: PosZFloat, double: Double) =>
        (pzfloat >= double) shouldEqual (pzfloat.toFloat >= double)
      }
    }

    it("should offer a '+' method that is consistent with Float") {
      forAll { (pzfloat: PosZFloat, byte: Byte) =>
        (pzfloat + byte) shouldEqual (pzfloat.toFloat + byte)
      }
      forAll { (pzfloat: PosZFloat, short: Short) =>
        (pzfloat + short) shouldEqual (pzfloat.toFloat + short)
      }
      forAll { (pzfloat: PosZFloat, char: Char) =>
        (pzfloat + char) shouldEqual (pzfloat.toFloat + char)
      }
      forAll { (pzfloat: PosZFloat, int: Int) =>
        (pzfloat + int) shouldEqual (pzfloat.toFloat + int)
      }
      forAll { (pzfloat: PosZFloat, long: Long) =>
        (pzfloat + long) shouldEqual (pzfloat.toFloat + long)
      }
      forAll { (pzfloat: PosZFloat, float: Float) =>
        (pzfloat + float) shouldEqual (pzfloat.toFloat + float)
      }
      forAll { (pzfloat: PosZFloat, double: Double) =>
        (pzfloat + double) shouldEqual (pzfloat.toFloat + double)
      }
    }

    it("should offer a '-' method that is consistent with Float") {
      forAll { (pzfloat: PosZFloat, byte: Byte) =>
        (pzfloat - byte) shouldEqual (pzfloat.toFloat - byte)
      }
      forAll { (pzfloat: PosZFloat, short: Short) =>
        (pzfloat - short) shouldEqual (pzfloat.toFloat - short)
      }
      forAll { (pzfloat: PosZFloat, char: Char) =>
        (pzfloat - char) shouldEqual (pzfloat.toFloat - char)
      }
      forAll { (pzfloat: PosZFloat, int: Int) =>
        (pzfloat - int) shouldEqual (pzfloat.toFloat - int)
      }
      forAll { (pzfloat: PosZFloat, long: Long) =>
        (pzfloat - long) shouldEqual (pzfloat.toFloat - long)
      }
      forAll { (pzfloat: PosZFloat, float: Float) =>
        (pzfloat - float) shouldEqual (pzfloat.toFloat - float)
      }
      forAll { (pzfloat: PosZFloat, double: Double) =>
        (pzfloat - double) shouldEqual (pzfloat.toFloat - double)
      }
    }

    it("should offer a '*' method that is consistent with Float") {
      forAll { (pzfloat: PosZFloat, byte: Byte) =>
        (pzfloat * byte) shouldEqual (pzfloat.toFloat * byte)
      }
      forAll { (pzfloat: PosZFloat, short: Short) =>
        (pzfloat * short) shouldEqual (pzfloat.toFloat * short)
      }
      forAll { (pzfloat: PosZFloat, char: Char) =>
        (pzfloat * char) shouldEqual (pzfloat.toFloat * char)
      }
      forAll { (pzfloat: PosZFloat, int: Int) =>
        (pzfloat * int) shouldEqual (pzfloat.toFloat * int)
      }
      forAll { (pzfloat: PosZFloat, long: Long) =>
        (pzfloat * long) shouldEqual (pzfloat.toFloat * long)
      }
      forAll { (pzfloat: PosZFloat, float: Float) =>
        (pzfloat * float) shouldEqual (pzfloat.toFloat * float)
      }
      forAll { (pzfloat: PosZFloat, double: Double) =>
        (pzfloat * double) shouldEqual (pzfloat.toFloat * double)
      }
    }

    it("should offer a '/' method that is consistent with Float") {
      forAll { (pzfloat: PosZFloat, byte: Byte) =>
        pzfloat / byte shouldEqual pzfloat.toFloat / byte
      }
      forAll { (pzfloat: PosZFloat, short: Short) =>
        pzfloat / short shouldEqual pzfloat.toFloat / short
      }
      forAll { (pzfloat: PosZFloat, char: Char) =>
        pzfloat / char shouldEqual pzfloat.toFloat / char
      }
      forAll { (pzfloat: PosZFloat, int: Int) =>
        pzfloat / int shouldEqual pzfloat.toFloat / int
      }
      forAll { (pzfloat: PosZFloat, long: Long) =>
        pzfloat / long shouldEqual pzfloat.toFloat / long
      }
      forAll { (pzfloat: PosZFloat, float: Float) =>
        pzfloat / float shouldEqual pzfloat.toFloat / float
      }
      forAll { (pzfloat: PosZFloat, double: Double) =>
        pzfloat / double shouldEqual pzfloat.toFloat / double
      }
    }

    // note: since a PosInt % 0 is NaN (as opposed to PosInt / 0, which is Infinity)
    // extra logic is needed to convert to a comparable type (boolean, in this case)
    it("should offer a '%' method that is consistent with Float") {
      forAll { (pzfloat: PosZFloat, byte: Byte) =>
        val res = pzfloat % byte
        if (res.isNaN)
          (pzfloat.toFloat % byte).isNaN shouldBe true
        else
          res shouldEqual pzfloat.toFloat % byte
      }
      forAll { (pzfloat: PosZFloat, short: Short) =>
        val res = pzfloat % short
        if (res.isNaN)
          (pzfloat.toFloat % short).isNaN shouldBe true
        else
          res shouldEqual pzfloat.toFloat % short
      }
      forAll { (pzfloat: PosZFloat, char: Char) =>
        val res = pzfloat % char
        if (res.isNaN)
          (pzfloat.toFloat % char).isNaN shouldBe true
        else
          res shouldEqual pzfloat.toFloat % char
      }
      forAll { (pzfloat: PosZFloat, int: Int) =>
        val res = pzfloat % int
        if (res.isNaN)
          (pzfloat.toFloat % int).isNaN shouldBe true
        else
          res shouldEqual pzfloat.toFloat % int
      }
      forAll { (pzfloat: PosZFloat, long: Long) =>
        val res = pzfloat % long
        if (res.isNaN)
          (pzfloat.toFloat % long).isNaN shouldBe true
        else
          res shouldEqual pzfloat.toFloat % long
      }
      forAll { (pzfloat: PosZFloat, float: Float) =>
        val res = pzfloat % float
        if (res.isNaN)
          (pzfloat.toFloat % float).isNaN shouldBe true
        else
          res shouldEqual pzfloat.toFloat % float
      }
      forAll { (pzfloat: PosZFloat, double: Double) =>
        val res = pzfloat % double
        if (res.isNaN)
          (pzfloat.toFloat % double).isNaN shouldBe true
        else
          res shouldEqual pzfloat.toFloat % double
      }
    }

    it("should offer 'min' and 'max' methods that are consistent with Float") {
      forAll { (pfloat1: PosZFloat, pfloat2: PosZFloat) =>
        pfloat1.max(pfloat2).toFloat shouldEqual pfloat1.toFloat.max(pfloat2.toFloat)
        pfloat1.min(pfloat2).toFloat shouldEqual pfloat1.toFloat.min(pfloat2.toFloat)
      }
    }

    it("should offer an 'isWhole' method that is consistent with Float") {
      forAll { (pzfloat: PosZFloat) =>
        pzfloat.isWhole shouldEqual pzfloat.toFloat.isWhole
      }
    }

    it("should offer 'round', 'ceil', and 'floor' methods that are consistent with Float") {
      forAll { (pzfloat: PosZFloat) =>
        pzfloat.round.toFloat shouldEqual pzfloat.toFloat.round
        pzfloat.ceil.toFloat shouldEqual pzfloat.toFloat.ceil
        pzfloat.floor.toFloat shouldEqual pzfloat.toFloat.floor
      }
    }

    it("should offer 'toRadians' and 'toDegrees' methods that are consistent with Float") {
      forAll { (pzfloat: PosZFloat) =>
        pzfloat.toRadians.toFloat shouldEqual pzfloat.toFloat.toRadians
        pzfloat.toDegrees.toFloat shouldEqual pzfloat.toFloat.toDegrees
      }
    }

    // SKIP-SCALATESTJS-START
    it("should offer 'to' and 'until' method that is consistent with Float") {
      def rangeEqual[T](a: NumericRange[T], b: NumericRange[T]): Boolean =
        a.start == b.start && a.end == b.end && a.step == b.step

      forAll { (pzfloat: PosZFloat, end: Float, step: Float) =>
        rangeEqual(pzfloat.until(end).by(1f), pzfloat.toFloat.until(end).by(1f)) shouldBe true
        rangeEqual(pzfloat.until(end, step), pzfloat.toFloat.until(end, step)) shouldBe true
        rangeEqual(pzfloat.to(end).by(1f), pzfloat.toFloat.to(end).by(1f)) shouldBe true
        rangeEqual(pzfloat.to(end, step), pzfloat.toFloat.to(end, step)) shouldBe true
      }
    }
    // SKIP-SCALATESTJS-END

    it("should offer widening methods for basic types that are consistent with Float") {
      forAll { (pzfloat: PosZFloat) =>
        def widen(value: Float): Float = value
        widen(pzfloat) shouldEqual widen(pzfloat.toFloat)
      }
      forAll { (pzfloat: PosZFloat) =>
        def widen(value: Double): Double = value
        widen(pzfloat) shouldEqual widen(pzfloat.toFloat)
      }
      forAll { (pzfloat: PosZFloat) =>
        def widen(value: PosZDouble): PosZDouble = value
        widen(pzfloat) shouldEqual widen(PosZDouble.from(pzfloat.toFloat).get)
      }
    }
  }
}

