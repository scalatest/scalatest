/*
 * Copyright 2001-2016 Artima, Inc.
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

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Gen.choose
import org.scalatest._
import org.scalactic.Equality
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.prop.PropertyChecks
// SKIP-SCALATESTJS-START
import scala.collection.immutable.NumericRange
// SKIP-SCALATESTJS-END
import scala.collection.mutable.WrappedArray
import OptionValues._
import scala.util.{Failure, Success, Try}
import org.scalatest.Inspectors

class NonZeroDoubleSpec extends FunSpec with Matchers with PropertyChecks with TypeCheckedTripleEquals {

  val nonZeroDoubleGen: Gen[NonZeroDouble] =
    for {i <- choose(Double.MinValue, Double.MaxValue)} yield {
      if (i == 0.0)
        NonZeroDouble.ensuringValid(1.0)
      else
        NonZeroDouble.ensuringValid(i)
    }

  implicit val arbNonZeroDouble: Arbitrary[NonZeroDouble] = Arbitrary(nonZeroDoubleGen)

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

  describe("A NonZeroDouble") {
    describe("should offer a from factory method that") {
      it("returns Some[NonZeroDouble] if the passed Double is greater than 0") {
        NonZeroDouble.from(50.23).value.value shouldBe 50.23
        NonZeroDouble.from(100.0).value.value shouldBe 100.0
      }
      it("returns Some[NonZeroDouble] if the passed Double is lesser than 0") {
        NonZeroDouble.from(-0.00001).value.value shouldBe -0.00001
        NonZeroDouble.from(-99.9).value.value shouldBe -99.9
      }
      it("returns None if the passed Double is 0") {
        NonZeroDouble.from(0.0) shouldBe None
      }
    }
    describe("should offer an ensuringValid factory method that") {
      it("returns NonZeroDouble if the passed Double is greater than 0") {
        NonZeroDouble.ensuringValid(50.23).value shouldBe 50.23
        NonZeroDouble.ensuringValid(100.0).value shouldBe 100.0
        NonZeroDouble.ensuringValid(Double.PositiveInfinity).value shouldBe Double.PositiveInfinity
      }
      it("returns NonZeroDouble if the passed Double is lesser than 0") {
        NonZeroDouble.ensuringValid(-0.00001).value shouldBe -0.00001
        NonZeroDouble.ensuringValid(-99.9).value shouldBe -99.9
        NonZeroDouble.ensuringValid(Double.NegativeInfinity).value shouldBe Double.NegativeInfinity
      }
      it("throws AssertionError if the passed Double is NOT greater than 0 or NaN") {
        an [AssertionError] should be thrownBy NonZeroDouble.ensuringValid(0.0)
        an [AssertionError] should be thrownBy NonZeroDouble.ensuringValid(Double.NaN)
      }
    }
    describe("should offer an isValid predicate method that") {
      it("returns true if the passed Double is greater than 0") {
        NonZeroDouble.isValid(50.23) shouldBe true
        NonZeroDouble.isValid(100.0) shouldBe true
        NonZeroDouble.isValid(0.0) shouldBe false
        NonZeroDouble.isValid(-0.0) shouldBe false
        NonZeroDouble.isValid(-0.00001) shouldBe true
        NonZeroDouble.isValid(-99.9) shouldBe true
      }
    }
    describe("should offer a fromOrElse factory method that") {
      it("returns a NonZeroDouble if the passed Double is greater than 0") {
        NonZeroDouble.fromOrElse(50.23, NonZeroDouble(42.0)).value shouldBe 50.23
        NonZeroDouble.fromOrElse(100.0, NonZeroDouble(42.0)).value shouldBe 100.0
      }
      it("returns a NonZeroDouble if the passed Double is lesser than 0") {
        NonZeroDouble.fromOrElse(-0.00001, NonZeroDouble(42.0)).value shouldBe -0.00001
        NonZeroDouble.fromOrElse(-99.9, NonZeroDouble(42.0)).value shouldBe -99.9
      }
      it("returns a given default if the passed Double is 0 or NaN") {
        NonZeroDouble.fromOrElse(0.0, NonZeroDouble(42.0)).value shouldBe 42.0
        NonZeroDouble.fromOrElse(Double.NaN, NonZeroDouble(42.0)).value shouldBe 42.0
      }
    }
    it("should offer MaxValue, MinValue, and MinPositiveValue factory methods") {
      NonZeroDouble.MaxValue shouldEqual NonZeroDouble.from(Double.MaxValue).get
      NonZeroDouble.MinValue shouldEqual
        NonZeroDouble.from(Double.MinValue).get
      NonZeroDouble.MinPositiveValue shouldEqual
        NonZeroDouble.from(Double.MinPositiveValue).get
    }
    it("should offer a PositiveInfinity factory method") {
      NonZeroDouble.PositiveInfinity shouldEqual NonZeroDouble.ensuringValid(Double.PositiveInfinity)
    }
    it("should offer a NegativeInfinity factory method") {
      NonZeroDouble.NegativeInfinity shouldEqual NonZeroDouble.ensuringValid(Double.NegativeInfinity)
    }
    it("should have a pretty toString") {
      // SKIP-SCALATESTJS-START
      NonZeroDouble.from(42.0).value.toString shouldBe "NonZeroDouble(42.0)"
      // SKIP-SCALATESTJS-END
      //SCALATESTJS-ONLY NonZeroDouble.from(42.0).value.toString shouldBe "NonZeroDouble(42)"
    }
    it("should return the same type from its unary_+ method") {
      +NonZeroDouble(3.0) shouldEqual NonZeroDouble(3.0)
    }
    it("should be automatically widened to compatible AnyVal targets") {
      "NonZeroDouble(3.0): Int" shouldNot typeCheck
      "NonZeroDouble(3.0): Long" shouldNot typeCheck
      "NonZeroDouble(3.0): Float" shouldNot typeCheck
      (NonZeroDouble(3.0): Double) shouldEqual 3.0

      "NonZeroDouble(3.0): PosInt" shouldNot typeCheck
      "NonZeroDouble(3.0): PosLong" shouldNot typeCheck
      "NonZeroDouble(3.0): PosFloat" shouldNot typeCheck
      "NonZeroDouble(3.0): PosDouble" shouldNot typeCheck

      "NonZeroDouble(3.0): PosZInt" shouldNot typeCheck
      "NonZeroDouble(3.0): PosZLong" shouldNot typeCheck
      "NonZeroDouble(3.0): PosZFloat" shouldNot typeCheck
      "NonZeroDouble(3.0): PosZDouble" shouldNot typeCheck
    }

    it("should be sortable") {
      val xs = List(NonZeroDouble(2.2), NonZeroDouble(4.4), NonZeroDouble(1.1),
        NonZeroDouble(3.3))
      xs.sorted shouldEqual List(NonZeroDouble(1.1), NonZeroDouble(2.2), NonZeroDouble(3.3),
        NonZeroDouble(4.4))
    }

    describe("when a compatible AnyVal is passed to a + method invoked on it") {
      it("should give the same AnyVal type back at compile time, and correct value at runtime") {
        // When adding a "primitive"
        val opInt = NonZeroDouble(3.0) + 3
        opInt shouldEqual 6.0

        val opLong = NonZeroDouble(3.0) + 3L
        opLong shouldEqual 6.0

        val opFloat = NonZeroDouble(3.0) + 3.0F
        opFloat shouldEqual 6.0

        val opDouble = NonZeroDouble(3.0) + 3.0
        opDouble shouldEqual 6.0

        // When adding a Pos*
        val opPosInt = NonZeroDouble(3.0) + PosInt(3)
        opPosInt shouldEqual 6.0

        val opPosLong = NonZeroDouble(3.0) + PosLong(3L)
        opPosLong shouldEqual 6.0

        val opPosFloat = NonZeroDouble(3.0) + PosFloat(3.0F)
        opPosFloat shouldEqual 6.0

        val opPosDouble = NonZeroDouble(3.0) + PosDouble(3.0)
        opPosDouble shouldEqual 6.0

        // When adding a *PosZ
        val opPosZ = NonZeroDouble(3.0) + PosZInt(3)
        opPosZ shouldEqual 6.0

        val opPosZLong = NonZeroDouble(3.0) + PosZLong(3L)
        opPosZLong shouldEqual 6.0

        val opPosZFloat = NonZeroDouble(3.0) + PosZFloat(3.0F)
        opPosZFloat shouldEqual 6.0

        val opPosZDouble = NonZeroDouble(3.0) + PosZDouble(3.0)
        opPosZDouble shouldEqual 6.0

        // When adding a NonZero*
        val opNonZeroInt = NonZeroDouble(3.0) + NonZeroInt(3)
        opNonZeroInt shouldEqual 6

        val opNonZeroLong = NonZeroDouble(3.0) + NonZeroLong(3L)
        opNonZeroLong shouldEqual 6L

        val opNonZeroFloat = NonZeroDouble(3.0) + NonZeroFloat(3.0F)
        opNonZeroFloat shouldEqual 6.0F

        val opNonZeroDouble = NonZeroDouble(3.0) + NonZeroDouble(3.0)
        opNonZeroDouble shouldEqual 6.0
      }
    }

    describe("when created with apply method") {

      it("should compile when 8 is passed in") {
        "NonZeroDouble(8)" should compile
        NonZeroDouble(8).value shouldEqual 8.0
        "NonZeroDouble(8L)" should compile
        NonZeroDouble(8L).value shouldEqual 8.0
        "NonZeroDouble(8.0F)" should compile
        NonZeroDouble(8.0F).value shouldEqual 8.0
        "NonZeroDouble(8.0)" should compile
        NonZeroDouble(8.0).value shouldEqual 8.0
      }

      it("should not compile when 0 is passed in") {
        "NonZeroDouble(0)" shouldNot compile
        "NonZeroDouble(0L)" shouldNot compile
        "NonZeroDouble(0.0F)" shouldNot compile
        "NonZeroDouble(0.0)" shouldNot compile
      }

      it("should compile when -8 is passed in") {
        "NonZeroDouble(-8)" should compile
        NonZeroDouble(-8).value shouldEqual -8.0
        "NonZeroDouble(-8L)" should compile
        NonZeroDouble(-8L).value shouldEqual -8.0
        "NonZeroDouble(-8.0F)" should compile
        NonZeroDouble(-8.0F).value shouldEqual -8.0
        "NonZeroDouble(-8.0)" should compile
        NonZeroDouble(-8.0).value shouldEqual -8.0
      }
      it("should not compile when x is passed in") {
        val a: Int = -8
        "NonZeroDouble(a)" shouldNot compile
        val b: Long = -8L
        "NonZeroDouble(b)" shouldNot compile
        val c: Float = -8.0F
        "NonZeroDouble(c)" shouldNot compile
        val d: Double = -8.0
        "NonZeroDouble(d)" shouldNot compile
      }
    }
    describe("when specified as a plain-old Double") {

      def takesNonZeroDouble(pos: NonZeroDouble): Double = pos.value

      it("should compile when 8 is passed in") {
        "takesNonZeroDouble(8)" should compile
        takesNonZeroDouble(8) shouldEqual 8.0
        "takesNonZeroDouble(8L)" should compile
        takesNonZeroDouble(8L) shouldEqual 8.0
        "takesNonZeroDouble(8.0F)" should compile
        takesNonZeroDouble(8.0F) shouldEqual 8.0
        "takesNonZeroDouble(8.0)" should compile
        takesNonZeroDouble(8.0) shouldEqual 8.0
      }

      it("should not compile when 0 is passed in") {
        "takesNonZeroDouble(0)" shouldNot compile
        "takesNonZeroDouble(0L)" shouldNot compile
        "takesNonZeroDouble(0.0F)" shouldNot compile
        "takesNonZeroDouble(0.0)" shouldNot compile
      }

      it("should compile when -8 is passed in") {
        "takesNonZeroDouble(-8)" should compile
        takesNonZeroDouble(-8) shouldEqual -8.0
        "takesNonZeroDouble(-8L)" should compile
        takesNonZeroDouble(-8L) shouldEqual -8.0
        "takesNonZeroDouble(-8.0F)" should compile
        takesNonZeroDouble(-8.0F) shouldEqual -8.0
        "takesNonZeroDouble(-8.0)" should compile
        takesNonZeroDouble(-8.0) shouldEqual -8.0
      }

      it("should not compile when x is passed in") {
        val x: Int = -8
        "takesNonZeroDouble(x)" shouldNot compile
        val b: Long = -8L
        "takesNonZeroDouble(b)" shouldNot compile
        val c: Float = -8.0F
        "takesNonZeroDouble(c)" shouldNot compile
        val d: Double = -8.0
        "takesNonZeroDouble(d)" shouldNot compile
      }
    }

    it("should offer a unary + method that is consistent with Double") {
      forAll { (pdouble: NonZeroDouble) =>
        (+pdouble).toDouble shouldEqual (+(pdouble.toDouble))
      }
    }

    it("should offer a unary - method that is consistent with Double") {
      forAll { (pdouble: NonZeroDouble) =>
        (-pdouble) shouldEqual (-(pdouble.toDouble))
      }
    }

    it("should offer '<' comparison that is consistent with Double") {
      forAll { (pdouble: NonZeroDouble, byte: Byte) =>
        (pdouble < byte) shouldEqual (pdouble.toDouble < byte)
      }
      forAll { (pdouble: NonZeroDouble, short: Short) =>
        (pdouble < short) shouldEqual (pdouble.toDouble < short)
      }
      forAll { (pdouble: NonZeroDouble, char: Char) =>
        (pdouble < char) shouldEqual (pdouble.toDouble < char)
      }
      forAll { (pdouble: NonZeroDouble, int: Int) =>
        (pdouble < int) shouldEqual (pdouble.toDouble < int)
      }
      forAll { (pdouble: NonZeroDouble, long: Long) =>
        (pdouble < long) shouldEqual (pdouble.toDouble < long)
      }
      forAll { (pdouble: NonZeroDouble, float: Float) =>
        (pdouble < float) shouldEqual (pdouble.toDouble < float)
      }
      forAll { (pdouble: NonZeroDouble, double: Double) =>
        (pdouble < double) shouldEqual (pdouble.toDouble < double)
      }
    }

    it("should offer '<=' comparison that is consistent with Double") {
      forAll { (pdouble: NonZeroDouble, byte: Byte) =>
        (pdouble <= byte) shouldEqual (pdouble.toDouble <= byte)
      }
      forAll { (pdouble: NonZeroDouble, char: Char) =>
        (pdouble <= char) shouldEqual (pdouble.toDouble <= char)
      }
      forAll { (pdouble: NonZeroDouble, short: Short) =>
        (pdouble <= short) shouldEqual (pdouble.toDouble <= short)
      }
      forAll { (pdouble: NonZeroDouble, int: Int) =>
        (pdouble <= int) shouldEqual (pdouble.toDouble <= int)
      }
      forAll { (pdouble: NonZeroDouble, long: Long) =>
        (pdouble <= long) shouldEqual (pdouble.toDouble <= long)
      }
      forAll { (pdouble: NonZeroDouble, float: Float) =>
        (pdouble <= float) shouldEqual (pdouble.toDouble <= float)
      }
      forAll { (pdouble: NonZeroDouble, double: Double) =>
        (pdouble <= double) shouldEqual (pdouble.toDouble <= double)
      }
    }

    it("should offer '>' comparison that is consistent with Double") {
      forAll { (pdouble: NonZeroDouble, byte: Byte) =>
        (pdouble > byte) shouldEqual (pdouble.toDouble > byte)
      }
      forAll { (pdouble: NonZeroDouble, short: Short) =>
        (pdouble > short) shouldEqual (pdouble.toDouble > short)
      }
      forAll { (pdouble: NonZeroDouble, char: Char) =>
        (pdouble > char) shouldEqual (pdouble.toDouble > char)
      }
      forAll { (pdouble: NonZeroDouble, int: Int) =>
        (pdouble > int) shouldEqual (pdouble.toDouble > int)
      }
      forAll { (pdouble: NonZeroDouble, long: Long) =>
        (pdouble > long) shouldEqual (pdouble.toDouble > long)
      }
      forAll { (pdouble: NonZeroDouble, float: Float) =>
        (pdouble > float) shouldEqual (pdouble.toDouble > float)
      }
      forAll { (pdouble: NonZeroDouble, double: Double) =>
        (pdouble > double) shouldEqual (pdouble.toDouble > double)
      }
    }

    it("should offer '>=' comparison that is consistent with Double") {
      forAll { (pdouble: NonZeroDouble, byte: Byte) =>
        (pdouble >= byte) shouldEqual (pdouble.toDouble >= byte)
      }
      forAll { (pdouble: NonZeroDouble, short: Short) =>
        (pdouble >= short) shouldEqual (pdouble.toDouble >= short)
      }
      forAll { (pdouble: NonZeroDouble, char: Char) =>
        (pdouble >= char) shouldEqual (pdouble.toDouble >= char)
      }
      forAll { (pdouble: NonZeroDouble, int: Int) =>
        (pdouble >= int) shouldEqual (pdouble.toDouble >= int)
      }
      forAll { (pdouble: NonZeroDouble, long: Long) =>
        (pdouble >= long) shouldEqual (pdouble.toDouble >= long)
      }
      forAll { (pdouble: NonZeroDouble, float: Float) =>
        (pdouble >= float) shouldEqual (pdouble.toDouble >= float)
      }
      forAll { (pdouble: NonZeroDouble, double: Double) =>
        (pdouble >= double) shouldEqual (pdouble.toDouble >= double)
      }
    }

    it("should offer a '+' method that is consistent with Double") {
      forAll { (pdouble: NonZeroDouble, byte: Byte) =>
        (pdouble + byte) shouldEqual (pdouble.toDouble + byte)
      }
      forAll { (pdouble: NonZeroDouble, short: Short) =>
        (pdouble + short) shouldEqual (pdouble.toDouble + short)
      }
      forAll { (pdouble: NonZeroDouble, char: Char) =>
        (pdouble + char) shouldEqual (pdouble.toDouble + char)
      }
      forAll { (pdouble: NonZeroDouble, int: Int) =>
        (pdouble + int) shouldEqual (pdouble.toDouble + int)
      }
      forAll { (pdouble: NonZeroDouble, long: Long) =>
        (pdouble + long) shouldEqual (pdouble.toDouble + long)
      }
      forAll { (pdouble: NonZeroDouble, float: Float) =>
        (pdouble + float) shouldEqual (pdouble.toDouble + float)
      }
      forAll { (pdouble: NonZeroDouble, double: Double) =>
        (pdouble + double) shouldEqual (pdouble.toDouble + double)
      }
    }

    it("should offer a '-' method that is consistent with Double") {
      forAll { (pdouble: NonZeroDouble, byte: Byte) =>
        (pdouble - byte) shouldEqual (pdouble.toDouble - byte)
      }
      forAll { (pdouble: NonZeroDouble, short: Short) =>
        (pdouble - short) shouldEqual (pdouble.toDouble - short)
      }
      forAll { (pdouble: NonZeroDouble, char: Char) =>
        (pdouble - char) shouldEqual (pdouble.toDouble - char)
      }
      forAll { (pdouble: NonZeroDouble, int: Int) =>
        (pdouble - int) shouldEqual (pdouble.toDouble - int)
      }
      forAll { (pdouble: NonZeroDouble, long: Long) =>
        (pdouble - long) shouldEqual (pdouble.toDouble - long)
      }
      forAll { (pdouble: NonZeroDouble, float: Float) =>
        (pdouble - float) shouldEqual (pdouble.toDouble - float)
      }
      forAll { (pdouble: NonZeroDouble, double: Double) =>
        (pdouble - double) shouldEqual (pdouble.toDouble - double)
      }
    }

    it("should offer a '*' method that is consistent with Double") {
      forAll { (pdouble: NonZeroDouble, byte: Byte) =>
        (pdouble * byte) shouldEqual (pdouble.toDouble * byte)
      }
      forAll { (pdouble: NonZeroDouble, short: Short) =>
        (pdouble * short) shouldEqual (pdouble.toDouble * short)
      }
      forAll { (pdouble: NonZeroDouble, char: Char) =>
        (pdouble * char) shouldEqual (pdouble.toDouble * char)
      }
      forAll { (pdouble: NonZeroDouble, int: Int) =>
        (pdouble * int) shouldEqual (pdouble.toDouble * int)
      }
      forAll { (pdouble: NonZeroDouble, long: Long) =>
        (pdouble * long) shouldEqual (pdouble.toDouble * long)
      }
      forAll { (pdouble: NonZeroDouble, float: Float) =>
        (pdouble * float) shouldEqual (pdouble.toDouble * float)
      }
      forAll { (pdouble: NonZeroDouble, double: Double) =>
        (pdouble * double) shouldEqual (pdouble.toDouble * double)
      }
    }

    it("should offer a '/' method that is consistent with Double") {
      forAll { (pdouble: NonZeroDouble, byte: Byte) =>
        pdouble / byte shouldEqual pdouble.toDouble / byte
      }
      forAll { (pdouble: NonZeroDouble, short: Short) =>
        pdouble / short shouldEqual pdouble.toDouble / short
      }
      forAll { (pdouble: NonZeroDouble, char: Char) =>
        pdouble / char shouldEqual pdouble.toDouble / char
      }
      forAll { (pdouble: NonZeroDouble, int: Int) =>
        pdouble / int shouldEqual pdouble.toDouble / int
      }
      forAll { (pdouble: NonZeroDouble, long: Long) =>
        pdouble / long shouldEqual pdouble.toDouble / long
      }
      forAll { (pdouble: NonZeroDouble, float: Float) =>
        pdouble / float shouldEqual pdouble.toDouble / float
      }
      forAll { (pdouble: NonZeroDouble, double: Double) =>
        pdouble / double shouldEqual pdouble.toDouble / double
      }
    }

    // note: since a PosInt % 0 is NaN (as opposed to PosInt / 0, which is Infinity)
    // extra logic is needed to convert to a comparable type (boolean, in this case)
    it("should offer a '%' method that is consistent with Double") {
      forAll { (pdouble: NonZeroDouble, byte: Byte) =>
        val res = pdouble % byte
        if (res.isNaN)
          (pdouble.toDouble % byte).isNaN shouldBe true
        else
          res shouldEqual pdouble.toDouble % byte
      }
      forAll { (pdouble: NonZeroDouble, short: Short) =>
        val res = pdouble % short
        if (res.isNaN)
          (pdouble.toDouble % short).isNaN shouldBe true
        else
          res shouldEqual pdouble.toDouble % short
      }
      forAll { (pdouble: NonZeroDouble, char: Char) =>
        val res = pdouble % char
        if (res.isNaN)
          (pdouble.toDouble % char).isNaN shouldBe true
        else
          res shouldEqual pdouble.toDouble % char
      }
      forAll { (pdouble: NonZeroDouble, int: Int) =>
        val res = pdouble % int
        if (res.isNaN)
          (pdouble.toDouble % int).isNaN shouldBe true
        else
          res shouldEqual pdouble.toDouble % int
      }
      forAll { (pdouble: NonZeroDouble, long: Long) =>
        val res = pdouble % long
        if (res.isNaN)
          (pdouble.toDouble % long).isNaN shouldBe true
        else
          res shouldEqual pdouble.toDouble % long
      }
      forAll { (pdouble: NonZeroDouble, float: Float) =>
        val res = pdouble % float
        if (res.isNaN)
          (pdouble.toDouble % float).isNaN shouldBe true
        else
          res shouldEqual pdouble.toDouble % float
      }
      forAll { (pdouble: NonZeroDouble, double: Double) =>
        val res = pdouble % double
        if (res.isNaN)
          (pdouble.toDouble % double).isNaN shouldBe true
        else
          res shouldEqual pdouble.toDouble % double
      }
    }

    it("should offer 'min' and 'max' methods that are consistent with Double") {
      forAll { (pdouble1: NonZeroDouble, pdouble2: NonZeroDouble) =>
        pdouble1.max(pdouble2).toDouble shouldEqual pdouble1.toDouble.max(pdouble2.toDouble)
        pdouble1.min(pdouble2).toDouble shouldEqual pdouble1.toDouble.min(pdouble2.toDouble)
      }
    }

    it("should offer an 'isWhole' method that is consistent with Double") {
      forAll { (pdouble: NonZeroDouble) =>
        pdouble.isWhole shouldEqual pdouble.toDouble.isWhole
      }
    }

    it("should offer 'toRadians' and 'toDegrees' methods that are consistent with Double") {
      forAll { (pdouble: NonZeroDouble) =>
        pdouble.toRadians shouldEqual pdouble.toDouble.toRadians
      }
    }

    // SKIP-SCALATESTJS-START
    it("should offer 'to' and 'until' method that is consistent with Double") {
      def rangeEqual[T](a: NumericRange[T], b: NumericRange[T]): Boolean =
        a.start == b.start && a.end == b.end && a.step == b.step

      forAll { (pdouble: NonZeroDouble, end: Double, step: Double) =>
        rangeEqual(pdouble.until(end).by(1f), pdouble.toDouble.until(end).by(1f)) shouldBe true
        rangeEqual(pdouble.until(end, step), pdouble.toDouble.until(end, step)) shouldBe true
        rangeEqual(pdouble.to(end).by(1f), pdouble.toDouble.to(end).by(1f)) shouldBe true
        rangeEqual(pdouble.to(end, step), pdouble.toDouble.to(end, step)) shouldBe true
      }
    }
    // SKIP-SCALATESTJS-END

    it("should offer widening methods for basic types that are consistent with Double") {
      forAll { (pdouble: NonZeroDouble) =>
        def widen(value: Double): Double = value
        widen(pdouble) shouldEqual widen(pdouble.toDouble)
      }
    }

    it("should offer an ensuringValid method that takes a Double => Double, throwing AssertionError if the result is invalid") {
      NonZeroDouble(33.0).ensuringValid(_ + 1.0) shouldEqual NonZeroDouble(34.0)
      NonZeroDouble(33.0).ensuringValid(_ => Double.PositiveInfinity) shouldEqual NonZeroDouble.ensuringValid(Double.PositiveInfinity)
      NonZeroDouble(-33.0).ensuringValid(_ + 1.0) shouldEqual NonZeroDouble(-32.0)
      NonZeroDouble(-33.0).ensuringValid(_ => Double.NegativeInfinity) shouldEqual NonZeroDouble.ensuringValid(Double.NegativeInfinity)
      an [AssertionError] should be thrownBy { NonZeroDouble.MaxValue.ensuringValid(_ - NonZeroDouble.MaxValue) }
      an [AssertionError] should be thrownBy { NonZeroDouble.MaxValue.ensuringValid(_ => Double.NaN) }
    }
  }
}