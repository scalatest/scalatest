/*
 * Copyright 2001-2025 Artima, Inc.
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
package org.scalactic.opaquetypes

import org.scalactic.Equality
import org.scalatest.*
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import OptionValues.*

import scala.util.{Failure, Success, Try}
import org.scalactic.{Validation, Pass, Fail}
import org.scalactic.{Or, Good, Bad}

trait NegIntSpecSupport {

  implicit def tryEquality[T]: Equality[Try[T]] = new Equality[Try[T]] {
    override def areEqual(a: Try[T], b: Any): Boolean = a match {
      case Success(double: Double) if double.isNaN =>  // This is because in scala.js x/0 results to NaN not ArithmetricException like in jvm, and we need to make sure Success(NaN) == Success(NaN) is true to pass the test.
        b match {
          case Success(bDouble: Double) if bDouble.isNaN => true
          case _ => false
        }
      // I needed this because with GenDrivenPropertyChecks, got:
      // [info] - should offer a '%' method that is consistent with Int *** FAILED ***
      // [info]   Success(NaN) did not equal Success(NaN) (NegIntExperiment.scala:498)
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

class NegIntSpec extends funspec.AnyFunSpec with matchers.should.Matchers with GeneratorDrivenPropertyChecks with NegIntSpecSupport {

  describe("A NegInt") {

    describe("should offer a from factory method that") {
      it("returns Some[NegInt] if the passed Int is lesser than 0") {
        NegInt.from(-50).value.value shouldBe -50
        NegInt.from(-100).value.value shouldBe -100
      }

      it("returns None if the passed Int is NOT lesser than 0") {
        NegInt.from(0) shouldBe None
        NegInt.from(1) shouldBe None
        NegInt.from(99) shouldBe None
      }
    }
    describe("should offer an ensuringValid factory method that") {
      it("returns NegInt if the passed Int is lesser than 0") {
        NegInt.ensuringValid(-50).value shouldBe -50
        NegInt.ensuringValid(-100).value shouldBe -100
      }

      it("throws AssertionError if the passed Int is NOT lesser than 0") {
        an [AssertionError] should be thrownBy NegInt.ensuringValid(0)
        an [AssertionError] should be thrownBy NegInt.ensuringValid(1)
        an [AssertionError] should be thrownBy NegInt.ensuringValid(99)
      }
    }
    describe("should offer a tryingValid factory method that") {
      import TryValues.*
      it("returns a NegInt wrapped in a Success if the passed Int is lesser than 0") {
        NegInt.tryingValid(-50).success.value.value shouldBe -50
        NegInt.tryingValid(-100).success.value.value shouldBe -100
      }

      it("returns an AssertionError wrapped in a Failure if the passed Int is NOT lesser than 0") {
        NegInt.tryingValid(0).failure.exception shouldBe an [AssertionError]
        NegInt.tryingValid(1).failure.exception shouldBe an [AssertionError]
        NegInt.tryingValid(99).failure.exception shouldBe an [AssertionError]
      }
    }
    describe("should offer a passOrElse factory method that") {
      it("returns a Pass if the given Int is lesser than 0") {
        NegInt.passOrElse(-50)(i => i) shouldBe Pass
        NegInt.passOrElse(-100)(i => i) shouldBe Pass
      }
      it("returns an error value produced by passing the given Int to the given function if the passed Int is NOT lesser than 0, wrapped in a Fail") {
        NegInt.passOrElse(0)(i => s"$i did not taste good") shouldBe Fail("0 did not taste good")
        NegInt.passOrElse(1)(i => i) shouldBe Fail(1)
        NegInt.passOrElse(99)(i => i.toLong + 3L) shouldBe Fail(102L)
      }
    }
    describe("should offer a goodOrElse factory method that") {
      it("returns a NegInt wrapped in a Good if the given Int is lesser than 0") {
        NegInt.goodOrElse(-50)(i => i) shouldBe Good(NegInt(-50))
        NegInt.goodOrElse(-100)(i => i) shouldBe Good(NegInt(-100))
      }
      it("returns an error value produced by passing the given Int to the given function if the passed Int is NOT lesser than 0, wrapped in a Bad") {
        NegInt.goodOrElse(0)(i => s"$i did not taste good") shouldBe Bad("0 did not taste good")
        NegInt.goodOrElse(1)(i => i) shouldBe Bad(1)
        NegInt.goodOrElse(99)(i => i.toLong + 3L) shouldBe Bad(102L)
      }
    }
    describe("should offer a rightOrElse factory method that") {
      it("returns a NegInt wrapped in a Right if the given Int is lesser than 0") {
        NegInt.rightOrElse(-50)(i => i) shouldBe Right(NegInt(-50))
        NegInt.rightOrElse(-100)(i => i) shouldBe Right(NegInt(-100))
      }
      it("returns an error value produced by passing the given Int to the given function if the passed Int is NOT lesser than 0, wrapped in a Left") {
        NegInt.rightOrElse(0)(i => s"$i did not taste good") shouldBe Left("0 did not taste good")
        NegInt.rightOrElse(1)(i => i) shouldBe Left(1)
        NegInt.rightOrElse(99)(i => i.toLong + 3L) shouldBe Left(102L)
      }
    }
    describe("should offer an isValid predicate method that") {
      it("returns true if the passed Int is lesser than 0") {
        NegInt.isValid(-50) shouldBe true
        NegInt.isValid(-100) shouldBe true
        NegInt.isValid(0) shouldBe false
        NegInt.isValid(-0) shouldBe false
        NegInt.isValid(1) shouldBe false
        NegInt.isValid(99) shouldBe false
      }
    }
    describe("should offer a fromOrElse factory method that") {
      it("returns a NegInt if the passed Int is lesser than 0") {
        NegInt.fromOrElse(-50, NegInt(-42)).value shouldBe -50
        NegInt.fromOrElse(-100, NegInt(-42)).value shouldBe -100
      }
      it("returns a given default if the passed Int is NOT lesser than 0") {
        NegInt.fromOrElse(0, NegInt(-42)).value shouldBe -42
        NegInt.fromOrElse(1, NegInt(-42)).value shouldBe -42
        NegInt.fromOrElse(99, NegInt(-42)).value shouldBe -42
      }
    }
    it("should offer MaxValue and MinValue factory methods") {
      NegInt.MaxValue shouldEqual NegInt.from(-1).get
    }

    it("should be sortable") {
      val xs = List(NegInt(-2), NegInt(-4), NegInt(-1), NegInt(-3))
      xs.sorted shouldEqual List(NegInt(-4), NegInt(-3), NegInt(-2), NegInt(-1))
    }

    describe("when created with apply method") {

      it("should compile when 8 is passed in") {
        "NegInt(-8)" should compile
        NegInt(-8).value shouldEqual -8
      }

      it("should not compile when 0 is passed in") {
        "NegInt(0)" shouldNot compile
      }

      it("should not compile when -8 is passed in") {
        "NegInt(8)" shouldNot compile
      }

      it("should not compile when x is passed in") {
        val x: Int = -8
        "NegInt(x)" shouldNot compile
      }
    }

    describe("when specified as a plain-old Int") {

      def takesNegInt(pos: NegInt): Int = pos.value

      it("should compile when -8 is passed in") {
        "takesNegInt(-8)" should compile
        takesNegInt(-8) shouldEqual -8
      }

      it("should not compile when 0 is passed in") {
        "takesNegInt(0)" shouldNot compile
      }

      it("should not compile when 8 is passed in") {
        "takesNegInt(8)" shouldNot compile
      }

      it("should not compile when x is passed in") {
        val x: Int = -8
        "takesNegInt(x)" shouldNot compile
      }
    }

    it("should offer a unary ~ method that is consistent with Int") {
      forAll { (pint: NegInt) =>
        (~pint) shouldEqual (~(pint.toInt))
      }
    }

    it("should offer a unary + method that is consistent with Int") {
      forAll { (p: NegInt) =>
        (+p).toInt shouldEqual (+(p.toInt))
      }
    }

    it("should offer a unary - method that returns PosInt") {
      forAll { (p: NegInt) =>
        (-p) shouldEqual (-(p.toInt))
      }
    }

    it("should offer << methods that are consistent with Int") {
      forAll { (pint: NegInt, shift: Int) =>
        pint << shift shouldEqual pint.toInt << shift
      }
      forAll { (pint: NegInt, shift: Long) =>
        pint << shift shouldEqual pint.toInt << shift
      }
    }

    it("should offer >>> methods that are consistent with Int") {
      forAll { (pint: NegInt, shift: Int) =>
        pint >>> shift shouldEqual pint.toInt >>> shift
      }
      forAll { (pint: NegInt, shift: Long) =>
        pint >>> shift shouldEqual pint.toInt >>> shift
      }
    }

    it("should offer >> methods that are consistent with Int") {
      forAll { (pint: NegInt, shift: Int) =>
        pint >> shift shouldEqual pint.toInt >> shift
      }
      forAll { (pint: NegInt, shift: Long) =>
        pint >> shift shouldEqual pint.toInt >> shift
      }
    }

    it("should offer a '|' method that is consistent with Int") {
      forAll { (pint: NegInt, byte: Byte) =>
        (pint | byte) shouldEqual (pint.toInt | byte)
      }
      forAll { (pint: NegInt, short: Short) =>
        (pint | short) shouldEqual (pint.toInt | short)
      }
      forAll { (pint: NegInt, char: Char) =>
        (pint | char) shouldEqual (pint.toInt | char)
      }
      forAll { (pint: NegInt, int: Int) =>
        (pint | int) shouldEqual (pint.toInt | int)
      }
      forAll { (pint: NegInt, long: Long) =>
        (pint | long) shouldEqual (pint.toInt | long)
      }
    }

    it("should offer an '&' method that is consistent with Int") {
      forAll { (pint: NegInt, byte: Byte) =>
        (pint & byte) shouldEqual (pint.toInt & byte)
      }
      forAll { (pint: NegInt, short: Short) =>
        (pint & short) shouldEqual (pint.toInt & short)
      }
      forAll { (pint: NegInt, char: Char) =>
        (pint & char) shouldEqual (pint.toInt & char)
      }
      forAll { (pint: NegInt, int: Int) =>
        (pint & int) shouldEqual (pint.toInt & int)
      }
      forAll { (pint: NegInt, long: Long) =>
        (pint & long) shouldEqual (pint.toInt & long)
      }
    }

    it("should offer an '^' method that is consistent with Int") {
      forAll { (pint: NegInt, byte: Byte) =>
        (pint ^ byte) shouldEqual (pint.toInt ^ byte)
      }
      forAll { (pint: NegInt, char: Char) =>
        (pint ^ char) shouldEqual (pint.toInt ^ char)
      }
      forAll { (pint: NegInt, short: Short) =>
        (pint ^ short) shouldEqual (pint.toInt ^ short)
      }
      forAll { (pint: NegInt, int: Int) =>
        (pint ^ int) shouldEqual (pint.toInt ^ int)
      }
      forAll { (pint: NegInt, long: Long) =>
        (pint ^ long) shouldEqual (pint.toInt ^ long)
      }
    }

    it("should offer 'min' and 'max' methods that are consistent with Int") {
      forAll { (pint1: NegInt, pint2: NegInt) =>
        pint1.max(pint2).toInt shouldEqual pint1.toInt.max(pint2.toInt)
        pint1.min(pint2).toInt shouldEqual pint1.toInt.min(pint2.toInt)
      }
    }

    it("should offer a 'toBinaryString' method that is consistent with Int") {
      forAll { (pint: NegInt) =>
        pint.toBinaryString shouldEqual pint.toInt.toBinaryString
      }
    }

    it("should offer a 'toHexString' method that is consistent with Int") {
      forAll { (pint: NegInt) =>
        pint.toHexString shouldEqual pint.toInt.toHexString
      }
    }

    it("should offer a 'toOctalString' method that is consistent with Int") {
      forAll { (pint: NegInt) =>
        pint.toOctalString shouldEqual pint.toInt.toOctalString
      }
    }

    it("should offer 'to' and 'until' methods that are consistent with Int") {
      forAll { (pint: NegInt, end: Int, step: Int) =>
        // The reason we need this is that in Scala 2.10, the equals check (used by shouldEqual below) will call range.length
        // and it'll cause IllegalArgumentException to be thrown when we do the Try(x) shouldEqual Try(y) assertion below,
        // while starting from scala 2.11 the equals call implementation does not call .length.
        // To make the behavior consistent for all scala versions, we explicitly call .length for all returned Range, and
        // shall it throws IllegalArgumentException, it will be wrapped as Failure for the Try.
        def ensuringValid(range: Range): Range = {
          range.length  // IllegalArgumentException will be thrown if it is an invalid range, this will turn the Success to Failure for Try
          range
        }

        Try(ensuringValid(pint.to(end))) shouldEqual Try(ensuringValid(pint.toInt.to(end)))
        Try(ensuringValid(pint.to(end, step))) shouldEqual Try(ensuringValid(pint.toInt.to(end, step)))
        Try(ensuringValid(pint.until(end))) shouldEqual Try(ensuringValid(pint.toInt.until(end)))
        Try(ensuringValid(pint.until(end, step))) shouldEqual Try(ensuringValid(pint.toInt.until(end, step)))
      }
    }

    it("should offer an ensuringValid method that takes an Int => Int, throwing AssertionError if the result is invalid") {
      NegInt(-33).ensuringValid(_ + 1) shouldEqual NegInt(-32)
      an [AssertionError] should be thrownBy { NegInt(-1).ensuringValid(_ + 1) }
    }
  }
  describe("NegInt") {
    it("should be automatically widened to compatible AnyVal targets") {
      (NegInts.NegInt(-3) : Int) shouldEqual -3
      "(NegInts.NegInt(-3) : Byte)" shouldNot compile
      "(NegInts.NegInt(-3) : Short)" shouldNot compile
      "(NegInts.NegInt(-3) : Char)" shouldNot compile
      "(NegInts.NegInt(-3) : Long)" shouldNot compile
      "(NegInts.NegInt(-3) : Float)" shouldNot compile
      "(NegInts.NegInt(-3) : Double)" shouldNot compile
      "(NegInts.NegInt(-3) : PosInts.PosInt)" shouldNot compile
      "(NegInts.NegInt(-3) : PosInts.PosZInt)" shouldNot compile
      "(NegInts.NegInt(-3) : NegInts.NegZInt)" shouldNot compile
      "(NegInts.NegInt(-3) : NonZeroInts.NonZeroInt)" shouldNot compile
      "(NegInts.NegInt(-3) : Percentages.PercentageInt)" shouldNot compile
      "(NegInts.NegInt(-3) : Numerics.NumericChar)" shouldNot compile
      "(NegInts.NegInt(-3) : PosLongs.PosLong)" shouldNot compile
      "(NegInts.NegInt(-3) : PosLongs.PosZLong)" shouldNot compile
      "(NegInts.NegInt(-3) : NegLongs.NegLong)" shouldNot compile
      "(NegInts.NegInt(-3) : NegLongs.NegZLong)" shouldNot compile
      "(NegInts.NegInt(-3) : NonZeroLongs.NonZeroLong)" shouldNot compile
      "(NegInts.NegInt(-3) : PosFloats.PosFloat)" shouldNot compile
      "(NegInts.NegInt(-3) : PosFloats.PosZFloat)" shouldNot compile
      "(NegInts.NegInt(-3) : NegFloats.NegFloat)" shouldNot compile
      "(NegInts.NegInt(-3) : NegFloats.NegZFloat)" shouldNot compile
      "(NegInts.NegInt(-3) : NonZeroFloats.NonZeroFloat)" shouldNot compile
      "(NegInts.NegInt(-3) : NonZeroFloats.NonZeroFiniteFloat)" shouldNot compile
      "(NegInts.NegInt(-3) : PosFloats.PosFiniteFloat)" shouldNot compile
      "(NegInts.NegInt(-3) : PosFloats.PosZFiniteFloat)" shouldNot compile
      "(NegInts.NegInt(-3) : NegFloats.NegFiniteFloat)" shouldNot compile
      "(NegInts.NegInt(-3) : NegFloats.NegZFiniteFloat)" shouldNot compile
      "(NegInts.NegInt(-3) : Finites.FiniteFloat)" shouldNot compile
      "(NegInts.NegInt(-3) : PosDoubles.PosDouble)" shouldNot compile
      "(NegInts.NegInt(-3) : PosDoubles.PosZDouble)" shouldNot compile
      "(NegInts.NegInt(-3) : NegDoubles.NegDouble)" shouldNot compile
      "(NegInts.NegInt(-3) : NegDoubles.NegZDouble)" shouldNot compile
      "(NegInts.NegInt(-3) : NonZeroDoubles.NonZeroDouble)" shouldNot compile
      "(NegInts.NegInt(-3) : NonZeroDoubles.NonZeroFiniteDouble)" shouldNot compile
      "(NegInts.NegInt(-3) : PosDoubles.PosFiniteDouble)" shouldNot compile
      "(NegInts.NegInt(-3) : PosDoubles.PosZFiniteDouble)" shouldNot compile
      "(NegInts.NegInt(-3) : NegDoubles.NegFiniteDouble)" shouldNot compile
      "(NegInts.NegInt(-3) : NegDoubles.NegZFiniteDouble)" shouldNot compile
      "(NegInts.NegInt(-3) : Finites.FiniteDouble)" shouldNot compile
    }
    it("when a compatible AnyVal is passed to a + method invoked on it should give the same AnyVal type back at compile time, and correct value at runtime") {
      NegInts.NegInt(-3) + 3.toByte shouldEqual 0
      NegInts.NegInt(-3) + 3.toShort shouldEqual 0
      NegInts.NegInt(-3) + '3' shouldEqual 48
      NegInts.NegInt(-3) + 3 shouldEqual 0
      NegInts.NegInt(-3) + 3L shouldEqual 0L
      NegInts.NegInt(-3) + 3.0f shouldEqual 0.0f
      NegInts.NegInt(-3) + 3.0 shouldEqual 0.0
      NegInts.NegInt(-3) + PosInts.PosInt(3) shouldEqual 0
      NegInts.NegInt(-3) + PosInts.PosZInt(3) shouldEqual 0
      NegInts.NegInt(-3) + NegInts.NegZInt(-3) shouldEqual -6
      NegInts.NegInt(-3) + NonZeroInts.NonZeroInt(3) shouldEqual 0
      NegInts.NegInt(-3) + Percentages.PercentageInt(50) shouldEqual 47
      NegInts.NegInt(-3) + PosLongs.PosLong(3L) shouldEqual 0L
      NegInts.NegInt(-3) + PosLongs.PosZLong(3L) shouldEqual 0L
      NegInts.NegInt(-3) + NegLongs.NegLong(-3L) shouldEqual -6L
      NegInts.NegInt(-3) + NegLongs.NegZLong(-3L) shouldEqual -6L
      NegInts.NegInt(-3) + NonZeroLongs.NonZeroLong(3L) shouldEqual 0L
      NegInts.NegInt(-3) + PosFloats.PosFloat(3.0f) shouldEqual 0.0f
      NegInts.NegInt(-3) + PosFloats.PosZFloat(3.0f) shouldEqual 0.0f
      NegInts.NegInt(-3) + NegFloats.NegFloat(-3.0f) shouldEqual -6.0f
      NegInts.NegInt(-3) + NegFloats.NegZFloat(-3.0f) shouldEqual -6.0f
      NegInts.NegInt(-3) + NonZeroFloats.NonZeroFloat(3.0f) shouldEqual 0.0f
      NegInts.NegInt(-3) + NonZeroFloats.NonZeroFiniteFloat(3.0f) shouldEqual 0.0f
      NegInts.NegInt(-3) + PosFloats.PosFiniteFloat(3.0f) shouldEqual 0.0f
      NegInts.NegInt(-3) + PosFloats.PosZFiniteFloat(3.0f) shouldEqual 0.0f
      NegInts.NegInt(-3) + NegFloats.NegFiniteFloat(-3.0f) shouldEqual -6.0f
      NegInts.NegInt(-3) + NegFloats.NegZFiniteFloat(-3.0f) shouldEqual -6.0f
      NegInts.NegInt(-3) + Finites.FiniteFloat(3.0f) shouldEqual 0.0f
      NegInts.NegInt(-3) + PosDoubles.PosDouble(3.0) shouldEqual 0.0
      NegInts.NegInt(-3) + NegDoubles.NegDouble(-3.0) shouldEqual -6.0
      NegInts.NegInt(-3) + NegDoubles.NegZDouble(-3.0) shouldEqual -6.0
      NegInts.NegInt(-3) + NonZeroDoubles.NonZeroDouble(3.0) shouldEqual 0.0
      NegInts.NegInt(-3) + NonZeroDoubles.NonZeroFiniteDouble(3.0) shouldEqual 0.0
      NegInts.NegInt(-3) + PosDoubles.PosFiniteDouble(3.0) shouldEqual 0.0
      NegInts.NegInt(-3) + PosDoubles.PosZFiniteDouble(3.0) shouldEqual 0.0
      NegInts.NegInt(-3) + NegDoubles.NegFiniteDouble(-3.0) shouldEqual -6.0
      NegInts.NegInt(-3) + NegDoubles.NegZFiniteDouble(-3.0) shouldEqual -6.0
      NegInts.NegInt(-3) + Finites.FiniteDouble(3.0) shouldEqual 0.0
    }
    it("when a compatible AnyVal is passed to a - method invoked on it should give the same AnyVal type back at compile time, and correct value at runtime") {
      NegInts.NegInt(-3) - 3.toByte shouldEqual -6
      NegInts.NegInt(-3) - 3.toShort shouldEqual -6
      NegInts.NegInt(-3) - '3' shouldEqual -54
      NegInts.NegInt(-3) - 3 shouldEqual -6
      NegInts.NegInt(-3) - 3L shouldEqual -6L
      NegInts.NegInt(-3) - 3.0f shouldEqual -6.0f
      NegInts.NegInt(-3) - 3.0 shouldEqual -6.0
      NegInts.NegInt(-3) - PosInts.PosInt(3) shouldEqual -6
      NegInts.NegInt(-3) - PosInts.PosZInt(3) shouldEqual -6
      NegInts.NegInt(-3) - NegInts.NegZInt(-3) shouldEqual 0
      NegInts.NegInt(-3) - NonZeroInts.NonZeroInt(3) shouldEqual -6
      NegInts.NegInt(-3) - Percentages.PercentageInt(50) shouldEqual -53
      NegInts.NegInt(-3) - PosLongs.PosLong(3L) shouldEqual -6L
      NegInts.NegInt(-3) - PosLongs.PosZLong(3L) shouldEqual -6L
      NegInts.NegInt(-3) - NegLongs.NegLong(-3L) shouldEqual 0L
      NegInts.NegInt(-3) - NegLongs.NegZLong(-3L) shouldEqual 0L
      NegInts.NegInt(-3) - NonZeroLongs.NonZeroLong(3L) shouldEqual -6L
      NegInts.NegInt(-3) - PosFloats.PosFloat(3.0f) shouldEqual -6.0f
      NegInts.NegInt(-3) - PosFloats.PosZFloat(3.0f) shouldEqual -6.0f
      NegInts.NegInt(-3) - NegFloats.NegFloat(-3.0f) shouldEqual 0.0f
      NegInts.NegInt(-3) - NegFloats.NegZFloat(-3.0f) shouldEqual 0.0f
      NegInts.NegInt(-3) - NonZeroFloats.NonZeroFloat(3.0f) shouldEqual -6.0f
      NegInts.NegInt(-3) - NonZeroFloats.NonZeroFiniteFloat(3.0f) shouldEqual -6.0f
      NegInts.NegInt(-3) - PosFloats.PosFiniteFloat(3.0f) shouldEqual -6.0f
      NegInts.NegInt(-3) - PosFloats.PosZFiniteFloat(3.0f) shouldEqual -6.0f
      NegInts.NegInt(-3) - NegFloats.NegFiniteFloat(-3.0f) shouldEqual 0.0f
      NegInts.NegInt(-3) - NegFloats.NegZFiniteFloat(-3.0f) shouldEqual 0.0f
      NegInts.NegInt(-3) - Finites.FiniteFloat(3.0f) shouldEqual -6.0f
      NegInts.NegInt(-3) - PosDoubles.PosDouble(3.0) shouldEqual -6.0
      NegInts.NegInt(-3) - NegDoubles.NegDouble(-3.0) shouldEqual 0.0
      NegInts.NegInt(-3) - NegDoubles.NegZDouble(-3.0) shouldEqual 0.0
      NegInts.NegInt(-3) - NonZeroDoubles.NonZeroDouble(3.0) shouldEqual -6.0
      NegInts.NegInt(-3) - NonZeroDoubles.NonZeroFiniteDouble(3.0) shouldEqual -6.0
      NegInts.NegInt(-3) - PosDoubles.PosFiniteDouble(3.0) shouldEqual -6.0
      NegInts.NegInt(-3) - PosDoubles.PosZFiniteDouble(3.0) shouldEqual -6.0
      NegInts.NegInt(-3) - NegDoubles.NegFiniteDouble(-3.0) shouldEqual 0.0
      NegInts.NegInt(-3) - NegDoubles.NegZFiniteDouble(-3.0) shouldEqual 0.0
      NegInts.NegInt(-3) - Finites.FiniteDouble(3.0) shouldEqual -6.0
    }
    it("when a compatible AnyVal is passed to a * method invoked on it should give the same AnyVal type back at compile time, and correct value at runtime") {
      NegInts.NegInt(-3) * 3.toByte shouldEqual -9
      NegInts.NegInt(-3) * 3.toShort shouldEqual -9
      NegInts.NegInt(-3) * '3' shouldEqual -153
      NegInts.NegInt(-3) * 3 shouldEqual -9
      NegInts.NegInt(-3) * 3L shouldEqual -9L
      NegInts.NegInt(-3) * 3.0f shouldEqual -9.0f
      NegInts.NegInt(-3) * 3.0 shouldEqual -9.0
      NegInts.NegInt(-3) * PosInts.PosInt(3) shouldEqual -9
      NegInts.NegInt(-3) * PosInts.PosZInt(3) shouldEqual -9
      NegInts.NegInt(-3) * NegInts.NegZInt(-3) shouldEqual 9
      NegInts.NegInt(-3) * NonZeroInts.NonZeroInt(3) shouldEqual -9
      NegInts.NegInt(-3) * Percentages.PercentageInt(50) shouldEqual -150
      NegInts.NegInt(-3) * PosLongs.PosLong(3L) shouldEqual -9L
      NegInts.NegInt(-3) * PosLongs.PosZLong(3L) shouldEqual -9L
      NegInts.NegInt(-3) * NegLongs.NegLong(-3L) shouldEqual 9L
      NegInts.NegInt(-3) * NegLongs.NegZLong(-3L) shouldEqual 9L
      NegInts.NegInt(-3) * NonZeroLongs.NonZeroLong(3L) shouldEqual -9L
      NegInts.NegInt(-3) * PosFloats.PosFloat(3.0f) shouldEqual -9.0f
      NegInts.NegInt(-3) * PosFloats.PosZFloat(3.0f) shouldEqual -9.0f
      NegInts.NegInt(-3) * NegFloats.NegFloat(-3.0f) shouldEqual 9.0f
      NegInts.NegInt(-3) * NegFloats.NegZFloat(-3.0f) shouldEqual 9.0f
      NegInts.NegInt(-3) * NonZeroFloats.NonZeroFloat(3.0f) shouldEqual -9.0f
      NegInts.NegInt(-3) * NonZeroFloats.NonZeroFiniteFloat(3.0f) shouldEqual -9.0f
      NegInts.NegInt(-3) * PosFloats.PosFiniteFloat(3.0f) shouldEqual -9.0f
      NegInts.NegInt(-3) * PosFloats.PosZFiniteFloat(3.0f) shouldEqual -9.0f
      NegInts.NegInt(-3) * NegFloats.NegFiniteFloat(-3.0f) shouldEqual 9.0f
      NegInts.NegInt(-3) * NegFloats.NegZFiniteFloat(-3.0f) shouldEqual 9.0f
      NegInts.NegInt(-3) * Finites.FiniteFloat(3.0f) shouldEqual -9.0f
      NegInts.NegInt(-3) * PosDoubles.PosDouble(3.0) shouldEqual -9.0
      NegInts.NegInt(-3) * NegDoubles.NegDouble(-3.0) shouldEqual 9.0
      NegInts.NegInt(-3) * NegDoubles.NegZDouble(-3.0) shouldEqual 9.0
      NegInts.NegInt(-3) * NonZeroDoubles.NonZeroDouble(3.0) shouldEqual -9.0
      NegInts.NegInt(-3) * NonZeroDoubles.NonZeroFiniteDouble(3.0) shouldEqual -9.0
      NegInts.NegInt(-3) * PosDoubles.PosFiniteDouble(3.0) shouldEqual -9.0
      NegInts.NegInt(-3) * PosDoubles.PosZFiniteDouble(3.0) shouldEqual -9.0
      NegInts.NegInt(-3) * NegDoubles.NegFiniteDouble(-3.0) shouldEqual 9.0
      NegInts.NegInt(-3) * NegDoubles.NegZFiniteDouble(-3.0) shouldEqual 9.0
      NegInts.NegInt(-3) * Finites.FiniteDouble(3.0) shouldEqual -9.0
    }
    it("when a compatible AnyVal is passed to a / method invoked on it should give the same AnyVal type back at compile time, and correct value at runtime") {
      NegInts.NegInt(-3) / 3.toByte shouldEqual -1
      NegInts.NegInt(-3) / 3.toShort shouldEqual -1
      NegInts.NegInt(-3) / '3' shouldEqual 0
      NegInts.NegInt(-3) / 3 shouldEqual -1
      NegInts.NegInt(-3) / 3L shouldEqual -1L
      NegInts.NegInt(-3) / 3.0f shouldEqual -1.0f
      NegInts.NegInt(-3) / 3.0 shouldEqual -1.0
      NegInts.NegInt(-3) / PosInts.PosInt(3) shouldEqual -1
      NegInts.NegInt(-3) / PosInts.PosZInt(3) shouldEqual -1
      NegInts.NegInt(-3) / NegInts.NegZInt(-3) shouldEqual 1
      NegInts.NegInt(-3) / NonZeroInts.NonZeroInt(3) shouldEqual -1
      NegInts.NegInt(-3) / Percentages.PercentageInt(50) shouldEqual 0
      NegInts.NegInt(-3) / PosLongs.PosLong(3L) shouldEqual -1L
      NegInts.NegInt(-3) / PosLongs.PosZLong(3L) shouldEqual -1L
      NegInts.NegInt(-3) / NegLongs.NegLong(-3L) shouldEqual 1L
      NegInts.NegInt(-3) / NegLongs.NegZLong(-3L) shouldEqual 1L
      NegInts.NegInt(-3) / NonZeroLongs.NonZeroLong(3L) shouldEqual -1L
      NegInts.NegInt(-3) / PosFloats.PosFloat(3.0f) shouldEqual -1.0f
      NegInts.NegInt(-3) / PosFloats.PosZFloat(3.0f) shouldEqual -1.0f
      NegInts.NegInt(-3) / NegFloats.NegFloat(-3.0f) shouldEqual 1.0f
      NegInts.NegInt(-3) / NegFloats.NegZFloat(-3.0f) shouldEqual 1.0f
      NegInts.NegInt(-3) / NonZeroFloats.NonZeroFloat(3.0f) shouldEqual -1.0f
      NegInts.NegInt(-3) / NonZeroFloats.NonZeroFiniteFloat(3.0f) shouldEqual -1.0f
      NegInts.NegInt(-3) / PosFloats.PosFiniteFloat(3.0f) shouldEqual -1.0f
      NegInts.NegInt(-3) / PosFloats.PosZFiniteFloat(3.0f) shouldEqual -1.0f
      NegInts.NegInt(-3) / NegFloats.NegFiniteFloat(-3.0f) shouldEqual 1.0f
      NegInts.NegInt(-3) / NegFloats.NegZFiniteFloat(-3.0f) shouldEqual 1.0f
      NegInts.NegInt(-3) / Finites.FiniteFloat(3.0f) shouldEqual -1.0f
      NegInts.NegInt(-3) / PosDoubles.PosDouble(3.0) shouldEqual -1.0
      NegInts.NegInt(-3) / NegDoubles.NegDouble(-3.0) shouldEqual 1.0
      NegInts.NegInt(-3) / NegDoubles.NegZDouble(-3.0) shouldEqual 1.0
      NegInts.NegInt(-3) / NonZeroDoubles.NonZeroDouble(3.0) shouldEqual -1.0
      NegInts.NegInt(-3) / NonZeroDoubles.NonZeroFiniteDouble(3.0) shouldEqual -1.0
      NegInts.NegInt(-3) / PosDoubles.PosFiniteDouble(3.0) shouldEqual -1.0
      NegInts.NegInt(-3) / PosDoubles.PosZFiniteDouble(3.0) shouldEqual -1.0
      NegInts.NegInt(-3) / NegDoubles.NegFiniteDouble(-3.0) shouldEqual 1.0
      NegInts.NegInt(-3) / NegDoubles.NegZFiniteDouble(-3.0) shouldEqual 1.0
      NegInts.NegInt(-3) / Finites.FiniteDouble(3.0) shouldEqual -1.0
    }
    it("when a compatible AnyVal is passed to a % method invoked on it should give the same AnyVal type back at compile time, and correct value at runtime") {
      NegInts.NegInt(-3) % 3.toByte shouldEqual 0
      NegInts.NegInt(-3) % 3.toShort shouldEqual 0
      NegInts.NegInt(-3) % '3' shouldEqual -3
      NegInts.NegInt(-3) % 3 shouldEqual 0
      NegInts.NegInt(-3) % 3L shouldEqual 0L
      NegInts.NegInt(-3) % 3.0f shouldEqual -0.0f
      NegInts.NegInt(-3) % 3.0 shouldEqual -0.0
      NegInts.NegInt(-3) % PosInts.PosInt(3) shouldEqual 0
      NegInts.NegInt(-3) % PosInts.PosZInt(3) shouldEqual 0
      NegInts.NegInt(-3) % NegInts.NegZInt(-3) shouldEqual 0
      NegInts.NegInt(-3) % NonZeroInts.NonZeroInt(3) shouldEqual 0
      NegInts.NegInt(-3) % Percentages.PercentageInt(50) shouldEqual -3
      NegInts.NegInt(-3) % PosLongs.PosLong(3L) shouldEqual 0L
      NegInts.NegInt(-3) % PosLongs.PosZLong(3L) shouldEqual 0L
      NegInts.NegInt(-3) % NegLongs.NegLong(-3L) shouldEqual 0L
      NegInts.NegInt(-3) % NegLongs.NegZLong(-3L) shouldEqual 0L
      NegInts.NegInt(-3) % NonZeroLongs.NonZeroLong(3L) shouldEqual 0L
      NegInts.NegInt(-3) % PosFloats.PosFloat(3.0f) shouldEqual -0.0f
      NegInts.NegInt(-3) % PosFloats.PosZFloat(3.0f) shouldEqual -0.0f
      NegInts.NegInt(-3) % NegFloats.NegFloat(-3.0f) shouldEqual -0.0f
      NegInts.NegInt(-3) % NegFloats.NegZFloat(-3.0f) shouldEqual -0.0f
      NegInts.NegInt(-3) % NonZeroFloats.NonZeroFloat(3.0f) shouldEqual -0.0f
      NegInts.NegInt(-3) % NonZeroFloats.NonZeroFiniteFloat(3.0f) shouldEqual -0.0f
      NegInts.NegInt(-3) % PosFloats.PosFiniteFloat(3.0f) shouldEqual -0.0f
      NegInts.NegInt(-3) % PosFloats.PosZFiniteFloat(3.0f) shouldEqual -0.0f
      NegInts.NegInt(-3) % NegFloats.NegFiniteFloat(-3.0f) shouldEqual -0.0f
      NegInts.NegInt(-3) % NegFloats.NegZFiniteFloat(-3.0f) shouldEqual -0.0f
      NegInts.NegInt(-3) % Finites.FiniteFloat(3.0f) shouldEqual -0.0f
      NegInts.NegInt(-3) % PosDoubles.PosDouble(3.0) shouldEqual -0.0
      NegInts.NegInt(-3) % NegDoubles.NegDouble(-3.0) shouldEqual -0.0
      NegInts.NegInt(-3) % NegDoubles.NegZDouble(-3.0) shouldEqual -0.0
      NegInts.NegInt(-3) % NonZeroDoubles.NonZeroDouble(3.0) shouldEqual -0.0
      NegInts.NegInt(-3) % NonZeroDoubles.NonZeroFiniteDouble(3.0) shouldEqual -0.0
      NegInts.NegInt(-3) % PosDoubles.PosFiniteDouble(3.0) shouldEqual -0.0
      NegInts.NegInt(-3) % PosDoubles.PosZFiniteDouble(3.0) shouldEqual -0.0
      NegInts.NegInt(-3) % NegDoubles.NegFiniteDouble(-3.0) shouldEqual -0.0
      NegInts.NegInt(-3) % NegDoubles.NegZFiniteDouble(-3.0) shouldEqual -0.0
      NegInts.NegInt(-3) % Finites.FiniteDouble(3.0) shouldEqual -0.0
    }
    it("should offer '<' comparison that is consistent with Int") {
      (NegInts.NegInt(-3) < 3.toByte) shouldEqual true
      (NegInts.NegInt(-3) < 3.toShort) shouldEqual true
      (NegInts.NegInt(-3) < '3') shouldEqual true
      (NegInts.NegInt(-3) < 3) shouldEqual true
      (NegInts.NegInt(-3) < 3L) shouldEqual true
      (NegInts.NegInt(-3) < 3.0f) shouldEqual true
      (NegInts.NegInt(-3) < 3.0) shouldEqual true
      (NegInts.NegInt(-3) < PosInts.PosInt(3)) shouldEqual true
      (NegInts.NegInt(-3) < PosInts.PosZInt(3)) shouldEqual true
      (NegInts.NegInt(-3) < NegInts.NegZInt(-3)) shouldEqual false
      (NegInts.NegInt(-3) < NonZeroInts.NonZeroInt(3)) shouldEqual true
      (NegInts.NegInt(-3) < Percentages.PercentageInt(50)) shouldEqual true
      (NegInts.NegInt(-3) < PosLongs.PosLong(3L)) shouldEqual true
      (NegInts.NegInt(-3) < PosLongs.PosZLong(3L)) shouldEqual true
      (NegInts.NegInt(-3) < NegLongs.NegLong(-3L)) shouldEqual false
      (NegInts.NegInt(-3) < NegLongs.NegZLong(-3L)) shouldEqual false
      (NegInts.NegInt(-3) < NonZeroLongs.NonZeroLong(3L)) shouldEqual true
      (NegInts.NegInt(-3) < PosFloats.PosFloat(3.0f)) shouldEqual true
      (NegInts.NegInt(-3) < PosFloats.PosZFloat(3.0f)) shouldEqual true
      (NegInts.NegInt(-3) < NegFloats.NegFloat(-3.0f)) shouldEqual false
      (NegInts.NegInt(-3) < NegFloats.NegZFloat(-3.0f)) shouldEqual false
      (NegInts.NegInt(-3) < NonZeroFloats.NonZeroFloat(3.0f)) shouldEqual true
      (NegInts.NegInt(-3) < NonZeroFloats.NonZeroFiniteFloat(3.0f)) shouldEqual true
      (NegInts.NegInt(-3) < PosFloats.PosFiniteFloat(3.0f)) shouldEqual true
      (NegInts.NegInt(-3) < PosFloats.PosZFiniteFloat(3.0f)) shouldEqual true
      (NegInts.NegInt(-3) < NegFloats.NegFiniteFloat(-3.0f)) shouldEqual false
      (NegInts.NegInt(-3) < NegFloats.NegZFiniteFloat(-3.0f)) shouldEqual false
      (NegInts.NegInt(-3) < Finites.FiniteFloat(3.0f)) shouldEqual true
      (NegInts.NegInt(-3) < PosDoubles.PosDouble(3.0)) shouldEqual true
      (NegInts.NegInt(-3) < NegDoubles.NegDouble(-3.0)) shouldEqual false
      (NegInts.NegInt(-3) < NegDoubles.NegZDouble(-3.0)) shouldEqual false
      (NegInts.NegInt(-3) < NonZeroDoubles.NonZeroDouble(3.0)) shouldEqual true
      (NegInts.NegInt(-3) < NonZeroDoubles.NonZeroFiniteDouble(3.0)) shouldEqual true
      (NegInts.NegInt(-3) < PosDoubles.PosFiniteDouble(3.0)) shouldEqual true
      (NegInts.NegInt(-3) < PosDoubles.PosZFiniteDouble(3.0)) shouldEqual true
      (NegInts.NegInt(-3) < NegDoubles.NegFiniteDouble(-3.0)) shouldEqual false
      (NegInts.NegInt(-3) < NegDoubles.NegZFiniteDouble(-3.0)) shouldEqual false
      (NegInts.NegInt(-3) < Finites.FiniteDouble(3.0)) shouldEqual true
    }
    it("should offer '<=' comparison that is consistent with Int") {
      (NegInts.NegInt(-3) <= 3.toByte) shouldEqual true
      (NegInts.NegInt(-3) <= 3.toShort) shouldEqual true
      (NegInts.NegInt(-3) <= '3') shouldEqual true
      (NegInts.NegInt(-3) <= 3) shouldEqual true
      (NegInts.NegInt(-3) <= 3L) shouldEqual true
      (NegInts.NegInt(-3) <= 3.0f) shouldEqual true
      (NegInts.NegInt(-3) <= 3.0) shouldEqual true
      (NegInts.NegInt(-3) <= PosInts.PosInt(3)) shouldEqual true
      (NegInts.NegInt(-3) <= PosInts.PosZInt(3)) shouldEqual true
      (NegInts.NegInt(-3) <= NegInts.NegZInt(-3)) shouldEqual true
      (NegInts.NegInt(-3) <= NonZeroInts.NonZeroInt(3)) shouldEqual true
      (NegInts.NegInt(-3) <= Percentages.PercentageInt(50)) shouldEqual true
      (NegInts.NegInt(-3) <= PosLongs.PosLong(3L)) shouldEqual true
      (NegInts.NegInt(-3) <= PosLongs.PosZLong(3L)) shouldEqual true
      (NegInts.NegInt(-3) <= NegLongs.NegLong(-3L)) shouldEqual true
      (NegInts.NegInt(-3) <= NegLongs.NegZLong(-3L)) shouldEqual true
      (NegInts.NegInt(-3) <= NonZeroLongs.NonZeroLong(3L)) shouldEqual true
      (NegInts.NegInt(-3) <= PosFloats.PosFloat(3.0f)) shouldEqual true
      (NegInts.NegInt(-3) <= PosFloats.PosZFloat(3.0f)) shouldEqual true
      (NegInts.NegInt(-3) <= NegFloats.NegFloat(-3.0f)) shouldEqual true
      (NegInts.NegInt(-3) <= NegFloats.NegZFloat(-3.0f)) shouldEqual true
      (NegInts.NegInt(-3) <= NonZeroFloats.NonZeroFloat(3.0f)) shouldEqual true
      (NegInts.NegInt(-3) <= NonZeroFloats.NonZeroFiniteFloat(3.0f)) shouldEqual true
      (NegInts.NegInt(-3) <= PosFloats.PosFiniteFloat(3.0f)) shouldEqual true
      (NegInts.NegInt(-3) <= PosFloats.PosZFiniteFloat(3.0f)) shouldEqual true
      (NegInts.NegInt(-3) <= NegFloats.NegFiniteFloat(-3.0f)) shouldEqual true
      (NegInts.NegInt(-3) <= NegFloats.NegZFiniteFloat(-3.0f)) shouldEqual true
      (NegInts.NegInt(-3) <= Finites.FiniteFloat(3.0f)) shouldEqual true
      (NegInts.NegInt(-3) <= PosDoubles.PosDouble(3.0)) shouldEqual true
      (NegInts.NegInt(-3) <= NegDoubles.NegDouble(-3.0)) shouldEqual true
      (NegInts.NegInt(-3) <= NegDoubles.NegZDouble(-3.0)) shouldEqual true
      (NegInts.NegInt(-3) <= NonZeroDoubles.NonZeroDouble(3.0)) shouldEqual true
      (NegInts.NegInt(-3) <= NonZeroDoubles.NonZeroFiniteDouble(3.0)) shouldEqual true
      (NegInts.NegInt(-3) <= PosDoubles.PosFiniteDouble(3.0)) shouldEqual true
      (NegInts.NegInt(-3) <= PosDoubles.PosZFiniteDouble(3.0)) shouldEqual true
      (NegInts.NegInt(-3) <= NegDoubles.NegFiniteDouble(-3.0)) shouldEqual true
      (NegInts.NegInt(-3) <= NegDoubles.NegZFiniteDouble(-3.0)) shouldEqual true
      (NegInts.NegInt(-3) <= Finites.FiniteDouble(3.0)) shouldEqual true
    }
    it("should offer '>' comparison that is consistent with Int") {
      (NegInts.NegInt(-3) > 3.toByte) shouldEqual false
      (NegInts.NegInt(-3) > 3.toShort) shouldEqual false
      (NegInts.NegInt(-3) > '3') shouldEqual false
      (NegInts.NegInt(-3) > 3) shouldEqual false
      (NegInts.NegInt(-3) > 3L) shouldEqual false
      (NegInts.NegInt(-3) > 3.0f) shouldEqual false
      (NegInts.NegInt(-3) > 3.0) shouldEqual false
      (NegInts.NegInt(-3) > PosInts.PosInt(3)) shouldEqual false
      (NegInts.NegInt(-3) > PosInts.PosZInt(3)) shouldEqual false
      (NegInts.NegInt(-3) > NegInts.NegZInt(-3)) shouldEqual false
      (NegInts.NegInt(-3) > NonZeroInts.NonZeroInt(3)) shouldEqual false
      (NegInts.NegInt(-3) > Percentages.PercentageInt(50)) shouldEqual false
      (NegInts.NegInt(-3) > PosLongs.PosLong(3L)) shouldEqual false
      (NegInts.NegInt(-3) > PosLongs.PosZLong(3L)) shouldEqual false
      (NegInts.NegInt(-3) > NegLongs.NegLong(-3L)) shouldEqual false
      (NegInts.NegInt(-3) > NegLongs.NegZLong(-3L)) shouldEqual false
      (NegInts.NegInt(-3) > NonZeroLongs.NonZeroLong(3L)) shouldEqual false
      (NegInts.NegInt(-3) > PosFloats.PosFloat(3.0f)) shouldEqual false
      (NegInts.NegInt(-3) > PosFloats.PosZFloat(3.0f)) shouldEqual false
      (NegInts.NegInt(-3) > NegFloats.NegFloat(-3.0f)) shouldEqual false
      (NegInts.NegInt(-3) > NegFloats.NegZFloat(-3.0f)) shouldEqual false
      (NegInts.NegInt(-3) > NonZeroFloats.NonZeroFloat(3.0f)) shouldEqual false
      (NegInts.NegInt(-3) > NonZeroFloats.NonZeroFiniteFloat(3.0f)) shouldEqual false
      (NegInts.NegInt(-3) > PosFloats.PosFiniteFloat(3.0f)) shouldEqual false
      (NegInts.NegInt(-3) > PosFloats.PosZFiniteFloat(3.0f)) shouldEqual false
      (NegInts.NegInt(-3) > NegFloats.NegFiniteFloat(-3.0f)) shouldEqual false
      (NegInts.NegInt(-3) > NegFloats.NegZFiniteFloat(-3.0f)) shouldEqual false
      (NegInts.NegInt(-3) > Finites.FiniteFloat(3.0f)) shouldEqual false
      (NegInts.NegInt(-3) > PosDoubles.PosDouble(3.0)) shouldEqual false
      (NegInts.NegInt(-3) > NegDoubles.NegDouble(-3.0)) shouldEqual false
      (NegInts.NegInt(-3) > NegDoubles.NegZDouble(-3.0)) shouldEqual false
      (NegInts.NegInt(-3) > NonZeroDoubles.NonZeroDouble(3.0)) shouldEqual false
      (NegInts.NegInt(-3) > NonZeroDoubles.NonZeroFiniteDouble(3.0)) shouldEqual false
      (NegInts.NegInt(-3) > PosDoubles.PosFiniteDouble(3.0)) shouldEqual false
      (NegInts.NegInt(-3) > PosDoubles.PosZFiniteDouble(3.0)) shouldEqual false
      (NegInts.NegInt(-3) > NegDoubles.NegFiniteDouble(-3.0)) shouldEqual false
      (NegInts.NegInt(-3) > NegDoubles.NegZFiniteDouble(-3.0)) shouldEqual false
      (NegInts.NegInt(-3) > Finites.FiniteDouble(3.0)) shouldEqual false
    }
    it("should offer '>=' comparison that is consistent with Int") {
      (NegInts.NegInt(-3) >= 3.toByte) shouldEqual false
      (NegInts.NegInt(-3) >= 3.toShort) shouldEqual false
      (NegInts.NegInt(-3) >= '3') shouldEqual false
      (NegInts.NegInt(-3) >= 3) shouldEqual false
      (NegInts.NegInt(-3) >= 3L) shouldEqual false
      (NegInts.NegInt(-3) >= 3.0f) shouldEqual false
      (NegInts.NegInt(-3) >= 3.0) shouldEqual false
      (NegInts.NegInt(-3) >= PosInts.PosInt(3)) shouldEqual false
      (NegInts.NegInt(-3) >= PosInts.PosZInt(3)) shouldEqual false
      (NegInts.NegInt(-3) >= NegInts.NegZInt(-3)) shouldEqual true
      (NegInts.NegInt(-3) >= NonZeroInts.NonZeroInt(3)) shouldEqual false
      (NegInts.NegInt(-3) >= Percentages.PercentageInt(50)) shouldEqual false
      (NegInts.NegInt(-3) >= PosLongs.PosLong(3L)) shouldEqual false
      (NegInts.NegInt(-3) >= PosLongs.PosZLong(3L)) shouldEqual false
      (NegInts.NegInt(-3) >= NegLongs.NegLong(-3L)) shouldEqual true
      (NegInts.NegInt(-3) >= NegLongs.NegZLong(-3L)) shouldEqual true
      (NegInts.NegInt(-3) >= NonZeroLongs.NonZeroLong(3L)) shouldEqual false
      (NegInts.NegInt(-3) >= PosFloats.PosFloat(3.0f)) shouldEqual false
      (NegInts.NegInt(-3) >= PosFloats.PosZFloat(3.0f)) shouldEqual false
      (NegInts.NegInt(-3) >= NegFloats.NegFloat(-3.0f)) shouldEqual true
      (NegInts.NegInt(-3) >= NegFloats.NegZFloat(-3.0f)) shouldEqual true
      (NegInts.NegInt(-3) >= NonZeroFloats.NonZeroFloat(3.0f)) shouldEqual false
      (NegInts.NegInt(-3) >= NonZeroFloats.NonZeroFiniteFloat(3.0f)) shouldEqual false
      (NegInts.NegInt(-3) >= PosFloats.PosFiniteFloat(3.0f)) shouldEqual false
      (NegInts.NegInt(-3) >= PosFloats.PosZFiniteFloat(3.0f)) shouldEqual false
      (NegInts.NegInt(-3) >= NegFloats.NegFiniteFloat(-3.0f)) shouldEqual true
      (NegInts.NegInt(-3) >= NegFloats.NegZFiniteFloat(-3.0f)) shouldEqual true
      (NegInts.NegInt(-3) >= Finites.FiniteFloat(3.0f)) shouldEqual false
      (NegInts.NegInt(-3) >= PosDoubles.PosDouble(3.0)) shouldEqual false
      (NegInts.NegInt(-3) >= NegDoubles.NegDouble(-3.0)) shouldEqual true
      (NegInts.NegInt(-3) >= NegDoubles.NegZDouble(-3.0)) shouldEqual true
      (NegInts.NegInt(-3) >= NonZeroDoubles.NonZeroDouble(3.0)) shouldEqual false
      (NegInts.NegInt(-3) >= NonZeroDoubles.NonZeroFiniteDouble(3.0)) shouldEqual false
      (NegInts.NegInt(-3) >= PosDoubles.PosFiniteDouble(3.0)) shouldEqual false
      (NegInts.NegInt(-3) >= PosDoubles.PosZFiniteDouble(3.0)) shouldEqual false
      (NegInts.NegInt(-3) >= NegDoubles.NegFiniteDouble(-3.0)) shouldEqual true
      (NegInts.NegInt(-3) >= NegDoubles.NegZFiniteDouble(-3.0)) shouldEqual true
      (NegInts.NegInt(-3) >= Finites.FiniteDouble(3.0)) shouldEqual false
    }
    it("should offer 'min' and 'max' methods that are consistent with Int") {
      NegInts.NegInt(-3).max(NegInts.NegInt(-3)) shouldEqual NegInts.NegInt(-3)
      NegInts.NegInt(-3).min(NegInts.NegInt(-3)) shouldEqual NegInts.NegInt(-3)
    }
    it("should offer widening methods for basic type Int that are consistent with Int") {
      def widen(value: Int): Int = value
      widen(NegInts.NegInt(-3)) shouldEqual -3
    }
    it("should have a toString consistent with Int") {
      (NegInts.NegInt(-3) : Int).toString shouldBe -3.toString
    }
    it("should return the same type from its unary_+ method") {
      +NegInts.NegInt(-3) shouldEqual NegInts.NegInt(-3)
    }
    it("should offer a unary - method that is consistent with Int") {
      (-NegInts.NegInt(-3)) shouldEqual 3
    }
  }
}
