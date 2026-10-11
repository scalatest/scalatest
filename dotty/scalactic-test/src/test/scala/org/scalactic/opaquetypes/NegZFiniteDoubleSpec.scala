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

import org.scalatest.*
import org.scalatest.prop.PropertyChecks
// SKIP-SCALATESTJS,NATIVE-START
import scala.collection.immutable.NumericRange
// SKIP-SCALATESTJS,NATIVE-END
import OptionValues.*
import scala.collection.mutable.WrappedArray
//import org.scalactic.StrictCheckedEquality
import Double.NaN
import org.scalactic.Equality
import org.scalactic.{Pass, Fail}
import org.scalactic.{Good, Bad}
import scala.util.{Try, Success, Failure}

trait NegZFiniteDoubleSpecSupport {

  implicit val doubleEquality: Equality[Double] =
    new Equality[Double] {
      override def areEqual(a: Double, b: Any): Boolean =
        (a, b) match {
          case (a, bDouble: Double) if a.isNaN && bDouble.isNaN  => true
          case _ => a == b
        }
    }

  implicit val floatEquality: Equality[Float] =
    new Equality[Float] {
      override def areEqual(a: Float, b: Any): Boolean =
        (a, b) match {
          case (a, bFloat: Float) if a.isNaN && bFloat.isNaN => true
          case _ => a == b
        }
    }

  implicit def tryEquality[T]: Equality[Try[T]] = new Equality[Try[T]] {
    override def areEqual(a: Try[T], b: Any): Boolean = a match {
      case Success(double: Double) if double.isNaN =>  // This is because in scala.js x/0 results to NaN not ArithmetricException like in jvm, and we need to make sure Success(NaN) == Success(NaN) is true to pass the test.
        b match {
          case Success(bDouble: Double) if bDouble.isNaN => true
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

class NegZFiniteDoubleSpec extends funspec.AnyFunSpec with matchers.should.Matchers with PropertyChecks with NegZFiniteDoubleSpecSupport {

  describe("A NegZFiniteDouble") {
    describe("should offer a from factory method that") {
      it("returns Some[NegZFiniteDouble] if the passed Double is lesser than or equal to 0") {
        NegZFiniteDouble.from(0.0).value.value shouldBe 0.0
        NegZFiniteDouble.from(-50.23).value.value shouldBe -50.23
        NegZFiniteDouble.from(-100.0).value.value shouldBe -100.0
      }
      it("returns None if the passed Double is greater than 0") {
        NegZFiniteDouble.from(0.00001) shouldBe None
        NegZFiniteDouble.from(99.9) shouldBe None
      }
    }
    describe("should offer an ensuringValid factory method that") {
      it("returns NegZFiniteDouble if the passed Double is lesser than or equal to 0") {
        NegZFiniteDouble.ensuringValid(0.0).value shouldBe 0.0
        NegZFiniteDouble.ensuringValid(-50.23).value shouldBe -50.23
        NegZFiniteDouble.ensuringValid(-100.0).value shouldBe -100.0
      }
      it("throws AssertionError if the passed Double is greater than 0") {
        an [AssertionError] should be thrownBy NegZFiniteDouble.ensuringValid(0.00001)
        an [AssertionError] should be thrownBy NegZFiniteDouble.ensuringValid(99.9)
        an [AssertionError] should be thrownBy NegZFiniteDouble.ensuringValid(Double.PositiveInfinity)
        an [AssertionError] should be thrownBy NegZFiniteDouble.ensuringValid(Double.NegativeInfinity)
      }
    }
    describe("should offer a tryingValid factory method that") {
      import TryValues.*
      it("returns a NegZFiniteDouble wrapped in a Success if the passed Double is lesser than or equal 0") {
        NegZFiniteDouble.tryingValid(0.0).success.value.value shouldBe 0.0
        NegZFiniteDouble.tryingValid(-50.0).success.value.value shouldBe -50.0
        NegZFiniteDouble.tryingValid(-100.0f).success.value.value shouldBe -100.0
      }

      it("returns an AssertionError wrapped in a Failure if the passed Double is greater than 0") {
        NegZFiniteDouble.tryingValid(1.0).failure.exception shouldBe an [AssertionError]
        NegZFiniteDouble.tryingValid(99.0).failure.exception shouldBe an [AssertionError]
      }
    }
    describe("should offer a passOrElse factory method that") {
      it("returns a Pass if the given Double is lesser than or equal 0") {
        NegZFiniteDouble.passOrElse(0.0)(i => i) shouldBe Pass
        NegZFiniteDouble.passOrElse(-50.0)(i => i) shouldBe Pass
        NegZFiniteDouble.passOrElse(-100.0)(i => i) shouldBe Pass
      }
      it("returns an error value produced by passing the given Double to the given function if the passed Double is greater than 0, wrapped in a Fail") {
        NegZFiniteDouble.passOrElse(1.0)(i => i) shouldBe Fail(1.0)
        NegZFiniteDouble.passOrElse(99.0)(i => i + 3.0) shouldBe Fail(102.0)
      }
    }
    describe("should offer a goodOrElse factory method that") {
      it("returns a NegZFiniteDouble wrapped in a Good if the given Double is lesser than or equal 0") {
        NegZFiniteDouble.goodOrElse(0.0)(i => i) shouldBe Good(NegZFiniteDouble(0.0))
        NegZFiniteDouble.goodOrElse(-50.0)(i => i) shouldBe Good(NegZFiniteDouble(-50.0))
        NegZFiniteDouble.goodOrElse(-100.0)(i => i) shouldBe Good(NegZFiniteDouble(-100.0))
      }
      it("returns an error value produced by passing the given Double to the given function if the passed Double is greater than 0, wrapped in a Bad") {
        NegZFiniteDouble.goodOrElse(1.0)(i => i) shouldBe Bad(1.0)
        NegZFiniteDouble.goodOrElse(99.0)(i => i + 3.0f) shouldBe Bad(102.0)
      }
    }
    describe("should offer a rightOrElse factory method that") {
      it("returns a NegZFiniteDouble wrapped in a Right if the given Double is lesser than or equal 0") {
        NegZFiniteDouble.rightOrElse(0.0)(i => i) shouldBe Right(NegZFiniteDouble(0.0))
        NegZFiniteDouble.rightOrElse(-50.0)(i => i) shouldBe Right(NegZFiniteDouble(-50.0))
        NegZFiniteDouble.rightOrElse(-100.0)(i => i) shouldBe Right(NegZFiniteDouble(-100.0))
      }
      it("returns an error value produced by passing the given Double to the given function if the passed Double is greater than 0, wrapped in a Left") {
        NegZFiniteDouble.rightOrElse(1.0)(i => i) shouldBe Left(1.0)
        NegZFiniteDouble.rightOrElse(99.0)(i => i + 3.0f) shouldBe Left(102.0)
      }
    }
    describe("should offer an isValid predicate method that") {
      it("returns true if the passed Double is lesser than or equal to 0") {
        NegZFiniteDouble.isValid(-50.23) shouldBe true
        NegZFiniteDouble.isValid(-100.0) shouldBe true
        NegZFiniteDouble.isValid(0.0) shouldBe true
        NegZFiniteDouble.isValid(-0.0) shouldBe true
        NegZFiniteDouble.isValid(0.00001) shouldBe false
        NegZFiniteDouble.isValid(99.9) shouldBe false
      }
    }
    describe("should offer a fromOrElse factory method that") {
      it("returns a NegZFiniteDouble if the passed Double is lesser than or equal to 0") {
        NegZFiniteDouble.fromOrElse(-50.23, NegZFiniteDouble(-42.0)).value shouldBe -50.23
        NegZFiniteDouble.fromOrElse(-100.0, NegZFiniteDouble(-42.0)).value shouldBe -100.0
        NegZFiniteDouble.fromOrElse(0.0, NegZFiniteDouble(-42.0)).value shouldBe 0.0
      }
      it("returns a given default if the passed Double is greater than or equal to 0") {
        NegZFiniteDouble.fromOrElse(0.00001, NegZFiniteDouble(-42.0)).value shouldBe -42.0
        NegZFiniteDouble.fromOrElse(99.9, NegZFiniteDouble(-42.0)).value shouldBe -42.0
      }
    }
    it("should offer MaxValue and MinValue factory methods") {
      NegZFiniteDouble.MaxValue shouldEqual NegZFiniteDouble(0.0)
      NegZFiniteDouble.MinValue shouldEqual NegZFiniteDouble.from(Double.MinValue).get
    }
    it("should not offer a NegativeInfinity factory method") {
      "NegZFiniteDouble.NegativeInfinity" shouldNot compile
    }
    it("should not offer a PositiveInfinity factory method") {
      "NegZFiniteDouble.PositiveInfinity" shouldNot compile
    }
/* This one now compiles, because of the newly added implicit NegZFiniteDouble => NegZDouble
    NegZFiniteDouble(-1.0).isNegInfinity
*/
    it("should not offer a isPosInfinity method") {
      "NegZFiniteDouble(-1.0f).isPosInfinity" shouldNot compile
    }

    it("should be sortable") {
      val xs = List(NegZFiniteDouble(-2.2), NegZFiniteDouble(-0.0), NegZFiniteDouble(-1.1),
        NegZFiniteDouble(-3.3))
      xs.sorted shouldEqual List(NegZFiniteDouble(-3.3), NegZFiniteDouble(-2.2),
        NegZFiniteDouble(-1.1), NegZFiniteDouble(0.0))
    }

    describe("when created with apply method") {

      it("should compile when -8 is passed in") {
        "NegZFiniteDouble(-8)" should compile
        NegZFiniteDouble(-8).value shouldEqual -8.0
        "NegZFiniteDouble(-8L)" shouldNot compile
        "NegZFiniteDouble(-8.0F)" should compile
        NegZFiniteDouble(-8.0F).value shouldEqual -8.0
        "NegZFiniteDouble(-8.0)" should compile
        NegZFiniteDouble(-8.0).value shouldEqual -8.0
      }

      it("should compile when 0 is passed in") {
        "NegZFiniteDouble(0)" should compile
        NegZFiniteDouble(0).value shouldEqual 0.0
        "NegZFiniteDouble(0L)" shouldNot compile
        "NegZFiniteDouble(0.0F)" should compile
        NegZFiniteDouble(0.0F).value shouldEqual 0.0
        "NegZFiniteDouble(0.0)" should compile
        NegZFiniteDouble(0.0).value shouldEqual 0.0
      }

      it("should not compile when 8 is passed in") {
        "NegZFiniteDouble(8)" shouldNot compile
        "NegZFiniteDouble(8L)" shouldNot compile
        "NegZFiniteDouble(8.0F)" shouldNot compile
        "NegZFiniteDouble(8.0)" shouldNot compile
      }
      it("should not compile when x is passed in") {
        val a: Int = -8
        "NegZFiniteDouble(a)" shouldNot compile
        val b: Long = -8L
        "NegZFiniteDouble(b)" shouldNot compile
        val c: Float = -8.0F
        "NegZFiniteDouble(c)" shouldNot compile
        val d: Double = -8.0
        "NegZFiniteDouble(d)" shouldNot compile
      }
    }
    describe("when specified as a plain-old Double") {

      def takesNegZFiniteDouble(poz: NegZFiniteDouble): Double = poz.value

      it("should compile when -8 is passed in") {
        "takesNegZFiniteDouble(-8)" should compile
        takesNegZFiniteDouble(-8) shouldEqual -8.0
        "takesNegZFiniteDouble(-8L)" shouldNot compile
        "takesNegZFiniteDouble(-8.0F)" should compile
        takesNegZFiniteDouble(-8.0F) shouldEqual -8.0
        "takesNegZFiniteDouble(-8.0)" should compile
        takesNegZFiniteDouble(-8.0) shouldEqual -8.0
      }

      it("should compile when 0 is passed in") {
        "takesNegZFiniteDouble(0)" should compile
        takesNegZFiniteDouble(0) shouldEqual 0.0
        "takesNegZFiniteDouble(0L)" shouldNot compile
        "takesNegZFiniteDouble(0.0F)" should compile
        takesNegZFiniteDouble(0.0F) shouldEqual 0.0
        "takesNegZFiniteDouble(0.0)" should compile
        takesNegZFiniteDouble(0.0) shouldEqual 0.0
      }

      it("should not compile when 8 is passed in") {
        "takesNegZFiniteDouble(8)" shouldNot compile
        "takesNegZFiniteDouble(8L)" shouldNot compile
        "takesNegZFiniteDouble(8.0F)" shouldNot compile
        "takesNegZFiniteDouble(8.0)" shouldNot compile
      }

      it("should not compile when x is passed in") {
        val x: Int = -8
        "takesNegZFiniteDouble(x)" shouldNot compile
        val b: Long = -8L
        "takesNegZFiniteDouble(b)" shouldNot compile
        val c: Float = -8.0F
        "takesNegZFiniteDouble(c)" shouldNot compile
        val d: Double = -8.0
        "takesNegZFiniteDouble(d)" shouldNot compile
      }

      it("should offer a unary + method that is consistent with Double") {
        forAll { (p: NegZFiniteDouble) =>
          (+p).toDouble shouldEqual (+(p.toDouble))
        }
      }

      it("should offer a unary - method that returns PosZFiniteDouble") {
        forAll { (p: NegZFiniteDouble) =>
          (-p) shouldEqual (PosZFiniteDouble.ensuringValid(-(p.toDouble)))
        }
      }
    }

    it("should offer 'min' and 'max' methods that are consistent with Double") {
      forAll { (pzdouble1: NegZFiniteDouble, pzdouble2: NegZFiniteDouble) =>
        pzdouble1.max(pzdouble2).toDouble shouldEqual pzdouble1.toDouble.max(pzdouble2.toDouble)
        pzdouble1.min(pzdouble2).toDouble shouldEqual pzdouble1.toDouble.min(pzdouble2.toDouble)
      }
    }

    it("should offer an 'isWhole' method that is consistent with Double") {
      forAll { (pzdouble: NegZFiniteDouble) =>
        pzdouble.isWhole shouldEqual pzdouble.toDouble.isWhole
      }
    }

    it("should offer 'round', 'ceil', and 'floor' methods that are consistent with Double") {
      forAll { (pzdouble: NegZFiniteDouble) =>
        pzdouble.round.toDouble shouldEqual pzdouble.toDouble.round
        pzdouble.ceil.toDouble shouldEqual pzdouble.toDouble.ceil
        pzdouble.floor.toDouble shouldEqual pzdouble.toDouble.floor
      }
    }

    it("should offer 'toRadians' and 'toDegrees' methods that are consistent with Double") {
      forAll { (pzdouble: NegZFiniteDouble) =>
        pzdouble.toRadians shouldEqual pzdouble.toDouble.toRadians
      }
    }

    it("should offer widening methods for basic types that are consistent with Double") {
      forAll { (pzdouble: NegZFiniteDouble) =>
        def widen(value: Double): Double = value
        widen(pzdouble) shouldEqual widen(pzdouble.toDouble)
      }
    }
    it("should offer an ensuringValid method that takes a Double => Double, throwing AssertionError if the result is invalid") {
      NegZFiniteDouble(-33.0).ensuringValid(_ + 1.0) shouldEqual NegZFiniteDouble(-32.0)
      an [AssertionError] should be thrownBy { NegZFiniteDouble.MaxValue.ensuringValid(_ - NegZFiniteDouble.MaxValue + 1) }
      an [AssertionError] should be thrownBy { NegZFiniteDouble.MaxValue.ensuringValid(_ => Double.PositiveInfinity) }
      an [AssertionError] should be thrownBy { NegZFiniteDouble.MaxValue.ensuringValid(_ => Double.NegativeInfinity) }
    }
  }
  describe("NegZFiniteDouble") {
    it("should be automatically widened to compatible AnyVal targets") {
      (NegDoubles.NegZFiniteDouble(-3.0) : Double) shouldEqual -3.0
      (NegDoubles.NegZFiniteDouble(-3.0) : NegDoubles.NegZDouble) shouldEqual NegDoubles.NegZDouble(-3.0)
      (NegDoubles.NegZFiniteDouble(-3.0) : NonZeroDoubles.NonZeroDouble) shouldEqual NonZeroDoubles.NonZeroDouble(-3.0)
      "(NegDoubles.NegZFiniteDouble(-3.0) : Byte)" shouldNot compile
      "(NegDoubles.NegZFiniteDouble(-3.0) : Short)" shouldNot compile
      "(NegDoubles.NegZFiniteDouble(-3.0) : Char)" shouldNot compile
      "(NegDoubles.NegZFiniteDouble(-3.0) : Int)" shouldNot compile
      "(NegDoubles.NegZFiniteDouble(-3.0) : Long)" shouldNot compile
      "(NegDoubles.NegZFiniteDouble(-3.0) : Float)" shouldNot compile
      "(NegDoubles.NegZFiniteDouble(-3.0) : PosInts.PosInt)" shouldNot compile
      "(NegDoubles.NegZFiniteDouble(-3.0) : PosInts.PosZInt)" shouldNot compile
      "(NegDoubles.NegZFiniteDouble(-3.0) : NegInts.NegInt)" shouldNot compile
      "(NegDoubles.NegZFiniteDouble(-3.0) : NegInts.NegZInt)" shouldNot compile
      "(NegDoubles.NegZFiniteDouble(-3.0) : NonZeroInts.NonZeroInt)" shouldNot compile
      "(NegDoubles.NegZFiniteDouble(-3.0) : Percentages.PercentageInt)" shouldNot compile
      "(NegDoubles.NegZFiniteDouble(-3.0) : Numerics.NumericChar)" shouldNot compile
      "(NegDoubles.NegZFiniteDouble(-3.0) : PosLongs.PosLong)" shouldNot compile
      "(NegDoubles.NegZFiniteDouble(-3.0) : PosLongs.PosZLong)" shouldNot compile
      "(NegDoubles.NegZFiniteDouble(-3.0) : NegLongs.NegLong)" shouldNot compile
      "(NegDoubles.NegZFiniteDouble(-3.0) : NegLongs.NegZLong)" shouldNot compile
      "(NegDoubles.NegZFiniteDouble(-3.0) : NonZeroLongs.NonZeroLong)" shouldNot compile
      "(NegDoubles.NegZFiniteDouble(-3.0) : PosFloats.PosFloat)" shouldNot compile
      "(NegDoubles.NegZFiniteDouble(-3.0) : PosFloats.PosZFloat)" shouldNot compile
      "(NegDoubles.NegZFiniteDouble(-3.0) : NegFloats.NegFloat)" shouldNot compile
      "(NegDoubles.NegZFiniteDouble(-3.0) : NegFloats.NegZFloat)" shouldNot compile
      "(NegDoubles.NegZFiniteDouble(-3.0) : NonZeroFloats.NonZeroFloat)" shouldNot compile
      "(NegDoubles.NegZFiniteDouble(-3.0) : NonZeroFloats.NonZeroFiniteFloat)" shouldNot compile
      "(NegDoubles.NegZFiniteDouble(-3.0) : PosFloats.PosFiniteFloat)" shouldNot compile
      "(NegDoubles.NegZFiniteDouble(-3.0) : PosFloats.PosZFiniteFloat)" shouldNot compile
      "(NegDoubles.NegZFiniteDouble(-3.0) : NegFloats.NegFiniteFloat)" shouldNot compile
      "(NegDoubles.NegZFiniteDouble(-3.0) : NegFloats.NegZFiniteFloat)" shouldNot compile
      "(NegDoubles.NegZFiniteDouble(-3.0) : Finites.FiniteFloat)" shouldNot compile
      "(NegDoubles.NegZFiniteDouble(-3.0) : PosDoubles.PosDouble)" shouldNot compile
      "(NegDoubles.NegZFiniteDouble(-3.0) : PosDoubles.PosZDouble)" shouldNot compile
      "(NegDoubles.NegZFiniteDouble(-3.0) : NegDoubles.NegDouble)" shouldNot compile
      "(NegDoubles.NegZFiniteDouble(-3.0) : NonZeroDoubles.NonZeroFiniteDouble)" shouldNot compile
      "(NegDoubles.NegZFiniteDouble(-3.0) : PosDoubles.PosFiniteDouble)" shouldNot compile
      "(NegDoubles.NegZFiniteDouble(-3.0) : PosDoubles.PosZFiniteDouble)" shouldNot compile
      "(NegDoubles.NegZFiniteDouble(-3.0) : NegDoubles.NegFiniteDouble)" shouldNot compile
      "(NegDoubles.NegZFiniteDouble(-3.0) : Finites.FiniteDouble)" shouldNot compile
    }
    it("when a compatible AnyVal is passed to a + method invoked on it should give the same AnyVal type back at compile time, and correct value at runtime") {
      NegDoubles.NegZFiniteDouble(-3.0) + 3.toByte shouldEqual 0.0
      NegDoubles.NegZFiniteDouble(-3.0) + 3.toShort shouldEqual 0.0
      NegDoubles.NegZFiniteDouble(-3.0) + '3' shouldEqual 48.0
      NegDoubles.NegZFiniteDouble(-3.0) + 3 shouldEqual 0.0
      NegDoubles.NegZFiniteDouble(-3.0) + 3L shouldEqual 0.0
      NegDoubles.NegZFiniteDouble(-3.0) + 3.0f shouldEqual 0.0
      NegDoubles.NegZFiniteDouble(-3.0) + 3.0 shouldEqual 0.0
      NegDoubles.NegZFiniteDouble(-3.0) + PosInts.PosInt(3) shouldEqual 0.0
      NegDoubles.NegZFiniteDouble(-3.0) + PosInts.PosZInt(3) shouldEqual 0.0
      NegDoubles.NegZFiniteDouble(-3.0) + NegInts.NegInt(-3) shouldEqual -6.0
      NegDoubles.NegZFiniteDouble(-3.0) + NegInts.NegZInt(-3) shouldEqual -6.0
      NegDoubles.NegZFiniteDouble(-3.0) + NonZeroInts.NonZeroInt(3) shouldEqual 0.0
      NegDoubles.NegZFiniteDouble(-3.0) + Percentages.PercentageInt(50) shouldEqual 47.0
      NegDoubles.NegZFiniteDouble(-3.0) + PosLongs.PosLong(3L) shouldEqual 0.0
      NegDoubles.NegZFiniteDouble(-3.0) + PosLongs.PosZLong(3L) shouldEqual 0.0
      NegDoubles.NegZFiniteDouble(-3.0) + NegLongs.NegLong(-3L) shouldEqual -6.0
      NegDoubles.NegZFiniteDouble(-3.0) + NegLongs.NegZLong(-3L) shouldEqual -6.0
      NegDoubles.NegZFiniteDouble(-3.0) + NonZeroLongs.NonZeroLong(3L) shouldEqual 0.0
      NegDoubles.NegZFiniteDouble(-3.0) + PosFloats.PosFloat(3.0f) shouldEqual 0.0
      NegDoubles.NegZFiniteDouble(-3.0) + PosFloats.PosZFloat(3.0f) shouldEqual 0.0
      NegDoubles.NegZFiniteDouble(-3.0) + NegFloats.NegFloat(-3.0f) shouldEqual -6.0
      NegDoubles.NegZFiniteDouble(-3.0) + NegFloats.NegZFloat(-3.0f) shouldEqual -6.0
      NegDoubles.NegZFiniteDouble(-3.0) + NonZeroFloats.NonZeroFloat(3.0f) shouldEqual 0.0
      NegDoubles.NegZFiniteDouble(-3.0) + NonZeroFloats.NonZeroFiniteFloat(3.0f) shouldEqual 0.0
      NegDoubles.NegZFiniteDouble(-3.0) + PosFloats.PosFiniteFloat(3.0f) shouldEqual 0.0
      NegDoubles.NegZFiniteDouble(-3.0) + PosFloats.PosZFiniteFloat(3.0f) shouldEqual 0.0
      NegDoubles.NegZFiniteDouble(-3.0) + NegFloats.NegFiniteFloat(-3.0f) shouldEqual -6.0
      NegDoubles.NegZFiniteDouble(-3.0) + NegFloats.NegZFiniteFloat(-3.0f) shouldEqual -6.0
      NegDoubles.NegZFiniteDouble(-3.0) + Finites.FiniteFloat(3.0f) shouldEqual 0.0
      NegDoubles.NegZFiniteDouble(-3.0) + PosDoubles.PosDouble(3.0) shouldEqual 0.0
      NegDoubles.NegZFiniteDouble(-3.0) + NegDoubles.NegDouble(-3.0) shouldEqual -6.0
      NegDoubles.NegZFiniteDouble(-3.0) + NegDoubles.NegZDouble(-3.0) shouldEqual -6.0
      NegDoubles.NegZFiniteDouble(-3.0) + NonZeroDoubles.NonZeroDouble(3.0) shouldEqual 0.0
      NegDoubles.NegZFiniteDouble(-3.0) + NonZeroDoubles.NonZeroFiniteDouble(3.0) shouldEqual 0.0
      NegDoubles.NegZFiniteDouble(-3.0) + PosDoubles.PosFiniteDouble(3.0) shouldEqual 0.0
      NegDoubles.NegZFiniteDouble(-3.0) + PosDoubles.PosZFiniteDouble(3.0) shouldEqual 0.0
      NegDoubles.NegZFiniteDouble(-3.0) + NegDoubles.NegFiniteDouble(-3.0) shouldEqual -6.0
      NegDoubles.NegZFiniteDouble(-3.0) + Finites.FiniteDouble(3.0) shouldEqual 0.0
    }
    it("when a compatible AnyVal is passed to a - method invoked on it should give the same AnyVal type back at compile time, and correct value at runtime") {
      NegDoubles.NegZFiniteDouble(-3.0) - 3.toByte shouldEqual -6.0
      NegDoubles.NegZFiniteDouble(-3.0) - 3.toShort shouldEqual -6.0
      NegDoubles.NegZFiniteDouble(-3.0) - '3' shouldEqual -54.0
      NegDoubles.NegZFiniteDouble(-3.0) - 3 shouldEqual -6.0
      NegDoubles.NegZFiniteDouble(-3.0) - 3L shouldEqual -6.0
      NegDoubles.NegZFiniteDouble(-3.0) - 3.0f shouldEqual -6.0
      NegDoubles.NegZFiniteDouble(-3.0) - 3.0 shouldEqual -6.0
      NegDoubles.NegZFiniteDouble(-3.0) - PosInts.PosInt(3) shouldEqual -6.0
      NegDoubles.NegZFiniteDouble(-3.0) - PosInts.PosZInt(3) shouldEqual -6.0
      NegDoubles.NegZFiniteDouble(-3.0) - NegInts.NegInt(-3) shouldEqual 0.0
      NegDoubles.NegZFiniteDouble(-3.0) - NegInts.NegZInt(-3) shouldEqual 0.0
      NegDoubles.NegZFiniteDouble(-3.0) - NonZeroInts.NonZeroInt(3) shouldEqual -6.0
      NegDoubles.NegZFiniteDouble(-3.0) - Percentages.PercentageInt(50) shouldEqual -53.0
      NegDoubles.NegZFiniteDouble(-3.0) - PosLongs.PosLong(3L) shouldEqual -6.0
      NegDoubles.NegZFiniteDouble(-3.0) - PosLongs.PosZLong(3L) shouldEqual -6.0
      NegDoubles.NegZFiniteDouble(-3.0) - NegLongs.NegLong(-3L) shouldEqual 0.0
      NegDoubles.NegZFiniteDouble(-3.0) - NegLongs.NegZLong(-3L) shouldEqual 0.0
      NegDoubles.NegZFiniteDouble(-3.0) - NonZeroLongs.NonZeroLong(3L) shouldEqual -6.0
      NegDoubles.NegZFiniteDouble(-3.0) - PosFloats.PosFloat(3.0f) shouldEqual -6.0
      NegDoubles.NegZFiniteDouble(-3.0) - PosFloats.PosZFloat(3.0f) shouldEqual -6.0
      NegDoubles.NegZFiniteDouble(-3.0) - NegFloats.NegFloat(-3.0f) shouldEqual 0.0
      NegDoubles.NegZFiniteDouble(-3.0) - NegFloats.NegZFloat(-3.0f) shouldEqual 0.0
      NegDoubles.NegZFiniteDouble(-3.0) - NonZeroFloats.NonZeroFloat(3.0f) shouldEqual -6.0
      NegDoubles.NegZFiniteDouble(-3.0) - NonZeroFloats.NonZeroFiniteFloat(3.0f) shouldEqual -6.0
      NegDoubles.NegZFiniteDouble(-3.0) - PosFloats.PosFiniteFloat(3.0f) shouldEqual -6.0
      NegDoubles.NegZFiniteDouble(-3.0) - PosFloats.PosZFiniteFloat(3.0f) shouldEqual -6.0
      NegDoubles.NegZFiniteDouble(-3.0) - NegFloats.NegFiniteFloat(-3.0f) shouldEqual 0.0
      NegDoubles.NegZFiniteDouble(-3.0) - NegFloats.NegZFiniteFloat(-3.0f) shouldEqual 0.0
      NegDoubles.NegZFiniteDouble(-3.0) - Finites.FiniteFloat(3.0f) shouldEqual -6.0
      NegDoubles.NegZFiniteDouble(-3.0) - PosDoubles.PosDouble(3.0) shouldEqual -6.0
      NegDoubles.NegZFiniteDouble(-3.0) - NegDoubles.NegDouble(-3.0) shouldEqual 0.0
      NegDoubles.NegZFiniteDouble(-3.0) - NegDoubles.NegZDouble(-3.0) shouldEqual 0.0
      NegDoubles.NegZFiniteDouble(-3.0) - NonZeroDoubles.NonZeroDouble(3.0) shouldEqual -6.0
      NegDoubles.NegZFiniteDouble(-3.0) - NonZeroDoubles.NonZeroFiniteDouble(3.0) shouldEqual -6.0
      NegDoubles.NegZFiniteDouble(-3.0) - PosDoubles.PosFiniteDouble(3.0) shouldEqual -6.0
      NegDoubles.NegZFiniteDouble(-3.0) - PosDoubles.PosZFiniteDouble(3.0) shouldEqual -6.0
      NegDoubles.NegZFiniteDouble(-3.0) - NegDoubles.NegFiniteDouble(-3.0) shouldEqual 0.0
      NegDoubles.NegZFiniteDouble(-3.0) - Finites.FiniteDouble(3.0) shouldEqual -6.0
    }
    it("when a compatible AnyVal is passed to a * method invoked on it should give the same AnyVal type back at compile time, and correct value at runtime") {
      NegDoubles.NegZFiniteDouble(-3.0) * 3.toByte shouldEqual -9.0
      NegDoubles.NegZFiniteDouble(-3.0) * 3.toShort shouldEqual -9.0
      NegDoubles.NegZFiniteDouble(-3.0) * '3' shouldEqual -153.0
      NegDoubles.NegZFiniteDouble(-3.0) * 3 shouldEqual -9.0
      NegDoubles.NegZFiniteDouble(-3.0) * 3L shouldEqual -9.0
      NegDoubles.NegZFiniteDouble(-3.0) * 3.0f shouldEqual -9.0
      NegDoubles.NegZFiniteDouble(-3.0) * 3.0 shouldEqual -9.0
      NegDoubles.NegZFiniteDouble(-3.0) * PosInts.PosInt(3) shouldEqual -9.0
      NegDoubles.NegZFiniteDouble(-3.0) * PosInts.PosZInt(3) shouldEqual -9.0
      NegDoubles.NegZFiniteDouble(-3.0) * NegInts.NegInt(-3) shouldEqual 9.0
      NegDoubles.NegZFiniteDouble(-3.0) * NegInts.NegZInt(-3) shouldEqual 9.0
      NegDoubles.NegZFiniteDouble(-3.0) * NonZeroInts.NonZeroInt(3) shouldEqual -9.0
      NegDoubles.NegZFiniteDouble(-3.0) * Percentages.PercentageInt(50) shouldEqual -150.0
      NegDoubles.NegZFiniteDouble(-3.0) * PosLongs.PosLong(3L) shouldEqual -9.0
      NegDoubles.NegZFiniteDouble(-3.0) * PosLongs.PosZLong(3L) shouldEqual -9.0
      NegDoubles.NegZFiniteDouble(-3.0) * NegLongs.NegLong(-3L) shouldEqual 9.0
      NegDoubles.NegZFiniteDouble(-3.0) * NegLongs.NegZLong(-3L) shouldEqual 9.0
      NegDoubles.NegZFiniteDouble(-3.0) * NonZeroLongs.NonZeroLong(3L) shouldEqual -9.0
      NegDoubles.NegZFiniteDouble(-3.0) * PosFloats.PosFloat(3.0f) shouldEqual -9.0
      NegDoubles.NegZFiniteDouble(-3.0) * PosFloats.PosZFloat(3.0f) shouldEqual -9.0
      NegDoubles.NegZFiniteDouble(-3.0) * NegFloats.NegFloat(-3.0f) shouldEqual 9.0
      NegDoubles.NegZFiniteDouble(-3.0) * NegFloats.NegZFloat(-3.0f) shouldEqual 9.0
      NegDoubles.NegZFiniteDouble(-3.0) * NonZeroFloats.NonZeroFloat(3.0f) shouldEqual -9.0
      NegDoubles.NegZFiniteDouble(-3.0) * NonZeroFloats.NonZeroFiniteFloat(3.0f) shouldEqual -9.0
      NegDoubles.NegZFiniteDouble(-3.0) * PosFloats.PosFiniteFloat(3.0f) shouldEqual -9.0
      NegDoubles.NegZFiniteDouble(-3.0) * PosFloats.PosZFiniteFloat(3.0f) shouldEqual -9.0
      NegDoubles.NegZFiniteDouble(-3.0) * NegFloats.NegFiniteFloat(-3.0f) shouldEqual 9.0
      NegDoubles.NegZFiniteDouble(-3.0) * NegFloats.NegZFiniteFloat(-3.0f) shouldEqual 9.0
      NegDoubles.NegZFiniteDouble(-3.0) * Finites.FiniteFloat(3.0f) shouldEqual -9.0
      NegDoubles.NegZFiniteDouble(-3.0) * PosDoubles.PosDouble(3.0) shouldEqual -9.0
      NegDoubles.NegZFiniteDouble(-3.0) * NegDoubles.NegDouble(-3.0) shouldEqual 9.0
      NegDoubles.NegZFiniteDouble(-3.0) * NegDoubles.NegZDouble(-3.0) shouldEqual 9.0
      NegDoubles.NegZFiniteDouble(-3.0) * NonZeroDoubles.NonZeroDouble(3.0) shouldEqual -9.0
      NegDoubles.NegZFiniteDouble(-3.0) * NonZeroDoubles.NonZeroFiniteDouble(3.0) shouldEqual -9.0
      NegDoubles.NegZFiniteDouble(-3.0) * PosDoubles.PosFiniteDouble(3.0) shouldEqual -9.0
      NegDoubles.NegZFiniteDouble(-3.0) * PosDoubles.PosZFiniteDouble(3.0) shouldEqual -9.0
      NegDoubles.NegZFiniteDouble(-3.0) * NegDoubles.NegFiniteDouble(-3.0) shouldEqual 9.0
      NegDoubles.NegZFiniteDouble(-3.0) * Finites.FiniteDouble(3.0) shouldEqual -9.0
    }
    it("when a compatible AnyVal is passed to a / method invoked on it should give the same AnyVal type back at compile time, and correct value at runtime") {
      NegDoubles.NegZFiniteDouble(-3.0) / 3.toByte shouldEqual -1.0
      NegDoubles.NegZFiniteDouble(-3.0) / 3.toShort shouldEqual -1.0
      NegDoubles.NegZFiniteDouble(-3.0) / '3' shouldEqual -0.058823529411764705
      NegDoubles.NegZFiniteDouble(-3.0) / 3 shouldEqual -1.0
      NegDoubles.NegZFiniteDouble(-3.0) / 3L shouldEqual -1.0
      NegDoubles.NegZFiniteDouble(-3.0) / 3.0f shouldEqual -1.0
      NegDoubles.NegZFiniteDouble(-3.0) / 3.0 shouldEqual -1.0
      NegDoubles.NegZFiniteDouble(-3.0) / PosInts.PosInt(3) shouldEqual -1.0
      NegDoubles.NegZFiniteDouble(-3.0) / PosInts.PosZInt(3) shouldEqual -1.0
      NegDoubles.NegZFiniteDouble(-3.0) / NegInts.NegInt(-3) shouldEqual 1.0
      NegDoubles.NegZFiniteDouble(-3.0) / NegInts.NegZInt(-3) shouldEqual 1.0
      NegDoubles.NegZFiniteDouble(-3.0) / NonZeroInts.NonZeroInt(3) shouldEqual -1.0
      NegDoubles.NegZFiniteDouble(-3.0) / Percentages.PercentageInt(50) shouldEqual -0.06
      NegDoubles.NegZFiniteDouble(-3.0) / PosLongs.PosLong(3L) shouldEqual -1.0
      NegDoubles.NegZFiniteDouble(-3.0) / PosLongs.PosZLong(3L) shouldEqual -1.0
      NegDoubles.NegZFiniteDouble(-3.0) / NegLongs.NegLong(-3L) shouldEqual 1.0
      NegDoubles.NegZFiniteDouble(-3.0) / NegLongs.NegZLong(-3L) shouldEqual 1.0
      NegDoubles.NegZFiniteDouble(-3.0) / NonZeroLongs.NonZeroLong(3L) shouldEqual -1.0
      NegDoubles.NegZFiniteDouble(-3.0) / PosFloats.PosFloat(3.0f) shouldEqual -1.0
      NegDoubles.NegZFiniteDouble(-3.0) / PosFloats.PosZFloat(3.0f) shouldEqual -1.0
      NegDoubles.NegZFiniteDouble(-3.0) / NegFloats.NegFloat(-3.0f) shouldEqual 1.0
      NegDoubles.NegZFiniteDouble(-3.0) / NegFloats.NegZFloat(-3.0f) shouldEqual 1.0
      NegDoubles.NegZFiniteDouble(-3.0) / NonZeroFloats.NonZeroFloat(3.0f) shouldEqual -1.0
      NegDoubles.NegZFiniteDouble(-3.0) / NonZeroFloats.NonZeroFiniteFloat(3.0f) shouldEqual -1.0
      NegDoubles.NegZFiniteDouble(-3.0) / PosFloats.PosFiniteFloat(3.0f) shouldEqual -1.0
      NegDoubles.NegZFiniteDouble(-3.0) / PosFloats.PosZFiniteFloat(3.0f) shouldEqual -1.0
      NegDoubles.NegZFiniteDouble(-3.0) / NegFloats.NegFiniteFloat(-3.0f) shouldEqual 1.0
      NegDoubles.NegZFiniteDouble(-3.0) / NegFloats.NegZFiniteFloat(-3.0f) shouldEqual 1.0
      NegDoubles.NegZFiniteDouble(-3.0) / Finites.FiniteFloat(3.0f) shouldEqual -1.0
      NegDoubles.NegZFiniteDouble(-3.0) / PosDoubles.PosDouble(3.0) shouldEqual -1.0
      NegDoubles.NegZFiniteDouble(-3.0) / NegDoubles.NegDouble(-3.0) shouldEqual 1.0
      NegDoubles.NegZFiniteDouble(-3.0) / NegDoubles.NegZDouble(-3.0) shouldEqual 1.0
      NegDoubles.NegZFiniteDouble(-3.0) / NonZeroDoubles.NonZeroDouble(3.0) shouldEqual -1.0
      NegDoubles.NegZFiniteDouble(-3.0) / NonZeroDoubles.NonZeroFiniteDouble(3.0) shouldEqual -1.0
      NegDoubles.NegZFiniteDouble(-3.0) / PosDoubles.PosFiniteDouble(3.0) shouldEqual -1.0
      NegDoubles.NegZFiniteDouble(-3.0) / PosDoubles.PosZFiniteDouble(3.0) shouldEqual -1.0
      NegDoubles.NegZFiniteDouble(-3.0) / NegDoubles.NegFiniteDouble(-3.0) shouldEqual 1.0
      NegDoubles.NegZFiniteDouble(-3.0) / Finites.FiniteDouble(3.0) shouldEqual -1.0
    }
    it("when a compatible AnyVal is passed to a % method invoked on it should give the same AnyVal type back at compile time, and correct value at runtime") {
      NegDoubles.NegZFiniteDouble(-3.0) % 3.toByte shouldEqual -0.0
      NegDoubles.NegZFiniteDouble(-3.0) % 3.toShort shouldEqual -0.0
      NegDoubles.NegZFiniteDouble(-3.0) % '3' shouldEqual -3.0
      NegDoubles.NegZFiniteDouble(-3.0) % 3 shouldEqual -0.0
      NegDoubles.NegZFiniteDouble(-3.0) % 3L shouldEqual -0.0
      NegDoubles.NegZFiniteDouble(-3.0) % 3.0f shouldEqual -0.0
      NegDoubles.NegZFiniteDouble(-3.0) % 3.0 shouldEqual -0.0
      NegDoubles.NegZFiniteDouble(-3.0) % PosInts.PosInt(3) shouldEqual -0.0
      NegDoubles.NegZFiniteDouble(-3.0) % PosInts.PosZInt(3) shouldEqual -0.0
      NegDoubles.NegZFiniteDouble(-3.0) % NegInts.NegInt(-3) shouldEqual -0.0
      NegDoubles.NegZFiniteDouble(-3.0) % NegInts.NegZInt(-3) shouldEqual -0.0
      NegDoubles.NegZFiniteDouble(-3.0) % NonZeroInts.NonZeroInt(3) shouldEqual -0.0
      NegDoubles.NegZFiniteDouble(-3.0) % Percentages.PercentageInt(50) shouldEqual -3.0
      NegDoubles.NegZFiniteDouble(-3.0) % PosLongs.PosLong(3L) shouldEqual -0.0
      NegDoubles.NegZFiniteDouble(-3.0) % PosLongs.PosZLong(3L) shouldEqual -0.0
      NegDoubles.NegZFiniteDouble(-3.0) % NegLongs.NegLong(-3L) shouldEqual -0.0
      NegDoubles.NegZFiniteDouble(-3.0) % NegLongs.NegZLong(-3L) shouldEqual -0.0
      NegDoubles.NegZFiniteDouble(-3.0) % NonZeroLongs.NonZeroLong(3L) shouldEqual -0.0
      NegDoubles.NegZFiniteDouble(-3.0) % PosFloats.PosFloat(3.0f) shouldEqual -0.0
      NegDoubles.NegZFiniteDouble(-3.0) % PosFloats.PosZFloat(3.0f) shouldEqual -0.0
      NegDoubles.NegZFiniteDouble(-3.0) % NegFloats.NegFloat(-3.0f) shouldEqual -0.0
      NegDoubles.NegZFiniteDouble(-3.0) % NegFloats.NegZFloat(-3.0f) shouldEqual -0.0
      NegDoubles.NegZFiniteDouble(-3.0) % NonZeroFloats.NonZeroFloat(3.0f) shouldEqual -0.0
      NegDoubles.NegZFiniteDouble(-3.0) % NonZeroFloats.NonZeroFiniteFloat(3.0f) shouldEqual -0.0
      NegDoubles.NegZFiniteDouble(-3.0) % PosFloats.PosFiniteFloat(3.0f) shouldEqual -0.0
      NegDoubles.NegZFiniteDouble(-3.0) % PosFloats.PosZFiniteFloat(3.0f) shouldEqual -0.0
      NegDoubles.NegZFiniteDouble(-3.0) % NegFloats.NegFiniteFloat(-3.0f) shouldEqual -0.0
      NegDoubles.NegZFiniteDouble(-3.0) % NegFloats.NegZFiniteFloat(-3.0f) shouldEqual -0.0
      NegDoubles.NegZFiniteDouble(-3.0) % Finites.FiniteFloat(3.0f) shouldEqual -0.0
      NegDoubles.NegZFiniteDouble(-3.0) % PosDoubles.PosDouble(3.0) shouldEqual -0.0
      NegDoubles.NegZFiniteDouble(-3.0) % NegDoubles.NegDouble(-3.0) shouldEqual -0.0
      NegDoubles.NegZFiniteDouble(-3.0) % NegDoubles.NegZDouble(-3.0) shouldEqual -0.0
      NegDoubles.NegZFiniteDouble(-3.0) % NonZeroDoubles.NonZeroDouble(3.0) shouldEqual -0.0
      NegDoubles.NegZFiniteDouble(-3.0) % NonZeroDoubles.NonZeroFiniteDouble(3.0) shouldEqual -0.0
      NegDoubles.NegZFiniteDouble(-3.0) % PosDoubles.PosFiniteDouble(3.0) shouldEqual -0.0
      NegDoubles.NegZFiniteDouble(-3.0) % PosDoubles.PosZFiniteDouble(3.0) shouldEqual -0.0
      NegDoubles.NegZFiniteDouble(-3.0) % NegDoubles.NegFiniteDouble(-3.0) shouldEqual -0.0
      NegDoubles.NegZFiniteDouble(-3.0) % Finites.FiniteDouble(3.0) shouldEqual -0.0
    }
    it("should offer '<' comparison that is consistent with Double") {
      (NegDoubles.NegZFiniteDouble(-3.0) < 3.toByte) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) < 3.toShort) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) < '3') shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) < 3) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) < 3L) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) < 3.0f) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) < 3.0) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) < PosInts.PosInt(3)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) < PosInts.PosZInt(3)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) < NegInts.NegInt(-3)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) < NegInts.NegZInt(-3)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) < NonZeroInts.NonZeroInt(3)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) < Percentages.PercentageInt(50)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) < PosLongs.PosLong(3L)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) < PosLongs.PosZLong(3L)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) < NegLongs.NegLong(-3L)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) < NegLongs.NegZLong(-3L)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) < NonZeroLongs.NonZeroLong(3L)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) < PosFloats.PosFloat(3.0f)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) < PosFloats.PosZFloat(3.0f)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) < NegFloats.NegFloat(-3.0f)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) < NegFloats.NegZFloat(-3.0f)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) < NonZeroFloats.NonZeroFloat(3.0f)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) < NonZeroFloats.NonZeroFiniteFloat(3.0f)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) < PosFloats.PosFiniteFloat(3.0f)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) < PosFloats.PosZFiniteFloat(3.0f)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) < NegFloats.NegFiniteFloat(-3.0f)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) < NegFloats.NegZFiniteFloat(-3.0f)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) < Finites.FiniteFloat(3.0f)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) < PosDoubles.PosDouble(3.0)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) < NegDoubles.NegDouble(-3.0)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) < NegDoubles.NegZDouble(-3.0)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) < NonZeroDoubles.NonZeroDouble(3.0)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) < NonZeroDoubles.NonZeroFiniteDouble(3.0)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) < PosDoubles.PosFiniteDouble(3.0)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) < PosDoubles.PosZFiniteDouble(3.0)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) < NegDoubles.NegFiniteDouble(-3.0)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) < Finites.FiniteDouble(3.0)) shouldEqual true
    }
    it("should offer '<=' comparison that is consistent with Double") {
      (NegDoubles.NegZFiniteDouble(-3.0) <= 3.toByte) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) <= 3.toShort) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) <= '3') shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) <= 3) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) <= 3L) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) <= 3.0f) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) <= 3.0) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) <= PosInts.PosInt(3)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) <= PosInts.PosZInt(3)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) <= NegInts.NegInt(-3)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) <= NegInts.NegZInt(-3)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) <= NonZeroInts.NonZeroInt(3)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) <= Percentages.PercentageInt(50)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) <= PosLongs.PosLong(3L)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) <= PosLongs.PosZLong(3L)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) <= NegLongs.NegLong(-3L)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) <= NegLongs.NegZLong(-3L)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) <= NonZeroLongs.NonZeroLong(3L)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) <= PosFloats.PosFloat(3.0f)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) <= PosFloats.PosZFloat(3.0f)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) <= NegFloats.NegFloat(-3.0f)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) <= NegFloats.NegZFloat(-3.0f)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) <= NonZeroFloats.NonZeroFloat(3.0f)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) <= NonZeroFloats.NonZeroFiniteFloat(3.0f)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) <= PosFloats.PosFiniteFloat(3.0f)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) <= PosFloats.PosZFiniteFloat(3.0f)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) <= NegFloats.NegFiniteFloat(-3.0f)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) <= NegFloats.NegZFiniteFloat(-3.0f)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) <= Finites.FiniteFloat(3.0f)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) <= PosDoubles.PosDouble(3.0)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) <= NegDoubles.NegDouble(-3.0)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) <= NegDoubles.NegZDouble(-3.0)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) <= NonZeroDoubles.NonZeroDouble(3.0)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) <= NonZeroDoubles.NonZeroFiniteDouble(3.0)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) <= PosDoubles.PosFiniteDouble(3.0)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) <= PosDoubles.PosZFiniteDouble(3.0)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) <= NegDoubles.NegFiniteDouble(-3.0)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) <= Finites.FiniteDouble(3.0)) shouldEqual true
    }
    it("should offer '>' comparison that is consistent with Double") {
      (NegDoubles.NegZFiniteDouble(-3.0) > 3.toByte) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) > 3.toShort) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) > '3') shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) > 3) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) > 3L) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) > 3.0f) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) > 3.0) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) > PosInts.PosInt(3)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) > PosInts.PosZInt(3)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) > NegInts.NegInt(-3)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) > NegInts.NegZInt(-3)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) > NonZeroInts.NonZeroInt(3)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) > Percentages.PercentageInt(50)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) > PosLongs.PosLong(3L)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) > PosLongs.PosZLong(3L)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) > NegLongs.NegLong(-3L)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) > NegLongs.NegZLong(-3L)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) > NonZeroLongs.NonZeroLong(3L)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) > PosFloats.PosFloat(3.0f)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) > PosFloats.PosZFloat(3.0f)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) > NegFloats.NegFloat(-3.0f)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) > NegFloats.NegZFloat(-3.0f)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) > NonZeroFloats.NonZeroFloat(3.0f)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) > NonZeroFloats.NonZeroFiniteFloat(3.0f)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) > PosFloats.PosFiniteFloat(3.0f)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) > PosFloats.PosZFiniteFloat(3.0f)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) > NegFloats.NegFiniteFloat(-3.0f)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) > NegFloats.NegZFiniteFloat(-3.0f)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) > Finites.FiniteFloat(3.0f)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) > PosDoubles.PosDouble(3.0)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) > NegDoubles.NegDouble(-3.0)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) > NegDoubles.NegZDouble(-3.0)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) > NonZeroDoubles.NonZeroDouble(3.0)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) > NonZeroDoubles.NonZeroFiniteDouble(3.0)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) > PosDoubles.PosFiniteDouble(3.0)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) > PosDoubles.PosZFiniteDouble(3.0)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) > NegDoubles.NegFiniteDouble(-3.0)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) > Finites.FiniteDouble(3.0)) shouldEqual false
    }
    it("should offer '>=' comparison that is consistent with Double") {
      (NegDoubles.NegZFiniteDouble(-3.0) >= 3.toByte) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) >= 3.toShort) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) >= '3') shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) >= 3) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) >= 3L) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) >= 3.0f) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) >= 3.0) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) >= PosInts.PosInt(3)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) >= PosInts.PosZInt(3)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) >= NegInts.NegInt(-3)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) >= NegInts.NegZInt(-3)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) >= NonZeroInts.NonZeroInt(3)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) >= Percentages.PercentageInt(50)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) >= PosLongs.PosLong(3L)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) >= PosLongs.PosZLong(3L)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) >= NegLongs.NegLong(-3L)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) >= NegLongs.NegZLong(-3L)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) >= NonZeroLongs.NonZeroLong(3L)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) >= PosFloats.PosFloat(3.0f)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) >= PosFloats.PosZFloat(3.0f)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) >= NegFloats.NegFloat(-3.0f)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) >= NegFloats.NegZFloat(-3.0f)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) >= NonZeroFloats.NonZeroFloat(3.0f)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) >= NonZeroFloats.NonZeroFiniteFloat(3.0f)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) >= PosFloats.PosFiniteFloat(3.0f)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) >= PosFloats.PosZFiniteFloat(3.0f)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) >= NegFloats.NegFiniteFloat(-3.0f)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) >= NegFloats.NegZFiniteFloat(-3.0f)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) >= Finites.FiniteFloat(3.0f)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) >= PosDoubles.PosDouble(3.0)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) >= NegDoubles.NegDouble(-3.0)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) >= NegDoubles.NegZDouble(-3.0)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) >= NonZeroDoubles.NonZeroDouble(3.0)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) >= NonZeroDoubles.NonZeroFiniteDouble(3.0)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) >= PosDoubles.PosFiniteDouble(3.0)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) >= PosDoubles.PosZFiniteDouble(3.0)) shouldEqual false
      (NegDoubles.NegZFiniteDouble(-3.0) >= NegDoubles.NegFiniteDouble(-3.0)) shouldEqual true
      (NegDoubles.NegZFiniteDouble(-3.0) >= Finites.FiniteDouble(3.0)) shouldEqual false
    }
    it("should offer 'min' and 'max' methods that are consistent with Double") {
      NegDoubles.NegZFiniteDouble(-3.0).max(NegDoubles.NegZFiniteDouble(-3.0)) shouldEqual NegDoubles.NegZFiniteDouble(-3.0)
      NegDoubles.NegZFiniteDouble(-3.0).min(NegDoubles.NegZFiniteDouble(-3.0)) shouldEqual NegDoubles.NegZFiniteDouble(-3.0)
    }
    it("should return the same type from its unary_+ method") {
      succeed
    }
    it("should offer a unary - method that is consistent with Double") {
      (-NegDoubles.NegZFiniteDouble(-3.0)) shouldEqual 3.0
    }
    it("should offer widening methods for basic type Double that are consistent with Double") {
      def widen(value: Double): Double = value
      widen(NegDoubles.NegZFiniteDouble(-3.0)) shouldEqual -3.0
    }
    it("should offer widening methods for basic type NegDoubles.NegZDouble that are consistent with Double") {
      def widen(value: NegDoubles.NegZDouble): NegDoubles.NegZDouble = value
      widen(NegDoubles.NegZFiniteDouble(-3.0)) shouldEqual NegDoubles.NegZDouble(-3.0)
    }
    it("should offer widening methods for basic type NonZeroDoubles.NonZeroDouble that are consistent with Double") {
      def widen(value: NonZeroDoubles.NonZeroDouble): NonZeroDoubles.NonZeroDouble = value
      widen(NegDoubles.NegZFiniteDouble(-3.0)) shouldEqual NonZeroDoubles.NonZeroDouble(-3.0)
    }
    it("should have a toString consistent with Double") {
      (NegDoubles.NegZFiniteDouble(-3.0) : Double).toString shouldBe -3.0.toString
    }
  }
}
