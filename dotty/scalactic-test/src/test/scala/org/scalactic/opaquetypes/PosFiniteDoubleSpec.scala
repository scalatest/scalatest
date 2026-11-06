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
import OptionValues.*
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.prop.PropertyChecks
// SKIP-SCALATESTJS,NATIVE-START
import scala.collection.immutable.NumericRange
// SKIP-SCALATESTJS,NATIVE-END
import scala.util.{Failure, Success, Try}
import org.scalactic.{Good, Bad}
import org.scalactic.{Pass, Fail}
import org.scalactic.Equality

import PosDoubles.{PosFiniteDouble, PosZFiniteDouble, PosZDouble}

trait PosFiniteDoubleSpecSupport {

  implicit def tryEquality[T]: Equality[Try[T]] = new Equality[Try[T]] {
    override def areEqual(a: Try[T], b: Any): Boolean = a match {
      case Success(double: Double) if double.isNaN =>
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

class PosFiniteDoubleSpec extends funspec.AnyFunSpec with matchers.should.Matchers with PropertyChecks with TypeCheckedTripleEquals with PosFiniteDoubleSpecSupport {

  describe("A PosFiniteDouble") {
    describe("should offer a from factory method that") {
      it("returns Some[PosFiniteDouble] if the passed Double is > 0 and finite") {
        PosFiniteDouble.from(50.23) shouldBe Some(PosFiniteDouble(50.23))
        PosFiniteDouble.from(100.0) shouldBe Some(PosFiniteDouble(100.0))
        PosFiniteDouble.from(Double.MaxValue) shouldBe Some(PosFiniteDouble(Double.MaxValue))
        PosFiniteDouble.from(Double.MinPositiveValue) shouldBe Some(PosFiniteDouble(Double.MinPositiveValue))
      }
      it("returns None if the passed Double is 0, negative, or infinite") {
        PosFiniteDouble.from(0.0) shouldBe None
        PosFiniteDouble.from(-0.00001) shouldBe None
        PosFiniteDouble.from(-99.9) shouldBe None
        PosFiniteDouble.from(Double.PositiveInfinity) shouldBe None
        PosFiniteDouble.from(Double.NegativeInfinity) shouldBe None
      }
    }
    describe("should offer an ensuringValid factory method that") {
      it("returns PosFiniteDouble if the passed Double is > 0 and finite") {
        PosFiniteDouble.ensuringValid(50.23) shouldBe PosFiniteDouble(50.23)
        PosFiniteDouble.ensuringValid(100.0) shouldBe PosFiniteDouble(100.0)
        PosFiniteDouble.ensuringValid(Double.MaxValue) shouldBe PosFiniteDouble(Double.MaxValue)
      }
      it("throws AssertionError if the passed Double is 0, negative, or infinite") {
        an [AssertionError] should be thrownBy PosFiniteDouble.ensuringValid(0.0)
        an [AssertionError] should be thrownBy PosFiniteDouble.ensuringValid(-0.00001)
        an [AssertionError] should be thrownBy PosFiniteDouble.ensuringValid(-99.9)
        an [AssertionError] should be thrownBy PosFiniteDouble.ensuringValid(Double.PositiveInfinity)
        an [AssertionError] should be thrownBy PosFiniteDouble.ensuringValid(Double.NegativeInfinity)
      }
    }
    describe("should offer a tryingValid factory method that") {
      import TryValues.*
      it("returns a PosFiniteDouble wrapped in a Success if the passed Double is > 0 and finite") {
        PosFiniteDouble.tryingValid(50.3).success.value.value shouldBe 50.3
        PosFiniteDouble.tryingValid(100.0).success.value.value shouldBe 100.0
      }
      it("returns an AssertionError wrapped in a Failure if the passed Double is invalid") {
        PosFiniteDouble.tryingValid(0.0).failure.exception shouldBe an [AssertionError]
        PosFiniteDouble.tryingValid(-1.0).failure.exception shouldBe an [AssertionError]
        PosFiniteDouble.tryingValid(-99.9).failure.exception shouldBe an [AssertionError]
        PosFiniteDouble.tryingValid(Double.PositiveInfinity).failure.exception shouldBe an [AssertionError]
      }
    }
    describe("should offer a passOrElse factory method that") {
      it("returns a Pass if the given Double is > 0 and finite") {
        PosFiniteDouble.passOrElse(50.0)(i => i) shouldBe Pass
        PosFiniteDouble.passOrElse(100.0)(i => i) shouldBe Pass
      }
      it("returns a Fail if the passed Double is invalid") {
        PosFiniteDouble.passOrElse(0.0)(i => s"$i did not taste good") shouldBe Fail(0.0 + " did not taste good")
        PosFiniteDouble.passOrElse(-1.1)(i => i) shouldBe Fail(-1.1)
        PosFiniteDouble.passOrElse(-99.0)(i => i + 3.0) shouldBe Fail(-96.0)
        PosFiniteDouble.passOrElse(Double.PositiveInfinity)(i => i) shouldBe Fail(Double.PositiveInfinity)
      }
    }
    describe("should offer a goodOrElse factory method that") {
      it("returns a PosFiniteDouble wrapped in a Good if the given Double is > 0 and finite") {
        PosFiniteDouble.goodOrElse(50.3)(i => i) shouldBe Good(PosFiniteDouble(50.3))
        PosFiniteDouble.goodOrElse(100.0)(i => i) shouldBe Good(PosFiniteDouble(100.0))
      }
      it("returns a Bad if the passed Double is invalid") {
        PosFiniteDouble.goodOrElse(0.0)(i => s"$i did not taste good") shouldBe Bad(0.0 + " did not taste good")
        PosFiniteDouble.goodOrElse(-1.1)(i => i) shouldBe Bad(-1.1)
        PosFiniteDouble.goodOrElse(-99.0)(i => i + 3.0) shouldBe Bad(-96.0)
        PosFiniteDouble.goodOrElse(Double.PositiveInfinity)(i => i) shouldBe Bad(Double.PositiveInfinity)
      }
    }
    describe("should offer a rightOrElse factory method that") {
      it("returns a PosFiniteDouble wrapped in a Right if the given Double is > 0 and finite") {
        PosFiniteDouble.rightOrElse(50.3)(i => i) shouldBe Right(PosFiniteDouble(50.3))
        PosFiniteDouble.rightOrElse(100.0)(i => i) shouldBe Right(PosFiniteDouble(100.0))
      }
      it("returns a Left if the passed Double is invalid") {
        PosFiniteDouble.rightOrElse(0.0)(i => s"$i did not taste good") shouldBe Left(0.0 + " did not taste good")
        PosFiniteDouble.rightOrElse(-1.1)(i => i) shouldBe Left(-1.1)
        PosFiniteDouble.rightOrElse(-99.9)(i => i + 3.0) shouldBe Left(-96.9)
        PosFiniteDouble.rightOrElse(Double.PositiveInfinity)(i => i) shouldBe Left(Double.PositiveInfinity)
      }
    }
    describe("should offer an isValid predicate method that") {
      it("returns true if the passed Double is > 0 and finite, false otherwise") {
        PosFiniteDouble.isValid(50.23) shouldBe true
        PosFiniteDouble.isValid(100.0) shouldBe true
        PosFiniteDouble.isValid(Double.MinPositiveValue) shouldBe true
        PosFiniteDouble.isValid(0.0) shouldBe false
        PosFiniteDouble.isValid(-0.0) shouldBe false
        PosFiniteDouble.isValid(-0.00001) shouldBe false
        PosFiniteDouble.isValid(-99.9) shouldBe false
        PosFiniteDouble.isValid(Double.PositiveInfinity) shouldBe false
        PosFiniteDouble.isValid(Double.NegativeInfinity) shouldBe false
      }
    }
    describe("should offer a fromOrElse factory method that") {
      it("returns a PosFiniteDouble if the passed Double is > 0 and finite") {
        PosFiniteDouble.fromOrElse(50.23, PosFiniteDouble(42.0)).value shouldBe 50.23
        PosFiniteDouble.fromOrElse(100.0, PosFiniteDouble(42.0)).value shouldBe 100.0
      }
      it("returns a given default if the passed Double is invalid") {
        PosFiniteDouble.fromOrElse(0.0, PosFiniteDouble(42.0)).value shouldBe 42.0
        PosFiniteDouble.fromOrElse(-0.00001, PosFiniteDouble(42.0)).value shouldBe 42.0
        PosFiniteDouble.fromOrElse(-99.9, PosFiniteDouble(42.0)).value shouldBe 42.0
        PosFiniteDouble.fromOrElse(Double.PositiveInfinity, PosFiniteDouble(42.0)).value shouldBe 42.0
      }
    }
    it("should offer MaxValue and MinValue constants") {
      PosFiniteDouble.MaxValue shouldEqual PosFiniteDouble.from(Double.MaxValue).get
      PosFiniteDouble.MinValue shouldEqual PosFiniteDouble.from(Double.MinPositiveValue).get
      PosFiniteDouble.MinPositiveValue shouldEqual PosFiniteDouble(Double.MinPositiveValue)
    }
    it("should not offer a PositiveInfinity constant") {
      "PosFiniteDouble.PositiveInfinity" shouldNot compile
    }
    it("should not offer a NegativeInfinity constant") {
      "PosFiniteDouble.NegativeInfinity" shouldNot compile
    }

    it("should be a subtype of PosZFiniteDouble") {
      val pfd: PosFiniteDouble = PosFiniteDouble(1.0)
      def takesPosZFiniteDouble(x: PosZFiniteDouble): Double = x.value
      takesPosZFiniteDouble(pfd) shouldEqual 1.0
    }

    it("should be a subtype of PosZDouble") {
      val pfd: PosFiniteDouble = PosFiniteDouble(1.0)
      def takesPosZDouble(x: PosZDouble): Double = x.value
      takesPosZDouble(pfd) shouldEqual 1.0
    }

    it("should be sortable") {
      val xs = List(PosFiniteDouble(2.2), PosFiniteDouble(4.4), PosFiniteDouble(1.1),
                    PosFiniteDouble(3.3))
      xs.sorted shouldEqual List(PosFiniteDouble(1.1), PosFiniteDouble(2.2), PosFiniteDouble(3.3),
                                 PosFiniteDouble(4.4))
    }
    describe("when created with apply method") {
      it("should compile when 8 is passed in") {
        "PosFiniteDouble(8)" should compile
        PosFiniteDouble(8).value shouldEqual 8.0
        "PosFiniteDouble(8.0F)" should compile
        PosFiniteDouble(8.0F).value shouldEqual 8.0
        "PosFiniteDouble(8.0)" should compile
        PosFiniteDouble(8.0).value shouldEqual 8.0
      }
      it("should not compile when 8L is passed in (precision loss prevention)") {
        "PosFiniteDouble(8L)" shouldNot compile
      }
      it("should not compile when 0 is passed in") {
        "PosFiniteDouble(0)" shouldNot compile
        "PosFiniteDouble(0L)" shouldNot compile
        "PosFiniteDouble(0.0F)" shouldNot compile
        "PosFiniteDouble(0.0)" shouldNot compile
      }
      it("should not compile when -8 is passed in") {
        "PosFiniteDouble(-8)" shouldNot compile
        "PosFiniteDouble(-8L)" shouldNot compile
        "PosFiniteDouble(-8.0F)" shouldNot compile
        "PosFiniteDouble(-8.0)" shouldNot compile
      }
      it("should not compile when a variable is passed in") {
        val a: Int = -8
        "PosFiniteDouble(a)" shouldNot compile
        val b: Long = -8L
        "PosFiniteDouble(b)" shouldNot compile
        val c: Float = -8.0F
        "PosFiniteDouble(c)" shouldNot compile
        val d: Double = -8.0
        "PosFiniteDouble(d)" shouldNot compile
      }
    }

    describe("when specified as a plain-old Double") {
      def takesPosFiniteDouble(pos: PosFiniteDouble): Double = pos.value

      it("should compile when 8 is passed in") {
        "takesPosFiniteDouble(8)" should compile
        takesPosFiniteDouble(8) shouldEqual 8.0
        "takesPosFiniteDouble(8.0F)" should compile
        takesPosFiniteDouble(8.0F) shouldEqual 8.0
        "takesPosFiniteDouble(8.0)" should compile
        takesPosFiniteDouble(8.0) shouldEqual 8.0
      }
      it("should not compile when 8L is passed in (precision loss prevention)") {
        "takesPosFiniteDouble(8L)" shouldNot compile
      }
      it("should not compile when 0 is passed in") {
        "takesPosFiniteDouble(0)" shouldNot compile
        "takesPosFiniteDouble(0L)" shouldNot compile
        "takesPosFiniteDouble(0.0F)" shouldNot compile
        "takesPosFiniteDouble(0.0)" shouldNot compile
      }
      it("should not compile when -8 is passed in") {
        "takesPosFiniteDouble(-8)" shouldNot compile
        "takesPosFiniteDouble(-8L)" shouldNot compile
        "takesPosFiniteDouble(-8.0F)" shouldNot compile
        "takesPosFiniteDouble(-8.0)" shouldNot compile
      }
      it("should not compile when a variable is passed in") {
        val a: Int = -8
        "takesPosFiniteDouble(a)" shouldNot compile
        val b: Long = -8L
        "takesPosFiniteDouble(b)" shouldNot compile
        val c: Float = -8.0F
        "takesPosFiniteDouble(c)" shouldNot compile
        val d: Double = -8.0
        "takesPosFiniteDouble(d)" shouldNot compile
      }
    }

    it("should offer an ensuringValid method that takes a Double => Double, throwing AssertionError if the result is invalid") {
      PosFiniteDouble(33.0).ensuringValid(_ + 1.0) shouldEqual PosFiniteDouble(34.0)
      PosFiniteDouble(33.0).ensuringValid(_ * 2.0) shouldEqual PosFiniteDouble(66.0)
      an [AssertionError] should be thrownBy { PosFiniteDouble.MaxValue.ensuringValid(_ - PosFiniteDouble.MaxValue) }
      an [AssertionError] should be thrownBy { PosFiniteDouble.MaxValue.ensuringValid(_ => Double.PositiveInfinity) }
      an [AssertionError] should be thrownBy { PosFiniteDouble.MaxValue.ensuringValid(_ => Double.NegativeInfinity) }
      an [AssertionError] should be thrownBy { PosFiniteDouble.MaxValue.ensuringValid(_ => 0.0) }
    }

    it("should offer an isFinite method that always returns true") {
      PosFiniteDouble(1.0).isFinite shouldBe true
      PosFiniteDouble(Double.MaxValue).isFinite shouldBe true
    }
  }
  describe("PosFiniteDouble") {
    it("should be automatically widened to compatible AnyVal targets") {
      (PosDoubles.PosFiniteDouble(3.0) : Double) shouldEqual 3.0
      (PosDoubles.PosFiniteDouble(3.0) : PosDoubles.PosZDouble) shouldEqual PosDoubles.PosZDouble(3.0)
      (PosDoubles.PosFiniteDouble(3.0) : PosDoubles.PosZFiniteDouble) shouldEqual PosDoubles.PosZFiniteDouble(3.0)
      "(PosDoubles.PosFiniteDouble(3.0) : Byte)" shouldNot compile
      "(PosDoubles.PosFiniteDouble(3.0) : Short)" shouldNot compile
      "(PosDoubles.PosFiniteDouble(3.0) : Char)" shouldNot compile
      "(PosDoubles.PosFiniteDouble(3.0) : Int)" shouldNot compile
      "(PosDoubles.PosFiniteDouble(3.0) : Long)" shouldNot compile
      "(PosDoubles.PosFiniteDouble(3.0) : Float)" shouldNot compile
      "(PosDoubles.PosFiniteDouble(3.0) : PosInts.PosInt)" shouldNot compile
      "(PosDoubles.PosFiniteDouble(3.0) : PosInts.PosZInt)" shouldNot compile
      "(PosDoubles.PosFiniteDouble(3.0) : NegInts.NegInt)" shouldNot compile
      "(PosDoubles.PosFiniteDouble(3.0) : NegInts.NegZInt)" shouldNot compile
      "(PosDoubles.PosFiniteDouble(3.0) : NonZeroInts.NonZeroInt)" shouldNot compile
      "(PosDoubles.PosFiniteDouble(3.0) : Percentages.PercentageInt)" shouldNot compile
      "(PosDoubles.PosFiniteDouble(3.0) : Numerics.NumericChar)" shouldNot compile
      "(PosDoubles.PosFiniteDouble(3.0) : PosLongs.PosLong)" shouldNot compile
      "(PosDoubles.PosFiniteDouble(3.0) : PosLongs.PosZLong)" shouldNot compile
      "(PosDoubles.PosFiniteDouble(3.0) : NegLongs.NegLong)" shouldNot compile
      "(PosDoubles.PosFiniteDouble(3.0) : NegLongs.NegZLong)" shouldNot compile
      "(PosDoubles.PosFiniteDouble(3.0) : NonZeroLongs.NonZeroLong)" shouldNot compile
      "(PosDoubles.PosFiniteDouble(3.0) : PosFloats.PosFloat)" shouldNot compile
      "(PosDoubles.PosFiniteDouble(3.0) : PosFloats.PosZFloat)" shouldNot compile
      "(PosDoubles.PosFiniteDouble(3.0) : NegFloats.NegFloat)" shouldNot compile
      "(PosDoubles.PosFiniteDouble(3.0) : NegFloats.NegZFloat)" shouldNot compile
      "(PosDoubles.PosFiniteDouble(3.0) : NonZeroFloats.NonZeroFloat)" shouldNot compile
      "(PosDoubles.PosFiniteDouble(3.0) : NonZeroFloats.NonZeroFiniteFloat)" shouldNot compile
      "(PosDoubles.PosFiniteDouble(3.0) : PosFloats.PosFiniteFloat)" shouldNot compile
      "(PosDoubles.PosFiniteDouble(3.0) : PosFloats.PosZFiniteFloat)" shouldNot compile
      "(PosDoubles.PosFiniteDouble(3.0) : NegFloats.NegFiniteFloat)" shouldNot compile
      "(PosDoubles.PosFiniteDouble(3.0) : NegFloats.NegZFiniteFloat)" shouldNot compile
      "(PosDoubles.PosFiniteDouble(3.0) : Finites.FiniteFloat)" shouldNot compile
      "(PosDoubles.PosFiniteDouble(3.0) : PosDoubles.PosDouble)" shouldNot compile
      "(PosDoubles.PosFiniteDouble(3.0) : NegDoubles.NegDouble)" shouldNot compile
      "(PosDoubles.PosFiniteDouble(3.0) : NegDoubles.NegZDouble)" shouldNot compile
      "(PosDoubles.PosFiniteDouble(3.0) : NonZeroDoubles.NonZeroDouble)" shouldNot compile
      "(PosDoubles.PosFiniteDouble(3.0) : NonZeroDoubles.NonZeroFiniteDouble)" shouldNot compile
      "(PosDoubles.PosFiniteDouble(3.0) : NegDoubles.NegFiniteDouble)" shouldNot compile
      "(PosDoubles.PosFiniteDouble(3.0) : NegDoubles.NegZFiniteDouble)" shouldNot compile
      "(PosDoubles.PosFiniteDouble(3.0) : Finites.FiniteDouble)" shouldNot compile
    }
    it("when a compatible AnyVal is passed to a + method invoked on it should give the same AnyVal type back at compile time, and correct value at runtime") {
      PosDoubles.PosFiniteDouble(3.0) + 3.toByte shouldEqual 6.0
      PosDoubles.PosFiniteDouble(3.0) + 3.toShort shouldEqual 6.0
      PosDoubles.PosFiniteDouble(3.0) + '3' shouldEqual 54.0
      PosDoubles.PosFiniteDouble(3.0) + 3 shouldEqual 6.0
      PosDoubles.PosFiniteDouble(3.0) + 3L shouldEqual 6.0
      PosDoubles.PosFiniteDouble(3.0) + 3.0f shouldEqual 6.0
      PosDoubles.PosFiniteDouble(3.0) + 3.0 shouldEqual 6.0
      PosDoubles.PosFiniteDouble(3.0) + PosInts.PosInt(3) shouldEqual 6.0
      PosDoubles.PosFiniteDouble(3.0) + PosInts.PosZInt(3) shouldEqual 6.0
      PosDoubles.PosFiniteDouble(3.0) + NegInts.NegInt(-3) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) + NegInts.NegZInt(-3) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) + NonZeroInts.NonZeroInt(3) shouldEqual 6.0
      PosDoubles.PosFiniteDouble(3.0) + Percentages.PercentageInt(50) shouldEqual 53.0
      PosDoubles.PosFiniteDouble(3.0) + PosLongs.PosLong(3L) shouldEqual 6.0
      PosDoubles.PosFiniteDouble(3.0) + PosLongs.PosZLong(3L) shouldEqual 6.0
      PosDoubles.PosFiniteDouble(3.0) + NegLongs.NegLong(-3L) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) + NegLongs.NegZLong(-3L) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) + NonZeroLongs.NonZeroLong(3L) shouldEqual 6.0
      PosDoubles.PosFiniteDouble(3.0) + PosFloats.PosFloat(3.0f) shouldEqual 6.0
      PosDoubles.PosFiniteDouble(3.0) + PosFloats.PosZFloat(3.0f) shouldEqual 6.0
      PosDoubles.PosFiniteDouble(3.0) + NegFloats.NegFloat(-3.0f) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) + NegFloats.NegZFloat(-3.0f) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) + NonZeroFloats.NonZeroFloat(3.0f) shouldEqual 6.0
      PosDoubles.PosFiniteDouble(3.0) + NonZeroFloats.NonZeroFiniteFloat(3.0f) shouldEqual 6.0
      PosDoubles.PosFiniteDouble(3.0) + PosFloats.PosFiniteFloat(3.0f) shouldEqual 6.0
      PosDoubles.PosFiniteDouble(3.0) + PosFloats.PosZFiniteFloat(3.0f) shouldEqual 6.0
      PosDoubles.PosFiniteDouble(3.0) + NegFloats.NegFiniteFloat(-3.0f) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) + NegFloats.NegZFiniteFloat(-3.0f) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) + Finites.FiniteFloat(3.0f) shouldEqual 6.0
      PosDoubles.PosFiniteDouble(3.0) + PosDoubles.PosDouble(3.0) shouldEqual 6.0
      PosDoubles.PosFiniteDouble(3.0) + NegDoubles.NegDouble(-3.0) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) + NegDoubles.NegZDouble(-3.0) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) + NonZeroDoubles.NonZeroDouble(3.0) shouldEqual 6.0
      PosDoubles.PosFiniteDouble(3.0) + NonZeroDoubles.NonZeroFiniteDouble(3.0) shouldEqual 6.0
      PosDoubles.PosFiniteDouble(3.0) + PosDoubles.PosZFiniteDouble(3.0) shouldEqual 6.0
      PosDoubles.PosFiniteDouble(3.0) + NegDoubles.NegFiniteDouble(-3.0) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) + NegDoubles.NegZFiniteDouble(-3.0) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) + Finites.FiniteDouble(3.0) shouldEqual 6.0
    }
    it("when a compatible AnyVal is passed to a - method invoked on it should give the same AnyVal type back at compile time, and correct value at runtime") {
      PosDoubles.PosFiniteDouble(3.0) - 3.toByte shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) - 3.toShort shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) - '3' shouldEqual -48.0
      PosDoubles.PosFiniteDouble(3.0) - 3 shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) - 3L shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) - 3.0f shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) - 3.0 shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) - PosInts.PosInt(3) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) - PosInts.PosZInt(3) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) - NegInts.NegInt(-3) shouldEqual 6.0
      PosDoubles.PosFiniteDouble(3.0) - NegInts.NegZInt(-3) shouldEqual 6.0
      PosDoubles.PosFiniteDouble(3.0) - NonZeroInts.NonZeroInt(3) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) - Percentages.PercentageInt(50) shouldEqual -47.0
      PosDoubles.PosFiniteDouble(3.0) - PosLongs.PosLong(3L) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) - PosLongs.PosZLong(3L) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) - NegLongs.NegLong(-3L) shouldEqual 6.0
      PosDoubles.PosFiniteDouble(3.0) - NegLongs.NegZLong(-3L) shouldEqual 6.0
      PosDoubles.PosFiniteDouble(3.0) - NonZeroLongs.NonZeroLong(3L) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) - PosFloats.PosFloat(3.0f) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) - PosFloats.PosZFloat(3.0f) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) - NegFloats.NegFloat(-3.0f) shouldEqual 6.0
      PosDoubles.PosFiniteDouble(3.0) - NegFloats.NegZFloat(-3.0f) shouldEqual 6.0
      PosDoubles.PosFiniteDouble(3.0) - NonZeroFloats.NonZeroFloat(3.0f) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) - NonZeroFloats.NonZeroFiniteFloat(3.0f) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) - PosFloats.PosFiniteFloat(3.0f) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) - PosFloats.PosZFiniteFloat(3.0f) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) - NegFloats.NegFiniteFloat(-3.0f) shouldEqual 6.0
      PosDoubles.PosFiniteDouble(3.0) - NegFloats.NegZFiniteFloat(-3.0f) shouldEqual 6.0
      PosDoubles.PosFiniteDouble(3.0) - Finites.FiniteFloat(3.0f) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) - PosDoubles.PosDouble(3.0) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) - NegDoubles.NegDouble(-3.0) shouldEqual 6.0
      PosDoubles.PosFiniteDouble(3.0) - NegDoubles.NegZDouble(-3.0) shouldEqual 6.0
      PosDoubles.PosFiniteDouble(3.0) - NonZeroDoubles.NonZeroDouble(3.0) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) - NonZeroDoubles.NonZeroFiniteDouble(3.0) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) - PosDoubles.PosZFiniteDouble(3.0) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) - NegDoubles.NegFiniteDouble(-3.0) shouldEqual 6.0
      PosDoubles.PosFiniteDouble(3.0) - NegDoubles.NegZFiniteDouble(-3.0) shouldEqual 6.0
      PosDoubles.PosFiniteDouble(3.0) - Finites.FiniteDouble(3.0) shouldEqual 0.0
    }
    it("when a compatible AnyVal is passed to a * method invoked on it should give the same AnyVal type back at compile time, and correct value at runtime") {
      PosDoubles.PosFiniteDouble(3.0) * 3.toByte shouldEqual 9.0
      PosDoubles.PosFiniteDouble(3.0) * 3.toShort shouldEqual 9.0
      PosDoubles.PosFiniteDouble(3.0) * '3' shouldEqual 153.0
      PosDoubles.PosFiniteDouble(3.0) * 3 shouldEqual 9.0
      PosDoubles.PosFiniteDouble(3.0) * 3L shouldEqual 9.0
      PosDoubles.PosFiniteDouble(3.0) * 3.0f shouldEqual 9.0
      PosDoubles.PosFiniteDouble(3.0) * 3.0 shouldEqual 9.0
      PosDoubles.PosFiniteDouble(3.0) * PosInts.PosInt(3) shouldEqual 9.0
      PosDoubles.PosFiniteDouble(3.0) * PosInts.PosZInt(3) shouldEqual 9.0
      PosDoubles.PosFiniteDouble(3.0) * NegInts.NegInt(-3) shouldEqual -9.0
      PosDoubles.PosFiniteDouble(3.0) * NegInts.NegZInt(-3) shouldEqual -9.0
      PosDoubles.PosFiniteDouble(3.0) * NonZeroInts.NonZeroInt(3) shouldEqual 9.0
      PosDoubles.PosFiniteDouble(3.0) * Percentages.PercentageInt(50) shouldEqual 150.0
      PosDoubles.PosFiniteDouble(3.0) * PosLongs.PosLong(3L) shouldEqual 9.0
      PosDoubles.PosFiniteDouble(3.0) * PosLongs.PosZLong(3L) shouldEqual 9.0
      PosDoubles.PosFiniteDouble(3.0) * NegLongs.NegLong(-3L) shouldEqual -9.0
      PosDoubles.PosFiniteDouble(3.0) * NegLongs.NegZLong(-3L) shouldEqual -9.0
      PosDoubles.PosFiniteDouble(3.0) * NonZeroLongs.NonZeroLong(3L) shouldEqual 9.0
      PosDoubles.PosFiniteDouble(3.0) * PosFloats.PosFloat(3.0f) shouldEqual 9.0
      PosDoubles.PosFiniteDouble(3.0) * PosFloats.PosZFloat(3.0f) shouldEqual 9.0
      PosDoubles.PosFiniteDouble(3.0) * NegFloats.NegFloat(-3.0f) shouldEqual -9.0
      PosDoubles.PosFiniteDouble(3.0) * NegFloats.NegZFloat(-3.0f) shouldEqual -9.0
      PosDoubles.PosFiniteDouble(3.0) * NonZeroFloats.NonZeroFloat(3.0f) shouldEqual 9.0
      PosDoubles.PosFiniteDouble(3.0) * NonZeroFloats.NonZeroFiniteFloat(3.0f) shouldEqual 9.0
      PosDoubles.PosFiniteDouble(3.0) * PosFloats.PosFiniteFloat(3.0f) shouldEqual 9.0
      PosDoubles.PosFiniteDouble(3.0) * PosFloats.PosZFiniteFloat(3.0f) shouldEqual 9.0
      PosDoubles.PosFiniteDouble(3.0) * NegFloats.NegFiniteFloat(-3.0f) shouldEqual -9.0
      PosDoubles.PosFiniteDouble(3.0) * NegFloats.NegZFiniteFloat(-3.0f) shouldEqual -9.0
      PosDoubles.PosFiniteDouble(3.0) * Finites.FiniteFloat(3.0f) shouldEqual 9.0
      PosDoubles.PosFiniteDouble(3.0) * PosDoubles.PosDouble(3.0) shouldEqual 9.0
      PosDoubles.PosFiniteDouble(3.0) * NegDoubles.NegDouble(-3.0) shouldEqual -9.0
      PosDoubles.PosFiniteDouble(3.0) * NegDoubles.NegZDouble(-3.0) shouldEqual -9.0
      PosDoubles.PosFiniteDouble(3.0) * NonZeroDoubles.NonZeroDouble(3.0) shouldEqual 9.0
      PosDoubles.PosFiniteDouble(3.0) * NonZeroDoubles.NonZeroFiniteDouble(3.0) shouldEqual 9.0
      PosDoubles.PosFiniteDouble(3.0) * PosDoubles.PosZFiniteDouble(3.0) shouldEqual 9.0
      PosDoubles.PosFiniteDouble(3.0) * NegDoubles.NegFiniteDouble(-3.0) shouldEqual -9.0
      PosDoubles.PosFiniteDouble(3.0) * NegDoubles.NegZFiniteDouble(-3.0) shouldEqual -9.0
      PosDoubles.PosFiniteDouble(3.0) * Finites.FiniteDouble(3.0) shouldEqual 9.0
    }
    it("when a compatible AnyVal is passed to a / method invoked on it should give the same AnyVal type back at compile time, and correct value at runtime") {
      PosDoubles.PosFiniteDouble(3.0) / 3.toByte shouldEqual 1.0
      PosDoubles.PosFiniteDouble(3.0) / 3.toShort shouldEqual 1.0
      PosDoubles.PosFiniteDouble(3.0) / '3' shouldEqual 0.058823529411764705
      PosDoubles.PosFiniteDouble(3.0) / 3 shouldEqual 1.0
      PosDoubles.PosFiniteDouble(3.0) / 3L shouldEqual 1.0
      PosDoubles.PosFiniteDouble(3.0) / 3.0f shouldEqual 1.0
      PosDoubles.PosFiniteDouble(3.0) / 3.0 shouldEqual 1.0
      PosDoubles.PosFiniteDouble(3.0) / PosInts.PosInt(3) shouldEqual 1.0
      PosDoubles.PosFiniteDouble(3.0) / PosInts.PosZInt(3) shouldEqual 1.0
      PosDoubles.PosFiniteDouble(3.0) / NegInts.NegInt(-3) shouldEqual -1.0
      PosDoubles.PosFiniteDouble(3.0) / NegInts.NegZInt(-3) shouldEqual -1.0
      PosDoubles.PosFiniteDouble(3.0) / NonZeroInts.NonZeroInt(3) shouldEqual 1.0
      PosDoubles.PosFiniteDouble(3.0) / Percentages.PercentageInt(50) shouldEqual 0.06
      PosDoubles.PosFiniteDouble(3.0) / PosLongs.PosLong(3L) shouldEqual 1.0
      PosDoubles.PosFiniteDouble(3.0) / PosLongs.PosZLong(3L) shouldEqual 1.0
      PosDoubles.PosFiniteDouble(3.0) / NegLongs.NegLong(-3L) shouldEqual -1.0
      PosDoubles.PosFiniteDouble(3.0) / NegLongs.NegZLong(-3L) shouldEqual -1.0
      PosDoubles.PosFiniteDouble(3.0) / NonZeroLongs.NonZeroLong(3L) shouldEqual 1.0
      PosDoubles.PosFiniteDouble(3.0) / PosFloats.PosFloat(3.0f) shouldEqual 1.0
      PosDoubles.PosFiniteDouble(3.0) / PosFloats.PosZFloat(3.0f) shouldEqual 1.0
      PosDoubles.PosFiniteDouble(3.0) / NegFloats.NegFloat(-3.0f) shouldEqual -1.0
      PosDoubles.PosFiniteDouble(3.0) / NegFloats.NegZFloat(-3.0f) shouldEqual -1.0
      PosDoubles.PosFiniteDouble(3.0) / NonZeroFloats.NonZeroFloat(3.0f) shouldEqual 1.0
      PosDoubles.PosFiniteDouble(3.0) / NonZeroFloats.NonZeroFiniteFloat(3.0f) shouldEqual 1.0
      PosDoubles.PosFiniteDouble(3.0) / PosFloats.PosFiniteFloat(3.0f) shouldEqual 1.0
      PosDoubles.PosFiniteDouble(3.0) / PosFloats.PosZFiniteFloat(3.0f) shouldEqual 1.0
      PosDoubles.PosFiniteDouble(3.0) / NegFloats.NegFiniteFloat(-3.0f) shouldEqual -1.0
      PosDoubles.PosFiniteDouble(3.0) / NegFloats.NegZFiniteFloat(-3.0f) shouldEqual -1.0
      PosDoubles.PosFiniteDouble(3.0) / Finites.FiniteFloat(3.0f) shouldEqual 1.0
      PosDoubles.PosFiniteDouble(3.0) / PosDoubles.PosDouble(3.0) shouldEqual 1.0
      PosDoubles.PosFiniteDouble(3.0) / NegDoubles.NegDouble(-3.0) shouldEqual -1.0
      PosDoubles.PosFiniteDouble(3.0) / NegDoubles.NegZDouble(-3.0) shouldEqual -1.0
      PosDoubles.PosFiniteDouble(3.0) / NonZeroDoubles.NonZeroDouble(3.0) shouldEqual 1.0
      PosDoubles.PosFiniteDouble(3.0) / NonZeroDoubles.NonZeroFiniteDouble(3.0) shouldEqual 1.0
      PosDoubles.PosFiniteDouble(3.0) / PosDoubles.PosZFiniteDouble(3.0) shouldEqual 1.0
      PosDoubles.PosFiniteDouble(3.0) / NegDoubles.NegFiniteDouble(-3.0) shouldEqual -1.0
      PosDoubles.PosFiniteDouble(3.0) / NegDoubles.NegZFiniteDouble(-3.0) shouldEqual -1.0
      PosDoubles.PosFiniteDouble(3.0) / Finites.FiniteDouble(3.0) shouldEqual 1.0
    }
    it("when a compatible AnyVal is passed to a % method invoked on it should give the same AnyVal type back at compile time, and correct value at runtime") {
      PosDoubles.PosFiniteDouble(3.0) % 3.toByte shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) % 3.toShort shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) % '3' shouldEqual 3.0
      PosDoubles.PosFiniteDouble(3.0) % 3 shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) % 3L shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) % 3.0f shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) % 3.0 shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) % PosInts.PosInt(3) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) % PosInts.PosZInt(3) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) % NegInts.NegInt(-3) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) % NegInts.NegZInt(-3) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) % NonZeroInts.NonZeroInt(3) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) % Percentages.PercentageInt(50) shouldEqual 3.0
      PosDoubles.PosFiniteDouble(3.0) % PosLongs.PosLong(3L) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) % PosLongs.PosZLong(3L) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) % NegLongs.NegLong(-3L) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) % NegLongs.NegZLong(-3L) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) % NonZeroLongs.NonZeroLong(3L) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) % PosFloats.PosFloat(3.0f) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) % PosFloats.PosZFloat(3.0f) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) % NegFloats.NegFloat(-3.0f) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) % NegFloats.NegZFloat(-3.0f) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) % NonZeroFloats.NonZeroFloat(3.0f) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) % NonZeroFloats.NonZeroFiniteFloat(3.0f) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) % PosFloats.PosFiniteFloat(3.0f) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) % PosFloats.PosZFiniteFloat(3.0f) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) % NegFloats.NegFiniteFloat(-3.0f) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) % NegFloats.NegZFiniteFloat(-3.0f) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) % Finites.FiniteFloat(3.0f) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) % PosDoubles.PosDouble(3.0) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) % NegDoubles.NegDouble(-3.0) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) % NegDoubles.NegZDouble(-3.0) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) % NonZeroDoubles.NonZeroDouble(3.0) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) % NonZeroDoubles.NonZeroFiniteDouble(3.0) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) % PosDoubles.PosZFiniteDouble(3.0) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) % NegDoubles.NegFiniteDouble(-3.0) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) % NegDoubles.NegZFiniteDouble(-3.0) shouldEqual 0.0
      PosDoubles.PosFiniteDouble(3.0) % Finites.FiniteDouble(3.0) shouldEqual 0.0
    }
    it("should offer '<' comparison that is consistent with Double") {
      (PosDoubles.PosFiniteDouble(3.0) < 3.toByte) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) < 3.toShort) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) < '3') shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) < 3) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) < 3L) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) < 3.0f) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) < 3.0) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) < PosInts.PosInt(3)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) < PosInts.PosZInt(3)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) < NegInts.NegInt(-3)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) < NegInts.NegZInt(-3)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) < NonZeroInts.NonZeroInt(3)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) < Percentages.PercentageInt(50)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) < PosLongs.PosLong(3L)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) < PosLongs.PosZLong(3L)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) < NegLongs.NegLong(-3L)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) < NegLongs.NegZLong(-3L)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) < NonZeroLongs.NonZeroLong(3L)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) < PosFloats.PosFloat(3.0f)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) < PosFloats.PosZFloat(3.0f)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) < NegFloats.NegFloat(-3.0f)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) < NegFloats.NegZFloat(-3.0f)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) < NonZeroFloats.NonZeroFloat(3.0f)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) < NonZeroFloats.NonZeroFiniteFloat(3.0f)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) < PosFloats.PosFiniteFloat(3.0f)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) < PosFloats.PosZFiniteFloat(3.0f)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) < NegFloats.NegFiniteFloat(-3.0f)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) < NegFloats.NegZFiniteFloat(-3.0f)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) < Finites.FiniteFloat(3.0f)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) < PosDoubles.PosDouble(3.0)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) < NegDoubles.NegDouble(-3.0)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) < NegDoubles.NegZDouble(-3.0)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) < NonZeroDoubles.NonZeroDouble(3.0)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) < NonZeroDoubles.NonZeroFiniteDouble(3.0)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) < PosDoubles.PosZFiniteDouble(3.0)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) < NegDoubles.NegFiniteDouble(-3.0)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) < NegDoubles.NegZFiniteDouble(-3.0)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) < Finites.FiniteDouble(3.0)) shouldEqual false
    }
    it("should offer '<=' comparison that is consistent with Double") {
      (PosDoubles.PosFiniteDouble(3.0) <= 3.toByte) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) <= 3.toShort) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) <= '3') shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) <= 3) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) <= 3L) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) <= 3.0f) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) <= 3.0) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) <= PosInts.PosInt(3)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) <= PosInts.PosZInt(3)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) <= NegInts.NegInt(-3)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) <= NegInts.NegZInt(-3)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) <= NonZeroInts.NonZeroInt(3)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) <= Percentages.PercentageInt(50)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) <= PosLongs.PosLong(3L)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) <= PosLongs.PosZLong(3L)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) <= NegLongs.NegLong(-3L)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) <= NegLongs.NegZLong(-3L)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) <= NonZeroLongs.NonZeroLong(3L)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) <= PosFloats.PosFloat(3.0f)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) <= PosFloats.PosZFloat(3.0f)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) <= NegFloats.NegFloat(-3.0f)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) <= NegFloats.NegZFloat(-3.0f)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) <= NonZeroFloats.NonZeroFloat(3.0f)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) <= NonZeroFloats.NonZeroFiniteFloat(3.0f)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) <= PosFloats.PosFiniteFloat(3.0f)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) <= PosFloats.PosZFiniteFloat(3.0f)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) <= NegFloats.NegFiniteFloat(-3.0f)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) <= NegFloats.NegZFiniteFloat(-3.0f)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) <= Finites.FiniteFloat(3.0f)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) <= PosDoubles.PosDouble(3.0)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) <= NegDoubles.NegDouble(-3.0)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) <= NegDoubles.NegZDouble(-3.0)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) <= NonZeroDoubles.NonZeroDouble(3.0)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) <= NonZeroDoubles.NonZeroFiniteDouble(3.0)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) <= PosDoubles.PosZFiniteDouble(3.0)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) <= NegDoubles.NegFiniteDouble(-3.0)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) <= NegDoubles.NegZFiniteDouble(-3.0)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) <= Finites.FiniteDouble(3.0)) shouldEqual true
    }
    it("should offer '>' comparison that is consistent with Double") {
      (PosDoubles.PosFiniteDouble(3.0) > 3.toByte) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) > 3.toShort) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) > '3') shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) > 3) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) > 3L) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) > 3.0f) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) > 3.0) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) > PosInts.PosInt(3)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) > PosInts.PosZInt(3)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) > NegInts.NegInt(-3)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) > NegInts.NegZInt(-3)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) > NonZeroInts.NonZeroInt(3)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) > Percentages.PercentageInt(50)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) > PosLongs.PosLong(3L)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) > PosLongs.PosZLong(3L)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) > NegLongs.NegLong(-3L)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) > NegLongs.NegZLong(-3L)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) > NonZeroLongs.NonZeroLong(3L)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) > PosFloats.PosFloat(3.0f)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) > PosFloats.PosZFloat(3.0f)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) > NegFloats.NegFloat(-3.0f)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) > NegFloats.NegZFloat(-3.0f)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) > NonZeroFloats.NonZeroFloat(3.0f)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) > NonZeroFloats.NonZeroFiniteFloat(3.0f)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) > PosFloats.PosFiniteFloat(3.0f)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) > PosFloats.PosZFiniteFloat(3.0f)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) > NegFloats.NegFiniteFloat(-3.0f)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) > NegFloats.NegZFiniteFloat(-3.0f)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) > Finites.FiniteFloat(3.0f)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) > PosDoubles.PosDouble(3.0)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) > NegDoubles.NegDouble(-3.0)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) > NegDoubles.NegZDouble(-3.0)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) > NonZeroDoubles.NonZeroDouble(3.0)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) > NonZeroDoubles.NonZeroFiniteDouble(3.0)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) > PosDoubles.PosZFiniteDouble(3.0)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) > NegDoubles.NegFiniteDouble(-3.0)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) > NegDoubles.NegZFiniteDouble(-3.0)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) > Finites.FiniteDouble(3.0)) shouldEqual false
    }
    it("should offer '>=' comparison that is consistent with Double") {
      (PosDoubles.PosFiniteDouble(3.0) >= 3.toByte) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) >= 3.toShort) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) >= '3') shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) >= 3) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) >= 3L) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) >= 3.0f) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) >= 3.0) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) >= PosInts.PosInt(3)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) >= PosInts.PosZInt(3)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) >= NegInts.NegInt(-3)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) >= NegInts.NegZInt(-3)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) >= NonZeroInts.NonZeroInt(3)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) >= Percentages.PercentageInt(50)) shouldEqual false
      (PosDoubles.PosFiniteDouble(3.0) >= PosLongs.PosLong(3L)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) >= PosLongs.PosZLong(3L)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) >= NegLongs.NegLong(-3L)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) >= NegLongs.NegZLong(-3L)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) >= NonZeroLongs.NonZeroLong(3L)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) >= PosFloats.PosFloat(3.0f)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) >= PosFloats.PosZFloat(3.0f)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) >= NegFloats.NegFloat(-3.0f)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) >= NegFloats.NegZFloat(-3.0f)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) >= NonZeroFloats.NonZeroFloat(3.0f)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) >= NonZeroFloats.NonZeroFiniteFloat(3.0f)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) >= PosFloats.PosFiniteFloat(3.0f)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) >= PosFloats.PosZFiniteFloat(3.0f)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) >= NegFloats.NegFiniteFloat(-3.0f)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) >= NegFloats.NegZFiniteFloat(-3.0f)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) >= Finites.FiniteFloat(3.0f)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) >= PosDoubles.PosDouble(3.0)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) >= NegDoubles.NegDouble(-3.0)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) >= NegDoubles.NegZDouble(-3.0)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) >= NonZeroDoubles.NonZeroDouble(3.0)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) >= NonZeroDoubles.NonZeroFiniteDouble(3.0)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) >= PosDoubles.PosZFiniteDouble(3.0)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) >= NegDoubles.NegFiniteDouble(-3.0)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) >= NegDoubles.NegZFiniteDouble(-3.0)) shouldEqual true
      (PosDoubles.PosFiniteDouble(3.0) >= Finites.FiniteDouble(3.0)) shouldEqual true
    }
    it("should offer 'min' and 'max' methods that are consistent with Double") {
      PosDoubles.PosFiniteDouble(3.0).max(PosDoubles.PosFiniteDouble(3.0)) shouldEqual PosDoubles.PosFiniteDouble(3.0)
      PosDoubles.PosFiniteDouble(3.0).min(PosDoubles.PosFiniteDouble(3.0)) shouldEqual PosDoubles.PosFiniteDouble(3.0)
    }
    it("should offer widening methods for basic type Double that are consistent with Double") {
      def widen(value: Double): Double = value
      widen(PosDoubles.PosFiniteDouble(3.0)) shouldEqual 3.0
    }
    it("should offer widening methods for basic type PosDoubles.PosZDouble that are consistent with Double") {
      def widen(value: PosDoubles.PosZDouble): PosDoubles.PosZDouble = value
      widen(PosDoubles.PosFiniteDouble(3.0)) shouldEqual PosDoubles.PosZDouble(3.0)
    }
    it("should offer widening methods for basic type PosDoubles.PosZFiniteDouble that are consistent with Double") {
      def widen(value: PosDoubles.PosZFiniteDouble): PosDoubles.PosZFiniteDouble = value
      widen(PosDoubles.PosFiniteDouble(3.0)) shouldEqual PosDoubles.PosZFiniteDouble(3.0)
    }
    it("should have a toString consistent with Double") {
      (PosDoubles.PosFiniteDouble(3.0) : Double).toString shouldBe 3.0.toString
    }
    it("should return the same type from its unary_+ method") {
      +PosDoubles.PosFiniteDouble(3.0) shouldEqual PosDoubles.PosFiniteDouble(3.0)
    }
    it("should offer a unary - method that is consistent with Double") {
      (-PosDoubles.PosFiniteDouble(3.0)) shouldEqual -3.0
    }
  }
}
