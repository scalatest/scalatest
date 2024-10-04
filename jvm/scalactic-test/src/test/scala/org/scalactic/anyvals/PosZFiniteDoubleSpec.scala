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
import org.scalatest.prop.PropertyChecks
// SKIP-SCALATESTJS,NATIVE-START
import scala.collection.immutable.NumericRange
// SKIP-SCALATESTJS,NATIVE-END
import OptionValues._
import scala.collection.mutable.WrappedArray
//import org.scalactic.StrictCheckedEquality
import Double.NaN
import org.scalactic.Equality
import org.scalactic.{Pass, Fail}
import org.scalactic.{Good, Bad}
import scala.util.{Try, Success, Failure}

trait PosZFiniteDoubleSpecSupport {

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

class PosZFiniteDoubleSpec extends funspec.AnyFunSpec with matchers.should.Matchers with PropertyChecks with PosZFiniteDoubleSpecSupport {

  describe("A PosZFiniteDouble") {
    describe("should offer a from factory method that") {
      it("returns Some[PosZFiniteDouble] if the passed Double is greater than or equal to 0") {
        PosZFiniteDouble.from(0.0).value.value shouldBe 0.0
        PosZFiniteDouble.from(50.23).value.value shouldBe 50.23
        PosZFiniteDouble.from(100.0).value.value shouldBe 100.0
      }
      it("returns None if the passed Double is NOT greater than or equal to 0") {
        PosZFiniteDouble.from(-0.00001) shouldBe None
        PosZFiniteDouble.from(-99.9) shouldBe None
      }
    }
    describe("should offer an ensuringValid factory method that") {
      it("returns PosZFiniteDouble if the passed Double is greater than or equal to 0") {
        PosZFiniteDouble.ensuringValid(0.0).value shouldBe 0.0
        PosZFiniteDouble.ensuringValid(50.23).value shouldBe 50.23
        PosZFiniteDouble.ensuringValid(100.0).value shouldBe 100.0
      }
      it("throws AssertionError if the passed Double is NOT greater than or equal to 0") {
        an [AssertionError] should be thrownBy PosZFiniteDouble.ensuringValid(-0.00001)
        an [AssertionError] should be thrownBy PosZFiniteDouble.ensuringValid(-99.9)
        an [AssertionError] should be thrownBy PosZFiniteDouble.ensuringValid(Double.PositiveInfinity)
        an [AssertionError] should be thrownBy PosZFiniteDouble.ensuringValid(Double.NegativeInfinity)
        // SKIP-DOTTY-START
        // https://github.com/lampepfl/dotty/issues/6710
        an [AssertionError] should be thrownBy PosZFiniteDouble.ensuringValid(Double.NaN)
        // SKIP-DOTTY-END
      }
    }
    describe("should offer a tryingValid factory method that") {
      import TryValues._
      it("returns a PosZFiniteDouble wrapped in a Success if the passed Double is greater than or equal 0") {
        PosZFiniteDouble.tryingValid(0.0).success.value.value shouldBe 0.0
        PosZFiniteDouble.tryingValid(50.0).success.value.value shouldBe 50.0
        PosZFiniteDouble.tryingValid(100.0f).success.value.value shouldBe 100.0
      }

      it("returns an AssertionError wrapped in a Failure if the passed Double is lesser than 0") {
        PosZFiniteDouble.tryingValid(-1.0).failure.exception shouldBe an [AssertionError]
        PosZFiniteDouble.tryingValid(-99.0).failure.exception shouldBe an [AssertionError]
      }
    }
    describe("should offer a passOrElse factory method that") {
      it("returns a Pass if the given Double is greater than or equal 0") {
        PosZFiniteDouble.passOrElse(0.0)(i => i) shouldBe Pass
        PosZFiniteDouble.passOrElse(50.0)(i => i) shouldBe Pass
        PosZFiniteDouble.passOrElse(100.0)(i => i) shouldBe Pass
      }
      it("returns an error value produced by passing the given Double to the given function if the passed Double is lesser than 0, wrapped in a Fail") {
        PosZFiniteDouble.passOrElse(-1.0)(i => i) shouldBe Fail(-1.0)
        PosZFiniteDouble.passOrElse(-99.0)(i => i + 3.0) shouldBe Fail(-96.0)
      }
    }
    describe("should offer a goodOrElse factory method that") {
      it("returns a PosZFiniteDouble wrapped in a Good if the given Double is greater than or equal 0") {
        PosZFiniteDouble.goodOrElse(0.0)(i => i) shouldBe Good(PosZFiniteDouble(0.0))
        PosZFiniteDouble.goodOrElse(50.0)(i => i) shouldBe Good(PosZFiniteDouble(50.0))
        PosZFiniteDouble.goodOrElse(100.0)(i => i) shouldBe Good(PosZFiniteDouble(100.0))
      }
      it("returns an error value produced by passing the given Double to the given function if the passed Double is lesser than 0, wrapped in a Bad") {
        PosZFiniteDouble.goodOrElse(-1.0)(i => i) shouldBe Bad(-1.0)
        PosZFiniteDouble.goodOrElse(-99.0)(i => i + 3.0f) shouldBe Bad(-96.0)
      }
    }
    describe("should offer a rightOrElse factory method that") {
      it("returns a PosZFiniteDouble wrapped in a Right if the given Double is greater than or equal 0") {
        PosZFiniteDouble.rightOrElse(0.0)(i => i) shouldBe Right(PosZFiniteDouble(0.0))
        PosZFiniteDouble.rightOrElse(50.0)(i => i) shouldBe Right(PosZFiniteDouble(50.0))
        PosZFiniteDouble.rightOrElse(100.0)(i => i) shouldBe Right(PosZFiniteDouble(100.0))
      }
      it("returns an error value produced by passing the given Double to the given function if the passed Double is lesser than 0, wrapped in a Left") {
        PosZFiniteDouble.rightOrElse(-1.0)(i => i) shouldBe Left(-1.0)
        PosZFiniteDouble.rightOrElse(-99.0)(i => i + 3.0f) shouldBe Left(-96.0)
      }
    }
    describe("should offer an isValid predicate method that") {
      it("returns true if the passed Double is greater than or equal to 0") {
        PosZFiniteDouble.isValid(50.23) shouldBe true
        PosZFiniteDouble.isValid(100.0) shouldBe true
        PosZFiniteDouble.isValid(0.0) shouldBe true
        PosZFiniteDouble.isValid(-0.0) shouldBe true
        PosZFiniteDouble.isValid(-0.00001) shouldBe false
        PosZFiniteDouble.isValid(-99.9) shouldBe false
      }
    }
    describe("should offer a fromOrElse factory method that") {
      it("returns a PosZFiniteDouble if the passed Double is greater than or equal to 0") {
        PosZFiniteDouble.fromOrElse(50.23, PosZFiniteDouble(42.0)).value shouldBe 50.23
        PosZFiniteDouble.fromOrElse(100.0, PosZFiniteDouble(42.0)).value shouldBe 100.0
        PosZFiniteDouble.fromOrElse(0.0, PosZFiniteDouble(42.0)).value shouldBe 0.0
      }
      it("returns a given default if the passed Double is NOT greater than or equal to 0") {
        PosZFiniteDouble.fromOrElse(-0.00001, PosZFiniteDouble(42.0)).value shouldBe 42.0
        PosZFiniteDouble.fromOrElse(-99.9, PosZFiniteDouble(42.0)).value shouldBe 42.0
      }
    }
    it("should offer MaxValue, MinValue, and MinPositiveValue factory methods") {
      PosZFiniteDouble.MaxValue shouldEqual PosZFiniteDouble.from(Double.MaxValue).get
      PosZFiniteDouble.MinValue shouldEqual PosZFiniteDouble(0.0)
      PosZFiniteDouble.MinPositiveValue shouldEqual
        PosZFiniteDouble.from(Double.MinPositiveValue).get
    }
    it("should not offer a PositiveInfinity factory method") {
      "PosZFiniteDouble.PositiveInfinity" shouldNot compile
    }
    it("should not offer a NegativeInfinity factory method") {
      "PosZFiniteDouble.NegativeInfinity" shouldNot compile
    }
/* This now compiles because of the newly added implicit PosZFiniteDouble => PosZDouble method
    PosZFiniteDouble(1.0f).isPosInfinity
*/
    it("should not offer a isNegInfinity method") {
      "PosZFiniteDouble(1.0f).isNegInfinity" shouldNot compile
    }

    it("should be sortable") {
      val xs = List(PosZFiniteDouble(2.2), PosZFiniteDouble(0.0), PosZFiniteDouble(1.1),
        PosZFiniteDouble(3.3))
      xs.sorted shouldEqual List(PosZFiniteDouble(0.0), PosZFiniteDouble(1.1),
        PosZFiniteDouble(2.2), PosZFiniteDouble(3.3))
    }

    describe("when created with apply method") {

      it("should compile when 8 is passed in") {
        "PosZFiniteDouble(8)" should compile
        PosZFiniteDouble(8).value shouldEqual 8.0
        "PosZFiniteDouble(8L)" should compile
        PosZFiniteDouble(8L).value shouldEqual 8.0
        "PosZFiniteDouble(8.0F)" should compile
        PosZFiniteDouble(8.0F).value shouldEqual 8.0
        "PosZFiniteDouble(8.0)" should compile
        PosZFiniteDouble(8.0).value shouldEqual 8.0
      }

      it("should compile when 0 is passed in") {
        "PosZFiniteDouble(0)" should compile
        PosZFiniteDouble(0).value shouldEqual 0.0
        "PosZFiniteDouble(0L)" should compile
        PosZFiniteDouble(0L).value shouldEqual 0.0
        "PosZFiniteDouble(0.0F)" should compile
        PosZFiniteDouble(0.0F).value shouldEqual 0.0
        "PosZFiniteDouble(0.0)" should compile
        PosZFiniteDouble(0.0).value shouldEqual 0.0
      }

      it("should not compile when -8 is passed in") {
        "PosZFiniteDouble(-8)" shouldNot compile
        "PosZFiniteDouble(-8L)" shouldNot compile
        "PosZFiniteDouble(-8.0F)" shouldNot compile
        "PosZFiniteDouble(-8.0)" shouldNot compile
      }
      it("should not compile when x is passed in") {
        val a: Int = -8
        "PosZFiniteDouble(a)" shouldNot compile
        val b: Long = -8L
        "PosZFiniteDouble(b)" shouldNot compile
        val c: Float = -8.0F
        "PosZFiniteDouble(c)" shouldNot compile
        val d: Double = -8.0
        "PosZFiniteDouble(d)" shouldNot compile
      }
    }
    describe("when specified as a plain-old Double") {

      def takesPosZFiniteDouble(poz: PosZFiniteDouble): Double = poz.value

      it("should compile when 8 is passed in") {
        "takesPosZFiniteDouble(8)" should compile
        takesPosZFiniteDouble(8) shouldEqual 8.0
        "takesPosZFiniteDouble(8L)" should compile
        takesPosZFiniteDouble(8L) shouldEqual 8.0
        "takesPosZFiniteDouble(8.0F)" should compile
        takesPosZFiniteDouble(8.0F) shouldEqual 8.0
        "takesPosZFiniteDouble(8.0)" should compile
        takesPosZFiniteDouble(8.0) shouldEqual 8.0
      }

      it("should compile when 0 is passed in") {
        "takesPosZFiniteDouble(0)" should compile
        takesPosZFiniteDouble(0) shouldEqual 0.0
        "takesPosZFiniteDouble(0L)" should compile
        takesPosZFiniteDouble(0L) shouldEqual 0.0
        "takesPosZFiniteDouble(0.0F)" should compile
        takesPosZFiniteDouble(0.0F) shouldEqual 0.0
        "takesPosZFiniteDouble(0.0)" should compile
        takesPosZFiniteDouble(0.0) shouldEqual 0.0
      }

      it("should not compile when -8 is passed in") {
        "takesPosZFiniteDouble(-8)" shouldNot compile
        "takesPosZFiniteDouble(-8L)" shouldNot compile
        "takesPosZFiniteDouble(-8.0F)" shouldNot compile
        "takesPosZFiniteDouble(-8.0)" shouldNot compile
      }

      it("should not compile when x is passed in") {
        val x: Int = -8
        "takesPosZFiniteDouble(x)" shouldNot compile
        val b: Long = -8L
        "takesPosZFiniteDouble(b)" shouldNot compile
        val c: Float = -8.0F
        "takesPosZFiniteDouble(c)" shouldNot compile
        val d: Double = -8.0
        "takesPosZFiniteDouble(d)" shouldNot compile
      }
    }

    it("should offer a unary + method that is consistent with Double") {
      forAll { (p: PosZFiniteDouble) =>
        (+p).toDouble shouldEqual (+(p.toDouble))
      }
    }

    it("should offer a unary - method that returns NegZFiniteDouble") {
      forAll { (p: PosZFiniteDouble) =>
        (-p) shouldEqual (NegZFiniteDouble.ensuringValid(-(p.toDouble)))
      }
    }

    it("should offer 'min' and 'max' methods that are consistent with Double") {
      forAll { (pzdouble1: PosZFiniteDouble, pzdouble2: PosZFiniteDouble) =>
        pzdouble1.max(pzdouble2).toDouble shouldEqual pzdouble1.toDouble.max(pzdouble2.toDouble)
        pzdouble1.min(pzdouble2).toDouble shouldEqual pzdouble1.toDouble.min(pzdouble2.toDouble)
      }
    }

    it("should offer an 'isWhole' method that is consistent with Double") {
      forAll { (pzdouble: PosZFiniteDouble) =>
        pzdouble.isWhole shouldEqual pzdouble.toDouble.isWhole
      }
    }

    it("should offer 'round', 'ceil', and 'floor' methods that are consistent with Double") {
      forAll { (pzdouble: PosZFiniteDouble) =>
        pzdouble.round.toDouble shouldEqual pzdouble.toDouble.round
        pzdouble.ceil.toDouble shouldEqual pzdouble.toDouble.ceil
        pzdouble.floor.toDouble shouldEqual pzdouble.toDouble.floor
      }
    }

    it("should offer 'toRadians' and 'toDegrees' methods that are consistent with Double") {
      forAll { (pzdouble: PosZFiniteDouble) =>
        pzdouble.toRadians shouldEqual pzdouble.toDouble.toRadians
      }
    }

    it("should offer widening methods for basic types that are consistent with Double") {
      forAll { (pzdouble: PosZFiniteDouble) =>
        def widen(value: Double): Double = value
        widen(pzdouble) shouldEqual widen(pzdouble.toDouble)
      }
    }
    it("should offer an ensuringValid method that takes a Double => Double, throwing AssertionError if the result is invalid") {
      PosZFiniteDouble(33.0).ensuringValid(_ + 1.0) shouldEqual PosZFiniteDouble(34.0)
      an [AssertionError] should be thrownBy { PosZFiniteDouble.MaxValue.ensuringValid(_ - PosZFiniteDouble.MaxValue - 1) }
      an [AssertionError] should be thrownBy { PosZFiniteDouble.MaxValue.ensuringValid(_ => Double.PositiveInfinity) }
      an [AssertionError] should be thrownBy { PosZFiniteDouble.MaxValue.ensuringValid(_ => Double.NegativeInfinity) }
      // SKIP-DOTTY-START
      // https://github.com/lampepfl/dotty/issues/6710
      an [AssertionError] should be thrownBy { PosZFiniteDouble.MaxValue.ensuringValid(_ => Double.NaN) }
      // SKIP-DOTTY-END
    }
  }
}
