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
import org.scalactic.Equality
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.prop.PropertyChecks
// SKIP-SCALATESTJS,NATIVE-START
import scala.collection.immutable.NumericRange
// SKIP-SCALATESTJS,NATIVE-END
import scala.collection.mutable.WrappedArray
import OptionValues._
import scala.util.{Failure, Success, Try}
import org.scalatest.Inspectors
import org.scalactic.{Good, Bad}
import org.scalactic.{Pass, Fail}

trait NegFiniteDoubleSpecSupport {

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

class NegFiniteDoubleSpec extends funspec.AnyFunSpec with matchers.should.Matchers with PropertyChecks with TypeCheckedTripleEquals with NegFiniteDoubleSpecSupport {

  describe("A NegFiniteDouble") {
    describe("should offer a from factory method that") {
      it("returns Some[NegFiniteDouble] if the passed Double is lesser than 0") {
        NegFiniteDouble.from(-50.23).value.value shouldBe -50.23
        NegFiniteDouble.from(-100.0).value.value shouldBe -100.0
      }
      it("returns None if the passed Double is NOT lesser than 0") {
        NegFiniteDouble.from(0.0) shouldBe None
        NegFiniteDouble.from(0.00001) shouldBe None
        NegFiniteDouble.from(99.9) shouldBe None
      }
    }
    describe("should offer an ensuringValid factory method that") {
      it("returns NegFiniteDouble if the passed Double is lesser than 0") {
        NegFiniteDouble.ensuringValid(-50.23).value shouldBe -50.23
        NegFiniteDouble.ensuringValid(-100.0).value shouldBe -100.0
      }
      it("throws AssertionError if the passed Double is NOT lesser than 0") {
        an [AssertionError] should be thrownBy NegFiniteDouble.ensuringValid(0.0)
        an [AssertionError] should be thrownBy NegFiniteDouble.ensuringValid(0.00001)
        an [AssertionError] should be thrownBy NegFiniteDouble.ensuringValid(99.9)
        an [AssertionError] should be thrownBy NegFiniteDouble.ensuringValid(Double.PositiveInfinity)
        an [AssertionError] should be thrownBy NegFiniteDouble.ensuringValid(Double.NegativeInfinity)
        // SKIP-DOTTY-START
        // https://github.com/lampepfl/dotty/issues/6710
        an [AssertionError] should be thrownBy NegFiniteDouble.ensuringValid(Double.NaN)
        // SKIP-DOTTY-END
      }
    }
    describe("should offer a tryingValid factory method that") {
      import TryValues._
      it("returns a NegFiniteDouble wrapped in a Success if the passed NegFiniteDouble is lesser than 0") {
        NegFiniteDouble.tryingValid(-50.3).success.value.value shouldBe -50.3
        NegFiniteDouble.tryingValid(-100.0).success.value.value shouldBe -100.0
      }

      it("returns an AssertionError wrapped in a Failure if the passed Double is NOT lesser than 0") {
        NegFiniteDouble.tryingValid(0.0).failure.exception shouldBe an [AssertionError]
        NegFiniteDouble.tryingValid(1.0).failure.exception shouldBe an [AssertionError]
        NegFiniteDouble.tryingValid(99.9).failure.exception shouldBe an [AssertionError]
      }
    }
    describe("should offer a passOrElse factory method that") {
      it("returns a Pass if the given Double is lesser than 0") {
        NegFiniteDouble.passOrElse(-50.0)(i => i) shouldBe Pass
        NegFiniteDouble.passOrElse(-100.0)(i => i) shouldBe Pass
      }
      it("returns an error value produced by passing the given Double to the given function if the passed Double is NOT lesser than 0, wrapped in a Fail") {
        NegFiniteDouble.passOrElse(0.0)(i => s"$i did not taste good") shouldBe Fail(0.0 + " did not taste good")
        NegFiniteDouble.passOrElse(1.1)(i => i) shouldBe Fail(1.1)
        NegFiniteDouble.passOrElse(99.0)(i => i + 3.0) shouldBe Fail(102.0)
      }
    }
    describe("should offer a goodOrElse factory method that") {
      it("returns a NegFiniteDouble wrapped in a Good if the given Double is lesser than 0") {
        NegFiniteDouble.goodOrElse(-50.3)(i => i) shouldBe Good(NegFiniteDouble(-50.3))
        NegFiniteDouble.goodOrElse(-100.0)(i => i) shouldBe Good(NegFiniteDouble(-100.0))
      }
      it("returns an error value produced by passing the given Double to the given function if the passed Double is NOT lesser than 0, wrapped in a Bad") {
        NegFiniteDouble.goodOrElse(0.0)(i => s"$i did not taste good") shouldBe Bad(0.0 + " did not taste good")
        NegFiniteDouble.goodOrElse(1.1)(i => i) shouldBe Bad(1.1)
        NegFiniteDouble.goodOrElse(99.0)(i => i + 3.0) shouldBe Bad(102.0)
      }
    }
    describe("should offer a rightOrElse factory method that") {
      it("returns a NegFiniteDouble wrapped in a Right if the given Double is lesser than 0") {
        NegFiniteDouble.rightOrElse(-50.3)(i => i) shouldBe Right(NegFiniteDouble(-50.3))
        NegFiniteDouble.rightOrElse(-100.0)(i => i) shouldBe Right(NegFiniteDouble(-100.0))
      }
      it("returns an error value produced by passing the given Double to the given function if the passed Double is NOT lesser than 0, wrapped in a Left") {
        NegFiniteDouble.rightOrElse(0.0)(i => s"$i did not taste good") shouldBe Left(0.0 + " did not taste good")
        NegFiniteDouble.rightOrElse(1.1)(i => i) shouldBe Left(1.1)
        NegFiniteDouble.rightOrElse(99.9)(i => i + 3.0) shouldBe Left(102.9)
      }
    }
    describe("should offer an isValid predicate method that") {
      it("returns true if the passed Double is lesser than 0") {
        NegFiniteDouble.isValid(-50.23) shouldBe true
        NegFiniteDouble.isValid(-100.0) shouldBe true
        NegFiniteDouble.isValid(0.0) shouldBe false
        NegFiniteDouble.isValid(-0.0) shouldBe false
        NegFiniteDouble.isValid(0.00001) shouldBe false
        NegFiniteDouble.isValid(99.9) shouldBe false
      }
    }
    describe("should offer a fromOrElse factory method that") {
      it("returns a NegFiniteDouble if the passed Double is lesser than 0") {
        NegFiniteDouble.fromOrElse(-50.23, NegFiniteDouble(-42.0)).value shouldBe -50.23
        NegFiniteDouble.fromOrElse(-100.0, NegFiniteDouble(-42.0)).value shouldBe -100.0
      }
      it("returns a given default if the passed Double is NOT lesser than 0") {
        NegFiniteDouble.fromOrElse(0.0, NegFiniteDouble(-42.0)).value shouldBe -42.0
        NegFiniteDouble.fromOrElse(0.00001, NegFiniteDouble(-42.0)).value shouldBe -42.0
        NegFiniteDouble.fromOrElse(99.9, NegFiniteDouble(-42.0)).value shouldBe -42.0
      }
    }
    it("should offer MaxValue and MinValue factory methods") {
      NegFiniteDouble.MaxValue shouldEqual NegFiniteDouble.from(-Double.MinPositiveValue).get
      NegFiniteDouble.MinValue shouldEqual
        NegFiniteDouble.from(Double.MinValue).get
    }
    it("should not offer a PositiveInfinity factory method") {
      "NegFiniteDouble.PositiveInfinity" shouldNot compile
    }
    it("should not offer a NegativeInfinity factory method") {
      "NegFiniteDouble.NegativeInfinity" shouldNot compile
    }
    it("should offer a isNegInfinity method that returns true if the instance is NegativeInfinity") {
      "NegFiniteDouble(-1.0).isNegInfinity" shouldNot compile
    }
    it("should be sortable") {
      val xs = List(NegFiniteDouble(-2.2), NegFiniteDouble(-4.4), NegFiniteDouble(-1.1),
        NegFiniteDouble(-3.3))
      xs.sorted shouldEqual List(NegFiniteDouble(-4.4), NegFiniteDouble(-3.3), NegFiniteDouble(-2.2),
        NegFiniteDouble(-1.1))
    }

    describe("when created with apply method") {

      it("should compile when -8 is passed in") {
        "NegFiniteDouble(-8)" should compile
        NegFiniteDouble(-8).value shouldEqual -8.0
        "NegFiniteDouble(-8L)" should compile
        NegFiniteDouble(-8L).value shouldEqual -8.0
        "NegFiniteDouble(-8.0F)" should compile
        NegFiniteDouble(-8.0F).value shouldEqual -8.0
        "NegFiniteDouble(-8.0)" should compile
        NegFiniteDouble(-8.0).value shouldEqual -8.0
      }

      it("should not compile when 0 is passed in") {
        "NegFiniteDouble(0)" shouldNot compile
        "NegFiniteDouble(0L)" shouldNot compile
        "NegFiniteDouble(0.0F)" shouldNot compile
        "NegFiniteDouble(0.0)" shouldNot compile
      }

      it("should not compile when 8 is passed in") {
        "NegFiniteDouble(8)" shouldNot compile
        "NegFiniteDouble(8L)" shouldNot compile
        "NegFiniteDouble(8.0F)" shouldNot compile
        "NegFiniteDouble(8.0)" shouldNot compile
      }
      it("should not compile when x is passed in") {
        val a: Int = 8
        "NegFiniteDouble(a)" shouldNot compile
        val b: Long = 8L
        "NegFiniteDouble(b)" shouldNot compile
        val c: Float = 8.0F
        "NegFiniteDouble(c)" shouldNot compile
        val d: Double = 8.0
        "NegFiniteDouble(d)" shouldNot compile
      }
    }
    describe("when specified as a plain-old Double") {

      def takesNegFiniteDouble(pos: NegFiniteDouble): Double = pos.value

      it("should compile when -8 is passed in") {
        "takesNegFiniteDouble(-8)" should compile
        takesNegFiniteDouble(-8) shouldEqual -8.0
        "takesNegFiniteDouble(-8L)" should compile
        takesNegFiniteDouble(-8L) shouldEqual -8.0
        "takesNegFiniteDouble(-8.0F)" should compile
        takesNegFiniteDouble(-8.0F) shouldEqual -8.0
        "takesNegFiniteDouble(-8.0)" should compile
        takesNegFiniteDouble(-8.0) shouldEqual -8.0
      }

      it("should not compile when 0 is passed in") {
        "takesNegFiniteDouble(0)" shouldNot compile
        "takesNegFiniteDouble(0L)" shouldNot compile
        "takesNegFiniteDouble(0.0F)" shouldNot compile
        "takesNegFiniteDouble(0.0)" shouldNot compile
      }

      it("should not compile when 8 is passed in") {
        "takesNegFiniteDouble(8)" shouldNot compile
        "takesNegFiniteDouble(8L)" shouldNot compile
        "takesNegFiniteDouble(8.0F)" shouldNot compile
        "takesNegFiniteDouble(8.0)" shouldNot compile
      }

      it("should not compile when x is passed in") {
        val x: Int = -8
        "takesNegFiniteDouble(x)" shouldNot compile
        val b: Long = -8L
        "takesNegFiniteDouble(b)" shouldNot compile
        val c: Float = -8.0F
        "takesNegFiniteDouble(c)" shouldNot compile
        val d: Double = -8.0
        "takesNegFiniteDouble(d)" shouldNot compile
      }

      it("should offer a unary + method that is consistent with Double") {
        forAll { (p: NegFiniteDouble) =>
          (+p).toDouble shouldEqual (+(p.toDouble))
        }
      }

      it("should offer a unary - method that returns PosFiniteDouble") {
        forAll { (p: NegFiniteDouble) =>
          (-p) shouldEqual (PosFiniteDouble.ensuringValid(-(p.toDouble)))
        }
      }
    }

    it("should offer 'min' and 'max' methods that are consistent with Double") {
      forAll { (pdouble1: NegFiniteDouble, pdouble2: NegFiniteDouble) =>
        pdouble1.max(pdouble2).toDouble shouldEqual pdouble1.toDouble.max(pdouble2.toDouble)
        pdouble1.min(pdouble2).toDouble shouldEqual pdouble1.toDouble.min(pdouble2.toDouble)
      }
    }

    it("should offer an 'isWhole' method that is consistent with Double") {
      forAll { (pdouble: NegFiniteDouble) =>
        pdouble.isWhole shouldEqual pdouble.toDouble.isWhole
      }
    }

    it("should offer 'round', 'ceil', and 'floor' methods that are consistent with Double") {
      forAll { (pdouble: NegFiniteDouble) =>
        pdouble.round.toDouble shouldEqual pdouble.toDouble.round
        pdouble.ceil.toDouble shouldEqual pdouble.toDouble.ceil
        pdouble.floor.toDouble shouldEqual pdouble.toDouble.floor
      }
    }

    it("should offer 'toRadians' and 'toDegrees' methods that are consistent with Double") {
      forAll { (pdouble: NegFiniteDouble) =>
        pdouble.toRadians shouldEqual pdouble.toDouble.toRadians
      }
    }

    it("should offer widening methods for basic types that are consistent with Double") {
      forAll { (pdouble: NegFiniteDouble) =>
        def widen(value: Double): Double = value
        widen(pdouble) shouldEqual widen(pdouble.toDouble)
      }
      forAll { (pdouble: NegFiniteDouble) =>
        def widen(value: NegZFiniteDouble): NegZFiniteDouble = value
        widen(pdouble) shouldEqual widen(NegZFiniteDouble.from(pdouble.toDouble).get)
      }
      forAll { (pdouble: NegFiniteDouble) =>
        def widen(value: NonZeroDouble): NonZeroDouble = value
        widen(pdouble) shouldEqual widen(NonZeroDouble.from(pdouble.toDouble).get)
      }
    }

    it("should offer an ensuringValid method that takes a Double => Double, throwing AssertionError if the result is invalid") {
      NegFiniteDouble(-33.0).ensuringValid(_ + 1.0) shouldEqual NegFiniteDouble(-32.0)
      an [AssertionError] should be thrownBy { NegFiniteDouble.MaxValue.ensuringValid(_ - NegFiniteDouble.MaxValue) }
      an [AssertionError] should be thrownBy { NegFiniteDouble.MaxValue.ensuringValid(_ => Double.PositiveInfinity) }
      an [AssertionError] should be thrownBy { NegFiniteDouble.MaxValue.ensuringValid(_ => Double.NegativeInfinity) }
      // SKIP-DOTTY-START
      // https://github.com/lampepfl/dotty/issues/6710
      an [AssertionError] should be thrownBy { NegFiniteDouble.MaxValue.ensuringValid(_ => Double.NaN) }
      // SKIP-DOTTY-END
    }
  }
}