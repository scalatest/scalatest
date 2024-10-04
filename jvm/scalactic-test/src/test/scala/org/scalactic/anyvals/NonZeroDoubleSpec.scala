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
import org.scalactic.{Pass, Fail}
import org.scalactic.{Good, Bad}

trait NonZeroDoubleSpecSupport {

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

class NonZeroDoubleSpec extends funspec.AnyFunSpec with matchers.should.Matchers with PropertyChecks with TypeCheckedTripleEquals with NonZeroDoubleSpecSupport {

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
    describe("should offer a tryingValid factory method that") {
      import TryValues._
      it("returns a NonZeroDouble wrapped in a Success if the passed Double is non-zero") {
        NonZeroDouble.tryingValid(50.23).success.value.value shouldBe 50.23
        NonZeroDouble.tryingValid(100.0).success.value.value shouldBe 100
        NonZeroDouble.tryingValid(-50.23).success.value.value shouldBe -50.23
        NonZeroDouble.tryingValid(-100.0).success.value.value shouldBe -100.0
      }

      it("returns an AssertionError wrapped in a Failure if the passed Double is NOT non-zero") {
        NonZeroDouble.tryingValid(0.0).failure.exception shouldBe an[AssertionError]
      }
    }
    describe("should offer a passOrElse factory method that") {
      it("returns a Pass if the given Double is non-zero") {
        NonZeroDouble.passOrElse(50.23)(i => i) shouldBe Pass
        NonZeroDouble.passOrElse(100.0)(i => i) shouldBe Pass

        NonZeroDouble.passOrElse(-1.23)(i => i) shouldBe Pass
        NonZeroDouble.passOrElse(-99.0)(i => i) shouldBe Pass
      }
      it("returns an error value produced by passing the given Double to the given function if the passed Doule is NOT non-zero, wrapped in a Fail") {
        NonZeroDouble.passOrElse(0.0)(i => s"$i did not taste good") shouldBe Fail(0.0 + " did not taste good")
      }
    }
    describe("should offer a goodOrElse factory method that") {
      it("returns a NonZeroFloat wrapped in a Good if the given Double is non-zero") {
        NonZeroDouble.goodOrElse(50.23)(i => i) shouldBe Good(NonZeroDouble(50.23))
        NonZeroDouble.goodOrElse(100.0)(i => i) shouldBe Good(NonZeroDouble(100.0))

        NonZeroDouble.goodOrElse(-1.23)(i => i) shouldBe Good(NonZeroDouble(-1.23))
        NonZeroDouble.goodOrElse(-99.0)(i => i) shouldBe Good(NonZeroDouble(-99.0))
      }
      it("returns an error value produced by passing the given Double to the given function if the passed Double is NOT non-zero, wrapped in a Bad") {
        NonZeroDouble.goodOrElse(0.0)(i => s"$i did not taste good") shouldBe Bad(0.0 + " did not taste good")
      }
    }
    describe("should offer a rightOrElse factory method that") {
      it("returns a NonZeroDouble wrapped in a Right if the given Double is non-zero") {
        NonZeroDouble.rightOrElse(50.23)(i => i) shouldBe Right(NonZeroDouble(50.23))
        NonZeroDouble.rightOrElse(100.0)(i => i) shouldBe Right(NonZeroDouble(100.0))

        NonZeroDouble.rightOrElse(-1.23)(i => i) shouldBe Right(NonZeroDouble(-1.23))
        NonZeroDouble.rightOrElse(-99.0)(i => i) shouldBe Right(NonZeroDouble(-99.0))
      }
      it("returns an error value produced by passing the given Double to the given function if the passed Double is NOT non-zero, wrapped in a Left") {
        NonZeroDouble.rightOrElse(0.0)(i => s"$i did not taste good") shouldBe Left(0.0 + " did not taste good")
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
    it("should offer a isNegInfinity method that returns true if the instance is NegativeInfinity") {
      NonZeroDouble.ensuringValid(Double.NegativeInfinity).isNegInfinity shouldBe true
      NonZeroDouble(-1.0).isNegInfinity shouldBe false
    }
    it("should offer a isPosInfinity method that returns true if the instance is PositiveInfinity") {
      NonZeroDouble.ensuringValid(Double.PositiveInfinity).isPosInfinity shouldBe true
      NonZeroDouble(-1.0).isPosInfinity shouldBe false
    }

    it("should be sortable") {
      val xs = List(NonZeroDouble(2.2), NonZeroDouble(4.4), NonZeroDouble(1.1),
        NonZeroDouble(3.3))
      xs.sorted shouldEqual List(NonZeroDouble(1.1), NonZeroDouble(2.2), NonZeroDouble(3.3),
        NonZeroDouble(4.4))
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
      forAll { (p: NonZeroDouble) =>
        (+p).toDouble shouldEqual (+(p.toDouble))
      }
    }

    it("should offer a unary - method that returns NonZeroDouble") {
      forAll { (p: NonZeroDouble) =>
        (-p) shouldEqual (NonZeroDouble.ensuringValid(-(p.toDouble)))
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
    it("should offer an isFinite method that returns true if the value does not represent infinity") {
      forAll { (n: NonZeroFiniteDouble) =>
        (n: NonZeroDouble).isFinite should be (true)
        NonZeroDouble.NegativeInfinity.isFinite should be (false)
        NonZeroDouble.PositiveInfinity.isFinite should be (false)
      }
    }
    it("should offer an isInfinite method that returns true if the value represents positive or negative infinity") {
      forAll { (n: NonZeroFiniteDouble) =>
        (n: NonZeroDouble).isInfinite should be (false)
        NonZeroDouble.NegativeInfinity.isInfinite should be (true)
        NonZeroDouble.PositiveInfinity.isInfinite should be (true)
      }
    }
  }
}
