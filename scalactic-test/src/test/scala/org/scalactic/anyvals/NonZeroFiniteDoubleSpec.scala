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

trait NonZeroFiniteDoubleSpecSupport {

  implicit def tryEquality[T]: Equality[Try[T]] = new Equality[Try[T]] {
    override def areEqual(a: Try[T], b: Any): Boolean = a match {
      case Success(double: Double) if double.isNaN =>  // This is because in Scala.js x/0 results to NaN not ArithmetricException like in jvm, and we need to make sure Success(NaN) == Success(NaN) is true to pass the test.
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

class NonZeroFiniteDoubleSpec extends FunSpec with Matchers with PropertyChecks with TypeCheckedTripleEquals with NonZeroFiniteDoubleSpecSupport {

  describe("A NonZeroFiniteDouble") {
    describe("should offer a from factory method that") {
      it("returns Some[NonZeroFiniteDouble] if the passed Double is greater than 0") {
        NonZeroFiniteDouble.from(50.23).value.value shouldBe 50.23
        NonZeroFiniteDouble.from(100.0).value.value shouldBe 100.0
      }
      it("returns Some[NonZeroFiniteDouble] if the passed Double is lesser than 0") {
        NonZeroFiniteDouble.from(-0.00001).value.value shouldBe -0.00001
        NonZeroFiniteDouble.from(-99.9).value.value shouldBe -99.9
      }
      it("returns None if the passed Double is 0") {
        NonZeroFiniteDouble.from(0.0) shouldBe None
      }
    }
    describe("should offer an ensuringValid factory method that") {
      it("returns NonZeroFiniteDouble if the passed Double is greater than 0") {
        NonZeroFiniteDouble.ensuringValid(50.23).value shouldBe 50.23
        NonZeroFiniteDouble.ensuringValid(100.0).value shouldBe 100.0
      }
      it("returns NonZeroFiniteDouble if the passed Double is lesser than 0") {
        NonZeroFiniteDouble.ensuringValid(-0.00001).value shouldBe -0.00001
        NonZeroFiniteDouble.ensuringValid(-99.9).value shouldBe -99.9
      }
      it("throws AssertionError if the passed Double is NOT greater than 0 or NaN") {
        an [AssertionError] should be thrownBy NonZeroFiniteDouble.ensuringValid(0.0)
        an [AssertionError] should be thrownBy NonZeroFiniteDouble.ensuringValid(Double.NaN)
      }
      it("throws AssertionError if the passed Double is PositiveInfinity") {
        an [AssertionError] should be thrownBy NonZeroFiniteDouble.ensuringValid(Double.PositiveInfinity)
      }
      it("throws AssertionError if the passed Double is NegativeInfinity") {
        an [AssertionError] should be thrownBy NonZeroFiniteDouble.ensuringValid(Double.NegativeInfinity)
      }
    }
    describe("should offer a tryingValid factory method that") {
      import TryValues._
      it("returns a NonZeroFiniteDouble wrapped in a Success if the passed Double is non-zero") {
        NonZeroFiniteDouble.tryingValid(50.23).success.value.value shouldBe 50.23
        NonZeroFiniteDouble.tryingValid(100.0).success.value.value shouldBe 100
        NonZeroFiniteDouble.tryingValid(-50.23).success.value.value shouldBe -50.23
        NonZeroFiniteDouble.tryingValid(-100.0).success.value.value shouldBe -100.0
      }

      it("returns an AssertionError wrapped in a Failure if the passed Double is NOT non-zero") {
        NonZeroFiniteDouble.tryingValid(0.0).failure.exception shouldBe an[AssertionError]
      }
    }
    describe("should offer a passOrElse factory method that") {
      it("returns a Pass if the given Double is non-zero") {
        NonZeroFiniteDouble.passOrElse(50.23)(i => i) shouldBe Pass
        NonZeroFiniteDouble.passOrElse(100.0)(i => i) shouldBe Pass

        NonZeroFiniteDouble.passOrElse(-1.23)(i => i) shouldBe Pass
        NonZeroFiniteDouble.passOrElse(-99.0)(i => i) shouldBe Pass
      }
      it("returns an error value produced by passing the given Double to the given function if the passed Doule is NOT non-zero, wrapped in a Fail") {
        NonZeroFiniteDouble.passOrElse(0.0)(i => s"$i did not taste good") shouldBe Fail(0.0 + " did not taste good")
      }
    }
    describe("should offer a goodOrElse factory method that") {
      it("returns a NonZeroFloat wrapped in a Good if the given Double is non-zero") {
        NonZeroFiniteDouble.goodOrElse(50.23)(i => i) shouldBe Good(NonZeroFiniteDouble(50.23))
        NonZeroFiniteDouble.goodOrElse(100.0)(i => i) shouldBe Good(NonZeroFiniteDouble(100.0))

        NonZeroFiniteDouble.goodOrElse(-1.23)(i => i) shouldBe Good(NonZeroFiniteDouble(-1.23))
        NonZeroFiniteDouble.goodOrElse(-99.0)(i => i) shouldBe Good(NonZeroFiniteDouble(-99.0))
      }
      it("returns an error value produced by passing the given Double to the given function if the passed Double is NOT non-zero, wrapped in a Bad") {
        NonZeroFiniteDouble.goodOrElse(0.0)(i => s"$i did not taste good") shouldBe Bad(0.0 + " did not taste good")
      }
    }
    describe("should offer a rightOrElse factory method that") {
      it("returns a NonZeroFiniteDouble wrapped in a Right if the given Double is non-zero") {
        NonZeroFiniteDouble.rightOrElse(50.23)(i => i) shouldBe Right(NonZeroFiniteDouble(50.23))
        NonZeroFiniteDouble.rightOrElse(100.0)(i => i) shouldBe Right(NonZeroFiniteDouble(100.0))

        NonZeroFiniteDouble.rightOrElse(-1.23)(i => i) shouldBe Right(NonZeroFiniteDouble(-1.23))
        NonZeroFiniteDouble.rightOrElse(-99.0)(i => i) shouldBe Right(NonZeroFiniteDouble(-99.0))
      }
      it("returns an error value produced by passing the given Double to the given function if the passed Double is NOT non-zero, wrapped in a Left") {
        NonZeroFiniteDouble.rightOrElse(0.0)(i => s"$i did not taste good") shouldBe Left(0.0 + " did not taste good")
      }
    }
    describe("should offer an isValid predicate method that") {
      it("returns true if the passed Double is greater than 0") {
        NonZeroFiniteDouble.isValid(50.23) shouldBe true
        NonZeroFiniteDouble.isValid(100.0) shouldBe true
        NonZeroFiniteDouble.isValid(0.0) shouldBe false
        NonZeroFiniteDouble.isValid(-0.0) shouldBe false
        NonZeroFiniteDouble.isValid(-0.00001) shouldBe true
        NonZeroFiniteDouble.isValid(-99.9) shouldBe true
      }
    }
    describe("should offer a fromOrElse factory method that") {
      it("returns a NonZeroFiniteDouble if the passed Double is greater than 0") {
        NonZeroFiniteDouble.fromOrElse(50.23, NonZeroFiniteDouble(42.0)).value shouldBe 50.23
        NonZeroFiniteDouble.fromOrElse(100.0, NonZeroFiniteDouble(42.0)).value shouldBe 100.0
      }
      it("returns a NonZeroFiniteDouble if the passed Double is lesser than 0") {
        NonZeroFiniteDouble.fromOrElse(-0.00001, NonZeroFiniteDouble(42.0)).value shouldBe -0.00001
        NonZeroFiniteDouble.fromOrElse(-99.9, NonZeroFiniteDouble(42.0)).value shouldBe -99.9
      }
      it("returns a given default if the passed Double is 0 or NaN") {
        NonZeroFiniteDouble.fromOrElse(0.0, NonZeroFiniteDouble(42.0)).value shouldBe 42.0
        NonZeroFiniteDouble.fromOrElse(Double.NaN, NonZeroFiniteDouble(42.0)).value shouldBe 42.0
      }
    }
    it("should offer MaxValue, MinValue, and MinPositiveValue factory methods") {
      NonZeroFiniteDouble.MaxValue shouldEqual NonZeroFiniteDouble.from(Double.MaxValue).get
      NonZeroFiniteDouble.MinValue shouldEqual
        NonZeroFiniteDouble.from(Double.MinValue).get
      NonZeroFiniteDouble.MinPositiveValue shouldEqual
        NonZeroFiniteDouble.from(Double.MinPositiveValue).get
    }
    it("should not offer a PositiveInfinity factory method") {
      "NonZeroFiniteDouble.PositiveInfinity" shouldNot compile
    }
    it("should not offer a NegativeInfinity factory method") {
      "NonZeroFiniteDouble.NegativeInfinity" shouldNot compile
    }
/* These now compile because of the new implicit widening conversion from NonZeroFiniteDouble to NonZeroDouble.
    NonZeroFiniteDouble(-1.0).isNegInfinity
    NonZeroFiniteDouble(-1.0).isPosInfinity
*/

    it("should be sortable") {
      val xs = List(NonZeroFiniteDouble(2.2), NonZeroFiniteDouble(4.4), NonZeroFiniteDouble(1.1),
        NonZeroFiniteDouble(3.3))
      xs.sorted shouldEqual List(NonZeroFiniteDouble(1.1), NonZeroFiniteDouble(2.2), NonZeroFiniteDouble(3.3),
        NonZeroFiniteDouble(4.4))
    }

    describe("when created with apply method") {

      it("should compile when 8 is passed in") {
        "NonZeroFiniteDouble(8)" should compile
        NonZeroFiniteDouble(8).value shouldEqual 8.0
        "NonZeroFiniteDouble(8L)" should compile
        NonZeroFiniteDouble(8L).value shouldEqual 8.0
        "NonZeroFiniteDouble(8.0F)" should compile
        NonZeroFiniteDouble(8.0F).value shouldEqual 8.0
        "NonZeroFiniteDouble(8.0)" should compile
        NonZeroFiniteDouble(8.0).value shouldEqual 8.0
      }

      it("should not compile when 0 is passed in") {
        "NonZeroFiniteDouble(0)" shouldNot compile
        "NonZeroFiniteDouble(0L)" shouldNot compile
        "NonZeroFiniteDouble(0.0F)" shouldNot compile
        "NonZeroFiniteDouble(0.0)" shouldNot compile
      }

      it("should compile when -8 is passed in") {
        "NonZeroFiniteDouble(-8)" should compile
        NonZeroFiniteDouble(-8).value shouldEqual -8.0
        "NonZeroFiniteDouble(-8L)" should compile
        NonZeroFiniteDouble(-8L).value shouldEqual -8.0
        "NonZeroFiniteDouble(-8.0F)" should compile
        NonZeroFiniteDouble(-8.0F).value shouldEqual -8.0
        "NonZeroFiniteDouble(-8.0)" should compile
        NonZeroFiniteDouble(-8.0).value shouldEqual -8.0
      }
      it("should not compile when x is passed in") {
        val a: Int = -8
        "NonZeroFiniteDouble(a)" shouldNot compile
        val b: Long = -8L
        "NonZeroFiniteDouble(b)" shouldNot compile
        val c: Float = -8.0F
        "NonZeroFiniteDouble(c)" shouldNot compile
        val d: Double = -8.0
        "NonZeroFiniteDouble(d)" shouldNot compile
      }
    }
    describe("when specified as a plain-old Double") {

      def takesNonZeroFiniteDouble(pos: NonZeroFiniteDouble): Double = pos.value

      it("should compile when 8 is passed in") {
        "takesNonZeroFiniteDouble(8)" should compile
        takesNonZeroFiniteDouble(8) shouldEqual 8.0
        "takesNonZeroFiniteDouble(8L)" should compile
        takesNonZeroFiniteDouble(8L) shouldEqual 8.0
        "takesNonZeroFiniteDouble(8.0F)" should compile
        takesNonZeroFiniteDouble(8.0F) shouldEqual 8.0
        "takesNonZeroFiniteDouble(8.0)" should compile
        takesNonZeroFiniteDouble(8.0) shouldEqual 8.0
      }

      it("should not compile when 0 is passed in") {
        "takesNonZeroFiniteDouble(0)" shouldNot compile
        "takesNonZeroFiniteDouble(0L)" shouldNot compile
        "takesNonZeroFiniteDouble(0.0F)" shouldNot compile
        "takesNonZeroFiniteDouble(0.0)" shouldNot compile
      }

      it("should compile when -8 is passed in") {
        "takesNonZeroFiniteDouble(-8)" should compile
        takesNonZeroFiniteDouble(-8) shouldEqual -8.0
        "takesNonZeroFiniteDouble(-8L)" should compile
        takesNonZeroFiniteDouble(-8L) shouldEqual -8.0
        "takesNonZeroFiniteDouble(-8.0F)" should compile
        takesNonZeroFiniteDouble(-8.0F) shouldEqual -8.0
        "takesNonZeroFiniteDouble(-8.0)" should compile
        takesNonZeroFiniteDouble(-8.0) shouldEqual -8.0
      }

      it("should not compile when x is passed in") {
        val x: Int = -8
        "takesNonZeroFiniteDouble(x)" shouldNot compile
        val b: Long = -8L
        "takesNonZeroFiniteDouble(b)" shouldNot compile
        val c: Float = -8.0F
        "takesNonZeroFiniteDouble(c)" shouldNot compile
        val d: Double = -8.0
        "takesNonZeroFiniteDouble(d)" shouldNot compile
      }
    }

    it("should offer a unary + method that is consistent with Double") {
      forAll { (p: NonZeroFiniteDouble) =>
        (+p).toDouble shouldEqual (+(p.toDouble))
      }
    }

    it("should offer a unary - method that returns NonZeroFiniteDouble") {
      forAll { (p: NonZeroFiniteDouble) =>
        (-p) shouldEqual (NonZeroFiniteDouble.ensuringValid(-(p.toDouble)))
      }
    }

    it("should offer 'min' and 'max' methods that are consistent with Double") {
      forAll { (pdouble1: NonZeroFiniteDouble, pdouble2: NonZeroFiniteDouble) =>
        pdouble1.max(pdouble2).toDouble shouldEqual pdouble1.toDouble.max(pdouble2.toDouble)
        pdouble1.min(pdouble2).toDouble shouldEqual pdouble1.toDouble.min(pdouble2.toDouble)
      }
    }

    it("should offer an 'isWhole' method that is consistent with Double") {
      forAll { (pdouble: NonZeroFiniteDouble) =>
        pdouble.isWhole shouldEqual pdouble.toDouble.isWhole
      }
    }

    it("should offer 'toRadians' and 'toDegrees' methods that are consistent with Double") {
      forAll { (pdouble: NonZeroFiniteDouble) =>
        pdouble.toRadians shouldEqual pdouble.toDouble.toRadians
      }
    }

    it("should offer widening methods for basic types that are consistent with Double") {
      forAll { (pdouble: NonZeroFiniteDouble) =>
        def widen(value: Double): Double = value
        widen(pdouble) shouldEqual widen(pdouble.toDouble)
      }
    }

    it("should offer an ensuringValid method that takes a Double => Double, throwing AssertionError if the result is invalid") {
      NonZeroFiniteDouble(33.0).ensuringValid(_ + 1.0) shouldEqual NonZeroFiniteDouble(34.0)
      NonZeroFiniteDouble(-33.0).ensuringValid(_ + 1.0) shouldEqual NonZeroFiniteDouble(-32.0)
      an [AssertionError] should be thrownBy { NonZeroFiniteDouble.MaxValue.ensuringValid(_ - NonZeroFiniteDouble.MaxValue) }
      an [AssertionError] should be thrownBy { NonZeroFiniteDouble.MaxValue.ensuringValid(_ => Double.NaN) }
      an [AssertionError] should be thrownBy { NonZeroFiniteDouble.MaxValue.ensuringValid(_ => Double.PositiveInfinity) }
      an [AssertionError] should be thrownBy { NonZeroFiniteDouble.MaxValue.ensuringValid(_ => Double.NegativeInfinity) }
    }
  }
}
