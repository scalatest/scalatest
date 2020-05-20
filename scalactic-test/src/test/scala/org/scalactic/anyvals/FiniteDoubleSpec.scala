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

trait FiniteDoubleSpecSupport {

  implicit val doubleEquality: Equality[Double] =
    new Equality[Double] {
      override def areEqual(a: Double, b: Any): Boolean =
        (a, b) match {
          case (a, bDouble: Double) if a.isNaN && bDouble.isNaN  => true
          case _ => a == b
        }
    }

  implicit val finiteDoubleEquality: Equality[FiniteDouble] =
    new Equality[FiniteDouble] {
      override def areEqual(a: FiniteDouble, b: Any): Boolean =
        (a, b) match {
          case (a, bDouble: FiniteDouble) if a.value.isNaN && bDouble.value.isNaN  => true
          case _ => a == b
        }
    }

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

class FiniteDoubleSpec extends FunSpec with Matchers with PropertyChecks with TypeCheckedTripleEquals with FiniteDoubleSpecSupport {

  describe("A FiniteDouble") {
    describe("should offer a from factory method that") {
      it("returns Some[FiniteDouble] if the passed Double is finite") {
        FiniteDouble.from(50.23).value.value shouldBe 50.23
        FiniteDouble.from(100.0).value.value shouldBe 100.0
        FiniteDouble.from(0.0).value.value shouldBe 0.0
        FiniteDouble.from(-0.00001).value.value shouldBe -0.00001
        FiniteDouble.from(-99.9).value.value shouldBe -99.9
        FiniteDouble.from(Double.MinPositiveValue).value.value shouldBe Double.MinPositiveValue
      }
      it("returns None if the passed Double is infinite") {
        FiniteDouble.from(Double.NegativeInfinity) shouldBe None
        FiniteDouble.from(Double.PositiveInfinity) shouldBe None
        FiniteDouble.from(Double.NaN) shouldBe None
      }
    }
    describe("should offer an ensuringValid factory method that") {
      it("returns FiniteDouble if the passed Double is finite") {
        FiniteDouble.ensuringValid(50.23).value shouldBe 50.23
        FiniteDouble.ensuringValid(100.0).value shouldBe 100.0
        FiniteDouble.ensuringValid(0.0).value shouldBe 0.0
        FiniteDouble.ensuringValid(-0.00001).value shouldBe -0.00001
        FiniteDouble.ensuringValid(-99.9).value shouldBe -99.9
        FiniteDouble.ensuringValid(Double.MinPositiveValue).value shouldBe Double.MinPositiveValue
      }
      it("throws AssertionError if the passed Double is infinite") {
        an [AssertionError] should be thrownBy FiniteDouble.ensuringValid(Double.PositiveInfinity)
        an [AssertionError] should be thrownBy FiniteDouble.ensuringValid(Double.NegativeInfinity)
        an [AssertionError] should be thrownBy FiniteDouble.ensuringValid(Double.NaN)
      }
    }
    describe("should offer a tryingValid factory method that") {
      import TryValues._
      it("returns a FiniteDouble wrapped in a Success if the passed FiniteDouble is finite") {
        FiniteDouble.tryingValid(50.3).success.value.value shouldBe 50.3
        FiniteDouble.tryingValid(100.0).success.value.value shouldBe 100.0
        FiniteDouble.tryingValid(0.0).success.value.value shouldBe 0.0
        FiniteDouble.tryingValid(-1.0).success.value.value shouldBe -1.0
        FiniteDouble.tryingValid(-99.9).success.value.value shouldBe -99.9
        FiniteDouble.tryingValid(Double.MinPositiveValue).success.value.value shouldBe Double.MinPositiveValue
      }

      it("returns an AssertionError wrapped in a Failure if the passed Double is infinite") {
        FiniteDouble.tryingValid(Double.NegativeInfinity).failure.exception shouldBe an [AssertionError]
        FiniteDouble.tryingValid(Double.PositiveInfinity).failure.exception shouldBe an [AssertionError]
        FiniteDouble.tryingValid(Double.NaN).failure.exception shouldBe an [AssertionError]
      }
    }
    describe("should offer a passOrElse factory method that") {
      it("returns a Pass if the given Double is finite") {
        FiniteDouble.passOrElse(50.0)(i => i) shouldBe Pass
        FiniteDouble.passOrElse(100.0)(i => i) shouldBe Pass
        FiniteDouble.passOrElse(0.0)(i => i) shouldBe Pass
        FiniteDouble.passOrElse(-1.1)(i => i) shouldBe Pass
        FiniteDouble.passOrElse(-99.0)(i => i) shouldBe Pass
        FiniteDouble.passOrElse(Double.MinPositiveValue)(i => i) shouldBe Pass
      }
      it("returns an error value produced by passing the given Double to the given function if the passed Double is infinite, wrapped in a Fail") {
        FiniteDouble.passOrElse(Double.NegativeInfinity)(i => s"$i did not taste good") shouldBe Fail("-Infinity did not taste good")
        FiniteDouble.passOrElse(Double.PositiveInfinity)(i => i) shouldBe Fail(Double.PositiveInfinity)
        FiniteDouble.passOrElse(Double.NaN)(i => 1.1) shouldBe Fail(1.1)
      }
    }
    describe("should offer a goodOrElse factory method that") {
      it("returns a FiniteDouble wrapped in a Good if the given Double is greater than 0") {
        FiniteDouble.goodOrElse(50.3)(i => i) shouldBe Good(FiniteDouble(50.3))
        FiniteDouble.goodOrElse(100.0)(i => i) shouldBe Good(FiniteDouble(100.0))
        FiniteDouble.goodOrElse(0.0)(i => i) shouldBe Good(FiniteDouble(0.0))
        FiniteDouble.goodOrElse(-1.1)(i => i) shouldBe Good(FiniteDouble(-1.1))
        FiniteDouble.goodOrElse(-99.0)(i => i) shouldBe Good(FiniteDouble(-99.0))
        // SKIP-DOTTY-START
        // not constant literal
        FiniteDouble.goodOrElse(Double.MinPositiveValue)(i => i) shouldBe Good(FiniteDouble(Double.MinPositiveValue))
        // SKIP-DOTTY-END
      }
      it("returns an error value produced by passing the given Double to the given function if the passed Double is NOT greater than 0, wrapped in a Bad") {
        FiniteDouble.goodOrElse(Double.NegativeInfinity)(i => s"$i did not taste good") shouldBe Bad("-Infinity did not taste good")
        FiniteDouble.goodOrElse(Double.PositiveInfinity)(i => i) shouldBe Bad(Double.PositiveInfinity)
        FiniteDouble.goodOrElse(Double.NaN)(i => s"$i did not taste good") shouldBe Bad("NaN did not taste good")
      }
    }
    describe("should offer a rightOrElse factory method that") {
      it("returns a FiniteDouble wrapped in a Right if the given Double is greater than 0") {
        FiniteDouble.rightOrElse(50.3)(i => i) shouldBe Right(FiniteDouble(50.3))
        FiniteDouble.rightOrElse(100.0)(i => i) shouldBe Right(FiniteDouble(100.0))
        FiniteDouble.rightOrElse(0.0)(i => i) shouldBe Right(FiniteDouble(0.0))
        FiniteDouble.rightOrElse(-1.1)(i => i) shouldBe Right(FiniteDouble(-1.1))
        FiniteDouble.rightOrElse(-99.9)(i => i) shouldBe Right(FiniteDouble(-99.9))
        // SKIP-DOTTY-START
        // not constant literal
        FiniteDouble.rightOrElse(Double.MinPositiveValue)(i => i) shouldBe Right(FiniteDouble(Double.MinPositiveValue))
        // SKIP-DOTTY-END
      }
      it("returns an error value produced by passing the given Double to the given function if the passed Double is infinite, wrapped in a Left") {
        FiniteDouble.rightOrElse(Double.NegativeInfinity)(i => s"$i did not taste good") shouldBe Left("-Infinity did not taste good")
        FiniteDouble.rightOrElse(Double.PositiveInfinity)(i => i) shouldBe Left(Double.PositiveInfinity)
        FiniteDouble.rightOrElse(Double.NaN)(i => s"$i did not taste good") shouldBe Left("NaN did not taste good")
      }
    }
    describe("should offer an isValid predicate method that") {
      it("returns true if the passed Double is finite") {
        FiniteDouble.isValid(50.23) shouldBe true
        FiniteDouble.isValid(100.0) shouldBe true
        FiniteDouble.isValid(0.0) shouldBe true
        FiniteDouble.isValid(-0.0) shouldBe true
        FiniteDouble.isValid(-0.00001) shouldBe true
        FiniteDouble.isValid(-99.9) shouldBe true
        FiniteDouble.isValid(Double.NaN) shouldBe false
        FiniteDouble.isValid(Double.MinPositiveValue) shouldBe true
        FiniteDouble.isValid(Double.NegativeInfinity) shouldBe false
        FiniteDouble.isValid(Double.PositiveInfinity) shouldBe false
      }
    }
    describe("should offer a fromOrElse factory method that") {
      it("returns a FiniteDouble if the passed Double is finite") {
        FiniteDouble.fromOrElse(50.23, FiniteDouble(42.0)).value shouldBe 50.23
        FiniteDouble.fromOrElse(100.0, FiniteDouble(42.0)).value shouldBe 100.0
        FiniteDouble.fromOrElse(0.0, FiniteDouble(42.0)).value shouldBe 0.0
        FiniteDouble.fromOrElse(-0.00001, FiniteDouble(42.0)).value shouldBe -0.00001
        FiniteDouble.fromOrElse(-99.9, FiniteDouble(42.0)).value shouldBe -99.9
        FiniteDouble.fromOrElse(Double.MinPositiveValue, FiniteDouble(42.0)).value shouldBe Double.MinPositiveValue
      }
      it("returns a given default if the passed Double is infinite") {
        FiniteDouble.fromOrElse(Double.NegativeInfinity, FiniteDouble(42.0)).value shouldBe 42.0
        FiniteDouble.fromOrElse(Double.PositiveInfinity, FiniteDouble(42.0)).value shouldBe 42.0
        FiniteDouble.fromOrElse(Double.NaN, FiniteDouble(42.0)).value shouldBe 42.0
      }
    }
    it("should offer MaxValue, MinValue, and MinPositiveValue factory methods") {
      FiniteDouble.MaxValue shouldEqual FiniteDouble.from(Double.MaxValue).get
      FiniteDouble.MinValue shouldEqual FiniteDouble.from(Double.MinValue).get
      FiniteDouble.MinPositiveValue shouldEqual FiniteDouble.from(Double.MinPositiveValue).get
    }
    it("should not offer a PositiveInfinity factory method") {
      "FiniteDouble.PositiveInfinity" shouldNot compile
    }
    it("should not offer a NegativeInfinity factory method") {
      "FiniteDouble.NegativeInfinity" shouldNot compile
    }
    it("should not offer a isPosInfinity method") {
      "FiniteDouble(1.0).isPosInfinity" shouldNot compile
    }
    it("should not offer a isNegInfinity method") {
      "FiniteDouble(1.0).isNegInfinity" shouldNot compile
    }

    it("should be sortable") {
      val xs = List(FiniteDouble(2.2), FiniteDouble(4.4), FiniteDouble(1.1),
        FiniteDouble(3.3))
      xs.sorted shouldEqual List(FiniteDouble(1.1), FiniteDouble(2.2), FiniteDouble(3.3),
        FiniteDouble(4.4))
    }

    describe("when created with apply method") {

      it("should compile when 8 is passed in") {
        "FiniteDouble(8)" should compile
        FiniteDouble(8).value shouldEqual 8.0
        "FiniteDouble(8L)" should compile
        FiniteDouble(8L).value shouldEqual 8.0
        "FiniteDouble(8.0F)" should compile
        FiniteDouble(8.0F).value shouldEqual 8.0
        "FiniteDouble(8.0)" should compile
        FiniteDouble(8.0).value shouldEqual 8.0
      }

      it("should compile when 0 is passed in") {
        "FiniteDouble(0)" should compile
        FiniteDouble(0).value shouldEqual 0
        "FiniteDouble(0L)" should compile
        FiniteDouble(0L).value shouldEqual 0.0
        "FiniteDouble(0.0F)" should compile
        FiniteDouble(0.0F).value shouldEqual 0.0
        "FiniteDouble(0.0)" should compile
        FiniteDouble(0.0).value shouldEqual 0.0
      }

      it("should compile when -8 is passed in") {
        "FiniteDouble(-8)" should compile
        FiniteDouble(-8).value shouldEqual -8.0
        "FiniteDouble(-8L)" should compile
        FiniteDouble(-8L).value shouldEqual -8.0
        "FiniteDouble(-8.0F)" should compile
        FiniteDouble(-8.0F).value shouldEqual -8.0
        "FiniteDouble(-8.0)" should compile
        FiniteDouble(-8.0).value shouldEqual -8.0
      }

      it("should not compile when Double.NegativeInfinity is passed in") {
        "FiniteDouble(Double.NegativeInfinity)" shouldNot compile
      }

      it("should not compile when Double.PositiveInfinity is passed in") {
        "FiniteDouble(Double.PositiveInfinity)" shouldNot compile
      }

      it("should not compile when x is passed in") {
        val a: Int = -8
        "FiniteDouble(a)" shouldNot compile
        val b: Long = -8L
        "FiniteDouble(b)" shouldNot compile
        val c: Float = -8.0F
        "FiniteDouble(c)" shouldNot compile
        val d: Double = -8.0
        "FiniteDouble(d)" shouldNot compile
      }

      it("should offer a unary + method that is consistent with Double") {
        forAll { (pDouble: FiniteDouble) =>
          (+pDouble).toDouble shouldEqual (+(pDouble.toDouble))
        }
      }

      it("should offer a unary - method that returns another FiniteDouble") {
        forAll { (pDouble: FiniteDouble) =>
          (-pDouble) shouldEqual (FiniteDouble.ensuringValid(-(pDouble.toDouble)))
        }
      }
    }
    describe("when specified as a plain-old Double") {

      def takesFiniteDouble(pos: FiniteDouble): Double = pos.value

      it("should compile when 8 is passed in") {
        "takesFiniteDouble(8)" should compile
        takesFiniteDouble(8) shouldEqual 8.0
        "takesFiniteDouble(8L)" should compile
        takesFiniteDouble(8L) shouldEqual 8.0
        "takesFiniteDouble(8.0F)" should compile
        takesFiniteDouble(8.0F) shouldEqual 8.0
        "takesFiniteDouble(8.0)" should compile
        takesFiniteDouble(8.0) shouldEqual 8.0
      }

      it("should compile when 0 is passed in") {
        "takesFiniteDouble(0)" should compile
        takesFiniteDouble(0) shouldEqual 0.0
        "takesFiniteDouble(0L)" should compile
        takesFiniteDouble(0L) shouldEqual 0.0
        "takesFiniteDouble(0.0F)" should compile
        takesFiniteDouble(0.0F) shouldEqual 0.0
        "takesFiniteDouble(0.0)" should compile
        takesFiniteDouble(0.0) shouldEqual 0.0
      }

      it("should compile when -8 is passed in") {
        "takesFiniteDouble(-8)" should compile
        takesFiniteDouble(-8) shouldEqual -8.0
        "takesFiniteDouble(-8L)" should compile
        takesFiniteDouble(-8L) shouldEqual -8.0
        "takesFiniteDouble(-8.0F)" should compile
        takesFiniteDouble(-8.0F) shouldEqual -8.0
        "takesFiniteDouble(-8.0)" should compile
        takesFiniteDouble(-8.0) shouldEqual -8.0
      }

      it("should not compile when 0 is passed in") {
        "takesFiniteDouble(Double.NegativeInfinity)" shouldNot compile
        "takesFiniteDouble(Double.PositiveInfinity)" shouldNot compile
      }

      it("should not compile when x is passed in") {
        val x: Int = -8
        "takesFiniteDouble(x)" shouldNot compile
        val b: Long = -8L
        "takesFiniteDouble(b)" shouldNot compile
        val c: Float = -8.0F
        "takesFiniteDouble(c)" shouldNot compile
        val d: Double = -8.0
        "takesFiniteDouble(d)" shouldNot compile
      }
    }

    it("should offer 'min' and 'max' methods that are consistent with Double") {
      forAll { (pdouble1: FiniteDouble, pdouble2: FiniteDouble) =>
        pdouble1.max(pdouble2).toDouble shouldEqual pdouble1.toDouble.max(pdouble2.toDouble)
        pdouble1.min(pdouble2).toDouble shouldEqual pdouble1.toDouble.min(pdouble2.toDouble)
      }
    }

    it("should offer an 'isWhole' method that is consistent with Double") {
      forAll { (pdouble: FiniteDouble) =>
        pdouble.isWhole shouldEqual pdouble.toDouble.isWhole
      }
    }

    it("should offer 'round', 'ceil', and 'floor' methods that are consistent with Double") {
      forAll { (pdouble: FiniteDouble) =>
        pdouble.round.toDouble shouldEqual pdouble.toDouble.round
        pdouble.ceil.toDouble shouldEqual pdouble.toDouble.ceil
        pdouble.floor.toDouble shouldEqual pdouble.toDouble.floor
      }
    }

    it("should offer 'toRadians' and 'toDegrees' methods that are consistent with Double") {
      forAll { (pdouble: FiniteDouble) =>
        pdouble.toRadians shouldEqual pdouble.toDouble.toRadians
      }
    }

    it("should offer an ensuringValid method that takes a Double => Double, throwing AssertionError if the result is invalid") {
      FiniteDouble(33.0).ensuringValid(_ + 1.0) shouldEqual FiniteDouble(34.0)
      FiniteDouble(0.0).ensuringValid(_ + Double.MinValue) shouldEqual FiniteDouble.MinValue
      FiniteDouble(0.0).ensuringValid(_ + Double.MaxValue) shouldEqual FiniteDouble.MaxValue
      FiniteDouble(0.0).ensuringValid(_ + Double.MinPositiveValue) shouldEqual FiniteDouble.MinPositiveValue
      an [AssertionError] should be thrownBy { FiniteDouble.MaxValue.ensuringValid(_ => Double.PositiveInfinity) }
      an [AssertionError] should be thrownBy { FiniteDouble.MaxValue.ensuringValid(_ => Double.NegativeInfinity) }
    }
  }
}

