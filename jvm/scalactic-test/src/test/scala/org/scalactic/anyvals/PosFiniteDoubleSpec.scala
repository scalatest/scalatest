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

trait PosFiniteDoubleSpecSupport {

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

class PosFiniteDoubleSpec extends funspec.AnyFunSpec with matchers.should.Matchers with PropertyChecks with TypeCheckedTripleEquals with PosFiniteDoubleSpecSupport {

  describe("A PosFiniteDouble") {
    describe("should offer a from factory method that") {
      it("returns Some[PosFiniteDouble] if the passed Double is greater than 0") {
        PosFiniteDouble.from(50.23).value.value shouldBe 50.23
        PosFiniteDouble.from(100.0).value.value shouldBe 100.0
      }
      it("returns None if the passed Double is NOT greater than 0") {
        PosFiniteDouble.from(0.0) shouldBe None
        PosFiniteDouble.from(-0.00001) shouldBe None
        PosFiniteDouble.from(-99.9) shouldBe None
      }
    }
    describe("should offer an ensuringValid factory method that") {
      it("returns PosFiniteDouble if the passed Double is greater than 0") {
        PosFiniteDouble.ensuringValid(50.23).value shouldBe 50.23
        PosFiniteDouble.ensuringValid(100.0).value shouldBe 100.0
      }
      it("throws AssertionError if the passed Double is NOT greater than 0") {
        an [AssertionError] should be thrownBy PosFiniteDouble.ensuringValid(0.0)
        an [AssertionError] should be thrownBy PosFiniteDouble.ensuringValid(-0.00001)
        an [AssertionError] should be thrownBy PosFiniteDouble.ensuringValid(-99.9)
        an [AssertionError] should be thrownBy PosFiniteDouble.ensuringValid(Double.PositiveInfinity)
        an [AssertionError] should be thrownBy PosFiniteDouble.ensuringValid(Double.NegativeInfinity)
        // SKIP-DOTTY-START
        // https://github.com/lampepfl/dotty/issues/6710
        an [AssertionError] should be thrownBy PosFiniteDouble.ensuringValid(Double.NaN)
        // SKIP-DOTTY-END
      }
    }
    describe("should offer a tryingValid factory method that") {
      import TryValues._
      it("returns a PosFiniteDouble wrapped in a Success if the passed PosFiniteDouble is greater than 0") {
        PosFiniteDouble.tryingValid(50.3).success.value.value shouldBe 50.3
        PosFiniteDouble.tryingValid(100.0).success.value.value shouldBe 100.0
      }

      it("returns an AssertionError wrapped in a Failure if the passed Double is NOT greater than 0") {
        PosFiniteDouble.tryingValid(0.0).failure.exception shouldBe an [AssertionError]
        PosFiniteDouble.tryingValid(-1.0).failure.exception shouldBe an [AssertionError]
        PosFiniteDouble.tryingValid(-99.9).failure.exception shouldBe an [AssertionError]
      }
    }
    describe("should offer a passOrElse factory method that") {
      it("returns a Pass if the given Double is greater than 0") {
        PosFiniteDouble.passOrElse(50.0)(i => i) shouldBe Pass
        PosFiniteDouble.passOrElse(100.0)(i => i) shouldBe Pass
      }
      it("returns an error value produced by passing the given Double to the given function if the passed Double is NOT greater than 0, wrapped in a Fail") {
        PosFiniteDouble.passOrElse(0.0)(i => s"$i did not taste good") shouldBe Fail(0.0 + " did not taste good")
        PosFiniteDouble.passOrElse(-1.1)(i => i) shouldBe Fail(-1.1)
        PosFiniteDouble.passOrElse(-99.0)(i => i + 3.0) shouldBe Fail(-96.0)
      }
    }
    describe("should offer a goodOrElse factory method that") {
      it("returns a PosFiniteDouble wrapped in a Good if the given Double is greater than 0") {
        PosFiniteDouble.goodOrElse(50.3)(i => i) shouldBe Good(PosFiniteDouble(50.3))
        PosFiniteDouble.goodOrElse(100.0)(i => i) shouldBe Good(PosFiniteDouble(100.0))
      }
      it("returns an error value produced by passing the given Double to the given function if the passed Double is NOT greater than 0, wrapped in a Bad") {
        PosFiniteDouble.goodOrElse(0.0)(i => s"$i did not taste good") shouldBe Bad(0.0 + " did not taste good")
        PosFiniteDouble.goodOrElse(-1.1)(i => i) shouldBe Bad(-1.1)
        PosFiniteDouble.goodOrElse(-99.0)(i => i + 3.0) shouldBe Bad(-96.0)
      }
    }
    describe("should offer a rightOrElse factory method that") {
      it("returns a PosFiniteDouble wrapped in a Right if the given Double is greater than 0") {
        PosFiniteDouble.rightOrElse(50.3)(i => i) shouldBe Right(PosFiniteDouble(50.3))
        PosFiniteDouble.rightOrElse(100.0)(i => i) shouldBe Right(PosFiniteDouble(100.0))
      }
      it("returns an error value produced by passing the given Double to the given function if the passed Double is NOT greater than 0, wrapped in a Left") {
        PosFiniteDouble.rightOrElse(0.0)(i => s"$i did not taste good") shouldBe Left(0.0 + " did not taste good")
        PosFiniteDouble.rightOrElse(-1.1)(i => i) shouldBe Left(-1.1)
        PosFiniteDouble.rightOrElse(-99.9)(i => i + 3.0) shouldBe Left(-96.9)
      }
    }
    describe("should offer an isValid predicate method that") {
      it("returns true if the passed Double is greater than 0") {
        PosFiniteDouble.isValid(50.23) shouldBe true
        PosFiniteDouble.isValid(100.0) shouldBe true
        PosFiniteDouble.isValid(0.0) shouldBe false
        PosFiniteDouble.isValid(-0.0) shouldBe false
        PosFiniteDouble.isValid(-0.00001) shouldBe false
        PosFiniteDouble.isValid(-99.9) shouldBe false
      }
    }
    describe("should offer a fromOrElse factory method that") {
      it("returns a PosFiniteDouble if the passed Double is greater than 0") {
        PosFiniteDouble.fromOrElse(50.23, PosFiniteDouble(42.0)).value shouldBe 50.23
        PosFiniteDouble.fromOrElse(100.0, PosFiniteDouble(42.0)).value shouldBe 100.0
      }
      it("returns a given default if the passed Double is NOT greater than 0") {
        PosFiniteDouble.fromOrElse(0.0, PosFiniteDouble(42.0)).value shouldBe 42.0
        PosFiniteDouble.fromOrElse(-0.00001, PosFiniteDouble(42.0)).value shouldBe 42.0
        PosFiniteDouble.fromOrElse(-99.9, PosFiniteDouble(42.0)).value shouldBe 42.0
      }
    }
    it("should offer MaxValue, MinValue, and MinPositiveValue factory methods") {
      PosFiniteDouble.MaxValue shouldEqual PosFiniteDouble.from(Double.MaxValue).get
      PosFiniteDouble.MinValue shouldEqual
        PosFiniteDouble.from(Double.MinPositiveValue).get
      PosFiniteDouble.MinPositiveValue shouldEqual
        PosFiniteDouble.from(Double.MinPositiveValue).get
    }
    it("should not offer a PositiveInfinity") {
      "PosFiniteDouble.PositiveInfinity" shouldNot compile
    }
    it("should not offer a NegativeInfinity factory method") {
      "PosFiniteDouble.NegativeInfinity" shouldNot compile
    }
    it("should not ffer a isPosInfinity method") {
      "PosFiniteDouble(1.0).isPosInfinity" shouldNot compile
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
        "PosFiniteDouble(8L)" should compile
        PosFiniteDouble(8L).value shouldEqual 8.0
        "PosFiniteDouble(8.0F)" should compile
        PosFiniteDouble(8.0F).value shouldEqual 8.0
        "PosFiniteDouble(8.0)" should compile
        PosFiniteDouble(8.0).value shouldEqual 8.0
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
      it("should not compile when x is passed in") {
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
        "takesPosFiniteDouble(8L)" should compile
        takesPosFiniteDouble(8L) shouldEqual 8.0
        "takesPosFiniteDouble(8.0F)" should compile
        takesPosFiniteDouble(8.0F) shouldEqual 8.0
        "takesPosFiniteDouble(8.0)" should compile
        takesPosFiniteDouble(8.0) shouldEqual 8.0
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

      it("should not compile when x is passed in") {
        val x: Int = -8
        "takesPosFiniteDouble(x)" shouldNot compile
        val b: Long = -8L
        "takesPosFiniteDouble(b)" shouldNot compile
        val c: Float = -8.0F
        "takesPosFiniteDouble(c)" shouldNot compile
        val d: Double = -8.0
        "takesPosFiniteDouble(d)" shouldNot compile
      }
    }

    it("should offer a unary + method that is consistent with Double") {
      forAll { (p: PosFiniteDouble) =>
        (+p).toDouble shouldEqual (+(p.toDouble))
      }
    }

    it("should offer a unary - method that returns NegFiniteDouble") {
      forAll { (p: PosFiniteDouble) =>
        (-p) shouldEqual (NegFiniteDouble.ensuringValid(-(p.toDouble)))
      }
    }

    it("should offer 'min' and 'max' methods that are consistent with Double") {
      forAll { (pdouble1: PosFiniteDouble, pdouble2: PosFiniteDouble) =>
        pdouble1.max(pdouble2).toDouble shouldEqual pdouble1.toDouble.max(pdouble2.toDouble)
        pdouble1.min(pdouble2).toDouble shouldEqual pdouble1.toDouble.min(pdouble2.toDouble)
      }
    }

    it("should offer an 'isWhole' method that is consistent with Double") {
      forAll { (pdouble: PosFiniteDouble) =>
        pdouble.isWhole shouldEqual pdouble.toDouble.isWhole
      }
    }

    it("should offer 'round', 'ceil', and 'floor' methods that are consistent with Double") {
      forAll { (pdouble: PosFiniteDouble) =>
        pdouble.round.toDouble shouldEqual pdouble.toDouble.round
        pdouble.ceil.toDouble shouldEqual pdouble.toDouble.ceil
        pdouble.floor.toDouble shouldEqual pdouble.toDouble.floor
      }
    }

    it("should offer 'toRadians' and 'toDegrees' methods that are consistent with Double") {
      forAll { (pdouble: PosFiniteDouble) =>
        pdouble.toRadians shouldEqual pdouble.toDouble.toRadians
      }
    }

    it("should offer an ensuringValid method that takes a Double => Double, throwing AssertionError if the result is invalid") {
      PosFiniteDouble(33.0).ensuringValid(_ + 1.0) shouldEqual PosFiniteDouble(34.0)
      an [AssertionError] should be thrownBy { PosFiniteDouble.MaxValue.ensuringValid(_ - PosFiniteDouble.MaxValue) }
      an [AssertionError] should be thrownBy { PosFiniteDouble.MaxValue.ensuringValid(_ => Double.PositiveInfinity) }
      an [AssertionError] should be thrownBy { PosFiniteDouble.MaxValue.ensuringValid(_ => Double.NegativeInfinity) }
      // SKIP-DOTTY-START
      // https://github.com/lampepfl/dotty/issues/6710
      an [AssertionError] should be thrownBy { PosFiniteDouble.MaxValue.ensuringValid(_ => Double.NaN) }
      // SKIP-DOTTY-END
    }
  }
}

