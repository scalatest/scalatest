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

trait PosDoubleSpecSupport {

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

class PosDoubleSpec extends funspec.AnyFunSpec with matchers.should.Matchers with PropertyChecks with TypeCheckedTripleEquals with PosDoubleSpecSupport {

  describe("A PosDouble") {
    describe("should offer a from factory method that") {
      it("returns Some[PosDouble] if the passed Double is greater than 0") {
        PosDouble.from(50.23).value.value shouldBe 50.23
        PosDouble.from(100.0).value.value shouldBe 100.0
      }
      it("returns None if the passed Double is NOT greater than 0") {
        PosDouble.from(0.0) shouldBe None
        PosDouble.from(-0.00001) shouldBe None
        PosDouble.from(-99.9) shouldBe None
      }
    }
    describe("should offer an ensuringValid factory method that") {
      it("returns PosDouble if the passed Double is greater than 0") {
        PosDouble.ensuringValid(50.23).value shouldBe 50.23
        PosDouble.ensuringValid(100.0).value shouldBe 100.0
        PosDouble.ensuringValid(Double.PositiveInfinity).value shouldBe Double.PositiveInfinity
      }
      it("throws AssertionError if the passed Double is NOT greater than 0") {
        an [AssertionError] should be thrownBy PosDouble.ensuringValid(0.0)
        an [AssertionError] should be thrownBy PosDouble.ensuringValid(-0.00001)
        an [AssertionError] should be thrownBy PosDouble.ensuringValid(-99.9)
        an [AssertionError] should be thrownBy PosDouble.ensuringValid(Double.NegativeInfinity)
        // SKIP-DOTTY-START
        // https://github.com/lampepfl/dotty/issues/6710
        an [AssertionError] should be thrownBy PosDouble.ensuringValid(Double.NaN)
        // SKIP-DOTTY-END
      }
    }
    describe("should offer a tryingValid factory method that") {
      import TryValues._
      it("returns a PosDouble wrapped in a Success if the passed PosDouble is greater than 0") {
        PosDouble.tryingValid(50.3).success.value.value shouldBe 50.3
        PosDouble.tryingValid(100.0).success.value.value shouldBe 100.0
      }

      it("returns an AssertionError wrapped in a Failure if the passed Double is NOT greater than 0") {
        PosDouble.tryingValid(0.0).failure.exception shouldBe an [AssertionError]
        PosDouble.tryingValid(-1.0).failure.exception shouldBe an [AssertionError]
        PosDouble.tryingValid(-99.9).failure.exception shouldBe an [AssertionError]
      }
    }
    describe("should offer a passOrElse factory method that") {
      it("returns a Pass if the given Double is greater than 0") {
        PosDouble.passOrElse(50.0)(i => i) shouldBe Pass
        PosDouble.passOrElse(100.0)(i => i) shouldBe Pass
      }
      it("returns an error value produced by passing the given Double to the given function if the passed Double is NOT greater than 0, wrapped in a Fail") {
        PosDouble.passOrElse(0.0)(i => s"$i did not taste good") shouldBe Fail(0.0 + " did not taste good")
        PosDouble.passOrElse(-1.1)(i => i) shouldBe Fail(-1.1)
        PosDouble.passOrElse(-99.0)(i => i + 3.0) shouldBe Fail(-96.0)
      }
    }
    describe("should offer a goodOrElse factory method that") {
      it("returns a PosDouble wrapped in a Good if the given Double is greater than 0") {
        PosDouble.goodOrElse(50.3)(i => i) shouldBe Good(PosDouble(50.3))
        PosDouble.goodOrElse(100.0)(i => i) shouldBe Good(PosDouble(100.0))
      }
      it("returns an error value produced by passing the given Double to the given function if the passed Double is NOT greater than 0, wrapped in a Bad") {
        PosDouble.goodOrElse(0.0)(i => s"$i did not taste good") shouldBe Bad(0.0 + " did not taste good")
        PosDouble.goodOrElse(-1.1)(i => i) shouldBe Bad(-1.1)
        PosDouble.goodOrElse(-99.0)(i => i + 3.0) shouldBe Bad(-96.0)
      }
    }
    describe("should offer a rightOrElse factory method that") {
      it("returns a PosDouble wrapped in a Right if the given Double is greater than 0") {
        PosDouble.rightOrElse(50.3)(i => i) shouldBe Right(PosDouble(50.3))
        PosDouble.rightOrElse(100.0)(i => i) shouldBe Right(PosDouble(100.0))
      }
      it("returns an error value produced by passing the given Double to the given function if the passed Double is NOT greater than 0, wrapped in a Left") {
        PosDouble.rightOrElse(0.0)(i => s"$i did not taste good") shouldBe Left(0.0 + " did not taste good")
        PosDouble.rightOrElse(-1.1)(i => i) shouldBe Left(-1.1)
        PosDouble.rightOrElse(-99.9)(i => i + 3.0) shouldBe Left(-96.9)
      }
    }
    describe("should offer an isValid predicate method that") {
      it("returns true if the passed Double is greater than 0") {
        PosDouble.isValid(50.23) shouldBe true
        PosDouble.isValid(100.0) shouldBe true
        PosDouble.isValid(0.0) shouldBe false
        PosDouble.isValid(-0.0) shouldBe false
        PosDouble.isValid(-0.00001) shouldBe false
        PosDouble.isValid(-99.9) shouldBe false
      }
    }
    describe("should offer a fromOrElse factory method that") {
      it("returns a PosDouble if the passed Double is greater than 0") {
        PosDouble.fromOrElse(50.23, PosDouble(42.0)).value shouldBe 50.23
        PosDouble.fromOrElse(100.0, PosDouble(42.0)).value shouldBe 100.0
      }
      it("returns a given default if the passed Double is NOT greater than 0") {
        PosDouble.fromOrElse(0.0, PosDouble(42.0)).value shouldBe 42.0
        PosDouble.fromOrElse(-0.00001, PosDouble(42.0)).value shouldBe 42.0
        PosDouble.fromOrElse(-99.9, PosDouble(42.0)).value shouldBe 42.0
      }
    }
    it("should offer MaxValue, MinValue, and MinPositiveValue factory methods") {
      PosDouble.MaxValue shouldEqual PosDouble.from(Double.MaxValue).get
      PosDouble.MinValue shouldEqual
        PosDouble.from(Double.MinPositiveValue).get
      PosDouble.MinPositiveValue shouldEqual
        PosDouble.from(Double.MinPositiveValue).get
    }
    it("should offer a PositiveInfinity factory method") {
      PosDouble.PositiveInfinity shouldEqual PosDouble.ensuringValid(Double.PositiveInfinity)
    }
    it("should not offer a NegativeInfinity factory method") {
      "PosDouble.NegativeInfinity" shouldNot compile
    }
    it("should offer a isPosInfinity method that returns true if the instance is PositiveInfinity") {
      PosDouble.ensuringValid(Double.PositiveInfinity).isPosInfinity shouldBe true
      PosDouble(1.0).isPosInfinity shouldBe false
    }

    it("should be sortable") {
      val xs = List(PosDouble(2.2), PosDouble(4.4), PosDouble(1.1),
                    PosDouble(3.3))
      xs.sorted shouldEqual List(PosDouble(1.1), PosDouble(2.2), PosDouble(3.3),
                                 PosDouble(4.4))
    }

    describe("when created with apply method") {

      it("should compile when 8 is passed in") {
        "PosDouble(8)" should compile
        PosDouble(8).value shouldEqual 8.0
        "PosDouble(8L)" should compile
        PosDouble(8L).value shouldEqual 8.0
        "PosDouble(8.0F)" should compile
        PosDouble(8.0F).value shouldEqual 8.0
        "PosDouble(8.0)" should compile
        PosDouble(8.0).value shouldEqual 8.0
      }

      it("should not compile when 0 is passed in") {
        "PosDouble(0)" shouldNot compile
        "PosDouble(0L)" shouldNot compile
        "PosDouble(0.0F)" shouldNot compile
        "PosDouble(0.0)" shouldNot compile
      }

      it("should not compile when -8 is passed in") {
        "PosDouble(-8)" shouldNot compile
        "PosDouble(-8L)" shouldNot compile
        "PosDouble(-8.0F)" shouldNot compile
        "PosDouble(-8.0)" shouldNot compile
      }
      it("should not compile when x is passed in") {
        val a: Int = -8
        "PosDouble(a)" shouldNot compile
        val b: Long = -8L
        "PosDouble(b)" shouldNot compile
        val c: Float = -8.0F
        "PosDouble(c)" shouldNot compile
        val d: Double = -8.0
        "PosDouble(d)" shouldNot compile
      }
    }
    describe("when specified as a plain-old Double") {

      def takesPosDouble(pos: PosDouble): Double = pos.value

      it("should compile when 8 is passed in") {
        "takesPosDouble(8)" should compile
        takesPosDouble(8) shouldEqual 8.0
        "takesPosDouble(8L)" should compile
        takesPosDouble(8L) shouldEqual 8.0
        "takesPosDouble(8.0F)" should compile
        takesPosDouble(8.0F) shouldEqual 8.0
        "takesPosDouble(8.0)" should compile
        takesPosDouble(8.0) shouldEqual 8.0
      }

      it("should not compile when 0 is passed in") {
        "takesPosDouble(0)" shouldNot compile
        "takesPosDouble(0L)" shouldNot compile
        "takesPosDouble(0.0F)" shouldNot compile
        "takesPosDouble(0.0)" shouldNot compile
      }

      it("should not compile when -8 is passed in") {
        "takesPosDouble(-8)" shouldNot compile
        "takesPosDouble(-8L)" shouldNot compile
        "takesPosDouble(-8.0F)" shouldNot compile
        "takesPosDouble(-8.0)" shouldNot compile
      }

      it("should not compile when x is passed in") {
        val x: Int = -8
        "takesPosDouble(x)" shouldNot compile
        val b: Long = -8L
        "takesPosDouble(b)" shouldNot compile
        val c: Float = -8.0F
        "takesPosDouble(c)" shouldNot compile
        val d: Double = -8.0
        "takesPosDouble(d)" shouldNot compile
      }
    }

    it("should offer a unary + method that is consistent with Double") {
      forAll { (p: PosDouble) =>
        (+p).toDouble shouldEqual (+(p.toDouble))
      }
    }

    it("should offer a unary - method that returns NegDouble") {
      forAll { (p: PosDouble) =>
        (-p) shouldEqual (NegDouble.ensuringValid(-(p.toDouble)))
      }
    }

    it("should offer a 'plus' method that takes a PosZDouble and returns a PosDouble") {

      forAll { (posDouble: PosDouble, posZDouble: PosZDouble) =>
        (posDouble plus posZDouble) should === (PosDouble.ensuringValid(posDouble.value + posZDouble.value))
      }

      val examples =
        Table(
          (                "posDouble",                "posZDouble" ),
          (         PosDouble.MinValue,         PosZDouble.MinValue ),
          (         PosDouble.MinValue, PosZDouble.MinPositiveValue ),
          (         PosDouble.MinValue,         PosZDouble.MaxValue ),
          (         PosDouble.MinValue, PosZDouble.PositiveInfinity ),
          (         PosDouble.MaxValue,         PosZDouble.MinValue ),
          (         PosDouble.MaxValue, PosZDouble.MinPositiveValue ),
          (         PosDouble.MaxValue,         PosZDouble.MaxValue ),
          (         PosDouble.MaxValue, PosZDouble.PositiveInfinity ),
          ( PosDouble.PositiveInfinity,         PosZDouble.MinValue ),
          ( PosDouble.PositiveInfinity, PosZDouble.MinPositiveValue ),
          ( PosDouble.PositiveInfinity,         PosZDouble.MaxValue ),
          ( PosDouble.PositiveInfinity, PosZDouble.PositiveInfinity )
        )

      forAll (examples) { (a, b) =>
        (a plus b).value should be > 0.0
      }

      // Sanity check that implicit widening conversions work too.
      (PosDouble(1.0) plus PosInt(2)) should === (PosDouble(3.0))
    }

    it("should offer 'min' and 'max' methods that are consistent with Double") {
      forAll { (pdouble1: PosDouble, pdouble2: PosDouble) =>
        pdouble1.max(pdouble2).toDouble shouldEqual pdouble1.toDouble.max(pdouble2.toDouble)
        pdouble1.min(pdouble2).toDouble shouldEqual pdouble1.toDouble.min(pdouble2.toDouble)
      }
    }

    it("should offer an 'isWhole' method that is consistent with Double") {
      forAll { (pdouble: PosDouble) =>
        pdouble.isWhole shouldEqual pdouble.toDouble.isWhole
      }
    }

    it("should offer 'round', 'ceil', and 'floor' methods that are consistent with Double") {
      forAll { (pdouble: PosDouble) =>
        pdouble.round.toDouble shouldEqual pdouble.toDouble.round
        pdouble.ceil.toDouble shouldEqual pdouble.toDouble.ceil
        pdouble.floor.toDouble shouldEqual pdouble.toDouble.floor
      }
    }

    it("should offer 'toRadians' and 'toDegrees' methods that are consistent with Double") {
      forAll { (pdouble: PosDouble) =>
        pdouble.toRadians shouldEqual pdouble.toDouble.toRadians
      }
    }

    it("should offer widening methods for basic types that are consistent with Double") {
      forAll { (pdouble: PosDouble) =>
        def widen(value: Double): Double = value
        widen(pdouble) shouldEqual widen(pdouble.toDouble)
      }
      forAll { (pdouble: PosDouble) =>
        def widen(value: PosZDouble): PosZDouble = value
        widen(pdouble) shouldEqual widen(PosZDouble.from(pdouble.toDouble).get)
      }
      forAll { (pdouble: PosDouble) =>
        def widen(value: NonZeroDouble): NonZeroDouble = value
        widen(pdouble) shouldEqual widen(NonZeroDouble.from(pdouble.toDouble).get)
      }
    }

    it("should offer an ensuringValid method that takes a Double => Double, throwing AssertionError if the result is invalid") {
      PosDouble(33.0).ensuringValid(_ + 1.0) shouldEqual PosDouble(34.0)
      PosDouble(33.0).ensuringValid(_ => Double.PositiveInfinity) shouldEqual PosDouble.ensuringValid(Double.PositiveInfinity)
      an [AssertionError] should be thrownBy { PosDouble.MaxValue.ensuringValid(_ - PosDouble.MaxValue) }
      an [AssertionError] should be thrownBy { PosDouble.MaxValue.ensuringValid(_ => Double.NegativeInfinity) }
      // SKIP-DOTTY-START
      // https://github.com/lampepfl/dotty/issues/6710
      an [AssertionError] should be thrownBy { PosDouble.MaxValue.ensuringValid(_ => Double.NaN) }
      // SKIP-DOTTY-END
    }
    it("should offer an isFinite method that returns true if the value does not represent infinity") {
      forAll { (n: PosFiniteDouble) =>
        (n: PosDouble).isFinite should be (true)
        PosDouble.PositiveInfinity.isFinite should be (false)
      }
    }
  }
}

