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
import org.scalactic.TypeCheckedTripleEquals
// SKIP-SCALATESTJS,NATIVE-START
import scala.collection.immutable.NumericRange
// SKIP-SCALATESTJS,NATIVE-END
import OptionValues._
import scala.collection.mutable.WrappedArray
//import org.scalactic.StrictCheckedEquality
import org.scalactic.Equality
import org.scalactic.{Pass, Fail}
import org.scalactic.{Good, Bad}
import scala.util.{Try, Success, Failure}

trait PosZFiniteFloatSpecSupport {

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
      // I needed this because with GenDrivenPropertyChecks, got:
      // [info] - should offer a '%' method that is consistent with Int *** FAILED ***
      // [info]   Success(NaN) did not equal Success(NaN) (PosIntExperiment.scala:498)
      case Success(float: Float) if float.isNaN =>
        b match {
          case Success(bFloat: Float) if bFloat.isNaN => true
          case _ => false
        }
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

class PosZFiniteFloatSpec extends funspec.AnyFunSpec with matchers.should.Matchers with PropertyChecks with TypeCheckedTripleEquals with PosZFiniteFloatSpecSupport {

  describe("A PosZFiniteFloat") {
    describe("should offer a from factory method that") {
      it("returns Some[PosZFiniteFloat] if the passed Float is greater than or equal to 0") {
        PosZFiniteFloat.from(0.0f).value.value shouldBe 0.0f
        PosZFiniteFloat.from(50.23f).value.value shouldBe 50.23f
        PosZFiniteFloat.from(100.0f).value.value shouldBe 100.0f
      }
      it("returns None if the passed Float is NOT greater than or equal to 0") {
        PosZFiniteFloat.from(-0.00001f) shouldBe None
        PosZFiniteFloat.from(-99.9f) shouldBe None
      }
    }
    describe("should offer an ensuringValid factory method that") {
      it("returns PosZFiniteFloat if the passed Float is greater than or equal to 0") {
        PosZFiniteFloat.ensuringValid(0.0f).value shouldBe 0.0f
        PosZFiniteFloat.ensuringValid(50.23f).value shouldBe 50.23f
        PosZFiniteFloat.ensuringValid(100.0f).value shouldBe 100.0f
      }
      it("throws AssertionError if the passed Float is NOT greater than or equal to 0") {
        an [AssertionError] should be thrownBy PosZFiniteFloat.ensuringValid(-0.00001f)
        an [AssertionError] should be thrownBy PosZFiniteFloat.ensuringValid(-99.9f)
        an [AssertionError] should be thrownBy PosZFiniteFloat.ensuringValid(Float.PositiveInfinity)
        an [AssertionError] should be thrownBy PosZFiniteFloat.ensuringValid(Float.NegativeInfinity)
        // SKIP-DOTTY-START
        // https://github.com/lampepfl/dotty/issues/6710
        an [AssertionError] should be thrownBy PosZFiniteFloat.ensuringValid(Float.NaN)
        // SKIP-DOTTY-END
      }
    }
    describe("should offer a tryingValid factory method that") {
      import TryValues._
      it("returns a PosZFiniteFloat wrapped in a Success if the passed Float is greater than or equal 0") {
        PosZFiniteFloat.tryingValid(0.0f).success.value.value shouldBe 0.0f
        PosZFiniteFloat.tryingValid(50.0f).success.value.value shouldBe 50.0f
        PosZFiniteFloat.tryingValid(100.0f).success.value.value shouldBe 100.0f
      }

      it("returns an AssertionError wrapped in a Failure if the passed Float is lesser than 0") {
        PosZFiniteFloat.tryingValid(-1.0f).failure.exception shouldBe an [AssertionError]
        PosZFiniteFloat.tryingValid(-99.0f).failure.exception shouldBe an [AssertionError]
      }
    }
    describe("should offer a passOrElse factory method that") {
      it("returns a Pass if the given Float is greater than or equal 0") {
        PosZFiniteFloat.passOrElse(0.0f)(i => i) shouldBe Pass
        PosZFiniteFloat.passOrElse(50.0f)(i => i) shouldBe Pass
        PosZFiniteFloat.passOrElse(100.0f)(i => i) shouldBe Pass
      }
      it("returns an error value produced by passing the given Float to the given function if the passed Float is lesser than 0, wrapped in a Fail") {
        PosZFiniteFloat.passOrElse(-1.0f)(i => i) shouldBe Fail(-1.0f)
        PosZFiniteFloat.passOrElse(-99.0f)(i => i + 3.0f) shouldBe Fail(-96.0f)
      }
    }
    describe("should offer a goodOrElse factory method that") {
      it("returns a PosZFiniteFloat wrapped in a Good if the given Float is greater than or equal 0") {
        PosZFiniteFloat.goodOrElse(0.0f)(i => i) shouldBe Good(PosZFiniteFloat(0.0f))
        PosZFiniteFloat.goodOrElse(50.0f)(i => i) shouldBe Good(PosZFiniteFloat(50.0f))
        PosZFiniteFloat.goodOrElse(100.0f)(i => i) shouldBe Good(PosZFiniteFloat(100.0f))
      }
      it("returns an error value produced by passing the given Float to the given function if the passed Float is lesser than 0, wrapped in a Bad") {
        PosZFiniteFloat.goodOrElse(-1.0f)(i => i) shouldBe Bad(-1.0f)
        PosZFiniteFloat.goodOrElse(-99.0f)(i => i + 3.0f) shouldBe Bad(-96.0f)
      }
    }
    describe("should offer a rightOrElse factory method that") {
      it("returns a PosZFiniteFloat wrapped in a Right if the given Float is greater than or equal 0") {
        PosZFiniteFloat.rightOrElse(0.0f)(i => i) shouldBe Right(PosZFiniteFloat(0.0f))
        PosZFiniteFloat.rightOrElse(50.0f)(i => i) shouldBe Right(PosZFiniteFloat(50.0f))
        PosZFiniteFloat.rightOrElse(100.0f)(i => i) shouldBe Right(PosZFiniteFloat(100.0f))
      }
      it("returns an error value produced by passing the given Float to the given function if the passed Float is lesser than 0, wrapped in a Left") {
        PosZFiniteFloat.rightOrElse(-1.0f)(i => i) shouldBe Left(-1.0f)
        PosZFiniteFloat.rightOrElse(-99.0f)(i => i + 3.0f) shouldBe Left(-96.0f)
      }
    }
    describe("should offer an isValid predicate method that") {
      it("returns true if the passed Float is greater than or equal to 0") {
        PosZFiniteFloat.isValid(50.23f) shouldBe true
        PosZFiniteFloat.isValid(100.0f) shouldBe true
        PosZFiniteFloat.isValid(0.0f) shouldBe true
        PosZFiniteFloat.isValid(-0.0f) shouldBe true
        PosZFiniteFloat.isValid(-0.00001f) shouldBe false
        PosZFiniteFloat.isValid(-99.9f) shouldBe false
      }
    }
    describe("should offer a fromOrElse factory method that") {
      it("returns a PosZFiniteFloat if the passed Float is greater than or equal to 0") {
        PosZFiniteFloat.fromOrElse(50.23f, PosZFiniteFloat(42.0f)).value shouldBe 50.23f
        PosZFiniteFloat.fromOrElse(100.0f, PosZFiniteFloat(42.0f)).value shouldBe 100.0f
        PosZFiniteFloat.fromOrElse(0.0f, PosZFiniteFloat(42.0f)).value shouldBe 0.0f
      }
      it("returns a given default if the passed Float is NOT greater than or equal to 0") {
        PosZFiniteFloat.fromOrElse(-0.00001f, PosZFiniteFloat(42.0f)).value shouldBe 42.0f
        PosZFiniteFloat.fromOrElse(-99.9f, PosZFiniteFloat(42.0f)).value shouldBe 42.0f
      }
    }
    it("should offer MaxValue and MinValue factory methods") {
      PosZFiniteFloat.MaxValue shouldEqual PosZFiniteFloat.from(Float.MaxValue).get
      PosZFiniteFloat.MinValue shouldEqual PosZFiniteFloat(0.0f)
    }
    it("should not offer a PositiveInfinity factory method") {
      "PosZFiniteFloat.PositiveInfinity" shouldNot compile
    }
    it("should not offer a NegativeInfinity factory method") {
      "PosZFiniteFloat.NegativeInfinity" shouldNot compile
    }
    it("should not offer a isNegInfinity method") {
      "PosZFiniteFloat(1.0f).isNegInfinity" shouldNot compile
    }

    it("should be sortable") {
      val xs = List(PosZFiniteFloat(2.2F), PosZFiniteFloat(0.0F), PosZFiniteFloat(1.1F),
        PosZFiniteFloat(3.3F))
      xs.sorted shouldEqual List(PosZFiniteFloat(0.0F), PosZFiniteFloat(1.1F),
        PosZFiniteFloat(2.2F), PosZFiniteFloat(3.3F))
    }

    describe("when created with apply method") {

      it("should compile when 8 is passed in") {
        "PosZFiniteFloat(8)" should compile
        PosZFiniteFloat(8).value shouldEqual 8.0F
        "PosZFiniteFloat(8L)" should compile
        PosZFiniteFloat(8L).value shouldEqual 8.0F
        "PosZFiniteFloat(8.0F)" should compile
        PosZFiniteFloat(8.0F).value shouldEqual 8.0F
      }

      it("should compile when 0 is passed in") {
        "PosZFiniteFloat(0)" should compile
        PosZFiniteFloat(0).value shouldEqual 0.0F
        "PosZFiniteFloat(0L)" should compile
        PosZFiniteFloat(0L).value shouldEqual 0.0F
        "PosZFiniteFloat(0.0F)" should compile
        PosZFiniteFloat(0.0F).value shouldEqual 0.0F
      }

      it("should not compile when -8 is passed in") {
        "PosZFiniteFloat(-8)" shouldNot compile
        "PosZFiniteFloat(-8L)" shouldNot compile
        "PosZFiniteFloat(-8.0F)" shouldNot compile
      }

      it("should not compile when x is passed in") {
        val a: Int = -8
        "PosZFiniteFloat(a)" shouldNot compile
        val b: Long = -8L
        "PosZFiniteFloat(b)" shouldNot compile
        val c: Float = -8.0F
        "PosZFiniteFloat(c)" shouldNot compile
      }
    }
    describe("when specified as a plain-old Float") {

      def takesPosZFiniteFloat(pos: PosZFiniteFloat): Float = pos.value

      it("should compile when 8 is passed in") {
        "takesPosZFiniteFloat(8)" should compile
        takesPosZFiniteFloat(8) shouldEqual 8.0F
        "takesPosZFiniteFloat(8L)" should compile
        takesPosZFiniteFloat(8L) shouldEqual 8.0F
        "takesPosZFiniteFloat(8.0F)" should compile
        takesPosZFiniteFloat(8.0F) shouldEqual 8.0F
      }

      it("should compile when 0 is passed in") {
        "takesPosZFiniteFloat(0)" should compile
        takesPosZFiniteFloat(0) shouldEqual 0.0F
        "takesPosZFiniteFloat(0L)" should compile
        takesPosZFiniteFloat(0L) shouldEqual 0.0F
        "takesPosZFiniteFloat(0.0F)" should compile
        takesPosZFiniteFloat(0.0F) shouldEqual 0.0F
      }

      it("should not compile when -8 is passed in") {
        "takesPosZFiniteFloat(-8)" shouldNot compile
        "takesPosZFiniteFloat(-8L)" shouldNot compile
        "takesPosZFiniteFloat(-8.0F)" shouldNot compile
      }

      it("should not compile when x is passed in") {
        val x: Int = -8
        "takesPosZFiniteFloat(x)" shouldNot compile
        val b: Long = -8L
        "takesPosZFiniteFloat(b)" shouldNot compile
        val c: Float = -8.0F
        "takesPosZFiniteFloat(c)" shouldNot compile
      }
    }

    it("should offer a unary + method that is consistent with Float") {
      forAll { (p: PosZFiniteFloat) =>
        (+p).toFloat shouldEqual (+(p.toFloat))
      }
    }

    it("should offer a unary - method that returns NegZFiniteFloat") {
      forAll { (p: PosZFiniteFloat) =>
        (-p) shouldEqual (NegZFiniteFloat.ensuringValid(-(p.toFloat)))
      }
    }

    it("should offer 'min' and 'max' methods that are consistent with Float") {
      forAll { (pfloat1: PosZFiniteFloat, pfloat2: PosZFiniteFloat) =>
        pfloat1.max(pfloat2).toFloat shouldEqual pfloat1.toFloat.max(pfloat2.toFloat)
        pfloat1.min(pfloat2).toFloat shouldEqual pfloat1.toFloat.min(pfloat2.toFloat)
      }
    }

    it("should offer an 'isWhole' method that is consistent with Float") {
      forAll { (pzfloat: PosZFiniteFloat) =>
        pzfloat.isWhole shouldEqual pzfloat.toFloat.isWhole
      }
    }

    it("should offer 'round', 'ceil', and 'floor' methods that are consistent with Float") {
      forAll { (pzfloat: PosZFiniteFloat) =>
        // SKIP-SCALATESTJS,NATIVE-START
        pzfloat.round.toFloat shouldEqual pzfloat.toFloat.round
        // SKIP-SCALATESTJS,NATIVE-END
        pzfloat.ceil.toFloat shouldEqual pzfloat.toFloat.ceil
        pzfloat.floor.toFloat shouldEqual pzfloat.toFloat.floor
      }
    }

    it("should offer 'toRadians' and 'toDegrees' methods that are consistent with Float") {
      forAll { (pzfloat: PosZFiniteFloat) =>
        pzfloat.toRadians.toFloat shouldEqual pzfloat.toFloat.toRadians
        pzfloat.toDegrees.toFloat shouldEqual pzfloat.toFloat.toDegrees
      }
    }

    it("should offer widening methods for basic types that are consistent with Float") {
      forAll { (pzfloat: PosZFiniteFloat) =>
        def widen(value: Float): Float = value
        widen(pzfloat) shouldEqual widen(pzfloat.toFloat)
      }
      forAll { (pzfloat: PosZFiniteFloat) =>
        def widen(value: Double): Double = value
        widen(pzfloat) shouldEqual widen(pzfloat.toFloat)
      }
      forAll { (pzfloat: PosZFiniteFloat) =>
        def widen(value: PosZDouble): PosZDouble = value
        widen(pzfloat) shouldEqual widen(PosZDouble.from(pzfloat.toFloat).get)
      }
    }
    it("should offer an ensuringValid method that takes a Float => Float, throwing AssertionError if the result is invalid") {
      PosZFiniteFloat(33.0f).ensuringValid(_ + 1.0f) shouldEqual PosZFiniteFloat(34.0f)
      an [AssertionError] should be thrownBy { PosZFiniteFloat.MaxValue.ensuringValid(_ - PosZFiniteFloat.MaxValue - 1) }
      an [AssertionError] should be thrownBy { PosFiniteFloat.MaxValue.ensuringValid(_ => Float.PositiveInfinity) }
      an [AssertionError] should be thrownBy { PosFiniteFloat.MaxValue.ensuringValid(_ => Float.NegativeInfinity) }
      // SKIP-DOTTY-START
      // https://github.com/lampepfl/dotty/issues/6710
      an [AssertionError] should be thrownBy { PosZFiniteFloat.MaxValue.ensuringValid(_ => Float.NaN) }
      // SKIP-DOTTY-END
    }
  }
}
