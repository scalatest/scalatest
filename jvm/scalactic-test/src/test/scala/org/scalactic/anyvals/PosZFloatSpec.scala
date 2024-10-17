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

trait PosZFloatSpecSupport {

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

class PosZFloatSpec extends funspec.AnyFunSpec with matchers.should.Matchers with PropertyChecks with TypeCheckedTripleEquals with PosZFloatSpecSupport {

  describe("A PosZFloat") {
    describe("should offer a from factory method that") {
      it("returns Some[PosZFloat] if the passed Float is greater than or equal to 0") {
        PosZFloat.from(0.0f).value.value shouldBe 0.0f
        PosZFloat.from(50.23f).value.value shouldBe 50.23f
        PosZFloat.from(100.0f).value.value shouldBe 100.0f
      }
      it("returns None if the passed Float is NOT greater than or equal to 0") {
        PosZFloat.from(-0.00001f) shouldBe None
        PosZFloat.from(-99.9f) shouldBe None
      }
    }
    describe("should offer an ensuringValid factory method that") {
      it("returns PosZFloat if the passed Float is greater than or equal to 0") {
        PosZFloat.ensuringValid(0.0f).value shouldBe 0.0f
        PosZFloat.ensuringValid(50.23f).value shouldBe 50.23f
        PosZFloat.ensuringValid(100.0f).value shouldBe 100.0f
        PosZFloat.ensuringValid(Float.PositiveInfinity).value shouldBe Float.PositiveInfinity
      }
      it("throws AssertionError if the passed Float is NOT greater than or equal to 0") {
        an [AssertionError] should be thrownBy PosZFloat.ensuringValid(-0.00001f)
        an [AssertionError] should be thrownBy PosZFloat.ensuringValid(-99.9f)
        an [AssertionError] should be thrownBy PosZFloat.ensuringValid(Float.NegativeInfinity)
        // SKIP-DOTTY-START
        // https://github.com/lampepfl/dotty/issues/6710
        an [AssertionError] should be thrownBy PosZFloat.ensuringValid(Float.NaN)
        // SKIP-DOTTY-END
      }
    }
    describe("should offer a tryingValid factory method that") {
      import TryValues._
      it("returns a PosZFloat wrapped in a Success if the passed Float is greater than or equal 0") {
        PosZFloat.tryingValid(0.0f).success.value.value shouldBe 0.0f
        PosZFloat.tryingValid(50.0f).success.value.value shouldBe 50.0f
        PosZFloat.tryingValid(100.0f).success.value.value shouldBe 100.0f
      }

      it("returns an AssertionError wrapped in a Failure if the passed Float is lesser than 0") {
        PosZFloat.tryingValid(-1.0f).failure.exception shouldBe an [AssertionError]
        PosZFloat.tryingValid(-99.0f).failure.exception shouldBe an [AssertionError]
      }
    }
    describe("should offer a passOrElse factory method that") {
      it("returns a Pass if the given Float is greater than or equal 0") {
        PosZFloat.passOrElse(0.0f)(i => i) shouldBe Pass
        PosZFloat.passOrElse(50.0f)(i => i) shouldBe Pass
        PosZFloat.passOrElse(100.0f)(i => i) shouldBe Pass
      }
      it("returns an error value produced by passing the given Float to the given function if the passed Float is lesser than 0, wrapped in a Fail") {
        PosZFloat.passOrElse(-1.0f)(i => i) shouldBe Fail(-1.0f)
        PosZFloat.passOrElse(-99.0f)(i => i + 3.0f) shouldBe Fail(-96.0f)
      }
    }
    describe("should offer a goodOrElse factory method that") {
      it("returns a PosZFloat wrapped in a Good if the given Float is greater than or equal 0") {
        PosZFloat.goodOrElse(0.0f)(i => i) shouldBe Good(PosZFloat(0.0f))
        PosZFloat.goodOrElse(50.0f)(i => i) shouldBe Good(PosZFloat(50.0f))
        PosZFloat.goodOrElse(100.0f)(i => i) shouldBe Good(PosZFloat(100.0f))
      }
      it("returns an error value produced by passing the given Float to the given function if the passed Float is lesser than 0, wrapped in a Bad") {
        PosZFloat.goodOrElse(-1.0f)(i => i) shouldBe Bad(-1.0f)
        PosZFloat.goodOrElse(-99.0f)(i => i + 3.0f) shouldBe Bad(-96.0f)
      }
    }
    describe("should offer a rightOrElse factory method that") {
      it("returns a PosZFloat wrapped in a Right if the given Float is greater than or equal 0") {
        PosZFloat.rightOrElse(0.0f)(i => i) shouldBe Right(PosZFloat(0.0f))
        PosZFloat.rightOrElse(50.0f)(i => i) shouldBe Right(PosZFloat(50.0f))
        PosZFloat.rightOrElse(100.0f)(i => i) shouldBe Right(PosZFloat(100.0f))
      }
      it("returns an error value produced by passing the given Float to the given function if the passed Float is lesser than 0, wrapped in a Left") {
        PosZFloat.rightOrElse(-1.0f)(i => i) shouldBe Left(-1.0f)
        PosZFloat.rightOrElse(-99.0f)(i => i + 3.0f) shouldBe Left(-96.0f)
      }
    }
    describe("should offer an isValid predicate method that") {
      it("returns true if the passed Float is greater than or equal to 0") {
        PosZFloat.isValid(50.23f) shouldBe true
        PosZFloat.isValid(100.0f) shouldBe true
        PosZFloat.isValid(0.0f) shouldBe true
        PosZFloat.isValid(-0.0f) shouldBe true
        PosZFloat.isValid(-0.00001f) shouldBe false
        PosZFloat.isValid(-99.9f) shouldBe false
      }
    }
    describe("should offer a fromOrElse factory method that") {
      it("returns a PosZFloat if the passed Float is greater than or equal to 0") {
        PosZFloat.fromOrElse(50.23f, PosZFloat(42.0f)).value shouldBe 50.23f
        PosZFloat.fromOrElse(100.0f, PosZFloat(42.0f)).value shouldBe 100.0f
        PosZFloat.fromOrElse(0.0f, PosZFloat(42.0f)).value shouldBe 0.0f
      }
      it("returns a given default if the passed Float is NOT greater than or equal to 0") {
        PosZFloat.fromOrElse(-0.00001f, PosZFloat(42.0f)).value shouldBe 42.0f
        PosZFloat.fromOrElse(-99.9f, PosZFloat(42.0f)).value shouldBe 42.0f
      }
    }
    it("should offer MaxValue and MinValue factory methods") {
      PosZFloat.MaxValue shouldEqual PosZFloat.from(Float.MaxValue).get
      PosZFloat.MinValue shouldEqual PosZFloat(0.0f)
    }
    it("should offer a PositiveInfinity factory method") {
      PosZFloat.PositiveInfinity shouldEqual PosZFloat.ensuringValid(Float.PositiveInfinity)
    }
    it("should offer a NegativeInfinity factory method") {
      "PosZFloat.NegativeInfinity" shouldNot compile
    }
    it("should offer a isPosInfinity method that returns true if the instance is PositiveInfinity") {
      PosZFloat.ensuringValid(Float.PositiveInfinity).isPosInfinity shouldBe true
      PosZFloat(1.0f).isPosInfinity shouldBe false
    }
    it("should not offer a isNegInfinity method") {
      "PosZFloat(1.0f).isNegInfinity" shouldNot compile
    }

    it("should be sortable") {
      val xs = List(PosZFloat(2.2F), PosZFloat(0.0F), PosZFloat(1.1F),
                    PosZFloat(3.3F))
      xs.sorted shouldEqual List(PosZFloat(0.0F), PosZFloat(1.1F),
                                 PosZFloat(2.2F), PosZFloat(3.3F))
    }

    describe("when created with apply method") {

      it("should compile when 8 is passed in") {
        "PosZFloat(8)" should compile
        PosZFloat(8).value shouldEqual 8.0F
        "PosZFloat(8L)" should compile
        PosZFloat(8L).value shouldEqual 8.0F
        "PosZFloat(8.0F)" should compile
        PosZFloat(8.0F).value shouldEqual 8.0F
      }

      it("should compile when 0 is passed in") {
        "PosZFloat(0)" should compile
        PosZFloat(0).value shouldEqual 0.0F
        "PosZFloat(0L)" should compile
        PosZFloat(0L).value shouldEqual 0.0F
        "PosZFloat(0.0F)" should compile
        PosZFloat(0.0F).value shouldEqual 0.0F
      }

      it("should not compile when -8 is passed in") {
        "PosZFloat(-8)" shouldNot compile
        "PosZFloat(-8L)" shouldNot compile
        "PosZFloat(-8.0F)" shouldNot compile
      }

      it("should not compile when x is passed in") {
        val a: Int = -8
        "PosZFloat(a)" shouldNot compile
        val b: Long = -8L
        "PosZFloat(b)" shouldNot compile
        val c: Float = -8.0F
        "PosZFloat(c)" shouldNot compile
      }
    }
    describe("when specified as a plain-old Float") {

      def takesPosZFloat(pos: PosZFloat): Float = pos.value

      it("should compile when 8 is passed in") {
        "takesPosZFloat(8)" should compile
        takesPosZFloat(8) shouldEqual 8.0F
        "takesPosZFloat(8L)" should compile
        takesPosZFloat(8L) shouldEqual 8.0F
        "takesPosZFloat(8.0F)" should compile
        takesPosZFloat(8.0F) shouldEqual 8.0F
      }

      it("should compile when 0 is passed in") {
        "takesPosZFloat(0)" should compile
        takesPosZFloat(0) shouldEqual 0.0F
        "takesPosZFloat(0L)" should compile
        takesPosZFloat(0L) shouldEqual 0.0F
        "takesPosZFloat(0.0F)" should compile
        takesPosZFloat(0.0F) shouldEqual 0.0F
      }

      it("should not compile when -8 is passed in") {
        "takesPosZFloat(-8)" shouldNot compile
        "takesPosZFloat(-8L)" shouldNot compile
        "takesPosZFloat(-8.0F)" shouldNot compile
      }

      it("should not compile when x is passed in") {
        val x: Int = -8
        "takesPosZFloat(x)" shouldNot compile
        val b: Long = -8L
        "takesPosZFloat(b)" shouldNot compile
        val c: Float = -8.0F
        "takesPosZFloat(c)" shouldNot compile
      }
    }

    it("should offer a unary + method that is consistent with Float") {
      forAll { (p: PosZFloat) =>
        (+p).toFloat shouldEqual (+(p.toFloat))
      }
    }

    it("should offer a unary - method that returns NegZFloat") {
      forAll { (p: PosZFloat) =>
        (-p) shouldEqual (NegZFloat.ensuringValid(-(p.toFloat)))
      }
    }

    it("should offer a 'plus' method that takes a PosZFloat and returns a PosFloat") {

      forAll { (posZFloat1: PosZFloat, posZFloat2: PosZFloat) =>
        (posZFloat1 plus posZFloat2) should === (PosZFloat.ensuringValid(posZFloat1.toFloat + posZFloat2.toFloat))
      }

      val examples =
        Table(
          (                "posZFloat1",                "posZFloat2" ),
          (         PosZFloat.MinValue,         PosZFloat.MinValue ),
          (         PosZFloat.MinValue, PosZFloat.MinPositiveValue ),
          (         PosZFloat.MinValue,         PosZFloat.MaxValue ),
          (         PosZFloat.MinValue, PosZFloat.PositiveInfinity ),
          (         PosZFloat.MaxValue,         PosZFloat.MinValue ),
          (         PosZFloat.MaxValue, PosZFloat.MinPositiveValue ),
          (         PosZFloat.MaxValue,         PosZFloat.MaxValue ),
          (         PosZFloat.MaxValue, PosZFloat.PositiveInfinity ),
          ( PosZFloat.PositiveInfinity,         PosZFloat.MinValue ),
          ( PosZFloat.PositiveInfinity, PosZFloat.MinPositiveValue ),
          ( PosZFloat.PositiveInfinity,         PosZFloat.MaxValue ),
          ( PosZFloat.PositiveInfinity, PosZFloat.PositiveInfinity )
        )

      forAll (examples) { (a, b) =>
        (a plus b).value should be >= 0.0f
      }

      // Sanity check that implicit widening conversions work too.
      // Here a PosInt gets "widened" to a PosZFloat.
      PosZFloat(1.0f) plus PosInt(2) should === (PosZFloat(3.0f))
    }

    it("should offer 'min' and 'max' methods that are consistent with Float") {
      forAll { (pfloat1: PosZFloat, pfloat2: PosZFloat) =>
        pfloat1.max(pfloat2).toFloat shouldEqual pfloat1.toFloat.max(pfloat2.toFloat)
        pfloat1.min(pfloat2).toFloat shouldEqual pfloat1.toFloat.min(pfloat2.toFloat)
      }
    }

    it("should offer an 'isWhole' method that is consistent with Float") {
      forAll { (pzfloat: PosZFloat) =>
        pzfloat.isWhole shouldEqual pzfloat.toFloat.isWhole
      }
    }

    it("should offer 'round', 'ceil', and 'floor' methods that are consistent with Float") {
      forAll { (pzfloat: PosZFloat) =>
        // SKIP-SCALATESTJS,NATIVE-START
        pzfloat.round.toFloat shouldEqual pzfloat.toFloat.round
        // SKIP-SCALATESTJS,NATIVE-END
        pzfloat.ceil.toFloat shouldEqual pzfloat.toFloat.ceil
        pzfloat.floor.toFloat shouldEqual pzfloat.toFloat.floor
      }
    }

    it("should offer 'toRadians' and 'toDegrees' methods that are consistent with Float") {
      forAll { (pzfloat: PosZFloat) =>
        pzfloat.toRadians.toFloat shouldEqual pzfloat.toFloat.toRadians
        pzfloat.toDegrees.toFloat shouldEqual pzfloat.toFloat.toDegrees
      }
    }

    it("should offer widening methods for basic types that are consistent with Float") {
      forAll { (pzfloat: PosZFloat) =>
        def widen(value: Float): Float = value
        widen(pzfloat) shouldEqual widen(pzfloat.toFloat)
      }
      forAll { (pzfloat: PosZFloat) =>
        def widen(value: Double): Double = value
        widen(pzfloat) shouldEqual widen(pzfloat.toFloat)
      }
      forAll { (pzfloat: PosZFloat) =>
        def widen(value: PosZDouble): PosZDouble = value
        widen(pzfloat) shouldEqual widen(PosZDouble.from(pzfloat.toFloat).get)
      }
    }
    it("should offer an ensuringValid method that takes a Float => Float, throwing AssertionError if the result is invalid") {
      PosZFloat(33.0f).ensuringValid(_ + 1.0f) shouldEqual PosZFloat(34.0f)
      PosZFloat(33.0f).ensuringValid(_ => Float.PositiveInfinity) shouldEqual PosZFloat.ensuringValid(Float.PositiveInfinity)
      an [AssertionError] should be thrownBy { PosZFloat.MaxValue.ensuringValid(_ - PosZFloat.MaxValue - 1) }
      an [AssertionError] should be thrownBy { PosFloat.MaxValue.ensuringValid(_ => Float.NegativeInfinity) }
      // SKIP-DOTTY-START
      // https://github.com/lampepfl/dotty/issues/6710
      an [AssertionError] should be thrownBy { PosZFloat.MaxValue.ensuringValid(_ => Float.NaN) }
      // SKIP-DOTTY-END
    }
    it("should offer an isFinite method that returns true if the value does not represent infinity") {
      forAll { (n: PosZFiniteFloat) =>
        (n: PosZFloat).isFinite should be (true)
        PosZFloat.PositiveInfinity.isFinite should be (false)
      }
    }
  }
}

