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

trait NegZFiniteFloatSpecSupport {

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

class NegZFiniteFloatSpec extends funspec.AnyFunSpec with matchers.should.Matchers with PropertyChecks with TypeCheckedTripleEquals with NegZFiniteFloatSpecSupport {

  describe("A NegZFiniteFloat") {
    describe("should offer a from factory method that") {
      it("returns Some[NegZFiniteFloat] if the passed Float is lesser than or equal to 0") {
        NegZFiniteFloat.from(0.0f).value.value shouldBe 0.0f
        NegZFiniteFloat.from(-50.23f).value.value shouldBe -50.23f
        NegZFiniteFloat.from(-100.0f).value.value shouldBe -100.0f
      }
      it("returns None if the passed Float is NOT greater than 0") {
        NegZFiniteFloat.from(0.00001f) shouldBe None
        NegZFiniteFloat.from(99.9f) shouldBe None
      }
    }
    describe("should offer an ensuringValid factory method that") {
      it("returns NegZFiniteFloat if the passed Float is lesser than or equal to 0") {
        NegZFiniteFloat.ensuringValid(0.0f).value shouldBe 0.0f
        NegZFiniteFloat.ensuringValid(-50.23f).value shouldBe -50.23f
        NegZFiniteFloat.ensuringValid(-100.0f).value shouldBe -100.0f
      }
      it("throws AssertionError if the passed Float is greater than 0") {
        an [AssertionError] should be thrownBy NegZFiniteFloat.ensuringValid(0.00001f)
        an [AssertionError] should be thrownBy NegZFiniteFloat.ensuringValid(99.9f)
        an [AssertionError] should be thrownBy NegZFiniteFloat.ensuringValid(Float.PositiveInfinity)
        an [AssertionError] should be thrownBy NegZFiniteFloat.ensuringValid(Float.NegativeInfinity)
        // SKIP-DOTTY-START
        // https://github.com/lampepfl/dotty/issues/6710
        an [AssertionError] should be thrownBy NegZFiniteFloat.ensuringValid(Float.NaN)
        // SKIP-DOTTY-END
      }
    }
    describe("should offer a tryingValid factory method that") {
      import TryValues._
      it("returns a NegZFiniteFloat wrapped in a Success if the passed Float is lesser than or equal 0") {
        NegZFiniteFloat.tryingValid(0.0f).success.value.value shouldBe 0.0f
        NegZFiniteFloat.tryingValid(-50.0f).success.value.value shouldBe -50.0f
        NegZFiniteFloat.tryingValid(-100.0f).success.value.value shouldBe -100.0f
      }

      it("returns an AssertionError wrapped in a Failure if the passed Float is greater than 0") {
        NegZFiniteFloat.tryingValid(1.0f).failure.exception shouldBe an [AssertionError]
        NegZFiniteFloat.tryingValid(99.0f).failure.exception shouldBe an [AssertionError]
      }
    }
    describe("should offer a passOrElse factory method that") {
      it("returns a Pass if the given Float is lesser than or equal 0") {
        NegZFiniteFloat.passOrElse(0.0f)(i => i) shouldBe Pass
        NegZFiniteFloat.passOrElse(-50.0f)(i => i) shouldBe Pass
        NegZFiniteFloat.passOrElse(-100.0f)(i => i) shouldBe Pass
      }
      it("returns an error value produced by passing the given Float to the given function if the passed Float is greater than 0, wrapped in a Fail") {
        NegZFiniteFloat.passOrElse(1.0f)(i => i) shouldBe Fail(1.0f)
        NegZFiniteFloat.passOrElse(99.0f)(i => i + 3.0f) shouldBe Fail(102.0f)
      }
    }
    describe("should offer a goodOrElse factory method that") {
      it("returns a NegZFiniteFloat wrapped in a Good if the given Float is lesser than or equal 0") {
        NegZFiniteFloat.goodOrElse(0.0f)(i => i) shouldBe Good(NegZFiniteFloat(0.0f))
        NegZFiniteFloat.goodOrElse(-50.0f)(i => i) shouldBe Good(NegZFiniteFloat(-50.0f))
        NegZFiniteFloat.goodOrElse(-100.0f)(i => i) shouldBe Good(NegZFiniteFloat(-100.0f))
      }
      it("returns an error value produced by passing the given Float to the given function if the passed Float is greater than 0, wrapped in a Bad") {
        NegZFiniteFloat.goodOrElse(1.0f)(i => i) shouldBe Bad(1.0f)
        NegZFiniteFloat.goodOrElse(99.0f)(i => i + 3.0f) shouldBe Bad(102.0f)
      }
    }
    describe("should offer a rightOrElse factory method that") {
      it("returns a NegZFiniteFloat wrapped in a Right if the given Float is lesser than or equal 0") {
        NegZFiniteFloat.rightOrElse(0.0f)(i => i) shouldBe Right(NegZFiniteFloat(0.0f))
        NegZFiniteFloat.rightOrElse(-50.0f)(i => i) shouldBe Right(NegZFiniteFloat(-50.0f))
        NegZFiniteFloat.rightOrElse(-100.0f)(i => i) shouldBe Right(NegZFiniteFloat(-100.0f))
      }
      it("returns an error value produced by passing the given Float to the given function if the passed Float is greater than 0, wrapped in a Left") {
        NegZFiniteFloat.rightOrElse(1.0f)(i => i) shouldBe Left(1.0f)
        NegZFiniteFloat.rightOrElse(99.0f)(i => i + 3.0f) shouldBe Left(102.0f)
      }
    }
    describe("should offer an isValid predicate method that") {
      it("returns true if the passed Float is lesser than or equal to 0") {
        NegZFiniteFloat.isValid(-50.23f) shouldBe true
        NegZFiniteFloat.isValid(-100.0f) shouldBe true
        NegZFiniteFloat.isValid(0.0f) shouldBe true
        NegZFiniteFloat.isValid(-0.0f) shouldBe true
        NegZFiniteFloat.isValid(0.00001f) shouldBe false
        NegZFiniteFloat.isValid(99.9f) shouldBe false
      }
    }
    describe("should offer a fromOrElse factory method that") {
      it("returns a NegZFiniteFloat if the passed Float is lesser than or equal to 0") {
        NegZFiniteFloat.fromOrElse(-50.23f, NegZFiniteFloat(-42.0f)).value shouldBe -50.23f
        NegZFiniteFloat.fromOrElse(-100.0f, NegZFiniteFloat(-42.0f)).value shouldBe -100.0f
        NegZFiniteFloat.fromOrElse(0.0f, NegZFiniteFloat(-42.0f)).value shouldBe -0.0f
      }
      it("returns a given default if the passed Float is NOT greater than 0") {
        NegZFiniteFloat.fromOrElse(0.00001f, NegZFiniteFloat(-42.0f)).value shouldBe -42.0f
        NegZFiniteFloat.fromOrElse(99.9f, NegZFiniteFloat(-42.0f)).value shouldBe -42.0f
      }
    }
    it("should offer MaxValue and MinValue factory methods") {
      NegZFiniteFloat.MaxValue shouldEqual NegZFiniteFloat(0.0f)
      NegZFiniteFloat.MinValue shouldEqual NegZFiniteFloat.from(Float.MinValue).get
    }
    it("should not offer a NegativeInfinity factory method") {
      "NegZFiniteFloat.NegativeInfinity" shouldNot compile
    }
    it("should not offer a PositiveInfinity factory method") {
      "NegZFiniteFloat.PositiveInfinity" shouldNot compile
    }
    it("should not offer a isPosInfinity method") {
      "NegZFiniteFloat(-1.0f).isPosInfinity" shouldNot compile
    }

    it("should be sortable") {
      val xs = List(NegZFiniteFloat(-2.2F), NegZFiniteFloat(-0.0F), NegZFiniteFloat(-1.1F),
        NegZFiniteFloat(-3.3F))
      xs.sorted shouldEqual List(NegZFiniteFloat(-3.3F), NegZFiniteFloat(-2.2F),
        NegZFiniteFloat(-1.1F), NegZFiniteFloat(0.0F))
    }

    describe("when created with apply method") {

      it("should compile when -8 is passed in") {
        "NegZFiniteFloat(-8)" should compile
        NegZFiniteFloat(-8).value shouldEqual -8.0F
        "NegZFiniteFloat(-8L)" should compile
        NegZFiniteFloat(-8L).value shouldEqual -8.0F
        "NegZFiniteFloat(-8.0F)" should compile
        NegZFiniteFloat(-8.0F).value shouldEqual -8.0F
      }

      it("should compile when 0 is passed in") {
        "NegZFiniteFloat(0)" should compile
        NegZFiniteFloat(0).value shouldEqual 0.0F
        "NegZFiniteFloat(0L)" should compile
        NegZFiniteFloat(0L).value shouldEqual 0.0F
        "NegZFiniteFloat(0.0F)" should compile
        NegZFiniteFloat(0.0F).value shouldEqual 0.0F
      }

      it("should not compile when 8 is passed in") {
        "NegZFiniteFloat(8)" shouldNot compile
        "NegZFiniteFloat(8L)" shouldNot compile
        "NegZFiniteFloat(8.0F)" shouldNot compile
      }

      it("should not compile when x is passed in") {
        val a: Int = -8
        "NegZFiniteFloat(a)" shouldNot compile
        val b: Long = -8L
        "NegZFiniteFloat(b)" shouldNot compile
        val c: Float = -8.0F
        "NegZFiniteFloat(c)" shouldNot compile
      }
    }
    describe("when specified as a plain-old Float") {

      def takesNegZFiniteFloat(pos: NegZFiniteFloat): Float = pos.value

      it("should compile when -8 is passed in") {
        "takesNegZFiniteFloat(-8)" should compile
        takesNegZFiniteFloat(-8) shouldEqual -8.0F
        "takesNegZFiniteFloat(-8L)" should compile
        takesNegZFiniteFloat(-8L) shouldEqual -8.0F
        "takesNegZFiniteFloat(-8.0F)" should compile
        takesNegZFiniteFloat(-8.0F) shouldEqual -8.0F
      }

      it("should compile when 0 is passed in") {
        "takesNegZFiniteFloat(0)" should compile
        takesNegZFiniteFloat(0) shouldEqual 0.0F
        "takesNegZFiniteFloat(0L)" should compile
        takesNegZFiniteFloat(0L) shouldEqual 0.0F
        "takesNegZFiniteFloat(0.0F)" should compile
        takesNegZFiniteFloat(0.0F) shouldEqual 0.0F
      }

      it("should not compile when 8 is passed in") {
        "takesNegZFiniteFloat(8)" shouldNot compile
        "takesNegZFiniteFloat(8L)" shouldNot compile
        "takesNegZFiniteFloat(8.0F)" shouldNot compile
      }

      it("should not compile when x is passed in") {
        val x: Int = -8
        "takesNegZFiniteFloat(x)" shouldNot compile
        val b: Long = -8L
        "takesNegZFiniteFloat(b)" shouldNot compile
        val c: Float = -8.0F
        "takesNegZFiniteFloat(c)" shouldNot compile
      }

      it("should offer a unary + method that is consistent with Float") {
        forAll { (p: NegZFiniteFloat) =>
          (+p).toFloat shouldEqual (+(p.toFloat))
        }
      }

      it("should offer a unary - method that returns PosZFiniteFloat") {
        forAll { (p: NegZFiniteFloat) =>
          (-p) shouldEqual (PosZFiniteFloat.ensuringValid(-(p.toFloat)))
        }
      }
    }

    it("should offer 'min' and 'max' methods that are consistent with Float") {
      forAll { (pfloat1: NegZFiniteFloat, pfloat2: NegZFiniteFloat) =>
        pfloat1.max(pfloat2).toFloat shouldEqual pfloat1.toFloat.max(pfloat2.toFloat)
        pfloat1.min(pfloat2).toFloat shouldEqual pfloat1.toFloat.min(pfloat2.toFloat)
      }
    }

    it("should offer an 'isWhole' method that is consistent with Float") {
      forAll { (pzfloat: NegZFiniteFloat) =>
        pzfloat.isWhole shouldEqual pzfloat.toFloat.isWhole
      }
    }

    it("should offer 'round', 'ceil', and 'floor' methods that are consistent with Float") {
      forAll { (pzfloat: NegZFiniteFloat) =>
        // SKIP-SCALATESTJS,NATIVE-START
        pzfloat.round.toFloat shouldEqual pzfloat.toFloat.round
        // SKIP-SCALATESTJS,NATIVE-END
        pzfloat.ceil.toFloat shouldEqual pzfloat.toFloat.ceil
        pzfloat.floor.toFloat shouldEqual pzfloat.toFloat.floor
      }
    }

    it("should offer 'toRadians' and 'toDegrees' methods that are consistent with Float") {
      forAll { (pzfloat: NegZFiniteFloat) =>
        pzfloat.toRadians.toFloat shouldEqual pzfloat.toFloat.toRadians
        pzfloat.toDegrees.toFloat shouldEqual pzfloat.toFloat.toDegrees
      }
    }

    it("should offer an ensuringValid method that takes a Float => Float, throwing AssertionError if the result is invalid") {
      NegZFiniteFloat(-33.0f).ensuringValid(_ + 1.0f) shouldEqual NegZFiniteFloat(-32.0f)
      an [AssertionError] should be thrownBy { NegZFiniteFloat.MaxValue.ensuringValid(_ - NegZFiniteFloat.MaxValue + 1) }
      an [AssertionError] should be thrownBy { NegZFiniteFloat.MaxValue.ensuringValid(_ => Float.PositiveInfinity) }
      an [AssertionError] should be thrownBy { NegZFiniteFloat.MaxValue.ensuringValid(_ => Float.NegativeInfinity) }
      // SKIP-DOTTY-START
      // https://github.com/lampepfl/dotty/issues/6710
      an [AssertionError] should be thrownBy { NegZFiniteFloat.MaxValue.ensuringValid(_ => Float.NaN) }
      // SKIP-DOTTY-END
    }
  }
}
