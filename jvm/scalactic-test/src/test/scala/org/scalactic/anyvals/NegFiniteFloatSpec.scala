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
import OptionValues._
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.prop.PropertyChecks
// SKIP-SCALATESTJS,NATIVE-START
import scala.collection.immutable.NumericRange
// SKIP-SCALATESTJS,NATIVE-END
import scala.util.{Failure, Success, Try}
import org.scalactic.{Good, Bad}
import org.scalactic.{Pass, Fail}
import org.scalactic.Equality

trait NegFiniteFloatSpecSupport {

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

class NegFiniteFloatSpec extends funspec.AnyFunSpec with matchers.should.Matchers with PropertyChecks with TypeCheckedTripleEquals with NegFiniteFloatSpecSupport {

  describe("A NegFiniteFloat") {
    describe("should offer a from factory method that") {
      it("returns Some[NegFiniteFloat] if the passed Float is lesser than 0") {
        NegFiniteFloat.from(-50.23f).value.value shouldBe -50.23f
        NegFiniteFloat.from(-100.0F).value.value shouldBe -100.0F
      }
      it("returns None if the passed Float is NOT lesser than 0") {
        NegFiniteFloat.from(0.0F) shouldBe None
        NegFiniteFloat.from(0.00001F) shouldBe None
        NegFiniteFloat.from(99.9F) shouldBe None
      }
    }
    describe("should offer an ensuringValid factory method that") {
      it("returns NegFiniteFloat if the passed Float is lesser than 0") {
        NegFiniteFloat.ensuringValid(-50.23F).value shouldBe -50.23F
        NegFiniteFloat.ensuringValid(-100.0F).value shouldBe -100.0F
      }
      it("throws AssertionError if the passed Float is NOT lesser than 0") {
        an [AssertionError] should be thrownBy NegFiniteFloat.ensuringValid(0.0F)
        an [AssertionError] should be thrownBy NegFiniteFloat.ensuringValid(0.00001F)
        an [AssertionError] should be thrownBy NegFiniteFloat.ensuringValid(99.9F)
        an [AssertionError] should be thrownBy NegFiniteFloat.ensuringValid(Float.PositiveInfinity)
        an [AssertionError] should be thrownBy NegFiniteFloat.ensuringValid(Float.NegativeInfinity)
        // SKIP-DOTTY-START
        // https://github.com/lampepfl/dotty/issues/6710
        an [AssertionError] should be thrownBy NegFiniteFloat.ensuringValid(Float.NaN)
        // SKIP-DOTTY-END
      }
    }
    describe("should offer a tryingValid factory method that") {
      import TryValues._
      it("returns a NegFiniteFloat wrapped in a Success if the passed NegFiniteFloat is lesser than 0") {
        NegFiniteFloat.tryingValid(-50.3f).success.value.value shouldBe -50.3f
        NegFiniteFloat.tryingValid(-100.0f).success.value.value shouldBe -100.0f
      }

      it("returns an AssertionError wrapped in a Failure if the passed Float is NOT lesser than 0") {
        NegFiniteFloat.tryingValid(0.0f).failure.exception shouldBe an [AssertionError]
        NegFiniteFloat.tryingValid(1.0f).failure.exception shouldBe an [AssertionError]
        NegFiniteFloat.tryingValid(99.9f).failure.exception shouldBe an [AssertionError]
      }
    }
    describe("should offer a passOrElse factory method that") {
      it("returns a Pass if the given Float is lesser than 0") {
        NegFiniteFloat.passOrElse(-50.0f)(i => i) shouldBe Pass
        NegFiniteFloat.passOrElse(-100.0f)(i => i) shouldBe Pass
      }
      it("returns an error value produced by passing the given Float to the given function if the passed Float is NOT lesser than 0, wrapped in a Fail") {
        NegFiniteFloat.passOrElse(0.0f)(i => s"$i did not taste good") shouldBe Fail(0.0f + " did not taste good")
        NegFiniteFloat.passOrElse(1.1f)(i => i) shouldBe Fail(1.1f)
        NegFiniteFloat.passOrElse(99.0f)(i => i + 3.0f) shouldBe Fail(102.0f)
      }
    }
    describe("should offer a goodOrElse factory method that") {
      it("returns a NegFiniteFloat wrapped in a Good if the given Float is lesser than 0") {
        NegFiniteFloat.goodOrElse(-50.3f)(i => i) shouldBe Good(NegFiniteFloat(-50.3f))
        NegFiniteFloat.goodOrElse(-100.0f)(i => i) shouldBe Good(NegFiniteFloat(-100.0f))
      }
      it("returns an error value produced by passing the given Float to the given function if the passed Float is NOT lesser than 0, wrapped in a Bad") {
        NegFiniteFloat.goodOrElse(0.0f)(i => s"$i did not taste good") shouldBe Bad(0.0f + " did not taste good")
        NegFiniteFloat.goodOrElse(1.1f)(i => i) shouldBe Bad(1.1f)
        NegFiniteFloat.goodOrElse(99.0f)(i => i + 3.0f) shouldBe Bad(102.0f)
      }
    }
    describe("should offer a rightOrElse factory method that") {
      it("returns a NegFiniteFloat wrapped in a Right if the given Float is lesser than 0") {
        NegFiniteFloat.rightOrElse(-50.3f)(i => i) shouldBe Right(NegFiniteFloat(-50.3f))
        NegFiniteFloat.rightOrElse(-100.0f)(i => i) shouldBe Right(NegFiniteFloat(-100.0f))
      }
      it("returns an error value produced by passing the given Float to the given function if the passed Float is NOT lesser than 0, wrapped in a Left") {
        NegFiniteFloat.rightOrElse(0.0f)(i => s"$i did not taste good") shouldBe Left(0.0f + " did not taste good")
        NegFiniteFloat.rightOrElse(1.1f)(i => i) shouldBe Left(1.1f)
        NegFiniteFloat.rightOrElse(99.9f)(i => i + 3.0f) shouldBe Left(102.9f)
      }
    }
    describe("should offer an isValid predicate method that") {
      it("returns true if the passed Float is greater than 0") {
        NegFiniteFloat.isValid(-50.23f) shouldBe true
        NegFiniteFloat.isValid(-100.0f) shouldBe true
        NegFiniteFloat.isValid(0.0f) shouldBe false
        NegFiniteFloat.isValid(-0.0f) shouldBe false
        NegFiniteFloat.isValid(0.00001f) shouldBe false
        NegFiniteFloat.isValid(99.9f) shouldBe false
      }
    }
    describe("should offer a fromOrElse factory method that") {
      it("returns a NegFiniteFloat if the passed Float is lesser than 0") {
        NegFiniteFloat.fromOrElse(-50.23f, NegFiniteFloat(-42.0f)).value shouldBe -50.23f
        NegFiniteFloat.fromOrElse(-100.0f, NegFiniteFloat(-42.0f)).value shouldBe -100.0f
      }
      it("returns a given default if the passed Float is NOT lesser than 0") {
        NegFiniteFloat.fromOrElse(0.0f, NegFiniteFloat(-42.0f)).value shouldBe -42.0f
        NegFiniteFloat.fromOrElse(0.00001f, NegFiniteFloat(-42.0f)).value shouldBe -42.0f
        NegFiniteFloat.fromOrElse(99.9f, NegFiniteFloat(-42.0f)).value shouldBe -42.0f
      }
    }
    it("should offer MaxValue and MinValue factory methods") {
      NegFiniteFloat.MaxValue shouldEqual NegFiniteFloat.from(-Float.MinPositiveValue).get
      NegFiniteFloat.MinValue shouldEqual NegFiniteFloat.from(Float.MinValue).get
    }
    it("should not offer a NegativeInfinity factory method") {
      "NegFiniteFloat.NegativeInfinity" shouldNot compile
    }
    it("should not offer a PositiveInfinity factory method") {
      "NegFiniteFloat.PositiveInfinity" shouldNot compile
    }
    it("should not offer a isNegInfinity method") {
      "NegFiniteFloat(-1.0f).isNegInfinity" shouldNot compile
    }
    it("should not offer a isPosInfinity method") {
      "NegFiniteFloat(-1.0f).isPosInfinity" shouldNot compile
    }
    it("should be sortable") {
      val xs = List(NegFiniteFloat(-2.2F), NegFiniteFloat(-4.4F), NegFiniteFloat(-1.1F),
        NegFiniteFloat(-3.3F))
      xs.sorted shouldEqual List(NegFiniteFloat(-4.4F), NegFiniteFloat(-3.3F), NegFiniteFloat(-2.2F),
        NegFiniteFloat(-1.1F))
    }

    describe("when created with apply method") {

      it("should compile when -8 is passed in") {
        "NegFiniteFloat(-8)" should compile
        NegFiniteFloat(-8).value shouldEqual -8.0F
        "NegFiniteFloat(-8L)" should compile
        NegFiniteFloat(-8L).value shouldEqual -8.0F
        "NegFiniteFloat(-8.0F)" should compile
        NegFiniteFloat(-8.0F).value shouldEqual -8.0F
      }

      it("should not compile when 0 is passed in") {
        "NegFiniteFloat(0)" shouldNot compile
        "NegFiniteFloat(0L)" shouldNot compile
        "NegFiniteFloat(0.0F)" shouldNot compile
      }

      it("should not compile when 8 is passed in") {
        "NegFiniteFloat(8)" shouldNot compile
        "NegFiniteFloat(8L)" shouldNot compile
        "NegFiniteFloat(8.0F)" shouldNot compile
      }
      it("should not compile when x is passed in") {
        val a: Int = -8
        "NegFiniteFloat(a)" shouldNot compile
        val b: Long = -8L
        "NegFiniteFloat(b)" shouldNot compile
        val c: Float = -8.0F
        "NegFiniteFloat(c)" shouldNot compile
      }
    }
    describe("when specified as a plain-old Float") {

      def takesNegFiniteFloat(pos: NegFiniteFloat): Float = pos.value

      it("should compile when -8 is passed in") {
        "takesNegFiniteFloat(-8)" should compile
        takesNegFiniteFloat(-8) shouldEqual -8.0F
        "takesNegFiniteFloat(-8L)" should compile
        takesNegFiniteFloat(-8L) shouldEqual -8.0F
        "takesNegFiniteFloat(-8.0F)" should compile
        takesNegFiniteFloat(-8.0F) shouldEqual -8.0F
      }

      it("should not compile when 0 is passed in") {
        "takesNegFiniteFloat(0)" shouldNot compile
        "takesNegFiniteFloat(0L)" shouldNot compile
        "takesNegFiniteFloat(0.0F)" shouldNot compile
      }

      it("should not compile when 8 is passed in") {
        "takesNegFiniteFloat(8)" shouldNot compile
        "takesNegFiniteFloat(8L)" shouldNot compile
        "takesNegFiniteFloat(8.0F)" shouldNot compile
      }

      it("should not compile when x is passed in") {
        val x: Int = -8
        "takesNegFiniteFloat(x)" shouldNot compile
        val b: Long = -8L
        "takesNegFiniteFloat(b)" shouldNot compile
        val c: Float = -8.0F
        "takesNegFiniteFloat(c)" shouldNot compile
      }

      it("should offer a unary + method that is consistent with Float") {
        forAll { (p: NegFiniteFloat) =>
          (+p).toFloat shouldEqual (+(p.toFloat))
        }
      }

      it("should offer a unary - method that returns PosFiniteFloat") {
        forAll { (p: NegFiniteFloat) =>
          (-p) shouldEqual (PosFiniteFloat.ensuringValid(-(p.toFloat)))
        }
      }
    }

    it("should offer 'min' and 'max' methods that are consistent with Float") {
      forAll { (pfloat1: NegFiniteFloat, pfloat2: NegFiniteFloat) =>
        pfloat1.max(pfloat2).toFloat shouldEqual pfloat1.toFloat.max(pfloat2.toFloat)
        pfloat1.min(pfloat2).toFloat shouldEqual pfloat1.toFloat.min(pfloat2.toFloat)
      }
    }

    it("should offer an 'isWhole' method that is consistent with Float") {
      forAll { (pfloat: NegFiniteFloat) =>
        pfloat.isWhole shouldEqual pfloat.toFloat.isWhole
      }
    }

    it("should offer 'round', 'ceil', and 'floor' methods that are consistent with Float") {
      forAll { (pfloat: NegFiniteFloat) =>
        // SKIP-SCALATESTJS,NATIVE-START
        pfloat.round.toFloat shouldEqual pfloat.toFloat.round
        // SKIP-SCALATESTJS,NATIVE-END
        pfloat.ceil.toFloat shouldEqual pfloat.toFloat.ceil
        pfloat.floor.toFloat shouldEqual pfloat.toFloat.floor
      }
    }

    it("should offer 'toRadians' and 'toDegrees' methods that are consistent with Float") {
      forAll { (pfloat: NegFiniteFloat) =>
        pfloat.toRadians shouldEqual pfloat.toFloat.toRadians
      }
    }
  }
  it("should offer an ensuringValid method that takes a Float => Float, throwing AssertionError if the result is invalid") {
    NegFiniteFloat(-33.0f).ensuringValid(_ + 1.0f) shouldEqual NegFiniteFloat(-32.0f)
    an [AssertionError] should be thrownBy { NegFiniteFloat.MaxValue.ensuringValid(_ - NegFiniteFloat.MaxValue) }
    an [AssertionError] should be thrownBy { NegFiniteFloat.MaxValue.ensuringValid(_ => Float.PositiveInfinity) }
    an [AssertionError] should be thrownBy { NegFiniteFloat.MaxValue.ensuringValid(_ => Float.NegativeInfinity) }
    // SKIP-DOTTY-START
    // https://github.com/lampepfl/dotty/issues/6710
    an [AssertionError] should be thrownBy { NegFiniteFloat.MaxValue.ensuringValid(_ => Float.NaN) }
    // SKIP-DOTTY-END
  }
}
