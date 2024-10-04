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

trait NegFloatSpecSupport {

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

class NegFloatSpec extends funspec.AnyFunSpec with matchers.should.Matchers with PropertyChecks with TypeCheckedTripleEquals with NegFloatSpecSupport {

  describe("A NegFloat") {
    describe("should offer a from factory method that") {
      it("returns Some[NegFloat] if the passed Float is lesser than 0") {
        NegFloat.from(-50.23f).value.value shouldBe -50.23f
        NegFloat.from(-100.0F).value.value shouldBe -100.0F
      }
      it("returns None if the passed Float is NOT lesser than 0") {
        NegFloat.from(0.0F) shouldBe None
        NegFloat.from(0.00001F) shouldBe None
        NegFloat.from(99.9F) shouldBe None
      }
    }
    describe("should offer an ensuringValid factory method that") {
      it("returns NegFloat if the passed Float is lesser than 0") {
        NegFloat.ensuringValid(-50.23F).value shouldBe -50.23F
        NegFloat.ensuringValid(-100.0F).value shouldBe -100.0F
        NegFloat.ensuringValid(Float.NegativeInfinity).value shouldBe Float.NegativeInfinity
      }
      it("throws AssertionError if the passed Float is NOT lesser than 0") {
        an [AssertionError] should be thrownBy NegFloat.ensuringValid(0.0F)
        an [AssertionError] should be thrownBy NegFloat.ensuringValid(0.00001F)
        an [AssertionError] should be thrownBy NegFloat.ensuringValid(99.9F)
        an [AssertionError] should be thrownBy NegFloat.ensuringValid(Float.PositiveInfinity)
        // SKIP-DOTTY-START
        // https://github.com/lampepfl/dotty/issues/6710
        an [AssertionError] should be thrownBy NegFloat.ensuringValid(Float.NaN)
        // SKIP-DOTTY-END
      }
    }
    describe("should offer a tryingValid factory method that") {
      import TryValues._
      it("returns a NegFloat wrapped in a Success if the passed NegFloat is lesser than 0") {
        NegFloat.tryingValid(-50.3f).success.value.value shouldBe -50.3f
        NegFloat.tryingValid(-100.0f).success.value.value shouldBe -100.0f
      }

      it("returns an AssertionError wrapped in a Failure if the passed Float is NOT lesser than 0") {
        NegFloat.tryingValid(0.0f).failure.exception shouldBe an [AssertionError]
        NegFloat.tryingValid(1.0f).failure.exception shouldBe an [AssertionError]
        NegFloat.tryingValid(99.9f).failure.exception shouldBe an [AssertionError]
      }
    }
    describe("should offer a passOrElse factory method that") {
      it("returns a Pass if the given Float is lesser than 0") {
        NegFloat.passOrElse(-50.0f)(i => i) shouldBe Pass
        NegFloat.passOrElse(-100.0f)(i => i) shouldBe Pass
      }
      it("returns an error value produced by passing the given Float to the given function if the passed Float is NOT lesser than 0, wrapped in a Fail") {
        NegFloat.passOrElse(0.0f)(i => s"$i did not taste good") shouldBe Fail(0.0f + " did not taste good")
        NegFloat.passOrElse(1.1f)(i => i) shouldBe Fail(1.1f)
        NegFloat.passOrElse(99.0f)(i => i + 3.0f) shouldBe Fail(102.0f)
      }
    }
    describe("should offer a goodOrElse factory method that") {
      it("returns a NegFloat wrapped in a Good if the given Float is lesser than 0") {
        NegFloat.goodOrElse(-50.3f)(i => i) shouldBe Good(NegFloat(-50.3f))
        NegFloat.goodOrElse(-100.0f)(i => i) shouldBe Good(NegFloat(-100.0f))
      }
      it("returns an error value produced by passing the given Float to the given function if the passed Float is NOT lesser than 0, wrapped in a Bad") {
        NegFloat.goodOrElse(0.0f)(i => s"$i did not taste good") shouldBe Bad(0.0f + " did not taste good")
        NegFloat.goodOrElse(1.1f)(i => i) shouldBe Bad(1.1f)
        NegFloat.goodOrElse(99.0f)(i => i + 3.0f) shouldBe Bad(102.0f)
      }
    }
    describe("should offer a rightOrElse factory method that") {
      it("returns a NegFloat wrapped in a Right if the given Float is lesser than 0") {
        NegFloat.rightOrElse(-50.3f)(i => i) shouldBe Right(NegFloat(-50.3f))
        NegFloat.rightOrElse(-100.0f)(i => i) shouldBe Right(NegFloat(-100.0f))
      }
      it("returns an error value produced by passing the given Float to the given function if the passed Float is NOT lesser than 0, wrapped in a Left") {
        NegFloat.rightOrElse(0.0f)(i => s"$i did not taste good") shouldBe Left(0.0f + " did not taste good")
        NegFloat.rightOrElse(1.1f)(i => i) shouldBe Left(1.1f)
        NegFloat.rightOrElse(99.9f)(i => i + 3.0f) shouldBe Left(102.9f)
      }
    }
    describe("should offer an isValid predicate method that") {
      it("returns true if the passed Float is greater than 0") {
        NegFloat.isValid(-50.23f) shouldBe true
        NegFloat.isValid(-100.0f) shouldBe true
        NegFloat.isValid(0.0f) shouldBe false
        NegFloat.isValid(-0.0f) shouldBe false
        NegFloat.isValid(0.00001f) shouldBe false
        NegFloat.isValid(99.9f) shouldBe false
      }
    }
    describe("should offer a fromOrElse factory method that") {
      it("returns a NegFloat if the passed Float is lesser than 0") {
        NegFloat.fromOrElse(-50.23f, NegFloat(-42.0f)).value shouldBe -50.23f
        NegFloat.fromOrElse(-100.0f, NegFloat(-42.0f)).value shouldBe -100.0f
      }
      it("returns a given default if the passed Float is NOT lesser than 0") {
        NegFloat.fromOrElse(0.0f, NegFloat(-42.0f)).value shouldBe -42.0f
        NegFloat.fromOrElse(0.00001f, NegFloat(-42.0f)).value shouldBe -42.0f
        NegFloat.fromOrElse(99.9f, NegFloat(-42.0f)).value shouldBe -42.0f
      }
    }
    it("should offer MaxValue and MinValue factory methods") {
      NegFloat.MaxValue shouldEqual NegFloat.from(-Float.MinPositiveValue).get
      NegFloat.MinValue shouldEqual NegFloat.from(Float.MinValue).get
    }
    it("should offer a NegativeInfinity factory method") {
      NegFloat.NegativeInfinity shouldEqual NegFloat.ensuringValid(Float.NegativeInfinity)
    }
    it("should offer a PositiveInfinity factory method") {
      "NegFloat.PositiveInfinity" shouldNot compile
    }
    it("should offer a isNegInfinity method that returns true if the instance is NegativeInfinity") {
      NegFloat.ensuringValid(Float.NegativeInfinity).isNegInfinity shouldBe true
      NegFloat(-1.0f).isNegInfinity shouldBe false
    }
    it("should not offer a isPosInfinity method") {
      "NegFloat(-1.0f).isPosInfinity" shouldNot compile
    }
    it("should be sortable") {
      val xs = List(NegFloat(-2.2F), NegFloat(-4.4F), NegFloat(-1.1F),
        NegFloat(-3.3F))
      xs.sorted shouldEqual List(NegFloat(-4.4F), NegFloat(-3.3F), NegFloat(-2.2F),
        NegFloat(-1.1F))
    }

    describe("when created with apply method") {

      it("should compile when -8 is passed in") {
        "NegFloat(-8)" should compile
        NegFloat(-8).value shouldEqual -8.0F
        "NegFloat(-8L)" should compile
        NegFloat(-8L).value shouldEqual -8.0F
        "NegFloat(-8.0F)" should compile
        NegFloat(-8.0F).value shouldEqual -8.0F
      }

      it("should not compile when 0 is passed in") {
        "NegFloat(0)" shouldNot compile
        "NegFloat(0L)" shouldNot compile
        "NegFloat(0.0F)" shouldNot compile
      }

      it("should not compile when 8 is passed in") {
        "NegFloat(8)" shouldNot compile
        "NegFloat(8L)" shouldNot compile
        "NegFloat(8.0F)" shouldNot compile
      }
      it("should not compile when x is passed in") {
        val a: Int = -8
        "NegFloat(a)" shouldNot compile
        val b: Long = -8L
        "NegFloat(b)" shouldNot compile
        val c: Float = -8.0F
        "NegFloat(c)" shouldNot compile
      }
    }
    describe("when specified as a plain-old Float") {

      def takesNegFloat(pos: NegFloat): Float = pos.value

      it("should compile when -8 is passed in") {
        "takesNegFloat(-8)" should compile
        takesNegFloat(-8) shouldEqual -8.0F
        "takesNegFloat(-8L)" should compile
        takesNegFloat(-8L) shouldEqual -8.0F
        "takesNegFloat(-8.0F)" should compile
        takesNegFloat(-8.0F) shouldEqual -8.0F
      }

      it("should not compile when 0 is passed in") {
        "takesNegFloat(0)" shouldNot compile
        "takesNegFloat(0L)" shouldNot compile
        "takesNegFloat(0.0F)" shouldNot compile
      }

      it("should not compile when 8 is passed in") {
        "takesNegFloat(8)" shouldNot compile
        "takesNegFloat(8L)" shouldNot compile
        "takesNegFloat(8.0F)" shouldNot compile
      }

      it("should not compile when x is passed in") {
        val x: Int = -8
        "takesNegFloat(x)" shouldNot compile
        val b: Long = -8L
        "takesNegFloat(b)" shouldNot compile
        val c: Float = -8.0F
        "takesNegFloat(c)" shouldNot compile
      }

      it("should offer a unary + method that is consistent with Float") {
        forAll { (p: NegFloat) =>
          (+p).toFloat shouldEqual (+(p.toFloat))
        }
      }

      it("should offer a unary - method that returns PosFloat") {
        forAll { (p: NegFloat) =>
          (-p) shouldEqual (PosFloat.ensuringValid(-(p.toFloat)))
        }
      }
    }

    it("should offer a 'plus' method that takes a NegZFloat and returns a NegFloat") {

      forAll { (negFloat: NegFloat, negZFloat: NegZFloat) =>
        (negFloat plus negZFloat) should === (NegFloat.ensuringValid(negFloat.value + negZFloat.value))
      }

      val examples =
        Table(
          (                "negFloat",                "negZFloat" ),
          (         NegFloat.MinValue,         NegZFloat.MinValue ),
          (         NegFloat.MinValue,         NegZFloat.MaxValue ),
          (         NegFloat.MinValue, NegZFloat.NegativeInfinity ),
          (         NegFloat.MaxValue,         NegZFloat.MinValue ),
          (         NegFloat.MaxValue,         NegZFloat.MaxValue ),
          (         NegFloat.MaxValue, NegZFloat.NegativeInfinity ),
          ( NegFloat.NegativeInfinity,         NegZFloat.MinValue ),
          ( NegFloat.NegativeInfinity,         NegZFloat.MaxValue ),
          ( NegFloat.NegativeInfinity, NegZFloat.NegativeInfinity )
        )

      forAll (examples) { (a, b) =>
        (a plus b).value should be < 0.0f
      }

      /*
      error] /Users/bv/nobkp/delus/st-add-to-3.1.x/scalactic-test/src/test/scala/org/scalactic/anyvals/NegFloatSpec.scala:392: type mismatch;
      [error]  found   : Double(3.0)
      [error]  required: Float
      [error]       (NegFloat(1.0) plus PosInt(2)) should === (NegFloat(3.0))
      [error]                                                           ^

      You know, I wonder if our macro could be friendlier and allow a Double literal for
      specifying floats so long as it is in the valid range for floats.
      */
      // Sanity check that implicit widening conversions work too.
      (NegFloat(-1.0f) plus NegInt(-2)) should === (NegFloat(-3.0f))
    }

    it("should offer 'min' and 'max' methods that are consistent with Float") {
      forAll { (pfloat1: NegFloat, pfloat2: NegFloat) =>
        pfloat1.max(pfloat2).toFloat shouldEqual pfloat1.toFloat.max(pfloat2.toFloat)
        pfloat1.min(pfloat2).toFloat shouldEqual pfloat1.toFloat.min(pfloat2.toFloat)
      }
    }

    it("should offer an 'isWhole' method that is consistent with Float") {
      forAll { (pfloat: NegFloat) =>
        pfloat.isWhole shouldEqual pfloat.toFloat.isWhole
      }
    }

    it("should offer 'round', 'ceil', and 'floor' methods that are consistent with Float") {
      forAll { (pfloat: NegFloat) =>
        // SKIP-SCALATESTJS,NATIVE-START
        pfloat.round.toFloat shouldEqual pfloat.toFloat.round
        // SKIP-SCALATESTJS,NATIVE-END
        pfloat.ceil.toFloat shouldEqual pfloat.toFloat.ceil
        pfloat.floor.toFloat shouldEqual pfloat.toFloat.floor
      }
    }

    it("should offer 'toRadians' and 'toDegrees' methods that are consistent with Float") {
      forAll { (pfloat: NegFloat) =>
        pfloat.toRadians shouldEqual pfloat.toFloat.toRadians
      }
    }
    it("should offer an isFinite method that returns true if the value does not represent infinity") {
      forAll { (n: NegFiniteFloat) =>
        (n: NegFloat).isFinite should be (true)
        NegFloat.NegativeInfinity.isFinite should be (false)
      }
    }
  }
  it("should offer an ensuringValid method that takes a Float => Float, throwing AssertionError if the result is invalid") {
    NegFloat(-33.0f).ensuringValid(_ + 1.0f) shouldEqual NegFloat(-32.0f)
    NegFloat(-33.0f).ensuringValid(_ => Float.NegativeInfinity) shouldEqual NegFloat.ensuringValid(Float.NegativeInfinity)
    an [AssertionError] should be thrownBy { NegFloat.MaxValue.ensuringValid(_ - NegFloat.MaxValue) }
    an [AssertionError] should be thrownBy { NegFloat.MaxValue.ensuringValid(_ => Float.PositiveInfinity) }
    // SKIP-DOTTY-START
    // https://github.com/lampepfl/dotty/issues/6710
    an [AssertionError] should be thrownBy { NegFloat.MaxValue.ensuringValid(_ => Float.NaN) }
    // SKIP-DOTTY-END
  }
}
