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

trait FiniteFloatSpecSupport {

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

class FiniteFloatSpec extends funspec.AnyFunSpec with matchers.should.Matchers with PropertyChecks with TypeCheckedTripleEquals with FiniteFloatSpecSupport {

  describe("A FiniteFloat") {
    describe("should offer a from factory method that") {
      it("returns Some[FiniteFloat] if the passed Float is finite") {
        FiniteFloat.from(50.23F).value.value shouldBe 50.23F
        FiniteFloat.from(100.0F).value.value shouldBe 100.0F
        FiniteFloat.from(0.0F).value.value shouldBe 0.0f
        FiniteFloat.from(-0.00001F).value.value shouldBe -0.00001F
        FiniteFloat.from(-99.9F).value.value shouldBe -99.9F
      }
      it("returns None if the passed Float is infinite") {
        FiniteFloat.from(Float.NegativeInfinity) shouldBe None
        FiniteFloat.from(Float.PositiveInfinity) shouldBe None
      }
    }
    describe("should offer an ensuringValid factory method that") {
      it("returns FiniteFloat if the passed Float is finite") {
        FiniteFloat.ensuringValid(50.23F).value shouldBe 50.23F
        FiniteFloat.ensuringValid(100.0F).value shouldBe 100.0F
        FiniteFloat.ensuringValid(50.23F).value shouldBe 50.23F
        FiniteFloat.ensuringValid(0.0F).value shouldBe 0.0F
        FiniteFloat.ensuringValid(-0.00001F).value shouldBe -0.00001F
        FiniteFloat.ensuringValid(-99.9F).value shouldBe -99.9F
      }
      it("throws AssertionError if the passed Float is infinite") {
        an [AssertionError] should be thrownBy FiniteFloat.ensuringValid(Float.PositiveInfinity)
        an [AssertionError] should be thrownBy FiniteFloat.ensuringValid(Float.NegativeInfinity)
        // SKIP-DOTTY-START
        // https://github.com/lampepfl/dotty/issues/6710
        an [AssertionError] should be thrownBy FiniteFloat.ensuringValid(Float.NaN)
        // SKIP-DOTTY-END
      }
    }
    describe("should offer a tryingValid factory method that") {
      import TryValues._
      it("returns a FiniteFloat wrapped in a Success if the passed FiniteFloat is finite") {
        FiniteFloat.tryingValid(50.3f).success.value.value shouldBe 50.3f
        FiniteFloat.tryingValid(100.0f).success.value.value shouldBe 100.0f
        FiniteFloat.tryingValid(0.0f).success.value.value shouldBe 0.0f
        FiniteFloat.tryingValid(-1.0f).success.value.value shouldBe -1.0f
        FiniteFloat.tryingValid(-99.9f).success.value.value shouldBe -99.9f
      }

      it("returns an AssertionError wrapped in a Failure if the passed Float is infinite") {
        FiniteFloat.tryingValid(Float.PositiveInfinity).failure.exception shouldBe an [AssertionError]
        FiniteFloat.tryingValid(Float.NegativeInfinity).failure.exception shouldBe an [AssertionError]
      }
    }
    describe("should offer a passOrElse factory method that") {
      it("returns a Pass if the given Float is finite") {
        FiniteFloat.passOrElse(50.0f)(i => i) shouldBe Pass
        FiniteFloat.passOrElse(100.0f)(i => i) shouldBe Pass
        FiniteFloat.passOrElse(0.0f)(i => i) shouldBe Pass
        FiniteFloat.passOrElse(-1.1f)(i => i) shouldBe Pass
        FiniteFloat.passOrElse(-99.0f)(i => i) shouldBe Pass
      }
      it("returns an error value produced by passing the given Float to the given function if the passed Float is infinite, wrapped in a Fail") {
        FiniteFloat.passOrElse(Float.PositiveInfinity)(i => s"$i did not taste good") shouldBe Fail("Infinity did not taste good")
        FiniteFloat.passOrElse(Float.NegativeInfinity)(i => s"$i did not taste good") shouldBe Fail("-Infinity did not taste good")
      }
    }
    describe("should offer a goodOrElse factory method that") {
      it("returns a FiniteFloat wrapped in a Good if the given Float is finite") {
        FiniteFloat.goodOrElse(50.3f)(i => i) shouldBe Good(FiniteFloat(50.3f))
        FiniteFloat.goodOrElse(100.0f)(i => i) shouldBe Good(FiniteFloat(100.0f))
        FiniteFloat.goodOrElse(0.0f)(i => i) shouldBe Good(FiniteFloat(0.0f))
        FiniteFloat.goodOrElse(-1.1f)(i => i) shouldBe Good(FiniteFloat(-1.1f))
        FiniteFloat.goodOrElse(-99.0f)(i => i) shouldBe Good(FiniteFloat(-99.0f))
      }
      it("returns an error value produced by passing the given Float to the given function if the passed Float is infinite, wrapped in a Bad") {
        FiniteFloat.goodOrElse(Float.NegativeInfinity)(i => s"$i did not taste good") shouldBe Bad("-Infinity did not taste good")
        FiniteFloat.goodOrElse(Float.PositiveInfinity)(i => s"$i did not taste good") shouldBe Bad("Infinity did not taste good")
      }
    }
    describe("should offer a rightOrElse factory method that") {
      it("returns a FiniteFloat wrapped in a Right if the given Float is finite") {
        FiniteFloat.rightOrElse(50.3f)(i => i) shouldBe Right(FiniteFloat(50.3f))
        FiniteFloat.rightOrElse(100.0f)(i => i) shouldBe Right(FiniteFloat(100.0f))
        FiniteFloat.rightOrElse(0.0f)(i => i) shouldBe Right(FiniteFloat(0.0f))
        FiniteFloat.rightOrElse(-1.1f)(i => i) shouldBe Right(FiniteFloat(-1.1f))
        FiniteFloat.rightOrElse(-99.9f)(i => i) shouldBe Right(FiniteFloat(-99.9f))
      }
      it("returns an error value produced by passing the given Float to the given function if the passed Float is infinite, wrapped in a Left") {
        FiniteFloat.rightOrElse(Float.NegativeInfinity)(i => s"$i did not taste good") shouldBe Left("-Infinity did not taste good")
        FiniteFloat.rightOrElse(Float.PositiveInfinity)(i => s"$i did not taste good") shouldBe Left("Infinity did not taste good")
      }
    }
    describe("should offer an isValid predicate method that") {
      it("returns true if the passed Float is greater than 0") {
        FiniteFloat.isValid(50.23f) shouldBe true
        FiniteFloat.isValid(100.0f) shouldBe true
        FiniteFloat.isValid(0.0f) shouldBe true
        FiniteFloat.isValid(-0.0f) shouldBe true
        FiniteFloat.isValid(-0.00001f) shouldBe true
        FiniteFloat.isValid(-99.9f) shouldBe true
        FiniteFloat.isValid(Float.NaN) shouldBe false
        FiniteFloat.isValid(Float.PositiveInfinity) shouldBe false
        FiniteFloat.isValid(Float.NegativeInfinity) shouldBe false
      }
    }
    describe("should offer a fromOrElse factory method that") {
      it("returns a FiniteFloat if the passed Float is finite") {
        FiniteFloat.fromOrElse(50.23f, FiniteFloat(42.0f)).value shouldBe 50.23f
        FiniteFloat.fromOrElse(100.0f, FiniteFloat(42.0f)).value shouldBe 100.0f
        FiniteFloat.fromOrElse(0.0f, FiniteFloat(42.0f)).value shouldBe 0.0f
        FiniteFloat.fromOrElse(-0.00001f, FiniteFloat(42.0f)).value shouldBe -0.00001f
        FiniteFloat.fromOrElse(-99.9f, FiniteFloat(42.0f)).value shouldBe -99.9f
      }
      it("returns a given default if the passed Float is infinite") {
        FiniteFloat.fromOrElse(Float.NegativeInfinity, FiniteFloat(42.0f)).value shouldBe 42.0f
        FiniteFloat.fromOrElse(Float.PositiveInfinity, FiniteFloat(42.0f)).value shouldBe 42.0f
      }
    }
    it("should offer MaxValue and MinValue factory methods") {
      FiniteFloat.MaxValue shouldEqual FiniteFloat.from(Float.MaxValue).get
      FiniteFloat.MinValue shouldEqual FiniteFloat.from(Float.MinValue).get
    }
    it("should not offer a PositiveInfinity factory method") {
      "FiniteFloat.PositiveInfinity" shouldNot compile
    }
    it("should not offer a NegativeInfinity factory method") {
      "FiniteFloat.NegativeInfinity" shouldNot compile
    }
    it("should not offer a isPosInfinity method") {
      "FiniteFloat(1.0f).isPosInfinity" shouldNot compile
    }
    it("should not offer a isNegInfinity method") {
      "FiniteFloat(1.0f).isNegInfinity" shouldNot compile
    }

    it("should be sortable") {
      val xs = List(FiniteFloat(2.2F), FiniteFloat(4.4F), FiniteFloat(1.1F),
        FiniteFloat(3.3F))
      xs.sorted shouldEqual List(FiniteFloat(1.1F), FiniteFloat(2.2F), FiniteFloat(3.3F),
        FiniteFloat(4.4F))
    }

    describe("when created with apply method") {

      it("should compile when 8 is passed in") {
        "FiniteFloat(8)" should compile
        FiniteFloat(8).value shouldEqual 8.0F
        "FiniteFloat(8L)" should compile
        FiniteFloat(8L).value shouldEqual 8.0F
        "FiniteFloat(8.0F)" should compile
        FiniteFloat(8.0F).value shouldEqual 8.0F
      }

      it("should compile when 0 is passed in") {
        "FiniteFloat(0)" should compile
        FiniteFloat(0).value shouldEqual 0.0F
        "FiniteFloat(0L)" should compile
        FiniteFloat(0L).value shouldEqual 0.0F
        "FiniteFloat(0.0F)" should compile
        FiniteFloat(0.0F).value shouldEqual 0.0F
      }

      it("should compile when -8 is passed in") {
        "FiniteFloat(-8)" should compile
        FiniteFloat(-8).value shouldEqual -8.0F
        "FiniteFloat(-8L)" should compile
        FiniteFloat(-8L).value shouldEqual -8.0F
        "FiniteFloat(-8.0F)" should compile
        FiniteFloat(-8.0F).value shouldEqual -8.0F
      }

      it("should not compile when Float.NegativeInfinity is passed in") {
        "FiniteFloat(Float.NegativeInfinity)" shouldNot compile
      }

      it("should not compile when Float.PositiveInfinity is passed in") {
        "FiniteFloat(Float.PositiveInfinity)" shouldNot compile
      }
      it("should not compile when x is passed in") {
        val a: Int = -8
        "FiniteFloat(a)" shouldNot compile
        val b: Long = -8L
        "FiniteFloat(b)" shouldNot compile
        val c: Float = -8.0F
        "FiniteFloat(c)" shouldNot compile
      }
    }
    describe("when specified as a plain-old Float") {

      def takesFiniteFloat(pos: FiniteFloat): Float = pos.value

      it("should compile when 8 is passed in") {
        "takesFiniteFloat(8)" should compile
        takesFiniteFloat(8) shouldEqual 8.0F
        "takesFiniteFloat(8L)" should compile
        takesFiniteFloat(8L) shouldEqual 8.0F
        "takesFiniteFloat(8.0F)" should compile
        takesFiniteFloat(8.0F) shouldEqual 8.0F
      }

      it("should compile when 0 is passed in") {
        "takesFiniteFloat(0)" should compile
        takesFiniteFloat(0) shouldEqual 0.0F
        "takesFiniteFloat(0L)" should compile
        takesFiniteFloat(0L) shouldEqual 0.0F
        "takesFiniteFloat(0.0F)" should compile
        takesFiniteFloat(0.0F) shouldEqual 0.0F
      }

      it("should compile when -8 is passed in") {
        "takesFiniteFloat(-8)" should compile
        takesFiniteFloat(-8) shouldEqual -8.0F
        "takesFiniteFloat(-8.0F)" should compile
        takesFiniteFloat(-8.0F) shouldEqual -8.0F
      }

      it("should not compile when Float.NegativeInfinity is passed in") {
        "takesFiniteFloat(Float.NegativeInfinity)" shouldNot compile
      }

      it("should not compile when Float.PositiveInfinity is passed in") {
        "takesFiniteFloat(Float.PositiveInfinity)" shouldNot compile
      }

      it("should not compile when x is passed in") {
        val x: Int = -8
        "takesFiniteFloat(x)" shouldNot compile
        val b: Long = -8L
        "takesFiniteFloat(b)" shouldNot compile
        val c: Float = -8.0F
        "takesFiniteFloat(c)" shouldNot compile
      }

      it("should offer a unary + method that is consistent with Float") {
        forAll { (v: FiniteFloat) =>
          (+v).toFloat shouldEqual (+(v.toFloat))
        }
      }

      it("should offer a unary - method that returns another FiniteFloat") {
        forAll { (v: FiniteFloat) =>
          (-v) shouldEqual (FiniteFloat.ensuringValid(-(v.toFloat)))
        }
      }
    }

    it("should offer 'min' and 'max' methods that are consistent with Float") {
      forAll { (pfloat1: FiniteFloat, pfloat2: FiniteFloat) =>
        pfloat1.max(pfloat2).toFloat shouldEqual pfloat1.toFloat.max(pfloat2.toFloat)
        pfloat1.min(pfloat2).toFloat shouldEqual pfloat1.toFloat.min(pfloat2.toFloat)
      }
    }

    it("should offer an 'isWhole' method that is consistent with Float") {
      forAll { (pfloat: FiniteFloat) =>
        pfloat.isWhole shouldEqual pfloat.toFloat.isWhole
      }
    }

    it("should offer 'round', 'ceil', and 'floor' methods that are consistent with Float") {
      forAll { (pfloat: FiniteFloat) =>
        // SKIP-SCALATESTJS,NATIVE-START
        pfloat.round.toFloat shouldEqual pfloat.toFloat.round
        // SKIP-SCALATESTJS,NATIVE-END
        pfloat.ceil.toFloat shouldEqual pfloat.toFloat.ceil
        pfloat.floor.toFloat shouldEqual pfloat.toFloat.floor
      }
    }

    it("should offer 'toRadians' and 'toDegrees' methods that are consistent with Float") {
      forAll { (pfloat: FiniteFloat) =>
        pfloat.toRadians shouldEqual pfloat.toFloat.toRadians
      }
    }
  }
  it("should offer an ensuringValid method that takes a Float => Float, throwing AssertionError if the result is invalid") {
    FiniteFloat(2.0f).ensuringValid(_ + 1.0f) shouldEqual FiniteFloat(3.0f)
    FiniteFloat(0.0f).ensuringValid(_ + Float.MinValue) shouldEqual FiniteFloat.MinValue
    FiniteFloat(0.0f).ensuringValid(_ + Float.MaxValue) shouldEqual FiniteFloat.MaxValue
    FiniteFloat(0.0f).ensuringValid(_ + Float.MinPositiveValue + 0.0f) shouldEqual FiniteFloat.MinPositiveValue
    an [AssertionError] should be thrownBy { FiniteFloat.MaxValue.ensuringValid(_ => Float.PositiveInfinity) }
    an [AssertionError] should be thrownBy { FiniteFloat.MaxValue.ensuringValid(_ => Float.NegativeInfinity) }
  }
}
