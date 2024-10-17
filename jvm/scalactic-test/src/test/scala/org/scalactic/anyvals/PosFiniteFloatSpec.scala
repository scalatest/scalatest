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

trait PosFiniteFloatSpecSupport {

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

class PosFiniteFloatSpec extends funspec.AnyFunSpec with matchers.should.Matchers with PropertyChecks with TypeCheckedTripleEquals with PosFiniteFloatSpecSupport {

  describe("A PosFiniteFloat") {
    describe("should offer a from factory method that") {
      it("returns Some[PosFiniteFloat] if the passed Float is greater than 0") {
        PosFiniteFloat.from(50.23F).value.value shouldBe 50.23F
        PosFiniteFloat.from(100.0F).value.value shouldBe 100.0F
      }
      it("returns None if the passed Float is NOT greater than 0") {
        PosFiniteFloat.from(0.0F) shouldBe None
        PosFiniteFloat.from(-0.00001F) shouldBe None
        PosFiniteFloat.from(-99.9F) shouldBe None
      }
    }
    describe("should offer an ensuringValid factory method that") {
      it("returns PosFiniteFloat if the passed Float is greater than 0") {
        PosFiniteFloat.ensuringValid(50.23F).value shouldBe 50.23F
        PosFiniteFloat.ensuringValid(100.0F).value shouldBe 100.0F
      }
      it("throws AssertionError if the passed Float is NOT greater than 0") {
        an [AssertionError] should be thrownBy PosFiniteFloat.ensuringValid(0.0F)
        an [AssertionError] should be thrownBy PosFiniteFloat.ensuringValid(-0.00001F)
        an [AssertionError] should be thrownBy PosFiniteFloat.ensuringValid(-99.9F)
        an [AssertionError] should be thrownBy PosFiniteFloat.ensuringValid(Float.PositiveInfinity)
        an [AssertionError] should be thrownBy PosFiniteFloat.ensuringValid(Float.NegativeInfinity)
        // SKIP-DOTTY-START
        // https://github.com/lampepfl/dotty/issues/6710
        an [AssertionError] should be thrownBy PosFiniteFloat.ensuringValid(Float.NaN)
        // SKIP-DOTTY-END
      }
    }
    describe("should offer a tryingValid factory method that") {
      import TryValues._
      it("returns a PosFiniteFloat wrapped in a Success if the passed PosFiniteFloat is greater than 0") {
        PosFiniteFloat.tryingValid(50.3f).success.value.value shouldBe 50.3f
        PosFiniteFloat.tryingValid(100.0f).success.value.value shouldBe 100.0f
      }

      it("returns an AssertionError wrapped in a Failure if the passed Float is NOT greater than 0") {
        PosFiniteFloat.tryingValid(0.0f).failure.exception shouldBe an [AssertionError]
        PosFiniteFloat.tryingValid(-1.0f).failure.exception shouldBe an [AssertionError]
        PosFiniteFloat.tryingValid(-99.9f).failure.exception shouldBe an [AssertionError]
      }
    }
    describe("should offer a passOrElse factory method that") {
      it("returns a Pass if the given Float is greater than 0") {
        PosFiniteFloat.passOrElse(50.0f)(i => i) shouldBe Pass
        PosFiniteFloat.passOrElse(100.0f)(i => i) shouldBe Pass
      }
      it("returns an error value produced by passing the given Float to the given function if the passed Float is NOT greater than 0, wrapped in a Fail") {
        PosFiniteFloat.passOrElse(0.0f)(i => s"$i did not taste good") shouldBe Fail(0.0f + " did not taste good")
        PosFiniteFloat.passOrElse(-1.1f)(i => i) shouldBe Fail(-1.1f)
        PosFiniteFloat.passOrElse(-99.0f)(i => i + 3.0f) shouldBe Fail(-96.0f)
      }
    }
    describe("should offer a goodOrElse factory method that") {
      it("returns a PosFiniteFloat wrapped in a Good if the given Float is greater than 0") {
        PosFiniteFloat.goodOrElse(50.3f)(i => i) shouldBe Good(PosFiniteFloat(50.3f))
        PosFiniteFloat.goodOrElse(100.0f)(i => i) shouldBe Good(PosFiniteFloat(100.0f))
      }
      it("returns an error value produced by passing the given Float to the given function if the passed Float is NOT greater than 0, wrapped in a Bad") {
        PosFiniteFloat.goodOrElse(0.0f)(i => s"$i did not taste good") shouldBe Bad(0.0f + " did not taste good")
        PosFiniteFloat.goodOrElse(-1.1f)(i => i) shouldBe Bad(-1.1f)
        PosFiniteFloat.goodOrElse(-99.0f)(i => i + 3.0f) shouldBe Bad(-96.0f)
      }
    }
    describe("should offer a rightOrElse factory method that") {
      it("returns a PosFiniteFloat wrapped in a Right if the given Float is greater than 0") {
        PosFiniteFloat.rightOrElse(50.3f)(i => i) shouldBe Right(PosFiniteFloat(50.3f))
        PosFiniteFloat.rightOrElse(100.0f)(i => i) shouldBe Right(PosFiniteFloat(100.0f))
      }
      it("returns an error value produced by passing the given Float to the given function if the passed Float is NOT greater than 0, wrapped in a Left") {
        PosFiniteFloat.rightOrElse(0.0f)(i => s"$i did not taste good") shouldBe Left(0.0f + " did not taste good")
        PosFiniteFloat.rightOrElse(-1.1f)(i => i) shouldBe Left(-1.1f)
        PosFiniteFloat.rightOrElse(-99.9f)(i => i + 3.0f) shouldBe Left(-96.9f)
      }
    }
    describe("should offer an isValid predicate method that") {
      it("returns true if the passed Float is greater than 0") {
        PosFiniteFloat.isValid(50.23f) shouldBe true
        PosFiniteFloat.isValid(100.0f) shouldBe true
        PosFiniteFloat.isValid(0.0f) shouldBe false
        PosFiniteFloat.isValid(-0.0f) shouldBe false
        PosFiniteFloat.isValid(-0.00001f) shouldBe false
        PosFiniteFloat.isValid(-99.9f) shouldBe false
      }
    }
    describe("should offer a fromOrElse factory method that") {
      it("returns a PosFiniteFloat if the passed Float is greater than 0") {
        PosFiniteFloat.fromOrElse(50.23f, PosFiniteFloat(42.0f)).value shouldBe 50.23f
        PosFiniteFloat.fromOrElse(100.0f, PosFiniteFloat(42.0f)).value shouldBe 100.0f
      }
      it("returns a given default if the passed Float is NOT greater than 0") {
        PosFiniteFloat.fromOrElse(0.0f, PosFiniteFloat(42.0f)).value shouldBe 42.0f
        PosFiniteFloat.fromOrElse(-0.00001f, PosFiniteFloat(42.0f)).value shouldBe 42.0f
        PosFiniteFloat.fromOrElse(-99.9f, PosFiniteFloat(42.0f)).value shouldBe 42.0f
      }
    }
    it("should offer MaxValue and MinValue factory methods") {
      PosFiniteFloat.MaxValue shouldEqual PosFiniteFloat.from(Float.MaxValue).get
      PosFiniteFloat.MinValue shouldEqual
        PosFiniteFloat.from(Float.MinPositiveValue).get
    }
    it("should not offer a PositiveInfinity factory method") {
      "PosFiniteFloat.PositiveInfinity" shouldNot compile
    }
    it("should not offer a NegativeInfinity factory method") {
      "PosFiniteFloat.NegativeInfinity" shouldNot compile
    }
    it("should not offer a isPosInfinity method") {
      "PosFiniteFloat(1.0f).isPosInfinity" shouldNot compile
    }
    it("should not offer a isNegInfinity method") {
      "PosFiniteFloat(1.0f).isNegInfinity" shouldNot compile
    }

    it("should be sortable") {
      val xs = List(PosFiniteFloat(2.2F), PosFiniteFloat(4.4F), PosFiniteFloat(1.1F),
        PosFiniteFloat(3.3F))
      xs.sorted shouldEqual List(PosFiniteFloat(1.1F), PosFiniteFloat(2.2F), PosFiniteFloat(3.3F),
        PosFiniteFloat(4.4F))
    }

    describe("when created with apply method") {

      it("should compile when 8 is passed in") {
        "PosFiniteFloat(8)" should compile
        PosFiniteFloat(8).value shouldEqual 8.0F
        "PosFiniteFloat(8L)" should compile
        PosFiniteFloat(8L).value shouldEqual 8.0F
        "PosFiniteFloat(8.0F)" should compile
        PosFiniteFloat(8.0F).value shouldEqual 8.0F
      }

      it("should not compile when 0 is passed in") {
        "PosFiniteFloat(0)" shouldNot compile
        "PosFiniteFloat(0L)" shouldNot compile
        "PosFiniteFloat(0.0F)" shouldNot compile
      }

      it("should not compile when -8 is passed in") {
        "PosFiniteFloat(-8)" shouldNot compile
        "PosFiniteFloat(-8L)" shouldNot compile
        "PosFiniteFloat(-8.0F)" shouldNot compile
      }
      it("should not compile when x is passed in") {
        val a: Int = -8
        "PosFiniteFloat(a)" shouldNot compile
        val b: Long = -8L
        "PosFiniteFloat(b)" shouldNot compile
        val c: Float = -8.0F
        "PosFiniteFloat(c)" shouldNot compile
      }
    }
    describe("when specified as a plain-old Float") {

      def takesPosFiniteFloat(pos: PosFiniteFloat): Float = pos.value

      it("should compile when 8 is passed in") {
        "takesPosFiniteFloat(8)" should compile
        takesPosFiniteFloat(8) shouldEqual 8.0F
        "takesPosFiniteFloat(8L)" should compile
        takesPosFiniteFloat(8L) shouldEqual 8.0F
        "takesPosFiniteFloat(8.0F)" should compile
        takesPosFiniteFloat(8.0F) shouldEqual 8.0F
      }

      it("should not compile when 0 is passed in") {
        "takesPosFiniteFloat(0)" shouldNot compile
        "takesPosFiniteFloat(0L)" shouldNot compile
        "takesPosFiniteFloat(0.0F)" shouldNot compile
      }

      it("should not compile when -8 is passed in") {
        "takesPosFiniteFloat(-8)" shouldNot compile
        "takesPosFiniteFloat(-8L)" shouldNot compile
        "takesPosFiniteFloat(-8.0F)" shouldNot compile
      }

      it("should not compile when x is passed in") {
        val x: Int = -8
        "takesPosFiniteFloat(x)" shouldNot compile
        val b: Long = -8L
        "takesPosFiniteFloat(b)" shouldNot compile
        val c: Float = -8.0F
        "takesPosFiniteFloat(c)" shouldNot compile
      }
    }

    it("should offer a unary + method that is consistent with Float") {
      forAll { (p: PosFiniteFloat) =>
        (+p).toFloat shouldEqual (+(p.toFloat))
      }
    }

    it("should offer a unary - method that returns NegFiniteFloat") {
      forAll { (p: PosFiniteFloat) =>
        (-p) shouldEqual (NegFiniteFloat.ensuringValid(-(p.toFloat)))
      }
    }

    it("should offer 'min' and 'max' methods that are consistent with Float") {
      forAll { (pfloat1: PosFiniteFloat, pfloat2: PosFiniteFloat) =>
        pfloat1.max(pfloat2).toFloat shouldEqual pfloat1.toFloat.max(pfloat2.toFloat)
        pfloat1.min(pfloat2).toFloat shouldEqual pfloat1.toFloat.min(pfloat2.toFloat)
      }
    }

    it("should offer an 'isWhole' method that is consistent with Float") {
      forAll { (pfloat: PosFiniteFloat) =>
        pfloat.isWhole shouldEqual pfloat.toFloat.isWhole
      }
    }

    it("should offer 'round', 'ceil', and 'floor' methods that are consistent with Float") {
      forAll { (pfloat: PosFiniteFloat) =>
        // SKIP-SCALATESTJS,NATIVE-START
        pfloat.round.toFloat shouldEqual pfloat.toFloat.round
        // SKIP-SCALATESTJS,NATIVE-END
        pfloat.ceil.toFloat shouldEqual pfloat.toFloat.ceil
        pfloat.floor.toFloat shouldEqual pfloat.toFloat.floor
      }
    }

    it("should offer 'toRadians' and 'toDegrees' methods that are consistent with Float") {
      forAll { (pfloat: PosFiniteFloat) =>
        pfloat.toRadians shouldEqual pfloat.toFloat.toRadians
      }
    }
  }
  it("should offer an ensuringValid method that takes a Float => Float, throwing AssertionError if the result is invalid") {
    PosFiniteFloat(33.0f).ensuringValid(_ + 1.0f) shouldEqual PosFiniteFloat(34.0f)
    an [AssertionError] should be thrownBy { PosFiniteFloat.MaxValue.ensuringValid(_ - PosFiniteFloat.MaxValue) }
    an [AssertionError] should be thrownBy { PosFiniteFloat.MaxValue.ensuringValid(_ => Float.PositiveInfinity) }
    an [AssertionError] should be thrownBy { PosFiniteFloat.MaxValue.ensuringValid(_ => Float.NegativeInfinity) }
    // SKIP-DOTTY-START
    // https://github.com/lampepfl/dotty/issues/6710
    an [AssertionError] should be thrownBy { PosFiniteFloat.MaxValue.ensuringValid(_ => Float.NaN) }
    // SKIP-DOTTY-END
  }
}

