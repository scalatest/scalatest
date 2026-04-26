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
package org.scalactic.opaquetypes

import org.scalatest.*
import OptionValues.*
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.prop.PropertyChecks
// SKIP-SCALATESTJS,NATIVE-START
import scala.collection.immutable.NumericRange
// SKIP-SCALATESTJS,NATIVE-END
import scala.util.{Failure, Success, Try}
import org.scalactic.{Good, Bad}
import org.scalactic.{Pass, Fail}
import org.scalactic.Equality

import PosDoubles.{PosFiniteDouble, PosZFiniteDouble, PosZDouble}

trait PosFiniteDoubleSpecSupport {

  implicit def tryEquality[T]: Equality[Try[T]] = new Equality[Try[T]] {
    override def areEqual(a: Try[T], b: Any): Boolean = a match {
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

class PosFiniteDoubleSpec extends funspec.AnyFunSpec with matchers.should.Matchers with PropertyChecks with TypeCheckedTripleEquals with PosFiniteDoubleSpecSupport {

  describe("A PosFiniteDouble") {
    describe("should offer a from factory method that") {
      it("returns Some[PosFiniteDouble] if the passed Double is > 0 and finite") {
        PosFiniteDouble.from(50.23) shouldBe Some(PosFiniteDouble(50.23))
        PosFiniteDouble.from(100.0) shouldBe Some(PosFiniteDouble(100.0))
        PosFiniteDouble.from(Double.MaxValue) shouldBe Some(PosFiniteDouble(Double.MaxValue))
        PosFiniteDouble.from(Double.MinPositiveValue) shouldBe Some(PosFiniteDouble(Double.MinPositiveValue))
      }
      it("returns None if the passed Double is 0, negative, or infinite") {
        PosFiniteDouble.from(0.0) shouldBe None
        PosFiniteDouble.from(-0.00001) shouldBe None
        PosFiniteDouble.from(-99.9) shouldBe None
        PosFiniteDouble.from(Double.PositiveInfinity) shouldBe None
        PosFiniteDouble.from(Double.NegativeInfinity) shouldBe None
      }
    }
    describe("should offer an ensuringValid factory method that") {
      it("returns PosFiniteDouble if the passed Double is > 0 and finite") {
        PosFiniteDouble.ensuringValid(50.23) shouldBe PosFiniteDouble(50.23)
        PosFiniteDouble.ensuringValid(100.0) shouldBe PosFiniteDouble(100.0)
        PosFiniteDouble.ensuringValid(Double.MaxValue) shouldBe PosFiniteDouble(Double.MaxValue)
      }
      it("throws AssertionError if the passed Double is 0, negative, or infinite") {
        an [AssertionError] should be thrownBy PosFiniteDouble.ensuringValid(0.0)
        an [AssertionError] should be thrownBy PosFiniteDouble.ensuringValid(-0.00001)
        an [AssertionError] should be thrownBy PosFiniteDouble.ensuringValid(-99.9)
        an [AssertionError] should be thrownBy PosFiniteDouble.ensuringValid(Double.PositiveInfinity)
        an [AssertionError] should be thrownBy PosFiniteDouble.ensuringValid(Double.NegativeInfinity)
      }
    }
    describe("should offer a tryingValid factory method that") {
      import TryValues.*
      it("returns a PosFiniteDouble wrapped in a Success if the passed Double is > 0 and finite") {
        PosFiniteDouble.tryingValid(50.3).success.value.value shouldBe 50.3
        PosFiniteDouble.tryingValid(100.0).success.value.value shouldBe 100.0
      }
      it("returns an AssertionError wrapped in a Failure if the passed Double is invalid") {
        PosFiniteDouble.tryingValid(0.0).failure.exception shouldBe an [AssertionError]
        PosFiniteDouble.tryingValid(-1.0).failure.exception shouldBe an [AssertionError]
        PosFiniteDouble.tryingValid(-99.9).failure.exception shouldBe an [AssertionError]
        PosFiniteDouble.tryingValid(Double.PositiveInfinity).failure.exception shouldBe an [AssertionError]
      }
    }
    describe("should offer a passOrElse factory method that") {
      it("returns a Pass if the given Double is > 0 and finite") {
        PosFiniteDouble.passOrElse(50.0)(i => i) shouldBe Pass
        PosFiniteDouble.passOrElse(100.0)(i => i) shouldBe Pass
      }
      it("returns a Fail if the passed Double is invalid") {
        PosFiniteDouble.passOrElse(0.0)(i => s"$i did not taste good") shouldBe Fail(0.0 + " did not taste good")
        PosFiniteDouble.passOrElse(-1.1)(i => i) shouldBe Fail(-1.1)
        PosFiniteDouble.passOrElse(-99.0)(i => i + 3.0) shouldBe Fail(-96.0)
        PosFiniteDouble.passOrElse(Double.PositiveInfinity)(i => i) shouldBe Fail(Double.PositiveInfinity)
      }
    }
    describe("should offer a goodOrElse factory method that") {
      it("returns a PosFiniteDouble wrapped in a Good if the given Double is > 0 and finite") {
        PosFiniteDouble.goodOrElse(50.3)(i => i) shouldBe Good(PosFiniteDouble(50.3))
        PosFiniteDouble.goodOrElse(100.0)(i => i) shouldBe Good(PosFiniteDouble(100.0))
      }
      it("returns a Bad if the passed Double is invalid") {
        PosFiniteDouble.goodOrElse(0.0)(i => s"$i did not taste good") shouldBe Bad(0.0 + " did not taste good")
        PosFiniteDouble.goodOrElse(-1.1)(i => i) shouldBe Bad(-1.1)
        PosFiniteDouble.goodOrElse(-99.0)(i => i + 3.0) shouldBe Bad(-96.0)
        PosFiniteDouble.goodOrElse(Double.PositiveInfinity)(i => i) shouldBe Bad(Double.PositiveInfinity)
      }
    }
    describe("should offer a rightOrElse factory method that") {
      it("returns a PosFiniteDouble wrapped in a Right if the given Double is > 0 and finite") {
        PosFiniteDouble.rightOrElse(50.3)(i => i) shouldBe Right(PosFiniteDouble(50.3))
        PosFiniteDouble.rightOrElse(100.0)(i => i) shouldBe Right(PosFiniteDouble(100.0))
      }
      it("returns a Left if the passed Double is invalid") {
        PosFiniteDouble.rightOrElse(0.0)(i => s"$i did not taste good") shouldBe Left(0.0 + " did not taste good")
        PosFiniteDouble.rightOrElse(-1.1)(i => i) shouldBe Left(-1.1)
        PosFiniteDouble.rightOrElse(-99.9)(i => i + 3.0) shouldBe Left(-96.9)
        PosFiniteDouble.rightOrElse(Double.PositiveInfinity)(i => i) shouldBe Left(Double.PositiveInfinity)
      }
    }
    describe("should offer an isValid predicate method that") {
      it("returns true if the passed Double is > 0 and finite, false otherwise") {
        PosFiniteDouble.isValid(50.23) shouldBe true
        PosFiniteDouble.isValid(100.0) shouldBe true
        PosFiniteDouble.isValid(Double.MinPositiveValue) shouldBe true
        PosFiniteDouble.isValid(0.0) shouldBe false
        PosFiniteDouble.isValid(-0.0) shouldBe false
        PosFiniteDouble.isValid(-0.00001) shouldBe false
        PosFiniteDouble.isValid(-99.9) shouldBe false
        PosFiniteDouble.isValid(Double.PositiveInfinity) shouldBe false
        PosFiniteDouble.isValid(Double.NegativeInfinity) shouldBe false
      }
    }
    describe("should offer a fromOrElse factory method that") {
      it("returns a PosFiniteDouble if the passed Double is > 0 and finite") {
        PosFiniteDouble.fromOrElse(50.23, PosFiniteDouble(42.0)).value shouldBe 50.23
        PosFiniteDouble.fromOrElse(100.0, PosFiniteDouble(42.0)).value shouldBe 100.0
      }
      it("returns a given default if the passed Double is invalid") {
        PosFiniteDouble.fromOrElse(0.0, PosFiniteDouble(42.0)).value shouldBe 42.0
        PosFiniteDouble.fromOrElse(-0.00001, PosFiniteDouble(42.0)).value shouldBe 42.0
        PosFiniteDouble.fromOrElse(-99.9, PosFiniteDouble(42.0)).value shouldBe 42.0
        PosFiniteDouble.fromOrElse(Double.PositiveInfinity, PosFiniteDouble(42.0)).value shouldBe 42.0
      }
    }
    it("should offer MaxValue and MinValue constants") {
      PosFiniteDouble.MaxValue shouldEqual PosFiniteDouble.from(Double.MaxValue).get
      PosFiniteDouble.MinValue shouldEqual PosFiniteDouble.from(Double.MinPositiveValue).get
    }
    it("should not offer a PositiveInfinity constant") {
      "PosFiniteDouble.PositiveInfinity" shouldNot compile
    }
    it("should not offer a NegativeInfinity constant") {
      "PosFiniteDouble.NegativeInfinity" shouldNot compile
    }

    it("should be a subtype of PosZFiniteDouble") {
      val pfd: PosFiniteDouble = PosFiniteDouble(1.0)
      def takesPosZFiniteDouble(x: PosZFiniteDouble): Double = x.value
      takesPosZFiniteDouble(pfd) shouldEqual 1.0
    }

    it("should be a subtype of PosZDouble") {
      val pfd: PosFiniteDouble = PosFiniteDouble(1.0)
      def takesPosZDouble(x: PosZDouble): Double = x.value
      takesPosZDouble(pfd) shouldEqual 1.0
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
      it("should not compile when a variable is passed in") {
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
      it("should not compile when a variable is passed in") {
        val a: Int = -8
        "takesPosFiniteDouble(a)" shouldNot compile
        val b: Long = -8L
        "takesPosFiniteDouble(b)" shouldNot compile
        val c: Float = -8.0F
        "takesPosFiniteDouble(c)" shouldNot compile
        val d: Double = -8.0
        "takesPosFiniteDouble(d)" shouldNot compile
      }
    }

    it("should offer an ensuringValid method that takes a Double => Double, throwing AssertionError if the result is invalid") {
      PosFiniteDouble(33.0).ensuringValid(_ + 1.0) shouldEqual PosFiniteDouble(34.0)
      PosFiniteDouble(33.0).ensuringValid(_ * 2.0) shouldEqual PosFiniteDouble(66.0)
      an [AssertionError] should be thrownBy { PosFiniteDouble.MaxValue.ensuringValid(_ - PosFiniteDouble.MaxValue) }
      an [AssertionError] should be thrownBy { PosFiniteDouble.MaxValue.ensuringValid(_ => Double.PositiveInfinity) }
      an [AssertionError] should be thrownBy { PosFiniteDouble.MaxValue.ensuringValid(_ => Double.NegativeInfinity) }
      an [AssertionError] should be thrownBy { PosFiniteDouble.MaxValue.ensuringValid(_ => 0.0) }
    }

    it("should offer an isFinite method that always returns true") {
      PosFiniteDouble(1.0).isFinite shouldBe true
      PosFiniteDouble(Double.MaxValue).isFinite shouldBe true
    }
  }
}
