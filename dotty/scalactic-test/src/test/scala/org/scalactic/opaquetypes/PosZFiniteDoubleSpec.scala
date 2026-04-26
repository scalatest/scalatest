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
import org.scalatest.prop.PropertyChecks
import org.scalactic.TypeCheckedTripleEquals
// SKIP-SCALATESTJS,NATIVE-START
import scala.collection.immutable.NumericRange
// SKIP-SCALATESTJS,NATIVE-END
import OptionValues.*
import org.scalactic.Equality
import org.scalactic.{Pass, Fail}
import org.scalactic.{Good, Bad}
import scala.util.{Try, Success, Failure}

import PosDoubles.{PosZFiniteDouble, PosFiniteDouble, PosZDouble}

trait PosZFiniteDoubleSpecSupport {

  implicit val doubleEquality: Equality[Double] =
    new Equality[Double] {
      override def areEqual(a: Double, b: Any): Boolean =
        (a, b) match {
          case (a, bDouble: Double) if a.isNaN && bDouble.isNaN => true
          case _ => a == b
        }
    }

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

class PosZFiniteDoubleSpec extends funspec.AnyFunSpec with matchers.should.Matchers with PropertyChecks with TypeCheckedTripleEquals with PosZFiniteDoubleSpecSupport {

  describe("A PosZFiniteDouble") {
    describe("should offer a from factory method that") {
      it("returns Some[PosZFiniteDouble] if the passed Double is >= 0 and finite") {
        PosZFiniteDouble.from(0.0) shouldBe Some(PosZFiniteDouble(0.0))
        PosZFiniteDouble.from(50.23) shouldBe Some(PosZFiniteDouble(50.23))
        PosZFiniteDouble.from(100.0) shouldBe Some(PosZFiniteDouble(100.0))
        PosZFiniteDouble.from(Double.MaxValue) shouldBe Some(PosZFiniteDouble(Double.MaxValue))
      }
      it("returns None if the passed Double is negative or infinite") {
        PosZFiniteDouble.from(-0.00001) shouldBe None
        PosZFiniteDouble.from(-99.9) shouldBe None
        PosZFiniteDouble.from(Double.PositiveInfinity) shouldBe None
        PosZFiniteDouble.from(Double.NegativeInfinity) shouldBe None
      }
    }
    describe("should offer an ensuringValid factory method that") {
      it("returns PosZFiniteDouble if the passed Double is >= 0 and finite") {
        PosZFiniteDouble.ensuringValid(0.0) shouldBe PosZFiniteDouble(0.0)
        PosZFiniteDouble.ensuringValid(50.23) shouldBe PosZFiniteDouble(50.23)
        PosZFiniteDouble.ensuringValid(100.0) shouldBe PosZFiniteDouble(100.0)
        PosZFiniteDouble.ensuringValid(Double.MaxValue) shouldBe PosZFiniteDouble(Double.MaxValue)
      }
      it("throws AssertionError if the passed Double is negative or infinite") {
        an [AssertionError] should be thrownBy PosZFiniteDouble.ensuringValid(-0.00001)
        an [AssertionError] should be thrownBy PosZFiniteDouble.ensuringValid(-99.9)
        an [AssertionError] should be thrownBy PosZFiniteDouble.ensuringValid(Double.PositiveInfinity)
        an [AssertionError] should be thrownBy PosZFiniteDouble.ensuringValid(Double.NegativeInfinity)
      }
    }
    describe("should offer a tryingValid factory method that") {
      import TryValues.*
      it("returns a PosZFiniteDouble wrapped in a Success if the passed Double is >= 0 and finite") {
        PosZFiniteDouble.tryingValid(0.0).success.value.value shouldBe 0.0
        PosZFiniteDouble.tryingValid(50.0).success.value.value shouldBe 50.0
        PosZFiniteDouble.tryingValid(100.0).success.value.value shouldBe 100.0
      }
      it("returns an AssertionError wrapped in a Failure if the passed Double is invalid") {
        PosZFiniteDouble.tryingValid(-1.0).failure.exception shouldBe an [AssertionError]
        PosZFiniteDouble.tryingValid(-99.0).failure.exception shouldBe an [AssertionError]
        PosZFiniteDouble.tryingValid(Double.PositiveInfinity).failure.exception shouldBe an [AssertionError]
      }
    }
    describe("should offer a passOrElse factory method that") {
      it("returns a Pass if the given Double is >= 0 and finite") {
        PosZFiniteDouble.passOrElse(0.0)(i => i) shouldBe Pass
        PosZFiniteDouble.passOrElse(50.0)(i => i) shouldBe Pass
        PosZFiniteDouble.passOrElse(100.0)(i => i) shouldBe Pass
      }
      it("returns a Fail if the passed Double is invalid") {
        PosZFiniteDouble.passOrElse(-1.0)(i => i) shouldBe Fail(-1.0)
        PosZFiniteDouble.passOrElse(-99.0)(i => i + 3.0) shouldBe Fail(-96.0)
        PosZFiniteDouble.passOrElse(Double.PositiveInfinity)(i => i) shouldBe Fail(Double.PositiveInfinity)
      }
    }
    describe("should offer a goodOrElse factory method that") {
      it("returns a PosZFiniteDouble wrapped in a Good if the given Double is >= 0 and finite") {
        PosZFiniteDouble.goodOrElse(0.0)(i => i) shouldBe Good(PosZFiniteDouble(0.0))
        PosZFiniteDouble.goodOrElse(50.0)(i => i) shouldBe Good(PosZFiniteDouble(50.0))
        PosZFiniteDouble.goodOrElse(100.0)(i => i) shouldBe Good(PosZFiniteDouble(100.0))
      }
      it("returns a Bad if the passed Double is invalid") {
        PosZFiniteDouble.goodOrElse(-1.0)(i => i) shouldBe Bad(-1.0)
        PosZFiniteDouble.goodOrElse(-99.0)(i => i + 3.0) shouldBe Bad(-96.0)
        PosZFiniteDouble.goodOrElse(Double.PositiveInfinity)(i => i) shouldBe Bad(Double.PositiveInfinity)
      }
    }
    describe("should offer a rightOrElse factory method that") {
      it("returns a PosZFiniteDouble wrapped in a Right if the given Double is >= 0 and finite") {
        PosZFiniteDouble.rightOrElse(0.0)(i => i) shouldBe Right(PosZFiniteDouble(0.0))
        PosZFiniteDouble.rightOrElse(50.0)(i => i) shouldBe Right(PosZFiniteDouble(50.0))
        PosZFiniteDouble.rightOrElse(100.0)(i => i) shouldBe Right(PosZFiniteDouble(100.0))
      }
      it("returns a Left if the passed Double is invalid") {
        PosZFiniteDouble.rightOrElse(-1.0)(i => i) shouldBe Left(-1.0)
        PosZFiniteDouble.rightOrElse(-99.0)(i => i + 3.0) shouldBe Left(-96.0)
        PosZFiniteDouble.rightOrElse(Double.PositiveInfinity)(i => i) shouldBe Left(Double.PositiveInfinity)
      }
    }
    describe("should offer an isValid predicate method that") {
      it("returns true if the passed Double is >= 0 and finite, false otherwise") {
        PosZFiniteDouble.isValid(50.23) shouldBe true
        PosZFiniteDouble.isValid(100.0) shouldBe true
        PosZFiniteDouble.isValid(0.0) shouldBe true
        PosZFiniteDouble.isValid(-0.0) shouldBe true
        PosZFiniteDouble.isValid(-0.00001) shouldBe false
        PosZFiniteDouble.isValid(-99.9) shouldBe false
        PosZFiniteDouble.isValid(Double.PositiveInfinity) shouldBe false
        PosZFiniteDouble.isValid(Double.NegativeInfinity) shouldBe false
      }
    }
    describe("should offer a fromOrElse factory method that") {
      it("returns a PosZFiniteDouble if the passed Double is >= 0 and finite") {
        PosZFiniteDouble.fromOrElse(50.23, PosZFiniteDouble(42.0)).value shouldBe 50.23
        PosZFiniteDouble.fromOrElse(100.0, PosZFiniteDouble(42.0)).value shouldBe 100.0
        PosZFiniteDouble.fromOrElse(0.0, PosZFiniteDouble(42.0)).value shouldBe 0.0
      }
      it("returns a given default if the passed Double is invalid") {
        PosZFiniteDouble.fromOrElse(-0.00001, PosZFiniteDouble(42.0)).value shouldBe 42.0
        PosZFiniteDouble.fromOrElse(-99.9, PosZFiniteDouble(42.0)).value shouldBe 42.0
        PosZFiniteDouble.fromOrElse(Double.PositiveInfinity, PosZFiniteDouble(42.0)).value shouldBe 42.0
      }
    }
    it("should offer MaxValue and MinValue constants") {
      PosZFiniteDouble.MaxValue shouldEqual PosZFiniteDouble.from(Double.MaxValue).get
      PosZFiniteDouble.MinValue shouldEqual PosZFiniteDouble(Double.MinPositiveValue)
    }
    it("should not offer a PositiveInfinity constant") {
      "PosZFiniteDouble.PositiveInfinity" shouldNot compile
    }
    it("should not offer a NegativeInfinity constant") {
      "PosZFiniteDouble.NegativeInfinity" shouldNot compile
    }

    it("should be sortable") {
      val xs = List(PosZFiniteDouble(2.2), PosZFiniteDouble(0.0), PosZFiniteDouble(1.1),
                    PosZFiniteDouble(3.3))
      xs.sorted shouldEqual List(PosZFiniteDouble(0.0), PosZFiniteDouble(1.1),
                                 PosZFiniteDouble(2.2), PosZFiniteDouble(3.3))
    }

    describe("when created with apply method") {
      it("should compile when 8 is passed in") {
        "PosZFiniteDouble(8)" should compile
        PosZFiniteDouble(8).value shouldEqual 8.0
        "PosZFiniteDouble(8L)" should compile
        PosZFiniteDouble(8L).value shouldEqual 8.0
        "PosZFiniteDouble(8.0F)" should compile
        PosZFiniteDouble(8.0F).value shouldEqual 8.0
        "PosZFiniteDouble(8.0)" should compile
        PosZFiniteDouble(8.0).value shouldEqual 8.0
      }
      it("should compile when 0 is passed in") {
        "PosZFiniteDouble(0)" should compile
        PosZFiniteDouble(0).value shouldEqual 0.0
        "PosZFiniteDouble(0L)" should compile
        PosZFiniteDouble(0L).value shouldEqual 0.0
        "PosZFiniteDouble(0.0F)" should compile
        PosZFiniteDouble(0.0F).value shouldEqual 0.0
        "PosZFiniteDouble(0.0)" should compile
        PosZFiniteDouble(0.0).value shouldEqual 0.0
      }
      it("should not compile when -8 is passed in") {
        "PosZFiniteDouble(-8)" shouldNot compile
        "PosZFiniteDouble(-8L)" shouldNot compile
        "PosZFiniteDouble(-8.0F)" shouldNot compile
        "PosZFiniteDouble(-8.0)" shouldNot compile
      }
      it("should not compile when a variable is passed in") {
        val a: Int = -8
        "PosZFiniteDouble(a)" shouldNot compile
        val b: Long = -8L
        "PosZFiniteDouble(b)" shouldNot compile
        val c: Float = -8.0F
        "PosZFiniteDouble(c)" shouldNot compile
        val d: Double = -8.0
        "PosZFiniteDouble(d)" shouldNot compile
      }
    }

    describe("when specified as a plain-old Double") {
      def takesPosZFiniteDouble(pos: PosZFiniteDouble): Double = pos.value

      it("should compile when 8 is passed in") {
        "takesPosZFiniteDouble(8)" should compile
        takesPosZFiniteDouble(8) shouldEqual 8.0
        "takesPosZFiniteDouble(8L)" should compile
        takesPosZFiniteDouble(8L) shouldEqual 8.0
        "takesPosZFiniteDouble(8.0F)" should compile
        takesPosZFiniteDouble(8.0F) shouldEqual 8.0
        "takesPosZFiniteDouble(8.0)" should compile
        takesPosZFiniteDouble(8.0) shouldEqual 8.0
      }
      it("should compile when 0 is passed in") {
        "takesPosZFiniteDouble(0)" should compile
        takesPosZFiniteDouble(0) shouldEqual 0.0
        "takesPosZFiniteDouble(0L)" should compile
        takesPosZFiniteDouble(0L) shouldEqual 0.0
        "takesPosZFiniteDouble(0.0F)" should compile
        takesPosZFiniteDouble(0.0F) shouldEqual 0.0
        "takesPosZFiniteDouble(0.0)" should compile
        takesPosZFiniteDouble(0.0) shouldEqual 0.0
      }
      it("should not compile when -8 is passed in") {
        "takesPosZFiniteDouble(-8)" shouldNot compile
        "takesPosZFiniteDouble(-8L)" shouldNot compile
        "takesPosZFiniteDouble(-8.0F)" shouldNot compile
        "takesPosZFiniteDouble(-8.0)" shouldNot compile
      }
      it("should not compile when a variable is passed in") {
        val a: Int = -8
        "takesPosZFiniteDouble(a)" shouldNot compile
        val b: Long = -8L
        "takesPosZFiniteDouble(b)" shouldNot compile
        val c: Float = -8.0F
        "takesPosZFiniteDouble(c)" shouldNot compile
        val d: Double = -8.0
        "takesPosZFiniteDouble(d)" shouldNot compile
      }
    }

    it("should offer a unary + method that is consistent with Double") {
      forAll { (p: PosZFiniteDouble) =>
        (+p).toDouble shouldEqual (+(p.toDouble))
      }
    }

    it("should offer 'min' and 'max' methods that are consistent with Double") {
      forAll { (p1: PosZFiniteDouble, p2: PosZFiniteDouble) =>
        p1.max(p2).toDouble shouldEqual p1.toDouble.max(p2.toDouble)
        p1.min(p2).toDouble shouldEqual p1.toDouble.min(p2.toDouble)
      }
    }

    it("should offer an 'isWhole' method that is consistent with Double") {
      forAll { (p: PosZFiniteDouble) =>
        p.isWhole shouldEqual p.toDouble.isWhole
      }
    }

    it("should offer 'round', 'ceil', and 'floor' methods that are consistent with Double") {
      forAll { (p: PosZFiniteDouble) =>
        p.round.toDouble shouldEqual p.toDouble.round
        p.ceil.toDouble shouldEqual p.toDouble.ceil
        p.floor.toDouble shouldEqual p.toDouble.floor
      }
    }

    it("should offer 'toRadians' and 'toDegrees' methods that are consistent with Double") {
      forAll { (p: PosZFiniteDouble) =>
        p.toRadians shouldEqual p.toDouble.toRadians
        p.toDegrees shouldEqual p.toDouble.toDegrees
      }
    }

    it("should offer widening methods for basic types that are consistent with Double") {
      forAll { (p: PosZFiniteDouble) =>
        def widen(value: Double): Double = value
        widen(p) shouldEqual widen(p.toDouble)
      }
      forAll { (p: PosZFiniteDouble) =>
        def widen(value: PosZDouble): PosZDouble = value
        widen(p) shouldEqual widen(PosZDouble.from(p.toDouble).get)
      }
    }

    it("should offer an ensuringValid method that takes a Double => Double, throwing AssertionError if the result is invalid") {
      PosZFiniteDouble(33.0).ensuringValid(_ + 1.0) shouldEqual PosZFiniteDouble(34.0)
      PosZFiniteDouble(33.0).ensuringValid(_ => 0.0) shouldEqual PosZFiniteDouble(0.0)
      an [AssertionError] should be thrownBy { PosZFiniteDouble.MaxValue.ensuringValid(_ - PosZFiniteDouble.MaxValue - 1.0) }
      an [AssertionError] should be thrownBy { PosZFiniteDouble.MaxValue.ensuringValid(_ => Double.PositiveInfinity) }
      an [AssertionError] should be thrownBy { PosZFiniteDouble.MaxValue.ensuringValid(_ => Double.NegativeInfinity) }
    }

    it("should offer an isFinite method that always returns true") {
      forAll { (p: PosZFiniteDouble) =>
        p.isFinite shouldBe true
      }
    }
  }
}
