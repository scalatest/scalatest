/*
 * Copyright 2001-2014 Artima, Inc.
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
// SKIP-SCALATESTJS,NATIVE-START
import scala.collection.immutable.NumericRange
// SKIP-SCALATESTJS,NATIVE-END
import OptionValues._
import scala.collection.mutable.WrappedArray
//import org.scalactic.StrictCheckedEquality
import Double.NaN
import org.scalactic.Equality
import org.scalactic.{Pass, Fail}
import org.scalactic.{Good, Bad}
import scala.util.{Try, Success, Failure}

trait NegZDoubleSpecSupport {

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
      case Success(double: Double) if double.isNaN =>  // This is because in Scala.js x/0 results to NaN not ArithmetricException like in jvm, and we need to make sure Success(NaN) == Success(NaN) is true to pass the test.
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

class NegZDoubleSpec extends FunSpec with Matchers with PropertyChecks with NegZDoubleSpecSupport {

  describe("A NegZDouble") {
    describe("should offer a from factory method that") {
      it("returns Some[NegZDouble] if the passed Double is lesser than or equal to 0") {
        NegZDouble.from(0.0).value.value shouldBe 0.0
        NegZDouble.from(-50.23).value.value shouldBe -50.23
        NegZDouble.from(-100.0).value.value shouldBe -100.0
      }
      it("returns None if the passed Double is greater than 0") {
        NegZDouble.from(0.00001) shouldBe None
        NegZDouble.from(99.9) shouldBe None
      }
    }
    describe("should offer an ensuringValid factory method that") {
      it("returns NegZDouble if the passed Double is lesser than or equal to 0") {
        NegZDouble.ensuringValid(0.0).value shouldBe 0.0
        NegZDouble.ensuringValid(-50.23).value shouldBe -50.23
        NegZDouble.ensuringValid(-100.0).value shouldBe -100.0
        NegZDouble.ensuringValid(Double.NegativeInfinity).value shouldBe Double.NegativeInfinity
      }
      it("throws AssertionError if the passed Double is greater than 0") {
        an [AssertionError] should be thrownBy NegZDouble.ensuringValid(0.00001)
        an [AssertionError] should be thrownBy NegZDouble.ensuringValid(99.9)
        an [AssertionError] should be thrownBy NegZDouble.ensuringValid(Double.PositiveInfinity)
        // SKIP-DOTTY-START
        // https://github.com/lampepfl/dotty/issues/6710
        an [AssertionError] should be thrownBy NegZDouble.ensuringValid(Double.NaN)
        // SKIP-DOTTY-END
      }
    }
    describe("should offer a tryingValid factory method that") {
      import TryValues._
      it("returns a NegZDouble wrapped in a Success if the passed Double is lesser than or equal 0") {
        NegZDouble.tryingValid(0.0).success.value.value shouldBe 0.0
        NegZDouble.tryingValid(-50.0).success.value.value shouldBe -50.0
        NegZDouble.tryingValid(-100.0f).success.value.value shouldBe -100.0
      }

      it("returns an AssertionError wrapped in a Failure if the passed Double is greater than 0") {
        NegZDouble.tryingValid(1.0).failure.exception shouldBe an [AssertionError]
        NegZDouble.tryingValid(99.0).failure.exception shouldBe an [AssertionError]
      }
    }
    describe("should offer a passOrElse factory method that") {
      it("returns a Pass if the given Double is lesser than or equal 0") {
        NegZDouble.passOrElse(0.0)(i => i) shouldBe Pass
        NegZDouble.passOrElse(-50.0)(i => i) shouldBe Pass
        NegZDouble.passOrElse(-100.0)(i => i) shouldBe Pass
      }
      it("returns an error value produced by passing the given Double to the given function if the passed Double is greater than 0, wrapped in a Fail") {
        NegZDouble.passOrElse(1.0)(i => i) shouldBe Fail(1.0)
        NegZDouble.passOrElse(99.0)(i => i + 3.0) shouldBe Fail(102.0)
      }
    }
    describe("should offer a goodOrElse factory method that") {
      it("returns a NegZDouble wrapped in a Good if the given Double is lesser than or equal 0") {
        NegZDouble.goodOrElse(0.0)(i => i) shouldBe Good(NegZDouble(0.0))
        NegZDouble.goodOrElse(-50.0)(i => i) shouldBe Good(NegZDouble(-50.0))
        NegZDouble.goodOrElse(-100.0)(i => i) shouldBe Good(NegZDouble(-100.0))
      }
      it("returns an error value produced by passing the given Double to the given function if the passed Double is greater than 0, wrapped in a Bad") {
        NegZDouble.goodOrElse(1.0)(i => i) shouldBe Bad(1.0)
        NegZDouble.goodOrElse(99.0)(i => i + 3.0f) shouldBe Bad(102.0)
      }
    }
    describe("should offer a rightOrElse factory method that") {
      it("returns a NegZDouble wrapped in a Right if the given Double is lesser than or equal 0") {
        NegZDouble.rightOrElse(0.0)(i => i) shouldBe Right(NegZDouble(0.0))
        NegZDouble.rightOrElse(-50.0)(i => i) shouldBe Right(NegZDouble(-50.0))
        NegZDouble.rightOrElse(-100.0)(i => i) shouldBe Right(NegZDouble(-100.0))
      }
      it("returns an error value produced by passing the given Double to the given function if the passed Double is greater than 0, wrapped in a Left") {
        NegZDouble.rightOrElse(1.0)(i => i) shouldBe Left(1.0)
        NegZDouble.rightOrElse(99.0)(i => i + 3.0f) shouldBe Left(102.0)
      }
    }
    describe("should offer an isValid predicate method that") {
      it("returns true if the passed Double is lesser than or equal to 0") {
        NegZDouble.isValid(-50.23) shouldBe true
        NegZDouble.isValid(-100.0) shouldBe true
        NegZDouble.isValid(0.0) shouldBe true
        NegZDouble.isValid(-0.0) shouldBe true
        NegZDouble.isValid(0.00001) shouldBe false
        NegZDouble.isValid(99.9) shouldBe false
      }
    }
    describe("should offer a fromOrElse factory method that") {
      it("returns a NegZDouble if the passed Double is lesser than or equal to 0") {
        NegZDouble.fromOrElse(-50.23, NegZDouble(-42.0)).value shouldBe -50.23
        NegZDouble.fromOrElse(-100.0, NegZDouble(-42.0)).value shouldBe -100.0
        NegZDouble.fromOrElse(0.0, NegZDouble(-42.0)).value shouldBe 0.0
      }
      it("returns a given default if the passed Double is greater than or equal to 0") {
        NegZDouble.fromOrElse(0.00001, NegZDouble(-42.0)).value shouldBe -42.0
        NegZDouble.fromOrElse(99.9, NegZDouble(-42.0)).value shouldBe -42.0
      }
    }
    it("should offer MaxValue and MinValue factory methods") {
      NegZDouble.MaxValue shouldEqual NegZDouble(0.0)
      NegZDouble.MinValue shouldEqual NegZDouble.from(Double.MinValue).get
    }
    it("should offer a NegativeInfinity factory method") {
      NegZDouble.NegativeInfinity shouldEqual NegZDouble.ensuringValid(Double.NegativeInfinity)
    }
    it("should not offer a PositiveInfinity factory method") {
      "NegZDouble.PositiveInfinity" shouldNot compile
    }
    it("should offer a isNegInfinity method that returns true if the instance is NegativeInfinity") {
      NegZDouble.ensuringValid(Double.NegativeInfinity).isNegInfinity shouldBe true
      NegZDouble(-1.0).isNegInfinity shouldBe false
    }
    it("should not offer a isPosInfinity method") {
      "NegZDouble(-1.0f).isPosInfinity" shouldNot compile
    }

    it("should be sortable") {
      val xs = List(NegZDouble(-2.2), NegZDouble(-0.0), NegZDouble(-1.1),
        NegZDouble(-3.3))
      xs.sorted shouldEqual List(NegZDouble(-3.3), NegZDouble(-2.2),
        NegZDouble(-1.1), NegZDouble(0.0))
    }

    describe("when created with apply method") {

      it("should compile when -8 is passed in") {
        "NegZDouble(-8)" should compile
        NegZDouble(-8).value shouldEqual -8.0
        "NegZDouble(-8L)" should compile
        NegZDouble(-8L).value shouldEqual -8.0
        "NegZDouble(-8.0F)" should compile
        NegZDouble(-8.0F).value shouldEqual -8.0
        "NegZDouble(-8.0)" should compile
        NegZDouble(-8.0).value shouldEqual -8.0
      }

      it("should compile when 0 is passed in") {
        "NegZDouble(0)" should compile
        NegZDouble(0).value shouldEqual 0.0
        "NegZDouble(0L)" should compile
        NegZDouble(0L).value shouldEqual 0.0
        "NegZDouble(0.0F)" should compile
        NegZDouble(0.0F).value shouldEqual 0.0
        "NegZDouble(0.0)" should compile
        NegZDouble(0.0).value shouldEqual 0.0
      }

      it("should not compile when 8 is passed in") {
        "NegZDouble(8)" shouldNot compile
        "NegZDouble(8L)" shouldNot compile
        "NegZDouble(8.0F)" shouldNot compile
        "NegZDouble(8.0)" shouldNot compile
      }
      it("should not compile when x is passed in") {
        val a: Int = -8
        "NegZDouble(a)" shouldNot compile
        val b: Long = -8L
        "NegZDouble(b)" shouldNot compile
        val c: Float = -8.0F
        "NegZDouble(c)" shouldNot compile
        val d: Double = -8.0
        "NegZDouble(d)" shouldNot compile
      }
    }
    describe("when specified as a plain-old Double") {

      def takesNegZDouble(poz: NegZDouble): Double = poz.value

      it("should compile when -8 is passed in") {
        "takesNegZDouble(-8)" should compile
        takesNegZDouble(-8) shouldEqual -8.0
        "takesNegZDouble(-8L)" should compile
        takesNegZDouble(-8L) shouldEqual -8.0
        "takesNegZDouble(-8.0F)" should compile
        takesNegZDouble(-8.0F) shouldEqual -8.0
        "takesNegZDouble(-8.0)" should compile
        takesNegZDouble(-8.0) shouldEqual -8.0
      }

      it("should compile when 0 is passed in") {
        "takesNegZDouble(0)" should compile
        takesNegZDouble(0) shouldEqual 0.0
        "takesNegZDouble(0L)" should compile
        takesNegZDouble(0L) shouldEqual 0.0
        "takesNegZDouble(0.0F)" should compile
        takesNegZDouble(0.0F) shouldEqual 0.0
        "takesNegZDouble(0.0)" should compile
        takesNegZDouble(0.0) shouldEqual 0.0
      }

      it("should not compile when 8 is passed in") {
        "takesNegZDouble(8)" shouldNot compile
        "takesNegZDouble(8L)" shouldNot compile
        "takesNegZDouble(8.0F)" shouldNot compile
        "takesNegZDouble(8.0)" shouldNot compile
      }

      it("should not compile when x is passed in") {
        val x: Int = -8
        "takesNegZDouble(x)" shouldNot compile
        val b: Long = -8L
        "takesNegZDouble(b)" shouldNot compile
        val c: Float = -8.0F
        "takesNegZDouble(c)" shouldNot compile
        val d: Double = -8.0
        "takesNegZDouble(d)" shouldNot compile
      }

      it("should offer a unary + method that is consistent with Double") {
        forAll { (p: NegZDouble) =>
          (+p).toDouble shouldEqual (+(p.toDouble))
        }
      }

      it("should offer a unary - method that returns PosZDouble") {
        forAll { (p: NegZDouble) =>
          (-p) shouldEqual (PosZDouble.ensuringValid(-(p.toDouble)))
        }
      }
    }

    it("should offer a 'plus' method that takes a NegZDouble and returns a PosDouble") {

      forAll { (negZDouble1: NegZDouble, negZDouble2: NegZDouble) =>
        (negZDouble1 plus negZDouble2) should === (NegZDouble.ensuringValid(negZDouble1.toDouble + negZDouble2.toDouble))
      }

      val examples =
        Table(
          (                "negZDouble1",                "negZDouble2" ),
          (         NegZDouble.MinValue,         NegZDouble.MinValue ),
          (         NegZDouble.MinValue,         NegZDouble.MaxValue ),
          (         NegZDouble.MaxValue,         NegZDouble.MinValue ),
          (         NegZDouble.MaxValue,         NegZDouble.MaxValue ),
          (         NegZDouble.MaxValue, NegZDouble.NegativeInfinity ),
          ( NegZDouble.NegativeInfinity,         NegZDouble.MinValue ),
          ( NegZDouble.NegativeInfinity,         NegZDouble.MaxValue ),
          ( NegZDouble.NegativeInfinity, NegZDouble.NegativeInfinity )
        )

      forAll (examples) { (a, b) =>
        (a plus b).value should be <= 0.0
      }

      // Sanity check that implicit widening conversions work too.
      // Here a PosDouble gets widened to a NegZDouble.
      NegZDouble(-1.0) plus NegDouble(-2.0) should === (NegZDouble(-3.0))
    }

    it("should offer 'min' and 'max' methods that are consistent with Double") {
      forAll { (pzdouble1: NegZDouble, pzdouble2: NegZDouble) =>
        pzdouble1.max(pzdouble2).toDouble shouldEqual pzdouble1.toDouble.max(pzdouble2.toDouble)
        pzdouble1.min(pzdouble2).toDouble shouldEqual pzdouble1.toDouble.min(pzdouble2.toDouble)
      }
    }

    it("should offer an 'isWhole' method that is consistent with Double") {
      forAll { (pzdouble: NegZDouble) =>
        pzdouble.isWhole shouldEqual pzdouble.toDouble.isWhole
      }
    }

    it("should offer 'round', 'ceil', and 'floor' methods that are consistent with Double") {
      forAll { (pzdouble: NegZDouble) =>
        pzdouble.round.toDouble shouldEqual pzdouble.toDouble.round
        pzdouble.ceil.toDouble shouldEqual pzdouble.toDouble.ceil
        pzdouble.floor.toDouble shouldEqual pzdouble.toDouble.floor
      }
    }

    it("should offer 'toRadians' and 'toDegrees' methods that are consistent with Double") {
      forAll { (pzdouble: NegZDouble) =>
        pzdouble.toRadians shouldEqual pzdouble.toDouble.toRadians
      }
    }

    it("should offer widening methods for basic types that are consistent with Double") {
      forAll { (pzdouble: NegZDouble) =>
        def widen(value: Double): Double = value
        widen(pzdouble) shouldEqual widen(pzdouble.toDouble)
      }
    }
    it("should offer an ensuringValid method that takes a Double => Double, throwing AssertionError if the result is invalid") {
      NegZDouble(-33.0).ensuringValid(_ + 1.0) shouldEqual NegZDouble(-32.0)
      NegZDouble(-33.0).ensuringValid(_ => Double.NegativeInfinity) shouldEqual NegZDouble.ensuringValid(Double.NegativeInfinity)
      an [AssertionError] should be thrownBy { NegZDouble.MaxValue.ensuringValid(_ - NegZDouble.MaxValue + 1) }
      an [AssertionError] should be thrownBy { NegZDouble.MaxValue.ensuringValid(_ => Double.PositiveInfinity) }
      // SKIP-DOTTY-START
      // https://github.com/lampepfl/dotty/issues/6710
      an [AssertionError] should be thrownBy { NegZDouble.MaxValue.ensuringValid(_ => Double.NaN) }
      // SKIP-DOTTY-END
    }
    it("should offer an isFinite method that returns true if the value does not represent infinity") {
      forAll { (n: NegZFiniteDouble) =>
        (n: NegZDouble).isFinite should be (true)
        NegZDouble.NegativeInfinity.isFinite should be (false)
      }
    }
  }
}
