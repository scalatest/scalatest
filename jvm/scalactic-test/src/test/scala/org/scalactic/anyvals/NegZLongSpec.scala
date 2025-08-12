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
import org.scalactic.Equality
import org.scalactic.{Pass, Fail}
import org.scalactic.{Good, Bad}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
// SKIP-SCALATESTJS,NATIVE-START
import scala.collection.immutable.NumericRange
// SKIP-SCALATESTJS,NATIVE-END
import scala.collection.mutable.WrappedArray
import OptionValues._

import scala.util.{Failure, Success, Try}

trait NegZLongSpecSupport {

  implicit def tryEquality[T]: Equality[Try[T]] = new Equality[Try[T]] {
    override def areEqual(a: Try[T], b: Any): Boolean = a match {
      case Success(double: Double) if double.isNaN =>  // This is because in scala.js x/0 results to NaN not ArithmetricException like in jvm, and we need to make sure Success(NaN) == Success(NaN) is true to pass the test.
        b match {
          case Success(bDouble: Double) if bDouble.isNaN => true
          case _ => false
        }
      // I needed this because with GenDrivenPropertyChecks, got:
      // [info] - should offer a '%' method that is consistent with Int *** FAILED ***
      // [info]   Success(NaN) did not equal Success(NaN) (PosIntExperiment.scala:498)
      case Success(float: Float) if float.isNaN =>
        b match {
          case Success(bFloat: Float) if bFloat.isNaN => true
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

class NegZLongSpec extends funspec.AnyFunSpec with matchers.should.Matchers with GeneratorDrivenPropertyChecks with NegZLongSpecSupport {

  describe("A NegZLong") {
    describe("should offer a from factory method that") {
      it("returns Some[NegZLong] if the passed Long is lesser than or equal to 0") {
        NegZLong.from(0L).value.value shouldBe 0L
        NegZLong.from(-50L).value.value shouldBe -50L
        NegZLong.from(-100L).value.value shouldBe -100L
      }
      it("returns None if the passed Long is greater than 0") {
        NegZLong.from(1L) shouldBe None
        NegZLong.from(99L) shouldBe None
      }
    }
    describe("should offer an ensuringValid factory method that") {
      it("returns NegZLong if the passed Long is lesser than or equal to 0") {
        NegZLong.ensuringValid(0L).value shouldBe 0L
        NegZLong.ensuringValid(-50L).value shouldBe -50L
        NegZLong.ensuringValid(-100L).value shouldBe -100L
      }
      it("throws AssertionError if the passed Long is greater than 0") {
        an [AssertionError] should be thrownBy NegZLong.ensuringValid(1L)
        an [AssertionError] should be thrownBy NegZLong.ensuringValid(99L)
      }
    }
    describe("should offer a tryingValid factory method that") {
      import TryValues._
      it("returns a NegZLong wrapped in a Success if the passed Long is lesser than or equal 0") {
        NegZLong.tryingValid(0L).success.value.value shouldBe 0L
        NegZLong.tryingValid(-50L).success.value.value shouldBe -50L
        NegZLong.tryingValid(-100L).success.value.value shouldBe -100L
      }

      it("returns an AssertionError wrapped in a Failure if the passed Long is greater than 0") {
        NegZLong.tryingValid(1L).failure.exception shouldBe an [AssertionError]
        NegZLong.tryingValid(99L).failure.exception shouldBe an [AssertionError]
      }
    }
    describe("should offer a passOrElse factory method that") {
      it("returns a Pass if the given Long is lesser than or equal 0") {
        NegZLong.passOrElse(0L)(i => i) shouldBe Pass
        NegZLong.passOrElse(-50L)(i => i) shouldBe Pass
        NegZLong.passOrElse(-100L)(i => i) shouldBe Pass
      }
      it("returns an error value produced by passing the given Long to the given function if the passed Long is greater than 0, wrapped in a Fail") {
        NegZLong.passOrElse(1L)(i => i) shouldBe Fail(1L)
        NegZLong.passOrElse(99L)(i => i.toLong + 3L) shouldBe Fail(102L)
      }
    }
    describe("should offer a goodOrElse factory method that") {
      it("returns a NegZInt wrapped in a Good if the given Long is lesser than or equal 0") {
        NegZLong.goodOrElse(0L)(i => i) shouldBe Good(NegZLong(0L))
        NegZLong.goodOrElse(-50L)(i => i) shouldBe Good(NegZLong(-50L))
        NegZLong.goodOrElse(-100L)(i => i) shouldBe Good(NegZLong(-100L))
      }
      it("returns an error value produced by passing the given Long to the given function if the passed Long is greater than 0, wrapped in a Bad") {
        NegZLong.goodOrElse(1L)(i => i) shouldBe Bad(1L)
        NegZLong.goodOrElse(99L)(i => i.toLong + 3L) shouldBe Bad(102L)
      }
    }
    describe("should offer a rightOrElse factory method that") {
      it("returns a NegZLong wrapped in a Right if the given Int is lesser than or equal 0") {
        NegZLong.rightOrElse(0L)(i => i) shouldBe Right(NegZLong(0L))
        NegZLong.rightOrElse(-50L)(i => i) shouldBe Right(NegZLong(-50L))
        NegZLong.rightOrElse(-100L)(i => i) shouldBe Right(NegZLong(-100L))
      }
      it("returns an error value produced by passing the given Long to the given function if the passed Long is greater than 0, wrapped in a Left") {
        NegZLong.rightOrElse(1L)(i => i) shouldBe Left(1L)
        NegZLong.rightOrElse(99L)(i => i.toLong + 3L) shouldBe Left(102L)
      }
    }
    describe("should offer an isValid predicate method that") {
      it("returns true if the passed Long is lesser than or equal to 0") {
        NegZLong.isValid(-50L) shouldBe true
        NegZLong.isValid(-100L) shouldBe true
        NegZLong.isValid(0L) shouldBe true
        NegZLong.isValid(-0L) shouldBe true
        NegZLong.isValid(99L) shouldBe false
      }
    }
    describe("should offer a fromOrElse factory method that") {
      it("returns a NegZLong if the passed Long is lesser than or equal to 0") {
        NegZLong.fromOrElse(-50L, NegZLong(-42L)).value shouldBe -50L
        NegZLong.fromOrElse(-100L, NegZLong(-42L)).value shouldBe -100L
        NegZLong.fromOrElse(0L, NegZLong(-42L)).value shouldBe 0L
      }
      it("returns a given default if the passed Long is NOT greater than 0") {
        NegZLong.fromOrElse(1L, NegZLong(-42L)).value shouldBe -42L
        NegZLong.fromOrElse(99L, NegZLong(-42L)).value shouldBe -42L
      }
    }
    it("should offer MaxValue and MinValue factory methods") {
      NegZLong.MaxValue shouldEqual NegZLong.from(0L).get
      // SKIP-DOTTY-START
      // not constant literal
      NegZLong.MinValue shouldEqual NegZLong(Long.MinValue)
      // SKIP-DOTTY-END
    }

    it("should be sortable") {
      val xs = List(NegZLong(-2), NegZLong(0), NegZLong(-1), NegZLong(-3))
      xs.sorted shouldEqual List(NegZLong(-3), NegZLong(-2), NegZLong(-1),
        NegZLong(0))
    }

    describe("when created with apply method") {

      it("should compile when -8 is passed in") {
        "NegZLong(-8)" should compile
        NegZLong(-8).value shouldEqual -8L
        "NegZLong(-8L)" should compile
        NegZLong(-8L).value shouldEqual -8L
      }

      it("should compile when 0 is passed in") {
        "NegZLong(0)" should compile
        NegZLong(0).value shouldEqual 0L
        "NegZLong(0L)" should compile
        NegZLong(0L).value shouldEqual 0L
      }

      it("should not compile when 8 is passed in") {
        "NegZLong(8)" shouldNot compile
        "NegZLong(8L)" shouldNot compile
      }

      it("should not compile when x is passed in") {
        val a: Int = -8
        "NegZLong(a)" shouldNot compile
        val b: Long = -8L
        "NegZLong(b)" shouldNot compile
      }
    }
    describe("when specified as a plain-old Long") {

      def takesNegZLong(pos: NegZLong): Long = pos.value

      it("should compile when -8 is passed in") {
        "takesNegZLong(-8)" should compile
        takesNegZLong(-8) shouldEqual -8L
        "takesNegZLong(-8L)" should compile
        takesNegZLong(-8L) shouldEqual -8L
      }

      it("should compile when 0 is passed in") {
        "takesNegZLong(0)" should compile
        takesNegZLong(0) shouldEqual 0L
        "takesNegZLong(0L)" should compile
        takesNegZLong(0L) shouldEqual 0L
      }

      it("should not compile when 8 is passed in") {
        "takesNegZLong(8)" shouldNot compile
        "takesNegZLong(8L)" shouldNot compile
      }

      it("should not compile when x is passed in") {
        val x: Int = -8
        "takesNegZLong(x)" shouldNot compile
        val b: Long = -8L
        "takesNegZLong(b)" shouldNot compile
      }
    }

    it("should offer a unary ~ method that is consistent with Long") {
      forAll { (pzlong: NegZLong) =>
        (~pzlong) shouldEqual (~(pzlong.toLong))
      }
    }

    it("should offer a unary + method that is consistent with Long") {
      forAll { (p: NegZLong) =>
        (+p).toLong shouldEqual (+(p.toLong))
      }
    }

    it("should offer a unary - method that returns PosZLong") {
      forAll { (p: NegZLong) =>
        (-p) shouldEqual (-(p.toLong))
      }
    }

    it("should offer << methods that are consistent with Long") {
      forAll { (pzlong: NegZLong, shift: Int) =>
        pzlong << shift shouldEqual pzlong.toLong << shift
      }
      forAll { (pzlong: NegZLong, shift: Long) =>
        pzlong << shift shouldEqual pzlong.toLong << shift
      }
    }

    it("should offer >>> methods that are consistent with Long") {
      forAll { (pzlong: NegZLong, shift: Int) =>
        pzlong >>> shift shouldEqual pzlong.toLong >>> shift
      }
      forAll { (pzlong: NegZLong, shift: Long) =>
        pzlong >>> shift shouldEqual pzlong.toLong >>> shift
      }
    }

    it("should offer >> methods that are consistent with Long") {
      forAll { (pzlong: NegZLong, shift: Int) =>
        pzlong >> shift shouldEqual pzlong.toLong >> shift
      }
      forAll { (pzlong: NegZLong, shift: Long) =>
        pzlong >> shift shouldEqual pzlong.toLong >> shift
      }
    }

    it("should offer a '|' method that is consistent with Long") {
      forAll { (pzlong: NegZLong, byte: Byte) =>
        (pzlong | byte) shouldEqual (pzlong.toLong | byte)
      }
      forAll { (pzlong: NegZLong, short: Short) =>
        (pzlong | short) shouldEqual (pzlong.toLong | short)
      }
      forAll { (pzlong: NegZLong, char: Char) =>
        (pzlong | char) shouldEqual (pzlong.toLong | char)
      }
      forAll { (pzlong: NegZLong, int: Int) =>
        (pzlong | int) shouldEqual (pzlong.toLong | int)
      }
      forAll { (pzlong: NegZLong, long: Long) =>
        (pzlong | long) shouldEqual (pzlong.toLong | long)
      }
    }

    it("should offer an '&' method that is consistent with Long") {
      forAll { (pzlong: NegZLong, byte: Byte) =>
        (pzlong & byte) shouldEqual (pzlong.toLong & byte)
      }
      forAll { (pzlong: NegZLong, short: Short) =>
        (pzlong & short) shouldEqual (pzlong.toLong & short)
      }
      forAll { (pzlong: NegZLong, char: Char) =>
        (pzlong & char) shouldEqual (pzlong.toLong & char)
      }
      forAll { (pzlong: NegZLong, int: Int) =>
        (pzlong & int) shouldEqual (pzlong.toLong & int)
      }
      forAll { (pzlong: NegZLong, long: Long) =>
        (pzlong & long) shouldEqual (pzlong.toLong & long)
      }
    }

    it("should offer an '^' method that is consistent with Long") {
      forAll { (pzlong: NegZLong, byte: Byte) =>
        (pzlong ^ byte) shouldEqual (pzlong.toLong ^ byte)
      }
      forAll { (pzlong: NegZLong, short: Short) =>
        (pzlong ^ short) shouldEqual (pzlong.toLong ^ short)
      }
      forAll { (pzlong: NegZLong, char: Char) =>
        (pzlong ^ char) shouldEqual (pzlong.toLong ^ char)
      }
      forAll { (pzlong: NegZLong, int: Int) =>
        (pzlong ^ int) shouldEqual (pzlong.toLong ^ int)
      }
      forAll { (pzlong: NegZLong, long: Long) =>
        (pzlong ^ long) shouldEqual (pzlong.toLong ^ long)
      }
    }

    it("should offer 'min' and 'max' methods that are consistent with Long") {
      forAll { (pzlong1: NegZLong, pzlong2: NegZLong) =>
        pzlong1.max(pzlong2).toLong shouldEqual pzlong1.toLong.max(pzlong2.toLong)
        pzlong1.min(pzlong2).toLong shouldEqual pzlong1.toLong.min(pzlong2.toLong)
      }
    }

    it("should offer a 'toBinaryString' method that is consistent with Long") {
      forAll { (pzlong: NegZLong) =>
        pzlong.toBinaryString shouldEqual pzlong.toLong.toBinaryString
      }
    }

    it("should offer a 'toHexString' method that is consistent with Long") {
      forAll { (pzlong: NegZLong) =>
        pzlong.toHexString shouldEqual pzlong.toLong.toHexString
      }
    }

    it("should offer a 'toOctalString' method that is consistent with Long") {
      forAll { (pzlong: NegZLong) =>
        pzlong.toOctalString shouldEqual pzlong.toLong.toOctalString
      }
    }

    // SKIP-SCALATESTJS,NATIVE-START
    it("should offer 'to' and 'until' method that is consistent with Long") {
      def rangeEqual[T](a: NumericRange[T], b: NumericRange[T]): Boolean =
        a.start == b.start && a.end == b.end && a.step == b.step

      forAll { (pzlong: NegZLong, end: Long, step: Long) =>
        rangeEqual(pzlong.until(end), pzlong.toLong.until(end)) shouldBe true
        rangeEqual(pzlong.until(end, step), pzlong.toLong.until(end, step)) shouldBe true
        rangeEqual(pzlong.to(end), pzlong.toLong.to(end)) shouldBe true
        rangeEqual(pzlong.to(end, step), pzlong.toLong.to(end, step)) shouldBe true
      }
    }
    // SKIP-SCALATESTJS,NATIVE-END

    it("should offer an ensuringValid method that takes a Long => Long, throwing AssertionError if the result is invalid") {
      NegZLong(-33L).ensuringValid(_ + 1L) shouldEqual NegZLong(-32L)
      an [AssertionError] should be thrownBy { NegZLong.MaxValue.ensuringValid(_ + 1L) }
    }
  }
}

