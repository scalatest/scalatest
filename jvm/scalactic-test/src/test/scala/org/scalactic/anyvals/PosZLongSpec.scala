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

//import org.scalactic.StrictCheckedEquality

trait PosZLongSpecSupport {

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

class PosZLongSpec extends funspec.AnyFunSpec with matchers.should.Matchers with GeneratorDrivenPropertyChecks with PosZLongSpecSupport {

  describe("A PosZLong") {
    describe("should offer a from factory method that") {
      it("returns Some[PosZLong] if the passed Long is greater than or equal to 0") {
        PosZLong.from(0L).value.value shouldBe 0L
        PosZLong.from(50L).value.value shouldBe 50L
        PosZLong.from(100L).value.value shouldBe 100L
      }
      it("returns None if the passed Long is NOT greater than or equal to 0") {
        PosZLong.from(-1L) shouldBe None
        PosZLong.from(-99L) shouldBe None
      }
    }
    describe("should offer an ensuringValid factory method that") {
      it("returns PosZLong if the passed Long is greater than or equal to 0") {
        PosZLong.ensuringValid(0L).value shouldBe 0L
        PosZLong.ensuringValid(50L).value shouldBe 50L
        PosZLong.ensuringValid(100L).value shouldBe 100L
      }
      it("throws AssertionError if the passed Long is NOT greater than or equal to 0") {
        an [AssertionError] should be thrownBy PosZLong.ensuringValid(-1L)
        an [AssertionError] should be thrownBy PosZLong.ensuringValid(-99L)
      }
    }
    describe("should offer a tryingValid factory method that") {
      import TryValues._
      it("returns a PosZLong wrapped in a Success if the passed Long is greater than or equal 0") {
        PosZLong.tryingValid(0L).success.value.value shouldBe 0L
        PosZLong.tryingValid(50L).success.value.value shouldBe 50L
        PosZLong.tryingValid(100L).success.value.value shouldBe 100L
      }

      it("returns an AssertionError wrapped in a Failure if the passed Long is lesser than 0") {
        PosZLong.tryingValid(-1L).failure.exception shouldBe an [AssertionError]
        PosZLong.tryingValid(-99L).failure.exception shouldBe an [AssertionError]
      }
    }
    describe("should offer a passOrElse factory method that") {
      it("returns a Pass if the given Long is greater than or equal 0") {
        PosZLong.passOrElse(0L)(i => i) shouldBe Pass
        PosZLong.passOrElse(50L)(i => i) shouldBe Pass
        PosZLong.passOrElse(100L)(i => i) shouldBe Pass
      }
      it("returns an error value produced by passing the given Long to the given function if the passed Long is lesser than 0, wrapped in a Fail") {
        PosZLong.passOrElse(-1L)(i => i) shouldBe Fail(-1L)
        PosZLong.passOrElse(-99L)(i => i.toLong + 3L) shouldBe Fail(-96L)
      }
    }
    describe("should offer a goodOrElse factory method that") {
      it("returns a PosZInt wrapped in a Good if the given Long is greater than or equal 0") {
        PosZLong.goodOrElse(0L)(i => i) shouldBe Good(PosZLong(0L))
        PosZLong.goodOrElse(50L)(i => i) shouldBe Good(PosZLong(50L))
        PosZLong.goodOrElse(100L)(i => i) shouldBe Good(PosZLong(100L))
      }
      it("returns an error value produced by passing the given Long to the given function if the passed Long is lesser than 0, wrapped in a Bad") {
        PosZLong.goodOrElse(-1L)(i => i) shouldBe Bad(-1L)
        PosZLong.goodOrElse(-99L)(i => i.toLong + 3L) shouldBe Bad(-96L)
      }
    }
    describe("should offer a rightOrElse factory method that") {
      it("returns a PosZLong wrapped in a Right if the given Int is greater than or equal 0") {
        PosZLong.rightOrElse(0L)(i => i) shouldBe Right(PosZLong(0L))
        PosZLong.rightOrElse(50L)(i => i) shouldBe Right(PosZLong(50L))
        PosZLong.rightOrElse(100L)(i => i) shouldBe Right(PosZLong(100L))
      }
      it("returns an error value produced by passing the given Long to the given function if the passed Long is lesser than 0, wrapped in a Left") {
        PosZLong.rightOrElse(-1L)(i => i) shouldBe Left(-1L)
        PosZLong.rightOrElse(-99L)(i => i.toLong + 3L) shouldBe Left(-96L)
      }
    }
    describe("should offer an isValid predicate method that") {
      it("returns true if the passed Long is greater than or equal to 0") {
        PosZLong.isValid(50L) shouldBe true
        PosZLong.isValid(100L) shouldBe true
        PosZLong.isValid(0L) shouldBe true
        PosZLong.isValid(-0L) shouldBe true
        PosZLong.isValid(-99L) shouldBe false
      }
    } 
    describe("should offer a fromOrElse factory method that") {
      it("returns a PosZLong if the passed Long is greater than or equal to 0") {
        PosZLong.fromOrElse(50L, PosZLong(42L)).value shouldBe 50L
        PosZLong.fromOrElse(100L, PosZLong(42L)).value shouldBe 100L
        PosZLong.fromOrElse(0L, PosZLong(42L)).value shouldBe 0L
      }
      it("returns a given default if the passed Long is NOT greater than or equal to 0") {
        PosZLong.fromOrElse(-1L, PosZLong(42L)).value shouldBe 42L
        PosZLong.fromOrElse(-99L, PosZLong(42L)).value shouldBe 42L
      }
    } 
    it("should offer MaxValue and MinValue factory methods") {
      PosZLong.MaxValue shouldEqual PosZLong.from(Long.MaxValue).get
      PosZLong.MinValue shouldEqual PosZLong(0L)
    }

    it("should be sortable") {
      val xs = List(PosZLong(2), PosZLong(0), PosZLong(1), PosZLong(3))
      xs.sorted shouldEqual List(PosZLong(0), PosZLong(1), PosZLong(2),
                                 PosZLong(3))
    }

    describe("when created with apply method") {

      it("should compile when 8 is passed in") {
        "PosZLong(8)" should compile
        PosZLong(8).value shouldEqual 8L
        "PosZLong(8L)" should compile
        PosZLong(8L).value shouldEqual 8L
      }

      it("should compile when 0 is passed in") {
        "PosZLong(0)" should compile
        PosZLong(0).value shouldEqual 0L
        "PosZLong(0L)" should compile
        PosZLong(0L).value shouldEqual 0L
      }

      it("should not compile when -8 is passed in") {
        "PosZLong(-8)" shouldNot compile
        "PosZLong(-8L)" shouldNot compile
      }

      it("should not compile when x is passed in") {
        val a: Int = -8
        "PosZLong(a)" shouldNot compile
        val b: Long = -8L
        "PosZLong(b)" shouldNot compile
      }
    }
    describe("when specified as a plain-old Long") {

      def takesPosZLong(pos: PosZLong): Long = pos.value

      it("should compile when 8 is passed in") {
        "takesPosZLong(8)" should compile
        takesPosZLong(8) shouldEqual 8L
        "takesPosZLong(8L)" should compile
        takesPosZLong(8L) shouldEqual 8L
      }

      it("should compile when 0 is passed in") {
        "takesPosZLong(0)" should compile
        takesPosZLong(0) shouldEqual 0L
        "takesPosZLong(0L)" should compile
        takesPosZLong(0L) shouldEqual 0L
      }

      it("should not compile when -8 is passed in") {
        "takesPosZLong(-8)" shouldNot compile
        "takesPosZLong(-8L)" shouldNot compile
      }

      it("should not compile when x is passed in") {
        val x: Int = -8
        "takesPosZLong(x)" shouldNot compile
        val b: Long = -8L
        "takesPosZLong(b)" shouldNot compile
      }
    }

    it("should offer a unary ~ method that is consistent with Long") {
      forAll { (pzlong: PosZLong) =>
        (~pzlong) shouldEqual (~(pzlong.toLong))
      }
    }

    it("should offer a unary + method that is consistent with Long") {
      forAll { (p: PosZLong) =>
        (+p).toLong shouldEqual (+(p.toLong))
      }
    }

    it("should offer a unary - method that returns NegZLong") {
      forAll { (p: PosZLong) =>
        (-p) shouldEqual (NegZLong.ensuringValid(-(p.toLong)))
      }
    }

    it("should offer << methods that are consistent with Long") {
      forAll { (pzlong: PosZLong, shift: Int) =>
        pzlong << shift shouldEqual pzlong.toLong << shift
      }
      forAll { (pzlong: PosZLong, shift: Long) =>
        pzlong << shift shouldEqual pzlong.toLong << shift
      }
    }

    it("should offer >>> methods that are consistent with Long") {
      forAll { (pzlong: PosZLong, shift: Int) =>
        pzlong >>> shift shouldEqual pzlong.toLong >>> shift
      }
      forAll { (pzlong: PosZLong, shift: Long) =>
        pzlong >>> shift shouldEqual pzlong.toLong >>> shift
      }
    }

    it("should offer >> methods that are consistent with Long") {
      forAll { (pzlong: PosZLong, shift: Int) =>
        pzlong >> shift shouldEqual pzlong.toLong >> shift
      }
      forAll { (pzlong: PosZLong, shift: Long) =>
        pzlong >> shift shouldEqual pzlong.toLong >> shift
      }
    }

    it("should offer a '|' method that is consistent with Long") {
      forAll { (pzlong: PosZLong, byte: Byte) =>
        (pzlong | byte) shouldEqual (pzlong.toLong | byte)
      }
      forAll { (pzlong: PosZLong, short: Short) =>
        (pzlong | short) shouldEqual (pzlong.toLong | short)
      }
      forAll { (pzlong: PosZLong, char: Char) =>
        (pzlong | char) shouldEqual (pzlong.toLong | char)
      }
      forAll { (pzlong: PosZLong, int: Int) =>
        (pzlong | int) shouldEqual (pzlong.toLong | int)
      }
      forAll { (pzlong: PosZLong, long: Long) =>
        (pzlong | long) shouldEqual (pzlong.toLong | long)
      }
    }

    it("should offer an '&' method that is consistent with Long") {
      forAll { (pzlong: PosZLong, byte: Byte) =>
        (pzlong & byte) shouldEqual (pzlong.toLong & byte)
      }
      forAll { (pzlong: PosZLong, short: Short) =>
        (pzlong & short) shouldEqual (pzlong.toLong & short)
      }
      forAll { (pzlong: PosZLong, char: Char) =>
        (pzlong & char) shouldEqual (pzlong.toLong & char)
      }
      forAll { (pzlong: PosZLong, int: Int) =>
        (pzlong & int) shouldEqual (pzlong.toLong & int)
      }
      forAll { (pzlong: PosZLong, long: Long) =>
        (pzlong & long) shouldEqual (pzlong.toLong & long)
      }
    }

    it("should offer an '^' method that is consistent with Long") {
      forAll { (pzlong: PosZLong, byte: Byte) =>
        (pzlong ^ byte) shouldEqual (pzlong.toLong ^ byte)
      }
      forAll { (pzlong: PosZLong, short: Short) =>
        (pzlong ^ short) shouldEqual (pzlong.toLong ^ short)
      }
      forAll { (pzlong: PosZLong, char: Char) =>
        (pzlong ^ char) shouldEqual (pzlong.toLong ^ char)
      }
      forAll { (pzlong: PosZLong, int: Int) =>
        (pzlong ^ int) shouldEqual (pzlong.toLong ^ int)
      }
      forAll { (pzlong: PosZLong, long: Long) =>
        (pzlong ^ long) shouldEqual (pzlong.toLong ^ long)
      }
    }

    it("should offer 'min' and 'max' methods that are consistent with Long") {
      forAll { (pzlong1: PosZLong, pzlong2: PosZLong) =>
        pzlong1.max(pzlong2).toLong shouldEqual pzlong1.toLong.max(pzlong2.toLong)
        pzlong1.min(pzlong2).toLong shouldEqual pzlong1.toLong.min(pzlong2.toLong)
      }
    }

    it("should offer a 'toBinaryString' method that is consistent with Long") {
      forAll { (pzlong: PosZLong) =>
        pzlong.toBinaryString shouldEqual pzlong.toLong.toBinaryString
      }
    }

    it("should offer a 'toHexString' method that is consistent with Long") {
      forAll { (pzlong: PosZLong) =>
        pzlong.toHexString shouldEqual pzlong.toLong.toHexString
      }
    }

    it("should offer a 'toOctalString' method that is consistent with Long") {
      forAll { (pzlong: PosZLong) =>
        pzlong.toOctalString shouldEqual pzlong.toLong.toOctalString
      }
    }

    // SKIP-SCALATESTJS,NATIVE-START
    it("should offer 'to' and 'until' method that is consistent with Long") {
      def rangeEqual[T](a: NumericRange[T], b: NumericRange[T]): Boolean =
        a.start == b.start && a.end == b.end && a.step == b.step

      forAll { (pzlong: PosZLong, end: Long, step: Long) =>
        rangeEqual(pzlong.until(end), pzlong.toLong.until(end)) shouldBe true
        rangeEqual(pzlong.until(end, step), pzlong.toLong.until(end, step)) shouldBe true
        rangeEqual(pzlong.to(end), pzlong.toLong.to(end)) shouldBe true
        rangeEqual(pzlong.to(end, step), pzlong.toLong.to(end, step)) shouldBe true
      }
    }
    // SKIP-SCALATESTJS,NATIVE-END

    it("should offer widening methods for basic types that are consistent with Long") {
      forAll { (pzlong: PosZLong) =>
        def widen(value: Long): Long = value
        widen(pzlong) shouldEqual widen(pzlong.toLong)
      }
      forAll { (pzlong: PosZLong) =>
        def widen(value: Float): Float = value
        widen(pzlong) shouldEqual widen(pzlong.toLong)
      }
      forAll { (pzlong: PosZLong) =>
        def widen(value: Double): Double = value
        widen(pzlong) shouldEqual widen(pzlong.toLong)
      }
      forAll { (pzlong: PosZLong) =>
        def widen(value: PosZFloat): PosZFloat = value
        widen(pzlong) shouldEqual widen(PosZFloat.from(pzlong.toLong).get)
      }
      forAll { (pzlong: PosZLong) =>
        def widen(value: PosZDouble): PosZDouble = value
        widen(pzlong) shouldEqual widen(PosZDouble.from(pzlong.toLong).get)
      }
    }
    it("should offer an ensuringValid method that takes a Long => Long, throwing AssertionError if the result is invalid") {
      PosZLong(33L).ensuringValid(_ + 1L) shouldEqual PosZLong(34L)
      an [AssertionError] should be thrownBy { PosZLong.MaxValue.ensuringValid(_ + 1L) }
    }
  }
}

