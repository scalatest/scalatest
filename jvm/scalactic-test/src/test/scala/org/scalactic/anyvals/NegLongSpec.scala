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
import OptionValues._
import org.scalactic.Equality
import org.scalatest.prop.GeneratorDrivenPropertyChecks

// SKIP-SCALATESTJS,NATIVE-START
import scala.collection.immutable.NumericRange
// SKIP-SCALATESTJS,NATIVE-END
import scala.util.{Failure, Success, Try}
import org.scalactic.{Pass, Fail}
import org.scalactic.{Good, Bad}

trait NegLongSpecSupport {

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

class NegLongSpec extends funspec.AnyFunSpec with matchers.should.Matchers with GeneratorDrivenPropertyChecks with NegLongSpecSupport {

  describe("A NegLong") {
    describe("should offer a from factory method that") {
      it("returns Some[NegLong] if the passed Long is lesser than 0") {
        NegLong.from(-50L).value.value shouldBe -50L
        NegLong.from(-100L).value.value shouldBe -100L
      }
      it("returns None if the passed Long is NOT lesser than 0") {
        NegLong.from(0L) shouldBe None
        NegLong.from(1L) shouldBe None
        NegLong.from(99L) shouldBe None
      }
    }
    describe("should offer an ensuringValid factory method that") {
      it("returns Some[NegLong if the passed Long is lesser than 0") {
        NegLong.ensuringValid(-50L).value shouldBe -50L
        NegLong.ensuringValid(-100L).value shouldBe -100L
      }
      it("throws AssertionError if the passed Long is NOT lesser than 0") {
        an [AssertionError] should be thrownBy NegLong.ensuringValid(0L)
        an [AssertionError] should be thrownBy NegLong.ensuringValid(1L)
        an [AssertionError] should be thrownBy NegLong.ensuringValid(99L)
      }
    }
    describe("should offer a tryingValid factory method that") {
      import TryValues._
      it("returns a NegLong wrapped in a Success if the passed Long is lesser than 0") {
        NegLong.tryingValid(-50L).success.value.value shouldBe -50L
        NegLong.tryingValid(-100L).success.value.value shouldBe -100L
      }

      it("returns an AssertionError wrapped in a Failure if the passed Long is NOT lesser than 0") {
        NegLong.tryingValid(0L).failure.exception shouldBe an [AssertionError]
        NegLong.tryingValid(1L).failure.exception shouldBe an [AssertionError]
        NegLong.tryingValid(99L).failure.exception shouldBe an [AssertionError]
      }
    }
    describe("should offer a passOrElse factory method that") {
      it("returns a Pass if the given Long is lesser than 0") {
        NegLong.passOrElse(-50L)(i => i) shouldBe Pass
        NegLong.passOrElse(-100L)(i => i) shouldBe Pass
      }
      it("returns an error value produced by passing the given Long to the given function if the passed Long is NOT lesser than 0, wrapped in a Fail") {
        NegLong.passOrElse(0L)(i => s"$i did not taste good") shouldBe Fail("0 did not taste good")
        NegLong.passOrElse(1L)(i => i) shouldBe Fail(1L)
        NegLong.passOrElse(99L)(i => i + 3L) shouldBe Fail(102L)
      }
    }
    describe("should offer a goodOrElse factory method that") {
      it("returns a NegLong wrapped in a Good if the given Long is lesser than 0") {
        NegLong.goodOrElse(-50L)(i => i) shouldBe Good(NegLong(-50L))
        NegLong.goodOrElse(-100L)(i => i) shouldBe Good(NegLong(-100L))
      }
      it("returns an error value produced by passing the given Long to the given function if the passed Long is NOT lesser than 0, wrapped in a Bad") {
        NegLong.goodOrElse(0L)(i => s"$i did not taste good") shouldBe Bad("0 did not taste good")
        NegLong.goodOrElse(1L)(i => i) shouldBe Bad(1L)
        NegLong.goodOrElse(99L)(i => i + 3L) shouldBe Bad(102L)
      }
    }
    describe("should offer a rightOrElse factory method that") {
      it("returns a NegLong wrapped in a Right if the given Long is lesser than 0") {
        NegLong.rightOrElse(-50L)(i => i) shouldBe Right(NegLong(-50L))
        NegLong.rightOrElse(-100L)(i => i) shouldBe Right(NegLong(-100L))
      }
      it("returns an error value produced by passing the given Long to the given function if the passed Long is NOT lesser than 0, wrapped in a Left") {
        NegLong.rightOrElse(0L)(i => s"$i did not taste good") shouldBe Left("0 did not taste good")
        NegLong.rightOrElse(1L)(i => i) shouldBe Left(1L)
        NegLong.rightOrElse(99L)(i => i + 3L) shouldBe Left(102L)
      }
    }
    describe("should offer an isValid predicate method that") {
      it("returns true if the passed Long is greater than 0") {
        NegLong.isValid(-50L) shouldBe true
        NegLong.isValid(-100L) shouldBe true
        NegLong.isValid(0L) shouldBe false
        NegLong.isValid(-0L) shouldBe false
        NegLong.isValid(1L) shouldBe false
        NegLong.isValid(99L) shouldBe false
      }
    }
    describe("should offer a fromOrElse factory method that") {
      it("returns a NegLong if the passed Long is greater than 0") {
        NegLong.fromOrElse(-50L, NegLong(-42L)).value shouldBe -50L
        NegLong.fromOrElse(-100L, NegLong(-42L)).value shouldBe -100L
      }
      it("returns a given default if the passed Long is NOT lesser than 0") {
        NegLong.fromOrElse(0L, NegLong(-42L)).value shouldBe -42L
        NegLong.fromOrElse(1L, NegLong(-42L)).value shouldBe -42L
        NegLong.fromOrElse(99L, NegLong(-42L)).value shouldBe -42L
      }
    }
    it("should offer MaxValue and MinValue factory methods") {
      NegLong.MaxValue shouldEqual NegLong.from(-1L).get
      // SKIP-DOTTY-START
      // not constant literal
      NegLong.MinValue shouldEqual NegLong(Long.MinValue)
      // SKIP-DOTTY-END
    }

    it("should be sortable") {
      val xs = List(NegLong(-2), NegLong(-4), NegLong(-1), NegLong(-3))
      xs.sorted shouldEqual List(NegLong(-4), NegLong(-3), NegLong(-2), NegLong(-1))
    }

    describe("when created with apply method") {

      it("should compile when -8 is passed in") {
        "NegLong(-8)" should compile
        NegLong(-8).value shouldEqual -8L
        "NegLong(-8L)" should compile
        NegLong(-8L).value shouldEqual -8L
      }

      it("should not compile when 0 is passed in") {
        "NegLong(0)" shouldNot compile
        "NegLong(0L)" shouldNot compile
      }

      it("should not compile when 8 is passed in") {
        "NegLong(8)" shouldNot compile
        "NegLong(8L)" shouldNot compile
      }

      it("should not compile when x is passed in") {
        val a: Int = -8
        "NegLong(a)" shouldNot compile
        val b: Long = -8L
        "NegLong(b)" shouldNot compile
      }
    }
    describe("when specified as a plain-old Long") {

      def takesNegLong(pos: NegLong): Long = pos.value

      it("should compile when -8 is passed in") {
        "takesNegLong(-8)" should compile
        takesNegLong(-8) shouldEqual -8L
        "takesNegLong(-8L)" should compile
        takesNegLong(-8L) shouldEqual -8L
      }

      it("should not compile when 0 is passed in") {
        "takesNegLong(0)" shouldNot compile
        "takesNegLong(0L)" shouldNot compile
      }

      it("should not compile when 8 is passed in") {
        "takesNegLong(8)" shouldNot compile
        "takesNegLong(8L)" shouldNot compile
      }

      it("should not compile when x is passed in") {
        val x: Int = -8
        "takesNegLong(x)" shouldNot compile
        val b: Long = -8L
        "takesNegLong(b)" shouldNot compile
      }

      it("should offer a unary ~ method that is consistent with Long") {
        forAll { (plong: NegLong) =>
          (~plong) shouldEqual (~(plong.toLong))
        }
      }

      it("should offer a unary + method that is consistent with Long") {
        forAll { (p: NegLong) =>
          (+p).toLong shouldEqual (+(p.toLong))
        }
      }

      it("should offer a unary - method that returns PosLong") {
        forAll { (p: NegLong) =>
          (-p) shouldEqual (-(p.toLong))
        }
      }

      it("should offer << methods that are consistent with Long") {
        forAll { (plong: NegLong, shift: Int) =>
          plong << shift shouldEqual plong.toLong << shift
        }
        forAll { (plong: NegLong, shift: Long) =>
          plong << shift shouldEqual plong.toLong << shift
        }
      }

      it("should offer >>> methods that are consistent with Long") {
        forAll { (plong: NegLong, shift: Int) =>
          plong >>> shift shouldEqual plong.toLong >>> shift
        }
        forAll { (plong: NegLong, shift: Long) =>
          plong >>> shift shouldEqual plong.toLong >>> shift
        }
      }

      it("should offer >> methods that are consistent with Long") {
        forAll { (plong: NegLong, shift: Int) =>
          plong >> shift shouldEqual plong.toLong >> shift
        }
        forAll { (plong: NegLong, shift: Long) =>
          plong >> shift shouldEqual plong.toLong >> shift
        }
      }

      it("should offer a '|' method that is consistent with Long") {
        forAll { (plong: NegLong, byte: Byte) =>
          (plong | byte) shouldEqual (plong.toLong | byte)
        }
        forAll { (plong: NegLong, short: Short) =>
          (plong | short) shouldEqual (plong.toLong | short)
        }
        forAll { (plong: NegLong, char: Char) =>
          (plong | char) shouldEqual (plong.toLong | char)
        }
        forAll { (plong: NegLong, int: Int) =>
          (plong | int) shouldEqual (plong.toLong | int)
        }
        forAll { (plong: NegLong, long: Long) =>
          (plong | long) shouldEqual (plong.toLong | long)
        }
      }

      it("should offer an '&' method that is consistent with Long") {
        forAll { (plong: NegLong, byte: Byte) =>
          (plong & byte) shouldEqual (plong.toLong & byte)
        }
        forAll { (plong: NegLong, short: Short) =>
          (plong & short) shouldEqual (plong.toLong & short)
        }
        forAll { (plong: NegLong, char: Char) =>
          (plong & char) shouldEqual (plong.toLong & char)
        }
        forAll { (plong: NegLong, int: Int) =>
          (plong & int) shouldEqual (plong.toLong & int)
        }
        forAll { (plong: NegLong, long: Long) =>
          (plong & long) shouldEqual (plong.toLong & long)
        }
      }

      it("should offer an '^' method that is consistent with Long") {
        forAll { (plong: NegLong, byte: Byte) =>
          (plong ^ byte) shouldEqual (plong.toLong ^ byte)
        }
        forAll { (plong: NegLong, short: Short) =>
          (plong ^ short) shouldEqual (plong.toLong ^ short)
        }
        forAll { (plong: NegLong, char: Char) =>
          (plong ^ char) shouldEqual (plong.toLong ^ char)
        }
        forAll { (plong: NegLong, int: Int) =>
          (plong ^ int) shouldEqual (plong.toLong ^ int)
        }
        forAll { (plong: NegLong, long: Long) =>
          (plong ^ long) shouldEqual (plong.toLong ^ long)
        }
      }

      it("should offer 'min' and 'max' methods that are consistent with Long") {
        forAll { (plong1: NegLong, plong2: NegLong) =>
          plong1.max(plong2).toLong shouldEqual plong1.toLong.max(plong2.toLong)
          plong1.min(plong2).toLong shouldEqual plong1.toLong.min(plong2.toLong)
        }
      }

      it("should offer a 'toBinaryString' method that is consistent with Long") {
        forAll { (plong: NegLong) =>
          plong.toBinaryString shouldEqual plong.toLong.toBinaryString
        }
      }

      it("should offer a 'toHexString' method that is consistent with Long") {
        forAll { (plong: NegLong) =>
          plong.toHexString shouldEqual plong.toLong.toHexString
        }
      }

      it("should offer a 'toOctalString' method that is consistent with Long") {
        forAll { (plong: NegLong) =>
          plong.toOctalString shouldEqual plong.toLong.toOctalString
        }
      }

      // SKIP-SCALATESTJS,NATIVE-START
      it("should offer 'to' and 'until' method that is consistent with Long") {
        def rangeEqual[T](a: NumericRange[T], b: NumericRange[T]): Boolean =
          a.start == b.start && a.end == b.end && a.step == b.step

        forAll { (plong: NegLong, end: Long, step: Long) =>
          rangeEqual(plong.until(end), plong.toLong.until(end)) shouldBe true
          rangeEqual(plong.until(end, step), plong.toLong.until(end, step)) shouldBe true
          rangeEqual(plong.to(end), plong.toLong.to(end)) shouldBe true
          rangeEqual(plong.to(end, step), plong.toLong.to(end, step)) shouldBe true
        }
      }
      // SKIP-SCALATESTJS,NATIVE-END
    }
    it("should offer an ensuringValid method that takes an Long => Long, throwing AssertionError if the result is invalid") {
      NegLong(-33L).ensuringValid(_ + 1L) shouldEqual NegLong(-32L)
      an [AssertionError] should be thrownBy { NegLong.MaxValue.ensuringValid(_ + 1L) }
    }
  }
}