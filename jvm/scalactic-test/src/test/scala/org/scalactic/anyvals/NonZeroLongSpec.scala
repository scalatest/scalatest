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
import org.scalactic.{Pass, Fail}
import org.scalactic.{Good, Bad}

// SKIP-SCALATESTJS,NATIVE-START
import scala.collection.immutable.NumericRange
// SKIP-SCALATESTJS,NATIVE-END
import scala.util.{Failure, Success, Try}

trait NonZeroLongSpecSupport {

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

class NonZeroLongSpec extends funspec.AnyFunSpec with matchers.should.Matchers with GeneratorDrivenPropertyChecks with NonZeroLongSpecSupport {

  describe("A NonZeroLong") {
    describe("should offer a from factory method that") {
      it("returns Some[NonZeroLong] if the passed Long is greater than 0") {
        NonZeroLong.from(50L).value.value shouldBe 50L
        NonZeroLong.from(100L).value.value shouldBe 100L
      }

      it("returns Some[NonZeroLong] if the passed Long is lesser than 0") {
        NonZeroLong.from(-1L).value.value shouldBe -1L
        NonZeroLong.from(-99L).value.value shouldBe -99L
      }

      it("returns None if the passed Long is 0") {
        NonZeroLong.from(0L) shouldBe None
      }
    }
    describe("should offer an ensuringValid factory method that") {
      it("returns Some[NonZeroLong if the passed Long is greater than 0") {
        NonZeroLong.ensuringValid(50L).value shouldBe 50L
        NonZeroLong.ensuringValid(100L).value shouldBe 100L
      }
      it("returns Some[NonZeroLong if the passed Long is lesser than 0") {
        NonZeroLong.ensuringValid(-1L).value shouldBe -1L
        NonZeroLong.ensuringValid(-99L).value shouldBe -99L
      }
      it("throws AssertionError if the passed Long is NOT greater than 0") {
        an [AssertionError] should be thrownBy NonZeroLong.ensuringValid(0L)
      }
    }
    describe("should offer a tryingValid factory method that") {
      import TryValues._
      it("returns a NonZeroLong wrapped in a Success if the passed Long is non-zero") {
        NonZeroLong.tryingValid(50L).success.value.value shouldBe 50L
        NonZeroLong.tryingValid(100L).success.value.value shouldBe 100L
        NonZeroLong.tryingValid(-50L).success.value.value shouldBe -50L
        NonZeroLong.tryingValid(-100L).success.value.value shouldBe -100L
      }

      it("returns an AssertionError wrapped in a Failure if the passed Long is NOT non-zero") {
        NonZeroLong.tryingValid(0L).failure.exception shouldBe an [AssertionError]
      }
    }
    describe("should offer a passOrElse factory method that") {
      it("returns a Pass if the given Long is non-zero") {
        NonZeroLong.passOrElse(50L)(i => i) shouldBe Pass
        NonZeroLong.passOrElse(100L)(i => i) shouldBe Pass

        NonZeroLong.passOrElse(-1L)(i => i) shouldBe Pass
        NonZeroLong.passOrElse(-99L)(i => i) shouldBe Pass
      }
      it("returns an error value produced by passing the given Long to the given function if the passed Long is NOT non-zero, wrapped in a Fail") {
        NonZeroLong.passOrElse(0L)(i => s"$i did not taste good") shouldBe Fail("0 did not taste good")
      }
    }
    describe("should offer a goodOrElse factory method that") {
      it("returns a NonZeroLong wrapped in a Good if the given Long is non-zero") {
        NonZeroLong.goodOrElse(50)(i => i) shouldBe Good(NonZeroLong(50))
        NonZeroLong.goodOrElse(100)(i => i) shouldBe Good(NonZeroLong(100))

        NonZeroLong.goodOrElse(-1)(i => i) shouldBe Good(NonZeroLong(-1))
        NonZeroLong.goodOrElse(-99)(i => i) shouldBe Good(NonZeroLong(-99))
      }
      it("returns an error value produced by passing the given Long to the given function if the passed Long is NOT non-zero, wrapped in a Bad") {
        NonZeroLong.goodOrElse(0)(i => s"$i did not taste good") shouldBe Bad("0 did not taste good")
      }
    }
    describe("should offer a rightOrElse factory method that") {
      it("returns a NonZeroLong wrapped in a Right if the given Long is non-zero") {
        NonZeroLong.rightOrElse(50L)(i => i) shouldBe Right(NonZeroLong(50L))
        NonZeroLong.rightOrElse(100L)(i => i) shouldBe Right(NonZeroLong(100L))

        NonZeroLong.rightOrElse(-1L)(i => i) shouldBe Right(NonZeroLong(-1L))
        NonZeroLong.rightOrElse(-99L)(i => i) shouldBe Right(NonZeroLong(-99L))
      }
      it("returns an error value produced by passing the given Long to the given function if the passed Long is NOT non-zero, wrapped in a Left") {
        NonZeroLong.rightOrElse(0L)(i => s"$i did not taste good") shouldBe Left("0 did not taste good")
      }
    }
    describe("should offer an isValid predicate method that") {
      it("returns true if the passed Long is not 0") {
        NonZeroLong.isValid(50L) shouldBe true
        NonZeroLong.isValid(100L) shouldBe true
        NonZeroLong.isValid(0L) shouldBe false
        NonZeroLong.isValid(-0L) shouldBe false
        NonZeroLong.isValid(-1L) shouldBe true
        NonZeroLong.isValid(-99L) shouldBe true
      }
    }
    describe("should offer a fromOrElse factory method that") {
      it("returns a NonZeroLong if the passed Long is greater than 0") {
        NonZeroLong.fromOrElse(50L, NonZeroLong(42L)).value shouldBe 50L
        NonZeroLong.fromOrElse(100L, NonZeroLong(42L)).value shouldBe 100L

      }
      it("returns a NonZeroLong if the passed Long is lesser than 0") {
        NonZeroLong.fromOrElse(-1L, NonZeroLong(42L)).value shouldBe -1L
        NonZeroLong.fromOrElse(-99L, NonZeroLong(42L)).value shouldBe -99L
      }
      it("returns a given default if the passed Long is NOT greater than 0") {
        NonZeroLong.fromOrElse(0L, NonZeroLong(42L)).value shouldBe 42L
      }
    }
    it("should offer MaxValue and MinValue factory methods") {
      NonZeroLong.MaxValue shouldEqual NonZeroLong.from(Long.MaxValue).get
      NonZeroLong.MinValue shouldEqual NonZeroLong.from(Long.MinValue).get
    }

    it("should be sortable") {
      val xs = List(NonZeroLong(2), NonZeroLong(4), NonZeroLong(1), NonZeroLong(3))
      xs.sorted shouldEqual List(NonZeroLong(1), NonZeroLong(2), NonZeroLong(3), NonZeroLong(4))
    }

    describe("when created with apply method") {

      it("should compile when 8 is passed in") {
        "NonZeroLong(8)" should compile
        NonZeroLong(8).value shouldEqual 8L
        "NonZeroLong(8L)" should compile
        NonZeroLong(8L).value shouldEqual 8L
      }

      it("should not compile when 0 is passed in") {
        "NonZeroLong(0)" shouldNot compile
        "NonZeroLong(0L)" shouldNot compile
      }

      it("should compile when -8 is passed in") {
        "NonZeroLong(-8)" should compile
        NonZeroLong(-8).value shouldEqual -8L
        "NonZeroLong(-8L)" should compile
        NonZeroLong(-8L).value shouldEqual -8L
      }

      it("should not compile when x is passed in") {
        val a: Int = -8
        "NonZeroLong(a)" shouldNot compile
        val b: Long = -8L
        "NonZeroLong(b)" shouldNot compile
      }
    }
    describe("when specified as a plain-old Long") {

      def takesNonZeroLong(pos: NonZeroLong): Long = pos.value

      it("should compile when 8 is passed in") {
        "takesNonZeroLong(8)" should compile
        takesNonZeroLong(8) shouldEqual 8L
        "takesNonZeroLong(8L)" should compile
        takesNonZeroLong(8L) shouldEqual 8L
      }

      it("should not compile when 0 is passed in") {
        "takesNonZeroLong(0)" shouldNot compile
        "takesNonZeroLong(0L)" shouldNot compile
      }

      it("should compile when -8 is passed in") {
        "takesNonZeroLong(-8)" should compile
        takesNonZeroLong(-8) shouldEqual -8L
        "takesNonZeroLong(-8L)" should compile
        takesNonZeroLong(-8L) shouldEqual -8L
      }

      it("should not compile when x is passed in") {
        val x: Int = -8
        "takesNonZeroLong(x)" shouldNot compile
        val b: Long = -8L
        "takesNonZeroLong(b)" shouldNot compile
      }

      it("should offer a unary ~ method that is consistent with Long") {
        forAll { (plong: NonZeroLong) =>
          (~plong) shouldEqual (~(plong.toLong))
        }
      }

      it("should offer a unary + method that is consistent with Long") {
        forAll { (p: NonZeroLong) =>
          (+p).toLong shouldEqual (+(p.toLong))
        }
      }

      it("should offer a unary - method that returns NonZeroLong") {
        forAll { (p: NonZeroLong) =>
          (-p) shouldEqual (NonZeroLong.ensuringValid(-(p.toLong)))
        }
      }

      it("should offer << methods that are consistent with Long") {
        forAll { (plong: NonZeroLong, shift: Int) =>
          plong << shift shouldEqual plong.toLong << shift
        }
        forAll { (plong: NonZeroLong, shift: Long) =>
          plong << shift shouldEqual plong.toLong << shift
        }
      }

      it("should offer >>> methods that are consistent with Long") {
        forAll { (plong: NonZeroLong, shift: Int) =>
          plong >>> shift shouldEqual plong.toLong >>> shift
        }
        forAll { (plong: NonZeroLong, shift: Long) =>
          plong >>> shift shouldEqual plong.toLong >>> shift
        }
      }

      it("should offer >> methods that are consistent with Long") {
        forAll { (plong: NonZeroLong, shift: Int) =>
          plong >> shift shouldEqual plong.toLong >> shift
        }
        forAll { (plong: NonZeroLong, shift: Long) =>
          plong >> shift shouldEqual plong.toLong >> shift
        }
      }

      it("should offer a '|' method that is consistent with Long") {
        forAll { (plong: NonZeroLong, byte: Byte) =>
          (plong | byte) shouldEqual (plong.toLong | byte)
        }
        forAll { (plong: NonZeroLong, short: Short) =>
          (plong | short) shouldEqual (plong.toLong | short)
        }
        forAll { (plong: NonZeroLong, char: Char) =>
          (plong | char) shouldEqual (plong.toLong | char)
        }
        forAll { (plong: NonZeroLong, int: Int) =>
          (plong | int) shouldEqual (plong.toLong | int)
        }
        forAll { (plong: NonZeroLong, long: Long) =>
          (plong | long) shouldEqual (plong.toLong | long)
        }
      }

      it("should offer an '&' method that is consistent with Long") {
        forAll { (plong: NonZeroLong, byte: Byte) =>
          (plong & byte) shouldEqual (plong.toLong & byte)
        }
        forAll { (plong: NonZeroLong, short: Short) =>
          (plong & short) shouldEqual (plong.toLong & short)
        }
        forAll { (plong: NonZeroLong, char: Char) =>
          (plong & char) shouldEqual (plong.toLong & char)
        }
        forAll { (plong: NonZeroLong, int: Int) =>
          (plong & int) shouldEqual (plong.toLong & int)
        }
        forAll { (plong: NonZeroLong, long: Long) =>
          (plong & long) shouldEqual (plong.toLong & long)
        }
      }

      it("should offer an '^' method that is consistent with Long") {
        forAll { (plong: NonZeroLong, byte: Byte) =>
          (plong ^ byte) shouldEqual (plong.toLong ^ byte)
        }
        forAll { (plong: NonZeroLong, short: Short) =>
          (plong ^ short) shouldEqual (plong.toLong ^ short)
        }
        forAll { (plong: NonZeroLong, char: Char) =>
          (plong ^ char) shouldEqual (plong.toLong ^ char)
        }
        forAll { (plong: NonZeroLong, int: Int) =>
          (plong ^ int) shouldEqual (plong.toLong ^ int)
        }
        forAll { (plong: NonZeroLong, long: Long) =>
          (plong ^ long) shouldEqual (plong.toLong ^ long)
        }
      }

      it("should offer 'min' and 'max' methods that are consistent with Long") {
        forAll { (plong1: NonZeroLong, plong2: NonZeroLong) =>
          plong1.max(plong2).toLong shouldEqual plong1.toLong.max(plong2.toLong)
          plong1.min(plong2).toLong shouldEqual plong1.toLong.min(plong2.toLong)
        }
      }

      it("should offer a 'toBinaryString' method that is consistent with Long") {
        forAll { (plong: NonZeroLong) =>
          plong.toBinaryString shouldEqual plong.toLong.toBinaryString
        }
      }

      it("should offer a 'toHexString' method that is consistent with Long") {
        forAll { (plong: NonZeroLong) =>
          plong.toHexString shouldEqual plong.toLong.toHexString
        }
      }

      it("should offer a 'toOctalString' method that is consistent with Long") {
        forAll { (plong: NonZeroLong) =>
          plong.toOctalString shouldEqual plong.toLong.toOctalString
        }
      }

      // SKIP-SCALATESTJS,NATIVE-START
      it("should offer 'to' and 'until' method that is consistent with Long") {
        def rangeEqual[T](a: NumericRange[T], b: NumericRange[T]): Boolean =
          a.start == b.start && a.end == b.end && a.step == b.step

        forAll { (plong: NonZeroLong, end: Long, step: Long) =>
          rangeEqual(plong.until(end), plong.toLong.until(end)) shouldBe true
          rangeEqual(plong.until(end, step), plong.toLong.until(end, step)) shouldBe true
          rangeEqual(plong.to(end), plong.toLong.to(end)) shouldBe true
          rangeEqual(plong.to(end, step), plong.toLong.to(end, step)) shouldBe true
        }
      }
      // SKIP-SCALATESTJS,NATIVE-END

      it("should offer widening methods for basic types that are consistent with Long") {
        forAll { (plong: NonZeroLong) =>
          def widen(value: Long): Long = value
          widen(plong) shouldEqual widen(plong.toLong)
        }
        forAll { (plong: NonZeroLong) =>
          def widen(value: Float): Float = value
          widen(plong) shouldEqual widen(plong.toLong)
        }
        forAll { (plong: NonZeroLong) =>
          def widen(value: Double): Double = value
          widen(plong) shouldEqual widen(plong.toLong)
        }
        /*forAll { (plong: NonZeroLong) =>
          def widen(value: NonZeroFloat): NonZeroFloat = value
          widen(plong) shouldEqual widen(NonZeroFloat.from(plong.toLong).get)
        }
        forAll { (plong: NonZeroLong) =>
          def widen(value: NonZeroDouble): NonZeroDouble = value
          widen(plong) shouldEqual widen(NonZeroDouble.from(plong.toLong).get)
        }*/
      }
    }
    it("should offer an ensuringValid method that takes an Long => Long, throwing AssertionError if the result is invalid") {
      NonZeroLong(33L).ensuringValid(_ + 1L) shouldEqual NonZeroLong(34L)
      an [AssertionError] should be thrownBy { NonZeroLong(-1L).ensuringValid(_ + 1L) }
    }
  }
}
