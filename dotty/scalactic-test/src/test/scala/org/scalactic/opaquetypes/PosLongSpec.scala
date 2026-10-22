/*
 * Copyright 2001-2026 Artima, Inc.
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
import org.scalactic.Equality
import org.scalatest.prop.GeneratorDrivenPropertyChecks

// SKIP-SCALATESTJS,NATIVE-START
import scala.collection.immutable.NumericRange
// SKIP-SCALATESTJS,NATIVE-END
import scala.util.{Failure, Success, Try}
import org.scalactic.{Pass, Fail}
import org.scalactic.{Good, Bad}

import PosLongs.{ PosLong, PosZLong }
import NegLongs.NegLong
import PosFloats.{ PosFloat, PosZFloat }
import PosDoubles.{ PosDouble, PosZDouble }
import NonZeroLongs.NonZeroLong
import NonZeroFloats.NonZeroFloat
import NonZeroDoubles.NonZeroDouble

trait PosLongSpecSupport {

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

class PosLongSpec extends funspec.AnyFunSpec with matchers.should.Matchers with GeneratorDrivenPropertyChecks with PosLongSpecSupport {

  describe("A PosLong") {
    describe("should offer a from factory method that") {
      it("returns Some[PosLong] if the passed Long is greater than 0") {
        PosLong.from(50L) shouldBe Some(PosLong(50L))
        PosLong.from(100L) shouldBe Some(PosLong(100L))
      }
      it("returns None if the passed Long is NOT greater than 0") {
        PosLong.from(0L) shouldBe None
        PosLong.from(-1L) shouldBe None
        PosLong.from(-99L) shouldBe None
      }
    } 
    describe("should offer an ensuringValid factory method that") {
      it("returns Some[PosLong if the passed Long is greater than 0") {
        PosLong.ensuringValid(50L).value shouldBe 50L
        PosLong.ensuringValid(100L).value shouldBe 100L
      }
      it("throws AssertionError if the passed Long is NOT greater than 0") {
        an [AssertionError] should be thrownBy PosLong.ensuringValid(0L)
        an [AssertionError] should be thrownBy PosLong.ensuringValid(-1L)
        an [AssertionError] should be thrownBy PosLong.ensuringValid(-99L)
      }
    }
    describe("should offer a tryingValid factory method that") {
      import TryValues.*
      it("returns a PosLong wrapped in a Success if the passed Long is greater than 0") {
        PosLong.tryingValid(50L).success.value.value shouldBe 50L
        PosLong.tryingValid(100L).success.value.value shouldBe 100L
      }

      it("returns an AssertionError wrapped in a Failure if the passed Long is NOT greater than 0") {
        PosLong.tryingValid(0L).failure.exception shouldBe an [AssertionError]
        PosLong.tryingValid(-1L).failure.exception shouldBe an [AssertionError]
        PosLong.tryingValid(-99L).failure.exception shouldBe an [AssertionError]
      }
    }
    describe("should offer a passOrElse factory method that") {
      it("returns a Pass if the given Long is greater than 0") {
        PosLong.passOrElse(50L)(i => i) shouldBe Pass
        PosLong.passOrElse(100L)(i => i) shouldBe Pass
      }
      it("returns an error value produced by passing the given Long to the given function if the passed Long is NOT greater than 0, wrapped in a Fail") {
        PosLong.passOrElse(0L)(i => s"$i did not taste good") shouldBe Fail("0 did not taste good")
        PosLong.passOrElse(-1L)(i => i) shouldBe Fail(-1L)
        PosLong.passOrElse(-99L)(i => i + 3L) shouldBe Fail(-96L)
      }
    }
    describe("should offer a goodOrElse factory method that") {
      it("returns a PosLong wrapped in a Good if the given Long is greater than 0") {
        PosLong.goodOrElse(50L)(i => i) shouldBe Good(PosLong(50L))
        PosLong.goodOrElse(100L)(i => i) shouldBe Good(PosLong(100L))
      }
      it("returns an error value produced by passing the given Long to the given function if the passed Long is NOT greater than 0, wrapped in a Bad") {
        PosLong.goodOrElse(0L)(i => s"$i did not taste good") shouldBe Bad("0 did not taste good")
        PosLong.goodOrElse(-1L)(i => i) shouldBe Bad(-1L)
        PosLong.goodOrElse(-99L)(i => i + 3L) shouldBe Bad(-96L)
      }
    }
    describe("should offer a rightOrElse factory method that") {
      it("returns a PosLong wrapped in a Right if the given Long is greater than 0") {
        PosLong.rightOrElse(50L)(i => i) shouldBe Right(PosLong(50L))
        PosLong.rightOrElse(100L)(i => i) shouldBe Right(PosLong(100L))
      }
      it("returns an error value produced by passing the given Long to the given function if the passed Long is NOT greater than 0, wrapped in a Left") {
        PosLong.rightOrElse(0L)(i => s"$i did not taste good") shouldBe Left("0 did not taste good")
        PosLong.rightOrElse(-1L)(i => i) shouldBe Left(-1L)
        PosLong.rightOrElse(-99L)(i => i + 3L) shouldBe Left(-96L)
      }
    }
    describe("should offer an isValid predicate method that") {
      it("returns true if the passed Long is greater than 0") {
        PosLong.isValid(50L) shouldBe true
        PosLong.isValid(100L) shouldBe true
        PosLong.isValid(0L) shouldBe false
        PosLong.isValid(-0L) shouldBe false
        PosLong.isValid(-1L) shouldBe false
        PosLong.isValid(-99L) shouldBe false
      }
    } 
    describe("should offer a fromOrElse factory method that") {
      it("returns a PosLong if the passed Long is greater than 0") {
        PosLong.fromOrElse(50L, PosLong(42L)).value shouldBe 50L
        PosLong.fromOrElse(100L, PosLong(42L)).value shouldBe 100L
      }
      it("returns a given default if the passed Long is NOT greater than 0") {
        PosLong.fromOrElse(0L, PosLong(42L)).value shouldBe 42L
        PosLong.fromOrElse(-1L, PosLong(42L)).value shouldBe 42L
        PosLong.fromOrElse(-99L, PosLong(42L)).value shouldBe 42L
      }
    } 
    it("should offer MaxValue and MinValue factory methods") {
      PosLong.MaxValue shouldEqual PosLong.from(Long.MaxValue).get
      PosLong.MinValue shouldEqual PosLong(1L)
    }

    it("should be sortable") {
      val xs = List(PosLong(2), PosLong(4), PosLong(1), PosLong(3))
      xs.sorted shouldEqual List(PosLong(1), PosLong(2), PosLong(3), PosLong(4))
    }

    describe("when created with apply method") {

      it("should compile when 8 is passed in") {
        "PosLong(8)" should compile
        PosLong(8).value shouldEqual 8L
        "PosLong(8L)" should compile
        PosLong(8L).value shouldEqual 8L
      }

      it("should not compile when 0 is passed in") {
        "PosLong(0)" shouldNot compile
        "PosLong(0L)" shouldNot compile
      }

      it("should not compile when -8 is passed in") {
        "PosLong(-8)" shouldNot compile
        "PosLong(-8L)" shouldNot compile
      }

      it("should not compile when x is passed in") {
        val a: Int = -8
        "PosLong(a)" shouldNot compile
        val b: Long = -8L
        "PosLong(b)" shouldNot compile
      }
    }
    describe("when specified as a plain-old Long") {

      def takesPosLong(pos: PosLong): Long = pos.value

      it("should compile when 8 is passed in") {
        "takesPosLong(8)" should compile
        takesPosLong(8) shouldEqual 8L
        "takesPosLong(8L)" should compile
        takesPosLong(8L) shouldEqual 8L
      }

      it("should not compile when 0 is passed in") {
        "takesPosLong(0)" shouldNot compile
        "takesPosLong(0L)" shouldNot compile
      }

      it("should not compile when -8 is passed in") {
        "takesPosLong(-8)" shouldNot compile
        "takesPosLong(-8L)" shouldNot compile
      }

      it("should not compile when x is passed in") {
        val x: Int = -8
        "takesPosLong(x)" shouldNot compile
        val b: Long = -8L
        "takesPosLong(b)" shouldNot compile
      }

      it("should offer a unary ~ method that is consistent with Long") {
        forAll { (plong: PosLong) =>
          (~plong) shouldEqual (~(plong.toLong))
        }
      }

      it("should offer a unary + method that is consistent with Long") {
        forAll { (p: PosLong) =>
          (+p).toLong shouldEqual (+(p.toLong))
        }
      }

      it("should offer a unary - method that returns NegLong") {
        forAll { (p: PosLong) =>
          (-p) shouldEqual (NegLong.ensuringValid(-(p.toLong)))
        }
      }

      it("should offer << methods that are consistent with Long") {
        forAll { (plong: PosLong, shift: Int) =>
          plong << shift shouldEqual plong.toLong << shift
        }
        forAll { (plong: PosLong, shift: Long) =>
          plong << shift shouldEqual plong.toLong << shift
        }
      }

      it("should offer >>> methods that are consistent with Long") {
        forAll { (plong: PosLong, shift: Int) =>
          plong >>> shift shouldEqual plong.toLong >>> shift
        }
        forAll { (plong: PosLong, shift: Long) =>
          plong >>> shift shouldEqual plong.toLong >>> shift
        }
      }

      it("should offer >> methods that are consistent with Long") {
        forAll { (plong: PosLong, shift: Int) =>
          plong >> shift shouldEqual plong.toLong >> shift
        }
        forAll { (plong: PosLong, shift: Long) =>
          plong >> shift shouldEqual plong.toLong >> shift
        }
      }

      it("should offer a '|' method that is consistent with Long") {
        forAll { (plong: PosLong, byte: Byte) =>
          (plong | byte) shouldEqual (plong.toLong | byte)
        }
        forAll { (plong: PosLong, short: Short) =>
          (plong | short) shouldEqual (plong.toLong | short)
        }
        forAll { (plong: PosLong, char: Char) =>
          (plong | char) shouldEqual (plong.toLong | char)
        }
        forAll { (plong: PosLong, int: Int) =>
          (plong | int) shouldEqual (plong.toLong | int)
        }
        forAll { (plong: PosLong, long: Long) =>
          (plong | long) shouldEqual (plong.toLong | long)
        }
      }

      it("should offer an '&' method that is consistent with Long") {
        forAll { (plong: PosLong, byte: Byte) =>
          (plong & byte) shouldEqual (plong.toLong & byte)
        }
        forAll { (plong: PosLong, short: Short) =>
          (plong & short) shouldEqual (plong.toLong & short)
        }
        forAll { (plong: PosLong, char: Char) =>
          (plong & char) shouldEqual (plong.toLong & char)
        }
        forAll { (plong: PosLong, int: Int) =>
          (plong & int) shouldEqual (plong.toLong & int)
        }
        forAll { (plong: PosLong, long: Long) =>
          (plong & long) shouldEqual (plong.toLong & long)
        }
      }

      it("should offer an '^' method that is consistent with Long") {
        forAll { (plong: PosLong, byte: Byte) =>
          (plong ^ byte) shouldEqual (plong.toLong ^ byte)
        }
        forAll { (plong: PosLong, short: Short) =>
          (plong ^ short) shouldEqual (plong.toLong ^ short)
        }
        forAll { (plong: PosLong, char: Char) =>
          (plong ^ char) shouldEqual (plong.toLong ^ char)
        }
        forAll { (plong: PosLong, int: Int) =>
          (plong ^ int) shouldEqual (plong.toLong ^ int)
        }
        forAll { (plong: PosLong, long: Long) =>
          (plong ^ long) shouldEqual (plong.toLong ^ long)
        }
      }

      it("should offer 'min' and 'max' methods that are consistent with Long") {
        forAll { (plong1: PosLong, plong2: PosLong) =>
          plong1.max(plong2).toLong shouldEqual plong1.toLong.max(plong2.toLong)
          plong1.min(plong2).toLong shouldEqual plong1.toLong.min(plong2.toLong)
        }
      }

      it("should offer a 'toBinaryString' method that is consistent with Long") {
        forAll { (plong: PosLong) =>
          plong.toBinaryString shouldEqual plong.toLong.toBinaryString
        }
      }

      it("should offer a 'toHexString' method that is consistent with Long") {
        forAll { (plong: PosLong) =>
          plong.toHexString shouldEqual plong.toLong.toHexString
        }
      }

      it("should offer a 'toOctalString' method that is consistent with Long") {
        forAll { (plong: PosLong) =>
          plong.toOctalString shouldEqual plong.toLong.toOctalString
        }
      }

      // SKIP-SCALATESTJS,NATIVE-START
      it("should offer 'to' and 'until' method that is consistent with Long") {
        def rangeEqual[T](a: NumericRange[T], b: NumericRange[T]): Boolean =
          a.start == b.start && a.end == b.end && a.step == b.step

        forAll { (plong: PosLong, end: Long, step: Long) =>
          rangeEqual(plong.until(end), plong.toLong.until(end)) shouldBe true
          rangeEqual(plong.until(end, step), plong.toLong.until(end, step)) shouldBe true
          rangeEqual(plong.to(end), plong.toLong.to(end)) shouldBe true
          rangeEqual(plong.to(end, step), plong.toLong.to(end, step)) shouldBe true
        }
      }
      // SKIP-SCALATESTJS,NATIVE-END

      it("should offer widening methods for basic types that are consistent with Long") {
        forAll { (plong: PosLong) =>
          def widen(value: Long): Long = value
          widen(plong) shouldEqual widen(plong.toLong)
        }
        forAll { (plong: PosLong) =>
          def widen(value: Float): Float = value
          "widen(plong) shouldEqual widen(plong.toLong)" shouldNot compile
          succeed
        }
        forAll { (plong: PosLong) =>
          def widen(value: Double): Double = value
          "widen(plong) shouldEqual widen(plong.toLong)" shouldNot compile
          succeed
        }
        forAll { (plong: PosLong) =>
          def widen(value: PosFloat): PosFloat = value
          "widen(plong) shouldEqual widen(PosFloat.from(plong.toLong).get)" shouldNot compile
          succeed
        }
        forAll { (plong: PosLong) =>
          def widen(value: PosDouble): PosDouble = value
          "widen(plong) shouldEqual widen(PosDouble.from(plong.toLong).get)" shouldNot compile
          succeed
        }
        forAll { (plong: PosLong) =>
          def widen(value: PosZLong): PosZLong = value
          widen(plong) shouldEqual widen(PosZLong.from(plong.toLong).get)
        }
        forAll { (plong: PosLong) =>
          def widen(value: PosZFloat): PosZFloat = value
          "widen(plong) shouldEqual widen(PosZFloat.from(plong.toLong).get)" shouldNot compile
          succeed
        }
        forAll { (plong: PosLong) =>
          def widen(value: PosZDouble): PosZDouble = value
          "widen(plong) shouldEqual widen(PosZDouble.from(plong.toLong).get)" shouldNot compile
          succeed
        }
        forAll { (plong: PosLong) =>
          def widen(value: NonZeroLong): NonZeroLong = value
          widen(plong) shouldEqual widen(NonZeroLong.from(plong.toLong).get)
        }
        forAll { (plong: PosLong) =>
          def widen(value: NonZeroFloat): NonZeroFloat = value
          "(widen(plong) shouldEqual widen(NonZeroFloat.from(plong.toLong).get))" shouldNot compile
          succeed
        }
        forAll { (plong: PosLong) =>
          def widen(value: NonZeroDouble): NonZeroDouble = value
          "widen(plong) shouldEqual widen(NonZeroDouble.from(plong.toLong).get)" shouldNot compile
          succeed
        }
      }
    }
    it("should offer an ensuringValid method that takes an Long => Long, throwing AssertionError if the result is invalid") {
      PosLong(33L).ensuringValid(_ + 1L) shouldEqual PosLong(34L)
      an [AssertionError] should be thrownBy { PosLong.MaxValue.ensuringValid(_ + 1L) }
    }
  }
  describe("PosLong") {
    it("should be automatically widened to compatible AnyVal targets") {
      (PosLongs.PosLong(3L) : Long) shouldEqual 3L
      (PosLongs.PosLong(3L) : PosLongs.PosZLong) shouldEqual PosLongs.PosZLong(3L)
      (PosLongs.PosLong(3L) : NonZeroLongs.NonZeroLong) shouldEqual NonZeroLongs.NonZeroLong(3L)
      "(PosLongs.PosLong(3L) : Byte)" shouldNot compile
      "(PosLongs.PosLong(3L) : Short)" shouldNot compile
      "(PosLongs.PosLong(3L) : Char)" shouldNot compile
      "(PosLongs.PosLong(3L) : Int)" shouldNot compile
      "(PosLongs.PosLong(3L) : Float)" shouldNot compile
      "(PosLongs.PosLong(3L) : Double)" shouldNot compile
      "(PosLongs.PosLong(3L) : PosInts.PosInt)" shouldNot compile
      "(PosLongs.PosLong(3L) : PosInts.PosZInt)" shouldNot compile
      "(PosLongs.PosLong(3L) : NegInts.NegInt)" shouldNot compile
      "(PosLongs.PosLong(3L) : NegInts.NegZInt)" shouldNot compile
      "(PosLongs.PosLong(3L) : NonZeroInts.NonZeroInt)" shouldNot compile
      "(PosLongs.PosLong(3L) : Percentages.PercentageInt)" shouldNot compile
      "(PosLongs.PosLong(3L) : Numerics.NumericChar)" shouldNot compile
      "(PosLongs.PosLong(3L) : NegLongs.NegLong)" shouldNot compile
      "(PosLongs.PosLong(3L) : NegLongs.NegZLong)" shouldNot compile
      "(PosLongs.PosLong(3L) : PosFloats.PosFloat)" shouldNot compile
      "(PosLongs.PosLong(3L) : PosFloats.PosZFloat)" shouldNot compile
      "(PosLongs.PosLong(3L) : NegFloats.NegFloat)" shouldNot compile
      "(PosLongs.PosLong(3L) : NegFloats.NegZFloat)" shouldNot compile
      "(PosLongs.PosLong(3L) : NonZeroFloats.NonZeroFloat)" shouldNot compile
      "(PosLongs.PosLong(3L) : NonZeroFloats.NonZeroFiniteFloat)" shouldNot compile
      "(PosLongs.PosLong(3L) : PosFloats.PosFiniteFloat)" shouldNot compile
      "(PosLongs.PosLong(3L) : PosFloats.PosZFiniteFloat)" shouldNot compile
      "(PosLongs.PosLong(3L) : NegFloats.NegFiniteFloat)" shouldNot compile
      "(PosLongs.PosLong(3L) : NegFloats.NegZFiniteFloat)" shouldNot compile
      "(PosLongs.PosLong(3L) : Finites.FiniteFloat)" shouldNot compile
      "(PosLongs.PosLong(3L) : PosDoubles.PosDouble)" shouldNot compile
      "(PosLongs.PosLong(3L) : PosDoubles.PosZDouble)" shouldNot compile
      "(PosLongs.PosLong(3L) : NegDoubles.NegDouble)" shouldNot compile
      "(PosLongs.PosLong(3L) : NegDoubles.NegZDouble)" shouldNot compile
      "(PosLongs.PosLong(3L) : NonZeroDoubles.NonZeroDouble)" shouldNot compile
      "(PosLongs.PosLong(3L) : NonZeroDoubles.NonZeroFiniteDouble)" shouldNot compile
      "(PosLongs.PosLong(3L) : PosDoubles.PosFiniteDouble)" shouldNot compile
      "(PosLongs.PosLong(3L) : PosDoubles.PosZFiniteDouble)" shouldNot compile
      "(PosLongs.PosLong(3L) : NegDoubles.NegFiniteDouble)" shouldNot compile
      "(PosLongs.PosLong(3L) : NegDoubles.NegZFiniteDouble)" shouldNot compile
      "(PosLongs.PosLong(3L) : Finites.FiniteDouble)" shouldNot compile
    }
    it("when a compatible AnyVal is passed to a + method invoked on it should give the same AnyVal type back at compile time, and correct value at runtime") {
      PosLongs.PosLong(3L) + 3.toByte shouldEqual 6L
      PosLongs.PosLong(3L) + 3.toShort shouldEqual 6L
      PosLongs.PosLong(3L) + '3' shouldEqual 54L
      PosLongs.PosLong(3L) + 3 shouldEqual 6L
      PosLongs.PosLong(3L) + 3L shouldEqual 6L
      PosLongs.PosLong(3L) + 3.0f shouldEqual 6.0f
      PosLongs.PosLong(3L) + 3.0 shouldEqual 6.0
      PosLongs.PosLong(3L) + PosInts.PosInt(3) shouldEqual 6L
      PosLongs.PosLong(3L) + PosInts.PosZInt(3) shouldEqual 6L
      PosLongs.PosLong(3L) + NegInts.NegInt(-3) shouldEqual 0L
      PosLongs.PosLong(3L) + NegInts.NegZInt(-3) shouldEqual 0L
      PosLongs.PosLong(3L) + NonZeroInts.NonZeroInt(3) shouldEqual 6L
      PosLongs.PosLong(3L) + Percentages.PercentageInt(50) shouldEqual 53L
      PosLongs.PosLong(3L) + PosLongs.PosZLong(3L) shouldEqual 6L
      PosLongs.PosLong(3L) + NegLongs.NegLong(-3L) shouldEqual 0L
      PosLongs.PosLong(3L) + NegLongs.NegZLong(-3L) shouldEqual 0L
      PosLongs.PosLong(3L) + NonZeroLongs.NonZeroLong(3L) shouldEqual 6L
      PosLongs.PosLong(3L) + PosFloats.PosFloat(3.0f) shouldEqual 6.0f
      PosLongs.PosLong(3L) + PosFloats.PosZFloat(3.0f) shouldEqual 6.0f
      PosLongs.PosLong(3L) + NegFloats.NegFloat(-3.0f) shouldEqual 0.0f
      PosLongs.PosLong(3L) + NegFloats.NegZFloat(-3.0f) shouldEqual 0.0f
      PosLongs.PosLong(3L) + NonZeroFloats.NonZeroFloat(3.0f) shouldEqual 6.0f
      PosLongs.PosLong(3L) + NonZeroFloats.NonZeroFiniteFloat(3.0f) shouldEqual 6.0f
      PosLongs.PosLong(3L) + PosFloats.PosFiniteFloat(3.0f) shouldEqual 6.0f
      PosLongs.PosLong(3L) + PosFloats.PosZFiniteFloat(3.0f) shouldEqual 6.0f
      PosLongs.PosLong(3L) + NegFloats.NegFiniteFloat(-3.0f) shouldEqual 0.0f
      PosLongs.PosLong(3L) + NegFloats.NegZFiniteFloat(-3.0f) shouldEqual 0.0f
      PosLongs.PosLong(3L) + Finites.FiniteFloat(3.0f) shouldEqual 6.0f
      PosLongs.PosLong(3L) + PosDoubles.PosDouble(3.0) shouldEqual 6.0
      PosLongs.PosLong(3L) + NegDoubles.NegDouble(-3.0) shouldEqual 0.0
      PosLongs.PosLong(3L) + NegDoubles.NegZDouble(-3.0) shouldEqual 0.0
      PosLongs.PosLong(3L) + NonZeroDoubles.NonZeroDouble(3.0) shouldEqual 6.0
      PosLongs.PosLong(3L) + NonZeroDoubles.NonZeroFiniteDouble(3.0) shouldEqual 6.0
      PosLongs.PosLong(3L) + PosDoubles.PosFiniteDouble(3.0) shouldEqual 6.0
      PosLongs.PosLong(3L) + PosDoubles.PosZFiniteDouble(3.0) shouldEqual 6.0
      PosLongs.PosLong(3L) + NegDoubles.NegFiniteDouble(-3.0) shouldEqual 0.0
      PosLongs.PosLong(3L) + NegDoubles.NegZFiniteDouble(-3.0) shouldEqual 0.0
      PosLongs.PosLong(3L) + Finites.FiniteDouble(3.0) shouldEqual 6.0
    }
    it("when a compatible AnyVal is passed to a - method invoked on it should give the same AnyVal type back at compile time, and correct value at runtime") {
      PosLongs.PosLong(3L) - 3.toByte shouldEqual 0L
      PosLongs.PosLong(3L) - 3.toShort shouldEqual 0L
      PosLongs.PosLong(3L) - '3' shouldEqual -48L
      PosLongs.PosLong(3L) - 3 shouldEqual 0L
      PosLongs.PosLong(3L) - 3L shouldEqual 0L
      PosLongs.PosLong(3L) - 3.0f shouldEqual 0.0f
      PosLongs.PosLong(3L) - 3.0 shouldEqual 0.0
      PosLongs.PosLong(3L) - PosInts.PosInt(3) shouldEqual 0L
      PosLongs.PosLong(3L) - PosInts.PosZInt(3) shouldEqual 0L
      PosLongs.PosLong(3L) - NegInts.NegInt(-3) shouldEqual 6L
      PosLongs.PosLong(3L) - NegInts.NegZInt(-3) shouldEqual 6L
      PosLongs.PosLong(3L) - NonZeroInts.NonZeroInt(3) shouldEqual 0L
      PosLongs.PosLong(3L) - Percentages.PercentageInt(50) shouldEqual -47L
      PosLongs.PosLong(3L) - PosLongs.PosZLong(3L) shouldEqual 0L
      PosLongs.PosLong(3L) - NegLongs.NegLong(-3L) shouldEqual 6L
      PosLongs.PosLong(3L) - NegLongs.NegZLong(-3L) shouldEqual 6L
      PosLongs.PosLong(3L) - NonZeroLongs.NonZeroLong(3L) shouldEqual 0L
      PosLongs.PosLong(3L) - PosFloats.PosFloat(3.0f) shouldEqual 0.0f
      PosLongs.PosLong(3L) - PosFloats.PosZFloat(3.0f) shouldEqual 0.0f
      PosLongs.PosLong(3L) - NegFloats.NegFloat(-3.0f) shouldEqual 6.0f
      PosLongs.PosLong(3L) - NegFloats.NegZFloat(-3.0f) shouldEqual 6.0f
      PosLongs.PosLong(3L) - NonZeroFloats.NonZeroFloat(3.0f) shouldEqual 0.0f
      PosLongs.PosLong(3L) - NonZeroFloats.NonZeroFiniteFloat(3.0f) shouldEqual 0.0f
      PosLongs.PosLong(3L) - PosFloats.PosFiniteFloat(3.0f) shouldEqual 0.0f
      PosLongs.PosLong(3L) - PosFloats.PosZFiniteFloat(3.0f) shouldEqual 0.0f
      PosLongs.PosLong(3L) - NegFloats.NegFiniteFloat(-3.0f) shouldEqual 6.0f
      PosLongs.PosLong(3L) - NegFloats.NegZFiniteFloat(-3.0f) shouldEqual 6.0f
      PosLongs.PosLong(3L) - Finites.FiniteFloat(3.0f) shouldEqual 0.0f
      PosLongs.PosLong(3L) - PosDoubles.PosDouble(3.0) shouldEqual 0.0
      PosLongs.PosLong(3L) - NegDoubles.NegDouble(-3.0) shouldEqual 6.0
      PosLongs.PosLong(3L) - NegDoubles.NegZDouble(-3.0) shouldEqual 6.0
      PosLongs.PosLong(3L) - NonZeroDoubles.NonZeroDouble(3.0) shouldEqual 0.0
      PosLongs.PosLong(3L) - NonZeroDoubles.NonZeroFiniteDouble(3.0) shouldEqual 0.0
      PosLongs.PosLong(3L) - PosDoubles.PosFiniteDouble(3.0) shouldEqual 0.0
      PosLongs.PosLong(3L) - PosDoubles.PosZFiniteDouble(3.0) shouldEqual 0.0
      PosLongs.PosLong(3L) - NegDoubles.NegFiniteDouble(-3.0) shouldEqual 6.0
      PosLongs.PosLong(3L) - NegDoubles.NegZFiniteDouble(-3.0) shouldEqual 6.0
      PosLongs.PosLong(3L) - Finites.FiniteDouble(3.0) shouldEqual 0.0
    }
    it("when a compatible AnyVal is passed to a * method invoked on it should give the same AnyVal type back at compile time, and correct value at runtime") {
      PosLongs.PosLong(3L) * 3.toByte shouldEqual 9L
      PosLongs.PosLong(3L) * 3.toShort shouldEqual 9L
      PosLongs.PosLong(3L) * '3' shouldEqual 153L
      PosLongs.PosLong(3L) * 3 shouldEqual 9L
      PosLongs.PosLong(3L) * 3L shouldEqual 9L
      PosLongs.PosLong(3L) * 3.0f shouldEqual 9.0f
      PosLongs.PosLong(3L) * 3.0 shouldEqual 9.0
      PosLongs.PosLong(3L) * PosInts.PosInt(3) shouldEqual 9L
      PosLongs.PosLong(3L) * PosInts.PosZInt(3) shouldEqual 9L
      PosLongs.PosLong(3L) * NegInts.NegInt(-3) shouldEqual -9L
      PosLongs.PosLong(3L) * NegInts.NegZInt(-3) shouldEqual -9L
      PosLongs.PosLong(3L) * NonZeroInts.NonZeroInt(3) shouldEqual 9L
      PosLongs.PosLong(3L) * Percentages.PercentageInt(50) shouldEqual 150L
      PosLongs.PosLong(3L) * PosLongs.PosZLong(3L) shouldEqual 9L
      PosLongs.PosLong(3L) * NegLongs.NegLong(-3L) shouldEqual -9L
      PosLongs.PosLong(3L) * NegLongs.NegZLong(-3L) shouldEqual -9L
      PosLongs.PosLong(3L) * NonZeroLongs.NonZeroLong(3L) shouldEqual 9L
      PosLongs.PosLong(3L) * PosFloats.PosFloat(3.0f) shouldEqual 9.0f
      PosLongs.PosLong(3L) * PosFloats.PosZFloat(3.0f) shouldEqual 9.0f
      PosLongs.PosLong(3L) * NegFloats.NegFloat(-3.0f) shouldEqual -9.0f
      PosLongs.PosLong(3L) * NegFloats.NegZFloat(-3.0f) shouldEqual -9.0f
      PosLongs.PosLong(3L) * NonZeroFloats.NonZeroFloat(3.0f) shouldEqual 9.0f
      PosLongs.PosLong(3L) * NonZeroFloats.NonZeroFiniteFloat(3.0f) shouldEqual 9.0f
      PosLongs.PosLong(3L) * PosFloats.PosFiniteFloat(3.0f) shouldEqual 9.0f
      PosLongs.PosLong(3L) * PosFloats.PosZFiniteFloat(3.0f) shouldEqual 9.0f
      PosLongs.PosLong(3L) * NegFloats.NegFiniteFloat(-3.0f) shouldEqual -9.0f
      PosLongs.PosLong(3L) * NegFloats.NegZFiniteFloat(-3.0f) shouldEqual -9.0f
      PosLongs.PosLong(3L) * Finites.FiniteFloat(3.0f) shouldEqual 9.0f
      PosLongs.PosLong(3L) * PosDoubles.PosDouble(3.0) shouldEqual 9.0
      PosLongs.PosLong(3L) * NegDoubles.NegDouble(-3.0) shouldEqual -9.0
      PosLongs.PosLong(3L) * NegDoubles.NegZDouble(-3.0) shouldEqual -9.0
      PosLongs.PosLong(3L) * NonZeroDoubles.NonZeroDouble(3.0) shouldEqual 9.0
      PosLongs.PosLong(3L) * NonZeroDoubles.NonZeroFiniteDouble(3.0) shouldEqual 9.0
      PosLongs.PosLong(3L) * PosDoubles.PosFiniteDouble(3.0) shouldEqual 9.0
      PosLongs.PosLong(3L) * PosDoubles.PosZFiniteDouble(3.0) shouldEqual 9.0
      PosLongs.PosLong(3L) * NegDoubles.NegFiniteDouble(-3.0) shouldEqual -9.0
      PosLongs.PosLong(3L) * NegDoubles.NegZFiniteDouble(-3.0) shouldEqual -9.0
      PosLongs.PosLong(3L) * Finites.FiniteDouble(3.0) shouldEqual 9.0
    }
    it("when a compatible AnyVal is passed to a / method invoked on it should give the same AnyVal type back at compile time, and correct value at runtime") {
      PosLongs.PosLong(3L) / 3.toByte shouldEqual 1L
      PosLongs.PosLong(3L) / 3.toShort shouldEqual 1L
      PosLongs.PosLong(3L) / '3' shouldEqual 0L
      PosLongs.PosLong(3L) / 3 shouldEqual 1L
      PosLongs.PosLong(3L) / 3L shouldEqual 1L
      PosLongs.PosLong(3L) / 3.0f shouldEqual 1.0f
      PosLongs.PosLong(3L) / 3.0 shouldEqual 1.0
      PosLongs.PosLong(3L) / PosInts.PosInt(3) shouldEqual 1L
      PosLongs.PosLong(3L) / PosInts.PosZInt(3) shouldEqual 1L
      PosLongs.PosLong(3L) / NegInts.NegInt(-3) shouldEqual -1L
      PosLongs.PosLong(3L) / NegInts.NegZInt(-3) shouldEqual -1L
      PosLongs.PosLong(3L) / NonZeroInts.NonZeroInt(3) shouldEqual 1L
      PosLongs.PosLong(3L) / Percentages.PercentageInt(50) shouldEqual 0L
      PosLongs.PosLong(3L) / PosLongs.PosZLong(3L) shouldEqual 1L
      PosLongs.PosLong(3L) / NegLongs.NegLong(-3L) shouldEqual -1L
      PosLongs.PosLong(3L) / NegLongs.NegZLong(-3L) shouldEqual -1L
      PosLongs.PosLong(3L) / NonZeroLongs.NonZeroLong(3L) shouldEqual 1L
      PosLongs.PosLong(3L) / PosFloats.PosFloat(3.0f) shouldEqual 1.0f
      PosLongs.PosLong(3L) / PosFloats.PosZFloat(3.0f) shouldEqual 1.0f
      PosLongs.PosLong(3L) / NegFloats.NegFloat(-3.0f) shouldEqual -1.0f
      PosLongs.PosLong(3L) / NegFloats.NegZFloat(-3.0f) shouldEqual -1.0f
      PosLongs.PosLong(3L) / NonZeroFloats.NonZeroFloat(3.0f) shouldEqual 1.0f
      PosLongs.PosLong(3L) / NonZeroFloats.NonZeroFiniteFloat(3.0f) shouldEqual 1.0f
      PosLongs.PosLong(3L) / PosFloats.PosFiniteFloat(3.0f) shouldEqual 1.0f
      PosLongs.PosLong(3L) / PosFloats.PosZFiniteFloat(3.0f) shouldEqual 1.0f
      PosLongs.PosLong(3L) / NegFloats.NegFiniteFloat(-3.0f) shouldEqual -1.0f
      PosLongs.PosLong(3L) / NegFloats.NegZFiniteFloat(-3.0f) shouldEqual -1.0f
      PosLongs.PosLong(3L) / Finites.FiniteFloat(3.0f) shouldEqual 1.0f
      PosLongs.PosLong(3L) / PosDoubles.PosDouble(3.0) shouldEqual 1.0
      PosLongs.PosLong(3L) / NegDoubles.NegDouble(-3.0) shouldEqual -1.0
      PosLongs.PosLong(3L) / NegDoubles.NegZDouble(-3.0) shouldEqual -1.0
      PosLongs.PosLong(3L) / NonZeroDoubles.NonZeroDouble(3.0) shouldEqual 1.0
      PosLongs.PosLong(3L) / NonZeroDoubles.NonZeroFiniteDouble(3.0) shouldEqual 1.0
      PosLongs.PosLong(3L) / PosDoubles.PosFiniteDouble(3.0) shouldEqual 1.0
      PosLongs.PosLong(3L) / PosDoubles.PosZFiniteDouble(3.0) shouldEqual 1.0
      PosLongs.PosLong(3L) / NegDoubles.NegFiniteDouble(-3.0) shouldEqual -1.0
      PosLongs.PosLong(3L) / NegDoubles.NegZFiniteDouble(-3.0) shouldEqual -1.0
      PosLongs.PosLong(3L) / Finites.FiniteDouble(3.0) shouldEqual 1.0
    }
    it("when a compatible AnyVal is passed to a % method invoked on it should give the same AnyVal type back at compile time, and correct value at runtime") {
      PosLongs.PosLong(3L) % 3.toByte shouldEqual 0L
      PosLongs.PosLong(3L) % 3.toShort shouldEqual 0L
      PosLongs.PosLong(3L) % '3' shouldEqual 3L
      PosLongs.PosLong(3L) % 3 shouldEqual 0L
      PosLongs.PosLong(3L) % 3L shouldEqual 0L
      PosLongs.PosLong(3L) % 3.0f shouldEqual 0.0f
      PosLongs.PosLong(3L) % 3.0 shouldEqual 0.0
      PosLongs.PosLong(3L) % PosInts.PosInt(3) shouldEqual 0L
      PosLongs.PosLong(3L) % PosInts.PosZInt(3) shouldEqual 0L
      PosLongs.PosLong(3L) % NegInts.NegInt(-3) shouldEqual 0L
      PosLongs.PosLong(3L) % NegInts.NegZInt(-3) shouldEqual 0L
      PosLongs.PosLong(3L) % NonZeroInts.NonZeroInt(3) shouldEqual 0L
      PosLongs.PosLong(3L) % Percentages.PercentageInt(50) shouldEqual 3L
      PosLongs.PosLong(3L) % PosLongs.PosZLong(3L) shouldEqual 0L
      PosLongs.PosLong(3L) % NegLongs.NegLong(-3L) shouldEqual 0L
      PosLongs.PosLong(3L) % NegLongs.NegZLong(-3L) shouldEqual 0L
      PosLongs.PosLong(3L) % NonZeroLongs.NonZeroLong(3L) shouldEqual 0L
      PosLongs.PosLong(3L) % PosFloats.PosFloat(3.0f) shouldEqual 0.0f
      PosLongs.PosLong(3L) % PosFloats.PosZFloat(3.0f) shouldEqual 0.0f
      PosLongs.PosLong(3L) % NegFloats.NegFloat(-3.0f) shouldEqual 0.0f
      PosLongs.PosLong(3L) % NegFloats.NegZFloat(-3.0f) shouldEqual 0.0f
      PosLongs.PosLong(3L) % NonZeroFloats.NonZeroFloat(3.0f) shouldEqual 0.0f
      PosLongs.PosLong(3L) % NonZeroFloats.NonZeroFiniteFloat(3.0f) shouldEqual 0.0f
      PosLongs.PosLong(3L) % PosFloats.PosFiniteFloat(3.0f) shouldEqual 0.0f
      PosLongs.PosLong(3L) % PosFloats.PosZFiniteFloat(3.0f) shouldEqual 0.0f
      PosLongs.PosLong(3L) % NegFloats.NegFiniteFloat(-3.0f) shouldEqual 0.0f
      PosLongs.PosLong(3L) % NegFloats.NegZFiniteFloat(-3.0f) shouldEqual 0.0f
      PosLongs.PosLong(3L) % Finites.FiniteFloat(3.0f) shouldEqual 0.0f
      PosLongs.PosLong(3L) % PosDoubles.PosDouble(3.0) shouldEqual 0.0
      PosLongs.PosLong(3L) % NegDoubles.NegDouble(-3.0) shouldEqual 0.0
      PosLongs.PosLong(3L) % NegDoubles.NegZDouble(-3.0) shouldEqual 0.0
      PosLongs.PosLong(3L) % NonZeroDoubles.NonZeroDouble(3.0) shouldEqual 0.0
      PosLongs.PosLong(3L) % NonZeroDoubles.NonZeroFiniteDouble(3.0) shouldEqual 0.0
      PosLongs.PosLong(3L) % PosDoubles.PosFiniteDouble(3.0) shouldEqual 0.0
      PosLongs.PosLong(3L) % PosDoubles.PosZFiniteDouble(3.0) shouldEqual 0.0
      PosLongs.PosLong(3L) % NegDoubles.NegFiniteDouble(-3.0) shouldEqual 0.0
      PosLongs.PosLong(3L) % NegDoubles.NegZFiniteDouble(-3.0) shouldEqual 0.0
      PosLongs.PosLong(3L) % Finites.FiniteDouble(3.0) shouldEqual 0.0
    }
    it("should offer '<' comparison that is consistent with Long") {
      (PosLongs.PosLong(3L) < 3.toByte) shouldEqual false
      (PosLongs.PosLong(3L) < 3.toShort) shouldEqual false
      (PosLongs.PosLong(3L) < '3') shouldEqual true
      (PosLongs.PosLong(3L) < 3) shouldEqual false
      (PosLongs.PosLong(3L) < 3L) shouldEqual false
      (PosLongs.PosLong(3L) < 3.0f) shouldEqual false
      (PosLongs.PosLong(3L) < 3.0) shouldEqual false
      (PosLongs.PosLong(3L) < PosInts.PosInt(3)) shouldEqual false
      (PosLongs.PosLong(3L) < PosInts.PosZInt(3)) shouldEqual false
      (PosLongs.PosLong(3L) < NegInts.NegInt(-3)) shouldEqual false
      (PosLongs.PosLong(3L) < NegInts.NegZInt(-3)) shouldEqual false
      (PosLongs.PosLong(3L) < NonZeroInts.NonZeroInt(3)) shouldEqual false
      (PosLongs.PosLong(3L) < Percentages.PercentageInt(50)) shouldEqual true
      (PosLongs.PosLong(3L) < PosLongs.PosZLong(3L)) shouldEqual false
      (PosLongs.PosLong(3L) < NegLongs.NegLong(-3L)) shouldEqual false
      (PosLongs.PosLong(3L) < NegLongs.NegZLong(-3L)) shouldEqual false
      (PosLongs.PosLong(3L) < NonZeroLongs.NonZeroLong(3L)) shouldEqual false
      (PosLongs.PosLong(3L) < PosFloats.PosFloat(3.0f)) shouldEqual false
      (PosLongs.PosLong(3L) < PosFloats.PosZFloat(3.0f)) shouldEqual false
      (PosLongs.PosLong(3L) < NegFloats.NegFloat(-3.0f)) shouldEqual false
      (PosLongs.PosLong(3L) < NegFloats.NegZFloat(-3.0f)) shouldEqual false
      (PosLongs.PosLong(3L) < NonZeroFloats.NonZeroFloat(3.0f)) shouldEqual false
      (PosLongs.PosLong(3L) < NonZeroFloats.NonZeroFiniteFloat(3.0f)) shouldEqual false
      (PosLongs.PosLong(3L) < PosFloats.PosFiniteFloat(3.0f)) shouldEqual false
      (PosLongs.PosLong(3L) < PosFloats.PosZFiniteFloat(3.0f)) shouldEqual false
      (PosLongs.PosLong(3L) < NegFloats.NegFiniteFloat(-3.0f)) shouldEqual false
      (PosLongs.PosLong(3L) < NegFloats.NegZFiniteFloat(-3.0f)) shouldEqual false
      (PosLongs.PosLong(3L) < Finites.FiniteFloat(3.0f)) shouldEqual false
      (PosLongs.PosLong(3L) < PosDoubles.PosDouble(3.0)) shouldEqual false
      (PosLongs.PosLong(3L) < NegDoubles.NegDouble(-3.0)) shouldEqual false
      (PosLongs.PosLong(3L) < NegDoubles.NegZDouble(-3.0)) shouldEqual false
      (PosLongs.PosLong(3L) < NonZeroDoubles.NonZeroDouble(3.0)) shouldEqual false
      (PosLongs.PosLong(3L) < NonZeroDoubles.NonZeroFiniteDouble(3.0)) shouldEqual false
      (PosLongs.PosLong(3L) < PosDoubles.PosFiniteDouble(3.0)) shouldEqual false
      (PosLongs.PosLong(3L) < PosDoubles.PosZFiniteDouble(3.0)) shouldEqual false
      (PosLongs.PosLong(3L) < NegDoubles.NegFiniteDouble(-3.0)) shouldEqual false
      (PosLongs.PosLong(3L) < NegDoubles.NegZFiniteDouble(-3.0)) shouldEqual false
      (PosLongs.PosLong(3L) < Finites.FiniteDouble(3.0)) shouldEqual false
    }
    it("should offer '<=' comparison that is consistent with Long") {
      (PosLongs.PosLong(3L) <= 3.toByte) shouldEqual true
      (PosLongs.PosLong(3L) <= 3.toShort) shouldEqual true
      (PosLongs.PosLong(3L) <= '3') shouldEqual true
      (PosLongs.PosLong(3L) <= 3) shouldEqual true
      (PosLongs.PosLong(3L) <= 3L) shouldEqual true
      (PosLongs.PosLong(3L) <= 3.0f) shouldEqual true
      (PosLongs.PosLong(3L) <= 3.0) shouldEqual true
      (PosLongs.PosLong(3L) <= PosInts.PosInt(3)) shouldEqual true
      (PosLongs.PosLong(3L) <= PosInts.PosZInt(3)) shouldEqual true
      (PosLongs.PosLong(3L) <= NegInts.NegInt(-3)) shouldEqual false
      (PosLongs.PosLong(3L) <= NegInts.NegZInt(-3)) shouldEqual false
      (PosLongs.PosLong(3L) <= NonZeroInts.NonZeroInt(3)) shouldEqual true
      (PosLongs.PosLong(3L) <= Percentages.PercentageInt(50)) shouldEqual true
      (PosLongs.PosLong(3L) <= PosLongs.PosZLong(3L)) shouldEqual true
      (PosLongs.PosLong(3L) <= NegLongs.NegLong(-3L)) shouldEqual false
      (PosLongs.PosLong(3L) <= NegLongs.NegZLong(-3L)) shouldEqual false
      (PosLongs.PosLong(3L) <= NonZeroLongs.NonZeroLong(3L)) shouldEqual true
      (PosLongs.PosLong(3L) <= PosFloats.PosFloat(3.0f)) shouldEqual true
      (PosLongs.PosLong(3L) <= PosFloats.PosZFloat(3.0f)) shouldEqual true
      (PosLongs.PosLong(3L) <= NegFloats.NegFloat(-3.0f)) shouldEqual false
      (PosLongs.PosLong(3L) <= NegFloats.NegZFloat(-3.0f)) shouldEqual false
      (PosLongs.PosLong(3L) <= NonZeroFloats.NonZeroFloat(3.0f)) shouldEqual true
      (PosLongs.PosLong(3L) <= NonZeroFloats.NonZeroFiniteFloat(3.0f)) shouldEqual true
      (PosLongs.PosLong(3L) <= PosFloats.PosFiniteFloat(3.0f)) shouldEqual true
      (PosLongs.PosLong(3L) <= PosFloats.PosZFiniteFloat(3.0f)) shouldEqual true
      (PosLongs.PosLong(3L) <= NegFloats.NegFiniteFloat(-3.0f)) shouldEqual false
      (PosLongs.PosLong(3L) <= NegFloats.NegZFiniteFloat(-3.0f)) shouldEqual false
      (PosLongs.PosLong(3L) <= Finites.FiniteFloat(3.0f)) shouldEqual true
      (PosLongs.PosLong(3L) <= PosDoubles.PosDouble(3.0)) shouldEqual true
      (PosLongs.PosLong(3L) <= NegDoubles.NegDouble(-3.0)) shouldEqual false
      (PosLongs.PosLong(3L) <= NegDoubles.NegZDouble(-3.0)) shouldEqual false
      (PosLongs.PosLong(3L) <= NonZeroDoubles.NonZeroDouble(3.0)) shouldEqual true
      (PosLongs.PosLong(3L) <= NonZeroDoubles.NonZeroFiniteDouble(3.0)) shouldEqual true
      (PosLongs.PosLong(3L) <= PosDoubles.PosFiniteDouble(3.0)) shouldEqual true
      (PosLongs.PosLong(3L) <= PosDoubles.PosZFiniteDouble(3.0)) shouldEqual true
      (PosLongs.PosLong(3L) <= NegDoubles.NegFiniteDouble(-3.0)) shouldEqual false
      (PosLongs.PosLong(3L) <= NegDoubles.NegZFiniteDouble(-3.0)) shouldEqual false
      (PosLongs.PosLong(3L) <= Finites.FiniteDouble(3.0)) shouldEqual true
    }
    it("should offer '>' comparison that is consistent with Long") {
      (PosLongs.PosLong(3L) > 3.toByte) shouldEqual false
      (PosLongs.PosLong(3L) > 3.toShort) shouldEqual false
      (PosLongs.PosLong(3L) > '3') shouldEqual false
      (PosLongs.PosLong(3L) > 3) shouldEqual false
      (PosLongs.PosLong(3L) > 3L) shouldEqual false
      (PosLongs.PosLong(3L) > 3.0f) shouldEqual false
      (PosLongs.PosLong(3L) > 3.0) shouldEqual false
      (PosLongs.PosLong(3L) > PosInts.PosInt(3)) shouldEqual false
      (PosLongs.PosLong(3L) > PosInts.PosZInt(3)) shouldEqual false
      (PosLongs.PosLong(3L) > NegInts.NegInt(-3)) shouldEqual true
      (PosLongs.PosLong(3L) > NegInts.NegZInt(-3)) shouldEqual true
      (PosLongs.PosLong(3L) > NonZeroInts.NonZeroInt(3)) shouldEqual false
      (PosLongs.PosLong(3L) > Percentages.PercentageInt(50)) shouldEqual false
      (PosLongs.PosLong(3L) > PosLongs.PosZLong(3L)) shouldEqual false
      (PosLongs.PosLong(3L) > NegLongs.NegLong(-3L)) shouldEqual true
      (PosLongs.PosLong(3L) > NegLongs.NegZLong(-3L)) shouldEqual true
      (PosLongs.PosLong(3L) > NonZeroLongs.NonZeroLong(3L)) shouldEqual false
      (PosLongs.PosLong(3L) > PosFloats.PosFloat(3.0f)) shouldEqual false
      (PosLongs.PosLong(3L) > PosFloats.PosZFloat(3.0f)) shouldEqual false
      (PosLongs.PosLong(3L) > NegFloats.NegFloat(-3.0f)) shouldEqual true
      (PosLongs.PosLong(3L) > NegFloats.NegZFloat(-3.0f)) shouldEqual true
      (PosLongs.PosLong(3L) > NonZeroFloats.NonZeroFloat(3.0f)) shouldEqual false
      (PosLongs.PosLong(3L) > NonZeroFloats.NonZeroFiniteFloat(3.0f)) shouldEqual false
      (PosLongs.PosLong(3L) > PosFloats.PosFiniteFloat(3.0f)) shouldEqual false
      (PosLongs.PosLong(3L) > PosFloats.PosZFiniteFloat(3.0f)) shouldEqual false
      (PosLongs.PosLong(3L) > NegFloats.NegFiniteFloat(-3.0f)) shouldEqual true
      (PosLongs.PosLong(3L) > NegFloats.NegZFiniteFloat(-3.0f)) shouldEqual true
      (PosLongs.PosLong(3L) > Finites.FiniteFloat(3.0f)) shouldEqual false
      (PosLongs.PosLong(3L) > PosDoubles.PosDouble(3.0)) shouldEqual false
      (PosLongs.PosLong(3L) > NegDoubles.NegDouble(-3.0)) shouldEqual true
      (PosLongs.PosLong(3L) > NegDoubles.NegZDouble(-3.0)) shouldEqual true
      (PosLongs.PosLong(3L) > NonZeroDoubles.NonZeroDouble(3.0)) shouldEqual false
      (PosLongs.PosLong(3L) > NonZeroDoubles.NonZeroFiniteDouble(3.0)) shouldEqual false
      (PosLongs.PosLong(3L) > PosDoubles.PosFiniteDouble(3.0)) shouldEqual false
      (PosLongs.PosLong(3L) > PosDoubles.PosZFiniteDouble(3.0)) shouldEqual false
      (PosLongs.PosLong(3L) > NegDoubles.NegFiniteDouble(-3.0)) shouldEqual true
      (PosLongs.PosLong(3L) > NegDoubles.NegZFiniteDouble(-3.0)) shouldEqual true
      (PosLongs.PosLong(3L) > Finites.FiniteDouble(3.0)) shouldEqual false
    }
    it("should offer '>=' comparison that is consistent with Long") {
      (PosLongs.PosLong(3L) >= 3.toByte) shouldEqual true
      (PosLongs.PosLong(3L) >= 3.toShort) shouldEqual true
      (PosLongs.PosLong(3L) >= '3') shouldEqual false
      (PosLongs.PosLong(3L) >= 3) shouldEqual true
      (PosLongs.PosLong(3L) >= 3L) shouldEqual true
      (PosLongs.PosLong(3L) >= 3.0f) shouldEqual true
      (PosLongs.PosLong(3L) >= 3.0) shouldEqual true
      (PosLongs.PosLong(3L) >= PosInts.PosInt(3)) shouldEqual true
      (PosLongs.PosLong(3L) >= PosInts.PosZInt(3)) shouldEqual true
      (PosLongs.PosLong(3L) >= NegInts.NegInt(-3)) shouldEqual true
      (PosLongs.PosLong(3L) >= NegInts.NegZInt(-3)) shouldEqual true
      (PosLongs.PosLong(3L) >= NonZeroInts.NonZeroInt(3)) shouldEqual true
      (PosLongs.PosLong(3L) >= Percentages.PercentageInt(50)) shouldEqual false
      (PosLongs.PosLong(3L) >= PosLongs.PosZLong(3L)) shouldEqual true
      (PosLongs.PosLong(3L) >= NegLongs.NegLong(-3L)) shouldEqual true
      (PosLongs.PosLong(3L) >= NegLongs.NegZLong(-3L)) shouldEqual true
      (PosLongs.PosLong(3L) >= NonZeroLongs.NonZeroLong(3L)) shouldEqual true
      (PosLongs.PosLong(3L) >= PosFloats.PosFloat(3.0f)) shouldEqual true
      (PosLongs.PosLong(3L) >= PosFloats.PosZFloat(3.0f)) shouldEqual true
      (PosLongs.PosLong(3L) >= NegFloats.NegFloat(-3.0f)) shouldEqual true
      (PosLongs.PosLong(3L) >= NegFloats.NegZFloat(-3.0f)) shouldEqual true
      (PosLongs.PosLong(3L) >= NonZeroFloats.NonZeroFloat(3.0f)) shouldEqual true
      (PosLongs.PosLong(3L) >= NonZeroFloats.NonZeroFiniteFloat(3.0f)) shouldEqual true
      (PosLongs.PosLong(3L) >= PosFloats.PosFiniteFloat(3.0f)) shouldEqual true
      (PosLongs.PosLong(3L) >= PosFloats.PosZFiniteFloat(3.0f)) shouldEqual true
      (PosLongs.PosLong(3L) >= NegFloats.NegFiniteFloat(-3.0f)) shouldEqual true
      (PosLongs.PosLong(3L) >= NegFloats.NegZFiniteFloat(-3.0f)) shouldEqual true
      (PosLongs.PosLong(3L) >= Finites.FiniteFloat(3.0f)) shouldEqual true
      (PosLongs.PosLong(3L) >= PosDoubles.PosDouble(3.0)) shouldEqual true
      (PosLongs.PosLong(3L) >= NegDoubles.NegDouble(-3.0)) shouldEqual true
      (PosLongs.PosLong(3L) >= NegDoubles.NegZDouble(-3.0)) shouldEqual true
      (PosLongs.PosLong(3L) >= NonZeroDoubles.NonZeroDouble(3.0)) shouldEqual true
      (PosLongs.PosLong(3L) >= NonZeroDoubles.NonZeroFiniteDouble(3.0)) shouldEqual true
      (PosLongs.PosLong(3L) >= PosDoubles.PosFiniteDouble(3.0)) shouldEqual true
      (PosLongs.PosLong(3L) >= PosDoubles.PosZFiniteDouble(3.0)) shouldEqual true
      (PosLongs.PosLong(3L) >= NegDoubles.NegFiniteDouble(-3.0)) shouldEqual true
      (PosLongs.PosLong(3L) >= NegDoubles.NegZFiniteDouble(-3.0)) shouldEqual true
      (PosLongs.PosLong(3L) >= Finites.FiniteDouble(3.0)) shouldEqual true
    }
    it("should offer 'min' and 'max' methods that are consistent with Long") {
      PosLongs.PosLong(3L).max(PosLongs.PosLong(3L)) shouldEqual PosLongs.PosLong(3L)
      PosLongs.PosLong(3L).min(PosLongs.PosLong(3L)) shouldEqual PosLongs.PosLong(3L)
    }
    it("should return the same type from its unary_+ method") {
      +PosLongs.PosLong(3L) shouldEqual PosLongs.PosLong(3L)
    }
    it("should offer a unary - method that is consistent with Long") {
      (-PosLongs.PosLong(3L)) shouldEqual -3L
    }
    it("should offer widening methods for basic type Long that are consistent with Long") {
      def widen(value: Long): Long = value
      widen(PosLongs.PosLong(3L)) shouldEqual 3L
    }
    it("should offer widening methods for basic type PosLongs.PosZLong that are consistent with Long") {
      def widen(value: PosLongs.PosZLong): PosLongs.PosZLong = value
      widen(PosLongs.PosLong(3L)) shouldEqual PosLongs.PosZLong(3L)
    }
    it("should offer widening methods for basic type NonZeroLongs.NonZeroLong that are consistent with Long") {
      def widen(value: NonZeroLongs.NonZeroLong): NonZeroLongs.NonZeroLong = value
      widen(PosLongs.PosLong(3L)) shouldEqual NonZeroLongs.NonZeroLong(3L)
    }
    it("should have a toString consistent with Long") {
      (PosLongs.PosLong(3L) : Long).toString shouldBe 3L.toString
    }
  }
}
