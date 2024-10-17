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

import org.scalactic.Equality
import org.scalactic.{Pass, Fail}
import org.scalactic.{Good, Bad}
import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import scala.collection.mutable.WrappedArray
import OptionValues._

import scala.util.{Failure, Success, Try}

//import org.scalactic.StrictCheckedEquality

trait PosZIntSpecSupport {

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

class PosZIntSpec extends funspec.AnyFunSpec with matchers.should.Matchers with GeneratorDrivenPropertyChecks with PosZIntSpecSupport {

  describe("A PosZInt") {
    describe("should offer a from factory method that") {
      it("returns Some[PosZInt] if the passed Int is greater than or equal to 0") {
        PosZInt.from(0).value.value shouldBe 0
        PosZInt.from(50).value.value shouldBe 50
        PosZInt.from(100).value.value shouldBe 100
      }
      it("returns None if the passed Int is NOT greater than or equal to 0") {
        PosZInt.from(-1) shouldBe None
        PosZInt.from(-99) shouldBe None
      }
    } 
    describe("should offer an ensuringValid factory method that") {
      it("returns PosZInt if the passed Int is greater than or equal to 0") {
        PosZInt.ensuringValid(0).value shouldBe 0
        PosZInt.ensuringValid(50).value shouldBe 50
        PosZInt.ensuringValid(100).value shouldBe 100
      }
      it("throws AssertionError if the passed Int is NOT greater than or equal to 0") {
        an [AssertionError] should be thrownBy PosZInt.ensuringValid(-1)
        an [AssertionError] should be thrownBy PosZInt.ensuringValid(-99)
      }
    }
    describe("should offer a tryingValid factory method that") {
      import TryValues._
      it("returns a PosZInt wrapped in a Success if the passed Int is greater than or equal 0") {
        PosZInt.tryingValid(0).success.value.value shouldBe 0
        PosZInt.tryingValid(50).success.value.value shouldBe 50
        PosZInt.tryingValid(100).success.value.value shouldBe 100
      }

      it("returns an AssertionError wrapped in a Failure if the passed Int is lesser than 0") {
        PosZInt.tryingValid(-1).failure.exception shouldBe an [AssertionError]
        PosZInt.tryingValid(-99).failure.exception shouldBe an [AssertionError]
      }
    }
    describe("should offer a passOrElse factory method that") {
      it("returns a Pass if the given Int is greater than or equal 0") {
        PosZInt.passOrElse(0)(i => i) shouldBe Pass
        PosZInt.passOrElse(50)(i => i) shouldBe Pass
        PosZInt.passOrElse(100)(i => i) shouldBe Pass
      }
      it("returns an error value produced by passing the given Int to the given function if the passed Int is lesser than 0, wrapped in a Fail") {
        PosZInt.passOrElse(-1)(i => i) shouldBe Fail(-1)
        PosZInt.passOrElse(-99)(i => i.toLong + 3L) shouldBe Fail(-96L)
      }
    }
    describe("should offer a goodOrElse factory method that") {
      it("returns a PosZInt wrapped in a Good if the given Int is greater than or equal 0") {
        PosZInt.goodOrElse(0)(i => i) shouldBe Good(PosZInt(0))
        PosZInt.goodOrElse(50)(i => i) shouldBe Good(PosZInt(50))
        PosZInt.goodOrElse(100)(i => i) shouldBe Good(PosZInt(100))
      }
      it("returns an error value produced by passing the given Int to the given function if the passed Int is lesser than 0, wrapped in a Bad") {
        PosZInt.goodOrElse(-1)(i => i) shouldBe Bad(-1)
        PosZInt.goodOrElse(-99)(i => i.toLong + 3L) shouldBe Bad(-96L)
      }
    }
    describe("should offer a rightOrElse factory method that") {
      it("returns a PosZInt wrapped in a Right if the given Int is greater than or equal 0") {
        PosZInt.rightOrElse(0)(i => i) shouldBe Right(PosZInt(0))
        PosZInt.rightOrElse(50)(i => i) shouldBe Right(PosZInt(50))
        PosZInt.rightOrElse(100)(i => i) shouldBe Right(PosZInt(100))
      }
      it("returns an error value produced by passing the given Int to the given function if the passed Int is lesser than 0, wrapped in a Left") {
        PosZInt.rightOrElse(-1)(i => i) shouldBe Left(-1)
        PosZInt.rightOrElse(-99)(i => i.toLong + 3L) shouldBe Left(-96L)
      }
    }
    describe("should offer an isValid predicate method that") {
      it("returns true if the passed Int is greater than or equal to 0") {
        PosZInt.isValid(50) shouldBe true
        PosZInt.isValid(100) shouldBe true
        PosZInt.isValid(0) shouldBe true
        PosZInt.isValid(-0) shouldBe true
        PosZInt.isValid(-1) shouldBe false
        PosZInt.isValid(-99) shouldBe false
      }
    } 
    describe("should offer a fromOrElse factory method that") {
      it("returns a PosZInt if the passed Int is greater than or equal to 0") {
        PosZInt.fromOrElse(50, PosZInt(42)).value shouldBe 50
        PosZInt.fromOrElse(100, PosZInt(42)).value shouldBe 100
        PosZInt.fromOrElse(0, PosZInt(42)).value shouldBe 0
      }
      it("returns a given default if the passed Int is NOT greater than or equal to 0") {
        PosZInt.fromOrElse(-1, PosZInt(42)).value shouldBe 42
        PosZInt.fromOrElse(-99, PosZInt(42)).value shouldBe 42
      }
    } 
    it("should offer MaxValue and MinValue factory methods") {
      PosZInt.MaxValue shouldEqual PosZInt.from(Int.MaxValue).get
      PosZInt.MinValue shouldEqual PosZInt(0)
    }

    it("should be sortable") {
      val xs = List(PosZInt(2), PosZInt(0), PosZInt(1), PosZInt(3))
      xs.sorted shouldEqual List(PosZInt(0), PosZInt(1), PosZInt(2), PosZInt(3))
    }

    describe("when created with apply method") {

      it("should compile when 8 is passed in") {
        "PosZInt(8)" should compile
        PosZInt(8).value shouldEqual 8
      }

      it("should compile when 0 is passed in") {
        "PosZInt(0)" should compile
        PosZInt(0).value shouldEqual 0
      }

      it("should not compile when -8 is passed in") {
        "PosZInt(-8)" shouldNot compile
      }

      it("should not compile when x is passed in") {
        val x: Int = -8
        "PosZInt(x)" shouldNot compile
      }
    }
    describe("when specified as a plain-old Int") {

      def takesPosZInt(pos: PosZInt): Int = pos.value

      it("should compile when 8 is passed in") {
        "takesPosZInt(8)" should compile
        takesPosZInt(8) shouldEqual 8
      }

      it("should compile when 0 is passed in") {
        "takesPosZInt(0)" should compile
      }

      it("should not compile when -8 is passed in") {
        "takesPosZInt(-8)" shouldNot compile
      }

      it("should not compile when x is passed in") {
        val x: Int = -8
        "takesPosZInt(x)" shouldNot compile
      }
    }

    it("should offer a unary ~ method that is consistent with Int") {
      forAll { (pzint: PosZInt) =>
        (~pzint) shouldEqual (~(pzint.toInt))
      }
    }

    it("should offer a unary + method that is consistent with Int") {
      forAll { (p: PosZInt) =>
        (+p).toInt shouldEqual (+(p.toInt))
      }
    }

    it("should offer a unary - method that returns NegZInt") {
      forAll { (p: PosZInt) =>
        (-p) shouldEqual (NegZInt.ensuringValid(-(p.toInt)))
      }
    }

    it("should offer << methods that are consistent with Int") {
      forAll { (pzint: PosZInt, shift: Int) =>
        pzint << shift shouldEqual pzint.toInt << shift
      }
      forAll { (pzint: PosZInt, shift: Long) =>
        pzint << shift shouldEqual pzint.toInt << shift
      }
    }

    it("should offer >>> methods that are consistent with Int") {
      forAll { (pzint: PosZInt, shift: Int) =>
        pzint >>> shift shouldEqual pzint.toInt >>> shift
      }
      forAll { (pzint: PosZInt, shift: Long) =>
        pzint >>> shift shouldEqual pzint.toInt >>> shift
      }
    }

    it("should offer >> methods that are consistent with Int") {
      forAll { (pzint: PosZInt, shift: Int) =>
        pzint >> shift shouldEqual pzint.toInt >> shift
      }
      forAll { (pzint: PosZInt, shift: Long) =>
        pzint >> shift shouldEqual pzint.toInt >> shift
      }
    }

    it("should offer a '|' method that is consistent with Int") {
      forAll { (pzint: PosZInt, byte: Byte) =>
        (pzint | byte) shouldEqual (pzint.toInt | byte)
      }
      forAll { (pzint: PosZInt, short: Short) =>
        (pzint | short) shouldEqual (pzint.toInt | short)
      }
      forAll { (pzint: PosZInt, char: Char) =>
        (pzint | char) shouldEqual (pzint.toInt | char)
      }
      forAll { (pzint: PosZInt, int: Int) =>
        (pzint | int) shouldEqual (pzint.toInt | int)
      }
      forAll { (pzint: PosZInt, long: Long) =>
        (pzint | long) shouldEqual (pzint.toInt | long)
      }
    }

    it("should offer an '&' method that is consistent with Int") {
      forAll { (pzint: PosZInt, byte: Byte) =>
        (pzint & byte) shouldEqual (pzint.toInt & byte)
      }
      forAll { (pzint: PosZInt, short: Short) =>
        (pzint & short) shouldEqual (pzint.toInt & short)
      }
      forAll { (pzint: PosZInt, char: Char) =>
        (pzint & char) shouldEqual (pzint.toInt & char)
      }
      forAll { (pzint: PosZInt, int: Int) =>
        (pzint & int) shouldEqual (pzint.toInt & int)
      }
      forAll { (pzint: PosZInt, long: Long) =>
        (pzint & long) shouldEqual (pzint.toInt & long)
      }
    }

    it("should offer an '^' method that is consistent with Int") {
      forAll { (pzint: PosZInt, byte: Byte) =>
        (pzint ^ byte) shouldEqual (pzint.toInt ^ byte)
      }
      forAll { (pzint: PosZInt, char: Char) =>
        (pzint ^ char) shouldEqual (pzint.toInt ^ char)
      }
      forAll { (pzint: PosZInt, short: Short) =>
        (pzint ^ short) shouldEqual (pzint.toInt ^ short)
      }
      forAll { (pzint: PosZInt, int: Int) =>
        (pzint ^ int) shouldEqual (pzint.toInt ^ int)
      }
      forAll { (pzint: PosZInt, long: Long) =>
        (pzint ^ long) shouldEqual (pzint.toInt ^ long)
      }
    }

    it("should offer 'min' and 'max' methods that are consistent with Int") {
      forAll { (pzint1: PosZInt, pzint2: PosZInt) =>
        pzint1.max(pzint2).toInt shouldEqual pzint1.toInt.max(pzint2.toInt)
        pzint1.min(pzint2).toInt shouldEqual pzint1.toInt.min(pzint2.toInt)
      }
    }

    it("should offer a 'toBinaryString' method that is consistent with Int") {
      forAll { (pzint: PosZInt) =>
        pzint.toBinaryString shouldEqual pzint.toInt.toBinaryString
      }
    }

    it("should offer a 'toHexString' method that is consistent with Int") {
      forAll { (pzint: PosZInt) =>
        pzint.toHexString shouldEqual pzint.toInt.toHexString
      }
    }

    it("should offer a 'toOctalString' method that is consistent with Int") {
      forAll { (pzint: PosZInt) =>
        pzint.toOctalString shouldEqual pzint.toInt.toOctalString
      }
    }

    it("should offer 'to' and 'until' methods that are consistent with Int") {
      forAll { (pzint: PosZInt, end: Int, step: Int) =>
        // The reason we need this is that in Scala 2.10, the equals check (used by shouldEqual below) will call range.length
        // and it'll cause IllegalArgumentException to be thrown when we do the Try(x) shouldEqual Try(y) assertion below,
        // while starting from scala 2.11 the equals call implementation does not call .length.
        // To make the behavior consistent for all scala versions, we explicitly call .length for all returned Range, and
        // shall it throws IllegalArgumentException, it will be wrapped as Failure for the Try.
        def ensuringValid(range: Range): Range = {
          range.length  // IllegalArgumentException will be thrown if it is an invalid range, this will turn the Success to Failure for Try
          range
        }

        Try(ensuringValid(pzint.to(end))) shouldEqual Try(ensuringValid(pzint.toInt.to(end)))
        Try(ensuringValid(pzint.to(end, step))) shouldEqual Try(ensuringValid(pzint.toInt.to(end, step)))
        Try(ensuringValid(pzint.until(end))) shouldEqual Try(ensuringValid(pzint.toInt.until(end)))
        Try(ensuringValid(pzint.until(end, step))) shouldEqual Try(ensuringValid(pzint.toInt.until(end, step)))
      }
    }

    it("should offer widening methods for basic types that are consistent with Int") {
      forAll { (pzint: PosZInt) =>
        def widen(value: Int): Int = value
        widen(pzint) shouldEqual widen(pzint.toInt)
      }
      forAll { (pzint: PosZInt) =>
        def widen(value: Long): Long = value
        widen(pzint) shouldEqual widen(pzint.toInt)
      }
      forAll { (pzint: PosZInt) =>
        def widen(value: Float): Float = value
        widen(pzint) shouldEqual widen(pzint.toInt)
      }
      forAll { (pzint: PosZInt) =>
        def widen(value: Double): Double = value
        widen(pzint) shouldEqual widen(pzint.toInt)
      }
      forAll { (pzint: PosZInt) =>
        def widen(value: PosZLong): PosZLong = value
        widen(pzint) shouldEqual widen(PosZLong.from(pzint.toInt).get)
      }
      forAll { (pzint: PosZInt) =>
        def widen(value: PosZFloat): PosZFloat = value
        widen(pzint) shouldEqual widen(PosZFloat.from(pzint.toInt).get)
      }
      forAll { (pzint: PosZInt) =>
        def widen(value: PosZDouble): PosZDouble = value
        widen(pzint) shouldEqual widen(PosZDouble.from(pzint.toInt).get)
      }
    }
    it("should offer an ensuringValid method that takes an Int => Int, throwing AssertionError if the result is invalid") {
      PosZInt(33).ensuringValid(_ + 1) shouldEqual PosZInt(34)
      an [AssertionError] should be thrownBy { PosZInt.MaxValue.ensuringValid(_ + 1) }
    }
  }
}

