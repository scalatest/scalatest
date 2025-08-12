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

import org.scalactic.Equality
import org.scalactic.{Pass, Fail}
import org.scalactic.{Good, Bad}
import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import scala.collection.mutable.WrappedArray
import OptionValues._

import scala.util.{Failure, Success, Try}

trait NegZIntSpecSupport {

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

class NegZIntSpec extends funspec.AnyFunSpec with matchers.should.Matchers with GeneratorDrivenPropertyChecks with NegZIntSpecSupport {

  describe("A NegZInt") {
    describe("should offer a from factory method that") {
      it("returns Some[NegZInt] if the passed Int is lesser than or equal to 0") {
        NegZInt.from(0).value.value shouldBe 0
        NegZInt.from(-50).value.value shouldBe -50
        NegZInt.from(-100).value.value shouldBe -100
      }
      it("returns None if the passed Int is NOT lesser than or equal to 0") {
        NegZInt.from(1) shouldBe None
        NegZInt.from(99) shouldBe None
      }
    }
    describe("should offer an ensuringValid factory method that") {
      it("returns NegZInt if the passed Int is lesser than or equal to 0") {
        NegZInt.ensuringValid(0).value shouldBe 0
        NegZInt.ensuringValid(-50).value shouldBe -50
        NegZInt.ensuringValid(-100).value shouldBe -100
      }
      it("throws AssertionError if the passed Int is NOT lesser than or equal to 0") {
        an [AssertionError] should be thrownBy NegZInt.ensuringValid(1)
        an [AssertionError] should be thrownBy NegZInt.ensuringValid(99)
      }
    }
    describe("should offer a tryingValid factory method that") {
      import TryValues._
      it("returns a NegZInt wrapped in a Success if the passed Int is lesser than or equal 0") {
        NegZInt.tryingValid(-0).success.value.value shouldBe -0
        NegZInt.tryingValid(-50).success.value.value shouldBe -50
        NegZInt.tryingValid(-100).success.value.value shouldBe -100
      }

      it("returns an AssertionError wrapped in a Failure if the passed Int is greater than 0") {
        NegZInt.tryingValid(1).failure.exception shouldBe an [AssertionError]
        NegZInt.tryingValid(99).failure.exception shouldBe an [AssertionError]
      }
    }
    describe("should offer a passOrElse factory method that") {
      it("returns a Pass if the given Int is lesser than or equal 0") {
        NegZInt.passOrElse(0)(i => i) shouldBe Pass
        NegZInt.passOrElse(-50)(i => i) shouldBe Pass
        NegZInt.passOrElse(-100)(i => i) shouldBe Pass
      }
      it("returns an error value produced by passing the given Int to the given function if the passed Int is greater than 0, wrapped in a Fail") {
        NegZInt.passOrElse(1)(i => i) shouldBe Fail(1)
        NegZInt.passOrElse(99)(i => i.toLong + 3L) shouldBe Fail(102L)
      }
    }
    describe("should offer a goodOrElse factory method that") {
      it("returns a NegZInt wrapped in a Good if the given Int is lesser than or equal 0") {
        NegZInt.goodOrElse(-0)(i => i) shouldBe Good(NegZInt(-0))
        NegZInt.goodOrElse(-50)(i => i) shouldBe Good(NegZInt(-50))
        NegZInt.goodOrElse(-100)(i => i) shouldBe Good(NegZInt(-100))
      }
      it("returns an error value produced by passing the given Int to the given function if the passed Int is greater than 0, wrapped in a Bad") {
        NegZInt.goodOrElse(1)(i => i) shouldBe Bad(1)
        NegZInt.goodOrElse(99)(i => i.toLong + 3L) shouldBe Bad(102L)
      }
    }
    describe("should offer a rightOrElse factory method that") {
      it("returns a NegZInt wrapped in a Right if the given Int is lesser than or equal 0") {
        NegZInt.rightOrElse(0)(i => i) shouldBe Right(NegZInt(0))
        NegZInt.rightOrElse(-50)(i => i) shouldBe Right(NegZInt(-50))
        NegZInt.rightOrElse(-100)(i => i) shouldBe Right(NegZInt(-100))
      }
      it("returns an error value produced by passing the given Int to the given function if the passed Int is greater than 0, wrapped in a Left") {
        NegZInt.rightOrElse(1)(i => i) shouldBe Left(1)
        NegZInt.rightOrElse(99)(i => i.toLong + 3L) shouldBe Left(102L)
      }
    }
    describe("should offer an isValid predicate method that") {
      it("returns true if the passed Int is lesser than or equal to 0") {
        NegZInt.isValid(-50) shouldBe true
        NegZInt.isValid(-100) shouldBe true
        NegZInt.isValid(0) shouldBe true
        NegZInt.isValid(-0) shouldBe true
        NegZInt.isValid(1) shouldBe false
        NegZInt.isValid(99) shouldBe false
      }
    }
    describe("should offer a fromOrElse factory method that") {
      it("returns a NegZInt if the passed Int is lesser than or equal to 0") {
        NegZInt.fromOrElse(-50, NegZInt(-42)).value shouldBe -50
        NegZInt.fromOrElse(-100, NegZInt(-42)).value shouldBe -100
        NegZInt.fromOrElse(0, NegZInt(-42)).value shouldBe 0
      }
      it("returns a given default if the passed Int is NOT greater than 0") {
        NegZInt.fromOrElse(1, NegZInt(-42)).value shouldBe -42
        NegZInt.fromOrElse(99, NegZInt(-42)).value shouldBe -42
      }
    }
    it("should offer MaxValue and MinValue factory methods") {
      NegZInt.MaxValue shouldEqual NegZInt.from(0).get
      NegZInt.MinValue shouldEqual NegZInt.from(Int.MinValue).get
    }

    it("should be sortable") {
      val xs = List(NegZInt(-2), NegZInt(-0), NegZInt(-1), NegZInt(-3))
      xs.sorted shouldEqual List(NegZInt(-3), NegZInt(-2), NegZInt(-1), NegZInt(0))
    }

    describe("when created with apply method") {

      it("should compile when -8 is passed in") {
        "NegZInt(-8)" should compile
        NegZInt(-8).value shouldEqual -8
      }

      it("should compile when 0 is passed in") {
        "NegZInt(0)" should compile
        NegZInt(0).value shouldEqual 0
      }

      it("should not compile when 8 is passed in") {
        "NegZInt(8)" shouldNot compile
      }

      it("should not compile when x is passed in") {
        val x: Int = -8
        "NegZInt(x)" shouldNot compile
      }
    }
    describe("when specified as a plain-old Int") {

      def takesNegZInt(pos: NegZInt): Int = pos.value

      it("should compile when -8 is passed in") {
        "takesNegZInt(-8)" should compile
        takesNegZInt(-8) shouldEqual -8
      }

      it("should compile when 0 is passed in") {
        "takesNegZInt(0)" should compile
      }

      it("should not compile when 8 is passed in") {
        "takesNegZInt(8)" shouldNot compile
      }

      it("should not compile when x is passed in") {
        val x: Int = -8
        "takesNegZInt(x)" shouldNot compile
      }
    }

    it("should offer a unary ~ method that is consistent with Int") {
      forAll { (pzint: NegZInt) =>
        (~pzint) shouldEqual (~(pzint.toInt))
      }
    }

    it("should offer a unary + method that is consistent with Int") {
      forAll { (p: NegZInt) =>
        (+p).toInt shouldEqual (+(p.toInt))
      }
    }

    it("should offer a unary - method that returns PosZInt") {
      forAll { (p: NegZInt) =>
        (-p) shouldEqual (-(p.toInt))
      }
    }

    it("should offer << methods that are consistent with Int") {
      forAll { (pzint: NegZInt, shift: Int) =>
        pzint << shift shouldEqual pzint.toInt << shift
      }
      forAll { (pzint: NegZInt, shift: Long) =>
        pzint << shift shouldEqual pzint.toInt << shift
      }
    }

    it("should offer >>> methods that are consistent with Int") {
      forAll { (pzint: NegZInt, shift: Int) =>
        pzint >>> shift shouldEqual pzint.toInt >>> shift
      }
      forAll { (pzint: NegZInt, shift: Long) =>
        pzint >>> shift shouldEqual pzint.toInt >>> shift
      }
    }

    it("should offer >> methods that are consistent with Int") {
      forAll { (pzint: NegZInt, shift: Int) =>
        pzint >> shift shouldEqual pzint.toInt >> shift
      }
      forAll { (pzint: NegZInt, shift: Long) =>
        pzint >> shift shouldEqual pzint.toInt >> shift
      }
    }

    it("should offer a '|' method that is consistent with Int") {
      forAll { (pzint: NegZInt, byte: Byte) =>
        (pzint | byte) shouldEqual (pzint.toInt | byte)
      }
      forAll { (pzint: NegZInt, short: Short) =>
        (pzint | short) shouldEqual (pzint.toInt | short)
      }
      forAll { (pzint: NegZInt, char: Char) =>
        (pzint | char) shouldEqual (pzint.toInt | char)
      }
      forAll { (pzint: NegZInt, int: Int) =>
        (pzint | int) shouldEqual (pzint.toInt | int)
      }
      forAll { (pzint: NegZInt, long: Long) =>
        (pzint | long) shouldEqual (pzint.toInt | long)
      }
    }

    it("should offer an '&' method that is consistent with Int") {
      forAll { (pzint: NegZInt, byte: Byte) =>
        (pzint & byte) shouldEqual (pzint.toInt & byte)
      }
      forAll { (pzint: NegZInt, short: Short) =>
        (pzint & short) shouldEqual (pzint.toInt & short)
      }
      forAll { (pzint: NegZInt, char: Char) =>
        (pzint & char) shouldEqual (pzint.toInt & char)
      }
      forAll { (pzint: NegZInt, int: Int) =>
        (pzint & int) shouldEqual (pzint.toInt & int)
      }
      forAll { (pzint: NegZInt, long: Long) =>
        (pzint & long) shouldEqual (pzint.toInt & long)
      }
    }

    it("should offer an '^' method that is consistent with Int") {
      forAll { (pzint: NegZInt, byte: Byte) =>
        (pzint ^ byte) shouldEqual (pzint.toInt ^ byte)
      }
      forAll { (pzint: NegZInt, char: Char) =>
        (pzint ^ char) shouldEqual (pzint.toInt ^ char)
      }
      forAll { (pzint: NegZInt, short: Short) =>
        (pzint ^ short) shouldEqual (pzint.toInt ^ short)
      }
      forAll { (pzint: NegZInt, int: Int) =>
        (pzint ^ int) shouldEqual (pzint.toInt ^ int)
      }
      forAll { (pzint: NegZInt, long: Long) =>
        (pzint ^ long) shouldEqual (pzint.toInt ^ long)
      }
    }

    it("should offer 'min' and 'max' methods that are consistent with Int") {
      forAll { (pzint1: NegZInt, pzint2: NegZInt) =>
        pzint1.max(pzint2).toInt shouldEqual pzint1.toInt.max(pzint2.toInt)
        pzint1.min(pzint2).toInt shouldEqual pzint1.toInt.min(pzint2.toInt)
      }
    }

    it("should offer a 'toBinaryString' method that is consistent with Int") {
      forAll { (pzint: NegZInt) =>
        pzint.toBinaryString shouldEqual pzint.toInt.toBinaryString
      }
    }

    it("should offer a 'toHexString' method that is consistent with Int") {
      forAll { (pzint: NegZInt) =>
        pzint.toHexString shouldEqual pzint.toInt.toHexString
      }
    }

    it("should offer a 'toOctalString' method that is consistent with Int") {
      forAll { (pzint: NegZInt) =>
        pzint.toOctalString shouldEqual pzint.toInt.toOctalString
      }
    }

    it("should offer 'to' and 'until' methods that are consistent with Int") {
      // The reason we need this is that in Scala 2.10, the equals check (used by shouldEqual below) will call range.length
      // and it'll cause IllegalArgumentException to be thrown when we do the Try(x) shouldEqual Try(y) assertion below,
      // while starting from scala 2.11 the equals call implementation does not call .length.
      // To make the behavior consistent for all scala versions, we explicitly call .length for all returned Range, and
      // shall it throws IllegalArgumentException, it will be wrapped as Failure for the Try.
      def ensuringValid(range: Range): Range = {
        range.length  // IllegalArgumentException will be thrown if it is an invalid range, this will turn the Success to Failure for Try
        range
      }

      forAll { (pzint: NegZInt, end: Int, step: Int) =>
        Try(ensuringValid(pzint.to(end)))shouldEqual Try(ensuringValid(pzint.toInt.to(end)))
        Try(ensuringValid(pzint.to(end, step))) shouldEqual Try(ensuringValid(pzint.toInt.to(end, step)))
        Try(ensuringValid(pzint.until(end))) shouldEqual Try(ensuringValid(pzint.toInt.until(end)))
        Try(ensuringValid(pzint.until(end, step))) shouldEqual Try(ensuringValid(pzint.toInt.until(end, step)))
      }
    }

    it("should offer an ensuringValid method that takes an Int => Int, throwing AssertionError if the result is invalid") {
      NegZInt(-33).ensuringValid(_ + 1) shouldEqual NegZInt(-32)
      an [AssertionError] should be thrownBy { NegZInt.MaxValue.ensuringValid(_ + 1) }
    }
  }
}