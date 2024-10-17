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
import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import OptionValues._

import scala.util.{Failure, Success, Try}
import org.scalactic.{Validation, Pass, Fail}
import org.scalactic.{Or, Good, Bad}

trait PosIntSpecSupport {

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

class PosIntSpec extends funspec.AnyFunSpec with matchers.should.Matchers with GeneratorDrivenPropertyChecks with PosIntSpecSupport {

  describe("A PosInt") {

    describe("should offer a from factory method that") {
      it("returns Some[PosInt] if the passed Int is greater than 0") {
        PosInt.from(50).value.value shouldBe 50
        PosInt.from(100).value.value shouldBe 100
      }

      it("returns None if the passed Int is NOT greater than 0") {
        PosInt.from(0) shouldBe None
        PosInt.from(-1) shouldBe None
        PosInt.from(-99) shouldBe None
      }
    }
    describe("should offer an ensuringValid factory method that") {
      it("returns PosInt if the passed Int is greater than 0") {
        PosInt.ensuringValid(50).value shouldBe 50
        PosInt.ensuringValid(100).value shouldBe 100
      }

      it("throws AssertionError if the passed Int is NOT greater than 0") {
        an [AssertionError] should be thrownBy PosInt.ensuringValid(0)
        an [AssertionError] should be thrownBy PosInt.ensuringValid(-1)
        an [AssertionError] should be thrownBy PosInt.ensuringValid(-99)
      }
    }
    describe("should offer a tryingValid factory method that") {
      import TryValues._
      it("returns a PosInt wrapped in a Success if the passed Int is greater than 0") {
        PosInt.tryingValid(50).success.value.value shouldBe 50
        PosInt.tryingValid(100).success.value.value shouldBe 100
      }

      it("returns an AssertionError wrapped in a Failure if the passed Int is NOT greater than 0") {
        PosInt.tryingValid(0).failure.exception shouldBe an [AssertionError]
        PosInt.tryingValid(-1).failure.exception shouldBe an [AssertionError]
        PosInt.tryingValid(-99).failure.exception shouldBe an [AssertionError]
      }
    }
    describe("should offer a passOrElse factory method that") {
      it("returns a Pass if the given Int is greater than 0") {
        PosInt.passOrElse(50)(i => i) shouldBe Pass
        PosInt.passOrElse(100)(i => i) shouldBe Pass
      }
      it("returns an error value produced by passing the given Int to the given function if the passed Int is NOT greater than 0, wrapped in a Fail") {
        PosInt.passOrElse(0)(i => s"$i did not taste good") shouldBe Fail("0 did not taste good")
        PosInt.passOrElse(-1)(i => i) shouldBe Fail(-1)
        PosInt.passOrElse(-99)(i => i.toLong + 3L) shouldBe Fail(-96L)
      }
    }
    describe("should offer a goodOrElse factory method that") {
      it("returns a PosInt wrapped in a Good if the given Int is greater than 0") {
        PosInt.goodOrElse(50)(i => i) shouldBe Good(PosInt(50))
        PosInt.goodOrElse(100)(i => i) shouldBe Good(PosInt(100))
      }
      it("returns an error value produced by passing the given Int to the given function if the passed Int is NOT greater than 0, wrapped in a Bad") {
        PosInt.goodOrElse(0)(i => s"$i did not taste good") shouldBe Bad("0 did not taste good")
        PosInt.goodOrElse(-1)(i => i) shouldBe Bad(-1)
        PosInt.goodOrElse(-99)(i => i.toLong + 3L) shouldBe Bad(-96L)
      }
    }
    describe("should offer a rightOrElse factory method that") {
      it("returns a PosInt wrapped in a Right if the given Int is greater than 0") {
        PosInt.rightOrElse(50)(i => i) shouldBe Right(PosInt(50))
        PosInt.rightOrElse(100)(i => i) shouldBe Right(PosInt(100))
      }
      it("returns an error value produced by passing the given Int to the given function if the passed Int is NOT greater than 0, wrapped in a Left") {
        PosInt.rightOrElse(0)(i => s"$i did not taste good") shouldBe Left("0 did not taste good")
        PosInt.rightOrElse(-1)(i => i) shouldBe Left(-1)
        PosInt.rightOrElse(-99)(i => i.toLong + 3L) shouldBe Left(-96L)
      }
    }
    describe("should offer an isValid predicate method that") {
      it("returns true if the passed Int is greater than 0") {
        PosInt.isValid(50) shouldBe true
        PosInt.isValid(100) shouldBe true
        PosInt.isValid(0) shouldBe false
        PosInt.isValid(-0) shouldBe false
        PosInt.isValid(-1) shouldBe false
        PosInt.isValid(-99) shouldBe false
      }
    } 
    describe("should offer a fromOrElse factory method that") {
      it("returns a PosInt if the passed Int is greater than 0") {
        PosInt.fromOrElse(50, PosInt(42)).value shouldBe 50
        PosInt.fromOrElse(100, PosInt(42)).value shouldBe 100
      }
      it("returns a given default if the passed Int is NOT greater than 0") {
        PosInt.fromOrElse(0, PosInt(42)).value shouldBe 42
        PosInt.fromOrElse(-1, PosInt(42)).value shouldBe 42
        PosInt.fromOrElse(-99, PosInt(42)).value shouldBe 42
      }
    } 
    it("should offer MaxValue and MinValue factory methods") {
      PosInt.MaxValue shouldEqual PosInt.from(Int.MaxValue).get
      PosInt.MinValue shouldEqual PosInt(1)
    }

    it("should be sortable") {
      val xs = List(PosInt(2), PosInt(4), PosInt(1), PosInt(3))
      xs.sorted shouldEqual List(PosInt(1), PosInt(2), PosInt(3), PosInt(4))
    }

    describe("when created with apply method") {

      it("should compile when 8 is passed in") {
        "PosInt(8)" should compile
        PosInt(8).value shouldEqual 8
      }

      it("should not compile when 0 is passed in") {
        "PosInt(0)" shouldNot compile
      }

      it("should not compile when -8 is passed in") {
        "PosInt(-8)" shouldNot compile
      }

      it("should not compile when x is passed in") {
        val x: Int = -8
        "PosInt(x)" shouldNot compile
      }
    }

    describe("when specified as a plain-old Int") {

      def takesPosInt(pos: PosInt): Int = pos.value

      it("should compile when 8 is passed in") {
        "takesPosInt(8)" should compile
        takesPosInt(8) shouldEqual 8
      }

      it("should not compile when 0 is passed in") {
        "takesPosInt(0)" shouldNot compile
      }

      it("should not compile when -8 is passed in") {
        "takesPosInt(-8)" shouldNot compile
      }

      it("should not compile when x is passed in") {
        val x: Int = -8
        "takesPosInt(x)" shouldNot compile
      }
    }

    it("should offer a unary ~ method that is consistent with Int") {
      forAll { (pint: PosInt) =>
        (~pint) shouldEqual (~(pint.toInt))
      }
    }

    it("should offer a unary + method that is consistent with Int") {
      forAll { (p: PosInt) =>
        (+p).toInt shouldEqual (+(p.toInt))
      }
    }

    it("should offer a unary - method that returns NegInt") {
      forAll { (p: PosInt) =>
        (-p) shouldEqual (NegInt.ensuringValid(-(p.toInt)))
      }
    }

    it("should offer << methods that are consistent with Int") {
      forAll { (pint: PosInt, shift: Int) =>
        pint << shift shouldEqual pint.toInt << shift
      }
      forAll { (pint: PosInt, shift: Long) =>
        pint << shift shouldEqual pint.toInt << shift
      }
    }

    it("should offer >>> methods that are consistent with Int") {
      forAll { (pint: PosInt, shift: Int) =>
        pint >>> shift shouldEqual pint.toInt >>> shift
      }
      forAll { (pint: PosInt, shift: Long) =>
        pint >>> shift shouldEqual pint.toInt >>> shift
      }
    }

    it("should offer >> methods that are consistent with Int") {
      forAll { (pint: PosInt, shift: Int) =>
        pint >> shift shouldEqual pint.toInt >> shift
      }
      forAll { (pint: PosInt, shift: Long) =>
        pint >> shift shouldEqual pint.toInt >> shift
      }
    }

    it("should offer a '|' method that is consistent with Int") {
      forAll { (pint: PosInt, byte: Byte) =>
        (pint | byte) shouldEqual (pint.toInt | byte)
      }
      forAll { (pint: PosInt, short: Short) =>
        (pint | short) shouldEqual (pint.toInt | short)
      }
      forAll { (pint: PosInt, char: Char) =>
        (pint | char) shouldEqual (pint.toInt | char)
      }
      forAll { (pint: PosInt, int: Int) =>
        (pint | int) shouldEqual (pint.toInt | int)
      }
      forAll { (pint: PosInt, long: Long) =>
        (pint | long) shouldEqual (pint.toInt | long)
      }
    }

    it("should offer an '&' method that is consistent with Int") {
      forAll { (pint: PosInt, byte: Byte) =>
        (pint & byte) shouldEqual (pint.toInt & byte)
      }
      forAll { (pint: PosInt, short: Short) =>
        (pint & short) shouldEqual (pint.toInt & short)
      }
      forAll { (pint: PosInt, char: Char) =>
        (pint & char) shouldEqual (pint.toInt & char)
      }
      forAll { (pint: PosInt, int: Int) =>
        (pint & int) shouldEqual (pint.toInt & int)
      }
      forAll { (pint: PosInt, long: Long) =>
        (pint & long) shouldEqual (pint.toInt & long)
      }
    }

    it("should offer an '^' method that is consistent with Int") {
      forAll { (pint: PosInt, byte: Byte) =>
        (pint ^ byte) shouldEqual (pint.toInt ^ byte)
      }
      forAll { (pint: PosInt, char: Char) =>
        (pint ^ char) shouldEqual (pint.toInt ^ char)
      }
      forAll { (pint: PosInt, short: Short) =>
        (pint ^ short) shouldEqual (pint.toInt ^ short)
      }
      forAll { (pint: PosInt, int: Int) =>
        (pint ^ int) shouldEqual (pint.toInt ^ int)
      }
      forAll { (pint: PosInt, long: Long) =>
        (pint ^ long) shouldEqual (pint.toInt ^ long)
      }
    }

    it("should offer 'min' and 'max' methods that are consistent with Int") {
      forAll { (pint1: PosInt, pint2: PosInt) =>
        pint1.max(pint2).toInt shouldEqual pint1.toInt.max(pint2.toInt)
        pint1.min(pint2).toInt shouldEqual pint1.toInt.min(pint2.toInt)
      }
    }

    it("should offer a 'toBinaryString' method that is consistent with Int") {
      forAll { (pint: PosInt) =>
        pint.toBinaryString shouldEqual pint.toInt.toBinaryString
      }
    }

    it("should offer a 'toHexString' method that is consistent with Int") {
      forAll { (pint: PosInt) =>
        pint.toHexString shouldEqual pint.toInt.toHexString
      }
    }

    it("should offer a 'toOctalString' method that is consistent with Int") {
      forAll { (pint: PosInt) =>
        pint.toOctalString shouldEqual pint.toInt.toOctalString
      }
    }

    it("should offer 'to' and 'until' methods that are consistent with Int") {
      forAll { (pint: PosInt, end: Int, step: Int) =>
        // The reason we need this is that in Scala 2.10, the equals check (used by shouldEqual below) will call range.length
        // and it'll cause IllegalArgumentException to be thrown when we do the Try(x) shouldEqual Try(y) assertion below,
        // while starting from scala 2.11 the equals call implementation does not call .length.
        // To make the behavior consistent for all scala versions, we explicitly call .length for all returned Range, and
        // shall it throws IllegalArgumentException, it will be wrapped as Failure for the Try.
        def ensuringValid(range: Range): Range = {
          range.length  // IllegalArgumentException will be thrown if it is an invalid range, this will turn the Success to Failure for Try
          range
        }

        Try(ensuringValid(pint.to(end))) shouldEqual Try(ensuringValid(pint.toInt.to(end)))
        Try(ensuringValid(pint.to(end, step))) shouldEqual Try(ensuringValid(pint.toInt.to(end, step)))
        Try(ensuringValid(pint.until(end))) shouldEqual Try(ensuringValid(pint.toInt.until(end)))
        Try(ensuringValid(pint.until(end, step))) shouldEqual Try(ensuringValid(pint.toInt.until(end, step)))
      }
    }

    it("should offer an ensuringValid method that takes an Int => Int, throwing AssertionError if the result is invalid") {
      PosInt(33).ensuringValid(_ + 1) shouldEqual PosInt(34)
      an [AssertionError] should be thrownBy { PosInt.MaxValue.ensuringValid(_ + 1) }
    }
  }
}

