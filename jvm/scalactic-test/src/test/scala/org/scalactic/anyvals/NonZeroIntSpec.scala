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
import org.scalactic.{Pass, Fail}
import org.scalactic.{Good, Bad}
import scala.util.{Failure, Success, Try}

trait NonZeroIntSpecSupport {

  implicit def tryEquality[T]: Equality[Try[T]] = new Equality[Try[T]] {
    override def areEqual(a: Try[T], b: Any): Boolean = a match {
      case Success(double: Double) if double.isNaN =>  // This is because in scala.js x/0 results to NaN not ArithmetricException like in jvm, and we need to make sure Success(NaN) == Success(NaN) is true to pass the test.
        b match {
          case Success(bDouble: Double) if bDouble.isNaN => true
          case _ => false
        }
      // I needed this because with GenDrivenPropertyChecks, got:
      // [info] - should offer a '%' method that is consistent with Int *** FAILED ***
      // [info]   Success(NaN) did not equal Success(NaN) (NonZeroIntExperiment.scala:498)
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

class NonZeroIntSpec extends funspec.AnyFunSpec with matchers.should.Matchers with GeneratorDrivenPropertyChecks with NonZeroIntSpecSupport {

  describe("A NonZeroInt") {

    describe("should offer a from factory method that") {
      it("returns Some[NonZeroInt] if the passed Int is greater than 0") {
        NonZeroInt.from(50).value.value shouldBe 50
        NonZeroInt.from(100).value.value shouldBe 100
      }

      it("returns Some[NonZeroInt] if the passed Int is lesser than 0") {
        NonZeroInt.from(-1).value.value shouldBe -1
        NonZeroInt.from(-99).value.value shouldBe -99
      }

      it("returns None if the passed Int is 0") {
        NonZeroInt.from(0) shouldBe None
      }
    }
    describe("should offer an ensuringValid factory method that") {
      it("returns NonZeroInt if the passed Int is greater than 0") {
        NonZeroInt.ensuringValid(50).value shouldBe 50
        NonZeroInt.ensuringValid(100).value shouldBe 100
      }

      it("returns NonZeroInt if the passed Int is lesser than 0") {
        NonZeroInt.ensuringValid(-1).value shouldBe -1
        NonZeroInt.ensuringValid(-99).value shouldBe -99
      }

      it("throws AssertionError if the passed Int is 0") {
        an [AssertionError] should be thrownBy NonZeroInt.ensuringValid(0)
      }
    }
    describe("should offer a tryingValid factory method that") {
      import TryValues._
      it("returns a NonZeroInt wrapped in a Success if the passed Int is non-zero") {
        NonZeroInt.tryingValid(50).success.value.value shouldBe 50
        NonZeroInt.tryingValid(100).success.value.value shouldBe 100
        NonZeroInt.tryingValid(-50).success.value.value shouldBe -50
        NonZeroInt.tryingValid(-100).success.value.value shouldBe -100
      }

      it("returns an AssertionError wrapped in a Failure if the passed Int is NOT non-zero") {
        NonZeroInt.tryingValid(0).failure.exception shouldBe an [AssertionError]
      }
    }
    describe("should offer a passOrElse factory method that") {
      it("returns a Pass if the given Int is non-zero") {
        NonZeroInt.passOrElse(50)(i => i) shouldBe Pass
        NonZeroInt.passOrElse(100)(i => i) shouldBe Pass

        NonZeroInt.passOrElse(-1)(i => i) shouldBe Pass
        NonZeroInt.passOrElse(-99)(i => i) shouldBe Pass
      }
      it("returns an error value produced by passing the given Int to the given function if the passed Int is NOT non-zero, wrapped in a Fail") {
        NonZeroInt.passOrElse(0)(i => s"$i did not taste good") shouldBe Fail("0 did not taste good")
      }
    }
    describe("should offer a goodOrElse factory method that") {
      it("returns a NonZeroInt wrapped in a Good if the given Int is non-zero") {
        NonZeroInt.goodOrElse(50)(i => i) shouldBe Good(NonZeroInt(50))
        NonZeroInt.goodOrElse(100)(i => i) shouldBe Good(NonZeroInt(100))

        NonZeroInt.goodOrElse(-1)(i => i) shouldBe Good(NonZeroInt(-1))
        NonZeroInt.goodOrElse(-99)(i => i) shouldBe Good(NonZeroInt(-99))
      }
      it("returns an error value produced by passing the given Int to the given function if the passed Int is NOT non-zero, wrapped in a Bad") {
        NonZeroInt.goodOrElse(0)(i => s"$i did not taste good") shouldBe Bad("0 did not taste good")
      }
    }
    describe("should offer a rightOrElse factory method that") {
      it("returns a NonZeroInt wrapped in a Right if the given Int is non-zero") {
        NonZeroInt.rightOrElse(50)(i => i) shouldBe Right(NonZeroInt(50))
        NonZeroInt.rightOrElse(100)(i => i) shouldBe Right(NonZeroInt(100))

        NonZeroInt.rightOrElse(-1)(i => i) shouldBe Right(NonZeroInt(-1))
        NonZeroInt.rightOrElse(-99)(i => i) shouldBe Right(NonZeroInt(-99))
      }
      it("returns an error value produced by passing the given Int to the given function if the passed Int is NOT non-zero, wrapped in a Left") {
        NonZeroInt.rightOrElse(0)(i => s"$i did not taste good") shouldBe Left("0 did not taste good")
      }
    }
    describe("should offer an isValid predicate method that") {
      it("returns true if the passed Int is not 0") {
        NonZeroInt.isValid(50) shouldBe true
        NonZeroInt.isValid(100) shouldBe true
        NonZeroInt.isValid(0) shouldBe false
        NonZeroInt.isValid(-0) shouldBe false
        NonZeroInt.isValid(-1) shouldBe true
        NonZeroInt.isValid(-99) shouldBe true
      }
    }
    describe("should offer a fromOrElse factory method that") {
      it("returns a NonZeroInt if the passed Int is greater than 0") {
        NonZeroInt.fromOrElse(50, NonZeroInt(42)).value shouldBe 50
        NonZeroInt.fromOrElse(100, NonZeroInt(42)).value shouldBe 100
      }
      it("returns a NonZeroInt if the passed Int is leser than 0") {
        NonZeroInt.fromOrElse(-1, NonZeroInt(42)).value shouldBe -1
        NonZeroInt.fromOrElse(-99, NonZeroInt(42)).value shouldBe -99
      }
      it("returns a given default if the passed Int is 0") {
        NonZeroInt.fromOrElse(0, NonZeroInt(42)).value shouldBe 42
      }
    }
    it("should offer MaxValue and MinValue factory methods") {
      NonZeroInt.MaxValue shouldEqual NonZeroInt.from(Int.MaxValue).get
      // SKIP-DOTTY-START
      // not a literal
      NonZeroInt.MinValue shouldEqual NonZeroInt(Int.MinValue)
      // SKIP-DOTTY-END
    }

    it("should be sortable") {
      val xs = List(NonZeroInt(2), NonZeroInt(4), NonZeroInt(1), NonZeroInt(3))
      xs.sorted shouldEqual List(NonZeroInt(1), NonZeroInt(2), NonZeroInt(3), NonZeroInt(4))
    }

    describe("when created with apply method") {

      it("should compile when 8 is passed in") {
        "NonZeroInt(8)" should compile
        NonZeroInt(8).value shouldEqual 8
      }

      it("should not compile when 0 is passed in") {
        "NonZeroInt(0)" shouldNot compile
      }

      it("should compile when -8 is passed in") {
        "NonZeroInt(-8)" should compile
        NonZeroInt(-8).value shouldEqual -8
      }

      it("should not compile when x is passed in") {
        val x: Int = -8
        "NonZeroInt(x)" shouldNot compile
      }
    }

    describe("when specified as a plain-old Int") {

      def takesNonZeroInt(non0: NonZeroInt): Int = non0.value

      it("should compile when 8 is passed in") {
        "takesNonZeroInt(8)" should compile
        takesNonZeroInt(8) shouldEqual 8
      }

      it("should not compile when 0 is passed in") {
        "takesNonZeroInt(0)" shouldNot compile
      }

      it("should compile when -8 is passed in") {
        "takesNonZeroInt(-8)" should compile
        takesNonZeroInt(-8) shouldEqual -8
      }

      it("should not compile when x is passed in") {
        val x: Int = -8
        "takesNonZeroInt(x)" shouldNot compile
      }
    }

    it("should offer a unary ~ method that is consistent with Int") {
      forAll { (nzint: NonZeroInt) =>
        (~nzint) shouldEqual (~(nzint.toInt))
      }
    }

    it("should offer << methods that are consistent with Int") {
      forAll { (nzint: NonZeroInt, shift: Int) =>
        nzint << shift shouldEqual nzint.toInt << shift
      }
      forAll { (nzint: NonZeroInt, shift: Long) =>
        nzint << shift shouldEqual nzint.toInt << shift
      }
    }

    it("should offer >>> methods that are consistent with Int") {
      forAll { (nzint: NonZeroInt, shift: Int) =>
        nzint >>> shift shouldEqual nzint.toInt >>> shift
      }
      forAll { (nzint: NonZeroInt, shift: Long) =>
        nzint >>> shift shouldEqual nzint.toInt >>> shift
      }
    }

    it("should offer >> methods that are consistent with Int") {
      forAll { (nzint: NonZeroInt, shift: Int) =>
        nzint >> shift shouldEqual nzint.toInt >> shift
      }
      forAll { (nzint: NonZeroInt, shift: Long) =>
        nzint >> shift shouldEqual nzint.toInt >> shift
      }
    }

    it("should offer a '|' method that is consistent with Int") {
      forAll { (nzint: NonZeroInt, byte: Byte) =>
        (nzint | byte) shouldEqual (nzint.toInt | byte)
      }
      forAll { (nzint: NonZeroInt, short: Short) =>
        (nzint | short) shouldEqual (nzint.toInt | short)
      }
      forAll { (nzint: NonZeroInt, char: Char) =>
        (nzint | char) shouldEqual (nzint.toInt | char)
      }
      forAll { (nzint: NonZeroInt, int: Int) =>
        (nzint | int) shouldEqual (nzint.toInt | int)
      }
      forAll { (nzint: NonZeroInt, long: Long) =>
        (nzint | long) shouldEqual (nzint.toInt | long)
      }
    }

    it("should offer an '&' method that is consistent with Int") {
      forAll { (nzint: NonZeroInt, byte: Byte) =>
        (nzint & byte) shouldEqual (nzint.toInt & byte)
      }
      forAll { (nzint: NonZeroInt, short: Short) =>
        (nzint & short) shouldEqual (nzint.toInt & short)
      }
      forAll { (nzint: NonZeroInt, char: Char) =>
        (nzint & char) shouldEqual (nzint.toInt & char)
      }
      forAll { (nzint: NonZeroInt, int: Int) =>
        (nzint & int) shouldEqual (nzint.toInt & int)
      }
      forAll { (nzint: NonZeroInt, long: Long) =>
        (nzint & long) shouldEqual (nzint.toInt & long)
      }
    }

    it("should offer an '^' method that is consistent with Int") {
      forAll { (nzint: NonZeroInt, byte: Byte) =>
        (nzint ^ byte) shouldEqual (nzint.toInt ^ byte)
      }
      forAll { (nzint: NonZeroInt, char: Char) =>
        (nzint ^ char) shouldEqual (nzint.toInt ^ char)
      }
      forAll { (nzint: NonZeroInt, short: Short) =>
        (nzint ^ short) shouldEqual (nzint.toInt ^ short)
      }
      forAll { (nzint: NonZeroInt, int: Int) =>
        (nzint ^ int) shouldEqual (nzint.toInt ^ int)
      }
      forAll { (nzint: NonZeroInt, long: Long) =>
        (nzint ^ long) shouldEqual (nzint.toInt ^ long)
      }
    }

    it("should offer a unary + method that is consistent with Int") {
      forAll { (p: NonZeroInt) =>
        (+p).toInt shouldEqual (+(p.toInt))
      }
    }

    it("should offer a unary - method that returns NonZeroInt") {
      forAll { (p: NonZeroInt) =>
        (-p) shouldEqual (NonZeroInt.ensuringValid(-(p.toInt)))
      }
    }

    it("should offer 'min' and 'max' methods that are consistent with Int") {
      forAll { (nzint1: NonZeroInt, nzint2: NonZeroInt) =>
        nzint1.max(nzint2).toInt shouldEqual nzint1.toInt.max(nzint2.toInt)
        nzint1.min(nzint2).toInt shouldEqual nzint1.toInt.min(nzint2.toInt)
      }
    }

    it("should offer a 'toBinaryString' method that is consistent with Int") {
      forAll { (nzint: NonZeroInt) =>
        nzint.toBinaryString shouldEqual nzint.toInt.toBinaryString
      }
    }

    it("should offer a 'toHexString' method that is consistent with Int") {
      forAll { (nzint: NonZeroInt) =>
        nzint.toHexString shouldEqual nzint.toInt.toHexString
      }
    }

    it("should offer a 'toOctalString' method that is consistent with Int") {
      forAll { (nzint: NonZeroInt) =>
        nzint.toOctalString shouldEqual nzint.toInt.toOctalString
      }
    }

    it("should offer 'to' and 'until' methods that are consistent with Int") {
      forAll { (nzint: NonZeroInt, end: Int, step: Int) =>
        // The reason we need this is that in Scala 2.10, the equals check (used by shouldEqual below) will call range.length
        // and it'll cause IllegalArgumentException to be thrown when we do the Try(x) shouldEqual Try(y) assertion below,
        // while starting from scala 2.11 the equals call implementation does not call .length.
        // To make the behavior consistent for all scala versions, we explicitly call .length for all returned Range, and
        // shall it throws IllegalArgumentException, it will be wrapped as Failure for the Try.
        def ensuringValid(range: Range): Range = {
          range.length  // IllegalArgumentException will be thrown if it is an invalid range, this will turn the Success to Failure for Try
          range
        }

        Try(ensuringValid(nzint.to(end))) shouldEqual Try(ensuringValid(nzint.toInt.to(end)))
        Try(ensuringValid(nzint.to(end, step))) shouldEqual Try(ensuringValid(nzint.toInt.to(end, step)))
        Try(ensuringValid(nzint.until(end))) shouldEqual Try(ensuringValid(nzint.toInt.until(end)))
        Try(ensuringValid(nzint.until(end, step))) shouldEqual Try(ensuringValid(nzint.toInt.until(end, step)))
      }
    }

    it("should offer an ensuringValid method that takes an Int => Int, throwing AssertionError if the result is invalid") {
      NonZeroInt(33).ensuringValid(_ + 1) shouldEqual NonZeroInt(34)
      an [AssertionError] should be thrownBy { NonZeroInt(-1).ensuringValid(_ + 1) }
    }

    it("should provide a Ordering that works for both negative and positive values") {
      NonZeroInt(-1924396667) should be <= NonZeroInt(1081481977)
    }

  }
}
