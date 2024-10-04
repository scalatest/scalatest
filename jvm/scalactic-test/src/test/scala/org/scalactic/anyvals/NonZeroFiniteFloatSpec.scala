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
import OptionValues._
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.prop.PropertyChecks
// SKIP-SCALATESTJS,NATIVE-START
import scala.collection.immutable.NumericRange
// SKIP-SCALATESTJS,NATIVE-END
import scala.util.{Failure, Success, Try}
import org.scalactic.{Pass, Fail}
import org.scalactic.{Good, Bad}

import org.scalactic.Equality

trait NonZeroFiniteFloatSpecSupport {

  implicit def tryEquality[T]: Equality[Try[T]] = new Equality[Try[T]] {
    override def areEqual(a: Try[T], b: Any): Boolean = a match {
      // I needed this because with GenDrivenPropertyChecks, got:
      // [info] - should offer a '%' method that is consistent with Int *** FAILED ***
      // [info]   Success(NaN) did not equal Success(NaN) (PosIntExperiment.scala:498)
      case Success(float: Float) if float.isNaN =>
        b match {
          case Success(bFloat: Float) if bFloat.isNaN => true
          case _ => false
        }
      case Success(double: Double) if double.isNaN => 
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

class NonZeroFiniteFloatSpec extends funspec.AnyFunSpec with matchers.should.Matchers with PropertyChecks with TypeCheckedTripleEquals with NonZeroFiniteFloatSpecSupport {

  describe("A NonZeroFiniteFloat") {
    describe("should offer a from factory method that") {
      it("returns Some[NonZeroFiniteFloat] if the passed Float is greater than 0") {
        NonZeroFiniteFloat.from(50.23F).value.value shouldBe 50.23F
        NonZeroFiniteFloat.from(100.0F).value.value shouldBe 100.0F
      }
      it("returns Some[NonZeroFiniteFloat] if the passed Float is lesser than 0") {
        NonZeroFiniteFloat.from(-0.00001F).value.value shouldBe -0.00001F
        NonZeroFiniteFloat.from(-99.9F).value.value shouldBe -99.9F
      }
      it("returns None if the passed Float is  0") {
        NonZeroFiniteFloat.from(0.0F) shouldBe None
      }
    }
    describe("should offer an ensuringValid factory method that") {
      it("returns NonZeroFiniteFloat if the passed Float is greater than 0") {
        NonZeroFiniteFloat.ensuringValid(50.23F).value shouldBe 50.23F
        NonZeroFiniteFloat.ensuringValid(100.0F).value shouldBe 100.0F
      }
      it("returns NonZeroFiniteFloat if the passed Float is lesser than 0") {
        NonZeroFiniteFloat.ensuringValid(-0.00001F).value shouldBe -0.00001F
        NonZeroFiniteFloat.ensuringValid(-99.9F).value shouldBe -99.9F
      }
      it("throws AssertionError if the passed Float is NaN") {
        an [AssertionError] should be thrownBy NonZeroFiniteFloat.ensuringValid(Float.NaN)
      }
      it("throws AssertionError if the passed Float is 0") {
        an [AssertionError] should be thrownBy NonZeroFiniteFloat.ensuringValid(0.0F)
      }
      it("throws AssertionError if the passed Float is PositiveInfinity") {
        an [AssertionError] should be thrownBy NonZeroFiniteFloat.ensuringValid(Float.PositiveInfinity)
      }
      it("throws AssertionError if the passed Float is NegativeInfinity") {
        an [AssertionError] should be thrownBy NonZeroFiniteFloat.ensuringValid(Float.NegativeInfinity)
      }
    }
    describe("should offer a tryingValid factory method that") {
      import TryValues._
      it("returns a NonZeroFiniteFloat wrapped in a Success if the passed Float is non-zero") {
        NonZeroFiniteFloat.tryingValid(50.23F).success.value.value shouldBe 50.23F
        NonZeroFiniteFloat.tryingValid(100.0F).success.value.value shouldBe 100F
        NonZeroFiniteFloat.tryingValid(-50.23F).success.value.value shouldBe -50.23F
        NonZeroFiniteFloat.tryingValid(-100.0F).success.value.value shouldBe -100.0F
      }

      it("returns an AssertionError wrapped in a Failure if the passed Float is NOT non-zero") {
        NonZeroFiniteFloat.tryingValid(0.0F).failure.exception shouldBe an [AssertionError]
      }
    }
    describe("should offer a passOrElse factory method that") {
      it("returns a Pass if the given Float is non-zero") {
        NonZeroFiniteFloat.passOrElse(50.23F)(i => i) shouldBe Pass
        NonZeroFiniteFloat.passOrElse(100.0F)(i => i) shouldBe Pass

        NonZeroFiniteFloat.passOrElse(-1.23F)(i => i) shouldBe Pass
        NonZeroFiniteFloat.passOrElse(-99.0F)(i => i) shouldBe Pass
      }
      it("returns an error value produced by passing the given Float to the given function if the passed Float is NOT non-zero, wrapped in a Fail") {
        NonZeroFiniteFloat.passOrElse(0.0F)(i => s"$i did not taste good") shouldBe Fail(0.0F + " did not taste good")
      }
    }
    describe("should offer a goodOrElse factory method that") {
      it("returns a NonZeroFiniteFloat wrapped in a Good if the given Float is non-zero") {
        NonZeroFiniteFloat.goodOrElse(50.23F)(i => i) shouldBe Good(NonZeroFiniteFloat(50.23F))
        NonZeroFiniteFloat.goodOrElse(100.0F)(i => i) shouldBe Good(NonZeroFiniteFloat(100.0F))

        NonZeroFiniteFloat.goodOrElse(-1.23F)(i => i) shouldBe Good(NonZeroFiniteFloat(-1.23F))
        NonZeroFiniteFloat.goodOrElse(-99.0F)(i => i) shouldBe Good(NonZeroFiniteFloat(-99.0F))
      }
      it("returns an error value produced by passing the given Float to the given function if the passed Float is NOT non-zero, wrapped in a Bad") {
        NonZeroFiniteFloat.goodOrElse(0.0F)(i => s"$i did not taste good") shouldBe Bad(0.0F + " did not taste good")
      }
    }
    describe("should offer a rightOrElse factory method that") {
      it("returns a NonZeroFiniteFloat wrapped in a Right if the given Long is non-zero") {
        NonZeroFiniteFloat.rightOrElse(50.23F)(i => i) shouldBe Right(NonZeroFiniteFloat(50.23F))
        NonZeroFiniteFloat.rightOrElse(100.0F)(i => i) shouldBe Right(NonZeroFiniteFloat(100.0F))

        NonZeroFiniteFloat.rightOrElse(-1.23F)(i => i) shouldBe Right(NonZeroFiniteFloat(-1.23F))
        NonZeroFiniteFloat.rightOrElse(-99.0F)(i => i) shouldBe Right(NonZeroFiniteFloat(-99.0F))
      }
      it("returns an error value produced by passing the given Float to the given function if the passed Float is NOT non-zero, wrapped in a Left") {
        NonZeroFiniteFloat.rightOrElse(0.0F)(i => s"$i did not taste good") shouldBe Left(0.0F + " did not taste good")
      }
    }
    describe("should offer an isValid predicate method that") {
      it("returns true if the passed Float is non-zero") {
        NonZeroFiniteFloat.isValid(50.23f) shouldBe true
        NonZeroFiniteFloat.isValid(100.0f) shouldBe true
        NonZeroFiniteFloat.isValid(0.0f) shouldBe false
        NonZeroFiniteFloat.isValid(-0.0f) shouldBe false
        NonZeroFiniteFloat.isValid(-0.00001f) shouldBe true
        NonZeroFiniteFloat.isValid(-99.9f) shouldBe true
      }
    }
    describe("should offer a fromOrElse factory method that") {
      it("returns a NonZeroFiniteFloat if the passed Float is greater than 0") {
        NonZeroFiniteFloat.fromOrElse(50.23f, NonZeroFiniteFloat(42.0f)).value shouldBe 50.23f
        NonZeroFiniteFloat.fromOrElse(100.0f, NonZeroFiniteFloat(42.0f)).value shouldBe 100.0f
      }
      it("returns a NonZeroFiniteFloat if the passed Float is lesser than 0") {
        NonZeroFiniteFloat.fromOrElse(-0.00001f, NonZeroFiniteFloat(42.0f)).value shouldBe -0.00001f
        NonZeroFiniteFloat.fromOrElse(-99.9f, NonZeroFiniteFloat(42.0f)).value shouldBe -99.9f
      }
      it("returns a given default if the passed Float is 0") {
        NonZeroFiniteFloat.fromOrElse(0.0f, NonZeroFiniteFloat(42.0f)).value shouldBe 42.0f
      }
    }
    it("should offer MaxValue and MinValue factory methods") {
      NonZeroFiniteFloat.MaxValue shouldEqual NonZeroFiniteFloat.from(Float.MaxValue).get
      NonZeroFiniteFloat.MinValue shouldEqual
        NonZeroFiniteFloat.from(Float.MinValue).get
    }
    it("should not offer a PositiveInfinity factory method") {
      "NonZeroFiniteFloat.PositiveInfinity" shouldNot compile
    }
    it("should not offer a NegativeInfinity factory method") {
      "NonZeroFiniteFloat.NegativeInfinity" shouldNot compile
    }
    it("should offer a MinPositiveValue factory method") {
      NonZeroFiniteFloat.MinPositiveValue shouldEqual NonZeroFiniteFloat.ensuringValid(Float.MinPositiveValue)
    }

    it("should be sortable") {
      val xs = List(NonZeroFiniteFloat(2.2F), NonZeroFiniteFloat(4.4F), NonZeroFiniteFloat(1.1F),
        NonZeroFiniteFloat(3.3F))
      xs.sorted shouldEqual List(NonZeroFiniteFloat(1.1F), NonZeroFiniteFloat(2.2F), NonZeroFiniteFloat(3.3F),
        NonZeroFiniteFloat(4.4F))
    }

    describe("when created with apply method") {

      it("should compile when 8 is passed in") {
        "NonZeroFiniteFloat(8)" should compile
        NonZeroFiniteFloat(8).value shouldEqual 8.0F
        "NonZeroFiniteFloat(8L)" should compile
        NonZeroFiniteFloat(8L).value shouldEqual 8.0F
        "NonZeroFiniteFloat(8.0F)" should compile
        NonZeroFiniteFloat(8.0F).value shouldEqual 8.0F
      }

      it("should not compile when 0 is passed in") {
        "NonZeroFiniteFloat(0)" shouldNot compile
        "NonZeroFiniteFloat(0L)" shouldNot compile
        "NonZeroFiniteFloat(0.0F)" shouldNot compile
      }


      it("should compile when -8 is passed in") {
        "NonZeroFiniteFloat(-8)" should compile
        NonZeroFiniteFloat(-8).value shouldEqual -8.0F
        "NonZeroFiniteFloat(-8L)" should compile
        NonZeroFiniteFloat(-8L).value shouldEqual -8.0F
        "NonZeroFiniteFloat(-8.0F)" should compile
        NonZeroFiniteFloat(-8.0F).value shouldEqual -8.0F
      }

      it("should not compile when x is passed in") {
        val a: Int = -8
        "NonZeroFiniteFloat(a)" shouldNot compile
        val b: Long = -8L
        "NonZeroFiniteFloat(b)" shouldNot compile
        val c: Float = -8.0F
        "NonZeroFiniteFloat(c)" shouldNot compile
      }
    }
    describe("when specified as a plain-old Float") {

      def takesNonZeroFiniteFloat(pos: NonZeroFiniteFloat): Float = pos.value

      it("should compile when 8 is passed in") {
        "takesNonZeroFiniteFloat(8)" should compile
        takesNonZeroFiniteFloat(8) shouldEqual 8.0F
        "takesNonZeroFiniteFloat(8L)" should compile
        takesNonZeroFiniteFloat(8L) shouldEqual 8.0F
        "takesNonZeroFiniteFloat(8.0F)" should compile
        takesNonZeroFiniteFloat(8.0F) shouldEqual 8.0F
      }

      it("should not compile when 0 is passed in") {
        "takesNonZeroFiniteFloat(0)" shouldNot compile
        "takesNonZeroFiniteFloat(0L)" shouldNot compile
        "takesNonZeroFiniteFloat(0.0F)" shouldNot compile
      }

      it("should compile when -8 is passed in") {
        "takesNonZeroFiniteFloat(-8)" should compile
        takesNonZeroFiniteFloat(-8) shouldEqual -8.0F
        "takesNonZeroFiniteFloat(-8L)" should compile
        takesNonZeroFiniteFloat(-8L) shouldEqual -8.0F
        "takesNonZeroFiniteFloat(-8.0F)" should compile
        takesNonZeroFiniteFloat(-8.0F) shouldEqual -8.0F
      }

      it("should not compile when x is passed in") {
        val x: Int = -8
        "takesNonZeroFiniteFloat(x)" shouldNot compile
        val b: Long = -8L
        "takesNonZeroFiniteFloat(b)" shouldNot compile
        val c: Float = -8.0F
        "takesNonZeroFiniteFloat(c)" shouldNot compile
      }
    }

    it("should offer a unary + method that is consistent with Float") {
      forAll { (pfloat: NonZeroFiniteFloat) =>
        (+pfloat).toFloat shouldEqual (+(pfloat.toFloat))
      }
    }

    it("should offer a unary - method that returns another NonZeroFiniteFloat") {
      forAll { (pfloat: NonZeroFiniteFloat) =>
        (-pfloat) shouldEqual NonZeroFiniteFloat.ensuringValid(-(pfloat.toFloat))
      }
    }

    it("should offer '<' comparison that is consistent with Float") {
      forAll { (pfloat: NonZeroFiniteFloat, byte: Byte) =>
        (pfloat < byte) shouldEqual (pfloat.toFloat < byte)
      }
      forAll { (pfloat: NonZeroFiniteFloat, short: Short) =>
        (pfloat < short) shouldEqual (pfloat.toFloat < short)
      }
      forAll { (pfloat: NonZeroFiniteFloat, char: Char) =>
        (pfloat < char) shouldEqual (pfloat.toFloat < char)
      }
      forAll { (pfloat: NonZeroFiniteFloat, int: Int) =>
        (pfloat < int) shouldEqual (pfloat.toFloat < int)
      }
      forAll { (pfloat: NonZeroFiniteFloat, long: Long) =>
        (pfloat < long) shouldEqual (pfloat.toFloat < long)
      }
      forAll { (pfloat: NonZeroFiniteFloat, float: Float) =>
        (pfloat < float) shouldEqual (pfloat.toFloat < float)
      }
      forAll { (pfloat: NonZeroFiniteFloat, double: Double) =>
        (pfloat < double) shouldEqual (pfloat.toFloat < double)
      }
    }

    it("should offer '<=' comparison that is consistent with Float") {
      forAll { (pfloat: NonZeroFiniteFloat, byte: Byte) =>
        (pfloat <= byte) shouldEqual (pfloat.toFloat <= byte)
      }
      forAll { (pfloat: NonZeroFiniteFloat, char: Char) =>
        (pfloat <= char) shouldEqual (pfloat.toFloat <= char)
      }
      forAll { (pfloat: NonZeroFiniteFloat, short: Short) =>
        (pfloat <= short) shouldEqual (pfloat.toFloat <= short)
      }
      forAll { (pfloat: NonZeroFiniteFloat, int: Int) =>
        (pfloat <= int) shouldEqual (pfloat.toFloat <= int)
      }
      forAll { (pfloat: NonZeroFiniteFloat, long: Long) =>
        (pfloat <= long) shouldEqual (pfloat.toFloat <= long)
      }
      forAll { (pfloat: NonZeroFiniteFloat, float: Float) =>
        (pfloat <= float) shouldEqual (pfloat.toFloat <= float)
      }
      forAll { (pfloat: NonZeroFiniteFloat, double: Double) =>
        (pfloat <= double) shouldEqual (pfloat.toFloat <= double)
      }
    }

    it("should offer '>' comparison that is consistent with Float") {
      forAll { (pfloat: NonZeroFiniteFloat, byte: Byte) =>
        (pfloat > byte) shouldEqual (pfloat.toFloat > byte)
      }
      forAll { (pfloat: NonZeroFiniteFloat, short: Short) =>
        (pfloat > short) shouldEqual (pfloat.toFloat > short)
      }
      forAll { (pfloat: NonZeroFiniteFloat, char: Char) =>
        (pfloat > char) shouldEqual (pfloat.toFloat > char)
      }
      forAll { (pfloat: NonZeroFiniteFloat, int: Int) =>
        (pfloat > int) shouldEqual (pfloat.toFloat > int)
      }
      forAll { (pfloat: NonZeroFiniteFloat, long: Long) =>
        (pfloat > long) shouldEqual (pfloat.toFloat > long)
      }
      forAll { (pfloat: NonZeroFiniteFloat, float: Float) =>
        (pfloat > float) shouldEqual (pfloat.toFloat > float)
      }
      forAll { (pfloat: NonZeroFiniteFloat, double: Double) =>
        (pfloat > double) shouldEqual (pfloat.toFloat > double)
      }
    }

    it("should offer '>=' comparison that is consistent with Float") {
      forAll { (pfloat: NonZeroFiniteFloat, byte: Byte) =>
        (pfloat >= byte) shouldEqual (pfloat.toFloat >= byte)
      }
      forAll { (pfloat: NonZeroFiniteFloat, short: Short) =>
        (pfloat >= short) shouldEqual (pfloat.toFloat >= short)
      }
      forAll { (pfloat: NonZeroFiniteFloat, char: Char) =>
        (pfloat >= char) shouldEqual (pfloat.toFloat >= char)
      }
      forAll { (pfloat: NonZeroFiniteFloat, int: Int) =>
        (pfloat >= int) shouldEqual (pfloat.toFloat >= int)
      }
      forAll { (pfloat: NonZeroFiniteFloat, long: Long) =>
        (pfloat >= long) shouldEqual (pfloat.toFloat >= long)
      }
      forAll { (pfloat: NonZeroFiniteFloat, float: Float) =>
        (pfloat >= float) shouldEqual (pfloat.toFloat >= float)
      }
      forAll { (pfloat: NonZeroFiniteFloat, double: Double) =>
        (pfloat >= double) shouldEqual (pfloat.toFloat >= double)
      }
    }

    it("should offer a '+' method that is consistent with Float") {
      forAll { (pfloat: NonZeroFiniteFloat, byte: Byte) =>
        (pfloat + byte) shouldEqual (pfloat.toFloat + byte)
      }
      forAll { (pfloat: NonZeroFiniteFloat, short: Short) =>
        (pfloat + short) shouldEqual (pfloat.toFloat + short)
      }
      forAll { (pfloat: NonZeroFiniteFloat, char: Char) =>
        (pfloat + char) shouldEqual (pfloat.toFloat + char)
      }
      forAll { (pfloat: NonZeroFiniteFloat, int: Int) =>
        (pfloat + int) shouldEqual (pfloat.toFloat + int)
      }
      forAll { (pfloat: NonZeroFiniteFloat, long: Long) =>
        (pfloat + long) shouldEqual (pfloat.toFloat + long)
      }
      forAll { (pfloat: NonZeroFiniteFloat, float: Float) =>
        (pfloat + float) shouldEqual (pfloat.toFloat + float)
      }
      forAll { (pfloat: NonZeroFiniteFloat, double: Double) =>
        (pfloat + double) shouldEqual (pfloat.toFloat + double)
      }
    }

    it("should offer a '-' method that is consistent with Float") {
      forAll { (pfloat: NonZeroFiniteFloat, byte: Byte) =>
        (pfloat - byte) shouldEqual (pfloat.toFloat - byte)
      }
      forAll { (pfloat: NonZeroFiniteFloat, short: Short) =>
        (pfloat - short) shouldEqual (pfloat.toFloat - short)
      }
      forAll { (pfloat: NonZeroFiniteFloat, char: Char) =>
        (pfloat - char) shouldEqual (pfloat.toFloat - char)
      }
      forAll { (pfloat: NonZeroFiniteFloat, int: Int) =>
        (pfloat - int) shouldEqual (pfloat.toFloat - int)
      }
      forAll { (pfloat: NonZeroFiniteFloat, long: Long) =>
        (pfloat - long) shouldEqual (pfloat.toFloat - long)
      }
      forAll { (pfloat: NonZeroFiniteFloat, float: Float) =>
        (pfloat - float) shouldEqual (pfloat.toFloat - float)
      }
      forAll { (pfloat: NonZeroFiniteFloat, double: Double) =>
        (pfloat - double) shouldEqual (pfloat.toFloat - double)
      }
    }

    it("should offer a '*' method that is consistent with Float") {
      forAll { (pfloat: NonZeroFiniteFloat, byte: Byte) =>
        (pfloat * byte) shouldEqual (pfloat.toFloat * byte)
      }
      forAll { (pfloat: NonZeroFiniteFloat, short: Short) =>
        (pfloat * short) shouldEqual (pfloat.toFloat * short)
      }
      forAll { (pfloat: NonZeroFiniteFloat, char: Char) =>
        (pfloat * char) shouldEqual (pfloat.toFloat * char)
      }
      forAll { (pfloat: NonZeroFiniteFloat, int: Int) =>
        (pfloat * int) shouldEqual (pfloat.toFloat * int)
      }
      forAll { (pfloat: NonZeroFiniteFloat, long: Long) =>
        (pfloat * long) shouldEqual (pfloat.toFloat * long)
      }
      forAll { (pfloat: NonZeroFiniteFloat, float: Float) =>
        (pfloat * float) shouldEqual (pfloat.toFloat * float)
      }
      forAll { (pfloat: NonZeroFiniteFloat, double: Double) =>
        (pfloat * double) shouldEqual (pfloat.toFloat * double)
      }
    }

    it("should offer a '/' method that is consistent with Float") {
      forAll { (pfloat: NonZeroFiniteFloat, byte: Byte) =>
        pfloat / byte shouldEqual pfloat.toFloat / byte
      }
      forAll { (pfloat: NonZeroFiniteFloat, short: Short) =>
        pfloat / short shouldEqual pfloat.toFloat / short
      }
      forAll { (pfloat: NonZeroFiniteFloat, char: Char) =>
        pfloat / char shouldEqual pfloat.toFloat / char
      }
      forAll { (pfloat: NonZeroFiniteFloat, int: Int) =>
        pfloat / int shouldEqual pfloat.toFloat / int
      }
      forAll { (pfloat: NonZeroFiniteFloat, long: Long) =>
        pfloat / long shouldEqual pfloat.toFloat / long
      }
      forAll { (pfloat: NonZeroFiniteFloat, float: Float) =>
        pfloat / float shouldEqual pfloat.toFloat / float
      }
      forAll { (pfloat: NonZeroFiniteFloat, double: Double) =>
        pfloat / double shouldEqual pfloat.toFloat / double
      }
    }

    // note: since a PosInt % 0 is NaN (as opposed to PosInt / 0, which is Infinity)
    // extra logic is needed to convert to a comparable type (boolean, in this case)
    it("should offer a '%' method that is consistent with Float") {
      forAll { (pfloat: NonZeroFiniteFloat, byte: Byte) =>
        val res = pfloat % byte
        if (res.isNaN)
          (pfloat.toFloat % byte).isNaN shouldBe true
        else
          res shouldEqual pfloat.toFloat % byte
      }
      forAll { (pfloat: NonZeroFiniteFloat, short: Short) =>
        val res = pfloat % short
        if (res.isNaN)
          (pfloat.toFloat % short).isNaN shouldBe true
        else
          res shouldEqual pfloat.toFloat % short
      }
      forAll { (pfloat: NonZeroFiniteFloat, char: Char) =>
        val res = pfloat % char
        if (res.isNaN)
          (pfloat.toFloat % char).isNaN shouldBe true
        else
          res shouldEqual pfloat.toFloat % char
      }
      forAll { (pfloat: NonZeroFiniteFloat, int: Int) =>
        val res = pfloat % int
        if (res.isNaN)
          (pfloat.toFloat % int).isNaN shouldBe true
        else
          res shouldEqual pfloat.toFloat % int
      }
      forAll { (pfloat: NonZeroFiniteFloat, long: Long) =>
        val res = pfloat % long
        if (res.isNaN)
          (pfloat.toFloat % long).isNaN shouldBe true
        else
          res shouldEqual pfloat.toFloat % long
      }
      forAll { (pfloat: NonZeroFiniteFloat, float: Float) =>
        val res = pfloat % float
        if (res.isNaN)
          (pfloat.toFloat % float).isNaN shouldBe true
        else
          res shouldEqual pfloat.toFloat % float
      }
      forAll { (pfloat: NonZeroFiniteFloat, double: Double) =>
        val res = pfloat % double
        if (res.isNaN)
          (pfloat.toFloat % double).isNaN shouldBe true
        else
          res shouldEqual pfloat.toFloat % double
      }
    }

    it("should offer 'min' and 'max' methods that are consistent with Float") {
      forAll { (pfloat1: NonZeroFiniteFloat, pfloat2: NonZeroFiniteFloat) =>
        pfloat1.max(pfloat2).toFloat shouldEqual pfloat1.toFloat.max(pfloat2.toFloat)
        pfloat1.min(pfloat2).toFloat shouldEqual pfloat1.toFloat.min(pfloat2.toFloat)
      }
    }

    it("should offer an 'isWhole' method that is consistent with Float") {
      forAll { (pfloat: NonZeroFiniteFloat) =>
        pfloat.isWhole shouldEqual pfloat.toFloat.isWhole
      }
    }

    it("should offer 'toRadians' and 'toDegrees' methods that are consistent with Float") {
      forAll { (pfloat: NonZeroFiniteFloat) =>
        pfloat.toRadians shouldEqual pfloat.toFloat.toRadians
      }
    }
  }
  it("should offer an ensuringValid method that takes a Float => Float, throwing AssertionError if the result is invalid") {
    NonZeroFiniteFloat(33.0f).ensuringValid(_ + 1.0f) shouldEqual NonZeroFiniteFloat(34.0f)
    an [AssertionError] should be thrownBy { NonZeroFiniteFloat.MaxValue.ensuringValid(_ => Float.NaN) }
    an [AssertionError] should be thrownBy { NonZeroFiniteFloat.MaxValue.ensuringValid(_ => Float.PositiveInfinity) }
    an [AssertionError] should be thrownBy { NonZeroFiniteFloat.MaxValue.ensuringValid(_ => Float.NegativeInfinity) }
    an [AssertionError] should be thrownBy { NonZeroFiniteFloat.MaxValue.ensuringValid(_ - NonZeroFiniteFloat.MaxValue) }
  }
}
