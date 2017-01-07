/*
 * Copyright 2001-2014 Artima, Inc.
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
import org.scalacheck.Gen._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.PropertyChecks
import org.scalactic.TypeCheckedTripleEquals
// SKIP-SCALATESTJS-START
import scala.collection.immutable.NumericRange
// SKIP-SCALATESTJS-END
import OptionValues._
import scala.collection.mutable.WrappedArray
//import org.scalactic.StrictCheckedEquality
import org.scalactic.Equality
import org.scalactic.{Pass, Fail}
import org.scalactic.{Good, Bad}

class PosZFloatSpec extends FunSpec with Matchers with PropertyChecks with TypeCheckedTripleEquals {

  val posZFloatGen: Gen[PosZFloat] =
    for {i <- choose(0, Float.MaxValue)} yield PosZFloat.from(i).get

  implicit val arbPosZFloat: Arbitrary[PosZFloat] = Arbitrary(posZFloatGen)

  implicit val doubleEquality: Equality[Double] =
    new Equality[Double] {
      override def areEqual(a: Double, b: Any): Boolean =
        (a, b) match {
          case (a, bDouble: Double) if a.isNaN && bDouble.isNaN  => true
          case _ => a == b
        }
    }

  implicit val floatEquality: Equality[Float] =
    new Equality[Float] {
      override def areEqual(a: Float, b: Any): Boolean =
        (a, b) match {
          case (a, bFloat: Float) if a.isNaN && bFloat.isNaN => true
          case _ => a == b
        }
    }

  describe("A PosZFloat") {
    describe("should offer a from factory method that") {
      it("returns Some[PosZFloat] if the passed Float is greater than or equal to 0") {
        PosZFloat.from(0.0f).value.value shouldBe 0.0f
        PosZFloat.from(50.23f).value.value shouldBe 50.23f
        PosZFloat.from(100.0f).value.value shouldBe 100.0f
      }
      it("returns None if the passed Float is NOT greater than or equal to 0") {
        PosZFloat.from(-0.00001f) shouldBe None
        PosZFloat.from(-99.9f) shouldBe None
      }
    } 
    describe("should offer an ensuringValid factory method that") {
      it("returns PosZFloat if the passed Float is greater than or equal to 0") {
        PosZFloat.ensuringValid(0.0f).value shouldBe 0.0f
        PosZFloat.ensuringValid(50.23f).value shouldBe 50.23f
        PosZFloat.ensuringValid(100.0f).value shouldBe 100.0f
        PosZFloat.ensuringValid(Float.PositiveInfinity).value shouldBe Float.PositiveInfinity
      }
      it("throws AssertionError if the passed Float is NOT greater than or equal to 0") {
        an [AssertionError] should be thrownBy PosZFloat.ensuringValid(-0.00001f)
        an [AssertionError] should be thrownBy PosZFloat.ensuringValid(-99.9f)
        an [AssertionError] should be thrownBy PosZFloat.ensuringValid(Float.NegativeInfinity)
        an [AssertionError] should be thrownBy PosZFloat.ensuringValid(Float.NaN)
      }
    }
    describe("should offer a tryingValid factory method that") {
      import TryValues._
      it("returns a PosZFloat wrapped in a Success if the passed Float is greater than or equal 0") {
        PosZFloat.tryingValid(0.0f).success.value.value shouldBe 0.0f
        PosZFloat.tryingValid(50.0f).success.value.value shouldBe 50.0f
        PosZFloat.tryingValid(100.0f).success.value.value shouldBe 100.0f
      }

      it("returns an AssertionError wrapped in a Failure if the passed Float is lesser than 0") {
        PosZFloat.tryingValid(-1.0f).failure.exception shouldBe an [AssertionError]
        PosZFloat.tryingValid(-99.0f).failure.exception shouldBe an [AssertionError]
      }
    }
    describe("should offer a passOrElse factory method that") {
      it("returns a Pass if the given Float is greater than or equal 0") {
        PosZFloat.passOrElse(0.0f)(i => i) shouldBe Pass
        PosZFloat.passOrElse(50.0f)(i => i) shouldBe Pass
        PosZFloat.passOrElse(100.0f)(i => i) shouldBe Pass
      }
      it("returns an error value produced by passing the given Float to the given function if the passed Float is lesser than 0, wrapped in a Fail") {
        PosZFloat.passOrElse(-1.0f)(i => i) shouldBe Fail(-1.0f)
        PosZFloat.passOrElse(-99.0f)(i => i + 3.0f) shouldBe Fail(-96.0f)
      }
    }
    describe("should offer a goodOrElse factory method that") {
      it("returns a PosZFloat wrapped in a Good if the given Float is greater than or equal 0") {
        PosZFloat.goodOrElse(0.0f)(i => i) shouldBe Good(PosZFloat(0.0f))
        PosZFloat.goodOrElse(50.0f)(i => i) shouldBe Good(PosZFloat(50.0f))
        PosZFloat.goodOrElse(100.0f)(i => i) shouldBe Good(PosZFloat(100.0f))
      }
      it("returns an error value produced by passing the given Float to the given function if the passed Float is lesser than 0, wrapped in a Bad") {
        PosZFloat.goodOrElse(-1.0f)(i => i) shouldBe Bad(-1.0f)
        PosZFloat.goodOrElse(-99.0f)(i => i + 3.0f) shouldBe Bad(-96.0f)
      }
    }
    describe("should offer a rightOrElse factory method that") {
      it("returns a PosZFloat wrapped in a Right if the given Float is greater than or equal 0") {
        PosZFloat.rightOrElse(0.0f)(i => i) shouldBe Right(PosZFloat(0.0f))
        PosZFloat.rightOrElse(50.0f)(i => i) shouldBe Right(PosZFloat(50.0f))
        PosZFloat.rightOrElse(100.0f)(i => i) shouldBe Right(PosZFloat(100.0f))
      }
      it("returns an error value produced by passing the given Float to the given function if the passed Float is lesser than 0, wrapped in a Left") {
        PosZFloat.rightOrElse(-1.0f)(i => i) shouldBe Left(-1.0f)
        PosZFloat.rightOrElse(-99.0f)(i => i + 3.0f) shouldBe Left(-96.0f)
      }
    }
    describe("should offer an isValid predicate method that") {
      it("returns true if the passed Float is greater than or equal to 0") {
        PosZFloat.isValid(50.23f) shouldBe true
        PosZFloat.isValid(100.0f) shouldBe true
        PosZFloat.isValid(0.0f) shouldBe true
        PosZFloat.isValid(-0.0f) shouldBe true
        PosZFloat.isValid(-0.00001f) shouldBe false
        PosZFloat.isValid(-99.9f) shouldBe false
      }
    } 
    describe("should offer a fromOrElse factory method that") {
      it("returns a PosZFloat if the passed Float is greater than or equal to 0") {
        PosZFloat.fromOrElse(50.23f, PosZFloat(42.0f)).value shouldBe 50.23f
        PosZFloat.fromOrElse(100.0f, PosZFloat(42.0f)).value shouldBe 100.0f
        PosZFloat.fromOrElse(0.0f, PosZFloat(42.0f)).value shouldBe 0.0f
      }
      it("returns a given default if the passed Float is NOT greater than or equal to 0") {
        PosZFloat.fromOrElse(-0.00001f, PosZFloat(42.0f)).value shouldBe 42.0f
        PosZFloat.fromOrElse(-99.9f, PosZFloat(42.0f)).value shouldBe 42.0f
      }
    } 
    it("should offer MaxValue and MinValue factory methods") {
      PosZFloat.MaxValue shouldEqual PosZFloat.from(Float.MaxValue).get
      PosZFloat.MinValue shouldEqual PosZFloat(0.0f)
    }
    it("should offer a PositiveInfinity factory method") {
      PosZFloat.PositiveInfinity shouldEqual PosZFloat.ensuringValid(Float.PositiveInfinity)
    }
    it("should have a pretty toString") {
      // SKIP-SCALATESTJS-START
      PosZFloat.from(42.0f).value.toString shouldBe "PosZFloat(42.0)"
      // SKIP-SCALATESTJS-END
      //SCALATESTJS-ONLY PosZFloat.from(42.0f).value.toString shouldBe "PosZFloat(42)"
    }
    it("should return the same type from its unary_+ method") {
      +PosZFloat(3.0F) shouldEqual PosZFloat(3.0F)
    } 
    it("should be automatically widened to compatible AnyVal targets") {
      "PosZFloat(3.0F): Int" shouldNot typeCheck
      "PosZFloat(3.0F): Long" shouldNot typeCheck
      (PosZFloat(3.0F): Float) shouldEqual 3.0F
      (PosZFloat(3.0F): Double) shouldEqual 3.0

      "PosZFloat(3.0F): PosInt" shouldNot typeCheck
      "PosZFloat(3.0F): PosLong" shouldNot typeCheck
      "PosZFloat(3.0F): PosFloat" shouldNot typeCheck
      "PosZFloat(3.0F): PosDouble" shouldNot typeCheck

      "PosZFloat(3.0F): PosZInt" shouldNot typeCheck
      "PosZFloat(3.0F): PosZLong" shouldNot typeCheck
      (PosZFloat(3.0F): PosZFloat) shouldEqual PosZFloat(3.0F)
      (PosZFloat(3.0F): PosZDouble) shouldEqual PosZDouble(3.0)

      "PosZFloat(3.0F): NonZeroInt" shouldNot typeCheck
      "PosZFloat(3.0F): NonZeroLong" shouldNot typeCheck
      "PosZFloat(3.0F): NonZeroFloat" shouldNot typeCheck
      "PosZFloat(3.0F): NonZeroDouble" shouldNot typeCheck
    }

    it("should be sortable") {
      val xs = List(PosZFloat(2.2F), PosZFloat(0.0F), PosZFloat(1.1F),
                    PosZFloat(3.3F))
      xs.sorted shouldEqual List(PosZFloat(0.0F), PosZFloat(1.1F),
                                 PosZFloat(2.2F), PosZFloat(3.3F))
    }

    describe("when a compatible AnyVal is passed to a + method invoked on it") {
      it("should give the same AnyVal type back at compile time, and correct value at runtime") {
        // When adding a "primitive"
        val opInt = PosZFloat(3.0F) + 3
        opInt shouldEqual 6.0F

        val opLong = PosZFloat(3.0F) + 3L
        opLong shouldEqual 6.0F

        val opFloat = PosZFloat(3.0F) + 3.0F
        opFloat shouldEqual 6.0F

        val opDouble = PosZFloat(3.0F) + 3.0
        opDouble shouldEqual 6.0

        // When adding a Pos*
        val opPosInt = PosZFloat(3.0F) + PosInt(3)
        opPosInt shouldEqual 6.0F

        val opPosLong = PosZFloat(3.0F) + PosLong(3L)
        opPosLong shouldEqual 6.0F

        val opPosFloat = PosZFloat(3.0F) + PosFloat(3.0F)
        opPosFloat shouldEqual 6.0F

        val opPosDouble = PosZFloat(3.0F) + PosDouble(3.0)
        opPosDouble shouldEqual 6.0

        // When adding a *PosZ
        val opPosZ = PosZFloat(3.0F) + PosZInt(3)
        opPosZ shouldEqual 6.0F

        val opPosZLong = PosZFloat(3.0F) + PosZLong(3L)
        opPosZLong shouldEqual 6.0F

        val opPosZFloat = PosZFloat(3.0F) + PosZFloat(3.0F)
        opPosZFloat shouldEqual 6.0F

        val opPosZDouble = PosZFloat(3.0F) + PosZDouble(3.0)
        opPosZDouble shouldEqual 6.0

        // When adding a *NonZero
        val opNonZero = PosZFloat(3.0F) + NonZeroInt(3)
        opNonZero shouldEqual 6.0F

        val opNonZeroLong = PosZFloat(3.0F) + NonZeroLong(3L)
        opNonZeroLong shouldEqual 6.0F

        val opNonZeroFloat = PosZFloat(3.0F) + NonZeroFloat(3.0F)
        opNonZeroFloat shouldEqual 6.0F

        val opNonZeroDouble = PosZFloat(3.0F) + NonZeroDouble(3.0)
        opNonZeroDouble shouldEqual 6.0
      }
    }

    describe("when created with apply method") {
  
      it("should compile when 8 is passed in") {
        "PosZFloat(8)" should compile
        PosZFloat(8).value shouldEqual 8.0F
        "PosZFloat(8L)" should compile
        PosZFloat(8L).value shouldEqual 8.0F
        "PosZFloat(8.0F)" should compile
        PosZFloat(8.0F).value shouldEqual 8.0F
      }
  
      it("should compile when 0 is passed in") {
        "PosZFloat(0)" should compile
        PosZFloat(0).value shouldEqual 0.0F
        "PosZFloat(0L)" should compile
        PosZFloat(0L).value shouldEqual 0.0F
        "PosZFloat(0.0F)" should compile
        PosZFloat(0.0F).value shouldEqual 0.0F
      }

      it("should not compile when -8 is passed in") {
        "PosZFloat(-8)" shouldNot compile
        "PosZFloat(-8L)" shouldNot compile
        "PosZFloat(-8.0F)" shouldNot compile
      }

      it("should not compile when x is passed in") {
        val a: Int = -8
        "PosZFloat(a)" shouldNot compile
        val b: Long = -8L
        "PosZFloat(b)" shouldNot compile
        val c: Float = -8.0F
        "PosZFloat(c)" shouldNot compile
      }
    }
    describe("when specified as a plain-old Float") {

      def takesPosZFloat(pos: PosZFloat): Float = pos.value

      it("should compile when 8 is passed in") {
        "takesPosZFloat(8)" should compile
        takesPosZFloat(8) shouldEqual 8.0F
        "takesPosZFloat(8L)" should compile
        takesPosZFloat(8L) shouldEqual 8.0F
        "takesPosZFloat(8.0F)" should compile
        takesPosZFloat(8.0F) shouldEqual 8.0F
      }

      it("should compile when 0 is passed in") {
        "takesPosZFloat(0)" should compile
        takesPosZFloat(0) shouldEqual 0.0F
        "takesPosZFloat(0L)" should compile
        takesPosZFloat(0L) shouldEqual 0.0F
        "takesPosZFloat(0.0F)" should compile
        takesPosZFloat(0.0F) shouldEqual 0.0F
      }

      it("should not compile when -8 is passed in") {
        "takesPosZFloat(-8)" shouldNot compile
        "takesPosZFloat(-8L)" shouldNot compile
        "takesPosZFloat(-8.0F)" shouldNot compile
      }

      it("should not compile when x is passed in") {
        val x: Int = -8
        "takesPosZFloat(x)" shouldNot compile
        val b: Long = -8L
        "takesPosZFloat(b)" shouldNot compile
        val c: Float = -8.0F
        "takesPosZFloat(c)" shouldNot compile
      }
    }

    it("should offer a unary + method that is consistent with Float") {
      forAll { (pzfloat: PosZFloat) =>
        (+pzfloat).toFloat shouldEqual (+(pzfloat.toFloat))
      }
    }

    it("should offer a unary - method that is consistent with Float") {
      forAll { (pzfloat: PosZFloat) =>
        (-pzfloat) shouldEqual (-(pzfloat.toFloat))
      }
    }

    it("should offer '<' comparison that is consistent with Float") {
      forAll { (pzfloat: PosZFloat, byte: Byte) =>
        (pzfloat < byte) shouldEqual (pzfloat.toFloat < byte)
      }
      forAll { (pzfloat: PosZFloat, short: Short) =>
        (pzfloat < short) shouldEqual (pzfloat.toFloat < short)
      }
      forAll { (pzfloat: PosZFloat, char: Char) =>
        (pzfloat < char) shouldEqual (pzfloat.toFloat < char)
      }
      forAll { (pzfloat: PosZFloat, int: Int) =>
        (pzfloat < int) shouldEqual (pzfloat.toFloat < int)
      }
      forAll { (pzfloat: PosZFloat, long: Long) =>
        (pzfloat < long) shouldEqual (pzfloat.toFloat < long)
      }
      forAll { (pzfloat: PosZFloat, float: Float) =>
        (pzfloat < float) shouldEqual (pzfloat.toFloat < float)
      }
      forAll { (pzfloat: PosZFloat, double: Double) =>
        (pzfloat < double) shouldEqual (pzfloat.toFloat < double)
      }
    }

    it("should offer '<=' comparison that is consistent with Float") {
      forAll { (pzfloat: PosZFloat, byte: Byte) =>
        (pzfloat <= byte) shouldEqual (pzfloat.toFloat <= byte)
      }
      forAll { (pzfloat: PosZFloat, char: Char) =>
        (pzfloat <= char) shouldEqual (pzfloat.toFloat <= char)
      }
      forAll { (pzfloat: PosZFloat, short: Short) =>
        (pzfloat <= short) shouldEqual (pzfloat.toFloat <= short)
      }
      forAll { (pzfloat: PosZFloat, int: Int) =>
        (pzfloat <= int) shouldEqual (pzfloat.toFloat <= int)
      }
      forAll { (pzfloat: PosZFloat, long: Long) =>
        (pzfloat <= long) shouldEqual (pzfloat.toFloat <= long)
      }
      forAll { (pzfloat: PosZFloat, float: Float) =>
        (pzfloat <= float) shouldEqual (pzfloat.toFloat <= float)
      }
      forAll { (pzfloat: PosZFloat, double: Double) =>
        (pzfloat <= double) shouldEqual (pzfloat.toFloat <= double)
      }
    }

    it("should offer '>' comparison that is consistent with Float") {
      forAll { (pzfloat: PosZFloat, byte: Byte) =>
        (pzfloat > byte) shouldEqual (pzfloat.toFloat > byte)
      }
      forAll { (pzfloat: PosZFloat, short: Short) =>
        (pzfloat > short) shouldEqual (pzfloat.toFloat > short)
      }
      forAll { (pzfloat: PosZFloat, char: Char) =>
        (pzfloat > char) shouldEqual (pzfloat.toFloat > char)
      }
      forAll { (pzfloat: PosZFloat, int: Int) =>
        (pzfloat > int) shouldEqual (pzfloat.toFloat > int)
      }
      forAll { (pzfloat: PosZFloat, long: Long) =>
        (pzfloat > long) shouldEqual (pzfloat.toFloat > long)
      }
      forAll { (pzfloat: PosZFloat, float: Float) =>
        (pzfloat > float) shouldEqual (pzfloat.toFloat > float)
      }
      forAll { (pzfloat: PosZFloat, double: Double) =>
        (pzfloat > double) shouldEqual (pzfloat.toFloat > double)
      }
    }

    it("should offer '>=' comparison that is consistent with Float") {
      forAll { (pzfloat: PosZFloat, byte: Byte) =>
        (pzfloat >= byte) shouldEqual (pzfloat.toFloat >= byte)
      }
      forAll { (pzfloat: PosZFloat, short: Short) =>
        (pzfloat >= short) shouldEqual (pzfloat.toFloat >= short)
      }
      forAll { (pzfloat: PosZFloat, char: Char) =>
        (pzfloat >= char) shouldEqual (pzfloat.toFloat >= char)
      }
      forAll { (pzfloat: PosZFloat, int: Int) =>
        (pzfloat >= int) shouldEqual (pzfloat.toFloat >= int)
      }
      forAll { (pzfloat: PosZFloat, long: Long) =>
        (pzfloat >= long) shouldEqual (pzfloat.toFloat >= long)
      }
      forAll { (pzfloat: PosZFloat, float: Float) =>
        (pzfloat >= float) shouldEqual (pzfloat.toFloat >= float)
      }
      forAll { (pzfloat: PosZFloat, double: Double) =>
        (pzfloat >= double) shouldEqual (pzfloat.toFloat >= double)
      }
    }

    it("should offer a '+' method that is consistent with Float") {
      forAll { (pzfloat: PosZFloat, byte: Byte) =>
        (pzfloat + byte) shouldEqual (pzfloat.toFloat + byte)
      }
      forAll { (pzfloat: PosZFloat, short: Short) =>
        (pzfloat + short) shouldEqual (pzfloat.toFloat + short)
      }
      forAll { (pzfloat: PosZFloat, char: Char) =>
        (pzfloat + char) shouldEqual (pzfloat.toFloat + char)
      }
      forAll { (pzfloat: PosZFloat, int: Int) =>
        (pzfloat + int) shouldEqual (pzfloat.toFloat + int)
      }
      forAll { (pzfloat: PosZFloat, long: Long) =>
        (pzfloat + long) shouldEqual (pzfloat.toFloat + long)
      }
      forAll { (pzfloat: PosZFloat, float: Float) =>
        (pzfloat + float) shouldEqual (pzfloat.toFloat + float)
      }
      forAll { (pzfloat: PosZFloat, double: Double) =>
        (pzfloat + double) shouldEqual (pzfloat.toFloat + double)
      }
    }

    it("should offer a 'plus' method that takes a PosZFloat and returns a PosFloat") {

      forAll { (posZFloat1: PosZFloat, posZFloat2: PosZFloat) =>
        (posZFloat1 plus posZFloat2) should === (PosZFloat.ensuringValid(posZFloat1.toFloat + posZFloat2.toFloat))
      }

      val examples =
        Table(
          (                "posZFloat1",                "posZFloat2" ),
          (         PosZFloat.MinValue,         PosZFloat.MinValue ),
          (         PosZFloat.MinValue, PosZFloat.MinPositiveValue ),
          (         PosZFloat.MinValue,         PosZFloat.MaxValue ),
          (         PosZFloat.MinValue, PosZFloat.PositiveInfinity ),
          (         PosZFloat.MaxValue,         PosZFloat.MinValue ),
          (         PosZFloat.MaxValue, PosZFloat.MinPositiveValue ),
          (         PosZFloat.MaxValue,         PosZFloat.MaxValue ),
          (         PosZFloat.MaxValue, PosZFloat.PositiveInfinity ),
          ( PosZFloat.PositiveInfinity,         PosZFloat.MinValue ),
          ( PosZFloat.PositiveInfinity, PosZFloat.MinPositiveValue ),
          ( PosZFloat.PositiveInfinity,         PosZFloat.MaxValue ),
          ( PosZFloat.PositiveInfinity, PosZFloat.PositiveInfinity )
        )

      forAll (examples) { (a, b) =>
        (a plus b).value should be >= 0.0f
      }

      // Sanity check that implicit widening conversions work too.
      // Here a PosInt gets "widened" to a PosZFloat.
      PosZFloat(1.0f) plus PosInt(2) should === (PosZFloat(3.0f))
    }

    it("should offer overloaded 'sumOf' methods on the companion that takes two or more PosZFloats and returns a PosZFloat") {

      // Run these with a relatively high minSuccessful for a while, just to see if we find a problem case.
      // Check the sumOf that takes exactly 2 args (the one that doesn't box)
      forAll (minSuccessful(1000)) { (posZFloat1: PosZFloat, posZFloat2: PosZFloat) =>
        PosZFloat.sumOf(posZFloat1, posZFloat2) should === (PosZFloat.ensuringValid(posZFloat1.value + posZFloat2.value))
      }

      // Check the sumOf that takes at least 2 args (the one that does box the var args part)
      // First just pass 2 to it and an empty list, which I wonder if that will do the other one,
      // but it doesn't matter. 
      forAll (minSuccessful(1000)) { (posZFloat1: PosZFloat, posZFloat2: PosZFloat) =>
        PosZFloat.sumOf(posZFloat1, posZFloat2, List.empty[PosZFloat]: _*) should === {
          PosZFloat.ensuringValid(posZFloat1.value + posZFloat2.value)
        }
      }
      // Then add some real lists in there
      forAll (minSuccessful(1000)) { (posZFloat1: PosZFloat, posZFloat2: PosZFloat, posZFloats: List[PosZFloat]) =>
        PosZFloat.sumOf(posZFloat1, posZFloat2, posZFloats: _*) should === {
          PosZFloat.ensuringValid(posZFloat1.value + posZFloat2.value + posZFloats.map(_.value).sum)
        }
      }

      // I want to try all combinations of edge cases in the boxing sumOf.
      // And out of an abundance of caution, all permutations of them (all the different orders)
      val posZEdgeValues = List(PosZFloat.MinValue, PosZFloat.MinPositiveValue, PosZFloat.MaxValue, PosZFloat.PositiveInfinity)
      Inspectors.forAll (posZEdgeValues.permutations.toList) { case List(a, b, c, d) =>
        PosZFloat.sumOf(a, b, c, d) should === {
          PosZFloat.ensuringValid(a.value + b.value + c.value + d.value)
        }
      }

      // Now try all combinations of 2 PosZEdgeFloats followed by both nothing and an empty varargs.
      // The idea is to test both forms with two args, though it is possible the compiler optiizes
      // the empty list (though I don't think it can tell at compile time, because I don't let it have
      // element type Nothing).
      // I get all combos by doing combinations ++ combinations.reverse. That seems to do the trick.
      val halfOfThePairs = posZEdgeValues.combinations(2).toList
      val posZPairCombos = halfOfThePairs ++ (halfOfThePairs.reverse)
      Inspectors.forAll (posZPairCombos) { case posZFloat1 :: posZFloat2 :: Nil  =>
        // Call the two-arg form
        PosZFloat.sumOf(posZFloat1, posZFloat2) should === {
          PosZFloat.ensuringValid(posZFloat1.value + posZFloat2.value)
        }
        // Most likely call the var-args form
        PosZFloat.sumOf(posZFloat1, posZFloat2, List.empty[PosZFloat]: _*) should === {
          PosZFloat.ensuringValid(posZFloat1.value + posZFloat2.value)
        }
      }

      val halfOfTheTriples = posZEdgeValues.combinations(3).toList
      val posZTripleCombos = halfOfTheTriples ++ (halfOfTheTriples.reverse)
      Inspectors.forAll (posZTripleCombos) { case posZFloat1 :: posZFloat2 :: posZFloat3 :: Nil  =>
        PosZFloat.sumOf(posZFloat1, posZFloat2, posZFloat3) should === {
          PosZFloat.ensuringValid(posZFloat1.value + posZFloat2.value + posZFloat3.value)
        }
      }
    }

    it("should offer a '-' method that is consistent with Float") {
      forAll { (pzfloat: PosZFloat, byte: Byte) =>
        (pzfloat - byte) shouldEqual (pzfloat.toFloat - byte)
      }
      forAll { (pzfloat: PosZFloat, short: Short) =>
        (pzfloat - short) shouldEqual (pzfloat.toFloat - short)
      }
      forAll { (pzfloat: PosZFloat, char: Char) =>
        (pzfloat - char) shouldEqual (pzfloat.toFloat - char)
      }
      forAll { (pzfloat: PosZFloat, int: Int) =>
        (pzfloat - int) shouldEqual (pzfloat.toFloat - int)
      }
      forAll { (pzfloat: PosZFloat, long: Long) =>
        (pzfloat - long) shouldEqual (pzfloat.toFloat - long)
      }
      forAll { (pzfloat: PosZFloat, float: Float) =>
        (pzfloat - float) shouldEqual (pzfloat.toFloat - float)
      }
      forAll { (pzfloat: PosZFloat, double: Double) =>
        (pzfloat - double) shouldEqual (pzfloat.toFloat - double)
      }
    }

    it("should offer a '*' method that is consistent with Float") {
      forAll { (pzfloat: PosZFloat, byte: Byte) =>
        (pzfloat * byte) shouldEqual (pzfloat.toFloat * byte)
      }
      forAll { (pzfloat: PosZFloat, short: Short) =>
        (pzfloat * short) shouldEqual (pzfloat.toFloat * short)
      }
      forAll { (pzfloat: PosZFloat, char: Char) =>
        (pzfloat * char) shouldEqual (pzfloat.toFloat * char)
      }
      forAll { (pzfloat: PosZFloat, int: Int) =>
        (pzfloat * int) shouldEqual (pzfloat.toFloat * int)
      }
      forAll { (pzfloat: PosZFloat, long: Long) =>
        (pzfloat * long) shouldEqual (pzfloat.toFloat * long)
      }
      forAll { (pzfloat: PosZFloat, float: Float) =>
        (pzfloat * float) shouldEqual (pzfloat.toFloat * float)
      }
      forAll { (pzfloat: PosZFloat, double: Double) =>
        (pzfloat * double) shouldEqual (pzfloat.toFloat * double)
      }
    }

    it("should offer a '/' method that is consistent with Float") {
      forAll { (pzfloat: PosZFloat, byte: Byte) =>
        pzfloat / byte shouldEqual pzfloat.toFloat / byte
      }
      forAll { (pzfloat: PosZFloat, short: Short) =>
        pzfloat / short shouldEqual pzfloat.toFloat / short
      }
      forAll { (pzfloat: PosZFloat, char: Char) =>
        pzfloat / char shouldEqual pzfloat.toFloat / char
      }
      forAll { (pzfloat: PosZFloat, int: Int) =>
        pzfloat / int shouldEqual pzfloat.toFloat / int
      }
      forAll { (pzfloat: PosZFloat, long: Long) =>
        pzfloat / long shouldEqual pzfloat.toFloat / long
      }
      forAll { (pzfloat: PosZFloat, float: Float) =>
        pzfloat / float shouldEqual pzfloat.toFloat / float
      }
      forAll { (pzfloat: PosZFloat, double: Double) =>
        pzfloat / double shouldEqual pzfloat.toFloat / double
      }
    }

    // note: since a PosInt % 0 is NaN (as opposed to PosInt / 0, which is Infinity)
    // extra logic is needed to convert to a comparable type (boolean, in this case)
    it("should offer a '%' method that is consistent with Float") {
      forAll { (pzfloat: PosZFloat, byte: Byte) =>
        val res = pzfloat % byte
        if (res.isNaN)
          (pzfloat.toFloat % byte).isNaN shouldBe true
        else
          res shouldEqual pzfloat.toFloat % byte
      }
      forAll { (pzfloat: PosZFloat, short: Short) =>
        val res = pzfloat % short
        if (res.isNaN)
          (pzfloat.toFloat % short).isNaN shouldBe true
        else
          res shouldEqual pzfloat.toFloat % short
      }
      forAll { (pzfloat: PosZFloat, char: Char) =>
        val res = pzfloat % char
        if (res.isNaN)
          (pzfloat.toFloat % char).isNaN shouldBe true
        else
          res shouldEqual pzfloat.toFloat % char
      }
      forAll { (pzfloat: PosZFloat, int: Int) =>
        val res = pzfloat % int
        if (res.isNaN)
          (pzfloat.toFloat % int).isNaN shouldBe true
        else
          res shouldEqual pzfloat.toFloat % int
      }
      forAll { (pzfloat: PosZFloat, long: Long) =>
        val res = pzfloat % long
        if (res.isNaN)
          (pzfloat.toFloat % long).isNaN shouldBe true
        else
          res shouldEqual pzfloat.toFloat % long
      }
      forAll { (pzfloat: PosZFloat, float: Float) =>
        val res = pzfloat % float
        if (res.isNaN)
          (pzfloat.toFloat % float).isNaN shouldBe true
        else
          res shouldEqual pzfloat.toFloat % float
      }
      forAll { (pzfloat: PosZFloat, double: Double) =>
        val res = pzfloat % double
        if (res.isNaN)
          (pzfloat.toFloat % double).isNaN shouldBe true
        else
          res shouldEqual pzfloat.toFloat % double
      }
    }

    it("should offer 'min' and 'max' methods that are consistent with Float") {
      forAll { (pfloat1: PosZFloat, pfloat2: PosZFloat) =>
        pfloat1.max(pfloat2).toFloat shouldEqual pfloat1.toFloat.max(pfloat2.toFloat)
        pfloat1.min(pfloat2).toFloat shouldEqual pfloat1.toFloat.min(pfloat2.toFloat)
      }
    }

    it("should offer an 'isWhole' method that is consistent with Float") {
      forAll { (pzfloat: PosZFloat) =>
        pzfloat.isWhole shouldEqual pzfloat.toFloat.isWhole
      }
    }

    it("should offer 'round', 'ceil', and 'floor' methods that are consistent with Float") {
      forAll { (pzfloat: PosZFloat) =>
        // SKIP-SCALATESTJS-START
        pzfloat.round.toFloat shouldEqual pzfloat.toFloat.round
        // SKIP-SCALATESTJS-END
        pzfloat.ceil.toFloat shouldEqual pzfloat.toFloat.ceil
        pzfloat.floor.toFloat shouldEqual pzfloat.toFloat.floor
      }
    }

    it("should offer 'toRadians' and 'toDegrees' methods that are consistent with Float") {
      forAll { (pzfloat: PosZFloat) =>
        pzfloat.toRadians.toFloat shouldEqual pzfloat.toFloat.toRadians
        pzfloat.toDegrees.toFloat shouldEqual pzfloat.toFloat.toDegrees
      }
    }

    // SKIP-SCALATESTJS-START
    it("should offer 'to' and 'until' method that is consistent with Float") {
      def rangeEqual[T](a: NumericRange[T], b: NumericRange[T]): Boolean =
        a.start == b.start && a.end == b.end && a.step == b.step

      forAll { (pzfloat: PosZFloat, end: Float, step: Float) =>
        rangeEqual(pzfloat.until(end).by(1f), pzfloat.toFloat.until(end).by(1f)) shouldBe true
        rangeEqual(pzfloat.until(end, step), pzfloat.toFloat.until(end, step)) shouldBe true
        rangeEqual(pzfloat.to(end).by(1f), pzfloat.toFloat.to(end).by(1f)) shouldBe true
        rangeEqual(pzfloat.to(end, step), pzfloat.toFloat.to(end, step)) shouldBe true
      }
    }
    // SKIP-SCALATESTJS-END

    it("should offer widening methods for basic types that are consistent with Float") {
      forAll { (pzfloat: PosZFloat) =>
        def widen(value: Float): Float = value
        widen(pzfloat) shouldEqual widen(pzfloat.toFloat)
      }
      forAll { (pzfloat: PosZFloat) =>
        def widen(value: Double): Double = value
        widen(pzfloat) shouldEqual widen(pzfloat.toFloat)
      }
      forAll { (pzfloat: PosZFloat) =>
        def widen(value: PosZDouble): PosZDouble = value
        widen(pzfloat) shouldEqual widen(PosZDouble.from(pzfloat.toFloat).get)
      }
    }
    it("should offer an ensuringValid method that takes a Float => Float, throwing AssertionError if the result is invalid") {
      PosZFloat(33.0f).ensuringValid(_ + 1.0f) shouldEqual PosZFloat(34.0f)
      PosZFloat(33.0f).ensuringValid(_ => Float.PositiveInfinity) shouldEqual PosZFloat.ensuringValid(Float.PositiveInfinity)
      an [AssertionError] should be thrownBy { PosZFloat.MaxValue.ensuringValid(_ - PosZFloat.MaxValue - 1) }
      an [AssertionError] should be thrownBy { PosFloat.MaxValue.ensuringValid(_ => Float.NegativeInfinity) }
      an [AssertionError] should be thrownBy { PosZFloat.MaxValue.ensuringValid(_ => Float.NaN) }
    }
  }
}

