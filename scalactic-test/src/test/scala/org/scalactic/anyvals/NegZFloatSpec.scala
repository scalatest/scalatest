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
import scala.util.{Try, Success, Failure}

trait NegZFloatSpecSupport {

  val negZFloatGen: Gen[NegZFloat] =
    for {i <- choose(Float.MinValue, 0.0f)} yield NegZFloat.from(i).get

  implicit val arbNegZFloat: Arbitrary[NegZFloat] = Arbitrary(negZFloatGen)

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
      case _: Success[_] => a == b
      case Failure(ex) => b match {
        case _: Success[_] => false
        case Failure(otherEx) => ex.getClass == otherEx.getClass && ex.getMessage == otherEx.getMessage
        case _ => false
      }
    }
  }

}

class NegZFloatSpec extends FunSpec with Matchers with PropertyChecks with TypeCheckedTripleEquals with NegZFloatSpecSupport {

  describe("A NegZFloat") {
    describe("should offer a from factory method that") {
      it("returns Some[NegZFloat] if the passed Float is lesser than or equal to 0") {
        NegZFloat.from(0.0f).value.value shouldBe 0.0f
        NegZFloat.from(-50.23f).value.value shouldBe -50.23f
        NegZFloat.from(-100.0f).value.value shouldBe -100.0f
      }
      it("returns None if the passed Float is NOT greater than 0") {
        NegZFloat.from(0.00001f) shouldBe None
        NegZFloat.from(99.9f) shouldBe None
      }
    }
    describe("should offer an ensuringValid factory method that") {
      it("returns NegZFloat if the passed Float is lesser than or equal to 0") {
        NegZFloat.ensuringValid(0.0f).value shouldBe 0.0f
        NegZFloat.ensuringValid(-50.23f).value shouldBe -50.23f
        NegZFloat.ensuringValid(-100.0f).value shouldBe -100.0f
        NegZFloat.ensuringValid(Float.NegativeInfinity).value shouldBe Float.NegativeInfinity
      }
      it("throws AssertionError if the passed Float is greater than 0") {
        an [AssertionError] should be thrownBy NegZFloat.ensuringValid(0.00001f)
        an [AssertionError] should be thrownBy NegZFloat.ensuringValid(99.9f)
        an [AssertionError] should be thrownBy NegZFloat.ensuringValid(Float.PositiveInfinity)
        an [AssertionError] should be thrownBy NegZFloat.ensuringValid(Float.NaN)
      }
    }
    describe("should offer a tryingValid factory method that") {
      import TryValues._
      it("returns a NegZFloat wrapped in a Success if the passed Float is lesser than or equal 0") {
        NegZFloat.tryingValid(0.0f).success.value.value shouldBe 0.0f
        NegZFloat.tryingValid(-50.0f).success.value.value shouldBe -50.0f
        NegZFloat.tryingValid(-100.0f).success.value.value shouldBe -100.0f
      }

      it("returns an AssertionError wrapped in a Failure if the passed Float is greater than 0") {
        NegZFloat.tryingValid(1.0f).failure.exception shouldBe an [AssertionError]
        NegZFloat.tryingValid(99.0f).failure.exception shouldBe an [AssertionError]
      }
    }
    describe("should offer a passOrElse factory method that") {
      it("returns a Pass if the given Float is lesser than or equal 0") {
        NegZFloat.passOrElse(0.0f)(i => i) shouldBe Pass
        NegZFloat.passOrElse(-50.0f)(i => i) shouldBe Pass
        NegZFloat.passOrElse(-100.0f)(i => i) shouldBe Pass
      }
      it("returns an error value produced by passing the given Float to the given function if the passed Float is greater than 0, wrapped in a Fail") {
        NegZFloat.passOrElse(1.0f)(i => i) shouldBe Fail(1.0f)
        NegZFloat.passOrElse(99.0f)(i => i + 3.0f) shouldBe Fail(102.0f)
      }
    }
    describe("should offer a goodOrElse factory method that") {
      it("returns a NegZFloat wrapped in a Good if the given Float is lesser than or equal 0") {
        NegZFloat.goodOrElse(0.0f)(i => i) shouldBe Good(NegZFloat(0.0f))
        NegZFloat.goodOrElse(-50.0f)(i => i) shouldBe Good(NegZFloat(-50.0f))
        NegZFloat.goodOrElse(-100.0f)(i => i) shouldBe Good(NegZFloat(-100.0f))
      }
      it("returns an error value produced by passing the given Float to the given function if the passed Float is greater than 0, wrapped in a Bad") {
        NegZFloat.goodOrElse(1.0f)(i => i) shouldBe Bad(1.0f)
        NegZFloat.goodOrElse(99.0f)(i => i + 3.0f) shouldBe Bad(102.0f)
      }
    }
    describe("should offer a rightOrElse factory method that") {
      it("returns a NegZFloat wrapped in a Right if the given Float is lesser than or equal 0") {
        NegZFloat.rightOrElse(0.0f)(i => i) shouldBe Right(NegZFloat(0.0f))
        NegZFloat.rightOrElse(-50.0f)(i => i) shouldBe Right(NegZFloat(-50.0f))
        NegZFloat.rightOrElse(-100.0f)(i => i) shouldBe Right(NegZFloat(-100.0f))
      }
      it("returns an error value produced by passing the given Float to the given function if the passed Float is greater than 0, wrapped in a Left") {
        NegZFloat.rightOrElse(1.0f)(i => i) shouldBe Left(1.0f)
        NegZFloat.rightOrElse(99.0f)(i => i + 3.0f) shouldBe Left(102.0f)
      }
    }
    describe("should offer an isValid predicate method that") {
      it("returns true if the passed Float is lesser than or equal to 0") {
        NegZFloat.isValid(-50.23f) shouldBe true
        NegZFloat.isValid(-100.0f) shouldBe true
        NegZFloat.isValid(0.0f) shouldBe true
        NegZFloat.isValid(-0.0f) shouldBe true
        NegZFloat.isValid(0.00001f) shouldBe false
        NegZFloat.isValid(99.9f) shouldBe false
      }
    }
    describe("should offer a fromOrElse factory method that") {
      it("returns a NegZFloat if the passed Float is lesser than or equal to 0") {
        NegZFloat.fromOrElse(-50.23f, NegZFloat(-42.0f)).value shouldBe -50.23f
        NegZFloat.fromOrElse(-100.0f, NegZFloat(-42.0f)).value shouldBe -100.0f
        NegZFloat.fromOrElse(0.0f, NegZFloat(-42.0f)).value shouldBe -0.0f
      }
      it("returns a given default if the passed Float is NOT greater than 0") {
        NegZFloat.fromOrElse(0.00001f, NegZFloat(-42.0f)).value shouldBe -42.0f
        NegZFloat.fromOrElse(99.9f, NegZFloat(-42.0f)).value shouldBe -42.0f
      }
    }
    it("should offer MaxValue and MinValue factory methods") {
      NegZFloat.MaxValue shouldEqual NegZFloat(0.0f)
      NegZFloat.MinValue shouldEqual NegZFloat.from(Float.MinValue).get
    }

    it("should be sortable") {
      val xs = List(NegZFloat(-2.2F), NegZFloat(-0.0F), NegZFloat(-1.1F),
        NegZFloat(-3.3F))
      xs.sorted shouldEqual List(NegZFloat(-3.3F), NegZFloat(-2.2F),
        NegZFloat(-1.1F), NegZFloat(0.0F))
    }

    describe("when created with apply method") {

      it("should compile when -8 is passed in") {
        "NegZFloat(-8)" should compile
        NegZFloat(-8).value shouldEqual -8.0F
        "NegZFloat(-8L)" should compile
        NegZFloat(-8L).value shouldEqual -8.0F
        "NegZFloat(-8.0F)" should compile
        NegZFloat(-8.0F).value shouldEqual -8.0F
      }

      it("should compile when 0 is passed in") {
        "NegZFloat(0)" should compile
        NegZFloat(0).value shouldEqual 0.0F
        "NegZFloat(0L)" should compile
        NegZFloat(0L).value shouldEqual 0.0F
        "NegZFloat(0.0F)" should compile
        NegZFloat(0.0F).value shouldEqual 0.0F
      }

      it("should not compile when 8 is passed in") {
        "NegZFloat(8)" shouldNot compile
        "NegZFloat(8L)" shouldNot compile
        "NegZFloat(8.0F)" shouldNot compile
      }

      it("should not compile when x is passed in") {
        val a: Int = -8
        "NegZFloat(a)" shouldNot compile
        val b: Long = -8L
        "NegZFloat(b)" shouldNot compile
        val c: Float = -8.0F
        "NegZFloat(c)" shouldNot compile
      }
    }
    describe("when specified as a plain-old Float") {

      def takesNegZFloat(pos: NegZFloat): Float = pos.value

      it("should compile when -8 is passed in") {
        "takesNegZFloat(-8)" should compile
        takesNegZFloat(-8) shouldEqual -8.0F
        "takesNegZFloat(-8L)" should compile
        takesNegZFloat(-8L) shouldEqual -8.0F
        "takesNegZFloat(-8.0F)" should compile
        takesNegZFloat(-8.0F) shouldEqual -8.0F
      }

      it("should compile when 0 is passed in") {
        "takesNegZFloat(0)" should compile
        takesNegZFloat(0) shouldEqual 0.0F
        "takesNegZFloat(0L)" should compile
        takesNegZFloat(0L) shouldEqual 0.0F
        "takesNegZFloat(0.0F)" should compile
        takesNegZFloat(0.0F) shouldEqual 0.0F
      }

      it("should not compile when 8 is passed in") {
        "takesNegZFloat(8)" shouldNot compile
        "takesNegZFloat(8L)" shouldNot compile
        "takesNegZFloat(8.0F)" shouldNot compile
      }

      it("should not compile when x is passed in") {
        val x: Int = -8
        "takesNegZFloat(x)" shouldNot compile
        val b: Long = -8L
        "takesNegZFloat(b)" shouldNot compile
        val c: Float = -8.0F
        "takesNegZFloat(c)" shouldNot compile
      }
    }

    it("should offer a 'plus' method that takes a NegZFloat and returns a PosFloat") {

      forAll { (negZFloat1: NegZFloat, negZFloat2: NegZFloat) =>
        (negZFloat1 plus negZFloat2) should === (NegZFloat.ensuringValid(negZFloat1.toFloat + negZFloat2.toFloat))
      }

      val examples =
        Table(
          (                "negZFloat1",                "negZFloat2" ),
          (         NegZFloat.MinValue,         NegZFloat.MinValue ),
          (         NegZFloat.MinValue,         NegZFloat.MaxValue ),
          (         NegZFloat.MinValue, NegZFloat.NegativeInfinity ),
          (         NegZFloat.MaxValue,         NegZFloat.MinValue ),
          (         NegZFloat.MaxValue,         NegZFloat.MaxValue ),
          (         NegZFloat.MaxValue, NegZFloat.NegativeInfinity ),
          ( NegZFloat.NegativeInfinity,         NegZFloat.MinValue ),
          ( NegZFloat.NegativeInfinity,         NegZFloat.MaxValue ),
          ( NegZFloat.NegativeInfinity, NegZFloat.NegativeInfinity )
        )

      forAll (examples) { (a, b) =>
        (a plus b).value should be <= 0.0f
      }

      // Sanity check that implicit widening conversions work too.
      // Here a PosInt gets "widened" to a NegZFloat.
      NegZFloat(-1.0f) plus NegInt(-2) should === (NegZFloat(-3.0f))
    }

    it("should offer overloaded 'sumOf' methods on the companion that takes two or more NegZFloats and returns a NegZFloat") {

      // Run these with a relatively high minSuccessful for a while, just to see if we find a problem case.
      // Check the sumOf that takes exactly 2 args (the one that doesn't box)
      forAll (minSuccessful(1000)) { (negZFloat1: NegZFloat, negZFloat2: NegZFloat) =>
        NegZFloat.sumOf(negZFloat1, negZFloat2) should === (NegZFloat.ensuringValid(negZFloat1.value + negZFloat2.value))
      }

      // Check the sumOf that takes at least 2 args (the one that does box the var args part)
      // First just pass 2 to it and an empty list, which I wonder if that will do the other one,
      // but it doesn't matter. 
      forAll (minSuccessful(1000)) { (negZFloat1: NegZFloat, negZFloat2: NegZFloat) =>
        NegZFloat.sumOf(negZFloat1, negZFloat2, List.empty[NegZFloat]: _*) should === {
          NegZFloat.ensuringValid(negZFloat1.value + negZFloat2.value)
        }
      }
      // Then add some real lists in there
      forAll (minSuccessful(1000)) { (negZFloat1: NegZFloat, negZFloat2: NegZFloat, negZFloats: List[NegZFloat]) =>
        NegZFloat.sumOf(negZFloat1, negZFloat2, negZFloats: _*) should === {
          NegZFloat.ensuringValid(negZFloat1.value + negZFloat2.value + negZFloats.map(_.value).sum)
        }
      }

      // I want to try all combinations of edge cases in the boxing sumOf.
      // And out of an abundance of caution, all permutations of them (all the different orders)
      val negZEdgeValues = List(NegZFloat.MinValue, NegZFloat.MaxValue, NegZFloat.NegativeInfinity)
      Inspectors.forAll (negZEdgeValues.permutations.toList) { case List(a, b, c) =>
        NegZFloat.sumOf(a, b, c) should === {
          NegZFloat.ensuringValid(a.value + b.value + c.value)
        }
      }

      // Now try all combinations of 2 NegZEdgeFloats followed by both nothing and an empty varargs.
      // The idea is to test both forms with two args, though it is possible the compiler optiizes
      // the empty list (though I don't think it can tell at compile time, because I don't let it have
      // element type Nothing).
      // I get all combos by doing combinations ++ combinations.reverse. That seems to do the trick.
      val halfOfThePairs = negZEdgeValues.combinations(2).toList
      val negZPairCombos = halfOfThePairs ++ (halfOfThePairs.reverse)
      Inspectors.forAll (negZPairCombos) { case negZFloat1 :: negZFloat2 :: Nil  =>
        // Call the two-arg form
        NegZFloat.sumOf(negZFloat1, negZFloat2) should === {
          NegZFloat.ensuringValid(negZFloat1.value + negZFloat2.value)
        }
        // Most likely call the var-args form
        NegZFloat.sumOf(negZFloat1, negZFloat2, List.empty[NegZFloat]: _*) should === {
          NegZFloat.ensuringValid(negZFloat1.value + negZFloat2.value)
        }
      }

      val halfOfTheTriples = negZEdgeValues.combinations(3).toList
      val negZTripleCombos = halfOfTheTriples ++ (halfOfTheTriples.reverse)
      Inspectors.forAll (negZTripleCombos) { case negZFloat1 :: negZFloat2 :: negZFloat3 :: Nil  =>
        NegZFloat.sumOf(negZFloat1, negZFloat2, negZFloat3) should === {
          NegZFloat.ensuringValid(negZFloat1.value + negZFloat2.value + negZFloat3.value)
        }
      }
    }

    it("should offer 'min' and 'max' methods that are consistent with Float") {
      forAll { (pfloat1: NegZFloat, pfloat2: NegZFloat) =>
        pfloat1.max(pfloat2).toFloat shouldEqual pfloat1.toFloat.max(pfloat2.toFloat)
        pfloat1.min(pfloat2).toFloat shouldEqual pfloat1.toFloat.min(pfloat2.toFloat)
      }
    }

    it("should offer an 'isWhole' method that is consistent with Float") {
      forAll { (pzfloat: NegZFloat) =>
        pzfloat.isWhole shouldEqual pzfloat.toFloat.isWhole
      }
    }

    it("should offer 'round', 'ceil', and 'floor' methods that are consistent with Float") {
      forAll { (pzfloat: NegZFloat) =>
        // SKIP-SCALATESTJS-START
        pzfloat.round.toFloat shouldEqual pzfloat.toFloat.round
        // SKIP-SCALATESTJS-END
        pzfloat.ceil.toFloat shouldEqual pzfloat.toFloat.ceil
        pzfloat.floor.toFloat shouldEqual pzfloat.toFloat.floor
      }
    }

    it("should offer 'toRadians' and 'toDegrees' methods that are consistent with Float") {
      forAll { (pzfloat: NegZFloat) =>
        pzfloat.toRadians.toFloat shouldEqual pzfloat.toFloat.toRadians
        pzfloat.toDegrees.toFloat shouldEqual pzfloat.toFloat.toDegrees
      }
    }

    // SKIP-SCALATESTJS-START
    it("should offer 'to' and 'until' method that is consistent with Float") {
      def rangeEqual[T](a: NumericRange[T], b: NumericRange[T]): Boolean =
        a.start == b.start && a.end == b.end && a.step == b.step

      forAll { (pzfloat: NegZFloat, end: Float, step: Float) =>
        rangeEqual(pzfloat.until(end).by(1f), pzfloat.toFloat.until(end).by(1f)) shouldBe true
        rangeEqual(pzfloat.until(end, step), pzfloat.toFloat.until(end, step)) shouldBe true
        rangeEqual(pzfloat.to(end).by(1f), pzfloat.toFloat.to(end).by(1f)) shouldBe true
        rangeEqual(pzfloat.to(end, step), pzfloat.toFloat.to(end, step)) shouldBe true
      }
    }
    // SKIP-SCALATESTJS-END

    it("should offer an ensuringValid method that takes a Float => Float, throwing AssertionError if the result is invalid") {
      NegZFloat(-33.0f).ensuringValid(_ + 1.0f) shouldEqual NegZFloat(-32.0f)
      NegZFloat(-33.0f).ensuringValid(_ => Float.NegativeInfinity) shouldEqual NegZFloat.ensuringValid(Float.NegativeInfinity)
      an [AssertionError] should be thrownBy { NegZFloat.MaxValue.ensuringValid(_ - NegZFloat.MaxValue + 1) }
      an [AssertionError] should be thrownBy { NegZFloat.MaxValue.ensuringValid(_ => Float.PositiveInfinity) }
      an [AssertionError] should be thrownBy { NegZFloat.MaxValue.ensuringValid(_ => Float.NaN) }
    }
  }
}

