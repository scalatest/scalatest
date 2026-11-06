package org.scalactic.opaquetypes

import org.scalatest._
import scala.util.{Success, Failure}
import org.scalactic.{Good, Bad, Pass, Fail}

import org.scalactic.opaquetypes.NonZeroFloats.NonZeroFiniteFloat

class NonZeroFiniteFloatSpec extends funspec.AnyFunSpec with matchers.should.Matchers with OptionValues {

  describe("NonZeroFiniteFloat") {

    describe("when created with apply method") {

      it("should compile when a valid float literal is passed in") {
        "NonZeroFiniteFloat(8.0F)" should compile
        NonZeroFiniteFloat(8.0F).value shouldEqual 8.0F
      }

      it("should not compile when 0.0F is passed in") {
        "NonZeroFiniteFloat(0.0F)" shouldNot compile
      }

      it("should compile when -8.0F is passed in") {
        "NonZeroFiniteFloat(-8.0F)" should compile
        NonZeroFiniteFloat(-8.0F).value shouldEqual -8.0F
      }

      it("should not compile when 8 or 8L is passed in due to the absence of precision-losing conversions") {
        "NonZeroFiniteFloat(8)" shouldNot compile
        "NonZeroFiniteFloat(8L)" shouldNot compile
      }

      it("should not compile when Float.PositiveInfinity or Float.NaN is passed in") {
        "NonZeroFiniteFloat(Float.PositiveInfinity)" shouldNot compile
        "NonZeroFiniteFloat(Float.NaN)" shouldNot compile
      }

      it("should not compile when x is passed in") {
        val a: Float = -8.0F
        "NonZeroFiniteFloat(a)" shouldNot compile
      }
    }

    describe("from") {
      it("should return Some(NonZeroFiniteFloat) for non-zero, finite Float values") {
        NonZeroFiniteFloat.from(3.0f).value.value shouldBe 3.0f
        NonZeroFiniteFloat.from(-8.2f).value.value shouldBe -8.2f
      }
      it("should return None for zero, infinite, and NaN values") {
        NonZeroFiniteFloat.from(0.0f) shouldBe None
        NonZeroFiniteFloat.from(Float.PositiveInfinity) shouldBe None
        NonZeroFiniteFloat.from(Float.NegativeInfinity) shouldBe None
        NonZeroFiniteFloat.from(Float.NaN) shouldBe None
      }
    }

    describe("ensuringValid") {
      it("should return the NonZeroFiniteFloat for non-zero, finite values") {
        NonZeroFiniteFloat.ensuringValid(3.0f).value shouldBe 3.0f
      }
      it("should throw AssertionError for zero, infinite, and NaN values") {
        an[AssertionError] should be thrownBy NonZeroFiniteFloat.ensuringValid(0.0f)
        an[AssertionError] should be thrownBy NonZeroFiniteFloat.ensuringValid(Float.PositiveInfinity)
        an[AssertionError] should be thrownBy NonZeroFiniteFloat.ensuringValid(Float.NegativeInfinity)
        an[AssertionError] should be thrownBy NonZeroFiniteFloat.ensuringValid(Float.NaN)
      }
    }

    describe("isValid") {
      it("should return true for non-zero, finite values") {
        NonZeroFiniteFloat.isValid(3.0f) shouldBe true
        NonZeroFiniteFloat.isValid(Float.MinValue) shouldBe true
      }
      it("should return false for zero, infinite, and NaN values") {
        NonZeroFiniteFloat.isValid(0.0f) shouldBe false
        NonZeroFiniteFloat.isValid(Float.PositiveInfinity) shouldBe false
        NonZeroFiniteFloat.isValid(Float.NegativeInfinity) shouldBe false
        NonZeroFiniteFloat.isValid(Float.NaN) shouldBe false
      }
    }

    describe("tryingValid") {
      it("should return Success for valid values") {
        NonZeroFiniteFloat.tryingValid(3.0f) shouldBe Success(NonZeroFiniteFloat.ensuringValid(3.0f))
      }
      it("should return Failure for zero, infinite, and NaN values") {
        NonZeroFiniteFloat.tryingValid(0.0f) shouldBe a[Failure[_]]
        NonZeroFiniteFloat.tryingValid(Float.PositiveInfinity) shouldBe a[Failure[_]]
        NonZeroFiniteFloat.tryingValid(Float.NaN) shouldBe a[Failure[_]]
      }
    }

    describe("Or / Validation helpers") {
      it("should return Good for valid values and Bad otherwise") {
        NonZeroFiniteFloat.goodOrElse(3.0f)(_ => "err") match {
          case Good(value) => value.value shouldBe 3.0f
          case _ => fail("expected Good")
        }
        NonZeroFiniteFloat.goodOrElse(0.0f)(_ => "err") shouldBe Bad("err")
        NonZeroFiniteFloat.goodOrElse(Float.PositiveInfinity)(_ => "err") shouldBe Bad("err")
        NonZeroFiniteFloat.goodOrElse(Float.NaN)(_ => "err") shouldBe Bad("err")
      }
      it("should return Right for valid values and Left otherwise") {
        NonZeroFiniteFloat.rightOrElse(3.0f)(_ => "err") match {
          case Right(value) => value.value shouldBe 3.0f
          case _ => fail("expected Right")
        }
        NonZeroFiniteFloat.rightOrElse(0.0f)(_ => "err") shouldBe Left("err")
        NonZeroFiniteFloat.rightOrElse(Float.PositiveInfinity)(_ => "err") shouldBe Left("err")
        NonZeroFiniteFloat.rightOrElse(Float.NaN)(_ => "err") shouldBe Left("err")
      }
      it("should return Pass for valid values and Fail otherwise") {
        NonZeroFiniteFloat.passOrElse(3.0f)(_ => "err") shouldBe Pass
        NonZeroFiniteFloat.passOrElse(0.0f)(_ => "err") shouldBe Fail("err")
        NonZeroFiniteFloat.passOrElse(Float.PositiveInfinity)(_ => "err") shouldBe Fail("err")
        NonZeroFiniteFloat.passOrElse(Float.NaN)(_ => "err") shouldBe Fail("err")
      }
      it("should return the default from fromOrElse when invalid") {
        NonZeroFiniteFloat.fromOrElse(42.0f, NonZeroFiniteFloat.ensuringValid(1.0f)).value shouldBe 42.0f
        NonZeroFiniteFloat.fromOrElse(0.0f, NonZeroFiniteFloat.ensuringValid(1.0f)).value shouldBe 1.0f
        NonZeroFiniteFloat.fromOrElse(Float.PositiveInfinity, NonZeroFiniteFloat.ensuringValid(1.0f)).value shouldBe 1.0f
        NonZeroFiniteFloat.fromOrElse(Float.NaN, NonZeroFiniteFloat.ensuringValid(1.0f)).value shouldBe 1.0f
      }
    }

    describe("ensuringValid extension") {
      it("should apply the function when the result is a valid NonZeroFiniteFloat") {
        NonZeroFiniteFloat.ensuringValid(42.0f).ensuringValid(_ * 2).value shouldBe 84.0f
        NonZeroFiniteFloat.ensuringValid(42.0f).ensuringValid(_ / 2).value shouldBe 21.0f
      }
      it("should throw AssertionError when the result is zero, infinite, or NaN") {
        an[AssertionError] should be thrownBy NonZeroFiniteFloat.ensuringValid(42.0f).ensuringValid(_ * 0)
        an[AssertionError] should be thrownBy NonZeroFiniteFloat.ensuringValid(42.0f).ensuringValid(_ => Float.PositiveInfinity)
        an[AssertionError] should be thrownBy NonZeroFiniteFloat.ensuringValid(42.0f).ensuringValid(_ => Float.NaN)
      }
    }

    describe("constants") {
      it("should expose MaxValue, MinValue and MinPositiveValue") {
        NonZeroFiniteFloat.MaxValue.value shouldBe Float.MaxValue
        NonZeroFiniteFloat.MinValue.value shouldBe Float.MinValue
        NonZeroFiniteFloat.MinPositiveValue.value shouldBe Float.MinPositiveValue
      }
    }

    it("should offer a isPosInfinity method that returns true if the instance is PositiveInfinity") {
      NonZeroFiniteFloat.ensuringValid(3.0f).isPosInfinity shouldBe false
      NonZeroFiniteFloat.MaxValue.isPosInfinity shouldBe false
    }

    it("should offer a isNegInfinity method that returns true if the instance is NegativeInfinity") {
      NonZeroFiniteFloat.ensuringValid(3.0f).isNegInfinity shouldBe false
      NonZeroFiniteFloat.MinValue.isNegInfinity shouldBe false
    }

    it("should offer an 'isWhole' method that is consistent with Float") {
      NonZeroFiniteFloat.ensuringValid(3.0f).isWhole shouldBe true
      NonZeroFiniteFloat.ensuringValid(-3.0f).isWhole shouldBe true
      NonZeroFiniteFloat.ensuringValid(2.5f).isWhole shouldBe false
    }

    it("should offer 'toRadians' and 'toDegrees' methods that are consistent with Float") {
      NonZeroFiniteFloat.ensuringValid(2.0f).toRadians shouldEqual math.toRadians(2.0f).toFloat
      NonZeroFiniteFloat.ensuringValid(2.0f).toDegrees shouldEqual math.toDegrees(2.0f).toFloat
    }

    it("should offer 'round', 'ceil', and 'floor' methods") {
      NonZeroFiniteFloat.ensuringValid(3.6f).round.value shouldBe 4L
      NonZeroFiniteFloat.ensuringValid(-3.6f).round.value shouldBe -4L
      NonZeroFiniteFloat.ensuringValid(3.6f).ceil.value shouldBe 4.0f
      NonZeroFiniteFloat.ensuringValid(-3.6f).ceil.value shouldBe -3.0f
      NonZeroFiniteFloat.ensuringValid(3.6f).floor.value shouldBe 3.0f
      NonZeroFiniteFloat.ensuringValid(-3.6f).floor.value shouldBe -4.0f
    }

    it("should offer an isFinite method that returns true if the value does not represent infinity") {
      NonZeroFiniteFloat.ensuringValid(3.0f).isFinite should be (true)
      NonZeroFiniteFloat.MaxValue.isFinite should be (true)
      NonZeroFiniteFloat.MinValue.isFinite should be (true)
    }

    it("should offer an isInfinite method that returns true if the value represents positive or negative infinity") {
      NonZeroFiniteFloat.ensuringValid(3.0f).isInfinite should be (false)
      NonZeroFiniteFloat.MaxValue.isInfinite should be (false)
      NonZeroFiniteFloat.MinValue.isInfinite should be (false)
    }

    it("should offer 'min' and 'max' methods that are consistent with Float") {
      NonZeroFiniteFloat.ensuringValid(3.0f).max(NonZeroFiniteFloat.ensuringValid(6.0f)).value shouldBe 6.0f
      NonZeroFiniteFloat.ensuringValid(3.0f).min(NonZeroFiniteFloat.ensuringValid(6.0f)).value shouldBe 3.0f
    }

    it("should return the same type from its unary_+ method") {
      +NonZeroFiniteFloat.ensuringValid(3.0f) shouldEqual NonZeroFiniteFloat.ensuringValid(3.0f)
    }

    it("should offer a unary - method that is consistent with Float") {
      (-NonZeroFiniteFloat.ensuringValid(3.0f)) shouldEqual -3.0
    }

    describe("Ordering") {
      it("should provide an Ordering that works for both negative and positive values") {
        val ord = summon[Ordering[NonZeroFiniteFloat]]
        ord.compare(NonZeroFiniteFloat.ensuringValid(-1.5f), NonZeroFiniteFloat.ensuringValid(2.5f)) should be < 0
      }

it("should be sortable") {
        val ord = summon[Ordering[NonZeroFiniteFloat]]
        val xs = List(
          NonZeroFiniteFloat.ensuringValid(2.0f),
          NonZeroFiniteFloat.ensuringValid(-1.5f),
          NonZeroFiniteFloat.ensuringValid(3.25f),
          NonZeroFiniteFloat.ensuringValid(-0.25f)
        )
        xs.sorted(ord) shouldEqual List(
          NonZeroFiniteFloat.ensuringValid(-1.5f),
          NonZeroFiniteFloat.ensuringValid(-0.25f),
          NonZeroFiniteFloat.ensuringValid(2.0f),
          NonZeroFiniteFloat.ensuringValid(3.25f)
        )
      }
    }

  }

  describe("NonZeroFiniteFloat") {
    it("should be automatically widened to compatible AnyVal targets") {
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) : Float) shouldEqual 3.0f
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) : NonZeroFloats.NonZeroFloat) shouldEqual NonZeroFloats.NonZeroFloat(3.0f)
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) : NonZeroDoubles.NonZeroDouble) shouldEqual NonZeroDoubles.NonZeroDouble(3.0)
      "(NonZeroFloats.NonZeroFiniteFloat(3.0f) : Byte)" shouldNot compile
      "(NonZeroFloats.NonZeroFiniteFloat(3.0f) : Short)" shouldNot compile
      "(NonZeroFloats.NonZeroFiniteFloat(3.0f) : Char)" shouldNot compile
      "(NonZeroFloats.NonZeroFiniteFloat(3.0f) : Int)" shouldNot compile
      "(NonZeroFloats.NonZeroFiniteFloat(3.0f) : Long)" shouldNot compile
      "(NonZeroFloats.NonZeroFiniteFloat(3.0f) : Double)" shouldNot compile
      "(NonZeroFloats.NonZeroFiniteFloat(3.0f) : PosInts.PosInt)" shouldNot compile
      "(NonZeroFloats.NonZeroFiniteFloat(3.0f) : PosInts.PosZInt)" shouldNot compile
      "(NonZeroFloats.NonZeroFiniteFloat(3.0f) : NegInts.NegInt)" shouldNot compile
      "(NonZeroFloats.NonZeroFiniteFloat(3.0f) : NegInts.NegZInt)" shouldNot compile
      "(NonZeroFloats.NonZeroFiniteFloat(3.0f) : NonZeroInts.NonZeroInt)" shouldNot compile
      "(NonZeroFloats.NonZeroFiniteFloat(3.0f) : Percentages.PercentageInt)" shouldNot compile
      "(NonZeroFloats.NonZeroFiniteFloat(3.0f) : Numerics.NumericChar)" shouldNot compile
      "(NonZeroFloats.NonZeroFiniteFloat(3.0f) : PosLongs.PosLong)" shouldNot compile
      "(NonZeroFloats.NonZeroFiniteFloat(3.0f) : PosLongs.PosZLong)" shouldNot compile
      "(NonZeroFloats.NonZeroFiniteFloat(3.0f) : NegLongs.NegLong)" shouldNot compile
      "(NonZeroFloats.NonZeroFiniteFloat(3.0f) : NegLongs.NegZLong)" shouldNot compile
      "(NonZeroFloats.NonZeroFiniteFloat(3.0f) : NonZeroLongs.NonZeroLong)" shouldNot compile
      "(NonZeroFloats.NonZeroFiniteFloat(3.0f) : PosFloats.PosFloat)" shouldNot compile
      "(NonZeroFloats.NonZeroFiniteFloat(3.0f) : PosFloats.PosZFloat)" shouldNot compile
      "(NonZeroFloats.NonZeroFiniteFloat(3.0f) : NegFloats.NegFloat)" shouldNot compile
      "(NonZeroFloats.NonZeroFiniteFloat(3.0f) : NegFloats.NegZFloat)" shouldNot compile
      "(NonZeroFloats.NonZeroFiniteFloat(3.0f) : PosFloats.PosFiniteFloat)" shouldNot compile
      "(NonZeroFloats.NonZeroFiniteFloat(3.0f) : PosFloats.PosZFiniteFloat)" shouldNot compile
      "(NonZeroFloats.NonZeroFiniteFloat(3.0f) : NegFloats.NegFiniteFloat)" shouldNot compile
      "(NonZeroFloats.NonZeroFiniteFloat(3.0f) : NegFloats.NegZFiniteFloat)" shouldNot compile
      "(NonZeroFloats.NonZeroFiniteFloat(3.0f) : Finites.FiniteFloat)" shouldNot compile
      "(NonZeroFloats.NonZeroFiniteFloat(3.0f) : PosDoubles.PosDouble)" shouldNot compile
      "(NonZeroFloats.NonZeroFiniteFloat(3.0f) : PosDoubles.PosZDouble)" shouldNot compile
      "(NonZeroFloats.NonZeroFiniteFloat(3.0f) : NegDoubles.NegDouble)" shouldNot compile
      "(NonZeroFloats.NonZeroFiniteFloat(3.0f) : NegDoubles.NegZDouble)" shouldNot compile
      "(NonZeroFloats.NonZeroFiniteFloat(3.0f) : NonZeroDoubles.NonZeroFiniteDouble)" shouldNot compile
      "(NonZeroFloats.NonZeroFiniteFloat(3.0f) : PosDoubles.PosFiniteDouble)" shouldNot compile
      "(NonZeroFloats.NonZeroFiniteFloat(3.0f) : PosDoubles.PosZFiniteDouble)" shouldNot compile
      "(NonZeroFloats.NonZeroFiniteFloat(3.0f) : NegDoubles.NegFiniteDouble)" shouldNot compile
      "(NonZeroFloats.NonZeroFiniteFloat(3.0f) : NegDoubles.NegZFiniteDouble)" shouldNot compile
      "(NonZeroFloats.NonZeroFiniteFloat(3.0f) : Finites.FiniteDouble)" shouldNot compile
    }
    it("when a compatible AnyVal is passed to a + method invoked on it should give the same AnyVal type back at compile time, and correct value at runtime") {
      NonZeroFloats.NonZeroFiniteFloat(3.0f) + 3.toByte shouldEqual 6.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) + 3.toShort shouldEqual 6.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) + '3' shouldEqual 54.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) + 3 shouldEqual 6.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) + 3L shouldEqual 6.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) + 3.0f shouldEqual 6.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) + 3.0 shouldEqual 6.0
      NonZeroFloats.NonZeroFiniteFloat(3.0f) + PosInts.PosInt(3) shouldEqual 6.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) + PosInts.PosZInt(3) shouldEqual 6.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) + NegInts.NegInt(-3) shouldEqual 0.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) + NegInts.NegZInt(-3) shouldEqual 0.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) + NonZeroInts.NonZeroInt(3) shouldEqual 6.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) + Percentages.PercentageInt(50) shouldEqual 53.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) + PosLongs.PosLong(3L) shouldEqual 6.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) + PosLongs.PosZLong(3L) shouldEqual 6.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) + NegLongs.NegLong(-3L) shouldEqual 0.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) + NegLongs.NegZLong(-3L) shouldEqual 0.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) + NonZeroLongs.NonZeroLong(3L) shouldEqual 6.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) + PosFloats.PosFloat(3.0f) shouldEqual 6.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) + PosFloats.PosZFloat(3.0f) shouldEqual 6.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) + NegFloats.NegFloat(-3.0f) shouldEqual 0.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) + NegFloats.NegZFloat(-3.0f) shouldEqual 0.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) + NonZeroFloats.NonZeroFloat(3.0f) shouldEqual 6.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) + PosFloats.PosFiniteFloat(3.0f) shouldEqual 6.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) + PosFloats.PosZFiniteFloat(3.0f) shouldEqual 6.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) + NegFloats.NegFiniteFloat(-3.0f) shouldEqual 0.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) + NegFloats.NegZFiniteFloat(-3.0f) shouldEqual 0.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) + Finites.FiniteFloat(3.0f) shouldEqual 6.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) + PosDoubles.PosDouble(3.0) shouldEqual 6.0
      NonZeroFloats.NonZeroFiniteFloat(3.0f) + PosDoubles.PosZDouble(3.0) shouldEqual 6.0
      NonZeroFloats.NonZeroFiniteFloat(3.0f) + NegDoubles.NegDouble(-3.0) shouldEqual 0.0
      NonZeroFloats.NonZeroFiniteFloat(3.0f) + NegDoubles.NegZDouble(-3.0) shouldEqual 0.0
      NonZeroFloats.NonZeroFiniteFloat(3.0f) + NonZeroDoubles.NonZeroDouble(3.0) shouldEqual 6.0
      NonZeroFloats.NonZeroFiniteFloat(3.0f) + NonZeroDoubles.NonZeroFiniteDouble(3.0) shouldEqual 6.0
      NonZeroFloats.NonZeroFiniteFloat(3.0f) + PosDoubles.PosFiniteDouble(3.0) shouldEqual 6.0
      NonZeroFloats.NonZeroFiniteFloat(3.0f) + PosDoubles.PosZFiniteDouble(3.0) shouldEqual 6.0
      NonZeroFloats.NonZeroFiniteFloat(3.0f) + NegDoubles.NegFiniteDouble(-3.0) shouldEqual 0.0
      NonZeroFloats.NonZeroFiniteFloat(3.0f) + NegDoubles.NegZFiniteDouble(-3.0) shouldEqual 0.0
      NonZeroFloats.NonZeroFiniteFloat(3.0f) + Finites.FiniteDouble(3.0) shouldEqual 6.0
    }
    it("when a compatible AnyVal is passed to a - method invoked on it should give the same AnyVal type back at compile time, and correct value at runtime") {
      NonZeroFloats.NonZeroFiniteFloat(3.0f) - 3.toByte shouldEqual 0.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) - 3.toShort shouldEqual 0.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) - '3' shouldEqual -48.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) - 3 shouldEqual 0.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) - 3L shouldEqual 0.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) - 3.0f shouldEqual 0.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) - 3.0 shouldEqual 0.0
      NonZeroFloats.NonZeroFiniteFloat(3.0f) - PosInts.PosInt(3) shouldEqual 0.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) - PosInts.PosZInt(3) shouldEqual 0.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) - NegInts.NegInt(-3) shouldEqual 6.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) - NegInts.NegZInt(-3) shouldEqual 6.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) - NonZeroInts.NonZeroInt(3) shouldEqual 0.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) - Percentages.PercentageInt(50) shouldEqual -47.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) - PosLongs.PosLong(3L) shouldEqual 0.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) - PosLongs.PosZLong(3L) shouldEqual 0.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) - NegLongs.NegLong(-3L) shouldEqual 6.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) - NegLongs.NegZLong(-3L) shouldEqual 6.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) - NonZeroLongs.NonZeroLong(3L) shouldEqual 0.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) - PosFloats.PosFloat(3.0f) shouldEqual 0.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) - PosFloats.PosZFloat(3.0f) shouldEqual 0.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) - NegFloats.NegFloat(-3.0f) shouldEqual 6.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) - NegFloats.NegZFloat(-3.0f) shouldEqual 6.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) - NonZeroFloats.NonZeroFloat(3.0f) shouldEqual 0.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) - PosFloats.PosFiniteFloat(3.0f) shouldEqual 0.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) - PosFloats.PosZFiniteFloat(3.0f) shouldEqual 0.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) - NegFloats.NegFiniteFloat(-3.0f) shouldEqual 6.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) - NegFloats.NegZFiniteFloat(-3.0f) shouldEqual 6.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) - Finites.FiniteFloat(3.0f) shouldEqual 0.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) - PosDoubles.PosDouble(3.0) shouldEqual 0.0
      NonZeroFloats.NonZeroFiniteFloat(3.0f) - PosDoubles.PosZDouble(3.0) shouldEqual 0.0
      NonZeroFloats.NonZeroFiniteFloat(3.0f) - NegDoubles.NegDouble(-3.0) shouldEqual 6.0
      NonZeroFloats.NonZeroFiniteFloat(3.0f) - NegDoubles.NegZDouble(-3.0) shouldEqual 6.0
      NonZeroFloats.NonZeroFiniteFloat(3.0f) - NonZeroDoubles.NonZeroDouble(3.0) shouldEqual 0.0
      NonZeroFloats.NonZeroFiniteFloat(3.0f) - NonZeroDoubles.NonZeroFiniteDouble(3.0) shouldEqual 0.0
      NonZeroFloats.NonZeroFiniteFloat(3.0f) - PosDoubles.PosFiniteDouble(3.0) shouldEqual 0.0
      NonZeroFloats.NonZeroFiniteFloat(3.0f) - PosDoubles.PosZFiniteDouble(3.0) shouldEqual 0.0
      NonZeroFloats.NonZeroFiniteFloat(3.0f) - NegDoubles.NegFiniteDouble(-3.0) shouldEqual 6.0
      NonZeroFloats.NonZeroFiniteFloat(3.0f) - NegDoubles.NegZFiniteDouble(-3.0) shouldEqual 6.0
      NonZeroFloats.NonZeroFiniteFloat(3.0f) - Finites.FiniteDouble(3.0) shouldEqual 0.0
    }
    it("when a compatible AnyVal is passed to a * method invoked on it should give the same AnyVal type back at compile time, and correct value at runtime") {
      NonZeroFloats.NonZeroFiniteFloat(3.0f) * 3.toByte shouldEqual 9.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) * 3.toShort shouldEqual 9.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) * '3' shouldEqual 153.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) * 3 shouldEqual 9.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) * 3L shouldEqual 9.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) * 3.0f shouldEqual 9.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) * 3.0 shouldEqual 9.0
      NonZeroFloats.NonZeroFiniteFloat(3.0f) * PosInts.PosInt(3) shouldEqual 9.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) * PosInts.PosZInt(3) shouldEqual 9.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) * NegInts.NegInt(-3) shouldEqual -9.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) * NegInts.NegZInt(-3) shouldEqual -9.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) * NonZeroInts.NonZeroInt(3) shouldEqual 9.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) * Percentages.PercentageInt(50) shouldEqual 150.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) * PosLongs.PosLong(3L) shouldEqual 9.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) * PosLongs.PosZLong(3L) shouldEqual 9.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) * NegLongs.NegLong(-3L) shouldEqual -9.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) * NegLongs.NegZLong(-3L) shouldEqual -9.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) * NonZeroLongs.NonZeroLong(3L) shouldEqual 9.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) * PosFloats.PosFloat(3.0f) shouldEqual 9.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) * PosFloats.PosZFloat(3.0f) shouldEqual 9.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) * NegFloats.NegFloat(-3.0f) shouldEqual -9.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) * NegFloats.NegZFloat(-3.0f) shouldEqual -9.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) * NonZeroFloats.NonZeroFloat(3.0f) shouldEqual 9.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) * PosFloats.PosFiniteFloat(3.0f) shouldEqual 9.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) * PosFloats.PosZFiniteFloat(3.0f) shouldEqual 9.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) * NegFloats.NegFiniteFloat(-3.0f) shouldEqual -9.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) * NegFloats.NegZFiniteFloat(-3.0f) shouldEqual -9.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) * Finites.FiniteFloat(3.0f) shouldEqual 9.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) * PosDoubles.PosDouble(3.0) shouldEqual 9.0
      NonZeroFloats.NonZeroFiniteFloat(3.0f) * PosDoubles.PosZDouble(3.0) shouldEqual 9.0
      NonZeroFloats.NonZeroFiniteFloat(3.0f) * NegDoubles.NegDouble(-3.0) shouldEqual -9.0
      NonZeroFloats.NonZeroFiniteFloat(3.0f) * NegDoubles.NegZDouble(-3.0) shouldEqual -9.0
      NonZeroFloats.NonZeroFiniteFloat(3.0f) * NonZeroDoubles.NonZeroDouble(3.0) shouldEqual 9.0
      NonZeroFloats.NonZeroFiniteFloat(3.0f) * NonZeroDoubles.NonZeroFiniteDouble(3.0) shouldEqual 9.0
      NonZeroFloats.NonZeroFiniteFloat(3.0f) * PosDoubles.PosFiniteDouble(3.0) shouldEqual 9.0
      NonZeroFloats.NonZeroFiniteFloat(3.0f) * PosDoubles.PosZFiniteDouble(3.0) shouldEqual 9.0
      NonZeroFloats.NonZeroFiniteFloat(3.0f) * NegDoubles.NegFiniteDouble(-3.0) shouldEqual -9.0
      NonZeroFloats.NonZeroFiniteFloat(3.0f) * NegDoubles.NegZFiniteDouble(-3.0) shouldEqual -9.0
      NonZeroFloats.NonZeroFiniteFloat(3.0f) * Finites.FiniteDouble(3.0) shouldEqual 9.0
    }
    it("when a compatible AnyVal is passed to a / method invoked on it should give the same AnyVal type back at compile time, and correct value at runtime") {
      NonZeroFloats.NonZeroFiniteFloat(3.0f) / 3.toByte shouldEqual 1.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) / 3.toShort shouldEqual 1.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) / '3' shouldEqual 0.058823529411764705f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) / 3 shouldEqual 1.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) / 3L shouldEqual 1.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) / 3.0f shouldEqual 1.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) / 3.0 shouldEqual 1.0
      NonZeroFloats.NonZeroFiniteFloat(3.0f) / PosInts.PosInt(3) shouldEqual 1.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) / PosInts.PosZInt(3) shouldEqual 1.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) / NegInts.NegInt(-3) shouldEqual -1.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) / NegInts.NegZInt(-3) shouldEqual -1.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) / NonZeroInts.NonZeroInt(3) shouldEqual 1.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) / Percentages.PercentageInt(50) shouldEqual 0.06f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) / PosLongs.PosLong(3L) shouldEqual 1.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) / PosLongs.PosZLong(3L) shouldEqual 1.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) / NegLongs.NegLong(-3L) shouldEqual -1.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) / NegLongs.NegZLong(-3L) shouldEqual -1.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) / NonZeroLongs.NonZeroLong(3L) shouldEqual 1.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) / PosFloats.PosFloat(3.0f) shouldEqual 1.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) / PosFloats.PosZFloat(3.0f) shouldEqual 1.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) / NegFloats.NegFloat(-3.0f) shouldEqual -1.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) / NegFloats.NegZFloat(-3.0f) shouldEqual -1.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) / NonZeroFloats.NonZeroFloat(3.0f) shouldEqual 1.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) / PosFloats.PosFiniteFloat(3.0f) shouldEqual 1.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) / PosFloats.PosZFiniteFloat(3.0f) shouldEqual 1.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) / NegFloats.NegFiniteFloat(-3.0f) shouldEqual -1.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) / NegFloats.NegZFiniteFloat(-3.0f) shouldEqual -1.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) / Finites.FiniteFloat(3.0f) shouldEqual 1.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) / PosDoubles.PosDouble(3.0) shouldEqual 1.0
      NonZeroFloats.NonZeroFiniteFloat(3.0f) / PosDoubles.PosZDouble(3.0) shouldEqual 1.0
      NonZeroFloats.NonZeroFiniteFloat(3.0f) / NegDoubles.NegDouble(-3.0) shouldEqual -1.0
      NonZeroFloats.NonZeroFiniteFloat(3.0f) / NegDoubles.NegZDouble(-3.0) shouldEqual -1.0
      NonZeroFloats.NonZeroFiniteFloat(3.0f) / NonZeroDoubles.NonZeroDouble(3.0) shouldEqual 1.0
      NonZeroFloats.NonZeroFiniteFloat(3.0f) / NonZeroDoubles.NonZeroFiniteDouble(3.0) shouldEqual 1.0
      NonZeroFloats.NonZeroFiniteFloat(3.0f) / PosDoubles.PosFiniteDouble(3.0) shouldEqual 1.0
      NonZeroFloats.NonZeroFiniteFloat(3.0f) / PosDoubles.PosZFiniteDouble(3.0) shouldEqual 1.0
      NonZeroFloats.NonZeroFiniteFloat(3.0f) / NegDoubles.NegFiniteDouble(-3.0) shouldEqual -1.0
      NonZeroFloats.NonZeroFiniteFloat(3.0f) / NegDoubles.NegZFiniteDouble(-3.0) shouldEqual -1.0
      NonZeroFloats.NonZeroFiniteFloat(3.0f) / Finites.FiniteDouble(3.0) shouldEqual 1.0
    }
    it("when a compatible AnyVal is passed to a % method invoked on it should give the same AnyVal type back at compile time, and correct value at runtime") {
      NonZeroFloats.NonZeroFiniteFloat(3.0f) % 3.toByte shouldEqual 0.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) % 3.toShort shouldEqual 0.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) % '3' shouldEqual 3.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) % 3 shouldEqual 0.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) % 3L shouldEqual 0.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) % 3.0f shouldEqual 0.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) % 3.0 shouldEqual 0.0
      NonZeroFloats.NonZeroFiniteFloat(3.0f) % PosInts.PosInt(3) shouldEqual 0.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) % PosInts.PosZInt(3) shouldEqual 0.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) % NegInts.NegInt(-3) shouldEqual 0.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) % NegInts.NegZInt(-3) shouldEqual 0.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) % NonZeroInts.NonZeroInt(3) shouldEqual 0.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) % Percentages.PercentageInt(50) shouldEqual 3.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) % PosLongs.PosLong(3L) shouldEqual 0.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) % PosLongs.PosZLong(3L) shouldEqual 0.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) % NegLongs.NegLong(-3L) shouldEqual 0.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) % NegLongs.NegZLong(-3L) shouldEqual 0.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) % NonZeroLongs.NonZeroLong(3L) shouldEqual 0.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) % PosFloats.PosFloat(3.0f) shouldEqual 0.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) % PosFloats.PosZFloat(3.0f) shouldEqual 0.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) % NegFloats.NegFloat(-3.0f) shouldEqual 0.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) % NegFloats.NegZFloat(-3.0f) shouldEqual 0.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) % NonZeroFloats.NonZeroFloat(3.0f) shouldEqual 0.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) % PosFloats.PosFiniteFloat(3.0f) shouldEqual 0.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) % PosFloats.PosZFiniteFloat(3.0f) shouldEqual 0.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) % NegFloats.NegFiniteFloat(-3.0f) shouldEqual 0.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) % NegFloats.NegZFiniteFloat(-3.0f) shouldEqual 0.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) % Finites.FiniteFloat(3.0f) shouldEqual 0.0f
      NonZeroFloats.NonZeroFiniteFloat(3.0f) % PosDoubles.PosDouble(3.0) shouldEqual 0.0
      NonZeroFloats.NonZeroFiniteFloat(3.0f) % PosDoubles.PosZDouble(3.0) shouldEqual 0.0
      NonZeroFloats.NonZeroFiniteFloat(3.0f) % NegDoubles.NegDouble(-3.0) shouldEqual 0.0
      NonZeroFloats.NonZeroFiniteFloat(3.0f) % NegDoubles.NegZDouble(-3.0) shouldEqual 0.0
      NonZeroFloats.NonZeroFiniteFloat(3.0f) % NonZeroDoubles.NonZeroDouble(3.0) shouldEqual 0.0
      NonZeroFloats.NonZeroFiniteFloat(3.0f) % NonZeroDoubles.NonZeroFiniteDouble(3.0) shouldEqual 0.0
      NonZeroFloats.NonZeroFiniteFloat(3.0f) % PosDoubles.PosFiniteDouble(3.0) shouldEqual 0.0
      NonZeroFloats.NonZeroFiniteFloat(3.0f) % PosDoubles.PosZFiniteDouble(3.0) shouldEqual 0.0
      NonZeroFloats.NonZeroFiniteFloat(3.0f) % NegDoubles.NegFiniteDouble(-3.0) shouldEqual 0.0
      NonZeroFloats.NonZeroFiniteFloat(3.0f) % NegDoubles.NegZFiniteDouble(-3.0) shouldEqual 0.0
      NonZeroFloats.NonZeroFiniteFloat(3.0f) % Finites.FiniteDouble(3.0) shouldEqual 0.0
    }
    it("should offer '<' comparison that is consistent with Float") {
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) < 3.toByte) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) < 3.toShort) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) < '3') shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) < 3) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) < 3L) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) < 3.0f) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) < 3.0) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) < PosInts.PosInt(3)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) < PosInts.PosZInt(3)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) < NegInts.NegInt(-3)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) < NegInts.NegZInt(-3)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) < NonZeroInts.NonZeroInt(3)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) < Percentages.PercentageInt(50)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) < PosLongs.PosLong(3L)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) < PosLongs.PosZLong(3L)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) < NegLongs.NegLong(-3L)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) < NegLongs.NegZLong(-3L)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) < NonZeroLongs.NonZeroLong(3L)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) < PosFloats.PosFloat(3.0f)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) < PosFloats.PosZFloat(3.0f)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) < NegFloats.NegFloat(-3.0f)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) < NegFloats.NegZFloat(-3.0f)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) < NonZeroFloats.NonZeroFloat(3.0f)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) < PosFloats.PosFiniteFloat(3.0f)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) < PosFloats.PosZFiniteFloat(3.0f)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) < NegFloats.NegFiniteFloat(-3.0f)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) < NegFloats.NegZFiniteFloat(-3.0f)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) < Finites.FiniteFloat(3.0f)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) < PosDoubles.PosDouble(3.0)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) < NegDoubles.NegDouble(-3.0)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) < NegDoubles.NegZDouble(-3.0)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) < NonZeroDoubles.NonZeroDouble(3.0)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) < NonZeroDoubles.NonZeroFiniteDouble(3.0)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) < PosDoubles.PosFiniteDouble(3.0)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) < PosDoubles.PosZFiniteDouble(3.0)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) < NegDoubles.NegFiniteDouble(-3.0)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) < NegDoubles.NegZFiniteDouble(-3.0)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) < Finites.FiniteDouble(3.0)) shouldEqual false
    }
    it("should offer '<=' comparison that is consistent with Float") {
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) <= 3.toByte) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) <= 3.toShort) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) <= '3') shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) <= 3) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) <= 3L) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) <= 3.0f) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) <= 3.0) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) <= PosInts.PosInt(3)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) <= PosInts.PosZInt(3)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) <= NegInts.NegInt(-3)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) <= NegInts.NegZInt(-3)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) <= NonZeroInts.NonZeroInt(3)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) <= Percentages.PercentageInt(50)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) <= PosLongs.PosLong(3L)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) <= PosLongs.PosZLong(3L)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) <= NegLongs.NegLong(-3L)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) <= NegLongs.NegZLong(-3L)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) <= NonZeroLongs.NonZeroLong(3L)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) <= PosFloats.PosFloat(3.0f)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) <= PosFloats.PosZFloat(3.0f)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) <= NegFloats.NegFloat(-3.0f)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) <= NegFloats.NegZFloat(-3.0f)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) <= NonZeroFloats.NonZeroFloat(3.0f)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) <= PosFloats.PosFiniteFloat(3.0f)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) <= PosFloats.PosZFiniteFloat(3.0f)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) <= NegFloats.NegFiniteFloat(-3.0f)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) <= NegFloats.NegZFiniteFloat(-3.0f)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) <= Finites.FiniteFloat(3.0f)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) <= PosDoubles.PosDouble(3.0)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) <= NegDoubles.NegDouble(-3.0)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) <= NegDoubles.NegZDouble(-3.0)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) <= NonZeroDoubles.NonZeroDouble(3.0)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) <= NonZeroDoubles.NonZeroFiniteDouble(3.0)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) <= PosDoubles.PosFiniteDouble(3.0)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) <= PosDoubles.PosZFiniteDouble(3.0)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) <= NegDoubles.NegFiniteDouble(-3.0)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) <= NegDoubles.NegZFiniteDouble(-3.0)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) <= Finites.FiniteDouble(3.0)) shouldEqual true
    }
    it("should offer '>' comparison that is consistent with Float") {
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) > 3.toByte) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) > 3.toShort) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) > '3') shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) > 3) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) > 3L) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) > 3.0f) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) > 3.0) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) > PosInts.PosInt(3)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) > PosInts.PosZInt(3)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) > NegInts.NegInt(-3)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) > NegInts.NegZInt(-3)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) > NonZeroInts.NonZeroInt(3)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) > Percentages.PercentageInt(50)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) > PosLongs.PosLong(3L)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) > PosLongs.PosZLong(3L)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) > NegLongs.NegLong(-3L)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) > NegLongs.NegZLong(-3L)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) > NonZeroLongs.NonZeroLong(3L)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) > PosFloats.PosFloat(3.0f)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) > PosFloats.PosZFloat(3.0f)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) > NegFloats.NegFloat(-3.0f)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) > NegFloats.NegZFloat(-3.0f)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) > NonZeroFloats.NonZeroFloat(3.0f)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) > PosFloats.PosFiniteFloat(3.0f)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) > PosFloats.PosZFiniteFloat(3.0f)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) > NegFloats.NegFiniteFloat(-3.0f)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) > NegFloats.NegZFiniteFloat(-3.0f)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) > Finites.FiniteFloat(3.0f)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) > PosDoubles.PosDouble(3.0)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) > NegDoubles.NegDouble(-3.0)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) > NegDoubles.NegZDouble(-3.0)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) > NonZeroDoubles.NonZeroDouble(3.0)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) > NonZeroDoubles.NonZeroFiniteDouble(3.0)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) > PosDoubles.PosFiniteDouble(3.0)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) > PosDoubles.PosZFiniteDouble(3.0)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) > NegDoubles.NegFiniteDouble(-3.0)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) > NegDoubles.NegZFiniteDouble(-3.0)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) > Finites.FiniteDouble(3.0)) shouldEqual false
    }
    it("should offer '>=' comparison that is consistent with Float") {
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) >= 3.toByte) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) >= 3.toShort) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) >= '3') shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) >= 3) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) >= 3L) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) >= 3.0f) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) >= 3.0) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) >= PosInts.PosInt(3)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) >= PosInts.PosZInt(3)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) >= NegInts.NegInt(-3)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) >= NegInts.NegZInt(-3)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) >= NonZeroInts.NonZeroInt(3)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) >= Percentages.PercentageInt(50)) shouldEqual false
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) >= PosLongs.PosLong(3L)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) >= PosLongs.PosZLong(3L)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) >= NegLongs.NegLong(-3L)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) >= NegLongs.NegZLong(-3L)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) >= NonZeroLongs.NonZeroLong(3L)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) >= PosFloats.PosFloat(3.0f)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) >= PosFloats.PosZFloat(3.0f)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) >= NegFloats.NegFloat(-3.0f)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) >= NegFloats.NegZFloat(-3.0f)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) >= NonZeroFloats.NonZeroFloat(3.0f)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) >= PosFloats.PosFiniteFloat(3.0f)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) >= PosFloats.PosZFiniteFloat(3.0f)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) >= NegFloats.NegFiniteFloat(-3.0f)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) >= NegFloats.NegZFiniteFloat(-3.0f)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) >= Finites.FiniteFloat(3.0f)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) >= PosDoubles.PosDouble(3.0)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) >= NegDoubles.NegDouble(-3.0)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) >= NegDoubles.NegZDouble(-3.0)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) >= NonZeroDoubles.NonZeroDouble(3.0)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) >= NonZeroDoubles.NonZeroFiniteDouble(3.0)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) >= PosDoubles.PosFiniteDouble(3.0)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) >= PosDoubles.PosZFiniteDouble(3.0)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) >= NegDoubles.NegFiniteDouble(-3.0)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) >= NegDoubles.NegZFiniteDouble(-3.0)) shouldEqual true
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) >= Finites.FiniteDouble(3.0)) shouldEqual true
    }
    it("should offer widening methods for basic type Float that are consistent with Float") {
      def widen(value: Float): Float = value
      widen(NonZeroFloats.NonZeroFiniteFloat(3.0f)) shouldEqual 3.0f
    }
    it("should offer widening methods for basic type NonZeroFloats.NonZeroFloat that are consistent with Float") {
      def widen(value: NonZeroFloats.NonZeroFloat): NonZeroFloats.NonZeroFloat = value
      widen(NonZeroFloats.NonZeroFiniteFloat(3.0f)) shouldEqual NonZeroFloats.NonZeroFloat(3.0f)
    }
    it("should offer widening methods for basic type NonZeroDoubles.NonZeroDouble that are consistent with Float") {
      def widen(value: NonZeroDoubles.NonZeroDouble): NonZeroDoubles.NonZeroDouble = value
      widen(NonZeroFloats.NonZeroFiniteFloat(3.0f)) shouldEqual NonZeroDoubles.NonZeroDouble(3.0)
    }
    it("should have a toString consistent with Float") {
      (NonZeroFloats.NonZeroFiniteFloat(3.0f) : Float).toString shouldBe 3.0f.toString
    }
  }
}
