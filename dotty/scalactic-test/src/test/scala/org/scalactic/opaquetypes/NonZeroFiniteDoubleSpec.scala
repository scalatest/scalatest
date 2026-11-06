package org.scalactic.opaquetypes

import org.scalatest._
import scala.util.{Success, Failure}
import org.scalactic.{Good, Bad, Pass, Fail}

import org.scalactic.opaquetypes.NonZeroDoubles.NonZeroFiniteDouble

class NonZeroFiniteDoubleSpec extends funspec.AnyFunSpec with matchers.should.Matchers with OptionValues {

  describe("NonZeroFiniteDouble") {

    describe("when created with apply method") {

      it("should compile when a valid double literal is passed in") {
        "NonZeroFiniteDouble(8.0)" should compile
        NonZeroFiniteDouble(8.0).value shouldEqual 8.0
      }

      it("should not compile when 0.0 is passed in") {
        "NonZeroFiniteDouble(0.0)" shouldNot compile
      }

      it("should compile when -8.0 is passed in") {
        "NonZeroFiniteDouble(-8.0)" should compile
        NonZeroFiniteDouble(-8.0).value shouldEqual -8.0
      }

      it("should not compile when 8, 8L, or 8.0f is passed in due to the absence of precision-losing conversions") {
        "NonZeroFiniteDouble(8)" shouldNot compile
        "NonZeroFiniteDouble(8L)" shouldNot compile
        "NonZeroFiniteDouble(8.0f)" shouldNot compile
      }

      it("should not compile when Double.PositiveInfinity or Double.NaN is passed in") {
        "NonZeroFiniteDouble(Double.PositiveInfinity)" shouldNot compile
        "NonZeroFiniteDouble(Double.NaN)" shouldNot compile
      }

      it("should not compile when x is passed in") {
        val a: Double = -8.0
        "NonZeroFiniteDouble(a)" shouldNot compile
      }
    }

    describe("from") {
      it("should return Some(NonZeroFiniteDouble) for non-zero, finite Double values") {
        NonZeroFiniteDouble.from(3.0).value.value shouldBe 3.0
        NonZeroFiniteDouble.from(-8.2).value.value shouldBe -8.2
      }
      it("should return None for zero, infinite, and NaN values") {
        NonZeroFiniteDouble.from(0.0) shouldBe None
        NonZeroFiniteDouble.from(Double.PositiveInfinity) shouldBe None
        NonZeroFiniteDouble.from(Double.NegativeInfinity) shouldBe None
        NonZeroFiniteDouble.from(Double.NaN) shouldBe None
      }
    }

    describe("ensuringValid") {
      it("should return the NonZeroFiniteDouble for non-zero, finite values") {
        NonZeroFiniteDouble.ensuringValid(3.0).value shouldBe 3.0
      }
      it("should throw AssertionError for zero, infinite, and NaN values") {
        an[AssertionError] should be thrownBy NonZeroFiniteDouble.ensuringValid(0.0)
        an[AssertionError] should be thrownBy NonZeroFiniteDouble.ensuringValid(Double.PositiveInfinity)
        an[AssertionError] should be thrownBy NonZeroFiniteDouble.ensuringValid(Double.NegativeInfinity)
        an[AssertionError] should be thrownBy NonZeroFiniteDouble.ensuringValid(Double.NaN)
      }
    }

    describe("isValid") {
      it("should return true for non-zero, finite values") {
        NonZeroFiniteDouble.isValid(3.0) shouldBe true
        NonZeroFiniteDouble.isValid(Double.MinValue) shouldBe true
      }
      it("should return false for zero, infinite, and NaN values") {
        NonZeroFiniteDouble.isValid(0.0) shouldBe false
        NonZeroFiniteDouble.isValid(Double.PositiveInfinity) shouldBe false
        NonZeroFiniteDouble.isValid(Double.NegativeInfinity) shouldBe false
        NonZeroFiniteDouble.isValid(Double.NaN) shouldBe false
      }
    }

    describe("tryingValid") {
      it("should return Success for valid values") {
        NonZeroFiniteDouble.tryingValid(3.0) shouldBe Success(NonZeroFiniteDouble.ensuringValid(3.0))
      }
      it("should return Failure for zero, infinite, and NaN values") {
        NonZeroFiniteDouble.tryingValid(0.0) shouldBe a[Failure[_]]
        NonZeroFiniteDouble.tryingValid(Double.PositiveInfinity) shouldBe a[Failure[_]]
        NonZeroFiniteDouble.tryingValid(Double.NaN) shouldBe a[Failure[_]]
      }
    }

    describe("Or / Validation helpers") {
      it("should return Good for valid values and Bad otherwise") {
        NonZeroFiniteDouble.goodOrElse(3.0)(_ => "err") match {
          case Good(value) => value.value shouldBe 3.0
          case _ => fail("expected Good")
        }
        NonZeroFiniteDouble.goodOrElse(0.0)(_ => "err") shouldBe Bad("err")
        NonZeroFiniteDouble.goodOrElse(Double.PositiveInfinity)(_ => "err") shouldBe Bad("err")
        NonZeroFiniteDouble.goodOrElse(Double.NaN)(_ => "err") shouldBe Bad("err")
      }
      it("should return Right for valid values and Left otherwise") {
        NonZeroFiniteDouble.rightOrElse(3.0)(_ => "err") match {
          case Right(value) => value.value shouldBe 3.0
          case _ => fail("expected Right")
        }
        NonZeroFiniteDouble.rightOrElse(0.0)(_ => "err") shouldBe Left("err")
        NonZeroFiniteDouble.rightOrElse(Double.PositiveInfinity)(_ => "err") shouldBe Left("err")
        NonZeroFiniteDouble.rightOrElse(Double.NaN)(_ => "err") shouldBe Left("err")
      }
      it("should return Pass for valid values and Fail otherwise") {
        NonZeroFiniteDouble.passOrElse(3.0)(_ => "err") shouldBe Pass
        NonZeroFiniteDouble.passOrElse(0.0)(_ => "err") shouldBe Fail("err")
        NonZeroFiniteDouble.passOrElse(Double.PositiveInfinity)(_ => "err") shouldBe Fail("err")
        NonZeroFiniteDouble.passOrElse(Double.NaN)(_ => "err") shouldBe Fail("err")
      }
      it("should return the default from fromOrElse when invalid") {
        NonZeroFiniteDouble.fromOrElse(42.0, NonZeroFiniteDouble.ensuringValid(1.0)).value shouldBe 42.0
        NonZeroFiniteDouble.fromOrElse(0.0, NonZeroFiniteDouble.ensuringValid(1.0)).value shouldBe 1.0
        NonZeroFiniteDouble.fromOrElse(Double.PositiveInfinity, NonZeroFiniteDouble.ensuringValid(1.0)).value shouldBe 1.0
        NonZeroFiniteDouble.fromOrElse(Double.NaN, NonZeroFiniteDouble.ensuringValid(1.0)).value shouldBe 1.0
      }
    }

    describe("ensuringValid extension") {
      it("should apply the function when the result is a valid NonZeroFiniteDouble") {
        NonZeroFiniteDouble.ensuringValid(42.0).ensuringValid(_ * 2).value shouldBe 84.0
        NonZeroFiniteDouble.ensuringValid(42.0).ensuringValid(_ / 2).value shouldBe 21.0
      }
      it("should throw AssertionError when the result is zero, infinite, or NaN") {
        an[AssertionError] should be thrownBy NonZeroFiniteDouble.ensuringValid(42.0).ensuringValid(_ * 0)
        an[AssertionError] should be thrownBy NonZeroFiniteDouble.ensuringValid(42.0).ensuringValid(_ => Double.PositiveInfinity)
        an[AssertionError] should be thrownBy NonZeroFiniteDouble.ensuringValid(42.0).ensuringValid(_ => Double.NaN)
      }
    }

    describe("constants") {
      it("should expose MaxValue, MinValue and MinPositiveValue") {
        NonZeroFiniteDouble.MaxValue.value shouldBe Double.MaxValue
        NonZeroFiniteDouble.MinValue.value shouldBe Double.MinValue
        NonZeroFiniteDouble.MinPositiveValue.value shouldBe Double.MinPositiveValue
      }
    }

    it("should offer a isPosInfinity method that returns true if the instance is PositiveInfinity") {
      NonZeroFiniteDouble.ensuringValid(3.0).isPosInfinity shouldBe false
      NonZeroFiniteDouble.MaxValue.isPosInfinity shouldBe false
    }

    it("should offer a isNegInfinity method that returns true if the instance is NegativeInfinity") {
      NonZeroFiniteDouble.ensuringValid(3.0).isNegInfinity shouldBe false
      NonZeroFiniteDouble.MinValue.isNegInfinity shouldBe false
    }

    it("should offer an 'isWhole' method that is consistent with Double") {
      NonZeroFiniteDouble.ensuringValid(3.0).isWhole shouldBe true
      NonZeroFiniteDouble.ensuringValid(-3.0).isWhole shouldBe true
      NonZeroFiniteDouble.ensuringValid(2.5).isWhole shouldBe false
    }

    it("should offer 'toRadians' and 'toDegrees' methods that are consistent with Double") {
      NonZeroFiniteDouble.ensuringValid(2.0).toRadians shouldEqual math.toRadians(2.0)
      NonZeroFiniteDouble.ensuringValid(2.0).toDegrees shouldEqual math.toDegrees(2.0)
    }

    it("should offer 'round', 'ceil', and 'floor' methods") {
      NonZeroFiniteDouble.ensuringValid(3.6).round.value shouldBe 4L
      NonZeroFiniteDouble.ensuringValid(-3.6).round.value shouldBe -4L
      NonZeroFiniteDouble.ensuringValid(3.6).ceil.value shouldBe 4.0
      NonZeroFiniteDouble.ensuringValid(-3.6).ceil.value shouldBe -3.0
      NonZeroFiniteDouble.ensuringValid(3.6).floor.value shouldBe 3.0
      NonZeroFiniteDouble.ensuringValid(-3.6).floor.value shouldBe -4.0
    }

    it("should offer an isFinite method that returns true if the value does not represent infinity") {
      NonZeroFiniteDouble.ensuringValid(3.0).isFinite should be (true)
      NonZeroFiniteDouble.MaxValue.isFinite should be (true)
      NonZeroFiniteDouble.MinValue.isFinite should be (true)
    }

    it("should offer an isInfinite method that returns true if the value represents positive or negative infinity") {
      NonZeroFiniteDouble.ensuringValid(3.0).isInfinite should be (false)
      NonZeroFiniteDouble.MaxValue.isInfinite should be (false)
      NonZeroFiniteDouble.MinValue.isInfinite should be (false)
    }

    it("should offer 'min' and 'max' methods that are consistent with Double") {
      NonZeroFiniteDouble.ensuringValid(3.0).max(NonZeroFiniteDouble.ensuringValid(6.0)).value shouldBe 6.0
      NonZeroFiniteDouble.ensuringValid(3.0).min(NonZeroFiniteDouble.ensuringValid(6.0)).value shouldBe 3.0
    }

    it("should return the same type from its unary_+ method") {
      +NonZeroFiniteDouble.ensuringValid(3.0) shouldEqual NonZeroFiniteDouble.ensuringValid(3.0)
    }

    it("should offer a unary - method that is consistent with Double") {
      (-NonZeroFiniteDouble.ensuringValid(3.0)) shouldEqual -3.0
    }

    describe("Ordering") {
      it("should provide an Ordering that works for both negative and positive values") {
        val ord = summon[Ordering[NonZeroFiniteDouble]]
        ord.compare(NonZeroFiniteDouble.ensuringValid(-1.5), NonZeroFiniteDouble.ensuringValid(2.5)) should be < 0
      }

      it("should be sortable") {
        val ord = summon[Ordering[NonZeroFiniteDouble]]
        val xs = List(
          NonZeroFiniteDouble.ensuringValid(2.0),
          NonZeroFiniteDouble.ensuringValid(-1.5),
          NonZeroFiniteDouble.ensuringValid(3.25),
          NonZeroFiniteDouble.ensuringValid(-0.25)
        )
        xs.sorted(ord) shouldEqual List(
          NonZeroFiniteDouble.ensuringValid(-1.5),
          NonZeroFiniteDouble.ensuringValid(-0.25),
          NonZeroFiniteDouble.ensuringValid(2.0),
          NonZeroFiniteDouble.ensuringValid(3.25)
        )
      }
    }

  }

  describe("NonZeroFiniteDouble") {
    it("should be automatically widened to compatible AnyVal targets") {
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) : Double) shouldEqual 3.0
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) : NonZeroDoubles.NonZeroDouble) shouldEqual NonZeroDoubles.NonZeroDouble(3.0)
      "(NonZeroDoubles.NonZeroFiniteDouble(3.0) : Byte)" shouldNot compile
      "(NonZeroDoubles.NonZeroFiniteDouble(3.0) : Short)" shouldNot compile
      "(NonZeroDoubles.NonZeroFiniteDouble(3.0) : Char)" shouldNot compile
      "(NonZeroDoubles.NonZeroFiniteDouble(3.0) : Int)" shouldNot compile
      "(NonZeroDoubles.NonZeroFiniteDouble(3.0) : Long)" shouldNot compile
      "(NonZeroDoubles.NonZeroFiniteDouble(3.0) : Float)" shouldNot compile
      "(NonZeroDoubles.NonZeroFiniteDouble(3.0) : PosInts.PosInt)" shouldNot compile
      "(NonZeroDoubles.NonZeroFiniteDouble(3.0) : PosInts.PosZInt)" shouldNot compile
      "(NonZeroDoubles.NonZeroFiniteDouble(3.0) : NegInts.NegInt)" shouldNot compile
      "(NonZeroDoubles.NonZeroFiniteDouble(3.0) : NegInts.NegZInt)" shouldNot compile
      "(NonZeroDoubles.NonZeroFiniteDouble(3.0) : NonZeroInts.NonZeroInt)" shouldNot compile
      "(NonZeroDoubles.NonZeroFiniteDouble(3.0) : Percentages.PercentageInt)" shouldNot compile
      "(NonZeroDoubles.NonZeroFiniteDouble(3.0) : Numerics.NumericChar)" shouldNot compile
      "(NonZeroDoubles.NonZeroFiniteDouble(3.0) : PosLongs.PosLong)" shouldNot compile
      "(NonZeroDoubles.NonZeroFiniteDouble(3.0) : PosLongs.PosZLong)" shouldNot compile
      "(NonZeroDoubles.NonZeroFiniteDouble(3.0) : NegLongs.NegLong)" shouldNot compile
      "(NonZeroDoubles.NonZeroFiniteDouble(3.0) : NegLongs.NegZLong)" shouldNot compile
      "(NonZeroDoubles.NonZeroFiniteDouble(3.0) : NonZeroLongs.NonZeroLong)" shouldNot compile
      "(NonZeroDoubles.NonZeroFiniteDouble(3.0) : PosFloats.PosFloat)" shouldNot compile
      "(NonZeroDoubles.NonZeroFiniteDouble(3.0) : PosFloats.PosZFloat)" shouldNot compile
      "(NonZeroDoubles.NonZeroFiniteDouble(3.0) : NegFloats.NegFloat)" shouldNot compile
      "(NonZeroDoubles.NonZeroFiniteDouble(3.0) : NegFloats.NegZFloat)" shouldNot compile
      "(NonZeroDoubles.NonZeroFiniteDouble(3.0) : NonZeroFloats.NonZeroFloat)" shouldNot compile
      "(NonZeroDoubles.NonZeroFiniteDouble(3.0) : NonZeroFloats.NonZeroFiniteFloat)" shouldNot compile
      "(NonZeroDoubles.NonZeroFiniteDouble(3.0) : PosFloats.PosFiniteFloat)" shouldNot compile
      "(NonZeroDoubles.NonZeroFiniteDouble(3.0) : PosFloats.PosZFiniteFloat)" shouldNot compile
      "(NonZeroDoubles.NonZeroFiniteDouble(3.0) : NegFloats.NegFiniteFloat)" shouldNot compile
      "(NonZeroDoubles.NonZeroFiniteDouble(3.0) : NegFloats.NegZFiniteFloat)" shouldNot compile
      "(NonZeroDoubles.NonZeroFiniteDouble(3.0) : Finites.FiniteFloat)" shouldNot compile
      "(NonZeroDoubles.NonZeroFiniteDouble(3.0) : PosDoubles.PosDouble)" shouldNot compile
      "(NonZeroDoubles.NonZeroFiniteDouble(3.0) : PosDoubles.PosZDouble)" shouldNot compile
      "(NonZeroDoubles.NonZeroFiniteDouble(3.0) : NegDoubles.NegDouble)" shouldNot compile
      "(NonZeroDoubles.NonZeroFiniteDouble(3.0) : NegDoubles.NegZDouble)" shouldNot compile
      "(NonZeroDoubles.NonZeroFiniteDouble(3.0) : PosDoubles.PosFiniteDouble)" shouldNot compile
      "(NonZeroDoubles.NonZeroFiniteDouble(3.0) : PosDoubles.PosZFiniteDouble)" shouldNot compile
      "(NonZeroDoubles.NonZeroFiniteDouble(3.0) : NegDoubles.NegFiniteDouble)" shouldNot compile
      "(NonZeroDoubles.NonZeroFiniteDouble(3.0) : NegDoubles.NegZFiniteDouble)" shouldNot compile
      "(NonZeroDoubles.NonZeroFiniteDouble(3.0) : Finites.FiniteDouble)" shouldNot compile
    }
    it("when a compatible AnyVal is passed to a + method invoked on it should give the same AnyVal type back at compile time, and correct value at runtime") {
      NonZeroDoubles.NonZeroFiniteDouble(3.0) + 3.toByte shouldEqual 6.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) + 3.toShort shouldEqual 6.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) + '3' shouldEqual 54.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) + 3 shouldEqual 6.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) + 3L shouldEqual 6.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) + 3.0f shouldEqual 6.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) + 3.0 shouldEqual 6.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) + PosInts.PosInt(3) shouldEqual 6.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) + PosInts.PosZInt(3) shouldEqual 6.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) + NegInts.NegInt(-3) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) + NegInts.NegZInt(-3) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) + NonZeroInts.NonZeroInt(3) shouldEqual 6.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) + Percentages.PercentageInt(50) shouldEqual 53.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) + PosLongs.PosLong(3L) shouldEqual 6.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) + PosLongs.PosZLong(3L) shouldEqual 6.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) + NegLongs.NegLong(-3L) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) + NegLongs.NegZLong(-3L) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) + NonZeroLongs.NonZeroLong(3L) shouldEqual 6.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) + PosFloats.PosFloat(3.0f) shouldEqual 6.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) + PosFloats.PosZFloat(3.0f) shouldEqual 6.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) + NegFloats.NegFloat(-3.0f) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) + NegFloats.NegZFloat(-3.0f) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) + NonZeroFloats.NonZeroFloat(3.0f) shouldEqual 6.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) + NonZeroFloats.NonZeroFiniteFloat(3.0f) shouldEqual 6.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) + PosFloats.PosFiniteFloat(3.0f) shouldEqual 6.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) + PosFloats.PosZFiniteFloat(3.0f) shouldEqual 6.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) + NegFloats.NegFiniteFloat(-3.0f) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) + NegFloats.NegZFiniteFloat(-3.0f) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) + Finites.FiniteFloat(3.0f) shouldEqual 6.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) + PosDoubles.PosDouble(3.0) shouldEqual 6.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) + PosDoubles.PosZDouble(3.0) shouldEqual 6.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) + NegDoubles.NegDouble(-3.0) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) + NegDoubles.NegZDouble(-3.0) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) + NonZeroDoubles.NonZeroDouble(3.0) shouldEqual 6.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) + PosDoubles.PosFiniteDouble(3.0) shouldEqual 6.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) + PosDoubles.PosZFiniteDouble(3.0) shouldEqual 6.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) + NegDoubles.NegFiniteDouble(-3.0) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) + NegDoubles.NegZFiniteDouble(-3.0) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) + Finites.FiniteDouble(3.0) shouldEqual 6.0
    }
    it("when a compatible AnyVal is passed to a - method invoked on it should give the same AnyVal type back at compile time, and correct value at runtime") {
      NonZeroDoubles.NonZeroFiniteDouble(3.0) - 3.toByte shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) - 3.toShort shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) - '3' shouldEqual -48.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) - 3 shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) - 3L shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) - 3.0f shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) - 3.0 shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) - PosInts.PosInt(3) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) - PosInts.PosZInt(3) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) - NegInts.NegInt(-3) shouldEqual 6.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) - NegInts.NegZInt(-3) shouldEqual 6.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) - NonZeroInts.NonZeroInt(3) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) - Percentages.PercentageInt(50) shouldEqual -47.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) - PosLongs.PosLong(3L) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) - PosLongs.PosZLong(3L) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) - NegLongs.NegLong(-3L) shouldEqual 6.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) - NegLongs.NegZLong(-3L) shouldEqual 6.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) - NonZeroLongs.NonZeroLong(3L) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) - PosFloats.PosFloat(3.0f) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) - PosFloats.PosZFloat(3.0f) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) - NegFloats.NegFloat(-3.0f) shouldEqual 6.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) - NegFloats.NegZFloat(-3.0f) shouldEqual 6.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) - NonZeroFloats.NonZeroFloat(3.0f) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) - NonZeroFloats.NonZeroFiniteFloat(3.0f) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) - PosFloats.PosFiniteFloat(3.0f) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) - PosFloats.PosZFiniteFloat(3.0f) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) - NegFloats.NegFiniteFloat(-3.0f) shouldEqual 6.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) - NegFloats.NegZFiniteFloat(-3.0f) shouldEqual 6.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) - Finites.FiniteFloat(3.0f) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) - PosDoubles.PosDouble(3.0) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) - PosDoubles.PosZDouble(3.0) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) - NegDoubles.NegDouble(-3.0) shouldEqual 6.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) - NegDoubles.NegZDouble(-3.0) shouldEqual 6.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) - NonZeroDoubles.NonZeroDouble(3.0) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) - PosDoubles.PosFiniteDouble(3.0) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) - PosDoubles.PosZFiniteDouble(3.0) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) - NegDoubles.NegFiniteDouble(-3.0) shouldEqual 6.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) - NegDoubles.NegZFiniteDouble(-3.0) shouldEqual 6.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) - Finites.FiniteDouble(3.0) shouldEqual 0.0
    }
    it("when a compatible AnyVal is passed to a * method invoked on it should give the same AnyVal type back at compile time, and correct value at runtime") {
      NonZeroDoubles.NonZeroFiniteDouble(3.0) * 3.toByte shouldEqual 9.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) * 3.toShort shouldEqual 9.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) * '3' shouldEqual 153.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) * 3 shouldEqual 9.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) * 3L shouldEqual 9.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) * 3.0f shouldEqual 9.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) * 3.0 shouldEqual 9.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) * PosInts.PosInt(3) shouldEqual 9.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) * PosInts.PosZInt(3) shouldEqual 9.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) * NegInts.NegInt(-3) shouldEqual -9.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) * NegInts.NegZInt(-3) shouldEqual -9.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) * NonZeroInts.NonZeroInt(3) shouldEqual 9.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) * Percentages.PercentageInt(50) shouldEqual 150.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) * PosLongs.PosLong(3L) shouldEqual 9.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) * PosLongs.PosZLong(3L) shouldEqual 9.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) * NegLongs.NegLong(-3L) shouldEqual -9.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) * NegLongs.NegZLong(-3L) shouldEqual -9.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) * NonZeroLongs.NonZeroLong(3L) shouldEqual 9.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) * PosFloats.PosFloat(3.0f) shouldEqual 9.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) * PosFloats.PosZFloat(3.0f) shouldEqual 9.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) * NegFloats.NegFloat(-3.0f) shouldEqual -9.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) * NegFloats.NegZFloat(-3.0f) shouldEqual -9.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) * NonZeroFloats.NonZeroFloat(3.0f) shouldEqual 9.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) * NonZeroFloats.NonZeroFiniteFloat(3.0f) shouldEqual 9.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) * PosFloats.PosFiniteFloat(3.0f) shouldEqual 9.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) * PosFloats.PosZFiniteFloat(3.0f) shouldEqual 9.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) * NegFloats.NegFiniteFloat(-3.0f) shouldEqual -9.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) * NegFloats.NegZFiniteFloat(-3.0f) shouldEqual -9.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) * Finites.FiniteFloat(3.0f) shouldEqual 9.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) * PosDoubles.PosDouble(3.0) shouldEqual 9.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) * PosDoubles.PosZDouble(3.0) shouldEqual 9.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) * NegDoubles.NegDouble(-3.0) shouldEqual -9.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) * NegDoubles.NegZDouble(-3.0) shouldEqual -9.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) * NonZeroDoubles.NonZeroDouble(3.0) shouldEqual 9.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) * PosDoubles.PosFiniteDouble(3.0) shouldEqual 9.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) * PosDoubles.PosZFiniteDouble(3.0) shouldEqual 9.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) * NegDoubles.NegFiniteDouble(-3.0) shouldEqual -9.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) * NegDoubles.NegZFiniteDouble(-3.0) shouldEqual -9.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) * Finites.FiniteDouble(3.0) shouldEqual 9.0
    }
    it("when a compatible AnyVal is passed to a / method invoked on it should give the same AnyVal type back at compile time, and correct value at runtime") {
      NonZeroDoubles.NonZeroFiniteDouble(3.0) / 3.toByte shouldEqual 1.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) / 3.toShort shouldEqual 1.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) / '3' shouldEqual 0.058823529411764705
      NonZeroDoubles.NonZeroFiniteDouble(3.0) / 3 shouldEqual 1.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) / 3L shouldEqual 1.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) / 3.0f shouldEqual 1.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) / 3.0 shouldEqual 1.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) / PosInts.PosInt(3) shouldEqual 1.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) / PosInts.PosZInt(3) shouldEqual 1.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) / NegInts.NegInt(-3) shouldEqual -1.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) / NegInts.NegZInt(-3) shouldEqual -1.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) / NonZeroInts.NonZeroInt(3) shouldEqual 1.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) / Percentages.PercentageInt(50) shouldEqual 0.06
      NonZeroDoubles.NonZeroFiniteDouble(3.0) / PosLongs.PosLong(3L) shouldEqual 1.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) / PosLongs.PosZLong(3L) shouldEqual 1.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) / NegLongs.NegLong(-3L) shouldEqual -1.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) / NegLongs.NegZLong(-3L) shouldEqual -1.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) / NonZeroLongs.NonZeroLong(3L) shouldEqual 1.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) / PosFloats.PosFloat(3.0f) shouldEqual 1.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) / PosFloats.PosZFloat(3.0f) shouldEqual 1.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) / NegFloats.NegFloat(-3.0f) shouldEqual -1.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) / NegFloats.NegZFloat(-3.0f) shouldEqual -1.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) / NonZeroFloats.NonZeroFloat(3.0f) shouldEqual 1.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) / NonZeroFloats.NonZeroFiniteFloat(3.0f) shouldEqual 1.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) / PosFloats.PosFiniteFloat(3.0f) shouldEqual 1.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) / PosFloats.PosZFiniteFloat(3.0f) shouldEqual 1.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) / NegFloats.NegFiniteFloat(-3.0f) shouldEqual -1.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) / NegFloats.NegZFiniteFloat(-3.0f) shouldEqual -1.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) / Finites.FiniteFloat(3.0f) shouldEqual 1.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) / PosDoubles.PosDouble(3.0) shouldEqual 1.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) / PosDoubles.PosZDouble(3.0) shouldEqual 1.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) / NegDoubles.NegDouble(-3.0) shouldEqual -1.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) / NegDoubles.NegZDouble(-3.0) shouldEqual -1.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) / NonZeroDoubles.NonZeroDouble(3.0) shouldEqual 1.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) / PosDoubles.PosFiniteDouble(3.0) shouldEqual 1.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) / PosDoubles.PosZFiniteDouble(3.0) shouldEqual 1.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) / NegDoubles.NegFiniteDouble(-3.0) shouldEqual -1.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) / NegDoubles.NegZFiniteDouble(-3.0) shouldEqual -1.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) / Finites.FiniteDouble(3.0) shouldEqual 1.0
    }
    it("when a compatible AnyVal is passed to a % method invoked on it should give the same AnyVal type back at compile time, and correct value at runtime") {
      NonZeroDoubles.NonZeroFiniteDouble(3.0) % 3.toByte shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) % 3.toShort shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) % '3' shouldEqual 3.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) % 3 shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) % 3L shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) % 3.0f shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) % 3.0 shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) % PosInts.PosInt(3) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) % PosInts.PosZInt(3) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) % NegInts.NegInt(-3) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) % NegInts.NegZInt(-3) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) % NonZeroInts.NonZeroInt(3) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) % Percentages.PercentageInt(50) shouldEqual 3.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) % PosLongs.PosLong(3L) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) % PosLongs.PosZLong(3L) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) % NegLongs.NegLong(-3L) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) % NegLongs.NegZLong(-3L) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) % NonZeroLongs.NonZeroLong(3L) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) % PosFloats.PosFloat(3.0f) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) % PosFloats.PosZFloat(3.0f) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) % NegFloats.NegFloat(-3.0f) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) % NegFloats.NegZFloat(-3.0f) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) % NonZeroFloats.NonZeroFloat(3.0f) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) % NonZeroFloats.NonZeroFiniteFloat(3.0f) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) % PosFloats.PosFiniteFloat(3.0f) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) % PosFloats.PosZFiniteFloat(3.0f) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) % NegFloats.NegFiniteFloat(-3.0f) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) % NegFloats.NegZFiniteFloat(-3.0f) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) % Finites.FiniteFloat(3.0f) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) % PosDoubles.PosDouble(3.0) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) % PosDoubles.PosZDouble(3.0) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) % NegDoubles.NegDouble(-3.0) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) % NegDoubles.NegZDouble(-3.0) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) % NonZeroDoubles.NonZeroDouble(3.0) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) % PosDoubles.PosFiniteDouble(3.0) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) % PosDoubles.PosZFiniteDouble(3.0) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) % NegDoubles.NegFiniteDouble(-3.0) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) % NegDoubles.NegZFiniteDouble(-3.0) shouldEqual 0.0
      NonZeroDoubles.NonZeroFiniteDouble(3.0) % Finites.FiniteDouble(3.0) shouldEqual 0.0
    }
    it("should offer '<' comparison that is consistent with Double") {
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) < 3.toByte) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) < 3.toShort) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) < '3') shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) < 3) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) < 3L) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) < 3.0f) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) < 3.0) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) < PosInts.PosInt(3)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) < PosInts.PosZInt(3)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) < NegInts.NegInt(-3)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) < NegInts.NegZInt(-3)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) < NonZeroInts.NonZeroInt(3)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) < Percentages.PercentageInt(50)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) < PosLongs.PosLong(3L)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) < PosLongs.PosZLong(3L)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) < NegLongs.NegLong(-3L)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) < NegLongs.NegZLong(-3L)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) < NonZeroLongs.NonZeroLong(3L)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) < PosFloats.PosFloat(3.0f)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) < PosFloats.PosZFloat(3.0f)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) < NegFloats.NegFloat(-3.0f)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) < NegFloats.NegZFloat(-3.0f)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) < NonZeroFloats.NonZeroFloat(3.0f)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) < NonZeroFloats.NonZeroFiniteFloat(3.0f)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) < PosFloats.PosFiniteFloat(3.0f)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) < PosFloats.PosZFiniteFloat(3.0f)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) < NegFloats.NegFiniteFloat(-3.0f)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) < NegFloats.NegZFiniteFloat(-3.0f)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) < Finites.FiniteFloat(3.0f)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) < PosDoubles.PosDouble(3.0)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) < NegDoubles.NegDouble(-3.0)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) < NegDoubles.NegZDouble(-3.0)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) < NonZeroDoubles.NonZeroDouble(3.0)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) < PosDoubles.PosFiniteDouble(3.0)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) < PosDoubles.PosZFiniteDouble(3.0)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) < NegDoubles.NegFiniteDouble(-3.0)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) < NegDoubles.NegZFiniteDouble(-3.0)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) < Finites.FiniteDouble(3.0)) shouldEqual false
    }
    it("should offer '<=' comparison that is consistent with Double") {
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) <= 3.toByte) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) <= 3.toShort) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) <= '3') shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) <= 3) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) <= 3L) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) <= 3.0f) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) <= 3.0) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) <= PosInts.PosInt(3)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) <= PosInts.PosZInt(3)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) <= NegInts.NegInt(-3)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) <= NegInts.NegZInt(-3)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) <= NonZeroInts.NonZeroInt(3)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) <= Percentages.PercentageInt(50)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) <= PosLongs.PosLong(3L)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) <= PosLongs.PosZLong(3L)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) <= NegLongs.NegLong(-3L)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) <= NegLongs.NegZLong(-3L)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) <= NonZeroLongs.NonZeroLong(3L)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) <= PosFloats.PosFloat(3.0f)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) <= PosFloats.PosZFloat(3.0f)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) <= NegFloats.NegFloat(-3.0f)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) <= NegFloats.NegZFloat(-3.0f)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) <= NonZeroFloats.NonZeroFloat(3.0f)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) <= NonZeroFloats.NonZeroFiniteFloat(3.0f)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) <= PosFloats.PosFiniteFloat(3.0f)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) <= PosFloats.PosZFiniteFloat(3.0f)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) <= NegFloats.NegFiniteFloat(-3.0f)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) <= NegFloats.NegZFiniteFloat(-3.0f)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) <= Finites.FiniteFloat(3.0f)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) <= PosDoubles.PosDouble(3.0)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) <= NegDoubles.NegDouble(-3.0)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) <= NegDoubles.NegZDouble(-3.0)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) <= NonZeroDoubles.NonZeroDouble(3.0)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) <= PosDoubles.PosFiniteDouble(3.0)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) <= PosDoubles.PosZFiniteDouble(3.0)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) <= NegDoubles.NegFiniteDouble(-3.0)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) <= NegDoubles.NegZFiniteDouble(-3.0)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) <= Finites.FiniteDouble(3.0)) shouldEqual true
    }
    it("should offer '>' comparison that is consistent with Double") {
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) > 3.toByte) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) > 3.toShort) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) > '3') shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) > 3) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) > 3L) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) > 3.0f) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) > 3.0) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) > PosInts.PosInt(3)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) > PosInts.PosZInt(3)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) > NegInts.NegInt(-3)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) > NegInts.NegZInt(-3)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) > NonZeroInts.NonZeroInt(3)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) > Percentages.PercentageInt(50)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) > PosLongs.PosLong(3L)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) > PosLongs.PosZLong(3L)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) > NegLongs.NegLong(-3L)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) > NegLongs.NegZLong(-3L)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) > NonZeroLongs.NonZeroLong(3L)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) > PosFloats.PosFloat(3.0f)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) > PosFloats.PosZFloat(3.0f)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) > NegFloats.NegFloat(-3.0f)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) > NegFloats.NegZFloat(-3.0f)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) > NonZeroFloats.NonZeroFloat(3.0f)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) > NonZeroFloats.NonZeroFiniteFloat(3.0f)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) > PosFloats.PosFiniteFloat(3.0f)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) > PosFloats.PosZFiniteFloat(3.0f)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) > NegFloats.NegFiniteFloat(-3.0f)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) > NegFloats.NegZFiniteFloat(-3.0f)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) > Finites.FiniteFloat(3.0f)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) > PosDoubles.PosDouble(3.0)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) > NegDoubles.NegDouble(-3.0)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) > NegDoubles.NegZDouble(-3.0)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) > NonZeroDoubles.NonZeroDouble(3.0)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) > PosDoubles.PosFiniteDouble(3.0)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) > PosDoubles.PosZFiniteDouble(3.0)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) > NegDoubles.NegFiniteDouble(-3.0)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) > NegDoubles.NegZFiniteDouble(-3.0)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) > Finites.FiniteDouble(3.0)) shouldEqual false
    }
    it("should offer '>=' comparison that is consistent with Double") {
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) >= 3.toByte) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) >= 3.toShort) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) >= '3') shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) >= 3) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) >= 3L) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) >= 3.0f) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) >= 3.0) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) >= PosInts.PosInt(3)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) >= PosInts.PosZInt(3)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) >= NegInts.NegInt(-3)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) >= NegInts.NegZInt(-3)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) >= NonZeroInts.NonZeroInt(3)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) >= Percentages.PercentageInt(50)) shouldEqual false
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) >= PosLongs.PosLong(3L)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) >= PosLongs.PosZLong(3L)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) >= NegLongs.NegLong(-3L)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) >= NegLongs.NegZLong(-3L)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) >= NonZeroLongs.NonZeroLong(3L)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) >= PosFloats.PosFloat(3.0f)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) >= PosFloats.PosZFloat(3.0f)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) >= NegFloats.NegFloat(-3.0f)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) >= NegFloats.NegZFloat(-3.0f)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) >= NonZeroFloats.NonZeroFloat(3.0f)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) >= NonZeroFloats.NonZeroFiniteFloat(3.0f)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) >= PosFloats.PosFiniteFloat(3.0f)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) >= PosFloats.PosZFiniteFloat(3.0f)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) >= NegFloats.NegFiniteFloat(-3.0f)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) >= NegFloats.NegZFiniteFloat(-3.0f)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) >= Finites.FiniteFloat(3.0f)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) >= PosDoubles.PosDouble(3.0)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) >= NegDoubles.NegDouble(-3.0)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) >= NegDoubles.NegZDouble(-3.0)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) >= NonZeroDoubles.NonZeroDouble(3.0)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) >= PosDoubles.PosFiniteDouble(3.0)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) >= PosDoubles.PosZFiniteDouble(3.0)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) >= NegDoubles.NegFiniteDouble(-3.0)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) >= NegDoubles.NegZFiniteDouble(-3.0)) shouldEqual true
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) >= Finites.FiniteDouble(3.0)) shouldEqual true
    }
    it("should offer widening methods for basic type Double that are consistent with Double") {
      def widen(value: Double): Double = value
      widen(NonZeroDoubles.NonZeroFiniteDouble(3.0)) shouldEqual 3.0
    }
    it("should offer widening methods for basic type NonZeroDoubles.NonZeroDouble that are consistent with Double") {
      def widen(value: NonZeroDoubles.NonZeroDouble): NonZeroDoubles.NonZeroDouble = value
      widen(NonZeroDoubles.NonZeroFiniteDouble(3.0)) shouldEqual NonZeroDoubles.NonZeroDouble(3.0)
    }
    it("should have a toString consistent with Double") {
      (NonZeroDoubles.NonZeroFiniteDouble(3.0) : Double).toString shouldBe 3.0.toString
    }
  }
}
