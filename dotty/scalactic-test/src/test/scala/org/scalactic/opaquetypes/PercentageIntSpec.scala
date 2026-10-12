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

import org.scalatest._
import org.scalactic._
import scala.util.{ Try, Success, Failure }

import org.scalactic.opaquetypes.Percentages.PercentageInt

class PercentageIntSpec extends funspec.AnyFunSpec with matchers.should.Matchers with OptionValues {

  describe("PercentageInt") {

    describe("compile-time apply") {

      it("should accept Int literals between 0 and 100 inclusive") {
        PercentageInt(0).value shouldBe 0
        PercentageInt(1).value shouldBe 1
        PercentageInt(8).value shouldBe 8
        PercentageInt(50).value shouldBe 50
        PercentageInt(100).value shouldBe 100
      }

      it("should not compile when a literal above 100 is passed in") {
        "PercentageInt(101)" shouldNot compile
        "PercentageInt(1000)" shouldNot compile
      }

      it("should not compile when a negative literal is passed in") {
        "PercentageInt(-1)" shouldNot compile
        "PercentageInt(-8)" shouldNot compile
      }

      it("should not compile when a non-literal is passed in") {
        val x: Int = -8
        "PercentageInt(x)" shouldNot compile
      }
    }

    describe("from") {

      it("should return Some(PercentageInt) for values between 0 and 100 inclusive") {
        PercentageInt.from(0).value.value shouldBe 0
        PercentageInt.from(50).value.value shouldBe 50
        PercentageInt.from(100).value.value shouldBe 100
      }

      it("should return None for values NOT between 0 and 100") {
        PercentageInt.from(101) shouldBe None
        PercentageInt.from(1000) shouldBe None
        PercentageInt.from(-1) shouldBe None
        PercentageInt.from(-99) shouldBe None
      }
    }

    describe("ensuringValid") {

      it("should return the PercentageInt for values between 0 and 100 inclusive") {
        PercentageInt.ensuringValid(0).value shouldBe 0
        PercentageInt.ensuringValid(50).value shouldBe 50
        PercentageInt.ensuringValid(100).value shouldBe 100
      }

      it("should throw AssertionError for values NOT between 0 and 100") {
        an[AssertionError] should be thrownBy PercentageInt.ensuringValid(101)
        an[AssertionError] should be thrownBy PercentageInt.ensuringValid(1000)
        an[AssertionError] should be thrownBy PercentageInt.ensuringValid(-1)
        an[AssertionError] should be thrownBy PercentageInt.ensuringValid(-99)
      }
    }

    describe("isValid") {

      it("should return true for values between 0 and 100 inclusive") {
        PercentageInt.isValid(0) shouldBe true
        PercentageInt.isValid(100) shouldBe true
      }

      it("should return false for values outside the range") {
        PercentageInt.isValid(101) shouldBe false
        PercentageInt.isValid(-1) shouldBe false
      }
    }

    describe("tryingValid") {

      it("should return Success for valid values") {
        PercentageInt.tryingValid(42).get.value shouldBe 42
      }

      it("should return Failure for invalid values") {
        PercentageInt.tryingValid(101) should matchPattern { case Failure(_: AssertionError) => }
      }
    }

    describe("Or / Validation helpers") {

      it("should return Good for valid values and Bad otherwise") {
        PercentageInt.goodOrElse(42)(_ => "err") match {
          case Good(v) => v.value shouldBe 42
          case Bad(_) => fail("expected Good")
        }
        PercentageInt.goodOrElse(101)(_ => "err") shouldBe Bad("err")
      }

      it("should return Right for valid values and Left otherwise") {
        PercentageInt.rightOrElse(42)(_ => "err") match {
          case Right(v) => v.value shouldBe 42
          case Left(_) => fail("expected Right")
        }
        PercentageInt.rightOrElse(101)(_ => "err") shouldBe Left("err")
      }

      it("should return Pass for valid values and Fail otherwise") {
        PercentageInt.passOrElse(42)(_ => "err") shouldBe Pass
        PercentageInt.passOrElse(101)(_ => "err") shouldBe Fail("err")
      }

      it("should return the default from fromOrElse when invalid") {
        PercentageInt.fromOrElse(42, PercentageInt(8)).value shouldBe 42
        PercentageInt.fromOrElse(101, PercentageInt(8)).value shouldBe 8
      }
    }

    describe("constants") {
      it("should expose MinValue and MaxValue") {
        PercentageInt.MinValue.value shouldBe 0
        PercentageInt.MaxValue.value shouldBe 100
      }
    }

    describe("extension methods") {
      it("should expose value and toInt") {
        PercentageInt(42).value shouldBe 42
        PercentageInt(42).toInt shouldBe 42
      }

      it("should expose isZero, isMin and isMax") {
        PercentageInt(0).isZero shouldBe true
        PercentageInt(0).isMin shouldBe true
        PercentageInt(0).isMax shouldBe false
        PercentageInt(100).isMax shouldBe true
        PercentageInt(100).isZero shouldBe false
        PercentageInt(50).isZero shouldBe false
      }
    }

    describe("Ordering") {
      it("should sort by numeric value") {
        List(PercentageInt(3), PercentageInt(1), PercentageInt(2)).sorted.map(_.value) shouldBe List(1, 2, 3)
      }
    }

    describe("interoperability") {
      it("should convert to Int") {
        val i: Int = PercentageInt(42)
        i shouldBe 42
      }

      it("should have a toString that yields the underlying Int, unlike anyvals PercentageInt") {
        PercentageInt(42).toString shouldBe "42"
      }
    }
  }
  describe("PercentageInt") {
    it("should be automatically widened to compatible AnyVal targets") {
      (Percentages.PercentageInt(50) : Int) shouldEqual 50
      "(Percentages.PercentageInt(50) : Byte)" shouldNot compile
      "(Percentages.PercentageInt(50) : Short)" shouldNot compile
      "(Percentages.PercentageInt(50) : Char)" shouldNot compile
      "(Percentages.PercentageInt(50) : Long)" shouldNot compile
      "(Percentages.PercentageInt(50) : Float)" shouldNot compile
      "(Percentages.PercentageInt(50) : Double)" shouldNot compile
      "(Percentages.PercentageInt(50) : PosInts.PosInt)" shouldNot compile
      "(Percentages.PercentageInt(50) : PosInts.PosZInt)" shouldNot compile
      "(Percentages.PercentageInt(50) : NegInts.NegInt)" shouldNot compile
      "(Percentages.PercentageInt(50) : NegInts.NegZInt)" shouldNot compile
      "(Percentages.PercentageInt(50) : NonZeroInts.NonZeroInt)" shouldNot compile
      "(Percentages.PercentageInt(50) : Numerics.NumericChar)" shouldNot compile
      "(Percentages.PercentageInt(50) : PosLongs.PosLong)" shouldNot compile
      "(Percentages.PercentageInt(50) : PosLongs.PosZLong)" shouldNot compile
      "(Percentages.PercentageInt(50) : NegLongs.NegLong)" shouldNot compile
      "(Percentages.PercentageInt(50) : NegLongs.NegZLong)" shouldNot compile
      "(Percentages.PercentageInt(50) : NonZeroLongs.NonZeroLong)" shouldNot compile
      "(Percentages.PercentageInt(50) : PosFloats.PosFloat)" shouldNot compile
      "(Percentages.PercentageInt(50) : PosFloats.PosZFloat)" shouldNot compile
      "(Percentages.PercentageInt(50) : NegFloats.NegFloat)" shouldNot compile
      "(Percentages.PercentageInt(50) : NegFloats.NegZFloat)" shouldNot compile
      "(Percentages.PercentageInt(50) : NonZeroFloats.NonZeroFloat)" shouldNot compile
      "(Percentages.PercentageInt(50) : NonZeroFloats.NonZeroFiniteFloat)" shouldNot compile
      "(Percentages.PercentageInt(50) : PosFloats.PosFiniteFloat)" shouldNot compile
      "(Percentages.PercentageInt(50) : PosFloats.PosZFiniteFloat)" shouldNot compile
      "(Percentages.PercentageInt(50) : NegFloats.NegFiniteFloat)" shouldNot compile
      "(Percentages.PercentageInt(50) : NegFloats.NegZFiniteFloat)" shouldNot compile
      "(Percentages.PercentageInt(50) : Finites.FiniteFloat)" shouldNot compile
      "(Percentages.PercentageInt(50) : PosDoubles.PosDouble)" shouldNot compile
      "(Percentages.PercentageInt(50) : PosDoubles.PosZDouble)" shouldNot compile
      "(Percentages.PercentageInt(50) : NegDoubles.NegDouble)" shouldNot compile
      "(Percentages.PercentageInt(50) : NegDoubles.NegZDouble)" shouldNot compile
      "(Percentages.PercentageInt(50) : NonZeroDoubles.NonZeroDouble)" shouldNot compile
      "(Percentages.PercentageInt(50) : NonZeroDoubles.NonZeroFiniteDouble)" shouldNot compile
      "(Percentages.PercentageInt(50) : PosDoubles.PosFiniteDouble)" shouldNot compile
      "(Percentages.PercentageInt(50) : PosDoubles.PosZFiniteDouble)" shouldNot compile
      "(Percentages.PercentageInt(50) : NegDoubles.NegFiniteDouble)" shouldNot compile
      "(Percentages.PercentageInt(50) : NegDoubles.NegZFiniteDouble)" shouldNot compile
      "(Percentages.PercentageInt(50) : Finites.FiniteDouble)" shouldNot compile
    }
    it("when a compatible AnyVal is passed to a + method invoked on it should give the same AnyVal type back at compile time, and correct value at runtime") {
      Percentages.PercentageInt(50) + 3.toByte shouldEqual 53
      Percentages.PercentageInt(50) + 3.toShort shouldEqual 53
      Percentages.PercentageInt(50) + '3' shouldEqual 101
      Percentages.PercentageInt(50) + 3 shouldEqual 53
      Percentages.PercentageInt(50) + 3L shouldEqual 53L
      Percentages.PercentageInt(50) + 3.0f shouldEqual 53.0f
      Percentages.PercentageInt(50) + 3.0 shouldEqual 53.0
      Percentages.PercentageInt(50) + PosInts.PosInt(3) shouldEqual 53
      Percentages.PercentageInt(50) + PosInts.PosZInt(3) shouldEqual 53
      Percentages.PercentageInt(50) + NegInts.NegInt(-3) shouldEqual 47
      Percentages.PercentageInt(50) + NegInts.NegZInt(-3) shouldEqual 47
      Percentages.PercentageInt(50) + NonZeroInts.NonZeroInt(3) shouldEqual 53
      Percentages.PercentageInt(50) + PosLongs.PosLong(3L) shouldEqual 53L
      Percentages.PercentageInt(50) + PosLongs.PosZLong(3L) shouldEqual 53L
      Percentages.PercentageInt(50) + NegLongs.NegLong(-3L) shouldEqual 47L
      Percentages.PercentageInt(50) + NegLongs.NegZLong(-3L) shouldEqual 47L
      Percentages.PercentageInt(50) + NonZeroLongs.NonZeroLong(3L) shouldEqual 53L
      Percentages.PercentageInt(50) + PosFloats.PosFloat(3.0f) shouldEqual 53.0f
      Percentages.PercentageInt(50) + PosFloats.PosZFloat(3.0f) shouldEqual 53.0f
      Percentages.PercentageInt(50) + NegFloats.NegFloat(-3.0f) shouldEqual 47.0f
      Percentages.PercentageInt(50) + NegFloats.NegZFloat(-3.0f) shouldEqual 47.0f
      Percentages.PercentageInt(50) + NonZeroFloats.NonZeroFloat(3.0f) shouldEqual 53.0f
      Percentages.PercentageInt(50) + NonZeroFloats.NonZeroFiniteFloat(3.0f) shouldEqual 53.0f
      Percentages.PercentageInt(50) + PosFloats.PosFiniteFloat(3.0f) shouldEqual 53.0f
      Percentages.PercentageInt(50) + PosFloats.PosZFiniteFloat(3.0f) shouldEqual 53.0f
      Percentages.PercentageInt(50) + NegFloats.NegFiniteFloat(-3.0f) shouldEqual 47.0f
      Percentages.PercentageInt(50) + NegFloats.NegZFiniteFloat(-3.0f) shouldEqual 47.0f
      Percentages.PercentageInt(50) + Finites.FiniteFloat(3.0f) shouldEqual 53.0f
      Percentages.PercentageInt(50) + PosDoubles.PosDouble(3.0) shouldEqual 53.0
      Percentages.PercentageInt(50) + NegDoubles.NegDouble(-3.0) shouldEqual 47.0
      Percentages.PercentageInt(50) + NegDoubles.NegZDouble(-3.0) shouldEqual 47.0
      Percentages.PercentageInt(50) + NonZeroDoubles.NonZeroDouble(3.0) shouldEqual 53.0
      Percentages.PercentageInt(50) + NonZeroDoubles.NonZeroFiniteDouble(3.0) shouldEqual 53.0
      Percentages.PercentageInt(50) + PosDoubles.PosFiniteDouble(3.0) shouldEqual 53.0
      Percentages.PercentageInt(50) + PosDoubles.PosZFiniteDouble(3.0) shouldEqual 53.0
      Percentages.PercentageInt(50) + NegDoubles.NegFiniteDouble(-3.0) shouldEqual 47.0
      Percentages.PercentageInt(50) + NegDoubles.NegZFiniteDouble(-3.0) shouldEqual 47.0
      Percentages.PercentageInt(50) + Finites.FiniteDouble(3.0) shouldEqual 53.0
    }
    it("when a compatible AnyVal is passed to a - method invoked on it should give the same AnyVal type back at compile time, and correct value at runtime") {
      Percentages.PercentageInt(50) - 3.toByte shouldEqual 47
      Percentages.PercentageInt(50) - 3.toShort shouldEqual 47
      Percentages.PercentageInt(50) - '3' shouldEqual -1
      Percentages.PercentageInt(50) - 3 shouldEqual 47
      Percentages.PercentageInt(50) - 3L shouldEqual 47L
      Percentages.PercentageInt(50) - 3.0f shouldEqual 47.0f
      Percentages.PercentageInt(50) - 3.0 shouldEqual 47.0
      Percentages.PercentageInt(50) - PosInts.PosInt(3) shouldEqual 47
      Percentages.PercentageInt(50) - PosInts.PosZInt(3) shouldEqual 47
      Percentages.PercentageInt(50) - NegInts.NegInt(-3) shouldEqual 53
      Percentages.PercentageInt(50) - NegInts.NegZInt(-3) shouldEqual 53
      Percentages.PercentageInt(50) - NonZeroInts.NonZeroInt(3) shouldEqual 47
      Percentages.PercentageInt(50) - PosLongs.PosLong(3L) shouldEqual 47L
      Percentages.PercentageInt(50) - PosLongs.PosZLong(3L) shouldEqual 47L
      Percentages.PercentageInt(50) - NegLongs.NegLong(-3L) shouldEqual 53L
      Percentages.PercentageInt(50) - NegLongs.NegZLong(-3L) shouldEqual 53L
      Percentages.PercentageInt(50) - NonZeroLongs.NonZeroLong(3L) shouldEqual 47L
      Percentages.PercentageInt(50) - PosFloats.PosFloat(3.0f) shouldEqual 47.0f
      Percentages.PercentageInt(50) - PosFloats.PosZFloat(3.0f) shouldEqual 47.0f
      Percentages.PercentageInt(50) - NegFloats.NegFloat(-3.0f) shouldEqual 53.0f
      Percentages.PercentageInt(50) - NegFloats.NegZFloat(-3.0f) shouldEqual 53.0f
      Percentages.PercentageInt(50) - NonZeroFloats.NonZeroFloat(3.0f) shouldEqual 47.0f
      Percentages.PercentageInt(50) - NonZeroFloats.NonZeroFiniteFloat(3.0f) shouldEqual 47.0f
      Percentages.PercentageInt(50) - PosFloats.PosFiniteFloat(3.0f) shouldEqual 47.0f
      Percentages.PercentageInt(50) - PosFloats.PosZFiniteFloat(3.0f) shouldEqual 47.0f
      Percentages.PercentageInt(50) - NegFloats.NegFiniteFloat(-3.0f) shouldEqual 53.0f
      Percentages.PercentageInt(50) - NegFloats.NegZFiniteFloat(-3.0f) shouldEqual 53.0f
      Percentages.PercentageInt(50) - Finites.FiniteFloat(3.0f) shouldEqual 47.0f
      Percentages.PercentageInt(50) - PosDoubles.PosDouble(3.0) shouldEqual 47.0
      Percentages.PercentageInt(50) - NegDoubles.NegDouble(-3.0) shouldEqual 53.0
      Percentages.PercentageInt(50) - NegDoubles.NegZDouble(-3.0) shouldEqual 53.0
      Percentages.PercentageInt(50) - NonZeroDoubles.NonZeroDouble(3.0) shouldEqual 47.0
      Percentages.PercentageInt(50) - NonZeroDoubles.NonZeroFiniteDouble(3.0) shouldEqual 47.0
      Percentages.PercentageInt(50) - PosDoubles.PosFiniteDouble(3.0) shouldEqual 47.0
      Percentages.PercentageInt(50) - PosDoubles.PosZFiniteDouble(3.0) shouldEqual 47.0
      Percentages.PercentageInt(50) - NegDoubles.NegFiniteDouble(-3.0) shouldEqual 53.0
      Percentages.PercentageInt(50) - NegDoubles.NegZFiniteDouble(-3.0) shouldEqual 53.0
      Percentages.PercentageInt(50) - Finites.FiniteDouble(3.0) shouldEqual 47.0
    }
    it("when a compatible AnyVal is passed to a * method invoked on it should give the same AnyVal type back at compile time, and correct value at runtime") {
      Percentages.PercentageInt(50) * 3.toByte shouldEqual 150
      Percentages.PercentageInt(50) * 3.toShort shouldEqual 150
      Percentages.PercentageInt(50) * '3' shouldEqual 2550
      Percentages.PercentageInt(50) * 3 shouldEqual 150
      Percentages.PercentageInt(50) * 3L shouldEqual 150L
      Percentages.PercentageInt(50) * 3.0f shouldEqual 150.0f
      Percentages.PercentageInt(50) * 3.0 shouldEqual 150.0
      Percentages.PercentageInt(50) * PosInts.PosInt(3) shouldEqual 150
      Percentages.PercentageInt(50) * PosInts.PosZInt(3) shouldEqual 150
      Percentages.PercentageInt(50) * NegInts.NegInt(-3) shouldEqual -150
      Percentages.PercentageInt(50) * NegInts.NegZInt(-3) shouldEqual -150
      Percentages.PercentageInt(50) * NonZeroInts.NonZeroInt(3) shouldEqual 150
      Percentages.PercentageInt(50) * PosLongs.PosLong(3L) shouldEqual 150L
      Percentages.PercentageInt(50) * PosLongs.PosZLong(3L) shouldEqual 150L
      Percentages.PercentageInt(50) * NegLongs.NegLong(-3L) shouldEqual -150L
      Percentages.PercentageInt(50) * NegLongs.NegZLong(-3L) shouldEqual -150L
      Percentages.PercentageInt(50) * NonZeroLongs.NonZeroLong(3L) shouldEqual 150L
      Percentages.PercentageInt(50) * PosFloats.PosFloat(3.0f) shouldEqual 150.0f
      Percentages.PercentageInt(50) * PosFloats.PosZFloat(3.0f) shouldEqual 150.0f
      Percentages.PercentageInt(50) * NegFloats.NegFloat(-3.0f) shouldEqual -150.0f
      Percentages.PercentageInt(50) * NegFloats.NegZFloat(-3.0f) shouldEqual -150.0f
      Percentages.PercentageInt(50) * NonZeroFloats.NonZeroFloat(3.0f) shouldEqual 150.0f
      Percentages.PercentageInt(50) * NonZeroFloats.NonZeroFiniteFloat(3.0f) shouldEqual 150.0f
      Percentages.PercentageInt(50) * PosFloats.PosFiniteFloat(3.0f) shouldEqual 150.0f
      Percentages.PercentageInt(50) * PosFloats.PosZFiniteFloat(3.0f) shouldEqual 150.0f
      Percentages.PercentageInt(50) * NegFloats.NegFiniteFloat(-3.0f) shouldEqual -150.0f
      Percentages.PercentageInt(50) * NegFloats.NegZFiniteFloat(-3.0f) shouldEqual -150.0f
      Percentages.PercentageInt(50) * Finites.FiniteFloat(3.0f) shouldEqual 150.0f
      Percentages.PercentageInt(50) * PosDoubles.PosDouble(3.0) shouldEqual 150.0
      Percentages.PercentageInt(50) * NegDoubles.NegDouble(-3.0) shouldEqual -150.0
      Percentages.PercentageInt(50) * NegDoubles.NegZDouble(-3.0) shouldEqual -150.0
      Percentages.PercentageInt(50) * NonZeroDoubles.NonZeroDouble(3.0) shouldEqual 150.0
      Percentages.PercentageInt(50) * NonZeroDoubles.NonZeroFiniteDouble(3.0) shouldEqual 150.0
      Percentages.PercentageInt(50) * PosDoubles.PosFiniteDouble(3.0) shouldEqual 150.0
      Percentages.PercentageInt(50) * PosDoubles.PosZFiniteDouble(3.0) shouldEqual 150.0
      Percentages.PercentageInt(50) * NegDoubles.NegFiniteDouble(-3.0) shouldEqual -150.0
      Percentages.PercentageInt(50) * NegDoubles.NegZFiniteDouble(-3.0) shouldEqual -150.0
      Percentages.PercentageInt(50) * Finites.FiniteDouble(3.0) shouldEqual 150.0
    }
    it("when a compatible AnyVal is passed to a / method invoked on it should give the same AnyVal type back at compile time, and correct value at runtime") {
      Percentages.PercentageInt(50) / 3.toByte shouldEqual 16
      Percentages.PercentageInt(50) / 3.toShort shouldEqual 16
      Percentages.PercentageInt(50) / '3' shouldEqual 0
      Percentages.PercentageInt(50) / 3 shouldEqual 16
      Percentages.PercentageInt(50) / 3L shouldEqual 16L
      Percentages.PercentageInt(50) / 3.0f shouldEqual 16.666666666666668f
      Percentages.PercentageInt(50) / 3.0 shouldEqual 16.666666666666668
      Percentages.PercentageInt(50) / PosInts.PosInt(3) shouldEqual 16
      Percentages.PercentageInt(50) / PosInts.PosZInt(3) shouldEqual 16
      Percentages.PercentageInt(50) / NegInts.NegInt(-3) shouldEqual -16
      Percentages.PercentageInt(50) / NegInts.NegZInt(-3) shouldEqual -16
      Percentages.PercentageInt(50) / NonZeroInts.NonZeroInt(3) shouldEqual 16
      Percentages.PercentageInt(50) / PosLongs.PosLong(3L) shouldEqual 16L
      Percentages.PercentageInt(50) / PosLongs.PosZLong(3L) shouldEqual 16L
      Percentages.PercentageInt(50) / NegLongs.NegLong(-3L) shouldEqual -16L
      Percentages.PercentageInt(50) / NegLongs.NegZLong(-3L) shouldEqual -16L
      Percentages.PercentageInt(50) / NonZeroLongs.NonZeroLong(3L) shouldEqual 16L
      Percentages.PercentageInt(50) / PosFloats.PosFloat(3.0f) shouldEqual 16.666666666666668f
      Percentages.PercentageInt(50) / PosFloats.PosZFloat(3.0f) shouldEqual 16.666666666666668f
      Percentages.PercentageInt(50) / NegFloats.NegFloat(-3.0f) shouldEqual -16.666666666666668f
      Percentages.PercentageInt(50) / NegFloats.NegZFloat(-3.0f) shouldEqual -16.666666666666668f
      Percentages.PercentageInt(50) / NonZeroFloats.NonZeroFloat(3.0f) shouldEqual 16.666666666666668f
      Percentages.PercentageInt(50) / NonZeroFloats.NonZeroFiniteFloat(3.0f) shouldEqual 16.666666666666668f
      Percentages.PercentageInt(50) / PosFloats.PosFiniteFloat(3.0f) shouldEqual 16.666666666666668f
      Percentages.PercentageInt(50) / PosFloats.PosZFiniteFloat(3.0f) shouldEqual 16.666666666666668f
      Percentages.PercentageInt(50) / NegFloats.NegFiniteFloat(-3.0f) shouldEqual -16.666666666666668f
      Percentages.PercentageInt(50) / NegFloats.NegZFiniteFloat(-3.0f) shouldEqual -16.666666666666668f
      Percentages.PercentageInt(50) / Finites.FiniteFloat(3.0f) shouldEqual 16.666666666666668f
      Percentages.PercentageInt(50) / PosDoubles.PosDouble(3.0) shouldEqual 16.666666666666668
      Percentages.PercentageInt(50) / NegDoubles.NegDouble(-3.0) shouldEqual -16.666666666666668
      Percentages.PercentageInt(50) / NegDoubles.NegZDouble(-3.0) shouldEqual -16.666666666666668
      Percentages.PercentageInt(50) / NonZeroDoubles.NonZeroDouble(3.0) shouldEqual 16.666666666666668
      Percentages.PercentageInt(50) / NonZeroDoubles.NonZeroFiniteDouble(3.0) shouldEqual 16.666666666666668
      Percentages.PercentageInt(50) / PosDoubles.PosFiniteDouble(3.0) shouldEqual 16.666666666666668
      Percentages.PercentageInt(50) / PosDoubles.PosZFiniteDouble(3.0) shouldEqual 16.666666666666668
      Percentages.PercentageInt(50) / NegDoubles.NegFiniteDouble(-3.0) shouldEqual -16.666666666666668
      Percentages.PercentageInt(50) / NegDoubles.NegZFiniteDouble(-3.0) shouldEqual -16.666666666666668
      Percentages.PercentageInt(50) / Finites.FiniteDouble(3.0) shouldEqual 16.666666666666668
    }
    it("when a compatible AnyVal is passed to a % method invoked on it should give the same AnyVal type back at compile time, and correct value at runtime") {
      Percentages.PercentageInt(50) % 3.toByte shouldEqual 2
      Percentages.PercentageInt(50) % 3.toShort shouldEqual 2
      Percentages.PercentageInt(50) % '3' shouldEqual 50
      Percentages.PercentageInt(50) % 3 shouldEqual 2
      Percentages.PercentageInt(50) % 3L shouldEqual 2L
      Percentages.PercentageInt(50) % 3.0f shouldEqual 2.0f
      Percentages.PercentageInt(50) % 3.0 shouldEqual 2.0
      Percentages.PercentageInt(50) % PosInts.PosInt(3) shouldEqual 2
      Percentages.PercentageInt(50) % PosInts.PosZInt(3) shouldEqual 2
      Percentages.PercentageInt(50) % NegInts.NegInt(-3) shouldEqual 2
      Percentages.PercentageInt(50) % NegInts.NegZInt(-3) shouldEqual 2
      Percentages.PercentageInt(50) % NonZeroInts.NonZeroInt(3) shouldEqual 2
      Percentages.PercentageInt(50) % PosLongs.PosLong(3L) shouldEqual 2L
      Percentages.PercentageInt(50) % PosLongs.PosZLong(3L) shouldEqual 2L
      Percentages.PercentageInt(50) % NegLongs.NegLong(-3L) shouldEqual 2L
      Percentages.PercentageInt(50) % NegLongs.NegZLong(-3L) shouldEqual 2L
      Percentages.PercentageInt(50) % NonZeroLongs.NonZeroLong(3L) shouldEqual 2L
      Percentages.PercentageInt(50) % PosFloats.PosFloat(3.0f) shouldEqual 2.0f
      Percentages.PercentageInt(50) % PosFloats.PosZFloat(3.0f) shouldEqual 2.0f
      Percentages.PercentageInt(50) % NegFloats.NegFloat(-3.0f) shouldEqual 2.0f
      Percentages.PercentageInt(50) % NegFloats.NegZFloat(-3.0f) shouldEqual 2.0f
      Percentages.PercentageInt(50) % NonZeroFloats.NonZeroFloat(3.0f) shouldEqual 2.0f
      Percentages.PercentageInt(50) % NonZeroFloats.NonZeroFiniteFloat(3.0f) shouldEqual 2.0f
      Percentages.PercentageInt(50) % PosFloats.PosFiniteFloat(3.0f) shouldEqual 2.0f
      Percentages.PercentageInt(50) % PosFloats.PosZFiniteFloat(3.0f) shouldEqual 2.0f
      Percentages.PercentageInt(50) % NegFloats.NegFiniteFloat(-3.0f) shouldEqual 2.0f
      Percentages.PercentageInt(50) % NegFloats.NegZFiniteFloat(-3.0f) shouldEqual 2.0f
      Percentages.PercentageInt(50) % Finites.FiniteFloat(3.0f) shouldEqual 2.0f
      Percentages.PercentageInt(50) % PosDoubles.PosDouble(3.0) shouldEqual 2.0
      Percentages.PercentageInt(50) % NegDoubles.NegDouble(-3.0) shouldEqual 2.0
      Percentages.PercentageInt(50) % NegDoubles.NegZDouble(-3.0) shouldEqual 2.0
      Percentages.PercentageInt(50) % NonZeroDoubles.NonZeroDouble(3.0) shouldEqual 2.0
      Percentages.PercentageInt(50) % NonZeroDoubles.NonZeroFiniteDouble(3.0) shouldEqual 2.0
      Percentages.PercentageInt(50) % PosDoubles.PosFiniteDouble(3.0) shouldEqual 2.0
      Percentages.PercentageInt(50) % PosDoubles.PosZFiniteDouble(3.0) shouldEqual 2.0
      Percentages.PercentageInt(50) % NegDoubles.NegFiniteDouble(-3.0) shouldEqual 2.0
      Percentages.PercentageInt(50) % NegDoubles.NegZFiniteDouble(-3.0) shouldEqual 2.0
      Percentages.PercentageInt(50) % Finites.FiniteDouble(3.0) shouldEqual 2.0
    }
    it("should offer '<' comparison that is consistent with Int") {
      (Percentages.PercentageInt(50) < 3.toByte) shouldEqual false
      (Percentages.PercentageInt(50) < 3.toShort) shouldEqual false
      (Percentages.PercentageInt(50) < '3') shouldEqual true
      (Percentages.PercentageInt(50) < 3) shouldEqual false
      (Percentages.PercentageInt(50) < 3L) shouldEqual false
      (Percentages.PercentageInt(50) < 3.0f) shouldEqual false
      (Percentages.PercentageInt(50) < 3.0) shouldEqual false
      (Percentages.PercentageInt(50) < PosInts.PosInt(3)) shouldEqual false
      (Percentages.PercentageInt(50) < PosInts.PosZInt(3)) shouldEqual false
      (Percentages.PercentageInt(50) < NegInts.NegInt(-3)) shouldEqual false
      (Percentages.PercentageInt(50) < NegInts.NegZInt(-3)) shouldEqual false
      (Percentages.PercentageInt(50) < NonZeroInts.NonZeroInt(3)) shouldEqual false
      (Percentages.PercentageInt(50) < PosLongs.PosLong(3L)) shouldEqual false
      (Percentages.PercentageInt(50) < PosLongs.PosZLong(3L)) shouldEqual false
      (Percentages.PercentageInt(50) < NegLongs.NegLong(-3L)) shouldEqual false
      (Percentages.PercentageInt(50) < NegLongs.NegZLong(-3L)) shouldEqual false
      (Percentages.PercentageInt(50) < NonZeroLongs.NonZeroLong(3L)) shouldEqual false
      (Percentages.PercentageInt(50) < PosFloats.PosFloat(3.0f)) shouldEqual false
      (Percentages.PercentageInt(50) < PosFloats.PosZFloat(3.0f)) shouldEqual false
      (Percentages.PercentageInt(50) < NegFloats.NegFloat(-3.0f)) shouldEqual false
      (Percentages.PercentageInt(50) < NegFloats.NegZFloat(-3.0f)) shouldEqual false
      (Percentages.PercentageInt(50) < NonZeroFloats.NonZeroFloat(3.0f)) shouldEqual false
      (Percentages.PercentageInt(50) < NonZeroFloats.NonZeroFiniteFloat(3.0f)) shouldEqual false
      (Percentages.PercentageInt(50) < PosFloats.PosFiniteFloat(3.0f)) shouldEqual false
      (Percentages.PercentageInt(50) < PosFloats.PosZFiniteFloat(3.0f)) shouldEqual false
      (Percentages.PercentageInt(50) < NegFloats.NegFiniteFloat(-3.0f)) shouldEqual false
      (Percentages.PercentageInt(50) < NegFloats.NegZFiniteFloat(-3.0f)) shouldEqual false
      (Percentages.PercentageInt(50) < Finites.FiniteFloat(3.0f)) shouldEqual false
      (Percentages.PercentageInt(50) < PosDoubles.PosDouble(3.0)) shouldEqual false
      (Percentages.PercentageInt(50) < NegDoubles.NegDouble(-3.0)) shouldEqual false
      (Percentages.PercentageInt(50) < NegDoubles.NegZDouble(-3.0)) shouldEqual false
      (Percentages.PercentageInt(50) < NonZeroDoubles.NonZeroDouble(3.0)) shouldEqual false
      (Percentages.PercentageInt(50) < NonZeroDoubles.NonZeroFiniteDouble(3.0)) shouldEqual false
      (Percentages.PercentageInt(50) < PosDoubles.PosFiniteDouble(3.0)) shouldEqual false
      (Percentages.PercentageInt(50) < PosDoubles.PosZFiniteDouble(3.0)) shouldEqual false
      (Percentages.PercentageInt(50) < NegDoubles.NegFiniteDouble(-3.0)) shouldEqual false
      (Percentages.PercentageInt(50) < NegDoubles.NegZFiniteDouble(-3.0)) shouldEqual false
      (Percentages.PercentageInt(50) < Finites.FiniteDouble(3.0)) shouldEqual false
    }
    it("should offer '<=' comparison that is consistent with Int") {
      (Percentages.PercentageInt(50) <= 3.toByte) shouldEqual false
      (Percentages.PercentageInt(50) <= 3.toShort) shouldEqual false
      (Percentages.PercentageInt(50) <= '3') shouldEqual true
      (Percentages.PercentageInt(50) <= 3) shouldEqual false
      (Percentages.PercentageInt(50) <= 3L) shouldEqual false
      (Percentages.PercentageInt(50) <= 3.0f) shouldEqual false
      (Percentages.PercentageInt(50) <= 3.0) shouldEqual false
      (Percentages.PercentageInt(50) <= PosInts.PosInt(3)) shouldEqual false
      (Percentages.PercentageInt(50) <= PosInts.PosZInt(3)) shouldEqual false
      (Percentages.PercentageInt(50) <= NegInts.NegInt(-3)) shouldEqual false
      (Percentages.PercentageInt(50) <= NegInts.NegZInt(-3)) shouldEqual false
      (Percentages.PercentageInt(50) <= NonZeroInts.NonZeroInt(3)) shouldEqual false
      (Percentages.PercentageInt(50) <= PosLongs.PosLong(3L)) shouldEqual false
      (Percentages.PercentageInt(50) <= PosLongs.PosZLong(3L)) shouldEqual false
      (Percentages.PercentageInt(50) <= NegLongs.NegLong(-3L)) shouldEqual false
      (Percentages.PercentageInt(50) <= NegLongs.NegZLong(-3L)) shouldEqual false
      (Percentages.PercentageInt(50) <= NonZeroLongs.NonZeroLong(3L)) shouldEqual false
      (Percentages.PercentageInt(50) <= PosFloats.PosFloat(3.0f)) shouldEqual false
      (Percentages.PercentageInt(50) <= PosFloats.PosZFloat(3.0f)) shouldEqual false
      (Percentages.PercentageInt(50) <= NegFloats.NegFloat(-3.0f)) shouldEqual false
      (Percentages.PercentageInt(50) <= NegFloats.NegZFloat(-3.0f)) shouldEqual false
      (Percentages.PercentageInt(50) <= NonZeroFloats.NonZeroFloat(3.0f)) shouldEqual false
      (Percentages.PercentageInt(50) <= NonZeroFloats.NonZeroFiniteFloat(3.0f)) shouldEqual false
      (Percentages.PercentageInt(50) <= PosFloats.PosFiniteFloat(3.0f)) shouldEqual false
      (Percentages.PercentageInt(50) <= PosFloats.PosZFiniteFloat(3.0f)) shouldEqual false
      (Percentages.PercentageInt(50) <= NegFloats.NegFiniteFloat(-3.0f)) shouldEqual false
      (Percentages.PercentageInt(50) <= NegFloats.NegZFiniteFloat(-3.0f)) shouldEqual false
      (Percentages.PercentageInt(50) <= Finites.FiniteFloat(3.0f)) shouldEqual false
      (Percentages.PercentageInt(50) <= PosDoubles.PosDouble(3.0)) shouldEqual false
      (Percentages.PercentageInt(50) <= NegDoubles.NegDouble(-3.0)) shouldEqual false
      (Percentages.PercentageInt(50) <= NegDoubles.NegZDouble(-3.0)) shouldEqual false
      (Percentages.PercentageInt(50) <= NonZeroDoubles.NonZeroDouble(3.0)) shouldEqual false
      (Percentages.PercentageInt(50) <= NonZeroDoubles.NonZeroFiniteDouble(3.0)) shouldEqual false
      (Percentages.PercentageInt(50) <= PosDoubles.PosFiniteDouble(3.0)) shouldEqual false
      (Percentages.PercentageInt(50) <= PosDoubles.PosZFiniteDouble(3.0)) shouldEqual false
      (Percentages.PercentageInt(50) <= NegDoubles.NegFiniteDouble(-3.0)) shouldEqual false
      (Percentages.PercentageInt(50) <= NegDoubles.NegZFiniteDouble(-3.0)) shouldEqual false
      (Percentages.PercentageInt(50) <= Finites.FiniteDouble(3.0)) shouldEqual false
    }
    it("should offer '>' comparison that is consistent with Int") {
      (Percentages.PercentageInt(50) > 3.toByte) shouldEqual true
      (Percentages.PercentageInt(50) > 3.toShort) shouldEqual true
      (Percentages.PercentageInt(50) > '3') shouldEqual false
      (Percentages.PercentageInt(50) > 3) shouldEqual true
      (Percentages.PercentageInt(50) > 3L) shouldEqual true
      (Percentages.PercentageInt(50) > 3.0f) shouldEqual true
      (Percentages.PercentageInt(50) > 3.0) shouldEqual true
      (Percentages.PercentageInt(50) > PosInts.PosInt(3)) shouldEqual true
      (Percentages.PercentageInt(50) > PosInts.PosZInt(3)) shouldEqual true
      (Percentages.PercentageInt(50) > NegInts.NegInt(-3)) shouldEqual true
      (Percentages.PercentageInt(50) > NegInts.NegZInt(-3)) shouldEqual true
      (Percentages.PercentageInt(50) > NonZeroInts.NonZeroInt(3)) shouldEqual true
      (Percentages.PercentageInt(50) > PosLongs.PosLong(3L)) shouldEqual true
      (Percentages.PercentageInt(50) > PosLongs.PosZLong(3L)) shouldEqual true
      (Percentages.PercentageInt(50) > NegLongs.NegLong(-3L)) shouldEqual true
      (Percentages.PercentageInt(50) > NegLongs.NegZLong(-3L)) shouldEqual true
      (Percentages.PercentageInt(50) > NonZeroLongs.NonZeroLong(3L)) shouldEqual true
      (Percentages.PercentageInt(50) > PosFloats.PosFloat(3.0f)) shouldEqual true
      (Percentages.PercentageInt(50) > PosFloats.PosZFloat(3.0f)) shouldEqual true
      (Percentages.PercentageInt(50) > NegFloats.NegFloat(-3.0f)) shouldEqual true
      (Percentages.PercentageInt(50) > NegFloats.NegZFloat(-3.0f)) shouldEqual true
      (Percentages.PercentageInt(50) > NonZeroFloats.NonZeroFloat(3.0f)) shouldEqual true
      (Percentages.PercentageInt(50) > NonZeroFloats.NonZeroFiniteFloat(3.0f)) shouldEqual true
      (Percentages.PercentageInt(50) > PosFloats.PosFiniteFloat(3.0f)) shouldEqual true
      (Percentages.PercentageInt(50) > PosFloats.PosZFiniteFloat(3.0f)) shouldEqual true
      (Percentages.PercentageInt(50) > NegFloats.NegFiniteFloat(-3.0f)) shouldEqual true
      (Percentages.PercentageInt(50) > NegFloats.NegZFiniteFloat(-3.0f)) shouldEqual true
      (Percentages.PercentageInt(50) > Finites.FiniteFloat(3.0f)) shouldEqual true
      (Percentages.PercentageInt(50) > PosDoubles.PosDouble(3.0)) shouldEqual true
      (Percentages.PercentageInt(50) > NegDoubles.NegDouble(-3.0)) shouldEqual true
      (Percentages.PercentageInt(50) > NegDoubles.NegZDouble(-3.0)) shouldEqual true
      (Percentages.PercentageInt(50) > NonZeroDoubles.NonZeroDouble(3.0)) shouldEqual true
      (Percentages.PercentageInt(50) > NonZeroDoubles.NonZeroFiniteDouble(3.0)) shouldEqual true
      (Percentages.PercentageInt(50) > PosDoubles.PosFiniteDouble(3.0)) shouldEqual true
      (Percentages.PercentageInt(50) > PosDoubles.PosZFiniteDouble(3.0)) shouldEqual true
      (Percentages.PercentageInt(50) > NegDoubles.NegFiniteDouble(-3.0)) shouldEqual true
      (Percentages.PercentageInt(50) > NegDoubles.NegZFiniteDouble(-3.0)) shouldEqual true
      (Percentages.PercentageInt(50) > Finites.FiniteDouble(3.0)) shouldEqual true
    }
    it("should offer '>=' comparison that is consistent with Int") {
      (Percentages.PercentageInt(50) >= 3.toByte) shouldEqual true
      (Percentages.PercentageInt(50) >= 3.toShort) shouldEqual true
      (Percentages.PercentageInt(50) >= '3') shouldEqual false
      (Percentages.PercentageInt(50) >= 3) shouldEqual true
      (Percentages.PercentageInt(50) >= 3L) shouldEqual true
      (Percentages.PercentageInt(50) >= 3.0f) shouldEqual true
      (Percentages.PercentageInt(50) >= 3.0) shouldEqual true
      (Percentages.PercentageInt(50) >= PosInts.PosInt(3)) shouldEqual true
      (Percentages.PercentageInt(50) >= PosInts.PosZInt(3)) shouldEqual true
      (Percentages.PercentageInt(50) >= NegInts.NegInt(-3)) shouldEqual true
      (Percentages.PercentageInt(50) >= NegInts.NegZInt(-3)) shouldEqual true
      (Percentages.PercentageInt(50) >= NonZeroInts.NonZeroInt(3)) shouldEqual true
      (Percentages.PercentageInt(50) >= PosLongs.PosLong(3L)) shouldEqual true
      (Percentages.PercentageInt(50) >= PosLongs.PosZLong(3L)) shouldEqual true
      (Percentages.PercentageInt(50) >= NegLongs.NegLong(-3L)) shouldEqual true
      (Percentages.PercentageInt(50) >= NegLongs.NegZLong(-3L)) shouldEqual true
      (Percentages.PercentageInt(50) >= NonZeroLongs.NonZeroLong(3L)) shouldEqual true
      (Percentages.PercentageInt(50) >= PosFloats.PosFloat(3.0f)) shouldEqual true
      (Percentages.PercentageInt(50) >= PosFloats.PosZFloat(3.0f)) shouldEqual true
      (Percentages.PercentageInt(50) >= NegFloats.NegFloat(-3.0f)) shouldEqual true
      (Percentages.PercentageInt(50) >= NegFloats.NegZFloat(-3.0f)) shouldEqual true
      (Percentages.PercentageInt(50) >= NonZeroFloats.NonZeroFloat(3.0f)) shouldEqual true
      (Percentages.PercentageInt(50) >= NonZeroFloats.NonZeroFiniteFloat(3.0f)) shouldEqual true
      (Percentages.PercentageInt(50) >= PosFloats.PosFiniteFloat(3.0f)) shouldEqual true
      (Percentages.PercentageInt(50) >= PosFloats.PosZFiniteFloat(3.0f)) shouldEqual true
      (Percentages.PercentageInt(50) >= NegFloats.NegFiniteFloat(-3.0f)) shouldEqual true
      (Percentages.PercentageInt(50) >= NegFloats.NegZFiniteFloat(-3.0f)) shouldEqual true
      (Percentages.PercentageInt(50) >= Finites.FiniteFloat(3.0f)) shouldEqual true
      (Percentages.PercentageInt(50) >= PosDoubles.PosDouble(3.0)) shouldEqual true
      (Percentages.PercentageInt(50) >= NegDoubles.NegDouble(-3.0)) shouldEqual true
      (Percentages.PercentageInt(50) >= NegDoubles.NegZDouble(-3.0)) shouldEqual true
      (Percentages.PercentageInt(50) >= NonZeroDoubles.NonZeroDouble(3.0)) shouldEqual true
      (Percentages.PercentageInt(50) >= NonZeroDoubles.NonZeroFiniteDouble(3.0)) shouldEqual true
      (Percentages.PercentageInt(50) >= PosDoubles.PosFiniteDouble(3.0)) shouldEqual true
      (Percentages.PercentageInt(50) >= PosDoubles.PosZFiniteDouble(3.0)) shouldEqual true
      (Percentages.PercentageInt(50) >= NegDoubles.NegFiniteDouble(-3.0)) shouldEqual true
      (Percentages.PercentageInt(50) >= NegDoubles.NegZFiniteDouble(-3.0)) shouldEqual true
      (Percentages.PercentageInt(50) >= Finites.FiniteDouble(3.0)) shouldEqual true
    }
    it("should return the same type from its unary_+ method") {
      +Percentages.PercentageInt(50) shouldEqual Percentages.PercentageInt(50)
    }
    it("should offer a unary - method that is consistent with Int") {
      (-Percentages.PercentageInt(50)) shouldEqual -50
    }
    it("should offer widening methods for basic type Int that are consistent with Int") {
      def widen(value: Int): Int = value
      widen(Percentages.PercentageInt(50)) shouldEqual 50
    }
    it("should have a toString consistent with Int") {
      (Percentages.PercentageInt(50) : Int).toString shouldBe 50.toString
    }
    it("should offer 'min' and 'max' methods that are consistent with Int") {
      Percentages.PercentageInt(50).max(Percentages.PercentageInt(50)) shouldEqual Percentages.PercentageInt(50)
      Percentages.PercentageInt(50).min(Percentages.PercentageInt(50)) shouldEqual Percentages.PercentageInt(50)
    }
  }
}
