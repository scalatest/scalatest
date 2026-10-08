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
}