/*
* Copyright 2001-2024 Artima, Inc.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package org.scalactic.anyvals

import org.scalatest._
import scala.collection.mutable.WrappedArray
import OptionValues._
//import org.scalactic.CheckedEquality._

class PercentageIntSpec extends funspec.AnyFunSpec with matchers.should.Matchers {
  describe("A PercentageInt") {
    describe("should offer a from factory method that") {
      it("returns Some[PercentageInt] if the passed Int is between 0 and 100") {
        PercentageInt.from(0).value.value shouldBe 0
        PercentageInt.from(50).value.value shouldBe 50
        PercentageInt.from(100).value.value shouldBe 100
      }
      it("returns None if the passed Int is NOT between 0 and 100") {
        PercentageInt.from(101) shouldBe None
        PercentageInt.from(1000) shouldBe None
        PercentageInt.from(-1) shouldBe None
        PercentageInt.from(-99) shouldBe None
      }
    }
    describe("should offer an ensuringValid factory method that") {
      it("returns PercentageInt if the passed Int is between 0 and 100") {
        PercentageInt.ensuringValid(0).value shouldBe 0
        PercentageInt.ensuringValid(50).value shouldBe 50
        PercentageInt.ensuringValid(100).value shouldBe 100
      }
      it("throws AssertionError if the passed Int is NOT between 0 and 100") {
        an [AssertionError] should be thrownBy PercentageInt.ensuringValid(101)
        an [AssertionError] should be thrownBy PercentageInt.ensuringValid(1000)
        an [AssertionError] should be thrownBy PercentageInt.ensuringValid(-1)
        an [AssertionError] should be thrownBy PercentageInt.ensuringValid(-99)
      }
    }
    it("should have a pretty toString") {
      PercentageInt.from(42).value.toString shouldBe "PercentageInt(42)"
    }
    describe("when created with apply method") {

      it("should compile when 8 is passed in") {
        "PercentageInt(8)" should compile
        PercentageInt(8).value shouldEqual 8
        "PercentageInt(0)" should compile
        PercentageInt(0).value shouldEqual 0
        "PercentageInt(100)" should compile
        PercentageInt(100).value shouldEqual 100
      }

      it("should not compile when -1 is passed in") {
        "PercentageInt(-1)" shouldNot compile
      }

      it("should not compile when 101 is passed in") {
        "PercentageInt(101)" shouldNot compile
      }

      it("should not compile when -8 is passed in") {
        "PercentageInt(-8)" shouldNot compile
      }

      it("should not compile when x is passed in") {
        val x: Int = -8
        "PercentageInt(x)" shouldNot compile
      }
    }
  }
}

