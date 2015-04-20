/*
* Copyright 2001-2014 Artima, Inc.
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
import org.scalactic.CheckedEquality._

class PercentSpec extends FunSpec with Matchers {
  describe("A Percent") {
    describe("should offer a from factory method that") {
      it("returns Some[Percent] if the passed Int is between 0 and 100") {
        Percent.from(0).value.value shouldBe 0
        Percent.from(50).value.value shouldBe 50
        Percent.from(100).value.value shouldBe 100
      }
      it("returns None if the passed Int is NOT between 0 and 100") {
        Percent.from(101) shouldBe None
        Percent.from(1000) shouldBe None
        Percent.from(-1) shouldBe None
        Percent.from(-99) shouldBe None
      }
    }
    it("should have a pretty toString") {
      Percent.from(42).value.toString shouldBe "Percent(42)"
    }
    describe("when created with apply method") {

      it("should compile when 8 is passed in") {
        "Percent(8)" should compile
        Percent(8).value shouldEqual 8
        "Percent(0)" should compile
        Percent(0).value shouldEqual 0
        "Percent(100)" should compile
        Percent(100).value shouldEqual 100
      }

      it("should not compile when -1 is passed in") {
        "Percent(-1)" shouldNot compile
      }

      it("should not compile when 101 is passed in") {
        "Percent(101)" shouldNot compile
      }

      it("should not compile when -8 is passed in") {
        "Percent(-8)" shouldNot compile
      }

      it("should not compile when x is passed in") {
        val x: Int = -8
        "Percent(x)" shouldNot compile
      }
    }
  }
  describe("An LPercent") {
    describe("should offer a from factory method that") {
      it("returns Some[LPercent] if the passed Long is between 0 and 100") {
        LPercent.from(0L).value.value shouldBe 0L
        LPercent.from(50L).value.value shouldBe 50L
        LPercent.from(100L).value.value shouldBe 100L
      }
      it("returns None if the passed Long is NOT between 0 and 100") {
        LPercent.from(101L) shouldBe None
        LPercent.from(1000L) shouldBe None
        LPercent.from(-1L) shouldBe None
        LPercent.from(-99L) shouldBe None
      }
    }
    it("should have a pretty toString") {
      LPercent.from(42L).value.toString shouldBe "LPercent(42)"
    }
  }

  describe("An FPercent") {
    describe("should offer a from factory method that") {
      it("returns Some[FPercent] if the passed Float is between 0 and 100") {
        FPercent.from(0.0F).value.value shouldBe 0.0F
        FPercent.from(50.0F).value.value shouldBe 50.0F
        FPercent.from(100.0F).value.value shouldBe 100.0F
      }
      it("returns None if the passed Float is NOT between 0 and 100") {
        FPercent.from(100.00001F) shouldBe None
        FPercent.from(1000.1F) shouldBe None
        FPercent.from(-.000001F) shouldBe None
        FPercent.from(-99.999F) shouldBe None
      }
    }
    it("should have a pretty toString") {
      // SKIP-SCALATESTJS-START
      FPercent.from(42.0F).value.toString shouldBe "FPercent(42.0)"
      // SKIP-SCALATESTJS-END
      //SCALATESTJS-ONLY FPercent.from(42.0F).value.toString shouldBe "FPercent(42)"

      // SKIP-SCALATESTJS-START
      FPercent.from(42.42F).value.toString shouldBe "FPercent(42.42)"
      // SKIP-SCALATESTJS-END
      //SCALATESTJS-ONLY FPercent.from(42.42F).value.toString shouldBe "FPercent(42.41999816894531)"
    }
  }
  describe("A DPercent") {
    describe("should offer a from factory method that") {
      it("returns Some[DPercent] if the passed Double is between 0 and 100") {
        DPercent.from(0.0).value.value shouldBe 0.0
        DPercent.from(50.0).value.value shouldBe 50.0
        DPercent.from(100.0).value.value shouldBe 100.0
      }
      it("returns None if the passed Double is NOT between 0 and 100") {
        DPercent.from(100.000001) shouldBe None
        DPercent.from(1000.1) shouldBe None
        DPercent.from(-.000001) shouldBe None
        DPercent.from(-99.999) shouldBe None
      }
    }
    it("should have a pretty toString") {
      // SKIP-SCALATESTJS-START
      DPercent.from(42.0).value.toString shouldBe "DPercent(42.0)"
      // SKIP-SCALATESTJS-END
      //SCALATESTJS-ONLY DPercent.from(42.0).value.toString shouldBe "DPercent(42)"

      DPercent.from(42.42).value.toString shouldBe "DPercent(42.42)"
    }
  }
}

