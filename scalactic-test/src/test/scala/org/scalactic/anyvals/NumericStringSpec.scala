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

import org.scalactic.Equality
import org.scalatest._
import org.scalatest.prop._
import OptionValues._

import scala.util.{Failure, Success, Try}

class NumericStringSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {

  import prop._

  implicit val numericStringGen: Generator[NumericString] =
    for (cs <- lists[Char](specificValues('0', '1', '2', '3', '4', '5', '6', '7', '8', '9'))) yield {
      if (cs.isEmpty) NumericString("000")
      else NumericString.ensuringValid(cs.mkString)
    }

  describe("A NumericString") {

    describe("should offer a from factory method that") {
      it("returns Some[NumericString] if the passed String contains only numeric characters") {
        NumericString.from("50").value.value shouldBe "50"
        NumericString.from("100").value.value shouldBe "100"
      }

      it("returns None if the passed String includes a non-numeric character") {
        NumericString.from("zero") shouldBe None
        NumericString.from("-1") shouldBe None
        NumericString.from("-99") shouldBe None
      }
    }
    describe("should offer an ensuringValid factory method that") {
      it("returns a NumericString if the passed String contains only numeric characters") {
        NumericString.ensuringValid("50").value shouldBe "50"
        NumericString.ensuringValid("100").value shouldBe "100"
      }

      it("throws AssertionError if the passed String includes a non-numeric character") {
        an [AssertionError] should be thrownBy NumericString.ensuringValid("zero")
        an [AssertionError] should be thrownBy NumericString.ensuringValid("-1")
        an [AssertionError] should be thrownBy NumericString.ensuringValid("-99")
      }
    }
    describe("should offer an isValid predicate method that") {
      it("returns true if the passed String contains only numeric characters") {
        NumericString.isValid("50") shouldBe true
        NumericString.isValid("100") shouldBe true
        NumericString.isValid("zero") shouldBe false
        NumericString.isValid("-1") shouldBe false
        NumericString.isValid("-99") shouldBe false
      }
    } 
    describe("should offer a fromOrElse factory method that") {
      it("returns a PosInt if the passed Int is greater than 0") {
        NumericString.fromOrElse("50", NumericString("42")).value shouldBe "50"
        NumericString.fromOrElse("100", NumericString("42")).value shouldBe "100"
      }
      it("returns a given default if the passed Int is NOT greater than 0") {
        NumericString.fromOrElse("zero", NumericString("42")).value shouldBe "42"
        NumericString.fromOrElse("-1", NumericString("42")).value shouldBe "42"
        NumericString.fromOrElse("-99", NumericString("42")).value shouldBe "42"
      }
    } 
    it("should offer an ensuringValid method that takes a String => String, throwing AssertionError if the result is invalid") {
      NumericString("33").ensuringValid(s => (s.toInt + 1).toString) shouldEqual NumericString("34")
      an [AssertionError] should be thrownBy { NumericString("33").ensuringValid(_ + "!") }
    }
    it("should offer a length method that is consistent with String") {
      forAll { (numStr: NumericString) =>
        numStr.length shouldEqual (numStr.value.length)
      }
    }
    it("should offer a charAt method that is consistent with String") {
      forAll { (numStr: NumericString, pint: PosInt) =>
        val idx = pint % numStr.length
        numStr.charAt(idx) shouldEqual numStr.value.charAt(idx)
      }
    }
  }
}

