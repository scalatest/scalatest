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

import org.scalactic.opaquetypes.Regexes.RegexString

class RegexStringSpec extends funspec.AnyFunSpec with matchers.should.Matchers with OptionValues {

  describe("RegexString") {

    describe("compile-time apply") {

      it("should accept String literals that are valid regular expressions") {
        RegexString("[a-z]+").value shouldBe "[a-z]+"
        RegexString("\\d{3}").value shouldBe "\\d{3}"
        RegexString("").value shouldBe ""
        RegexString("abc").value shouldBe "abc"
      }

      it("should not compile when a literal is an invalid regular expression") {
        """RegexString("[a-z")""" shouldNot compile
        """RegexString("(unclosed")""" shouldNot compile
        """RegexString("[z-a]")""" shouldNot compile
      }

      it("should not compile when a non-literal is passed in") {
        val s: String = "[a-z"
        "RegexString(s)" shouldNot compile
      }
    }

    describe("from") {

      it("should return Some(RegexString) for valid regular expressions") {
        RegexString.from("[a-z]+").value.value shouldBe "[a-z]+"
        RegexString.from("\\d+").value.value shouldBe "\\d+"
        RegexString.from("").value.value shouldBe ""
      }

      it("should return None for invalid regular expressions") {
        RegexString.from("[a-z") shouldBe None
        RegexString.from("(unclosed") shouldBe None
      }
    }

    describe("ensuringValid") {

      it("should return the RegexString for valid regular expressions") {
        RegexString.ensuringValid("[a-z]+").value shouldBe "[a-z]+"
      }

      it("should throw AssertionError for invalid regular expressions") {
        an[AssertionError] should be thrownBy RegexString.ensuringValid("[a-z")
      }

      it("should offer an ensuringValid method that takes a String => String, throwing AssertionError if the result is invalid") {
        RegexString("33").ensuringValid(s => (s.toInt + 1).toString).value shouldBe "34"
        an[AssertionError] should be thrownBy { RegexString("33").ensuringValid(_ + "(") }
      }
    }

    describe("isValid") {

      it("should return true for valid regular expressions") {
        RegexString.isValid("[a-z]+") shouldBe true
        RegexString.isValid("") shouldBe true
      }

      it("should return false for invalid regular expressions") {
        RegexString.isValid("[a-z") shouldBe false
      }
    }

    describe("tryingValid") {

      it("should return Success for valid regular expressions") {
        RegexString.tryingValid("[a-z]+").get.value shouldBe "[a-z]+"
      }

      it("should return Failure for invalid regular expressions") {
        RegexString.tryingValid("[a-z") should matchPattern { case Failure(_: AssertionError) => }
      }
    }

    describe("Or / Validation helpers") {

      it("should return Good for valid input and Bad otherwise") {
        RegexString.goodOrElse("[a-z]+")(_ => "err") match {
          case Good(v) => v.value shouldBe "[a-z]+"
          case Bad(_) => fail("expected Good")
        }
        RegexString.goodOrElse("[a-z")(_ => "err") shouldBe Bad("err")
      }

      it("should return Right for valid input and Left otherwise") {
        RegexString.rightOrElse("[a-z]+")(_ => "err") match {
          case Right(v) => v.value shouldBe "[a-z]+"
          case Left(_) => fail("expected Right")
        }
        RegexString.rightOrElse("[a-z")(_ => "err") shouldBe Left("err")
      }

      it("should return Pass for valid input and Fail otherwise") {
        RegexString.passOrElse("[a-z]+")(_ => "err") shouldBe Pass
        RegexString.passOrElse("[a-z")(_ => "err") shouldBe Fail("err")
      }

      it("should return the default from fromOrElse when invalid") {
        RegexString.fromOrElse("[a-z]+", RegexString("abc")).value shouldBe "[a-z]+"
        RegexString.fromOrElse("[a-z", RegexString("abc")).value shouldBe "abc"
      }
    }

    describe("extension methods") {

      it("should expose value, length and isEmpty") {
        RegexString("[a-z]+").value shouldBe "[a-z]+"
        RegexString("abcde").length shouldBe 5
        RegexString("").isEmpty shouldBe true
        RegexString("a").isEmpty shouldBe false
      }

      it("should support matches, which requires a full-string match") {
        RegexString("abc").matches("[a-z]+") shouldBe true
        RegexString("abc").matches("\\d+") shouldBe false
      }

      it("should support find, which matches anywhere in the string") {
        RegexString("xabcx").find("[a-z]+") shouldBe true
        RegexString("xabcx").find("\\d+") shouldBe false
      }

      it("should support replaceAll and replaceFirst") {
        RegexString("aaa").replaceAll("a", "b") shouldBe "bbb"
        RegexString("aaa").replaceFirst("a", "b") shouldBe "baa"
      }

      it("should support split") {
        RegexString("a,b,c").split(",").toList shouldBe List("a", "b", "c")
      }
    }

    describe("Ordering") {
      it("should sort by String ordering") {
        List(RegexString("b"), RegexString("a")).sorted.map(_.value) shouldBe List("a", "b")
      }
    }

    describe("interoperability") {
      it("should convert to String") {
        val s: String = RegexString("[a-z]+")
        s shouldBe "[a-z]+"
      }

      it("should have a toString that yields the underlying String, unlike anyvals RegexString") {
        RegexString("[a-z]+").toString shouldBe "[a-z]+"
      }
    }
  }
}