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
import scala.util.{Try, Success, Failure}

import org.scalactic.opaquetypes.Numerics.*

class NumericStringSpec extends funspec.AnyFunSpec with matchers.should.Matchers with OptionValues {

  private val identityFn: String => String = _.toString

  describe("NumericString") {

    describe("compile-time apply") {

      it("should accept digit string literals") {
        NumericString("0")
        NumericString("1")
        NumericString("9")
        NumericString("0123456789")
      }

      it("should reject non-digit string literals at runtime") {
        NumericString.from("abc").isEmpty shouldBe true
      }
    }

    describe("from") {

      it("should return Some(NumericString) for non-empty digit strings") {
        NumericString.from("0") shouldBe defined
        NumericString.from("0").get.value shouldBe "0"
        NumericString.from("123") shouldBe defined
        NumericString.from("123").get.value shouldBe "123"
        NumericString.from("0123456789") shouldBe defined
      }

      it("should return None for empty strings") {
        NumericString.from("") shouldBe None
      }

      it("should return None for strings with non-digit characters") {
        NumericString.from("abc") shouldBe None
        NumericString.from("12abc") shouldBe None
        NumericString.from("-5") shouldBe None
        NumericString.from("+3") shouldBe None
        NumericString.from(" 7") shouldBe None
        NumericString.from("3.") shouldBe None
        NumericString.from("1.2") shouldBe None
        NumericString.from("1e5") shouldBe None
      }
    }

    describe("ensuringValid") {

      it("should return the NumericString for valid input") {
        NumericString.ensuringValid("0").value shouldBe "0"
        NumericString.ensuringValid("123").value shouldBe "123"
      }

      it("should throw AssertionError for invalid input") {
        the [AssertionError] thrownBy {
          NumericString.ensuringValid("")
        } should have message Resources.invalidNumericString
        the [AssertionError] thrownBy {
          NumericString.ensuringValid("abc")
        } should have message Resources.invalidNumericString
        the [AssertionError] thrownBy {
          NumericString.ensuringValid("12abc")
        } should have message Resources.invalidNumericString
      }
    }

    describe("tryingValid") {

      it("should return Success for valid input") {
        NumericString.tryingValid("0") shouldBe Success(NumericString("0"))
        NumericString.tryingValid("123") shouldBe Success(NumericString("123"))
      }

      it("should return Failure for invalid input") {
        NumericString.tryingValid("") match {
          case Failure(e) => e.getMessage shouldEqual Resources.invalidNumericString
          case _ => fail("expected Failure")
        }
        NumericString.tryingValid("abc") match {
          case Failure(e) => e.getMessage shouldEqual Resources.invalidNumericString
          case _ => fail("expected Failure")
        }
      }
    }

    describe("isValid") {

      it("should return true for non-empty digit strings") {
        NumericString.isValid("0") shouldBe true
        NumericString.isValid("123") shouldBe true
        NumericString.isValid("0123456789") shouldBe true
      }

      it("should return false for empty strings") {
        NumericString.isValid("") shouldBe false
      }

      it("should return false for strings with non-digit characters") {
        NumericString.isValid("abc") shouldBe false
        NumericString.isValid("-5") shouldBe false
        NumericString.isValid("+3") shouldBe false
        NumericString.isValid(" 7") shouldBe false
        NumericString.isValid("1.2") shouldBe false
      }
    }

    describe("passOrElse") {

      it("should return Pass for valid input") {
        NumericString.passOrElse("123")(identityFn) shouldBe Pass
        NumericString.passOrElse("0")(identityFn) shouldBe Pass
      }

      it("should return Fail for invalid input") {
        NumericString.passOrElse("")(identityFn) shouldBe Fail("")
        NumericString.passOrElse("abc")(identityFn) shouldBe Fail("abc")
      }
    }

    describe("goodOrElse") {

      it("should return Good for valid input") {
        NumericString.goodOrElse("123")(identityFn) match {
          case Good(v) => v.value shouldBe "123"
          case Bad(_) => fail("expected Good")
        }
      }

      it("should return Bad for invalid input") {
        NumericString.goodOrElse("")(identityFn) shouldBe Bad("")
        NumericString.goodOrElse("abc")(identityFn) shouldBe Bad("abc")
      }
    }

    describe("rightOrElse") {

      it("should return Right for valid input") {
        NumericString.rightOrElse("123")(identityFn) match {
          case Right(v) => v.value shouldBe "123"
          case Left(_) => fail("expected Right")
        }
      }

      it("should return Left for invalid input") {
        NumericString.rightOrElse("")(identityFn) match {
          case Left(v) => v shouldBe ""
          case _ => fail("expected Left")
        }
        NumericString.rightOrElse("abc")(identityFn) match {
          case Left(v) => v shouldBe "abc"
          case _ => fail("expected Left")
        }
      }
    }

    describe("fromOrElse") {

      it("should return the value for valid input") {
        NumericString.fromOrElse("123", NumericString("0")).value shouldBe "123"
      }

      it("should return the default for invalid input") {
        NumericString.fromOrElse("", NumericString("0")).value shouldBe "0"
        NumericString.fromOrElse("abc", NumericString("5")).value shouldBe "5"
      }
    }

    describe("MinValue and MaxValue") {

      it("should have MinValue of '0'") {
        NumericString.MinValue.value shouldBe "0"
      }

      it("should have MaxValue of '9'") {
        NumericString.MaxValue.value shouldBe "9"
      }
    }

    describe("value and toString") {

      it("should expose the underlying string via .value") {
        NumericString("123").value shouldBe "123"
        NumericString("0").value shouldBe "0"
      }

      it("should have a pretty toString") {
        NumericString("123").toString shouldBe "123"
        NumericString("0").toString shouldBe "0"
      }
    }

    describe("string operations") {

      it("should have a length method") {
        NumericString("0").length shouldBe 1
        NumericString("123").length shouldBe 3
        NumericString("0123456789").length shouldBe 10
      }

      it("should support apply(index) to get a character") {
        NumericString("123")(0) shouldBe '1'
        NumericString("123")(1) shouldBe '2'
        NumericString("123")(2) shouldBe '3'
      }

      it("should support ++ with String") {
        val result1: String = NumericString("123") ++ "456"
        result1 shouldBe "123456"
        val result2: String = NumericString("0") ++ "0"
        result2 shouldBe "00"
      }

      it("should support ++ with NumericString") {
        val r1: NumericString = NumericString("123") ++ NumericString("456")
        r1.value shouldBe "123456"
        val r2: NumericString = NumericString("0") ++ NumericString("0")
        r2.value shouldBe "00"
      }

      it("should support head and last") {
        NumericString("123").head shouldBe '1'
        NumericString("123").last shouldBe '3'
      }

      it("should support headOption and lastOption") {
        NumericString("123").headOption shouldBe Some('1')
        NumericString("123").lastOption shouldBe Some('3')
      }

      it("should support isEmpty") {
        NumericString("123").isEmpty shouldBe false
      }

      it("should support contains") {
        NumericString("123").contains('2') shouldBe true
        NumericString("123").contains('5') shouldBe false
      }

      it("should support startsWith and endsWith") {
        NumericString("123").startsWith("1") shouldBe true
        NumericString("123").startsWith("12") shouldBe true
        NumericString("123").endsWith("3") shouldBe true
        NumericString("123").endsWith("23") shouldBe true
      }

      it("should support substring via slice") {
        val s: NumericString = NumericString("12345").slice(1, 4)
        s.value shouldBe "234"
      }

      it("should support reverse") {
        val r1: NumericString = NumericString("123").reverse
        r1.value shouldBe "321"
        val r2: NumericString = NumericString("12321").reverse
        r2.value shouldBe "12321"
      }

      it("should support toList") {
        NumericString("123").toList shouldBe List('1', '2', '3')
      }

      it("should support toVector") {
        NumericString("123").toVector shouldBe Vector('1', '2', '3')
      }

      it("should support iterator") {
        NumericString("123").iterator.toList shouldBe List('1', '2', '3')
      }

      it("should be iterable in for-comprehension") {
        val result = for (c <- NumericString("123")) yield (c - '0') * 2
        result.toList shouldBe List(2, 4, 6)
      }
    }

    describe("equality and ordering") {

      it("should support equality via stringValue") {
        NumericString("123").value shouldBe NumericString("123").value
        NumericString("123").value should not be NumericString("456").value
        NumericString("0").value shouldBe NumericString("0").value
      }

      it("should support Ordering") {
        import scala.math.Ordering.Implicits._
        (NumericString("123") < NumericString("456")) shouldBe true
        (NumericString("456") > NumericString("123")) shouldBe true
        (NumericString("123") <= NumericString("123")) shouldBe true
        (NumericString("123") >= NumericString("123")) shouldBe true
      }
    }

    describe("widenings") {

      it("should be convertible to String") {
        val s: String = NumericString("123")
        s shouldBe "123"
      }

      it("should compile to CharSequence via stringValue") {
        val cs: CharSequence = NumericString("123").value
        cs.toString shouldBe "123"
      }
    }
  }
}
