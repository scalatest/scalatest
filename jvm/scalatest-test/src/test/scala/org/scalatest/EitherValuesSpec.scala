/*
 * Copyright 2001-2024 Artima, Inc.
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
package org.scalatest

// SKIP-SCALATESTJS-START
import java.io.{ObjectOutputStream, ByteArrayOutputStream}
// SKIP-SCALATESTJS-END

import org.scalatest.EitherValues._
import org.scalatest.OptionValues._
import org.scalatest.SharedHelpers.thisLineNumber
import org.scalatest.exceptions.TestFailedException
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._
// SKIP-SCALATESTJS,NATIVE-START
import SharedHelpers.serializeRoundtrip
// SKIP-SCALATESTJS,NATIVE-END

class EitherValuesSpec extends AnyFunSpec {
  describe("values on Either") {

    it("should return the left value inside an either if left.value is defined") {
      val e: Either[String, String] = Left("hi there")
      e.left.value should === ("hi there")
      e.left.value should startWith ("hi")
    }

    it("should throw TestFailedException if left.value is empty") {
      val e: Either[String, String] = Right("hi there")
      val caught = 
        the [TestFailedException] thrownBy {
          e.left.value should startWith ("hi")
        }
      caught.failedCodeLineNumber.value should equal (thisLineNumber - 2)
      caught.failedCodeFileName.value should be ("EitherValuesSpec.scala")
      caught.message.value should be (Resources.eitherLeftValueNotDefined(e))
    }
    
    // SKIP-SCALATESTJS-START
    it("should throw a serialized TestFailedException") {
      val objectOutputStream: ObjectOutputStream = new ObjectOutputStream(new ByteArrayOutputStream())
      val e: Either[String, String] = Right("hi there")
      val caught =
        the [TestFailedException] thrownBy {
          e.left.value should startWith ("hi")
        }

      noException should be thrownBy objectOutputStream.writeObject(caught)
    }
    // SKIP-SCALATESTJS-END

    it("should return the right value inside an either if right.value is defined") {
      val e: Either[String, String] = Right("hi there")
      e.right.value should === ("hi there")
      e.right.value should startWith ("hi")
    }
    
    it("should throw TestFailedException if right.value is empty") {
      val e: Either[String, String] = Left("hi there")
      val caught = 
        the [TestFailedException] thrownBy {
          e.right.value should startWith ("hi")
        }
      caught.failedCodeLineNumber.value should equal (thisLineNumber - 2)
      caught.failedCodeFileName.value should be ("EitherValuesSpec.scala")
      caught.message.value should be (Resources.eitherRightValueNotDefined(e))
    }

    it("should return the right value inside an either if the either is a Right") {
      val e: Either[String, String] = Right("hi there")
      e.value should === ("hi there")
      e.value should startWith ("hi")
    }

    it("should throw TestFailedException if either is not a Right") {
      val e: Either[String, String] = Left("hi there")
      val caught =
        the [TestFailedException] thrownBy {
          e.value should startWith ("hi")
        }
      caught.failedCodeLineNumber.value should equal (thisLineNumber - 2)
      caught.failedCodeFileName.value should be ("EitherValuesSpec.scala")
      caught.message.value should be (Resources.eitherValueNotDefined(e))
    }

    it("should allow an immediate application of parens to invoke apply on the type contained in the Left") {
      val lefty: Either[Map[String, Int], String] = Left(Map("I" -> 1, "II" -> 2))
      lefty.left.value("II") shouldBe 2
    }

    it("should allow an immediate application of parens to invoke apply on the type contained in the Right") {
      val righty: Either[String, Map[String, Int]] = Right(Map("I" -> 1, "II" -> 2))
      righty.right.value("II") shouldBe 2
    }

    it("should allow an immediate application of parens to invoke apply on the type contained in the Right if the Either is a Right") {
      val righty: Either[String, Map[String, Int]] = Right(Map("I" -> 1, "II" -> 2))
      righty.value("II") shouldBe 2
    }

    it("should be able to used with OptionValues") {
      class TestSpec extends AnyFunSpec with EitherValues with OptionValues
    }

    // SKIP-SCALATESTJS,NATIVE-START
    it("should throw TestFailedException that is serializable") {
      class TestEitherValues extends AnyFunSuite with EitherValues
      val spec = new TestEitherValues
      val e = Left("error"): Either[String, Int]
      val v = spec.convertEitherToValuable(e)
      val caught = intercept[TestFailedException] {
        v.value
      }
      serializeRoundtrip(caught)
    }
    // SKIP-SCALATESTJS,NATIVE-END
  } 
}
