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

import org.scalatest.OptionValues._
import org.scalatest.SharedHelpers.thisLineNumber
import org.scalatest.exceptions.TestFailedException
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class OptionValuesSpec extends AnyFunSpec {

  describe("value on Option") {

    it("should return the value inside an option if that option is defined") {

      val o: Option[String] = Some("hi there")
      o.value should === ("hi there")
      o.value should startWith ("hi")
    }

    it("should throw TestFailedException if that option is empty") {

      val o: Option[String] = None
      val caught =
        the [TestFailedException] thrownBy {
          o.value should startWith ("hi")
        }
      caught.failedCodeLineNumber.value should equal (thisLineNumber - 2)
      caught.failedCodeFileName.value should be ("OptionValuesSpec.scala")
      caught.message.value should be (Resources.optionValueNotDefined)
    }

    it("should allow an immediate application of parens to invoke apply on the type contained in the Option") {
      val opt: Option[Map[String, Int]] = Some(Map("I" -> 1, "II" -> 2))
      opt.value("II") shouldBe 2
    }

    it("should be able to used with EitherValues") {
      class TestSpec extends AnyFunSpec with OptionValues with EitherValues
    }
  }
}


