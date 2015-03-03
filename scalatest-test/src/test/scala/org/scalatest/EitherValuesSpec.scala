/*
 * Copyright 2001-2013 Artima, Inc.
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

import org.scalatest.EitherValues._
import org.scalatest.OptionValues._
import org.scalatest.SharedHelpers.thisLineNumber
import Matchers._
import org.scalatest.exceptions.TestFailedException

class EitherValuesSpec extends FunSpec {
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
      caught.message.value should be (Resources.eitherLeftValueNotDefined)
    }
    
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
      caught.message.value should be (Resources.eitherRightValueNotDefined)
    }
  } 
}
