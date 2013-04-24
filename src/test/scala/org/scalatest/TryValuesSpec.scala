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

import org.scalatest.TryValues._
import org.scalatest.OptionValues._
import org.scalatest.Matchers._
import org.scalatest.SharedHelpers.thisLineNumber
import scala.util.Try
import scala.util.Failure
import scala.util.Success
/* Uncomment after remove type aliases in org.scalatest package object
import org.scalatest.exceptions.TestFailedException
*/

class TryValuesSpec extends FunSpec {

  describe("invoking failure on Try") {

    it("should return the Try as a Failure if it is one") {
      val ex = new Exception
      val t: Try[String] = new Failure(ex)
      t.failure.exception should be theSameInstanceAs ex
    }

    it("should throw TestFailedException if the Try is a Success") {
      val t: Try[String] = Success("hi there")
      val caught = 
        evaluating {
          t.failure.exception should equal (new Exception)
        } should produce [TestFailedException]
      caught.failedCodeLineNumber.value should equal (thisLineNumber - 2)
      caught.failedCodeFileName.value should be ("TryValuesSpec.scala")
      caught.message.value should be (Resources("tryNotAFailure"))
    }

    it("should return the value inside a Try if it is a Success") {
      val t: Try[String] = Success("hi there")
      t.success.value should equal ("hi there")
    }

    it("should throw TestFailedException if right.value is empty") {
      val ex = new Exception
      val t: Try[String] = Failure(ex)
      val caught = 
        evaluating {
          t.success.value should startWith ("hi")
        } should produce [TestFailedException]
      caught.failedCodeLineNumber.value should equal (thisLineNumber - 2)
      caught.failedCodeFileName.value should be ("TryValuesSpec.scala")
      caught.message.value should be (Resources("tryNotASuccess"))
    }
  } 
}

