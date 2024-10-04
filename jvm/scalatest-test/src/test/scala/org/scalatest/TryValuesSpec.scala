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

import org.scalatest.TryValues._
import org.scalatest.OptionValues._
import matchers.should.Matchers._
import org.scalatest.SharedHelpers.thisLineNumber
import scala.util.Try
import scala.util.Failure
import scala.util.Success
import org.scalatest.exceptions.TestFailedException
import org.scalatest.matchers
import org.scalatest.funspec.AnyFunSpec

class TryValuesSpec extends AnyFunSpec {

  describe("invoking failure on Try") {

    it("should return the Try as a Failure if it is one") {
      val ex = new Exception
      val t: Try[String] = new Failure(ex)
      t.failure.exception should be theSameInstanceAs ex
    }

    it("should throw TestFailedException if the Try is a Success") {
      val t: Try[String] = Success("hi there")
      val caught = 
        the [TestFailedException] thrownBy {
          t.failure.exception should equal (new Exception)
        }
      caught.failedCodeLineNumber.value should equal (thisLineNumber - 2)
      caught.failedCodeFileName.value should be ("TryValuesSpec.scala")
      caught.message.value should be (Resources.tryNotAFailure(t))
    }

    // SKIP-SCALATESTJS-START
    it("should throw a serializable TestFailedException") {
      val objectOutputStream: ObjectOutputStream = new ObjectOutputStream(new ByteArrayOutputStream())
      val t: Try[String] = Success("hi there")
      val caught =
        the [TestFailedException] thrownBy {
          t.failure.exception should equal (new Exception)
        }

      noException should be thrownBy objectOutputStream.writeObject(caught)
    }
    // SKIP-SCALATESTJS-END

    it("should return the value inside a Try if it is a Success") {
      val t: Try[String] = Success("hi there")
      t.success.value should equal ("hi there")
    }

    it("should throw TestFailedException if right.value is empty") {
      val ex = new Exception
      val t: Try[String] = Failure(ex)
      val caught = 
        the [TestFailedException] thrownBy {
          t.success.value should startWith ("hi")
        }
      caught.failedCodeLineNumber.value should equal (thisLineNumber - 2)
      caught.failedCodeFileName.value should be ("TryValuesSpec.scala")
      caught.message.value should be (Resources.tryNotASuccess(t))
    }
  } 
}

