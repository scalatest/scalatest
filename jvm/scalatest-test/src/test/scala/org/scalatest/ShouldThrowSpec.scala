/*
 * Copyright 2001-2025 Artima, Inc.
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

import org.scalatest.exceptions.TestFailedException
import SharedHelpers.thisLineNumber
import OptionValues._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class ShouldThrowSpec extends AnyFunSpec {

  describe("The a [ExceptionType] should be thrownBy { ... } syntax") {

    it("fail if a different exception is thrown") {
      val caught1 = intercept[TestFailedException] {
        an [UnsupportedOperationException] should be thrownBy { "hi".toInt }
      }
      assert(caught1.getMessage === "Expected exception java.lang.UnsupportedOperationException to be thrown, but java.lang.NumberFormatException was thrown")
    }

    it("fail if no exception is thrown") {
      val caught2 = intercept[TestFailedException] {
        an [IllegalArgumentException] should be thrownBy { "hi" }
      }
      assert(caught2.getMessage === "Expected exception java.lang.IllegalArgumentException to be thrown, but no exception was thrown")
    }

    it("succeed if the expected exception is thrown") {
      a [NumberFormatException] should be thrownBy { "hi".toInt }
    }

    it("succeed if a subtype of the expected exception is thrown, where the expected type is a class") {
      a [Exception] should be thrownBy { "hi".toInt }
    }

    it("succeed if a subtype of the expected exception is thrown, where the expected type is a trait") {
      trait Excitement
      def kaboom(): Unit = { throw new Exception with Excitement }
      a [Excitement] should be thrownBy { kaboom() }
    }
    
    it("return the caught exception") {
      def kaboom(): Unit = { throw new Exception("howdy") }
      val thrown = the [Exception] thrownBy { kaboom() }
      thrown.getMessage should === ("howdy")
    }  
    
    it("include that wrong exception as the TFE's cause") {
      val wrongException = new RuntimeException("oops!")
      val caught =
        intercept[TestFailedException] {
          an [IllegalArgumentException] should be thrownBy { throw wrongException }
        }
      assert(caught.cause.value eq wrongException)
    }
  }
}
