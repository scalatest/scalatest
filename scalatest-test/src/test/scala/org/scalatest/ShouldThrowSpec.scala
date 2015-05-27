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

import org.scalatest.exceptions.TestFailedException
import SharedHelpers.thisLineNumber
import Matchers._
import OptionValues._

class ShouldThrowSpec extends FunSpec {

  describe("The a [ExceptionType] should be thrownBy { ... } syntax") {

    // TODO: "hi".charAt(-1) is throwing UndefinedBehaviorError, not the expected StringIndexOutOfBoundsException, we'll re-enable this when scala-js fix this.
    // SKIP-SCALATESTJS-START
    it("fail if a different exception is thrown") {
      val caught1 = intercept[TestFailedException] {
        an [IllegalArgumentException] should be thrownBy { "hi".charAt(-1) }
      }
      assert(caught1.getMessage === "Expected exception java.lang.IllegalArgumentException to be thrown, but java.lang.StringIndexOutOfBoundsException was thrown.")
    }
    // SKIP-SCALATESTJS-END

    it("fail if no exception is thrown") {
      val caught2 = intercept[TestFailedException] {
        an [IllegalArgumentException] should be thrownBy { "hi" }
      }
      assert(caught2.getMessage === "Expected exception java.lang.IllegalArgumentException to be thrown, but no exception was thrown.")
    }

    // TODO: "hi".charAt(-1) is throwing UndefinedBehaviorError, not the expected StringIndexOutOfBoundsException, we'll re-enable this when scala-js fix this.
    // SKIP-SCALATESTJS-START
    it("succeed if the expected exception is thrown") {
      a [StringIndexOutOfBoundsException] should be thrownBy { "hi".charAt(-1) }
    }
    // SKIP-SCALATESTJS-END

    // TODO: "hi".charAt(-1) is throwing UndefinedBehaviorError, not the expected StringIndexOutOfBoundsException, we'll re-enable this when scala-js fix this.
    // SKIP-SCALATESTJS-START
    it("succeed if a subtype of the expected exception is thrown, where the expected type is a class") {
      a [Exception] should be thrownBy { "hi".charAt(-1) }
    }
    // SKIP-SCALATESTJS-END

    it("succeed if a subtype of the expected exception is thrown, where the expected type is a trait") {
      trait Excitement
      def kaboom() { throw new Exception with Excitement }
      a [Excitement] should be thrownBy { kaboom() }
    }
    
    it("return the caught exception") {
      def kaboom() { throw new Exception("howdy") }
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
