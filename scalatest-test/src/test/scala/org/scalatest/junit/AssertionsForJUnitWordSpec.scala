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
package org.scalatest.junit

import org.scalatest._
import _root_.junit.framework.AssertionFailedError
import org.junit.Test

class AssertionsForJUnitSpec extends JUnitSuite {

  @Test def assertionsForJUnit3TraitShouldThrowAssertionFailedErrorFromFailedAssertExpressions() {

    intercept[AssertionFailedError] {
      assert(false)
    }

    intercept[AssertionFailedError] {
      assert(1 === 2)
    }

    val caught1 = intercept[AssertionFailedError] {
      assert(false, "hi there")
    }
    assert(caught1.getMessage === "hi there")

    val caught2 = intercept[AssertionFailedError] {
      assert(1 === 2, "hi there")
    }
    assert(caught2.getMessage === "1 did not equal 2 hi there")
  }

  @Test def assertionsForJUnit3TraitShouldThrowAssertionFailedErrorFromFailedExpectExpressions() {

    intercept[AssertionFailedError] {
      assertResult(1) { 2 }
    }

    val caught = intercept[AssertionFailedError] {
      assertResult(1, "hi there") { 2 }
    }
    assert(caught.getMessage === "Expected 1, but got 2 hi there")
  }

  @Test def assertionsForJUnit3TraitShouldThrowAssertionFailedErrorFromFailedInterceptExpressions() {

    val caught1 = intercept[AssertionFailedError] {
      intercept[IllegalArgumentException] {
        "hi".charAt(-1)
      }
    }
    assert(caught1.getMessage === "Expected exception java.lang.IllegalArgumentException to be thrown, but java.lang.StringIndexOutOfBoundsException was thrown")

    val caught2 = intercept[AssertionFailedError] {
      intercept[IllegalArgumentException] {
        "hi"
      }
    }
    assert(caught2.getMessage === "Expected exception java.lang.IllegalArgumentException to be thrown, but no exception was thrown")
  }

  @Test def assertionsForJUnit3TraitShouldThrowAssertionFailedErrorFromFail() {

    val illegalArgumentException = new IllegalArgumentException

    intercept[AssertionFailedError] {
      fail()
    }

    val caught1 = intercept[AssertionFailedError] {
      fail("hi there")
    }
    assert(caught1.getMessage === "hi there")

    val caught2 = intercept[AssertionFailedError] {
      fail(illegalArgumentException)
    }
    assert(caught2.getCause eq illegalArgumentException)

    val caught3 = intercept[AssertionFailedError] {
      fail("hi there", illegalArgumentException)
    }
    assert(caught3.getMessage === "hi there")
    assert(caught3.getCause eq illegalArgumentException)
  }

}