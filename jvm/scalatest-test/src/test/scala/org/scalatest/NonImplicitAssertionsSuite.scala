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

import org.scalactic._
import org.scalatest.exceptions.TestFailedException
import org.scalatest.funspec.AnyFunSpec

class NonImplicitAssertionsSuite extends funsuite.AnyFunSuite with NonImplicitAssertions {
  test("make sure all the non-implicit assertions work") {
    assert(1 + 1 == 2)
    val thrown =
      intercept[TestFailedException] {
        assertResult(2) { 1 + 1 + 1 }
      }
    assertResult("Expected 2, but got 3") { thrown.getMessage }
  }
  test("TypeCheckedTripleEquals should work still if mixed in after NonImplicitAssertions") {
    class MySpec extends AnyFunSpec with NonImplicitAssertions with TypeCheckedTripleEquals {
      assert(1 === 1)
    }
  }
}

