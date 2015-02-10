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
package prop

class ConfigurationSuite extends FunSuite with Matchers {

  import Configuration._

  test("minSuccessful should not compile if less than 1") {
    "minSuccessful(0)" shouldNot compile
    "minSuccessful(-1)" shouldNot compile
    "minSuccessful(-2)" shouldNot compile
  }

  test("minSuccessful value is passed value, if valid") {
      minSuccessful(1).value.value should be (1)
      minSuccessful(2).value.value should be (2)
      minSuccessful(5678).value.value should be (5678)
  }

  test("maxDiscarded throws IAE if less than 0") {
    intercept[IllegalArgumentException] {
      maxDiscarded(-1)
    }
    intercept[IllegalArgumentException] {
      maxDiscarded(-2)
    }
  }

  test("maxDiscarded value is passed value, if valid") {
      maxDiscarded(0).value should be (0)
      maxDiscarded(1).value should be (1)
      maxDiscarded(2).value should be (2)
      maxDiscarded(5678).value should be (5678)
  }

  test("maxDiscardedFactor should not compile if less than 0 or a non-double") {
    "maxDiscardedFactor(-0.1)" shouldNot compile
    "maxDiscardedFactor(-100.0)" shouldNot compile
  }

  test("maxDiscardedFactor value is passed value, if valid") {
      maxDiscardedFactor(0.0).value.value should be (0.0)
      maxDiscardedFactor(1.0).value.value should be (1.0)
      maxDiscardedFactor(2.0).value.value should be (2.0)
      maxDiscardedFactor(5678.0).value.value should be (5678.0)
      maxDiscardedFactor(1.0).value.value should be (1.0)
  }

  test("minSize should not compile if less than 0") {
    "minSize(-1)" shouldNot compile
    "minSize(-2)" shouldNot compile
  }

  test("minSize value is passed value, if valid") {
      minSize(0).value.value should be (0)
      minSize(1).value.value should be (1)
      minSize(2).value.value should be (2)
      minSize(5678).value.value should be (5678)
  }

  test("maxSize throws IAE if less than 0") {
    intercept[IllegalArgumentException] {
      maxSize(-1)
    }
    intercept[IllegalArgumentException] {
      maxSize(-2)
    }
  }

  test("maxSize value is passed value, if valid") {
      maxSize(0).value should be (0)
      maxSize(1).value should be (1)
      maxSize(2).value should be (2)
      maxSize(5678).value should be (5678)
  }

  test("sizeRange should not compile if less than 0") {
    "sizeRange(-1)" shouldNot compile
    "sizeRange(-2)" shouldNot compile
  }

  test("sizeRange value is passed value, if valid") {
      sizeRange(0).value.value should be (0)
      sizeRange(1).value.value should be (1)
      sizeRange(2).value.value should be (2)
      sizeRange(5678).value.value should be (5678)
  }

  test("workers should not compile if less than 1") {
    "workers(0)" shouldNot compile
    "workers(-1)" shouldNot compile
    "workers(-2)" shouldNot compile
  }

  test("workers value is passed value, if valid") {
      workers(1).value.value should be (1)
      workers(2).value.value should be (2)
      workers(5678).value.value should be (5678)
  }
}
