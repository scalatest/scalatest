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

import Configuration._

class PropertyCheckConfigParamSuite extends FunSuite with Matchers {

/*
 require(minSuccessfulTests > 0)
 require(maxDiscardedTests >= 0)
 require(minSize >= 0)
 require(maxSize >= minSize)
 require(workers > 0)
*/

  test("minSuccussful should not compile if less than 1") {
    "MinSuccessful(0)" shouldNot compile
    "MinSuccessful(-1)" shouldNot compile
    "MinSuccessful(-2)" shouldNot compile
  }

  test("minSuccussful value is passed value, if valid") {
      MinSuccessful(1).value.value should be (1)
      MinSuccessful(2).value.value should be (2)
      MinSuccessful(5678).value.value should be (5678)
  }

  test("maxDiscarded throws IAE if less than 0") {
    intercept[IllegalArgumentException] {
      MaxDiscarded(-1)
    }
    intercept[IllegalArgumentException] {
      MaxDiscarded(-2)
    }
  }

  test("maxDiscarded value is passed value, if valid") {
      MaxDiscarded(0).value should be (0)
      MaxDiscarded(1).value should be (1)
      MaxDiscarded(2).value should be (2)
      MaxDiscarded(5678).value should be (5678)
  }

  test("maxDiscardedFactor value is passed value, if valid") {
    MaxDiscardedFactor(1.0).value.value should be (1.0)
    MaxDiscardedFactor(1.5).value.value should be (1.5)
  }

  test("maxDiscardedFactor should not compile if value less than 0") {
    "MaxDiscardedFactor(-0.1)" shouldNot compile
    "MaxDiscardedFactor(-100.0)" shouldNot compile
  }

  test("minSize should not compile if less than 0") {
    "MinSize(-1)" shouldNot compile
    "MinSize(-2)" shouldNot compile
  }

  test("minSize value is passed value, if valid") {
      MinSize(0).value.value should be (0)
      MinSize(1).value.value should be (1)
      MinSize(2).value.value should be (2)
      MinSize(5678).value.value should be (5678)
  }

  test("maxSize throws IAE if less than 0") {
    intercept[IllegalArgumentException] {
      MaxSize(-1)
    }
    intercept[IllegalArgumentException] {
      MaxSize(-2)
    }
  }

  test("maxSize value is passed value, if valid") {
      MaxSize(0).value should be (0)
      MaxSize(1).value should be (1)
      MaxSize(2).value should be (2)
      MaxSize(5678).value should be (5678)
  }

  test("sizeRange should not compile if less than 0") {
    "SizeRange(-1)" shouldNot compile
    "SizeRange(-2)" shouldNot compile
  }

  test("sizeRange value is passed value, if valid") {
      SizeRange(0).value.value should be (0)
      SizeRange(1).value.value should be (1)
      SizeRange(2).value.value should be (2)
      SizeRange(5678).value.value should be (5678)
  }

  
  test("workers should not compile if less than 1") {
    "Workers(0)" shouldNot compile
    "Workers(-1)" shouldNot compile
    "Workers(-2)" shouldNot compile
  }

  test("workers value is passed value, if valid") {
      Workers(1).value.value should be (1)
      Workers(2).value.value should be (2)
      Workers(5678).value.value should be (5678)
  }
}
