/*
 * Copyright 2001-2011 Artima, Inc.
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

import matchers.ShouldMatchers

import Configuration._

class PropertyCheckConfigParamSuite extends FunSuite with ShouldMatchers {

/*
 require(minSuccessfulTests > 0)
 require(maxDiscardedTests >= 0)
 require(minSize >= 0)
 require(maxSize >= minSize)
 require(workers > 0)
*/

  test("minSuccussful throws IAE if less than 1") {
    intercept[IllegalArgumentException] {
      MinSuccessful(0)
    }
    intercept[IllegalArgumentException] {
      MinSuccessful(-1)
    }
    intercept[IllegalArgumentException] {
      MinSuccessful(-2)
    }
  }

  test("minSuccussful value is passed value, if valid") {
      MinSuccessful(1).value should be (1)
      MinSuccessful(2).value should be (2)
      MinSuccessful(5678).value should be (5678)
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

  test("minSize throws IAE if less than 0") {
    intercept[IllegalArgumentException] {
      MinSize(-1)
    }
    intercept[IllegalArgumentException] {
      MinSize(-2)
    }
  }

  test("minSize value is passed value, if valid") {
      MinSize(0).value should be (0)
      MinSize(1).value should be (1)
      MinSize(2).value should be (2)
      MinSize(5678).value should be (5678)
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

  test("workers throws IAE if less than 1") {
    intercept[IllegalArgumentException] {
      Workers(0)
    }
    intercept[IllegalArgumentException] {
      Workers(-1)
    }
    intercept[IllegalArgumentException] {
      Workers(-2)
    }
  }

  test("workers value is passed value, if valid") {
      Workers(1).value should be (1)
      Workers(2).value should be (2)
      Workers(5678).value should be (5678)
  }
}
