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

class ConfigurationSuite extends FunSuite with ShouldMatchers {

  import Configuration._

  test("minSuccussful throws IAE if less than 1") {
    intercept[IllegalArgumentException] {
      minSuccessful(0)
    }
    intercept[IllegalArgumentException] {
      minSuccessful(-1)
    }
    intercept[IllegalArgumentException] {
      minSuccessful(-2)
    }
  }

  test("minSuccussful value is passed value, if valid") {
      minSuccessful(1).value should be (1)
      minSuccessful(2).value should be (2)
      minSuccessful(5678).value should be (5678)
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

  test("minSize throws IAE if less than 0") {
    intercept[IllegalArgumentException] {
      minSize(-1)
    }
    intercept[IllegalArgumentException] {
      minSize(-2)
    }
  }

  test("minSize value is passed value, if valid") {
      minSize(0).value should be (0)
      minSize(1).value should be (1)
      minSize(2).value should be (2)
      minSize(5678).value should be (5678)
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

  test("workers throws IAE if less than 1") {
    intercept[IllegalArgumentException] {
      workers(0)
    }
    intercept[IllegalArgumentException] {
      workers(-1)
    }
    intercept[IllegalArgumentException] {
      workers(-2)
    }
  }

  test("workers value is passed value, if valid") {
      workers(1).value should be (1)
      workers(2).value should be (2)
      workers(5678).value should be (5678)
  }
}
