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

@deprecated("delete when PropertyCheckConfig is removed")
class PropertyCheckConfigSuite extends FunSuite with Matchers {

  test("minSuccussful throws IAE if less than 1") {
    intercept[IllegalArgumentException] {
      PropertyCheckConfig(minSuccessful = 0)
    }
    intercept[IllegalArgumentException] {
      PropertyCheckConfig(minSuccessful = -1)
    }
    intercept[IllegalArgumentException] {
      PropertyCheckConfig(minSuccessful = -2)
    }
  }

  test("minSuccussful value is passed value, if valid") {
      PropertyCheckConfig(minSuccessful = 1).minSuccessful should be (1)
      PropertyCheckConfig(minSuccessful = 2).minSuccessful should be (2)
      PropertyCheckConfig(minSuccessful = 5678).minSuccessful should be (5678)
  }

  test("maxDiscarded throws IAE if less than 0") {
    intercept[IllegalArgumentException] {
      PropertyCheckConfig(maxDiscarded = -1)
    }
    intercept[IllegalArgumentException] {
      PropertyCheckConfig(maxDiscarded = -2)
    }
  }

  test("maxDiscarded value is passed value, if valid") {
      PropertyCheckConfig(maxDiscarded = 0).maxDiscarded should be (0)
      PropertyCheckConfig(maxDiscarded = 1).maxDiscarded should be (1)
      PropertyCheckConfig(maxDiscarded = 2).maxDiscarded should be (2)
      PropertyCheckConfig(maxDiscarded = 5678).maxDiscarded should be (5678)
  }

  test("minSize throws IAE if less than 0") {
    intercept[IllegalArgumentException] {
      PropertyCheckConfig(minSize = -1)
    }
    intercept[IllegalArgumentException] {
      PropertyCheckConfig(minSize = -2)
    }
  }

  test("minSize value is passed value, if valid") {
      PropertyCheckConfig(minSize = 0).minSize should be (0)
      PropertyCheckConfig(minSize = 1).minSize should be (1)
      PropertyCheckConfig(minSize = 2).minSize should be (2)
      PropertyCheckConfig(minSize = 99).minSize should be (99)
      PropertyCheckConfig(minSize = 100).minSize should be (100)
  }

  test("maxSize throws IAE if less than 0") {
    intercept[IllegalArgumentException] {
      PropertyCheckConfig(maxSize = -1)
    }
    intercept[IllegalArgumentException] {
      PropertyCheckConfig(maxSize = -2)
    }
  }

  test("maxSize value is passed value, if valid") {
      PropertyCheckConfig(maxSize = 0).maxSize should be (0)
      PropertyCheckConfig(maxSize = 1).maxSize should be (1)
      PropertyCheckConfig(maxSize = 2).maxSize should be (2)
      PropertyCheckConfig(maxSize = 5678).maxSize should be (5678)
  }

  test("workers throws IAE if less than 1") {
    intercept[IllegalArgumentException] {
      PropertyCheckConfig(workers = 0)
    }
    intercept[IllegalArgumentException] {
      PropertyCheckConfig(workers = -1)
    }
    intercept[IllegalArgumentException] {
      PropertyCheckConfig(workers = -2)
    }
  }

  test("workers value is passed value, if valid") {
      PropertyCheckConfig(workers = 1).workers should be (1)
      PropertyCheckConfig(workers = 2).workers should be (2)
      PropertyCheckConfig(workers = 5678).workers should be (5678)
  }

  test("Should throw IAE if minSize > maxSize") {
    intercept[IllegalArgumentException] {
      PropertyCheckConfig(minSize = 5, maxSize = 4)
    }
  }

  test("Should not throw IAE if minSize == maxSize") {
    PropertyCheckConfig(minSize = 5, maxSize = 5)
  }

  test("Should throw IAE if specified minSize > specified maxSize") {
    intercept[IllegalArgumentException] {
      PropertyCheckConfig(minSize = 5, maxSize = 4)
    }
  }

  test("Should throw IAE if specified minSize > default maxSize of 100") {
    intercept[IllegalArgumentException] {
      PropertyCheckConfig(minSize = 101)
    }
    intercept[IllegalArgumentException] {
      PropertyCheckConfig(minSize = 200)
    }
  }
}
