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
package org.scalatest.prop

import org.scalatest.exceptions.TestFailedException

import scala.concurrent.Future
import org.scalatest.funspec.AsyncFunSpec

class AsyncGeneratorDrivenPropertyChecksSpec extends AsyncFunSpec with GeneratorDrivenPropertyChecks {

  describe("GeneratorDrivenPropertyChecks") {

    it("should do nothing when non-blocking 1 argument block assertion passed") {
      forAll { (i: Int) =>
        Future { assert(i == i) }
      }
    }

    it("should fail with TestFailedException when non-blocking 1 argument block assertion failed") {
      recoverToSucceededIf[TestFailedException] {
        forAll { (i: Int) =>
          Future {
            assert(i != i)
          }
        }
      }
    }

    it("should do nothing when non-blocking 2 arguments block assertion passed") {
      forAll { (i1: Int, i2: Long) =>
        Future { assert(i1 == i1 && i2 == i2) }
      }
    }

    it("should fail with TestFailedException when non-blocking 2 arguments block assertion failed") {
      recoverToSucceededIf[TestFailedException] {
        forAll { (i1: Int, i2: Long) =>
          Future {
            assert(i1 == i1 && i2 != i2)
          }
        }
      }
    }

    it("should do nothing when non-blocking 3 arguments block assertion passed") {
      forAll { (i1: Int, i2: Long, i3: String) =>
        Future { assert(i1 == i1 && i2 == i2 && i3 == i3) }
      }
    }

    it("should fail with TestFailedException when non-blocking 3 arguments block assertion failed") {
      recoverToSucceededIf[TestFailedException] {
        forAll { (i1: Int, i2: Long, i3: String) =>
          Future {
            assert(i1 == i1 && i2 == i2 && i3 != i3)
          }
        }
      }
    }

    it("should do nothing when non-blocking 4 arguments block assertion passed") {
      forAll { (i1: Int, i2: Long, i3: String, i4: Short) =>
        Future { assert(i1 == i1 && i2 == i2 && i3 == i3 && i4 == i4) }
      }
    }

    it("should fail with TestFailedException when non-blocking 4 arguments block assertion failed") {
      recoverToSucceededIf[TestFailedException] {
        forAll { (i1: Int, i2: Long, i3: String, i4: Short) =>
          Future {
            assert(i1 == i1 && i2 == i2 && i3 == i3 && i4 != i4)
          }
        }
      }
    }

    it("should do nothing when non-blocking 5 arguments block assertion passed") {
      forAll { (i1: Int, i2: Long, i3: String, i4: Short, i5: Float) =>
        Future { assert(i1 == i1 && i2 == i2 && i3 == i3 && i4 == i4 && i5 == i5) }
      }
    }

    it("should fail with TestFailedException when non-blocking 5 arguments block assertion failed") {
      recoverToSucceededIf[TestFailedException] {
        forAll { (i1: Int, i2: Long, i3: String, i4: Short, i5: Float) =>
          Future {
            assert(i1 == i1 && i2 == i2 && i3 == i3 && i4 == i4 && i5 != i5)
          }
        }
      }
    }

    it("should do nothing when non-blocking 6 arguments block assertion passed") {
      forAll { (i1: Int, i2: Long, i3: String, i4: Short, i5: Float, i6: Double) =>
        Future { assert(i1 == i1 && i2 == i2 && i3 == i3 && i4 == i4 && i5 == i5 && i6 == i6) }
      }
    }

    it("should fail with TestFailedException when non-blocking 6 arguments block assertion failed") {
      recoverToSucceededIf[TestFailedException] {
        forAll { (i1: Int, i2: Long, i3: String, i4: Short, i5: Float, i6: Double) =>
          Future {
            assert(i1 == i1 && i2 == i2 && i3 == i3 && i4 == i4 && i5 == i5 && i6 != i6)
          }
        }
      }
    }

  }

}