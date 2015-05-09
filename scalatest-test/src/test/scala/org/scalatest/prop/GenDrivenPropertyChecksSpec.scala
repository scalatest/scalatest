/*
 * Copyright 2001-2015 Artima, Inc.
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

import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.scalatest.exceptions.TestFailedException

class GenDrivenPropertyChecksSpec extends FunSpec with Matchers {
  describe("GenDrivenPropertyChecks") {
    import GenDrivenPropertyChecks._
    it("should provide a forAll that takes one params") {
      forAll { (i: Int) => 
        i + i shouldEqual i * 2
      }
      a [TestFailedException] should be thrownBy {
        forAll { (i: Int) => 
          i + i shouldEqual i * 3
        }
      }
    }
    it("should provide a forAll that takes two params") {
      import GenDrivenPropertyChecks._
      forAll { (i: Int, j: Int) => 
        i + j shouldEqual j + i
      }
      a [TestFailedException] should be thrownBy {
        forAll { (i: Int, j: Int) => 
          i + j shouldEqual j * i
        }
      }
    }
    it("should provide a forAll that takes three params") {
      import GenDrivenPropertyChecks._
      forAll { (i: Int, j: Int, k: Int) => 
        i + j + k shouldEqual k + j + i
      }
      a [TestFailedException] should be thrownBy {
        forAll { (i: Int, j: Int, k: Int) => 
          i + j + k shouldEqual k * j * i
        }
      }
    }
  }
}

