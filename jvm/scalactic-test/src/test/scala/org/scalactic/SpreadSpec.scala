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
package org.scalactic

import org.scalatest._
import TripleEqualsSupport.Spread

class SpreadSpec extends funspec.AnyFunSpec {

  describe("A Spread") {
    it("should throw IllegalArgumentException if a negative tolerance is passed") {
      val caught =
        intercept[IllegalArgumentException] {
          new Spread(3, -1)
        }
      assert(caught.getMessage.endsWith("tolerance must be zero or greater, but was -1"))
    }
    it("should have pretty toString") {
      val spread = new Spread(8, 2)
      assert(spread.toString == "8 +- 2")
    }
  }
}
