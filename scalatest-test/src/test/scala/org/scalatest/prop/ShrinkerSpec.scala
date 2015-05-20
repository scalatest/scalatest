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
import org.scalactic.anyvals._

class ShrinkerSpec extends FunSpec with Matchers {
  describe("A shrinker Stream") {
    it("should be offered by a the default Int Generator") {
      import Generator._
      val is = intGenerator.shrink(100)
      val xs = is.toList
      xs(0) shouldBe 0
      xs(1) shouldBe 1
      xs(2) shouldBe -1
    }
  }
}

