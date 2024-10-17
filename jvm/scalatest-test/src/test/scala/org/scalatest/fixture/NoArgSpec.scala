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
package org.scalatest.fixture

import org.scalatest._
import NoArgSpec.invokedCount
import org.scalatest
import org.scalatest.matchers.should.Matchers

class NoArgSpec extends scalatest.funspec.AnyFunSpec with Matchers {
  describe("A NoArg") {
    it("should use the init function as the implementation of its apply method") {
      invokedCount = 0
      class ActorSys extends NoArg {
        invokedCount += 1
      }
      val noArg = new ActorSys
      invokedCount should === (0)
      noArg()
      invokedCount should === (1)
      noArg()
      invokedCount should === (2)
      noArg()
      invokedCount should === (3)
    }
    it("should gracefully handle an empty constructor") {
      class EmptyOne extends NoArg
      val emptyOne = new EmptyOne
      noException should be thrownBy emptyOne()
    }
  }
}

object NoArgSpec {
  // Had to put this here because was getting compiler crashes when it was a local var closed over
  var invokedCount = 0
}
