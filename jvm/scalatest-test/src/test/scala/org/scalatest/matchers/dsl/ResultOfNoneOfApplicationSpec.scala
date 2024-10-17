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
package org.scalatest.matchers.dsl

import org.scalatest._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class ResultOfNoneOfApplicationSpec extends AnyFunSpec {
  
  describe("ResultOfNoneOfApplication ") {
    
    it("should have pretty toString when right is empty") {
      val result = new ResultOfNoneOfApplication(Vector.empty)
      result.toString should be ("noneOf ()")
    }
    
    it("should have pretty toString when right contains 1 element") {
      val result = new ResultOfNoneOfApplication(Vector("Bob"))
      result.toString should be ("noneOf (\"Bob\")")
    }
    
    it("should have pretty toString when right contains > 1 elements") {
      val result = new ResultOfNoneOfApplication(Vector("Bob", "Alice"))
      result.toString should be ("noneOf (\"Bob\", \"Alice\")")
    }
  }
  
}
