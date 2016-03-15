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
package org.scalatest.matchers

import org.scalatest._
import Matchers._
import org.scalactic.Prettifier

class BeMatcherSpec extends FunSpec {
  
  describe("BeMatcher ") {
    
    describe("instance created by BeMatcher apply method") {
      
      val beMatcher = BeMatcher[List[Int]] { list =>
        MatchResult(true, "test", "test", Prettifier.default)
      }
      
      it("should have pretty toString") {
        beMatcher.toString should be ("BeMatcher[scala.collection.immutable.List](scala.collection.immutable.List => MatchResult)")
      }
      
    }
    
  }
  
}