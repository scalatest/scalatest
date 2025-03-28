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
import matchers.AMatcher
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class ResultOfAWordToAMatcherApplicationSpec extends AnyFunSpec with FileMocks {
  
  describe("ResultOfAWordToAMatcherApplication ") {
    
    it("should have pretty toString") {
      val result = new ResultOfAWordToAMatcherApplication(AMatcher[FileMock]("file") { _.file  })
      result.toString should be ("a (AMatcher[org.scalatest.FileMocks$FileMock](\"file\", org.scalatest.FileMocks$FileMock => Boolean))")
    }
  }
  
}
