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
package org.scalatest.matchers.dsl

import org.scalatest._
import Matchers._

class ResultOfBeWordForNoExceptionSpec extends FunSpec {
  
  describe("ResultOfBeWordForNoException ") {
    
    it("should have pretty toString when used") {
      val result = noException should be
      result.toString should be ("ResultOfBeWordForNoException")
    }
    
  }
  
}
