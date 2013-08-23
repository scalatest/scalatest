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
package org.scalatest.words

import org.scalatest._
import Matchers._

class ResultOfValueWordApplicationSpec extends Spec {
  
  object `ResultOfValueWordApplication ` {
    
    def `should have pretty toString when expectedKey is null` {
      val result = new ResultOfValueWordApplication(null)
      result.toString should be ("value null")
    }
    
    def `should have pretty toString when expectedKey is not null` {
      val result = new ResultOfValueWordApplication("Bob")
      result.toString should be ("value \"Bob\"")
    }
    
  }
  
}