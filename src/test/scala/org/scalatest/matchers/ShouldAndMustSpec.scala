/*
 * Copyright 2001-2008 Artima, Inc.
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
/*
// commenting out because I don't automatically build MustMatchers yet, but it does work
package org.scalatest.matchers

import org.scalatest._

class ShouldAndMustSpec extends FunSpec with ShouldMatchers with MustMatchers {

  describe("people who want both should and must at the same time") {
    it("should feel satisfied when they try it and it works") {
      1 should equal (1)
      1 must equal (1)
    }
  }
}
*/

package org.scalatest.matchers  // prevents unnecessary recompilation
class ShouldAndMustSpec {}      // prevents unnecessary recompilation
