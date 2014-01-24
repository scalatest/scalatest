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
package org.scalatest

import scala.collection.immutable.TreeSet
import org.scalatest.events._
import Matchers._


class FailureMessagesSuite extends Suite {

  def `test: prettify arrays should handle null array element values` {
    assertResult("Array(1, null, 3)") {
      FailureMessages.decorateToStringValue(Array(1, null, 3))
    }
    Array(1, null, 3) should be (Array(1, null, 3))
    assertResult("Array(1, Array(\"hi\", null), null, 3)") {
      FailureMessages.decorateToStringValue(Array(1, Array("hi", null), null, 3))
    }
  }
}
