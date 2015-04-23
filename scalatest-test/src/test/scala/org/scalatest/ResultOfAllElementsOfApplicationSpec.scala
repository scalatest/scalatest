/*
 * Copyright 2001-2014 Artima, Inc.
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

import org.scalactic.Prettifier
import org.scalatest._
import Matchers._

class ResultOfAllElementsOfApplicationSpec extends Spec {

  object `ResultOfAllElementsOfApplication ` {

    def `should have pretty toString when right is empty` {
      val result = new ResultOfAllElementsOfApplication(Vector.empty)
      result.toString should be ("allElementsOf (Vector())")
    }

    def `should have pretty toString when right contains 1 element` {
      val result = new ResultOfAllElementsOfApplication(Vector("Bob"))
      result.toString should be ("allElementsOf (Vector(\"Bob\"))")
    }

    def `should have pretty toString when right contains > 1 elements` {
      val result = new ResultOfAllElementsOfApplication(Vector("Bob", "Alice"))
      result.toString should be ("allElementsOf (Vector(\"Bob\", \"Alice\"))")
    }
  }

}