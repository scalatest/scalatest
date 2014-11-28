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
package org.scalactic

import org.scalatest._

class GeneralContainElementSpec extends Spec with Matchers with CheckedEquality {

  object `the contain theSameElementsAs syntax` {
    def `should work on different types` {
      val jul: java.util.List[Int] = new java.util.ArrayList[Int]
      jul.add(3)
      jul.add(2)
      jul.add(1)
      List(1, 2, 3) should (contain theSameElementsAs jul)
    }
  }
}
