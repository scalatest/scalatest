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
package org.scalactic

import java.text._
import org.scalatest._
import scala.collection.GenTraversable
import scala.collection.mutable.Buffer
import scala.collection.mutable.ListBuffer
import scala.collection.parallel.mutable.ParArray

class CollectionsSpec extends UnitSpec {
  "The Collections object" should "offer a default Collections[Any] that calls ==, except .deep on Arrays" in {
    import Collections.default._
    immutable.Set(1, 2, 3) intersect immutable.Set(2, 3, 4) shouldEqual immutable.Set(2, 3)
    immutable.FastSet(1, 2, 3) intersect immutable.FastSet(2, 3, 4) shouldEqual immutable.FastSet(2, 3)
    Set(1, 2, 3) intersect Set(2, 3, 4) shouldEqual Set(2, 3)
    FastSet(1, 2, 3) intersect FastSet(2, 3, 4) shouldEqual FastSet(2, 3)
  }
  it should "use structural Array equality from the default collections" in {
    import Collections.default.immutable._
    Set(Array(1, 2, 3), Array(1, 2, 3)) should have size 1
    Set(
      Array(
        Set(Array(1, 2, 3)),
        Set(Array(1, 2, 3), Array(1, 2, 3))
      )
    ) should equal (
      Set(
        Array(
          Set(Array(1, 2, 3), Array(1, 2, 3)),
          Set(Array(1, 2, 3))
        )
      )
    )
  }
  it should "offer an apply method that takes an implicit HashingEquality that by default treats arrays structurally" in {
    val intArrayColls = Collections[Array[Int]]
    import intArrayColls.immutable._
    Set(Array(1, 2, 3), Array(1, 2, 3)) should have size 1

    {
      val anyColls = Collections[Any]
      import anyColls.immutable._
      Set(
        Array(
          Set(Array(1, 2, 3)),
          Set(Array(1, 2, 3), Array(1, 2, 3))
        )
      ) should equal (
        Set(
          Array(
            Set(Array(1, 2, 3), Array(1, 2, 3)),
            Set(Array(1, 2, 3))
          )
        )
      )
    }
  }
}

