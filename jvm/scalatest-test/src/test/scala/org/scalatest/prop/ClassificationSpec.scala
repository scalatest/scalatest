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
package org.scalatest.prop

import org.scalatest._
import org.scalactic.anyvals._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ClassificationSpec extends AnyWordSpec with Matchers {
  "A Classification" should {
    "round to the nearest Int in its percentages method" in {
      val c =
        Classification(
          10000,
          Map(
            "one point eight" -> 180,
            "one point five" -> 150,
            "one point three" -> 130
          )
        )
      c.percentages shouldEqual Map(
        "one point eight" -> PosZInt(2),
        "one point five" -> PosZInt(2),
        "one point three" -> PosZInt(1)
      )
    }
    "handle percentages of 0" in {
      val c =
        Classification(
          10000,
          Map(
            "zero" -> 0
          )
        )
      c.percentages shouldEqual Map(
        "zero" -> PosZInt(0)
      )
    }
    "include the Int percentage in the toString" in {
      val c =
        Classification(
          100,
          Map(
            "ten" -> 10
          )
        )
      c.toString shouldBe "10% ten"
    }
  }
}
