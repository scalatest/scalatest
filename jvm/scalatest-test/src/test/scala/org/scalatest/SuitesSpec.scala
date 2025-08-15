/*
 * Copyright 2001-2025 Artima, Inc.
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

import SharedHelpers._
import org.scalatest.exceptions.NotAllowedException
import org.scalactic.exceptions.NullArgumentException
import org.scalatest.featurespec.AnyFeatureSpec
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.wordspec.AnyWordSpec

class SuitesSpec extends AnyFunSpec {

  val a = new Suite {}
  val b = new AnyFunSuite {}
  val c = new AnyFunSpec {}
  val d = new AnyWordSpec {}
  val e = new AnyFeatureSpec {}

  describe("Suites") {
    it("should return the passed suites from nestedSuites") {
      val f = new Suites(a, b, c, d, e)
      assert(f.nestedSuites == List(a, b, c, d, e))
      val g = new Suites(Array(a, b, c, d, e): _*)
      assert(g.nestedSuites == List(a, b, c, d, e))
      intercept[NullArgumentException] {
        new Suites(a, b, null, d, e)
      }
      intercept[NullArgumentException] {
        val aNull: Array[Suite] = null
        new Suites(aNull: _*)
      }
    }
  }
}

