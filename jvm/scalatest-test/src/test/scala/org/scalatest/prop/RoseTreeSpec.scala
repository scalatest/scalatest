/*
 * Copyright 2001-2015 Artima, Inc.
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

import org.scalactic.anyvals._
import org.scalatest.exceptions.TestFailedException
import scala.collection.immutable.SortedSet
import scala.collection.immutable.SortedMap
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class RoseTreeSpec extends AnyFunSpec with Matchers {
  describe("A RoseTree") {
    it("should offer a map and flatMap method") {

      def intShr: (Int, Randomizer) => (List[RoseTree[Int]], Randomizer) = { (n: Int, rnd: Randomizer) =>
        val roseTrees = if (n > 0) (0 to n - 1).toList.reverse.map(x => RoseTree(x, intShr)) else List.empty
        (roseTrees, rnd)
      }

      def dubShr: (Double, Randomizer) => (List[RoseTree[Double]], Randomizer) = { (n: Double, rnd: Randomizer) =>
        val roseTrees = if (n > 0) (0 to n.toInt - 1).toList.map(_.toDouble).reverse.map(x => RoseTree(x, dubShr)) else List.empty
        (roseTrees, rnd)
      }

      val rt = RoseTree(10, intShr)
      rt.value shouldBe 10
      rt.map(n => n.toString + "!").value shouldBe "10!"

      val rt2 = RoseTree(43.0, dubShr)
      rt.flatMap(n => rt2.map(d => (n.toString + "!", d - 1))).value shouldBe (("10!", 42.0))
    }
  }
}


