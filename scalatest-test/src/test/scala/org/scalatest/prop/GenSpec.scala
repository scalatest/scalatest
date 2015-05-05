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

import org.scalatest.FunSpec
import org.scalatest.Matchers

class GenSpec extends FunSpec with Matchers {

  describe("A Gen") {
    it("should do produce the same values in the same order given the same seed") {
      import Gen._
      val ints1 = intGen(33)
      val ints2 = intGen(33)
      ints1.next() shouldEqual ints2.next()
      ints1.next() shouldEqual ints2.next()
      ints1.next() shouldEqual ints2.next()
      val doubles1 = doubleGen(99)
      val doubles2 = doubleGen(99)
      doubles1.next() shouldEqual doubles2.next()
      doubles1.next() shouldEqual doubles2.next()
      doubles1.next() shouldEqual doubles2.next()
    }

    it("should offer a map and flatMap method so I can use it in for expressions like a cowboy") {
      import Gen._
      def pairGen(seed: Int): Gen[(Int, Double)] =
        for {
          i <- intGen(seed) // Can see a problem already. Want to force the same seed.
          d <- doubleGen(seed)
        } yield (i, d)
      val pairs1 = pairGen(99)
      val pairs2 = pairGen(99)
      pairs1.next() shouldEqual pairs2.next()
      pairs1.next() shouldEqual pairs2.next()
      pairs1.next() shouldEqual pairs2.next()
    }
  }
}

