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
import org.scalatest.exceptions.TestFailedException

class GenSpec extends FunSpec with Matchers {

  describe("A Gen") {
    it("should do produce the same Int values in the same order given the same Rnd") {
      import Gen._
      val aGen0 = intGen
      val bGen0 = intGen
      val (a1, ar1, aGen1) = aGen0.next(rnd = Rnd(100))
      aGen1 should be theSameInstanceAs aGen0
      val (a2, ar2, aGen2) = aGen1.next(rnd = ar1)
      val (a3, _, _) = aGen2.next(rnd = ar2)
      val (b1, br1, bGen1) = bGen0.next(rnd = Rnd(100))
      val (b2, br2, bGen2) = bGen1.next(rnd = br1)
      val (b3, _, _) = bGen2.next(rnd = br2)
      a1 shouldEqual b1
      a2 shouldEqual b2
      a3 shouldEqual b3
    }
    it("should do produce the same Double values in the same order given the same Rnd") {
      import Gen._
      val aGen0 = doubleGen
      val bGen0 = doubleGen
      val (a1, ar1, aGen1) = aGen0.next(rnd = Rnd(100))
      aGen1 should be theSameInstanceAs aGen0
      val (a2, ar2, aGen2) = aGen1.next(rnd = ar1)
      val (a3, _, _) = aGen2.next(rnd = ar2)
      val (b1, br1, bGen1) = bGen0.next(rnd = Rnd(100))
      val (b2, br2, bGen2) = bGen1.next(rnd = br1)
      val (b3, _, _) = bGen2.next(rnd = br2)
      a1 shouldEqual b1
      a2 shouldEqual b2
      a3 shouldEqual b3
    }
    it("should offer a map and flatMap method so I can use it in for expressions like a cowboy") {
      import Gen._
      def pairGen(): Gen[(Int, Double)] =
        // doubleGen().flatMap(d => intGen().map(i => (i, d)))
        for {
          d <- doubleGen
          i <- intGen
        } yield (i, d)
      val aGen0 = pairGen()
      val bGen0 = pairGen()
      val (a1, ar1, aGen1) = aGen0.next(rnd = Rnd(100))
      val (a2, ar2, aGen2) = aGen1.next(rnd = ar1)
      val (a3, _, _) = aGen2.next(rnd = ar2)
      val (b1, br1, bGen1) = bGen0.next(rnd = Rnd(100))
      val (b2, br2, bGen2) = bGen1.next(rnd = br1)
      val (b3, _, _) = bGen2.next(rnd = br2)
      a1 shouldEqual b1
      a2 shouldEqual b2
      a3 shouldEqual b3
    }
    it("should be usable in a forAll") {
      import ForAll._
      forAll { (i: Int) => 
        i + i shouldEqual i * 2
      }
      a [TestFailedException] should be thrownBy {
        forAll { (i: Int) => 
          i + i shouldEqual i * 3
        }
      }
    }
    it("should be used at least minSuccessful times in a forAll") {
      import ForAll._
      var count = 0
      forAll { (i: Int) => 
        count += 1
        i + i shouldEqual i * 2
      }
      count shouldEqual generatorDrivenConfig.minSuccessful

      {
        implicit val generatorDrivenConfig = PropertyCheckConfig(minSuccessful = 10)
        count = 0
        forAll { (i: Int) => 
          count += 1
          i + i shouldEqual i * 2
        }
        count shouldEqual generatorDrivenConfig.minSuccessful
      }
    }
    it("should be used at least maxDiscarded times in a forAll") {
      import ForAll._
      var count = 0
      a [TestFailedException] should be thrownBy {
        forAll { (i: Int) => 
          count += 1
          whenever(false) {
            i + i shouldEqual i * 3
          }
        }
      }
      count shouldEqual generatorDrivenConfig.maxDiscarded

      {
        implicit val generatorDrivenConfig = PropertyCheckConfig(maxDiscarded = 10)
        count = 0
        a [TestFailedException] should be thrownBy {
          forAll { (i: Int) => 
            count += 1
            whenever(false) {
              i + i shouldEqual i * 3
            }
          }
        }
        count shouldEqual generatorDrivenConfig.maxDiscarded
      }
    }
/*
    it("should produce edge values first in random order") {
      import Gen._
      val aInts = intGen
      val (a1, ar1) = aInts.next(rnd = Rnd(100))
      val (a2, ar2) = aInts.next(rnd = ar1)
      val (a3, ar3) = aInts.next(rnd = ar2)
      val (a4, ar4) = aInts.next(rnd = ar3)
      val (a5, _) = aInts.next(rnd = ar4)
      val edges = List(a1, a2, a3, a4, a5)
      edges should contain (0)
      edges should contain (1)
      edges should contain (-1)
      edges should contain (Int.MaxValue)
      edges should contain (Int.MinValue)
    }
*/
  }
}

