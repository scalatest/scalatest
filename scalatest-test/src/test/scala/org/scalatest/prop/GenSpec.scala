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
    it("should produce the same Int values in the same order given the same Rnd") {
      import Gen._
      val aGen0 = intGen
      val bGen0 = intGen
      val (a1, ar1, aGen1) = aGen0.next(rnd = Rnd(100))
      val (a2, ar2, aGen2) = aGen1.next(rnd = ar1)
      val (a3, ar3, aGen3) = aGen2.next(rnd = ar2)
      val (a4, ar4, aGen4) = aGen3.next(rnd = ar3)
      val (a5, ar5, aGen5) = aGen4.next(rnd = ar4)
      val (a6, ar6, aGen6) = aGen5.next(rnd = ar5)
      val (a7, _, _) = aGen6.next(rnd = ar6)
      val (b1, br1, bGen1) = bGen0.next(rnd = Rnd(100))
      val (b2, br2, bGen2) = bGen1.next(rnd = br1)
      val (b3, br3, bGen3) = bGen2.next(rnd = br2)
      val (b4, br4, bGen4) = bGen3.next(rnd = br3)
      val (b5, br5, bGen5) = bGen4.next(rnd = br4)
      val (b6, br6, bGen6) = bGen5.next(rnd = br5)
      val (b7, _, _) = bGen6.next(rnd = br6)
      List(a1, a2, a3, a4, a5) should contain theSameElementsAs List(b1, b2, b3, b4, b5)
      a6 shouldEqual b6
      a7 shouldEqual b7
    }
    it("should produce the same Double values in the same order given the same Rnd") {
      import Gen._
      val aGen0 = doubleGen
      val bGen0 = doubleGen
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
      a1._1 should not equal a2._1
      a1._2 should not equal a2._2
      val (b1, br1, bGen1) = bGen0.next(rnd = Rnd(100))
      val (b2, br2, bGen2) = bGen1.next(rnd = br1)
      val (b3, _, _) = bGen2.next(rnd = br2)
      a1 shouldEqual b1
      a2 shouldEqual b2
      a3 shouldEqual b3
    }
    it("should mix up both i and d when used in a for expression") {
      import Gen._
      def pairGen(): Gen[(Int, Double)] =
        // doubleGen().flatMap(d => intGen().map(i => (i, d)))
        for {
          i <- intGen
          d <- doubleGen
        } yield (i, d)
      val aGen0 = pairGen()
      val bGen0 = pairGen()
      val (a1, ar1, aGen1) = aGen0.next(rnd = Rnd(100))
      val (a2, ar2, aGen2) = aGen1.next(rnd = ar1)
      val (a3, _, _) = aGen2.next(rnd = ar2)
      a1._1 should not equal a2._1
      a1._2 should not equal a2._2
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
    it("should produce edge values first in random order") {
      import Gen._
      val ag0 = intGen
      val (a1, ar1, ag1) = ag0.next(rnd = Rnd(100))
      val (a2, ar2, ag2) = ag1.next(rnd = ar1)
      val (a3, ar3, ag3) = ag2.next(rnd = ar2)
      val (a4, ar4, ag4) = ag3.next(rnd = ar3)
      val (a5, _, _) = ag4.next(rnd = ar4)
      val edges = List(a1, a2, a3, a4, a5)
      edges should contain (0)
      edges should contain (1)
      edges should contain (-1)
      edges should contain (Int.MaxValue)
      edges should contain (Int.MinValue)
    }
    it("should offer a chooseInt method") {
      import Gen._
      import org.scalactic.anyvals._
      def posIntGen: Gen[PosInt] =
        for (i <- Gen.chooseInt(1, Int.MaxValue)) yield PosInt.from(i).get

      val aGen0 = posIntGen
      val (a1, ar1, aGen1) = aGen0.next(rnd = Rnd(100))
      val (a2, ar2, aGen2) = aGen1.next(rnd = ar1)
      val (a3, ar3, aGen3) = aGen2.next(rnd = ar2)
      val (a4, ar4, aGen4) = aGen3.next(rnd = ar3)
      val (a5, ar5, aGen5) = aGen4.next(rnd = ar4)
      val (a6, ar6, aGen6) = aGen5.next(rnd = ar5)
      val (a7, _, _) = aGen6.next(rnd = ar6)

      val bGen0 = posIntGen
      val (b1, br1, bGen1) = bGen0.next(rnd = Rnd(100))
      val (b2, br2, bGen2) = bGen1.next(rnd = br1)
      val (b3, br3, bGen3) = bGen2.next(rnd = br2)
      val (b4, br4, bGen4) = bGen3.next(rnd = br3)
      val (b5, br5, bGen5) = bGen4.next(rnd = br4)
      val (b6, br6, bGen6) = bGen5.next(rnd = br5)
      val (b7, _, _) = bGen6.next(rnd = br6)
      a1 shouldEqual b1
      a2 shouldEqual b2
      a3 shouldEqual b3
      a4 shouldEqual b4
      a5 shouldEqual b5
      a6 shouldEqual b6
      a7 shouldEqual b7
    }
  }
}

