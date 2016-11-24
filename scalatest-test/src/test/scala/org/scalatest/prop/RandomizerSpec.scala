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

class RandomizerSpec extends FunSpec with Matchers {

  describe("A Randomizer") {
    it("should offer a nextInt method that produces the same values as java.util.Random given the same seed") {
      val jr = new java.util.Random(100)
      val ja = jr.nextInt()
      val jb = jr.nextInt()
      val jc = jr.nextInt()

      val sr = Randomizer(100)
      val (ia, ra) = sr.nextInt
      val (ib, rb) = ra.nextInt
      val (ic, _) = rb.nextInt

      ja shouldEqual ia
      jb shouldEqual ib
      jc shouldEqual ic
    }

    it("should offer a nextLong method that produces the same values as java.util.Random given the same seed") {
      val jr = new java.util.Random(100)
      val ja = jr.nextLong()
      val jb = jr.nextLong()
      val jc = jr.nextLong()

      val sr = Randomizer(100)
      val (ia, ra) = sr.nextLong
      val (ib, rb) = ra.nextLong
      val (ic, _) = rb.nextLong

      ja shouldEqual ia
      jb shouldEqual ib
      jc shouldEqual ic
    }

    it("should offer a nextFloatBetween0And1 method that produces the same values as java.util.Random given the same seed") {
      val jr = new java.util.Random(100)
      val ja = jr.nextFloat()
      val jb = jr.nextFloat()
      val jc = jr.nextFloat()

      val sr = Randomizer(100)
      val (ia, ra) = sr.nextFloatBetween0And1
      val (ib, rb) = ra.nextFloatBetween0And1
      val (ic, _) = rb.nextFloatBetween0And1

      ja shouldEqual ia
      jb shouldEqual ib
      jc shouldEqual ic
    }

    it("should offer a nextDoubleBetween0And1 method that produces the same values as java.util.Random given the same seed") {
      val jr = new java.util.Random(100)
      val ja = jr.nextDouble()
      val jb = jr.nextDouble()
      val jc = jr.nextDouble()

      val sr = Randomizer(100)
      val (ia, ra) = sr.nextDoubleBetween0And1
      val (ib, rb) = ra.nextDoubleBetween0And1
      val (ic, _) = rb.nextDoubleBetween0And1

      ja shouldEqual ia
      jb shouldEqual ib
      jc shouldEqual ic
    }

    it("should offer a chooseInt method that initially produces Int values between from and to") {
      import GeneratorDrivenPropertyChecks._
      var rnd = Randomizer.default
      forAll { (i: Int, j: Int) =>
        val (k, nextRandomizer) = rnd.chooseInt(i, j)
        val min = i.min(j)
        val max = i.max(j)
        k should be <= max
        k should be >= min
        rnd = nextRandomizer
        succeed
      }
    }
    it("should offer a chooseLong method that initially produces Long values between from and to") {
      import GeneratorDrivenPropertyChecks._
      var rnd = Randomizer.default
      forAll { (i: Long, j: Long) =>
        val (k, nextRandomizer) = rnd.chooseLong(i, j)
        val min = i.min(j)
        val max = i.max(j)
        k should be <= max
        k should be >= min
        rnd = nextRandomizer
        succeed
      }
    }
    it("should offer a nextString method that produces a String of the requested 0 or greater size") {

      
      an [IllegalArgumentException] should be thrownBy { Randomizer(100).nextString(-1) }

      val (sa, ra) = Randomizer(100).nextString(0)
      sa should have length 0

      val (sb, rb) = ra.nextString(1)
      sb should have length 1

      val (sc, rc) = rb.nextString(10)
      sc should have length 10

      val (sd, _) = rc.nextString(100)
      sd should have length 100

      // Ensure not all chars are the same (because initially it did that, because
      // I was using calling nextChar on the initial Randomizer only)
      sd.distinct shouldNot have size 1
    }
    it("should offer a nextList[T] method that produces a List[T] of the requested 0 or greater size") {

      an [IllegalArgumentException] should be thrownBy { Randomizer(100).nextList[Int](-1) }

      val (la, ra) = Randomizer(100).nextList[Int](0)
      la should have length 0

      val (lb, rb) = ra.nextString(1)
      lb should have length 1

      val (lc, rc) = rb.nextString(10)
      lc should have length 10

      val (ld, _) = rc.nextString(100)
      ld should have length 100

      ld.distinct shouldNot have size 1
    }
    it("should offer a shuffle method in its companion object that shuffles a list.") {
      import GeneratorDrivenPropertyChecks._
      var nextRnd = Randomizer.default
      forAll { (xs: List[Int]) =>
        val (shuffled, nr) = Randomizer.shuffle(xs, nextRnd)
        nextRnd = nr
/* shuffled should not equal xs
[info] - should offer a shuffle method in its companion object that shuffles a list. *** FAILED *** (13 milliseconds)
[info]   GeneratorDrivenPropertyCheckFailedException was thrown during property evaluation.
[info]    (RandomizerSpec.scala:156)
[info]     Falsified after 0 successful property evaluations.
[info]     Location: (RandomizerSpec.scala:156)
[info]     Occurred when passed generated values (
[info]       arg0 = PropertyArgument(None,List())
[info]     )
This should just say arg0 = List()
*/
        shuffled should contain theSameElementsAs xs
      }
    }
  }
}

