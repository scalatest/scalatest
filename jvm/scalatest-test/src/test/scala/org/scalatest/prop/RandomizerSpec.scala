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
package org.scalatest.prop

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class RandomizerSpec extends AnyFunSpec with Matchers {

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
    it("should offer a nextBigInt method that produces a non-negative BigInt of the requested number of bits") {
      an [IllegalArgumentException] should be thrownBy Randomizer(100).nextBigInt(-1)

      val (ba, ra) = Randomizer(100).nextBigInt(0)
      ba shouldEqual BigInt(0)

      val (bb, rb) = ra.nextBigInt(1)
      bb should (be >= BigInt(0) and be < BigInt(2))

      val (bc, rc) = rb.nextBigInt(64)
      bc should (be >= BigInt(0) and be < (BigInt(1) << 64))

      val (bd, _) = rc.nextBigInt(200)
      bd should (be >= BigInt(0) and be < (BigInt(1) << 200))
    }

    it("should offer a chooseBigInt method that produces BigInt values between from and to") {
      // Exercise a variety of ranges, including negatives and very large values.
      val ranges: List[(BigInt, BigInt)] = List(
        (BigInt(-100), BigInt(-10)),
        (BigInt(-5), BigInt(5)),
        (BigInt(0), BigInt(10)),
        (BigInt(100), BigInt(1000)),
        (BigInt(-10), BigInt(10)),
        (BigInt(1) << 100, (BigInt(1) << 100) + 1000)
      )
      ranges.foreach { case (i, j) =>
        var rnd = Randomizer.default
        (0 to 100).foreach { _ =>
          val (k, nextRandomizer) = rnd.chooseBigInt(i, j)
          val min = i.min(j)
          val max = i.max(j)
          k should be <= max
          k should be >= min
          rnd = nextRandomizer
        }
      }
    }

    it("should offer a nextBigDecimal method that produces a non-negative BigDecimal of the requested number of bits") {
      an [IllegalArgumentException] should be thrownBy Randomizer(100).nextBigDecimal(-1)

      val (ba, ra) = Randomizer(100).nextBigDecimal(0)
      ba shouldEqual BigDecimal(0)

      val (bb, rb) = ra.nextBigDecimal(1)
      bb should (be >= BigDecimal(0) and be < BigDecimal(2))

      val upperBound64 = BigDecimal(BigInt(1) << 64)
      val (bc, rc) = rb.nextBigDecimal(64)
      bc should (be >= BigDecimal(0) and be < upperBound64)

      val (bd, _) = rc.nextBigDecimal(200)
      bd should be >= BigDecimal(0)
    }

    it("should offer a chooseBigDecimal method that produces BigDecimal values between from and to") {
      val ranges: List[(BigDecimal, BigDecimal)] = List(
        (BigDecimal(BigInt(-100)), BigDecimal(BigInt(-10))),
        (BigDecimal(BigInt(-5)), BigDecimal(BigInt(5))),
        (BigDecimal(BigInt(0)), BigDecimal(BigInt(10))),
        (BigDecimal(0.5), BigDecimal(9.9)),
        (BigDecimal(BigInt(-10)), BigDecimal(BigInt(10))),
        (BigDecimal(BigInt(1) << 100), BigDecimal(BigInt(1) << 100) + BigDecimal(16))
      )
      ranges.foreach { case (i, j) =>
        var rnd = Randomizer.default
        (0 to 100).foreach { _ =>
          val (k, nextRandomizer) = rnd.chooseBigDecimal(i, j)
          val min = i.min(j)
          val max = i.max(j)
          k should be <= max
          k should be >= min
          rnd = nextRandomizer
        }
      }
    }

    it("should offer a nextString method that produces a String of the requested 0 or greater size") {

      """Randomizer(100).nextString(-1)""" shouldNot compile

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

      """Randomizer(100).nextList[Int](-1)""" shouldNot compile

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
        shuffled should contain theSameElementsAs xs
      }
    }
  }
}

