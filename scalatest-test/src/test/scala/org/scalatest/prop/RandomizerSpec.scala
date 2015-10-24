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

    it("should offer a nextByteWithEdges method that initially produces Byte edge values") {
      val r0 = Randomizer(100)
      val (a1, r1) = r0.nextByteWithEdges
      val (a2, r2) = r1.nextByteWithEdges
      val (a3, r3) = r2.nextByteWithEdges
      val (a4, r4) = r3.nextByteWithEdges
      val (a5, _) = r4.nextByteWithEdges
      val edges = List(a1, a2, a3, a4, a5)
      edges should contain (0)
      edges should contain (1)
      edges should contain (-1)
      edges should contain (Byte.MaxValue)
      edges should contain (Byte.MinValue)
    }

    it("should offer a nextShortWithEdges method that initially produces Short edge values") {
      val r0 = Randomizer(100)
      val (a1, r1) = r0.nextShortWithEdges
      val (a2, r2) = r1.nextShortWithEdges
      val (a3, r3) = r2.nextShortWithEdges
      val (a4, r4) = r3.nextShortWithEdges
      val (a5, _) = r4.nextShortWithEdges
      val edges = List(a1, a2, a3, a4, a5)
      edges should contain (0)
      edges should contain (1)
      edges should contain (-1)
      edges should contain (Short.MaxValue)
      edges should contain (Short.MinValue)
    }

    it("should offer a nextCharWithEdges method that initially produces Char edge values") {
      val r0 = Randomizer(100)
      val (a1, r1) = r0.nextCharWithEdges
      val (a2, _) = r1.nextCharWithEdges
      val edges = List(a1, a2)
      edges should contain (Char.MaxValue)
      edges should contain (Char.MinValue)
    }

    it("should offer a nextIntWithEdges method that initially produces Int edge values") {
      val r0 = Randomizer(100)
      val (a1, r1) = r0.nextIntWithEdges
      val (a2, r2) = r1.nextIntWithEdges
      val (a3, r3) = r2.nextIntWithEdges
      val (a4, r4) = r3.nextIntWithEdges
      val (a5, _) = r4.nextIntWithEdges
      val edges = List(a1, a2, a3, a4, a5)
      edges should contain (0)
      edges should contain (1)
      edges should contain (-1)
      edges should contain (Int.MaxValue)
      edges should contain (Int.MinValue)
    }

    it("should offer a nextLongWithEdges method that initially produces Long edge values") {
      val r0 = Randomizer(100)
      val (a1, r1) = r0.nextLongWithEdges
      val (a2, r2) = r1.nextLongWithEdges
      val (a3, r3) = r2.nextLongWithEdges
      val (a4, r4) = r3.nextLongWithEdges
      val (a5, _) = r4.nextLongWithEdges
      val edges = List(a1, a2, a3, a4, a5)
      edges should contain (0)
      edges should contain (1)
      edges should contain (-1)
      edges should contain (Long.MaxValue)
      edges should contain (Long.MinValue)
    }

    it("should offer a nextDoubleWithEdges method that initially produces Double edge values") {
      val r0 = Randomizer(100)
      val (a1, r1) = r0.nextDoubleWithEdges
      a1 should be (0.0)
    }
    it("should offer a nextPosIntWithEdges method that initially produces PosInt edge values") {
      import org.scalactic.anyvals.PosInt
      val r0 = Randomizer(100)
      val (a1, r1) = r0.nextPosIntWithEdges
      val (a2, _) = r1.nextPosIntWithEdges
      val edges = List(a1, a2)
      edges should contain (PosInt(1))
      edges should contain (PosInt.MaxValue)
    }
    it("should offer a nextPosZIntWithEdges method that initially produces PosZInt edge values") {
      import org.scalactic.anyvals.PosZInt
      val r0 = Randomizer(100)
      val (a1, r1) = r0.nextPosZIntWithEdges
      val (a2, r2) = r1.nextPosZIntWithEdges
      val (a3, _) = r2.nextPosZIntWithEdges
      val edges = List(a1, a2, a3)
      edges should contain (PosZInt(0))
      edges should contain (PosZInt(1))
      edges should contain (PosZInt.MaxValue)
    }
    it("should offer a nextPosLongWithEdges method that initially produces PosLong edge values") {
      import org.scalactic.anyvals.PosLong
      val r0 = Randomizer(100)
      val (a1, r1) = r0.nextPosLongWithEdges
      val (a2, _) = r1.nextPosLongWithEdges
      val edges = List(a1, a2)
      edges should contain (PosLong(1L))
      edges should contain (PosLong.MaxValue)
    }
    it("should offer a nextPosZLongWithEdges method that initially produces PosZLong edge values") {
      import org.scalactic.anyvals.PosZLong
      val r0 = Randomizer(100)
      val (a1, r1) = r0.nextPosZLongWithEdges
      val (a2, r2) = r1.nextPosZLongWithEdges
      val (a3, _) = r2.nextPosZLongWithEdges
      val edges = List(a1, a2, a3)
      edges should contain (PosZLong(0L))
      edges should contain (PosZLong(1L))
      edges should contain (PosZLong.MaxValue)
    }
    it("should offer a nextPosFloatWithEdges method that initially produces PosFloat edge values") {
      import org.scalactic.anyvals.PosFloat
      val r0 = Randomizer(100)
      val (a1, r1) = r0.nextPosFloatWithEdges
      val (a2, _) = r1.nextPosFloatWithEdges
      val edges = List(a1, a2)
      edges should contain (PosFloat(1.0f))
      edges should contain (PosFloat.MaxValue)
    }
    it("should offer a nextPosZFloatWithEdges method that initially produces PosZFloat edge values") {
      import org.scalactic.anyvals.PosZFloat
      val r0 = Randomizer(100)
      val (a1, r1) = r0.nextPosZFloatWithEdges
      val (a2, r2) = r1.nextPosZFloatWithEdges
      val (a3, _) = r2.nextPosZFloatWithEdges
      val edges = List(a1, a2, a3)
      edges should contain (PosZFloat(0.0f))
      edges should contain (PosZFloat(1.0f))
      edges should contain (PosZFloat.MaxValue)
    }
    it("should offer a nextPosDoubleWithEdges method that initially produces PosDouble edge values") {
      import org.scalactic.anyvals.PosDouble
      val r0 = Randomizer(100)
      val (a1, r1) = r0.nextPosDoubleWithEdges
      val (a2, _) = r1.nextPosDoubleWithEdges
      val edges = List(a1, a2)
      edges should contain (PosDouble(1.0))
      edges should contain (PosDouble.MaxValue)
    }
    it("should offer a nextPosZDoubleWithEdges method that initially produces PosZDouble edge values") {
      import org.scalactic.anyvals.PosZDouble
      val r0 = Randomizer(100)
      val (a1, r1) = r0.nextPosZDoubleWithEdges
      val (a2, r2) = r1.nextPosZDoubleWithEdges
      val (a3, _) = r2.nextPosZDoubleWithEdges
      val edges = List(a1, a2, a3)
      edges should contain (PosZDouble(0.0))
      edges should contain (PosZDouble(1.0))
      edges should contain (PosZDouble.MaxValue)
    }
    it("should offer a chooseInt method that initially produces Int values between from and to") {
      import GeneratorChecks._
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
      import GeneratorChecks._
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

      import GeneratorChecks._

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

      import GeneratorChecks._

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
  }
}

