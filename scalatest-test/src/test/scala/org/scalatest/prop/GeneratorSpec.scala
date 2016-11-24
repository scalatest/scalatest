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
import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.scalatest.exceptions.TestFailedException

class GeneratorSpec extends FunSpec with Matchers {
  describe("A Generator") {
    it("should produce the same Byte values in the same order given the same Randomizer") {
      import Generator._
      val aGen = byteGenerator
      val bGen = byteGenerator
      val (a1, _, ar1) = aGen.next(rnd = Randomizer(100))
      val (a2, _, ar2) = aGen.next(rnd = ar1)
      val (a3, _, ar3) = aGen.next(rnd = ar2)
      val (a4, _, ar4) = aGen.next(rnd = ar3)
      val (a5, _, ar5) = aGen.next(rnd = ar4)
      val (a6, _, ar6) = aGen.next(rnd = ar5)
      val (a7, _, _) = aGen.next(rnd = ar6)
      val (b1, _, br1) = bGen.next(rnd = Randomizer(100))
      val (b2, _, br2) = bGen.next(rnd = br1)
      val (b3, _, br3) = bGen.next(rnd = br2)
      val (b4, _, br4) = bGen.next(rnd = br3)
      val (b5, _, br5) = bGen.next(rnd = br4)
      val (b6, _, br6) = bGen.next(rnd = br5)
      val (b7, _, _) = bGen.next(rnd = br6)
      List(a1, a2, a3, a4, a5) should contain theSameElementsAs List(b1, b2, b3, b4, b5)
      a6 shouldEqual b6
      a7 shouldEqual b7
    }

    it("should produce the same Short values in the same order given the same Randomizer") {
      import Generator._
      val aGen= shortGenerator
      val bGen = shortGenerator
      val (a1, _, ar1) = aGen.next(rnd = Randomizer(100))
      val (a2, _, ar2) = aGen.next(rnd = ar1)
      val (a3, _, ar3) = aGen.next(rnd = ar2)
      val (a4, _, ar4) = aGen.next(rnd = ar3)
      val (a5, _, ar5) = aGen.next(rnd = ar4)
      val (a6, _, ar6) = aGen.next(rnd = ar5)
      val (a7, _, _) = aGen.next(rnd = ar6)
      val (b1, _, br1) = bGen.next(rnd = Randomizer(100))
      val (b2, _, br2) = bGen.next(rnd = br1)
      val (b3, _, br3) = bGen.next(rnd = br2)
      val (b4, _, br4) = bGen.next(rnd = br3)
      val (b5, _, br5) = bGen.next(rnd = br4)
      val (b6, _, br6) = bGen.next(rnd = br5)
      val (b7, _, _) = bGen.next(rnd = br6)
      List(a1, a2, a3, a4, a5) should contain theSameElementsAs List(b1, b2, b3, b4, b5)
      a6 shouldEqual b6
      a7 shouldEqual b7
    }

    it("should produce the same Char values in the same order given the same Randomizer") {
      import Generator._
      val aGen= charGenerator
      val bGen = charGenerator
      val (a1, _, ar1) = aGen.next(rnd = Randomizer(100))
      val (a2, _, ar2) = aGen.next(rnd = ar1)
      val (a3, _, ar3) = aGen.next(rnd = ar2)
      val (a4, _, ar4) = aGen.next(rnd = ar3)
      val (a5, _, ar5) = aGen.next(rnd = ar4)
      val (a6, _, ar6) = aGen.next(rnd = ar5)
      val (a7, _, _) = aGen.next(rnd = ar6)
      val (b1, _, br1) = bGen.next(rnd = Randomizer(100))
      val (b2, _, br2) = bGen.next(rnd = br1)
      val (b3, _, br3) = bGen.next(rnd = br2)
      val (b4, _, br4) = bGen.next(rnd = br3)
      val (b5, _, br5) = bGen.next(rnd = br4)
      val (b6, _, br6) = bGen.next(rnd = br5)
      val (b7, _, _) = bGen.next(rnd = br6)
      List(a1, a2, a3, a4, a5) should contain theSameElementsAs List(b1, b2, b3, b4, b5)
      a6 shouldEqual b6
      a7 shouldEqual b7
    }

    it("should produce the same Int values in the same order given the same Randomizer") {
      import Generator._
      val aGen= intGenerator
      val bGen = intGenerator
      val (a1, _, ar1) = aGen.next(rnd = Randomizer(100))
      val (a2, _, ar2) = aGen.next(rnd = ar1)
      val (a3, _, ar3) = aGen.next(rnd = ar2)
      val (a4, _, ar4) = aGen.next(rnd = ar3)
      val (a5, _, ar5) = aGen.next(rnd = ar4)
      val (a6, _, ar6) = aGen.next(rnd = ar5)
      val (a7, _, _) = aGen.next(rnd = ar6)
      val (b1, _, br1) = bGen.next(rnd = Randomizer(100))
      val (b2, _, br2) = bGen.next(rnd = br1)
      val (b3, _, br3) = bGen.next(rnd = br2)
      val (b4, _, br4) = bGen.next(rnd = br3)
      val (b5, _, br5) = bGen.next(rnd = br4)
      val (b6, _, br6) = bGen.next(rnd = br5)
      val (b7, _, _) = bGen.next(rnd = br6)
      List(a1, a2, a3, a4, a5) should contain theSameElementsAs List(b1, b2, b3, b4, b5)
      a6 shouldEqual b6
      a7 shouldEqual b7
    }
    it("should produce the same Long values in the same order given the same Randomizer") {
      import Generator._
      val aGen= longGenerator
      val bGen = longGenerator
      val (a1, _, ar1) = aGen.next(rnd = Randomizer(100))
      val (a2, _, ar2) = aGen.next(rnd = ar1)
      val (a3, _, ar3) = aGen.next(rnd = ar2)
      val (a4, _, ar4) = aGen.next(rnd = ar3)
      val (a5, _, ar5) = aGen.next(rnd = ar4)
      val (a6, _, ar6) = aGen.next(rnd = ar5)
      val (a7, _, _) = aGen.next(rnd = ar6)
      val (b1, _, br1) = bGen.next(rnd = Randomizer(100))
      val (b2, _, br2) = bGen.next(rnd = br1)
      val (b3, _, br3) = bGen.next(rnd = br2)
      val (b4, _, br4) = bGen.next(rnd = br3)
      val (b5, _, br5) = bGen.next(rnd = br4)
      val (b6, _, br6) = bGen.next(rnd = br5)
      val (b7, _, _) = bGen.next(rnd = br6)
      List(a1, a2, a3, a4, a5) should contain theSameElementsAs List(b1, b2, b3, b4, b5)
      a6 shouldEqual b6
      a7 shouldEqual b7
    }
    it("should produce the same Float values in the same order given the same Randomizer") {
      import Generator._
      val aGen = floatGenerator
      val bGen = floatGenerator
      val (a1, _, ar1) = aGen.next(rnd = Randomizer(100))
      val (a2, _, ar2) = aGen.next(rnd = ar1)
      val (a3, _, _) = aGen.next(rnd = ar2)
      val (b1, _, br1) = bGen.next(rnd = Randomizer(100))
      val (b2, _, br2) = bGen.next(rnd = br1)
      val (b3, _, _) = bGen.next(rnd = br2)
      a1 shouldEqual b1
      a2 shouldEqual b2
      a3 shouldEqual b3
    }
    it("should produce the same Double values in the same order given the same Randomizer") {
      import Generator._
      val aGen = doubleGenerator
      val bGen = doubleGenerator
      val (a1, _, ar1) = aGen.next(rnd = Randomizer(100))
      val (a2, _, ar2) = aGen.next(rnd = ar1)
      val (a3, _, _) = aGen.next(rnd = ar2)
      val (b1, _, br1) = bGen.next(rnd = Randomizer(100))
      val (b2, _, br2) = bGen.next(rnd = br1)
      val (b3, _, _) = bGen.next(rnd = br2)
      a1 shouldEqual b1
      a2 shouldEqual b2
      a3 shouldEqual b3
    }
    it("should produce the same PosInt values in the same order given the same Randomizer") {
      import Generator._
      val aGen= posIntGenerator
      val bGen = posIntGenerator
      val (a1, _, ar1) = aGen.next(rnd = Randomizer(100))
      val (a2, _, ar2) = aGen.next(rnd = ar1)
      val (a3, _, ar3) = aGen.next(rnd = ar2)
      val (a4, _, ar4) = aGen.next(rnd = ar3)
      val (a5, _, ar5) = aGen.next(rnd = ar4)
      val (a6, _, ar6) = aGen.next(rnd = ar5)
      val (a7, _, _) = aGen.next(rnd = ar6)
      val (b1, _, br1) = bGen.next(rnd = Randomizer(100))
      val (b2, _, br2) = bGen.next(rnd = br1)
      val (b3, _, br3) = bGen.next(rnd = br2)
      val (b4, _, br4) = bGen.next(rnd = br3)
      val (b5, _, br5) = bGen.next(rnd = br4)
      val (b6, _, br6) = bGen.next(rnd = br5)
      val (b7, _, _) = bGen.next(rnd = br6)
      List(a1, a2, a3, a4, a5) should contain theSameElementsAs List(b1, b2, b3, b4, b5)
      a6 shouldEqual b6
      a7 shouldEqual b7
    }
    it("should offer a map and flatMap method so I can use it in for expressions like a cowboy") {
      import Generator._
      def pairGen(): Generator[(Int, Double)] =
        // doubleGen().flatMap(d => intGen().map(i => (i, d)))
        for {
          d <- doubleGenerator
          i <- intGenerator
        } yield (i, d)
      val aGen = pairGen()
      val bGen = pairGen()
      val (a1, _, ar1) = aGen.next(rnd = Randomizer(100))
      val (a2, _, ar2) = aGen.next(rnd = ar1)
      val (a3, _, _) = aGen.next(rnd = ar2)
      a1._1 should not equal a2._1
      a1._2 should not equal a2._2
      val (b1, _, br1) = bGen.next(rnd = Randomizer(100))
      val (b2, _, br2) = bGen.next(rnd = br1)
      val (b3, _, _) = bGen.next(rnd = br2)
      a1 shouldEqual b1
      a2 shouldEqual b2
      a3 shouldEqual b3
    }
    it("should mix up both i and d when used in a for expression") {
      import Generator._
      def pairGen(): Generator[(Int, Double)] =
        // doubleGen().flatMap(d => intGen().map(i => (i, d)))
        for {
          i <- intGenerator
          d <- doubleGenerator
        } yield (i, d)
      val aGen = pairGen()
      val bGen = pairGen()
      val (a1, _, ar1) = aGen.next(rnd = Randomizer(100))
      val (a2, _, ar2) = aGen.next(rnd = ar1)
      val (a3, _, _) = aGen.next(rnd = ar2)
      a1._1 should not equal a2._1
      a1._2 should not equal a2._2
      val (b1, _, br1) = bGen.next(rnd = Randomizer(100))
      val (b2, _, br2) = bGen.next(rnd = br1)
      val (b3, _, _) = bGen.next(rnd = br2)
      a1 shouldEqual b1
      a2 shouldEqual b2
      a3 shouldEqual b3
    }
    it("should be usable in a forAll") {
      import GeneratorDrivenPropertyChecks._
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
      import GeneratorDrivenPropertyChecks._
      var count = 0
      forAll { (i: Int) => 
        count += 1
        i + i shouldEqual i * 2
      }
      count shouldEqual generatorDrivenConfig.minSuccessful.value

      {
        implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 10)
        count = 0
        forAll { (i: Int) => 
          count += 1
          i + i shouldEqual i * 2
        }
        count shouldEqual generatorDrivenConfig.minSuccessful.value
      }
    }
    it("should be used at least maxDiscarded times in a forAll") {
      import GeneratorDrivenPropertyChecks._
      var count = 0
      a [TestFailedException] should be thrownBy {
        forAll { (i: Int) => 
          count += 1
          whenever(false) {
            i + i shouldEqual i * 3
          }
        }
      }
      val maxDiscarded = PropertyCheckConfiguration.calculateMaxDiscarded(generatorDrivenConfig.maxDiscardedFactor, generatorDrivenConfig.minSuccessful)
      count shouldEqual maxDiscarded

      {
        val expectedTestDiscarded = 10
        val minSuccessful = PosInt(10)
        implicit val generatorDrivenConfig = PropertyCheckConfiguration(maxDiscardedFactor = PosZDouble.from(PropertyCheckConfiguration.calculateMaxDiscardedFactor(minSuccessful, expectedTestDiscarded)).get)
        count = 0
        a [TestFailedException] should be thrownBy {
          forAll { (i: Int) => 
            count += 1
            whenever(false) {
              i + i shouldEqual i * 3
            }
          }
        }
        count shouldEqual expectedTestDiscarded
      }
    }
    it("should produce Byte edge values first in random order") {
      import Generator._
      val gen = byteGenerator
      val (a1: Byte, _: List[Byte], ar1: Randomizer) = gen.next(rnd = Randomizer(100))
      val (a2, _, ar2) = gen.next(rnd = ar1)
      val (a3, _, ar3) = gen.next(rnd = ar2)
      val (a4, _, ar4) = gen.next(rnd = ar3)
      val (a5, _, _) = gen.next(rnd = ar4)
      val edges = List(a1, a2, a3, a4, a5)
      edges should contain (0)
      edges should contain (1)
      edges should contain (-1)
      edges should contain (Byte.MaxValue)
      edges should contain (Byte.MinValue)
    }
    it("should produce Short edge values first in random order") {
      import Generator._
      val gen = shortGenerator
      val (a1: Short, _: List[Short], ar1: Randomizer) = gen.next(rnd = Randomizer(100))
      val (a2, _, ar2) = gen.next(rnd = ar1)
      val (a3, _, ar3) = gen.next(rnd = ar2)
      val (a4, _, ar4) = gen.next(rnd = ar3)
      val (a5, _, _) = gen.next(rnd = ar4)
      val edges = List(a1, a2, a3, a4, a5)
      edges should contain (0)
      edges should contain (1)
      edges should contain (-1)
      edges should contain (Short.MaxValue)
      edges should contain (Short.MinValue)
    }
    it("should produce Char edge values first in random order") {
      import Generator._
      val gen = charGenerator
      val (a1: Char, _, ar1: Randomizer) = gen.next(rnd = Randomizer(100))
      val (a2, _, _) = gen.next(rnd = ar1)
      val edges = List(a1, a2)
      edges should contain (Char.MinValue)
      edges should contain (Char.MaxValue)
    }
    it("should produce Int edge values first in random order") {
      import Generator._
      val gen = intGenerator
      val (a1: Int, _, ar1: Randomizer) = gen.next(rnd = Randomizer(100))
      val (a2, _, ar2) = gen.next(rnd = ar1)
      val (a3, _, ar3) = gen.next(rnd = ar2)
      val (a4, _, ar4) = gen.next(rnd = ar3)
      val (a5, _, _) = gen.next(rnd = ar4)
      val edges = List(a1, a2, a3, a4, a5)
      edges should contain (0)
      edges should contain (1)
      edges should contain (-1)
      edges should contain (Int.MaxValue)
      edges should contain (Int.MinValue)
    }
    it("should produce Long edge values first in random order") {
      import Generator._
      val gen = longGenerator
      val (a1: Long, _, ar1: Randomizer) = gen.next(rnd = Randomizer(100))
      val (a2, _, ar2) = gen.next(rnd = ar1)
      val (a3, _, ar3) = gen.next(rnd = ar2)
      val (a4, _, ar4) = gen.next(rnd = ar3)
      val (a5, _, _) = gen.next(rnd = ar4)
      val edges = List(a1, a2, a3, a4, a5)
      edges should contain (0)
      edges should contain (1)
      edges should contain (-1)
      edges should contain (Long.MaxValue)
      edges should contain (Long.MinValue)
    }
    it("should produce the Float edge value first") {
      import Generator._
      val gen = floatGenerator
      val (a1, _, _) = gen.next(rnd = Randomizer(100))
      a1 shouldEqual 0.0f
    }
    it("should produce the Double edge value first") {
      import Generator._
      val gen = doubleGenerator
      val (a1, _, _) = gen.next(rnd = Randomizer(100))
      a1 shouldEqual 0.0
    }
    it("should produce PosInt edge values first in random order") {
      import Generator._
      val gen = posIntGenerator
      val (a1: PosInt, _, ar1: Randomizer) = gen.next(rnd = Randomizer(100))
      val (a2, _, _) = gen.next(rnd = ar1)
      val edges = List(a1, a2)
      edges should contain (PosInt(1))
      edges should contain (PosInt.MaxValue)
    }
    it("should produce PosZInt edge values first in random order") {
      import Generator._
      val gen = posZIntGenerator
      val (a1: PosZInt, _, ar1: Randomizer) = gen.next(rnd = Randomizer(100))
      val (a2, _, ar2) = gen.next(rnd = ar1)
      val (a3, _, _) = gen.next(rnd = ar2)
      val edges = List(a1, a2, a3)
      edges should contain (PosZInt(0))
      edges should contain (PosZInt(1))
      edges should contain (PosZInt.MaxValue)
    }
    it("should produce PosLong edge values first in random order") {
      import Generator._
      val gen = posLongGenerator
      val (a1: PosLong, _, ar1: Randomizer) = gen.next(rnd = Randomizer(100))
      val (a2, _, _) = gen.next(rnd = ar1)
      val edges = List(a1, a2)
      edges should contain (PosLong(1L))
      edges should contain (PosLong.MaxValue)
    }
    it("should produce PosZLong edge values first in random order") {
      import Generator._
      val gen = posZLongGenerator
      val (a1: PosZLong, _, ar1: Randomizer) = gen.next(rnd = Randomizer(100))
      val (a2, _, ar2) = gen.next(rnd = ar1)
      val (a3, _, _) = gen.next(rnd = ar2)
      val edges = List(a1, a2, a3)
      edges should contain (PosZLong(0L))
      edges should contain (PosZLong(1L))
      edges should contain (PosZLong.MaxValue)
    }
    it("should produce PosFloat edge values first in random order") {
      import Generator._
      val gen = posFloatGenerator
      val (a1: PosFloat, _, ar1: Randomizer) = gen.next(rnd = Randomizer(100))
      val (a2, _, _) = gen.next(rnd = ar1)
      val edges = List(a1, a2)
      edges should contain (PosFloat(1.0f))
      edges should contain (PosFloat.MaxValue)
    }
    it("should produce PosZFloat edge values first in random order") {
      import Generator._
      val gen = posZFloatGenerator
      val (a1: PosZFloat, _, ar1: Randomizer) = gen.next(rnd = Randomizer(100))
      val (a2, _, ar2) = gen.next(rnd = ar1)
      val (a3, _, _) = gen.next(rnd = ar2)
      val edges = List(a1, a2, a3)
      edges should contain (PosZFloat(0.0f))
      edges should contain (PosZFloat(1.0f))
      edges should contain (PosZFloat.MaxValue)
    }
    it("should produce PosDouble edge values first in random order") {
      import Generator._
      val gen = posDoubleGenerator
      val (a1: PosDouble, _, ar1: Randomizer) = gen.next(rnd = Randomizer(100))
      val (a2, _, _) = gen.next(rnd = ar1)
      val edges = List(a1, a2)
      edges should contain (PosDouble(1.0))
      edges should contain (PosDouble.MaxValue)
    }
    it("should produce PosZDouble edge values first in random order") {
      import Generator._
      val gen = posZDoubleGenerator
      val (a1: PosZDouble, _, ar1: Randomizer) = gen.next(rnd = Randomizer(100))
      val (a2, _, ar2) = gen.next(rnd = ar1)
      val (a3, _, _) = gen.next(rnd = ar2)
      val edges = List(a1, a2, a3)
      edges should contain (PosZDouble(0.0))
      edges should contain (PosZDouble(1.0))
      edges should contain (PosZDouble.MaxValue)
    }
    it("should offer a String generator that returns a string whose length equals the passed size") {

      import Generator._
      val gen = stringGenerator

      val (s1, _, r1) = gen.next(size = 0, rnd = Randomizer(100))
      s1.length shouldBe 0

      val (s2, _, r2) = gen.next(size = 3, rnd = r1)
      s2.length shouldBe 3

      val (s3, _, r3) = gen.next(size = 38, rnd = r2)
      s3.length shouldBe 38

      val (s4, _, r4) = gen.next(size = 88, rnd = r3)
      s4.length shouldBe 88

      val (s5, _, _) = gen.next(size = 100, rnd = r4)
      s5.length shouldBe 100

      an [IllegalArgumentException] should be thrownBy {
        gen.next(size = -1, rnd = Randomizer(100))
      }
    }
    it("should offer a List[T] generator that returns a List[T] whose length equals the passed size") {

      import Generator._
      val gen = listGenerator[Int]

      val (l1, _, r1) = gen.next(size = 0, rnd = Randomizer(100))
      l1.length shouldBe 0

      val (l2, _, r2) = gen.next(size = 3, rnd = r1)
      l2.length shouldBe 3

      val (l3, _, r3) = gen.next(size = 38, rnd = r2)
      l3.length shouldBe 38

      val (l4, _, r4) = gen.next(size = 88, rnd = r3)
      l4.length shouldBe 88

      val (l5, _, _) = gen.next(size = 100, rnd = r4)
      l5.length shouldBe 100

      an [IllegalArgumentException] should be thrownBy {
        gen.next(size = -1, rnd = Randomizer(100))
      }
    }
    it("should offer a chooseInt method") {
      import org.scalactic.anyvals._
import Generator._
      def posIntGen: Generator[PosInt] =
        for (i <- Generator.chooseInt(1, Int.MaxValue)) yield PosInt.from(i).get

      val aGen = posIntGenerator
      val (a1, _, ar1) = aGen.next(rnd = Randomizer(100))
      val (a2, _, ar2) = aGen.next(rnd = ar1)
      val (a3, _, ar3) = aGen.next(rnd = ar2)
      val (a4, _, ar4) = aGen.next(rnd = ar3)
      val (a5, _, ar5) = aGen.next(rnd = ar4)
      val (a6, _, ar6) = aGen.next(rnd = ar5)
      val (a7, _, _) = aGen.next(rnd = ar6)

      val bGen = posIntGenerator
      val (b1, _, br1) = bGen.next(rnd = Randomizer(100))
      val (b2, _, br2) = bGen.next(rnd = br1)
      val (b3, _, br3) = bGen.next(rnd = br2)
      val (b4, _, br4) = bGen.next(rnd = br3)
      val (b5, _, br5) = bGen.next(rnd = br4)
      val (b6, _, br6) = bGen.next(rnd = br5)
      val (b7, _, _) = bGen.next(rnd = br6)
      a1 shouldEqual b1
      a2 shouldEqual b2
      a3 shouldEqual b3
      a4 shouldEqual b4
      a5 shouldEqual b5
      a6 shouldEqual b6
      a7 shouldEqual b7
    }
    it("on second thought I really want mapping and flatMapping to use the edges") {
      // import prop._
      import Generator._
      def pairGen(): Generator[(Int, Int)] =
        // intGen.flatMap(i => intGen.map(j => (i, j)))
        for {
          i <- intGenerator
          j <- intGenerator
        } yield (i, j)
      val gen = pairGen()
      val ((i1, j1), _, r1) = gen.next(rnd = Randomizer(100))
      val ((i2, j2), _, r2) = gen.next(rnd = r1)
      val ((i3, j3), _, r3) = gen.next(rnd = r2)
      val ((i4, j4), _, r4) = gen.next(rnd = r3)
      val ((i5, j5), _, r5) = gen.next(rnd = r4)
      val ((i6, j6), _, r6) = gen.next(rnd = r5)
      val ((i7, j7), _, r7) = gen.next(rnd = r6)
      val ((i8, j8), _, r8) = gen.next(rnd = r7)
      val ((i9, j9), _, r9) = gen.next(rnd = r8)
      val ((i10, j10), _, r10) = gen.next(rnd = r9)
      val values = List(i1, j1, i2, j2, i3, j3)
      values should contain (0)
      values should contain (1)
      values should contain (-1)
      values should contain (Int.MaxValue)
      values should contain (Int.MinValue)
    }
    it("should be able to use ScalaCheck Arbitary as underlying generator without problem") {
      val dateGen = Generator.scalacheckArbitaryGenerator(org.scalacheck.Arbitrary.arbDate)
      dateGen.next()
    }
  }
}

