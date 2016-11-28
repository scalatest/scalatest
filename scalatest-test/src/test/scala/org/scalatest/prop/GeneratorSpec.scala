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
    it("should offer a map and flatMap method that composes the next methods") {
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
    it("should offer a map method that composes canonicals methods and offers a shrink that uses the canonicals methods") {

      import Generator._

      val (intCanonicalsIt, _) = intGenerator.canonicals(Randomizer.default)
      val expectedTupCanonicals = intCanonicalsIt.map(i => ('A', i)).toList

      val tupGen = for (i <- intGenerator) yield ('A', i)
      val (tupShrinkIt, _) = tupGen.shrink(('A', 100), Randomizer.default)
      val (tupCanonicalsIt, _) = tupGen.canonicals(Randomizer.default)
      val tupShrink = tupShrinkIt.toList
      val tupCanonicals = tupCanonicalsIt.toList

      tupShrink shouldBe expectedTupCanonicals
      tupCanonicals shouldBe expectedTupCanonicals
    }
    it("should offer a flatMap method that composes canonicals methods and offers a shrink that uses the canonicals methods") {

      import Generator._

      val (intCanonicalsIt, _) = intGenerator.canonicals(Randomizer.default)
      val intCanonicals = intCanonicalsIt.toList
      val (doubleCanonicalsIt, _) = doubleGenerator.canonicals(Randomizer.default)
      val doubleCanonicals = doubleCanonicalsIt.toList
      val expectedTupCanonicals: List[(Int, Double)] =
        for {
          i <- intCanonicals
          d <- doubleCanonicals
        } yield (i, d)

      val tupGen =
        for {
          i <- intGenerator
          d <- doubleGenerator
        }  yield (i, d)
      val (tupShrinkIt, _) = tupGen.shrink((100, 100.0), Randomizer.default)
      val (tupCanonicalsIt, _) = tupGen.canonicals(Randomizer.default)
      val tupShrink = tupShrinkIt.toList
      val tupCanonicals = tupCanonicalsIt.toList

      tupShrink shouldBe expectedTupCanonicals
      tupCanonicals shouldBe expectedTupCanonicals
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
        val expectedTestDiscarded = 11
        val minSuccessful = PosInt(10)
        implicit val generatorDrivenConfig = PropertyCheckConfiguration(maxDiscardedFactor = PosZDouble(1.1))
        info(s"What is this one: ${generatorDrivenConfig.maxDiscardedFactor}")
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
      val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
      val (a1: Byte, ae1: List[Byte], ar1: Randomizer) = gen.next(edges = initEdges, rnd = ier)
      val (a2, ae2, ar2) = gen.next(edges = ae1, rnd = ar1)
      val (a3, ae3, ar3) = gen.next(edges = ae2, rnd = ar2)
      val (a4, ae4, ar4) = gen.next(edges = ae3, rnd = ar3)
      val (a5, _, _) = gen.next(edges = ae4, rnd = ar4)
      val edges = List(a1, a2, a3, a4, a5)
      edges should contain (0)
      edges should contain (1)
      edges should contain (-1)
      edges should contain (Byte.MaxValue)
      edges should contain (Byte.MinValue)
    }
    it("should produce Byte canonical values") {
      import Generator._
      val gen = byteGenerator
      val (canonicals, _) = gen.canonicals(Randomizer.default)
      canonicals.toList shouldBe List(0, 1, -1, 2, -2, 3, -3).map(_.toByte)
    }
    it("should produce Short edge values first in random order") {
      import Generator._
      val gen = shortGenerator
      val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
      val (a1: Short, ae1: List[Short], ar1: Randomizer) = gen.next(edges = initEdges, rnd = ier)
      val (a2, ae2, ar2) = gen.next(edges = ae1, rnd = ar1)
      val (a3, ae3, ar3) = gen.next(edges = ae2, rnd = ar2)
      val (a4, ae4, ar4) = gen.next(edges = ae3, rnd = ar3)
      val (a5, _, _) = gen.next(edges = ae4, rnd = ar4)
      val edges = List(a1, a2, a3, a4, a5)
      edges should contain (0)
      edges should contain (1)
      edges should contain (-1)
      edges should contain (Short.MaxValue)
      edges should contain (Short.MinValue)
    }
    it("should produce Short canonical values") {
      import Generator._
      val gen = shortGenerator
      val (canonicals, _) = gen.canonicals(Randomizer.default)
      canonicals.toList shouldBe List(0, 1, -1, 2, -2, 3, -3).map(_.toShort)
    }
    it("should produce Char edge values first in random order") {
      import Generator._
      val gen = charGenerator
      val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
      val (a1: Char, ae1: List[Char], ar1: Randomizer) = gen.next(edges = initEdges, rnd = ier)
      val (a2, _, _) = gen.next(edges = ae1, rnd = ar1)
      val edges = List(a1, a2)
      edges should contain (Char.MinValue)
      edges should contain (Char.MaxValue)
    }
    it("should produce Char canonical values") {
      import Generator._
      val gen = charGenerator
      val (canonicalsIt, _) = gen.canonicals(Randomizer.default)
      val canonicals = canonicalsIt.toList
      canonicals(0) should (be >= 'a' and be <= 'z')
      canonicals(1) should (be >= 'A' and be <= 'Z')
      canonicals(2) should (be >= '0' and be <= '9')
    }
    it("should produce Int edge values first in random order") {
      import Generator._
      val gen = intGenerator
      val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
      val (a1: Int, ae1: List[Int], ar1: Randomizer) = gen.next(edges = initEdges, rnd = ier)
      val (a2, ae2, ar2) = gen.next(edges = ae1, rnd = ar1)
      val (a3, ae3, ar3) = gen.next(edges = ae2, rnd = ar2)
      val (a4, ae4, ar4) = gen.next(edges = ae3, rnd = ar3)
      val (a5, _, _) = gen.next(edges = ae4, rnd = ar4)
      val edges = List(a1, a2, a3, a4, a5)
      edges should contain (0)
      edges should contain (1)
      edges should contain (-1)
      edges should contain (Int.MaxValue)
      edges should contain (Int.MinValue)
    }
    it("should produce Int canonical values") {
      import Generator._
      val gen = intGenerator
      val (canonicals, _) = gen.canonicals(Randomizer.default)
      canonicals.toList shouldBe List(0, 1, -1, 2, -2, 3, -3)
    }
    it("should produce Long edge values first in random order") {
      import Generator._
      val gen = longGenerator
      val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
      val (a1: Long, ae1: List[Long], ar1: Randomizer) = gen.next(edges = initEdges, rnd = ier)
      val (a2, ae2, ar2) = gen.next(edges = ae1, rnd = ar1)
      val (a3, ae3, ar3) = gen.next(edges = ae2, rnd = ar2)
      val (a4, ae4, ar4) = gen.next(edges = ae3, rnd = ar3)
      val (a5, _, _) = gen.next(edges = ae4, rnd = ar4)
      val edges = List(a1, a2, a3, a4, a5)
      edges should contain (0)
      edges should contain (1)
      edges should contain (-1)
      edges should contain (Long.MaxValue)
      edges should contain (Long.MinValue)
    }
    it("should produce Long canonical values") {
      import Generator._
      val gen = longGenerator
      val (canonicals, _) = gen.canonicals(Randomizer.default)
      canonicals.toList shouldBe List(0L, 1L, -1L, 2L, -2L, 3L, -3L)
    }
    it("should produce the Float edge value first") {
      import Generator._
      val gen = floatGenerator
      val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
      val (a1, _, _) = gen.next(edges = initEdges, rnd = ier)
      a1 shouldEqual 0.0f
    }
    it("should produce Float canonical values") {
      import Generator._
      val gen = floatGenerator
      val (canonicals, _) = gen.canonicals(Randomizer.default)
      canonicals.toList shouldBe List(0.0f, 1.0f, -1.0f, 2.0f, -2.0f, 3.0f, -3.0f)
    }
    it("should produce the Double edge value first") {
      import Generator._
      val gen = doubleGenerator
      val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
      val (a1, _, _) = gen.next(edges = initEdges, rnd = ier)
      a1 shouldEqual 0.0
    }
    it("should produce Double canonical values") {
      import Generator._
      val gen = doubleGenerator
      val (canonicals, _) = gen.canonicals(Randomizer.default)
      canonicals.toList shouldBe List(0.0, 1.0, -1.0, 2.0, -2.0, 3.0, -3.0)
    }
    it("should produce PosInt edge values first in random order") {
      import Generator._
      val gen = posIntGenerator
      val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
      val (a1: PosInt, ae1: List[PosInt], ar1: Randomizer) = gen.next(edges = initEdges, rnd = ier)
      val (a2, _, _) = gen.next(edges = ae1, rnd = ar1)
      val edges = List(a1, a2)
      edges should contain (PosInt(1))
      edges should contain (PosInt.MaxValue)
    }
    it("should produce PosZInt edge values first in random order") {
      import Generator._
      val gen = posZIntGenerator
      val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
      val (a1: PosZInt, ae1: List[PosZInt], ar1: Randomizer) = gen.next(edges = initEdges, rnd = ier)
      val (a2, ae2, ar2) = gen.next(edges = ae1, rnd = ar1)
      val (a3, _, _) = gen.next(edges = ae2, rnd = ar2)
      val edges = List(a1, a2, a3)
      edges should contain (PosZInt(0))
      edges should contain (PosZInt(1))
      edges should contain (PosZInt.MaxValue)
    }
    it("should produce PosLong edge values first in random order") {
      import Generator._
      val gen = posLongGenerator
      val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
      val (a1: PosLong, ae1: List[PosLong], ar1: Randomizer) = gen.next(edges = initEdges, rnd = ier)
      val (a2, _, _) = gen.next(edges = ae1, rnd = ar1)
      val edges = List(a1, a2)
      edges should contain (PosLong(1L))
      edges should contain (PosLong.MaxValue)
    }
    it("should produce PosZLong edge values first in random order") {
      import Generator._
      val gen = posZLongGenerator
      val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
      val (a1: PosZLong, ae1: List[PosZLong], ar1: Randomizer) = gen.next(edges = initEdges, rnd = ier)
      val (a2, ae2, ar2) = gen.next(edges = ae1, rnd = ar1)
      val (a3, _, _) = gen.next(edges = ae2, rnd = ar2)
      val edges = List(a1, a2, a3)
      edges should contain (PosZLong(0L))
      edges should contain (PosZLong(1L))
      edges should contain (PosZLong.MaxValue)
    }
    it("should produce PosFloat edge values first in random order") {
      import Generator._
      val gen = posFloatGenerator
      val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
      val (a1: PosFloat, ae1: List[PosFloat], ar1: Randomizer) = gen.next(edges = initEdges, rnd = ier)
      val (a2, _, _) = gen.next(edges = ae1, rnd = ar1)
      val edges = List(a1, a2)
      edges should contain (PosFloat(1.0f))
      edges should contain (PosFloat.MaxValue)
    }
    it("should produce PosZFloat edge values first in random order") {
      import Generator._
      val gen = posZFloatGenerator
      val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
      val (a1: PosZFloat, ae1: List[PosZFloat], ar1: Randomizer) = gen.next(edges = initEdges, rnd = ier)
      val (a2, ae2, ar2) = gen.next(edges = ae1, rnd = ar1)
      val (a3, _, _) = gen.next(edges = ae2, rnd = ar2)
      val edges = List(a1, a2, a3)
      edges should contain (PosZFloat(0.0f))
      edges should contain (PosZFloat(1.0f))
      edges should contain (PosZFloat.MaxValue)
    }
    it("should produce PosDouble edge values first in random order") {
      import Generator._
      val gen = posDoubleGenerator
      val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
      val (a1: PosDouble, ae1: List[PosDouble], ar1: Randomizer) = gen.next(edges = initEdges, rnd = ier)
      val (a2, _, _) = gen.next(edges = ae1, rnd = ar1)
      val edges = List(a1, a2)
      edges should contain (PosDouble(1.0))
      edges should contain (PosDouble.MaxValue)
    }
    it("should produce PosZDouble edge values first in random order") {
      import Generator._
      val gen = posZDoubleGenerator
      val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
      val (a1: PosZDouble, ae1: List[PosZDouble], ar1: Randomizer) = gen.next(edges = initEdges, rnd = ier)
      val (a2, ae2, ar2) = gen.next(edges = ae1, rnd = ar1)
      val (a3, _, _) = gen.next(edges = ae2, rnd = ar2)
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
    it("should offer a chooseInt method the produces Ints between min and max") {

      import org.scalatest.prop.GeneratorDrivenPropertyChecks._

      val minMaxPairs: Generator[(Int, Int)] = 
        for {
          min <- Generator.chooseInt(Int.MinValue, Int.MaxValue - 1)
          max <- Generator.chooseInt(min, Int.MaxValue)
        } yield (min, max)

      forAll (minMaxPairs) { case (min, max) =>
        val minMaxGen: Generator[Int] = Generator.chooseInt(min, max) 
        val samples = minMaxGen.samples(10)
        import org.scalatest.Inspectors._
        forAll (samples) { i =>
          i should be >= min
          i should be <= max
        }
      }
    }
    it("should offer a chooseInt method that returns a generator whose initEdges method includes min and max") {

      import org.scalatest.prop.GeneratorDrivenPropertyChecks._

      val minMaxPairs: Generator[(Int, Int)] = 
        for {
          min <- Generator.chooseInt(Int.MinValue, Int.MaxValue - 1)
          max <- Generator.chooseInt(min, Int.MaxValue)
        } yield (min, max)

      forAll (minMaxPairs) { case (min, max) =>
        val minMaxGen: Generator[Int] = Generator.chooseInt(min, max) 
        val (edges, _) = minMaxGen.initEdges(100, Randomizer.default)
        edges should (have length 1 or have length 2)
        edges should contain (min)
        edges should contain (max)
      }
    }
    it("mapping and flatMapping a Generator should compose the edges") {
      // import prop._
      import Generator._

      val intGenerator1 = intGenerator
      val intGenerator2 = intGenerator

      val (initEdges1, ir1) = intGenerator1.initEdges(100, Randomizer.default)
      val (initEdges2, ir2) = intGenerator2.initEdges(100, ir1)

      initEdges1 should contain theSameElementsAs initEdges2

      def pairGen(): Generator[(Int, Int)] =
        for {
          i <- intGenerator1
          j <- intGenerator2
        } yield (i, j)

      val gen = pairGen()
      val (initEdges, ier) = gen.initEdges(100, ir2)

      initEdges.length should equal (initEdges1.length * initEdges2.length)

      val comboLists: List[List[Int]] = initEdges1.combinations(2).toList
      val comboPairs: List[(Int, Int)] = comboLists.map(xs => (xs(0), xs(1)))
      val plusReversedPairs: List[(Int, Int)] = comboPairs flatMap { case (x, y) => List((x, y), (y, x)) }
      val sameValuePairs: List[(Int, Int)] = initEdges1.map(i => (i, i))
      val expectedInitEdges: List[(Int, Int)] = plusReversedPairs ++ sameValuePairs

      initEdges should contain theSameElementsAs expectedInitEdges

      val (tup1, e1, r1) = gen.next(edges = initEdges, rnd = ier)
      val (tup2, e2, r2) = gen.next(edges = e1, rnd = r1)
      val (tup3, e3, r3) = gen.next(edges = e2, rnd = r2)
      val (tup4, e4, r4) = gen.next(edges = e3, rnd = r3)
      val (tup5, e5, r5) = gen.next(edges = e4, rnd = r4)
      val (tup6, e6, r6) = gen.next(edges = e5, rnd = r5)
      val (tup7, e7, r7) = gen.next(edges = e6, rnd = r6)
      val (tup8, e8, r8) = gen.next(edges = e7, rnd = r7)
      val (tup9, e9, r9) = gen.next(edges = e8, rnd = r8)
      val (tup10, e10, r10) = gen.next(edges = e9, rnd = r9)
      val (tup11, e11, r11) = gen.next(edges = e10, rnd = r10)
      val (tup12, e12, r12) = gen.next(edges = e11, rnd = r11)
      val (tup13, e13, r13) = gen.next(edges = e12, rnd = r12)
      val (tup14, e14, r14) = gen.next(edges = e13, rnd = r13)
      val (tup15, e15, r15) = gen.next(edges = e14, rnd = r14)
      val (tup16, e16, r16) = gen.next(edges = e15, rnd = r15)
      val (tup17, e17, r17) = gen.next(edges = e16, rnd = r16)
      val (tup18, e18, r18) = gen.next(edges = e17, rnd = r17)
      val (tup19, e19, r19) = gen.next(edges = e18, rnd = r18)
      val (tup20, e20, r20) = gen.next(edges = e19, rnd = r19)
      val (tup21, e21, r21) = gen.next(edges = e20, rnd = r20)
      val (tup22, e22, r22) = gen.next(edges = e21, rnd = r21)
      val (tup23, e23, r23) = gen.next(edges = e22, rnd = r22)
      val (tup24, e24, r24) = gen.next(edges = e23, rnd = r23)
      val (tup25, _, _) = gen.next(edges = e24, rnd = r24)
      val values = List(tup1, tup2, tup3, tup4, tup5, tup6, tup7, tup8, tup9, tup10,
          tup11, tup12, tup13, tup14, tup15, tup16, tup17, tup18, tup19, tup20,
          tup21, tup22, tup23, tup24, tup25)
      values should contain theSameElementsAs expectedInitEdges
    }
    it("should be able to use a ScalaCheck Arbitary and Shrink") {
      import org.scalacheck.{Arbitrary, Gen, Shrink}
      import org.scalacheck.rng.Seed
      val intShrink = implicitly[Shrink[Int]] 
      val intArbitrary = implicitly[Arbitrary[Int]]
      val intGen = intArbitrary.arbitrary
      val intGenerator = Generator.scalaCheckArbitaryGenerator(intArbitrary, intShrink)
      val (edges, er) = intGenerator.initEdges(100, Randomizer.default)
      edges should equal (Nil) // A ScalaCheck-backed generator would have no edges
      val scalaCheckShrinkList = intShrink.shrink(100)
      val (scalaTestShrinkIt, _) = intGenerator.shrink(100, Randomizer.default)
      val scalaTestShrinkList = scalaTestShrinkIt.toList
      scalaTestShrinkList shouldEqual scalaCheckShrinkList.reverse
    }
    it("should shrink Ints by repeatedly halving and negating") {
      import GeneratorDrivenPropertyChecks._
      forAll { (i: Int) =>
        val generator = implicitly[Generator[Int]]
        val (shrinkIt, _) = generator.shrink(i, Randomizer.default)
        val shrinks: List[Int] = shrinkIt.toList
        shrinks.distinct.length shouldEqual shrinks.length
        if (i == 0)
          shrinks shouldBe empty
        else {
          if (i > 1)
            shrinks.last should be > 0
          else if (i < -1)
            shrinks.last should be < 0
          import org.scalatest.Inspectors._
          val pairs: List[(Int, Int)] = shrinks.zip(shrinks.tail)
          forAll (pairs) { case (x, y) =>
            assert(x == 0 || x == -y || x.abs == y.abs / 2)
          }
        }
      }
    }
    it("should shrink Longs by repeatedly halving and negating") {
      import GeneratorDrivenPropertyChecks._
      forAll { (n: Long) =>
        val generator = implicitly[Generator[Long]]
        val (shrinkIt, _) = generator.shrink(n, Randomizer.default)
        val shrinks: List[Long] = shrinkIt.toList
        shrinks.distinct.length shouldEqual shrinks.length
        if (n == 0)
          shrinks shouldBe empty
        else {
          if (n > 1L)
            shrinks.last should be > 0L
          else if (n < -1L)
            shrinks.last should be < 0L
          import org.scalatest.Inspectors._
          val pairs: List[(Long, Long)] = shrinks.zip(shrinks.tail)
          forAll (pairs) { case (x, y) =>
            assert(x == 0 || x == -y || x.abs == y.abs / 2)
          }
/*
          all (pairs) should satisfy { case (x, y) =>
            y == 0 || y == -x || y.abs == x.abs / 2
          }
*/
        }
      }
    }
    it("should shrink Shorts by repeatedly halving and negating") {
      import GeneratorDrivenPropertyChecks._
      forAll { (n: Short) =>
        val generator = implicitly[Generator[Short]]
        val (shrinkIt, _) = generator.shrink(n, Randomizer.default)
        val shrinks: List[Short] = shrinkIt.toList
        shrinks.distinct.length shouldEqual shrinks.length
        if (n == 0)
          shrinks shouldBe empty
        else {
          if (n > 1.toShort)
            shrinks.last should be > 0.toShort
          else if (n < -1.toShort)
            shrinks.last should be < 0.toShort
          import org.scalatest.Inspectors._
          val pairs: List[(Short, Short)] = shrinks.zip(shrinks.tail)
          forAll (pairs) { case (x, y) =>
            assert(x == 0 || x == -y || x.abs == y.abs / 2)
          }
        }
      }
    }
    it("should shrink Bytes by repeatedly halving and negating") {
      import GeneratorDrivenPropertyChecks._
      forAll { (b: Byte) =>
        val generator = implicitly[Generator[Byte]]
        val (shrinkIt, _) = generator.shrink(b, Randomizer.default)
        val shrinks: List[Byte] = shrinkIt.toList
        shrinks.distinct.length shouldEqual shrinks.length
        if (b == 0)
          shrinks shouldBe empty
        else {
          if (b > 1.toByte)
            shrinks.last should be > 0.toByte
          else if (b < -1.toByte)
            shrinks.last should be < 0.toByte
          import org.scalatest.Inspectors._
          val pairs: List[(Byte, Byte)] = shrinks.zip(shrinks.tail)
          forAll (pairs) { case (x, y) =>
            assert(x == 0 || x == -y || x.abs == y.abs / 2)
          }
        }
      }
    }
    it("should shrink Chars by trying selected printable characters") {
      import GeneratorDrivenPropertyChecks._
      val expectedChars = "abcdefghikjlmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789".toList
      val generator = implicitly[Generator[Char]]
      forAll { (c: Char) =>
        val (shrinkIt, _) = generator.shrink(c, Randomizer.default)
        val shrinks: List[Char] = shrinkIt.toList
        shrinks.distinct.length shouldEqual shrinks.length
        if (c >= '0' && c <= '9' || c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z')
          shrinks shouldBe empty
        else
          shrinks shouldEqual expectedChars
      }
      import org.scalatest.Inspectors
      Inspectors.forAll (expectedChars) { (c: Char) => 
        val (shrinkIt, _) = generator.shrink(c, Randomizer.default)
        val shrinks: List[Char] = shrinkIt.toList
        shrinks shouldBe empty
      }
    }
    it("should shrink Doubles by dropping the fraction part then repeatedly 'square-rooting' and negating") {
      import GeneratorDrivenPropertyChecks._
// try with -173126.1489439121
      forAll { (d: Double) =>
        val generator = implicitly[Generator[Double]]
        val (shrinkIt, _) = generator.shrink(d, Randomizer.default)
        val shrinks: List[Double] = shrinkIt.toList
        shrinks.distinct.length shouldEqual shrinks.length
        if (d == 0.0) {
          shrinks shouldBe empty
        }
        else {
          import org.scalatest.Inspectors._
          if (d > 1.0)
            shrinks.last should be > 0.0
          else if (d < -1.0)
            shrinks.last should be < 0.0
          if (!d.isWhole) {
            shrinks.last shouldEqual (if (d > 0.0) d.floor else d.ceil)
          }
          val pairs: List[(Double, Double)] = shrinks.zip(shrinks.tail)
          forAll (pairs) { case (x, y) =>
            assert(x == 0.0 || x == -y || x.abs < y.abs)
          }
        }
      }
    }
    it("should shrink Floats by dropping the fraction part then repeatedly 'square-rooting' and negating") {
      import GeneratorDrivenPropertyChecks._
      forAll { (f: Float) =>
        val generator = implicitly[Generator[Float]]
        val (shrinkIt, _) = generator.shrink(f, Randomizer.default)
        val shrinks: List[Float] = shrinkIt.toList
        shrinks.distinct.length shouldEqual shrinks.length
        if (f == 0.0f) {
          shrinks shouldBe empty
        } else {
          if (f > 1.0f)
            shrinks.last should be > 0.0f
          else if (f < -1.0f)
            shrinks.last should be < 0.0f
          import org.scalatest.Inspectors._
          if (!f.isWhole) {
            shrinks.last shouldEqual (if (f > 0.0f) f.floor else f.ceil)
          }
          val pairs: List[(Float, Float)] = shrinks.zip(shrinks.tail)
          forAll (pairs) { case (x, y) =>
            assert(x == 0.0f || x == -y || x.abs < y.abs)
          }
        }
      }
    }
    it("should shrink Strings using strategery") {
/*
I got
info] Compiling 1 Scala source to /Users/bv/nobkp/delus/st-fly-to-nyc-1/scalatest-test/target/scala-2.11/test-classes...
[error] /Users/bv/nobkp/delus/st-fly-to-nyc-1/scalatest-test/src/test/scala/org/scalatest/prop/GeneratorSpec.scala:815: could not find implicit value for parameter resultChecker: org.scalatest.prop.PropertyTestResultHandler[Any]
[error]       forAll {} (s: String) =>
[error]              ^
[error] one error found

      import GeneratorDrivenPropertyChecks._
      forAll { (s: String) =>
        val generator = implicitly[Generator[String]]
        val (shrinkIt, _) = generator.shrink(s, Randomizer.default)
        val shrinks: List[String] = shrinkIt.toList
        if (s.isEmpty)
          shrinks shouldBe empty
        else {
          shrinks(0) shouldBe ""
          shrinks(1) should have length 1
          shrinks(1).head should (be >= 'a' and be <= 'z')
          shrinks(2) should have length 1
          shrinks(2).head should (be >= 'A' and be <= 'Z')
          shrinks(3) should have length 1
          shrinks(3).head should (be >= '0' and be <= '9')

          val theChars = shrinks.drop(4)
          val distincts: List[String] = s.distinct.toList.map(_.toString)
          theChars.take(distincts.length).toList shouldEqual distincts

          val theHalves = shrinks.drop(4 + distincts.length)
          if (theHalves.length > 1) {
            import org.scalatest.Inspectors
            val zipped = theHalves.zip(theHalves.tail) 
            Inspectors.forAll (zipped) { case (s, t) => 
              s.length should be < t.length
            }
          }
        }
      }
*/
      import GeneratorDrivenPropertyChecks._
      forAll { (s: String) =>
        val generator = implicitly[Generator[String]]
        val (shrinkIt, _) = generator.shrink(s, Randomizer.default)
        val shrinks: List[String] = shrinkIt.toList
        if (s.isEmpty)
          shrinks shouldBe empty
        else {
          shrinks(0) shouldBe ""
          shrinks(1) should have length 1
          shrinks(1).head should (be >= 'a' and be <= 'z')
          shrinks(2) should have length 1
          shrinks(2).head should (be >= 'A' and be <= 'Z')
          shrinks(3) should have length 1
          shrinks(3).head should (be >= '0' and be <= '9')

          val theChars = shrinks.drop(4)
          val distincts: List[String] = s.distinct.toList.map(_.toString)
          theChars.take(distincts.length).toList shouldEqual distincts

          val theHalves = shrinks.drop(4 + distincts.length)
          if (theHalves.length > 1) {
            import org.scalatest.Inspectors
            val zipped = theHalves.zip(theHalves.tail) 
            Inspectors.forAll (zipped) { case (s, t) => 
              s.length should be < t.length
            }
          } else succeed
        }
      }
    }
    it("should offer an implicit provider for constant function0's with a pretty toString") {
      val function0s = Generator.function0Generator[Int]
      import GeneratorDrivenPropertyChecks._
      forAll (function0s) { (f: () => Int) =>
        val constantResult = f()
        import org.scalactic.TimesOnInt._
        10 times { f() shouldEqual constantResult }
        f.toString shouldBe s"() => $constantResult"
      }
    }
    it("should offer an implicit provider for constant function0's that returns the edges of the result type") {
      val ints = Generator.intGenerator
      val function0s = Generator.function0Generator[Int]
      val (intEdgesIt, rnd1) = ints.initEdges(100, Randomizer.default)
      val (function0EdgesIt, _) = function0s.initEdges(100, rnd1)
      val intEdges = intEdgesIt.toList
      val function0Edges = function0EdgesIt.toList
      function0Edges.map(f => f()) should contain theSameElementsAs intEdges
    }
    it("should offer an implicit provider for constant function0's that returns the canonicals of the result type") {
      val ints = Generator.intGenerator
      val function0s = Generator.function0Generator[Int]
      val (intCanonicalsIt, rnd1) = ints.canonicals(Randomizer.default)
      val (function0CanonicalsIt, _) = function0s.canonicals(rnd1)
      val intCanonicals = intCanonicalsIt.toList
      val function0Canonicals = function0CanonicalsIt.toList
      function0Canonicals.map(f => f()) should contain theSameElementsAs intCanonicals
    }
    it("should offer an implicit provider for constant function0's that returns the shrinks of the result type") {
      val ints = Generator.intGenerator
      val function0s = Generator.function0Generator[Int]
      import GeneratorDrivenPropertyChecks._
      forAll (ints) { (i: Int) =>
        val (intShrinksIt, rnd1) = ints.shrink(i, Randomizer.default)
        val (function0ShrinksIt, _) = function0s.shrink(() => i, rnd1)
        val intShrinks = intShrinksIt.toList
        val function0Shrinks = function0ShrinksIt.toList
        function0Shrinks.map(f => f()) should contain theSameElementsAs intShrinks
      }
    }
    it("should not exhibit this bug in List shrinking") {
      val lstGen = implicitly[Generator[List[List[Int]]]]
      val xss = List(List(100, 200, 300, 400, 300))
      lstGen.shrink(xss, Randomizer.default)._1.toList should not contain xss
    }
    it("should shrink Lists using strategery") {
      import GeneratorDrivenPropertyChecks._
      val intGenerator = Generator.intGenerator
      val (intCanonicalsIt, _) = intGenerator.canonicals(Randomizer.default)
      val intCanonicals = intCanonicalsIt.toList
      forAll { (xs: List[Int]) =>
        val generator = implicitly[Generator[List[Int]]]
        val (shrinkIt, _) = generator.shrink(xs, Randomizer.default)
        val shrinks: List[List[Int]] = shrinkIt.toList
        if (xs.isEmpty)
          shrinks shouldBe empty
        else {

          // First one should be the empty list
          shrinks(0) shouldBe Nil

          // Then should come one-element Lists of the canonicals of the type
          val phase2 = shrinks.drop(1).take(intCanonicals.length)
          phase2 shouldEqual (intCanonicals.map(i => List(i)))

          // Phase 3 should be one-element lists of all distinct values in the value passed to shrink
          // If xs already is a one-element list, then we don't do this, because then xs would appear in the output.
          val xsDistincts = if (xs.length > 1) xs.distinct else Nil
          val phase3 = shrinks.drop(1 + intCanonicals.length).take(xsDistincts.length)
          phase3 shouldEqual (xsDistincts.map(i => List(i)))

          // Phase 4 should be n-element lists that are prefixes cut in half
          val theHalves = shrinks.drop(1 + intCanonicals.length + xsDistincts.length)
          theHalves should not contain xs // This was a bug I noticed
          if (theHalves.length > 1) {
            import org.scalatest.Inspectors
            val zipped = theHalves.zip(theHalves.tail) 
            Inspectors.forAll (zipped) { case (s, t) => 
              s.length should be < t.length
            }
          } else succeed
        }
      }
    }
    it("should offer a list generator whose canonical method uses the canonical method of the underlying T") {
      import GeneratorDrivenPropertyChecks._
      val intGenerator = Generator.intGenerator
      val (intCanonicalsIt, _) = intGenerator.canonicals(Randomizer.default)
      val intCanonicals = intCanonicalsIt.toList
      val listOfIntGenerator = Generator.listGenerator[Int]
      val (listOfIntCanonicalsIt, _) = listOfIntGenerator.canonicals(Randomizer.default)
      val listOfIntCanonicals = listOfIntCanonicalsIt.toList
      listOfIntCanonicals shouldEqual intCanonicals.map(i => List(i))
    }
  }
}

