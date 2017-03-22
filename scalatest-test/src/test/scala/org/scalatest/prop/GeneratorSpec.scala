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
      val (a1, _, ar1) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
      val (a2, _, ar2) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar1)
      val (a3, _, _) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar2)
      a1._1 should not equal a2._1
      a1._2 should not equal a2._2
      val (b1, _, br1) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
      val (b2, _, br2) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br1)
      val (b3, _, _) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br2)
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
    it("should offer a filter method so that pattern matching can be used in for expressions with Generator generators") {
      """for ((a, b) <- CommonGenerators.tuple2s[String, Int]) yield (b, a)""" should compile
      case class Person(name: String, age: Int)
      val persons = CommonGenerators.instancesOf(Person) { p => (p.name, p.age) }
      """for (Person(a, b) <- persons) yield (b, a)""" should compile
    }
    it("should offer a filter method that throws an exception if too many objects are filtered out") {
      val doNotDoThisAtHome = CommonGenerators.ints.filter(i => i == 0) // Only keep zero
      a [IllegalStateException] should be thrownBy {
        doNotDoThisAtHome.next(SizeParam(PosZInt(0), 100, 100), Nil, Randomizer.default())
      }
      val okToDoThisAtHome = CommonGenerators.ints.filter(i => i != 0) // Only keep non-zeros
      noException should be thrownBy {
        okToDoThisAtHome.next(SizeParam(PosZInt(0), 100, 100), Nil, Randomizer.default())
      }
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
      val (a1, _, ar1) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
      val (a2, _, ar2) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar1)
      val (a3, _, _) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar2)
      a1._1 should not equal a2._1
      a1._2 should not equal a2._2
      val (b1, _, br1) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
      val (b2, _, br2) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br1)
      val (b3, _, _) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br2)
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

      val (tup1, e1, r1) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
      val (tup2, e2, r2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = e1, rnd = r1)
      val (tup3, e3, r3) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = e2, rnd = r2)
      val (tup4, e4, r4) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = e3, rnd = r3)
      val (tup5, e5, r5) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = e4, rnd = r4)
      val (tup6, e6, r6) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = e5, rnd = r5)
      val (tup7, e7, r7) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = e6, rnd = r6)
      val (tup8, e8, r8) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = e7, rnd = r7)
      val (tup9, e9, r9) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = e8, rnd = r8)
      val (tup10, e10, r10) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = e9, rnd = r9)
      val (tup11, e11, r11) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = e10, rnd = r10)
      val (tup12, e12, r12) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = e11, rnd = r11)
      val (tup13, e13, r13) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = e12, rnd = r12)
      val (tup14, e14, r14) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = e13, rnd = r13)
      val (tup15, e15, r15) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = e14, rnd = r14)
      val (tup16, e16, r16) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = e15, rnd = r15)
      val (tup17, e17, r17) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = e16, rnd = r16)
      val (tup18, e18, r18) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = e17, rnd = r17)
      val (tup19, e19, r19) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = e18, rnd = r18)
      val (tup20, e20, r20) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = e19, rnd = r19)
      val (tup21, e21, r21) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = e20, rnd = r20)
      val (tup22, e22, r22) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = e21, rnd = r21)
      val (tup23, e23, r23) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = e22, rnd = r22)
      val (tup24, e24, r24) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = e23, rnd = r23)
      val (tup25, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = e24, rnd = r24)
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

    describe("for Bytes") {
      it("should produce the same Byte values in the same order given the same Randomizer") {
        import Generator._
        val aGen = byteGenerator
        val bGen = byteGenerator
        val (a1, _, ar1) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (a2, _, ar2) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar1)
        val (a3, _, ar3) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar2)
        val (a4, _, ar4) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar3)
        val (a5, _, ar5) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar4)
        val (a6, _, ar6) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar5)
        val (a7, _, _) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar6)
        val (b1, _, br1) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (b2, _, br2) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br1)
        val (b3, _, br3) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br2)
        val (b4, _, br4) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br3)
        val (b5, _, br5) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br4)
        val (b6, _, br6) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br5)
        val (b7, _, _) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br6)
        List(a1, a2, a3, a4, a5) should contain theSameElementsAs List(b1, b2, b3, b4, b5)
        a6 shouldEqual b6
        a7 shouldEqual b7
      }
      it("should produce Byte edge values first in random order") {
        import Generator._
        val gen = byteGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: Byte, ae1: List[Byte], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, ae3, ar3) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val (a4, ae4, ar4) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae3, rnd = ar3)
        val (a5, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae4, rnd = ar4)
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
    }
    describe("for Shorts") {
      it("should produce the same Short values in the same order given the same Randomizer") {
        import Generator._
        val aGen= shortGenerator
        val bGen = shortGenerator
        val (a1, _, ar1) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (a2, _, ar2) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar1)
        val (a3, _, ar3) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar2)
        val (a4, _, ar4) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar3)
        val (a5, _, ar5) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar4)
        val (a6, _, ar6) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar5)
        val (a7, _, _) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar6)
        val (b1, _, br1) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (b2, _, br2) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br1)
        val (b3, _, br3) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br2)
        val (b4, _, br4) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br3)
        val (b5, _, br5) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br4)
        val (b6, _, br6) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br5)
        val (b7, _, _) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br6)
        List(a1, a2, a3, a4, a5) should contain theSameElementsAs List(b1, b2, b3, b4, b5)
        a6 shouldEqual b6
        a7 shouldEqual b7
      }
      it("should produce Short edge values first in random order") {
        import Generator._
        val gen = shortGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: Short, ae1: List[Short], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, ae3, ar3) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val (a4, ae4, ar4) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae3, rnd = ar3)
        val (a5, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae4, rnd = ar4)
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
    }
    describe("for Ints") {
      it("should produce the same Int values in the same order given the same Randomizer") {
        import Generator._
        val aGen= intGenerator
        val bGen = intGenerator
        val (a1, _, ar1) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (a2, _, ar2) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar1)
        val (a3, _, ar3) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar2)
        val (a4, _, ar4) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar3)
        val (a5, _, ar5) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar4)
        val (a6, _, ar6) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar5)
        val (a7, _, _) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar6)
        val (b1, _, br1) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (b2, _, br2) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br1)
        val (b3, _, br3) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br2)
        val (b4, _, br4) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br3)
        val (b5, _, br5) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br4)
        val (b6, _, br6) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br5)
        val (b7, _, _) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br6)
        List(a1, a2, a3, a4, a5) should contain theSameElementsAs List(b1, b2, b3, b4, b5)
        a6 shouldEqual b6
        a7 shouldEqual b7
      }
      it("should produce Int edge values first in random order") {
        import Generator._
        val gen = intGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: Int, ae1: List[Int], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, ae3, ar3) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val (a4, ae4, ar4) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae3, rnd = ar3)
        val (a5, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae4, rnd = ar4)
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
    }
    describe("for Longs") {
      it("should produce the same Long values in the same order given the same Randomizer") {
        import Generator._
        val aGen= longGenerator
        val bGen = longGenerator
        val (a1, _, ar1) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (a2, _, ar2) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar1)
        val (a3, _, ar3) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar2)
        val (a4, _, ar4) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar3)
        val (a5, _, ar5) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar4)
        val (a6, _, ar6) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar5)
        val (a7, _, _) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar6)
        val (b1, _, br1) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (b2, _, br2) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br1)
        val (b3, _, br3) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br2)
        val (b4, _, br4) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br3)
        val (b5, _, br5) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br4)
        val (b6, _, br6) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br5)
        val (b7, _, _) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br6)
        List(a1, a2, a3, a4, a5) should contain theSameElementsAs List(b1, b2, b3, b4, b5)
        a6 shouldEqual b6
        a7 shouldEqual b7
      }
      it("should produce Long edge values first in random order") {
        import Generator._
        val gen = longGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: Long, ae1: List[Long], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, ae3, ar3) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val (a4, ae4, ar4) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae3, rnd = ar3)
        val (a5, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae4, rnd = ar4)
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
    }
    describe("for Chars") {
      it("should produce the same Char values in the same order given the same Randomizer") {
        import Generator._
        val aGen= charGenerator
        val bGen = charGenerator
        val (a1, _, ar1) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (a2, _, ar2) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar1)
        val (a3, _, ar3) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar2)
        val (a4, _, ar4) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar3)
        val (a5, _, ar5) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar4)
        val (a6, _, ar6) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar5)
        val (a7, _, _) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar6)
        val (b1, _, br1) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (b2, _, br2) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br1)
        val (b3, _, br3) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br2)
        val (b4, _, br4) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br3)
        val (b5, _, br5) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br4)
        val (b6, _, br6) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br5)
        val (b7, _, _) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br6)
        List(a1, a2, a3, a4, a5) should contain theSameElementsAs List(b1, b2, b3, b4, b5)
        a6 shouldEqual b6
        a7 shouldEqual b7
      }
      it("should produce Char edge values first in random order") {
        import Generator._
        val gen = charGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: Char, ae1: List[Char], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
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
    }
    describe("for Floats") {
      it("should produce the same Float values in the same order given the same Randomizer") {
        import Generator._
        val aGen = floatGenerator
        val bGen = floatGenerator
        val (a1, _, ar1) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (a2, _, ar2) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar1)
        val (a3, _, _) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar2)
        val (b1, _, br1) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (b2, _, br2) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br1)
        val (b3, _, _) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br2)
        a1 shouldEqual b1
        a2 shouldEqual b2
        a3 shouldEqual b3
      }
      it("should produce the Float edge value first") {
        import Generator._
        val gen = floatGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        a1 shouldEqual 0.0f
      }
      it("should produce Float canonical values") {
        import Generator._
        val gen = floatGenerator
        val (canonicals, _) = gen.canonicals(Randomizer.default)
        canonicals.toList shouldBe List(0.0f, 1.0f, -1.0f, 2.0f, -2.0f, 3.0f, -3.0f)
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
    }
    describe("for Doubles") {
      it("should produce the same Double values in the same order given the same Randomizer") {
        import Generator._
        val aGen = doubleGenerator
        val bGen = doubleGenerator
        val (a1, _, ar1) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (a2, _, ar2) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar1)
        val (a3, _, _) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar2)
        val (b1, _, br1) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (b2, _, br2) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br1)
        val (b3, _, _) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br2)
        a1 shouldEqual b1
        a2 shouldEqual b2
        a3 shouldEqual b3
      }
      it("should produce the Double edge value first") {
        import Generator._
        val gen = doubleGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        a1 shouldEqual 0.0
      }
      it("should produce Double canonical values") {
        import Generator._
        val gen = doubleGenerator
        val (canonicals, _) = gen.canonicals(Randomizer.default)
        canonicals.toList shouldBe List(0.0, 1.0, -1.0, 2.0, -2.0, 3.0, -3.0)
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
    }
    describe("for PosInts") {
      it("should produce the same PosInt values in the same order given the same Randomizer") {
        import Generator._
        val aGen= posIntGenerator
        val bGen = posIntGenerator
        val (a1, _, ar1) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (a2, _, ar2) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar1)
        val (a3, _, ar3) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar2)
        val (a4, _, ar4) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar3)
        val (a5, _, ar5) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar4)
        val (a6, _, ar6) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar5)
        val (a7, _, _) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar6)
        val (b1, _, br1) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (b2, _, br2) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br1)
        val (b3, _, br3) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br2)
        val (b4, _, br4) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br3)
        val (b5, _, br5) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br4)
        val (b6, _, br6) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br5)
        val (b7, _, _) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br6)
        List(a1, a2, a3, a4, a5) should contain theSameElementsAs List(b1, b2, b3, b4, b5)
        a6 shouldEqual b6
        a7 shouldEqual b7
      }
      it("should produce PosInt edge values first in random order") {
        import Generator._
        val gen = posIntGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: PosInt, ae1: List[PosInt], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val edges = List(a1, a2)
        edges should contain (PosInt(1))
        edges should contain (PosInt.MaxValue)
      }
    }
    describe("for PosZInts") {
      it("should produce the same PosZInt values in the same order given the same Randomizer") {
        import Generator._
        val aGen= posZIntGenerator
        val bGen = posZIntGenerator
        val (a1, _, ar1) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (a2, _, ar2) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar1)
        val (a3, _, ar3) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar2)
        val (a4, _, ar4) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar3)
        val (a5, _, ar5) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar4)
        val (a6, _, ar6) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar5)
        val (a7, _, _) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar6)
        val (b1, _, br1) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (b2, _, br2) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br1)
        val (b3, _, br3) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br2)
        val (b4, _, br4) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br3)
        val (b5, _, br5) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br4)
        val (b6, _, br6) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br5)
        val (b7, _, _) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br6)
        List(a1, a2, a3, a4, a5) should contain theSameElementsAs List(b1, b2, b3, b4, b5)
        a6 shouldEqual b6
        a7 shouldEqual b7
      }
      it("should produce PosZInt edge values first in random order") {
        import Generator._
        val gen = posZIntGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: PosZInt, ae1: List[PosZInt], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val edges = List(a1, a2, a3)
        edges should contain (PosZInt(0))
        edges should contain (PosZInt(1))
        edges should contain (PosZInt.MaxValue)
      }
    }
    describe("for PosLongs") {
      it("should produce the same PosLong values in the same order given the same Randomizer") {
        import Generator._
        val aGen= posLongGenerator
        val bGen = posLongGenerator
        val (a1, _, ar1) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (a2, _, ar2) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar1)
        val (a3, _, ar3) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar2)
        val (a4, _, ar4) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar3)
        val (a5, _, ar5) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar4)
        val (a6, _, ar6) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar5)
        val (a7, _, _) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar6)
        val (b1, _, br1) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (b2, _, br2) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br1)
        val (b3, _, br3) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br2)
        val (b4, _, br4) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br3)
        val (b5, _, br5) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br4)
        val (b6, _, br6) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br5)
        val (b7, _, _) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br6)
        List(a1, a2, a3, a4, a5) should contain theSameElementsAs List(b1, b2, b3, b4, b5)
        a6 shouldEqual b6
        a7 shouldEqual b7
      }
      it("should produce PosLong edge values first in random order") {
        import Generator._
        val gen = posLongGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: PosLong, ae1: List[PosLong], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val edges = List(a1, a2)
        edges should contain (PosLong(1L))
        edges should contain (PosLong.MaxValue)
      }
    }
    describe("for PosZLongs") {
      it("should produce the same PosZLong values in the same order given the same Randomizer") {
        import Generator._
        val aGen= posZLongGenerator
        val bGen = posZLongGenerator
        val (a1, _, ar1) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (a2, _, ar2) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar1)
        val (a3, _, ar3) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar2)
        val (a4, _, ar4) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar3)
        val (a5, _, ar5) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar4)
        val (a6, _, ar6) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar5)
        val (a7, _, _) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar6)
        val (b1, _, br1) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (b2, _, br2) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br1)
        val (b3, _, br3) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br2)
        val (b4, _, br4) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br3)
        val (b5, _, br5) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br4)
        val (b6, _, br6) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br5)
        val (b7, _, _) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br6)
        List(a1, a2, a3, a4, a5) should contain theSameElementsAs List(b1, b2, b3, b4, b5)
        a6 shouldEqual b6
        a7 shouldEqual b7
      }
      it("should produce PosZLong edge values first in random order") {
        import Generator._
        val gen = posZLongGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: PosZLong, ae1: List[PosZLong], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val edges = List(a1, a2, a3)
        edges should contain (PosZLong(0L))
        edges should contain (PosZLong(1L))
        edges should contain (PosZLong.MaxValue)
      }
    }
    describe("for PosFloat") {
      it("should produce the same PosFloat values in the same order given the same Randomizer") {
        import Generator._
        val aGen= posFloatGenerator
        val bGen = posFloatGenerator
        val (a1, _, ar1) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (a2, _, ar2) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar1)
        val (a3, _, ar3) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar2)
        val (a4, _, ar4) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar3)
        val (a5, _, ar5) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar4)
        val (a6, _, ar6) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar5)
        val (a7, _, _) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar6)
        val (b1, _, br1) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (b2, _, br2) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br1)
        val (b3, _, br3) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br2)
        val (b4, _, br4) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br3)
        val (b5, _, br5) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br4)
        val (b6, _, br6) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br5)
        val (b7, _, _) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br6)
        List(a1, a2, a3, a4, a5) should contain theSameElementsAs List(b1, b2, b3, b4, b5)
        a6 shouldEqual b6
        a7 shouldEqual b7
      }
      it("should produce PosFloat edge values first in random order") {
        import Generator._
        val gen = posFloatGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: PosFloat, ae1: List[PosFloat], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val edges = List(a1, a2)
        edges should contain (PosFloat(1.0f))
        edges should contain (PosFloat.MaxValue)
      }
    }
    describe("for PosFiniteFloat") {
      it("should produce the same PosFiniteFloat values in the same order given the same Randomizer") {
        import Generator._
        val aGen= posFiniteFloatGenerator
        val bGen = posFiniteFloatGenerator
        val (a1, _, ar1) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (a2, _, ar2) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar1)
        val (a3, _, ar3) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar2)
        val (a4, _, ar4) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar3)
        val (a5, _, ar5) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar4)
        val (a6, _, ar6) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar5)
        val (a7, _, _) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar6)
        val (b1, _, br1) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (b2, _, br2) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br1)
        val (b3, _, br3) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br2)
        val (b4, _, br4) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br3)
        val (b5, _, br5) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br4)
        val (b6, _, br6) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br5)
        val (b7, _, _) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br6)
        List(a1, a2, a3, a4, a5) should contain theSameElementsAs List(b1, b2, b3, b4, b5)
        a6 shouldEqual b6
        a7 shouldEqual b7
      }
      it("should produce PosFiniteFloat edge values first in random order") {
        import Generator._
        val gen = posFiniteFloatGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: PosFiniteFloat, ae1: List[PosFiniteFloat], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val edges = List(a1, a2)
        edges should contain (PosFiniteFloat(1.0f))
        edges should contain (PosFiniteFloat.MaxValue)
      }
    }
    describe("for PosZFloat") {
      it("should produce the same PosZFloat values in the same order given the same Randomizer") {
        import Generator._
        val aGen= posZFloatGenerator
        val bGen = posZFloatGenerator
        val (a1, _, ar1) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (a2, _, ar2) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar1)
        val (a3, _, ar3) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar2)
        val (a4, _, ar4) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar3)
        val (a5, _, ar5) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar4)
        val (a6, _, ar6) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar5)
        val (a7, _, _) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar6)
        val (b1, _, br1) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (b2, _, br2) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br1)
        val (b3, _, br3) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br2)
        val (b4, _, br4) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br3)
        val (b5, _, br5) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br4)
        val (b6, _, br6) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br5)
        val (b7, _, _) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br6)
        List(a1, a2, a3, a4, a5) should contain theSameElementsAs List(b1, b2, b3, b4, b5)
        a6 shouldEqual b6
        a7 shouldEqual b7
      }
      it("should produce PosZFloat edge values first in random order") {
        import Generator._
        val gen = posZFloatGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: PosZFloat, ae1: List[PosZFloat], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val edges = List(a1, a2, a3)
        edges should contain (PosZFloat(0.0f))
        edges should contain (PosZFloat(1.0f))
        edges should contain (PosZFloat.MaxValue)
      }
    }
    describe("for PosZFiniteFloat") {
      it("should produce the same PosZFiniteFloat values in the same order given the same Randomizer") {
        import Generator._
        val aGen= posZFiniteFloatGenerator
        val bGen = posZFiniteFloatGenerator
        val (a1, _, ar1) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (a2, _, ar2) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar1)
        val (a3, _, ar3) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar2)
        val (a4, _, ar4) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar3)
        val (a5, _, ar5) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar4)
        val (a6, _, ar6) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar5)
        val (a7, _, _) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar6)
        val (b1, _, br1) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (b2, _, br2) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br1)
        val (b3, _, br3) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br2)
        val (b4, _, br4) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br3)
        val (b5, _, br5) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br4)
        val (b6, _, br6) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br5)
        val (b7, _, _) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br6)
        List(a1, a2, a3, a4, a5) should contain theSameElementsAs List(b1, b2, b3, b4, b5)
        a6 shouldEqual b6
        a7 shouldEqual b7
      }
      it("should produce PosZFiniteFloat edge values first in random order") {
        import Generator._
        val gen = posZFiniteFloatGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: PosZFiniteFloat, ae1: List[PosZFiniteFloat], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val edges = List(a1, a2, a3)
        edges should contain (PosZFiniteFloat(0.0f))
        edges should contain (PosZFiniteFloat(1.0f))
        edges should contain (PosZFiniteFloat.MaxValue)
      }
    }
    describe("for PosDouble") {
      it("should produce the same PosDouble values in the same order given the same Randomizer") {
        import Generator._
        val aGen= posDoubleGenerator
        val bGen = posDoubleGenerator
        val (a1, _, ar1) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (a2, _, ar2) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar1)
        val (a3, _, ar3) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar2)
        val (a4, _, ar4) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar3)
        val (a5, _, ar5) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar4)
        val (a6, _, ar6) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar5)
        val (a7, _, _) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar6)
        val (b1, _, br1) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (b2, _, br2) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br1)
        val (b3, _, br3) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br2)
        val (b4, _, br4) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br3)
        val (b5, _, br5) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br4)
        val (b6, _, br6) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br5)
        val (b7, _, _) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br6)
        List(a1, a2, a3, a4, a5) should contain theSameElementsAs List(b1, b2, b3, b4, b5)
        a6 shouldEqual b6
        a7 shouldEqual b7
      }
      it("should produce PosDouble edge values first in random order") {
        import Generator._
        val gen = posDoubleGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: PosDouble, ae1: List[PosDouble], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val edges = List(a1, a2)
        edges should contain (PosDouble(1.0))
        edges should contain (PosDouble.MaxValue)
      }
    }
    describe("for PosFiniteDouble") {
      it("should produce the same PosFiniteDouble values in the same order given the same Randomizer") {
        import Generator._
        val aGen= posFiniteDoubleGenerator
        val bGen = posFiniteDoubleGenerator
        val (a1, _, ar1) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (a2, _, ar2) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar1)
        val (a3, _, ar3) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar2)
        val (a4, _, ar4) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar3)
        val (a5, _, ar5) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar4)
        val (a6, _, ar6) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar5)
        val (a7, _, _) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar6)
        val (b1, _, br1) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (b2, _, br2) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br1)
        val (b3, _, br3) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br2)
        val (b4, _, br4) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br3)
        val (b5, _, br5) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br4)
        val (b6, _, br6) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br5)
        val (b7, _, _) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br6)
        List(a1, a2, a3, a4, a5) should contain theSameElementsAs List(b1, b2, b3, b4, b5)
        a6 shouldEqual b6
        a7 shouldEqual b7
      }
      it("should produce PosFiniteDouble edge values first in random order") {
        import Generator._
        val gen = posFiniteDoubleGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: PosFiniteDouble, ae1: List[PosFiniteDouble], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val edges = List(a1, a2)
        edges should contain (PosFiniteDouble(1.0))
        edges should contain (PosFiniteDouble.MaxValue)
      }
    }
    describe("for PosZDouble") {
      it("should produce the same PosZDouble values in the same order given the same Randomizer") {
        import Generator._
        val aGen= posZDoubleGenerator
        val bGen = posZDoubleGenerator
        val (a1, _, ar1) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (a2, _, ar2) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar1)
        val (a3, _, ar3) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar2)
        val (a4, _, ar4) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar3)
        val (a5, _, ar5) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar4)
        val (a6, _, ar6) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar5)
        val (a7, _, _) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar6)
        val (b1, _, br1) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (b2, _, br2) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br1)
        val (b3, _, br3) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br2)
        val (b4, _, br4) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br3)
        val (b5, _, br5) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br4)
        val (b6, _, br6) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br5)
        val (b7, _, _) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br6)
        List(a1, a2, a3, a4, a5) should contain theSameElementsAs List(b1, b2, b3, b4, b5)
        a6 shouldEqual b6
        a7 shouldEqual b7
      }
      it("should produce PosZDouble edge values first in random order") {
        import Generator._
        val gen = posZDoubleGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: PosZDouble, ae1: List[PosZDouble], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val edges = List(a1, a2, a3)
        edges should contain (PosZDouble(0.0))
        edges should contain (PosZDouble(1.0))
        edges should contain (PosZDouble.MaxValue)
      }
    }
    describe("for PosZFiniteDouble") {
      it("should produce the same PosZFiniteDouble values in the same order given the same Randomizer") {
        import Generator._
        val aGen= posZFiniteDoubleGenerator
        val bGen = posZFiniteDoubleGenerator
        val (a1, _, ar1) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (a2, _, ar2) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar1)
        val (a3, _, ar3) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar2)
        val (a4, _, ar4) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar3)
        val (a5, _, ar5) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar4)
        val (a6, _, ar6) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar5)
        val (a7, _, _) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar6)
        val (b1, _, br1) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (b2, _, br2) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br1)
        val (b3, _, br3) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br2)
        val (b4, _, br4) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br3)
        val (b5, _, br5) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br4)
        val (b6, _, br6) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br5)
        val (b7, _, _) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br6)
        List(a1, a2, a3, a4, a5) should contain theSameElementsAs List(b1, b2, b3, b4, b5)
        a6 shouldEqual b6
        a7 shouldEqual b7
      }
      it("should produce PosZFiniteDouble edge values first in random order") {
        import Generator._
        val gen = posZFiniteDoubleGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: PosZFiniteDouble, ae1: List[PosZFiniteDouble], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val edges = List(a1, a2, a3)
        edges should contain (PosZFiniteDouble(0.0))
        edges should contain (PosZFiniteDouble(1.0))
        edges should contain (PosZFiniteDouble.MaxValue)
      }
    }
    describe("for NegInts") {
      it("should produce the same NegInt values in the same order given the same Randomizer") {
        import Generator._
        val aGen= negIntGenerator
        val bGen = negIntGenerator
        val (a1, _, ar1) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (a2, _, ar2) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar1)
        val (a3, _, ar3) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar2)
        val (a4, _, ar4) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar3)
        val (a5, _, ar5) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar4)
        val (a6, _, ar6) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar5)
        val (a7, _, _) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar6)
        val (b1, _, br1) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (b2, _, br2) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br1)
        val (b3, _, br3) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br2)
        val (b4, _, br4) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br3)
        val (b5, _, br5) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br4)
        val (b6, _, br6) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br5)
        val (b7, _, _) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br6)
        List(a1, a2, a3, a4, a5) should contain theSameElementsAs List(b1, b2, b3, b4, b5)
        a6 shouldEqual b6
        a7 shouldEqual b7
      }
      it("should produce NegInt edge values first in random order") {
        import Generator._
        val gen = negIntGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: NegInt, ae1: List[NegInt], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val edges = List(a1, a2)
        edges should contain (NegInt(-1))
        edges should contain (NegInt.MaxValue)
      }
    }
    describe("for NegZInts") {
      it("should produce the same NegZInt values in the same order given the same Randomizer") {
        import Generator._
        val aGen= negZIntGenerator
        val bGen = negZIntGenerator
        val (a1, _, ar1) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (a2, _, ar2) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar1)
        val (a3, _, ar3) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar2)
        val (a4, _, ar4) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar3)
        val (a5, _, ar5) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar4)
        val (a6, _, ar6) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar5)
        val (a7, _, _) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar6)
        val (b1, _, br1) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (b2, _, br2) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br1)
        val (b3, _, br3) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br2)
        val (b4, _, br4) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br3)
        val (b5, _, br5) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br4)
        val (b6, _, br6) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br5)
        val (b7, _, _) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br6)
        List(a1, a2, a3, a4, a5) should contain theSameElementsAs List(b1, b2, b3, b4, b5)
        a6 shouldEqual b6
        a7 shouldEqual b7
      }
      it("should produce NegZInt edge values first in random order") {
        import Generator._
        val gen = negZIntGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: NegZInt, ae1: List[NegZInt], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val edges = List(a1, a2, a3)
        edges should contain (NegZInt(0))
        edges should contain (NegZInt(-1))
        edges should contain (NegZInt.MaxValue)
      }
    }
    describe("for NegLongs") {
      it("should produce the same NegLong values in the same order given the same Randomizer") {
        import Generator._
        val aGen= negLongGenerator
        val bGen = negLongGenerator
        val (a1, _, ar1) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (a2, _, ar2) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar1)
        val (a3, _, ar3) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar2)
        val (a4, _, ar4) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar3)
        val (a5, _, ar5) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar4)
        val (a6, _, ar6) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar5)
        val (a7, _, _) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar6)
        val (b1, _, br1) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (b2, _, br2) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br1)
        val (b3, _, br3) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br2)
        val (b4, _, br4) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br3)
        val (b5, _, br5) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br4)
        val (b6, _, br6) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br5)
        val (b7, _, _) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br6)
        List(a1, a2, a3, a4, a5) should contain theSameElementsAs List(b1, b2, b3, b4, b5)
        a6 shouldEqual b6
        a7 shouldEqual b7
      }
      it("should produce NegLong edge values first in random order") {
        import Generator._
        val gen = negLongGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: NegLong, ae1: List[NegLong], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val edges = List(a1, a2)
        edges should contain (NegLong(-1L))
        edges should contain (NegLong.MaxValue)
      }
    }
    describe("for NegZLongs") {
      it("should produce the same NegZLong values in the same order given the same Randomizer") {
        import Generator._
        val aGen= negZLongGenerator
        val bGen = negZLongGenerator
        val (a1, _, ar1) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (a2, _, ar2) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar1)
        val (a3, _, ar3) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar2)
        val (a4, _, ar4) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar3)
        val (a5, _, ar5) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar4)
        val (a6, _, ar6) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar5)
        val (a7, _, _) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar6)
        val (b1, _, br1) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (b2, _, br2) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br1)
        val (b3, _, br3) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br2)
        val (b4, _, br4) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br3)
        val (b5, _, br5) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br4)
        val (b6, _, br6) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br5)
        val (b7, _, _) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br6)
        List(a1, a2, a3, a4, a5) should contain theSameElementsAs List(b1, b2, b3, b4, b5)
        a6 shouldEqual b6
        a7 shouldEqual b7
      }
      it("should produce NegZLong edge values first in random order") {
        import Generator._
        val gen = negZLongGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: NegZLong, ae1: List[NegZLong], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val edges = List(a1, a2, a3)
        edges should contain (NegZLong(0L))
        edges should contain (NegZLong(-1L))
        edges should contain (NegZLong.MaxValue)
      }
    }
    describe("for NegFloat") {
      it("should produce the same NegFloat values in the same order given the same Randomizer") {
        import Generator._
        val aGen= negFloatGenerator
        val bGen = negFloatGenerator
        val (a1, _, ar1) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (a2, _, ar2) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar1)
        val (a3, _, ar3) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar2)
        val (a4, _, ar4) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar3)
        val (a5, _, ar5) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar4)
        val (a6, _, ar6) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar5)
        val (a7, _, _) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar6)
        val (b1, _, br1) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (b2, _, br2) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br1)
        val (b3, _, br3) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br2)
        val (b4, _, br4) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br3)
        val (b5, _, br5) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br4)
        val (b6, _, br6) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br5)
        val (b7, _, _) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br6)
        List(a1, a2, a3, a4, a5) should contain theSameElementsAs List(b1, b2, b3, b4, b5)
        a6 shouldEqual b6
        a7 shouldEqual b7
      }
      it("should produce NegFloat edge values first in random order") {
        import Generator._
        val gen = negFloatGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: NegFloat, ae1: List[NegFloat], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val edges = List(a1, a2, a3)
        edges should contain (NegFloat(-1.0f))
        edges should contain (NegFloat.MaxValue)
        edges should contain (NegFloat.MinValue)
      }
    }
    describe("for NegFiniteFloat") {
      it("should produce the same NegFiniteFloat values in the same order given the same Randomizer") {
        import Generator._
        val aGen= negFiniteFloatGenerator
        val bGen = negFiniteFloatGenerator
        val (a1, _, ar1) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (a2, _, ar2) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar1)
        val (a3, _, ar3) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar2)
        val (a4, _, ar4) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar3)
        val (a5, _, ar5) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar4)
        val (a6, _, ar6) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar5)
        val (a7, _, _) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar6)
        val (b1, _, br1) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (b2, _, br2) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br1)
        val (b3, _, br3) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br2)
        val (b4, _, br4) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br3)
        val (b5, _, br5) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br4)
        val (b6, _, br6) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br5)
        val (b7, _, _) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br6)
        List(a1, a2, a3, a4, a5) should contain theSameElementsAs List(b1, b2, b3, b4, b5)
        a6 shouldEqual b6
        a7 shouldEqual b7
      }
      it("should produce NegFiniteFloat edge values first in random order") {
        import Generator._
        val gen = negFiniteFloatGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: NegFiniteFloat, ae1: List[NegFiniteFloat], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val edges = List(a1, a2, a3)
        edges should contain (NegFiniteFloat(-1.0f))
        edges should contain (NegFiniteFloat.MaxValue)
        edges should contain (NegFiniteFloat.MinValue)
      }
    }
    describe("for NegZFloat") {
      it("should produce the same NegZFloat values in the same order given the same Randomizer") {
        import Generator._
        val aGen= negZFloatGenerator
        val bGen = negZFloatGenerator
        val (a1, _, ar1) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (a2, _, ar2) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar1)
        val (a3, _, ar3) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar2)
        val (a4, _, ar4) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar3)
        val (a5, _, ar5) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar4)
        val (a6, _, ar6) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar5)
        val (a7, _, _) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar6)
        val (b1, _, br1) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (b2, _, br2) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br1)
        val (b3, _, br3) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br2)
        val (b4, _, br4) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br3)
        val (b5, _, br5) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br4)
        val (b6, _, br6) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br5)
        val (b7, _, _) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br6)
        List(a1, a2, a3, a4, a5) should contain theSameElementsAs List(b1, b2, b3, b4, b5)
        a6 shouldEqual b6
        a7 shouldEqual b7
      }
      it("should produce NegZFloat edge values first in random order") {
        import Generator._
        val gen = negZFloatGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: NegZFloat, ae1: List[NegZFloat], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, ae3, ar3) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val (a4, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae3, rnd = ar3)
        val edges = List(a1, a2, a3, a4)
        edges should contain (NegZFloat.MinValue)
        edges should contain (NegZFloat(-1.0f))
        edges should contain (NegZFloat.MaxValue)
        edges should contain (NegZFloat.ensuringValid(-Float.MinPositiveValue))
      }
    }
    describe("for NegZFiniteFloat") {
      it("should produce the same NegZFiniteFloat values in the same order given the same Randomizer") {
        import Generator._
        val aGen= negZFiniteFloatGenerator
        val bGen = negZFiniteFloatGenerator
        val (a1, _, ar1) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (a2, _, ar2) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar1)
        val (a3, _, ar3) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar2)
        val (a4, _, ar4) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar3)
        val (a5, _, ar5) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar4)
        val (a6, _, ar6) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar5)
        val (a7, _, _) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar6)
        val (b1, _, br1) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (b2, _, br2) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br1)
        val (b3, _, br3) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br2)
        val (b4, _, br4) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br3)
        val (b5, _, br5) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br4)
        val (b6, _, br6) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br5)
        val (b7, _, _) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br6)
        List(a1, a2, a3, a4, a5) should contain theSameElementsAs List(b1, b2, b3, b4, b5)
        a6 shouldEqual b6
        a7 shouldEqual b7
      }
      it("should produce NegZFiniteFloat edge values first in random order") {
        import Generator._
        val gen = negZFiniteFloatGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: NegZFiniteFloat, ae1: List[NegZFiniteFloat], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, ae3, ar3) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val (a4, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae3, rnd = ar3)
        val edges = List(a1, a2, a3, a4)
        edges should contain (NegZFiniteFloat.MinValue)
        edges should contain (NegZFiniteFloat(-1.0f))
        edges should contain (NegZFiniteFloat.MaxValue)
        edges should contain (NegZFiniteFloat.ensuringValid(-Float.MinPositiveValue))
      }
    }
    describe("for NegDouble") {
      it("should produce the same NegDouble values in the same order given the same Randomizer") {
        import Generator._
        val aGen= negDoubleGenerator
        val bGen = negDoubleGenerator
        val (a1, _, ar1) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (a2, _, ar2) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar1)
        val (a3, _, ar3) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar2)
        val (a4, _, ar4) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar3)
        val (a5, _, ar5) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar4)
        val (a6, _, ar6) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar5)
        val (a7, _, _) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar6)
        val (b1, _, br1) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (b2, _, br2) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br1)
        val (b3, _, br3) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br2)
        val (b4, _, br4) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br3)
        val (b5, _, br5) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br4)
        val (b6, _, br6) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br5)
        val (b7, _, _) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br6)
        List(a1, a2, a3, a4, a5) should contain theSameElementsAs List(b1, b2, b3, b4, b5)
        a6 shouldEqual b6
        a7 shouldEqual b7
      }
      it("should produce NegDouble edge values first in random order") {
        import Generator._
        val gen = negDoubleGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: NegDouble, ae1: List[NegDouble], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val edges = List(a1, a2, a3)
        edges should contain (NegDouble(-1.0))
        edges should contain (NegDouble.MinValue)
        edges should contain (NegDouble.MaxValue)
      }
    }
    describe("for NegFiniteDouble") {
      it("should produce the same NegFiniteDouble values in the same order given the same Randomizer") {
        import Generator._
        val aGen= negFiniteDoubleGenerator
        val bGen = negFiniteDoubleGenerator
        val (a1, _, ar1) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (a2, _, ar2) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar1)
        val (a3, _, ar3) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar2)
        val (a4, _, ar4) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar3)
        val (a5, _, ar5) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar4)
        val (a6, _, ar6) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar5)
        val (a7, _, _) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar6)
        val (b1, _, br1) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (b2, _, br2) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br1)
        val (b3, _, br3) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br2)
        val (b4, _, br4) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br3)
        val (b5, _, br5) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br4)
        val (b6, _, br6) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br5)
        val (b7, _, _) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br6)
        List(a1, a2, a3, a4, a5) should contain theSameElementsAs List(b1, b2, b3, b4, b5)
        a6 shouldEqual b6
        a7 shouldEqual b7
      }
      it("should produce NegFiniteDouble edge values first in random order") {
        import Generator._
        val gen = negFiniteDoubleGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: NegFiniteDouble, ae1: List[NegFiniteDouble], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val edges = List(a1, a2, a3)
        edges should contain (NegFiniteDouble(-1.0))
        edges should contain (NegFiniteDouble.MinValue)
        edges should contain (NegFiniteDouble.MaxValue)
      }
    }
    describe("for NegZDouble") {
      it("should produce the same NegZDouble values in the same order given the same Randomizer") {
        import Generator._
        val aGen= negZDoubleGenerator
        val bGen = negZDoubleGenerator
        val (a1, _, ar1) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (a2, _, ar2) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar1)
        val (a3, _, ar3) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar2)
        val (a4, _, ar4) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar3)
        val (a5, _, ar5) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar4)
        val (a6, _, ar6) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar5)
        val (a7, _, _) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar6)
        val (b1, _, br1) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (b2, _, br2) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br1)
        val (b3, _, br3) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br2)
        val (b4, _, br4) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br3)
        val (b5, _, br5) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br4)
        val (b6, _, br6) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br5)
        val (b7, _, _) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br6)
        List(a1, a2, a3, a4, a5) should contain theSameElementsAs List(b1, b2, b3, b4, b5)
        a6 shouldEqual b6
        a7 shouldEqual b7
      }
      it("should produce NegZDouble edge values first in random order") {
        import Generator._
        val gen = negZDoubleGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: NegZDouble, ae1: List[NegZDouble], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, ae3, ar3) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val (a4, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae3, rnd = ar3)
        val edges = List(a1, a2, a3, a4)
        edges should contain (NegZDouble.MinValue)
        edges should contain (NegZDouble(-1.0))
        edges should contain (NegZDouble.MaxValue)
        edges should contain (NegZDouble.ensuringValid(-Double.MinPositiveValue))
      }
    }
    describe("for NegZFiniteDouble") {
      it("should produce the same NegZFiniteDouble values in the same order given the same Randomizer") {
        import Generator._
        val aGen= negZFiniteDoubleGenerator
        val bGen = negZFiniteDoubleGenerator
        val (a1, _, ar1) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (a2, _, ar2) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar1)
        val (a3, _, ar3) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar2)
        val (a4, _, ar4) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar3)
        val (a5, _, ar5) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar4)
        val (a6, _, ar6) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar5)
        val (a7, _, _) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar6)
        val (b1, _, br1) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (b2, _, br2) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br1)
        val (b3, _, br3) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br2)
        val (b4, _, br4) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br3)
        val (b5, _, br5) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br4)
        val (b6, _, br6) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br5)
        val (b7, _, _) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br6)
        List(a1, a2, a3, a4, a5) should contain theSameElementsAs List(b1, b2, b3, b4, b5)
        a6 shouldEqual b6
        a7 shouldEqual b7
      }
      it("should produce NegZFiniteDouble edge values first in random order") {
        import Generator._
        val gen = negZFiniteDoubleGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: NegZFiniteDouble, ae1: List[NegZFiniteDouble], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, ae3, ar3) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val (a4, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae3, rnd = ar3)
        val edges = List(a1, a2, a3, a4)
        edges should contain (NegZFiniteDouble.MinValue)
        edges should contain (NegZFiniteDouble(-1.0))
        edges should contain (NegZFiniteDouble.MaxValue)
        edges should contain (NegZFiniteDouble.ensuringValid(-Double.MinPositiveValue))
      }
    }
    describe("for NonZeroInts") {
      it("should produce the same NonZeroInt values in the same order given the same Randomizer") {
        import Generator._
        val aGen= nonZeroIntGenerator
        val bGen = nonZeroIntGenerator
        val (a1, _, ar1) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (a2, _, ar2) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar1)
        val (a3, _, ar3) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar2)
        val (a4, _, ar4) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar3)
        val (a5, _, ar5) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar4)
        val (a6, _, ar6) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar5)
        val (a7, _, _) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar6)
        val (b1, _, br1) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (b2, _, br2) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br1)
        val (b3, _, br3) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br2)
        val (b4, _, br4) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br3)
        val (b5, _, br5) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br4)
        val (b6, _, br6) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br5)
        val (b7, _, _) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br6)
        List(a1, a2, a3, a4, a5) should contain theSameElementsAs List(b1, b2, b3, b4, b5)
        a6 shouldEqual b6
        a7 shouldEqual b7
      }
      it("should produce NonZeroInt edge values first in random order") {
        import Generator._
        val gen = nonZeroIntGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: NonZeroInt, ae1: List[NonZeroInt], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, ae3, ar3) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val (a4, ae4, ar4) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae3, rnd = ar3)
        val edges = List(a1, a2, a3, a4)
        edges should contain (NonZeroInt(-1))
        edges should contain (NonZeroInt.MaxValue)
        edges should contain (NonZeroInt(1))
        edges should contain (NonZeroInt.MinValue)
      }
    }
    describe("for NonZeroLongs") {
      it("should produce the same NonZeroLong values in the same order given the same Randomizer") {
        import Generator._
        val aGen= nonZeroLongGenerator
        val bGen = nonZeroLongGenerator
        val (a1, _, ar1) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (a2, _, ar2) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar1)
        val (a3, _, ar3) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar2)
        val (a4, _, ar4) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar3)
        val (a5, _, ar5) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar4)
        val (a6, _, ar6) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar5)
        val (a7, _, _) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar6)
        val (b1, _, br1) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (b2, _, br2) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br1)
        val (b3, _, br3) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br2)
        val (b4, _, br4) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br3)
        val (b5, _, br5) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br4)
        val (b6, _, br6) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br5)
        val (b7, _, _) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br6)
        List(a1, a2, a3, a4, a5) should contain theSameElementsAs List(b1, b2, b3, b4, b5)
        a6 shouldEqual b6
        a7 shouldEqual b7
      }
      it("should produce NonZeroLong edge values first in random order") {
        import Generator._
        val gen = nonZeroLongGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: NonZeroLong, ae1: List[NonZeroLong], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, ae3, ar3) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val (a4, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae3, rnd = ar3)
        val edges = List(a1, a2, a3, a4)
        edges should contain (NonZeroLong(-1L))
        edges should contain (NonZeroLong.MaxValue)
        edges should contain (NonZeroLong(1L))
        edges should contain (NonZeroLong.MinValue)
      }
    }
    describe("for NonZeroFloat") {
      it("should produce the same NonZeroFloat values in the same order given the same Randomizer") {
        import Generator._
        val aGen= nonZeroFloatGenerator
        val bGen = nonZeroFloatGenerator
        val (a1, _, ar1) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (a2, _, ar2) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar1)
        val (a3, _, ar3) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar2)
        val (a4, _, ar4) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar3)
        val (a5, _, ar5) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar4)
        val (a6, _, ar6) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar5)
        val (a7, _, _) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar6)
        val (b1, _, br1) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (b2, _, br2) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br1)
        val (b3, _, br3) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br2)
        val (b4, _, br4) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br3)
        val (b5, _, br5) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br4)
        val (b6, _, br6) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br5)
        val (b7, _, _) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br6)
        List(a1, a2, a3, a4, a5) should contain theSameElementsAs List(b1, b2, b3, b4, b5)
        a6 shouldEqual b6
        a7 shouldEqual b7
      }
      it("should produce NonZeroFloat edge values first in random order") {
        import Generator._
        val gen = nonZeroFloatGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: NonZeroFloat, ae1: List[NonZeroFloat], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, ae3, ar3) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val (a4, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae3, rnd = ar3)
        val edges = List(a1, a2, a3, a4)
        edges should contain (NonZeroFloat(-1.0f))
        edges should contain (NonZeroFloat(1.0f))
        edges should contain (NonZeroFloat.MaxValue)
        edges should contain (NonZeroFloat.MinValue)
      }
    }
    describe("for NonZeroFiniteFloat") {
      it("should produce the same NonZeroFiniteFloat values in the same order given the same Randomizer") {
        import Generator._
        val aGen= nonZeroFiniteFloatGenerator
        val bGen = nonZeroFiniteFloatGenerator
        val (a1, _, ar1) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (a2, _, ar2) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar1)
        val (a3, _, ar3) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar2)
        val (a4, _, ar4) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar3)
        val (a5, _, ar5) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar4)
        val (a6, _, ar6) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar5)
        val (a7, _, _) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar6)
        val (b1, _, br1) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (b2, _, br2) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br1)
        val (b3, _, br3) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br2)
        val (b4, _, br4) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br3)
        val (b5, _, br5) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br4)
        val (b6, _, br6) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br5)
        val (b7, _, _) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br6)
        List(a1, a2, a3, a4, a5) should contain theSameElementsAs List(b1, b2, b3, b4, b5)
        a6 shouldEqual b6
        a7 shouldEqual b7
      }
      it("should produce NonZeroFiniteFloat edge values first in random order") {
        import Generator._
        val gen = nonZeroFiniteFloatGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: NonZeroFiniteFloat, ae1: List[NonZeroFiniteFloat], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, ae3, ar3) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val (a4, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae3, rnd = ar3)
        val edges = List(a1, a2, a3, a4)
        edges should contain (NonZeroFiniteFloat(-1.0f))
        edges should contain (NonZeroFiniteFloat(1.0f))
        edges should contain (NonZeroFiniteFloat.MaxValue)
        edges should contain (NonZeroFiniteFloat.MinValue)
      }
    }
    describe("for NonZeroDouble") {
      it("should produce the same NonZeroDouble values in the same order given the same Randomizer") {
        import Generator._
        val aGen= nonZeroDoubleGenerator
        val bGen = nonZeroDoubleGenerator
        val (a1, _, ar1) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (a2, _, ar2) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar1)
        val (a3, _, ar3) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar2)
        val (a4, _, ar4) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar3)
        val (a5, _, ar5) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar4)
        val (a6, _, ar6) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar5)
        val (a7, _, _) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar6)
        val (b1, _, br1) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (b2, _, br2) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br1)
        val (b3, _, br3) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br2)
        val (b4, _, br4) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br3)
        val (b5, _, br5) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br4)
        val (b6, _, br6) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br5)
        val (b7, _, _) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br6)
        List(a1, a2, a3, a4, a5) should contain theSameElementsAs List(b1, b2, b3, b4, b5)
        a6 shouldEqual b6
        a7 shouldEqual b7
      }
      it("should produce NonZeroDouble edge values first in random order") {
        import Generator._
        val gen = nonZeroDoubleGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: NonZeroDouble, ae1: List[NonZeroDouble], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, ae3, ar3) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val (a4, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae3, rnd = ar3)
        val edges = List(a1, a2, a3, a4)
        edges should contain (NonZeroDouble(-1.0))
        edges should contain (NonZeroDouble(1.0))
        edges should contain (NonZeroDouble.MinValue)
        edges should contain (NonZeroDouble.MaxValue)
      }
    }
    describe("for NonZeroFiniteDouble") {
      it("should produce the same NonZeroFiniteDouble values in the same order given the same Randomizer") {
        import Generator._
        val aGen= nonZeroFiniteDoubleGenerator
        val bGen = nonZeroFiniteDoubleGenerator
        val (a1, _, ar1) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (a2, _, ar2) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar1)
        val (a3, _, ar3) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar2)
        val (a4, _, ar4) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar3)
        val (a5, _, ar5) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar4)
        val (a6, _, ar6) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar5)
        val (a7, _, _) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar6)
        val (b1, _, br1) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (b2, _, br2) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br1)
        val (b3, _, br3) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br2)
        val (b4, _, br4) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br3)
        val (b5, _, br5) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br4)
        val (b6, _, br6) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br5)
        val (b7, _, _) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br6)
        List(a1, a2, a3, a4, a5) should contain theSameElementsAs List(b1, b2, b3, b4, b5)
        a6 shouldEqual b6
        a7 shouldEqual b7
      }
      it("should produce NonZeroFiniteDouble edge values first in random order") {
        import Generator._
        val gen = nonZeroFiniteDoubleGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: NonZeroFiniteDouble, ae1: List[NonZeroFiniteDouble], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, ae3, ar3) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val (a4, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae3, rnd = ar3)
        val edges = List(a1, a2, a3, a4)
        edges should contain (NonZeroFiniteDouble(-1.0))
        edges should contain (NonZeroFiniteDouble(1.0))
        edges should contain (NonZeroFiniteDouble.MinValue)
        edges should contain (NonZeroFiniteDouble.MaxValue)
      }
    }
    describe("for FiniteFloat") {
      it("should produce the same FiniteFloat values in the same order given the same Randomizer") {
        import Generator._
        val aGen = finiteFloatGenerator
        val bGen = finiteFloatGenerator
        val (a1, _, ar1) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (a2, _, ar2) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar1)
        val (a3, _, ar3) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar2)
        val (a4, _, ar4) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar3)
        val (a5, _, ar5) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar4)
        val (a6, _, ar6) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar5)
        val (a7, _, _) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar6)
        val (b1, _, br1) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (b2, _, br2) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br1)
        val (b3, _, br3) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br2)
        val (b4, _, br4) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br3)
        val (b5, _, br5) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br4)
        val (b6, _, br6) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br5)
        val (b7, _, _) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br6)
        List(a1, a2, a3, a4, a5) should contain theSameElementsAs List(b1, b2, b3, b4, b5)
        a6 shouldEqual b6
        a7 shouldEqual b7
      }
      it("should produce finiteFloat edge values first in random order") {
        import Generator._
        val gen = finiteFloatGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: FiniteFloat, ae1: List[FiniteFloat], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, ae3, ar3) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val (a4, ae4, ar4) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae3, rnd = ar3)
        val (a5, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae4, rnd = ar4)
        val edges = List(a1, a2, a3, a4, a5)
        edges should contain (FiniteFloat(0.0f))
        edges should contain (FiniteFloat(-1.0f))
        edges should contain (FiniteFloat(1.0f))
        edges should contain (FiniteFloat.MaxValue)
        edges should contain (FiniteFloat.MinValue)
      }
    }
    describe("for FiniteDouble") {
      it("should produce the same FiniteDouble values in the same order given the same Randomizer") {
        import Generator._
        val aGen = finiteDoubleGenerator
        val bGen = finiteDoubleGenerator
        val (a1, _, ar1) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (a2, _, ar2) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar1)
        val (a3, _, ar3) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar2)
        val (a4, _, ar4) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar3)
        val (a5, _, ar5) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar4)
        val (a6, _, ar6) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar5)
        val (a7, _, _) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar6)
        val (b1, _, br1) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (b2, _, br2) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br1)
        val (b3, _, br3) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br2)
        val (b4, _, br4) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br3)
        val (b5, _, br5) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br4)
        val (b6, _, br6) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br5)
        val (b7, _, _) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br6)
        List(a1, a2, a3, a4, a5) should contain theSameElementsAs List(b1, b2, b3, b4, b5)
        a6 shouldEqual b6
        a7 shouldEqual b7
      }
      it("should produce FiniteDouble edge values first in random order") {
        import Generator._
        val gen = finiteDoubleGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: FiniteDouble, ae1: List[FiniteDouble], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, ae3, ar3) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val (a4, ae4, ar4) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae3, rnd = ar3)
        val (a5, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae4, rnd = ar4)
        val edges = List(a1, a2, a3, a4, a5)
        edges should contain (FiniteDouble(0.0))
        edges should contain (FiniteDouble(-1.0))
        edges should contain (FiniteDouble(1.0))
        edges should contain (FiniteDouble.MinValue)
        edges should contain (FiniteDouble.MaxValue)
      }
    }
    describe("for NumericChar") {
      it("should produce the same NumericChar values in the same order given the same Randomizer") {
        import Generator._
        val aGen = numericCharGenerator
        val bGen = numericCharGenerator
        val (a1, _, ar1) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (a2, _, ar2) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar1)
        val (a3, _, ar3) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar2)
        val (a4, _, ar4) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar3)
        val (a5, _, ar5) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar4)
        val (a6, _, ar6) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar5)
        val (a7, _, _) = aGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = ar6)
        val (b1, _, br1) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
        val (b2, _, br2) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br1)
        val (b3, _, br3) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br2)
        val (b4, _, br4) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br3)
        val (b5, _, br5) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br4)
        val (b6, _, br6) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br5)
        val (b7, _, _) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br6)
        List(a1, a2, a3, a4, a5) should contain theSameElementsAs List(b1, b2, b3, b4, b5)
        a6 shouldEqual b6
        a7 shouldEqual b7
      }
      it("should produce NumericChar edge values first in random order") {
        import Generator._
        val gen = numericCharGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: NumericChar, ae1: List[NumericChar], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        a1 shouldBe NumericChar('0')
      }
    }

    describe("for Strings") {
      it("should offer a String generator that returns a string whose length equals the passed size") {
  
        import Generator._
        val gen = stringGenerator
  
        val (s1, _, r1) = gen.next(szp = SizeParam(PosZInt(0), 100, 0), edges = Nil, rnd = Randomizer(100))
        s1.length shouldBe 0
  
        val (s2, _, r2) = gen.next(szp = SizeParam(PosZInt(0), 100, 3), edges = Nil, rnd = r1)
        s2.length shouldBe 3
  
        val (s3, _, r3) = gen.next(szp = SizeParam(PosZInt(0), 100, 38), edges = Nil, rnd = r2)
        s3.length shouldBe 38
  
        val (s4, _, r4) = gen.next(szp = SizeParam(PosZInt(0), 100, 88), edges = Nil, rnd = r3)
        s4.length shouldBe 88
  
        val (s5, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = r4)
        s5.length shouldBe 100
      }
      it("should shrink Strings using strategery") {
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
      it("should offer a String generator that offers cononicals based on Char canonicals") {
        import Generator._
        val gen = stringGenerator
        val (canonicalsIt, _) = gen.canonicals(Randomizer.default)
        val canonicals = canonicalsIt.toList
        canonicals(0) shouldBe empty
        canonicals(1) should have length 1
        canonicals(1).head should (be >= 'a' and be <= 'z')
        canonicals(2) should have length 1
        canonicals(2).head should (be >= 'A' and be <= 'Z')
        canonicals(3) should have length 1
        canonicals(3).head should (be >= '0' and be <= '9')
      }
    }
    describe("for Lists") {
      it("should offer a List[T] generator that returns a List[T] whose length equals the passed size") {
  
        import Generator._
        val gen = listGenerator[Int]
  
        val (l1, _, r1) = gen.next(szp = SizeParam(PosZInt(0), 100, 0), edges = Nil, rnd = Randomizer(100))
        l1.length shouldBe 0
  
        val (l2, _, r2) = gen.next(szp = SizeParam(PosZInt(0), 100, 3), edges = Nil, rnd = r1)
        l2.length shouldBe 3
  
        val (l3, _, r3) = gen.next(szp = SizeParam(PosZInt(0), 100, 38), edges = Nil, rnd = r2)
        l3.length shouldBe 38
  
        val (l4, _, r4) = gen.next(szp = SizeParam(PosZInt(0), 100, 88), edges = Nil, rnd = r3)
        l4.length shouldBe 88
  
        val (l5, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = r4)
        l5.length shouldBe 100
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
      it("should return an empty Iterator when asked to shrink a List of size 0") {
        val lstGen = implicitly[Generator[List[Int]]]
        val xs = List.empty[Int]
        lstGen.shrink(xs, Randomizer.default)._1.toList shouldBe empty
      }
      it("should return an Iterator of the canonicals excluding the given values to shrink when asked to shrink a List of size 1") {
        val lstGen = implicitly[Generator[List[Int]]]
        val canonicalLists = List(0, 1, -1, 2, -2, 3, -3).map(i => List(i))
        val expectedLists = List(List.empty[Int]) ++ canonicalLists
        val nonCanonical = List(99)
        lstGen.shrink(nonCanonical, Randomizer.default)._1.toList should contain theSameElementsAs expectedLists
        val canonical = List(3)
        // Ensure 3 (an Int canonical value) does not show up twice in the output
        lstGen.shrink(canonical, Randomizer.default)._1.toList should contain theSameElementsAs expectedLists
      }
      it("should return an Iterator that does not repeat canonicals when asked to shrink a List of size 2 that includes canonicals") {
        val lstGen = implicitly[Generator[List[Int]]]
        val shrinkees = lstGen.shrink(List(3, 99), Randomizer.default)._1.toList
        shrinkees.distinct should contain theSameElementsAs shrinkees
      }
      it("should return an Iterator that does not repeat the passed list-to-shink even if that list has a power of 2 length") {
        // Since the last batch of lists produced by the list shrinker start at length 2 and then double in size each time,
        // they lengths will be powers of two: 2, 4, 8, 16, etc... So make sure that if the original length has length 16,
        // for example, that that one doesn't show up in the shrinks output, because it would be the original list-to-shrink.
        val lstGen = implicitly[Generator[List[Int]]]
        val listToShrink = List.fill(16)(99)
        val shrinkees = lstGen.shrink(listToShrink, Randomizer.default)._1.toList
        shrinkees.distinct should not contain listToShrink
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
    describe("for Function0s") {
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
    }
    describe("for arbitrary Function1s") {
      it("should offer an implicit provider that uses hashCode to tweak a seed and has a pretty toString") {
        val gen = implicitly[Generator[Option[Int] => List[Int]]]
        val sample = gen.sample
        sample.toString should include ("Option[Int]")
        sample.toString should include ("List[Int]")
      }
    }
    describe("for Tuple2s") {
      it("should offer a tuple2 generator") {
        val gen = implicitly[Generator[(Int, Int)]]
        val intGen = implicitly[Generator[Int]]
        val (it8, rnd1) = intGen.shrink(8, Randomizer.default())
        val (it18, rnd2)= intGen.shrink(18, rnd1)
        val list8 = it8.toList
        val list18 = it18.toList
        val listTup =
          for {
            x <- list8
            y <- list18
          } yield (x, y)
        gen.shrink((8, 18), rnd2)._1.toList shouldEqual listTup
      }
      it("should be able to transform a tuple generator to a case class generator") {
        val tupGen: Generator[(String, Int)] = Generator.tuple2Generator[String, Int]
        case class Person(name: String, age: Int)
        val persons = for (tup <- tupGen) yield Person(tup._1, tup._2)
        val (it, _) = persons.shrink(Person("Harry Potter", 32), Randomizer.default())
        it.toList should not be empty
      }
    }
    describe("for Int => Ints") {
      it("should have toString and simpleName that doesn't include org.scalatest.prop.valueOf") {
        import GeneratorDrivenPropertyChecks._
        forAll { (f: Int => Int) =>
          f.toString should startWith ("(i: Int) => ")
          f.toString should not include "org.scalatest.prop.valueOf"
        }
      }
    }
  }
}

