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
import org.scalatest.Inspectors.{forAll => inspectAll}
import org.scalatest.tagobjects.Flicker
import org.scalactic.ColCompatHelper.LazyListOrStream
import org.scalactic.source

class GeneratorSpec extends AnyFunSpec with Matchers {

  implicit def roseTreeGenerator[A](implicit genOfA: Generator[A]): Generator[RoseTree[A]]  = {
    new Generator[RoseTree[A]] {
      def nextImpl(szp: SizeParam, rnd: Randomizer): (RoseTree[RoseTree[A]], Randomizer) = {
        val (rtOfRTOfA, edgesOfRTOfA, nxtRnd) = genOfA.next(szp, List.empty, rnd)
        (Rose(rtOfRTOfA), nxtRnd)
      }
    }
  }

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
      a1.value._1 should not equal a2.value._1
      a1.value._2 should not equal a2.value._2
      val (b1, _, br1) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
      val (b2, _, br2) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br1)
      val (b3, _, _) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br2)
      a1.value shouldEqual b1.value
      a2.value shouldEqual b2.value
      a3.value shouldEqual b3.value
    }
    it("should offer a map method that composes canonicals methods") {

      import Generator._

      val rnd = Randomizer.default
      val intCanonicalsIt = intGenerator.canonicals
      val (charRt, _, _) = charGenerator.next(SizeParam(1, 0, 1), List.empty, rnd)
      val charValue = charRt.value
      val expectedTupCanonicals = intCanonicalsIt.map(i => (charValue, i.value)).toList

      val tupGen = for (i <- intGenerator) yield (charValue, i)
      val tupCanonicalsIt = tupGen.canonicals
      val tupCanonicals = tupCanonicalsIt.map(_.value).toList

      tupCanonicals shouldBe expectedTupCanonicals
    }
    it("should offer a flatMap method that composes canonicals methods") {

      import Generator._

      val rnd = Randomizer.default
      val intCanonicalsIt = intGenerator.canonicals
      val intCanonicals = intCanonicalsIt.toList
      val doubleCanonicalsIt = doubleGenerator.canonicals
      val doubleCanonicals = doubleCanonicalsIt.toList
      val expectedTupCanonicals: List[(Int, Double)] =
          for {
            i <- intCanonicals
            d <- doubleCanonicals
          } yield (i.value, d.value)

      val tupGen =
        for {
          i <- intGenerator
          d <- doubleGenerator
        }  yield (i, d)
      val tupCanonicalsIt = tupGen.canonicals
      val tupCanonicals = tupCanonicalsIt.map(rt => rt.value).toList

      tupCanonicals shouldBe expectedTupCanonicals
    }
    it("should offer a filter method so that pattern matching can be used in for expressions with Generator generators") {
      """for ((a, b) <- CommonGenerators.tuple2s[String, Int]) yield (b, a)""" should compile
      case class Person(name: String, age: Int)
      // SKIP-DOTTY-START
      val persons = CommonGenerators.instancesOf(Person) { p => (p.name, p.age) }
      // SKIP-DOTTY-END
      //DOTTY-ONLY val persons = CommonGenerators.instancesOf[(String, Int), Person](Person.apply) { p => (p.name, p.age) }
      """for (Person(a, b) <- persons) yield (b, a)""" should compile
    }
    it("should offer a filter method that throws an exception if too many objects are filtered out") {
      val doNotDoThisAtHome = CommonGenerators.ints.filter(i => i == 0) // Only keep zero
      a [IllegalStateException] should be thrownBy {
        doNotDoThisAtHome.next(SizeParam(PosZInt(0), 100, 100), Nil, Randomizer.default)
      }
      val okToDoThisAtHome = CommonGenerators.ints.filter(i => i != 0) // Only keep non-zeros
      noException should be thrownBy {
        okToDoThisAtHome.next(SizeParam(PosZInt(0), 100, 100), Nil, Randomizer.default)
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
      a1.value._1 should not equal a2.value._1
      a1.value._2 should not equal a2.value._2
      val (b1, _, br1) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = Randomizer(100))
      val (b2, _, br2) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br1)
      val (b3, _, _) = bGen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = br2)
      a1.value shouldEqual b1.value
      a2.value shouldEqual b2.value
      a3.value shouldEqual b3.value
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
        implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(minSuccessful = 10)
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
        val expectedTestDiscarded = 49
        val maxDiscardedFactor = PosZDouble.ensuringValid(PropertyCheckConfiguration.calculateMaxDiscardedFactor(10, expectedTestDiscarded))
        implicit val generatorDrivenConfig = PropertyCheckConfiguration(maxDiscardedFactor = maxDiscardedFactor)
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
      values.map(_.value) should contain theSameElementsAs expectedInitEdges
    }

    describe("for Booleans") {
      it("should produce true and false more or less equally") {
        import Generator._

        val classification = CommonGenerators.classify(100000, booleanGenerator) {
          case x if x => "true"
          case _ => "false"
        }
        classification.portions("true") should be (0.5 +- 0.01)
      }

      it("should produce the same Boolean values in the same order given the same Randomizer") {
        import Generator._
        @scala.annotation.tailrec
        def loop(n: Int, rnd: Randomizer, results: List[Boolean]): List[Boolean] = {
          if (n == 0)
            results
          else {
            val (bool, _, nextRnd) = booleanGenerator.next(SizeParam(0, 0, 0), Nil, rnd)
            loop(n - 1, nextRnd, bool.value :: results)
          }
        }

        val rnd = Randomizer.default
        val firstRound = loop(100, rnd, Nil)
        val secondRound = loop(100, rnd, Nil)

        firstRound should contain theSameElementsAs(secondRound)
      }
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
        List(a1.value, a2.value, a3.value, a4.value, a5.value) should contain theSameElementsAs List(b1.value, b2.value, b3.value, b4.value, b5.value)
        a6.value shouldEqual b6.value
        a7.value shouldEqual b7.value
      }
      it("should produce Byte edge values first in random order") {
        import Generator._
        val gen = byteGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: RoseTree[Byte], ae1: List[Byte], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, ae3, ar3) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val (a4, ae4, ar4) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae3, rnd = ar3)
        val (a5, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae4, rnd = ar4)
        val edges = List(a1, a2, a3, a4, a5)
        edges.map(_.value) should contain (0)
        edges.map(_.value) should contain (1)
        edges.map(_.value) should contain (-1)
        edges.map(_.value) should contain (Byte.MaxValue)
        edges.map(_.value) should contain (Byte.MinValue)
      }
      it("should produce Byte canonical values") {
        import Generator._
        val gen = byteGenerator
        val canonicals = gen.canonicals
        canonicals.map(_.value).toList shouldBe List(-3, 3, -2, 2, -1, 1, 0).map(_.toByte)
      }
      it("should shrink Bytes by repeatedly halving and negating") {
        import GeneratorDrivenPropertyChecks._
        forAll { (shrinkRoseTree: RoseTree[Byte]) =>
          val b = shrinkRoseTree.value
          val shrinks: LazyListOrStream[Byte] = shrinkRoseTree.shrinks.map(_.value)
          shrinks.distinct.length shouldEqual shrinks.length
          if (b == 0)
            shrinks shouldBe empty
          else {
            if (b > 1.toByte)
              shrinks.head should be < 0.toByte
            else if (b < -1.toByte)
              shrinks.head should be > 0.toByte
            import org.scalatest.Inspectors._
            val revShrinks = shrinks.reverse
            val pairs: LazyListOrStream[(Byte, Byte)] = revShrinks.zip(revShrinks.tail)
            forAll (pairs) { case (x, y) =>
              assert(x == 0 || x == -y || x.abs == y.abs / 2)
            }
          }
        }
      }
      it("should produce shrinkees following constraint determined by filter method") {
        val aGen= Generator.byteGenerator.filter(_ > 5)
        val (rs, _, _) = aGen.next(SizeParam(1, 0, 1), List(30.toByte), Randomizer.default)
        val shrinkees = rs.shrinks.map(_.value)
        shrinkees should not be empty
        shrinkees.toList shouldBe List(15.toByte, 7.toByte)
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
        List(a1.value, a2.value, a3.value, a4.value, a5.value) should contain theSameElementsAs List(b1.value, b2.value, b3.value, b4.value, b5.value)
        a6.value shouldEqual b6.value
        a7.value shouldEqual b7.value
      }
      it("should produce Short edge values first in random order") {
        import Generator._
        val gen = shortGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: RoseTree[Short], ae1: List[Short], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, ae3, ar3) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val (a4, ae4, ar4) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae3, rnd = ar3)
        val (a5, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae4, rnd = ar4)
        val edges = List(a1, a2, a3, a4, a5)
        edges.map(_.value) should contain (0)
        edges.map(_.value) should contain (1)
        edges.map(_.value) should contain (-1)
        edges.map(_.value) should contain (Short.MaxValue)
        edges.map(_.value) should contain (Short.MinValue)
      }
      it("should produce Short canonical values") {
        import Generator._
        val gen = shortGenerator
        val canonicals = gen.canonicals
        canonicals.map(_.value).toList shouldBe List(-3, 3, -2, 2, -1, 1, 0).map(_.toShort)
      }
      it("should shrink Shorts by repeatedly halving and negating") {
        import GeneratorDrivenPropertyChecks._
        forAll { (shrinkRoseTree: RoseTree[Short]) =>
          val n = shrinkRoseTree.value
          val shrinks: LazyListOrStream[Short] = shrinkRoseTree.shrinks.map(_.value)
          shrinks.distinct.length shouldEqual shrinks.length
          if (n == 0)
            shrinks shouldBe empty
          else {
            if (n > 1.toShort)
              shrinks.head should be < 0.toShort
            else if (n < -1.toShort)
              shrinks.head should be > 0.toShort
            import org.scalatest.Inspectors._
            val revShrinks = shrinks.reverse
            val pairs: LazyListOrStream[(Short, Short)] = revShrinks.zip(revShrinks.tail)
            forAll (pairs) { case (x, y) =>
              assert(x == 0 || x == -y || x.abs == y.abs / 2)
            }
          }
        }
      }
      it("should produce shrinkees following constraint determined by filter method") {
        val aGen= Generator.shortGenerator.filter(_ > 5)
        val (rs, _, _) = aGen.next(SizeParam(1, 0, 1), List(30.toShort), Randomizer.default)
        val shrinkees = rs.shrinks.map(_.value)
        shrinkees should not be empty
        shrinkees.toList shouldBe List(15.toShort, 7.toShort)
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
        List(a1.value, a2.value, a3.value, a4.value, a5.value) should contain theSameElementsAs List(b1.value, b2.value, b3.value, b4.value, b5.value)
        a6.value shouldEqual b6.value
        a7.value shouldEqual b7.value
      }
      it("should produce Int edge values first in random order") {
        import Generator._
        val gen = intGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: RoseTree[Int], ae1: List[Int], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, ae3, ar3) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val (a4, ae4, ar4) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae3, rnd = ar3)
        val (a5, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae4, rnd = ar4)
        val edges = List(a1, a2, a3, a4, a5)
        edges.map(_.value) should contain (0)
        edges.map(_.value) should contain (1)
        edges.map(_.value) should contain (-1)
        edges.map(_.value) should contain (Int.MaxValue)
        edges.map(_.value) should contain (Int.MinValue)
      }
      it("should produce Int canonical values") {
        import Generator._
        val gen = intGenerator
        val canonicals = gen.canonicals
        canonicals.map(_.value).toList shouldBe List(-3, 3, -2, 2, -1, 1, 0)
      }
      it("should shrink Ints by algo towards 0") {
        import GeneratorDrivenPropertyChecks._
        forAll { (shrinkRoseTree: RoseTree[Int]) =>
          val i = shrinkRoseTree.value
          val shrinks: LazyListOrStream[Int] = shrinkRoseTree.shrinks.map(_.value)
          shrinks.distinct.length shouldEqual shrinks.length
          if (i == 0)
            shrinks shouldBe empty
          else {
            shrinks should not be empty
            inspectAll(shrinks) { s =>
              if (i >= 0)
                s should be < i
              else
                s should be > i  
            }
          }
        }
      }
      it("should produce shrinkees following constraint determined by filter method") {
        val aGen= Generator.intGenerator.filter(_ > 5)
        val (rs, _, _) = aGen.next(SizeParam(1, 0, 1), List(30), Randomizer.default)
        val shrinkees = rs.shrinks.map(_.value)
        shrinkees should not be empty
        shrinkees.toList shouldBe List(15, 7)
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
        List(a1.value, a2.value, a3.value, a4.value, a5.value) should contain theSameElementsAs List(b1.value, b2.value, b3.value, b4.value, b5.value)
        a6.value shouldEqual b6.value
        a7.value shouldEqual b7.value
      }
      it("should produce Long edge values first in random order") {
        import Generator._
        val gen = longGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: RoseTree[Long], ae1: List[Long], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, ae3, ar3) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val (a4, ae4, ar4) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae3, rnd = ar3)
        val (a5, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae4, rnd = ar4)
        val edges = List(a1, a2, a3, a4, a5)
        edges.map(_.value) should contain (0)
        edges.map(_.value) should contain (1)
        edges.map(_.value) should contain (-1)
        edges.map(_.value) should contain (Long.MaxValue)
        edges.map(_.value) should contain (Long.MinValue)
      }
      it("should produce Long canonical values") {
        import Generator._
        val gen = longGenerator
        val canonicals = gen.canonicals
        canonicals.map(_.value).toList shouldBe List(-3L, 3L, -2L, 2L, -1L, 1L, 0L)
      }
      it("should shrink Longs by repeatedly halving and negating") {
        import GeneratorDrivenPropertyChecks._
        forAll { (shrinkRoseTree: RoseTree[Long]) =>
          val n = shrinkRoseTree.value
          val shrinks: LazyListOrStream[Long] = shrinkRoseTree.shrinks.map(_.value)
          shrinks.distinct.length shouldEqual shrinks.length
          if (n == 0)
            shrinks shouldBe empty
          else {
            if (n > 1L)
              shrinks.head should be < 0L
            else if (n < -1L)
              shrinks.head should be > 0L
            import org.scalatest.Inspectors._
            val revShrinks = shrinks.reverse
            val pairs: LazyListOrStream[(Long, Long)] = revShrinks.zip(revShrinks.tail)
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
      it("should produce shrinkees following constraint determined by filter method") {
        val aGen= Generator.longGenerator.filter(_ > 5)
        val (rs, _, _) = aGen.next(SizeParam(1, 0, 1), List(30L), Randomizer.default)
        val shrinkees = rs.shrinks.map(_.value)
        shrinkees should not be empty
        shrinkees.toList shouldBe List(15L, 7L)
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
        List(a1.value, a2.value, a3.value, a4.value, a5.value) should contain theSameElementsAs List(b1.value, b2.value, b3.value, b4.value, b5.value)
        a6.value shouldEqual b6.value
        a7.value shouldEqual b7.value
      }
      it("should produce Char edge values first in random order") {
        import Generator._
        val gen = charGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: RoseTree[Char], ae1: List[Char], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val edges = List(a1, a2)
        edges.map(_.value) should contain (Char.MinValue)
        edges.map(_.value) should contain (Char.MaxValue)
      }
      it("should produce Char canonical values") {
        import Generator._
        val gen = charGenerator
        val canonicalsIt = gen.canonicals
        val canonicals = canonicalsIt.map(_.value).toList
        import org.scalatest.Inspectors
        Inspectors.forAll (canonicals) { c => c should (be >= 'a' and be <= 'z') }
      }
      it("should shrink Chars by trying selected printable characters") {
        import GeneratorDrivenPropertyChecks._
        val expectedChars = "9876543210ZYXWVUTSRQPONMLKJIHGFEDCBAzyxwvutsrqponmljkihgfedcba".toList
        val generator = implicitly[Generator[Char]]
        forAll { (shrinkRoseTree: RoseTree[Char]) =>
          val c = shrinkRoseTree.value
          val shrinks: LazyListOrStream[Char] = shrinkRoseTree.shrinks.map(_.value)
          shrinks.distinct.length shouldEqual shrinks.length
          if (c >= '0' && c <= '9' || c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z')
            shrinks shouldBe empty
          else
            shrinks shouldEqual expectedChars
        }
        import org.scalatest.Inspectors
        Inspectors.forAll (expectedChars) { (c: Char) =>
          val (shrinkRoseTree, _, _) = generator.next(SizeParam(1, 0, 1), List(c), Randomizer.default)
          val shrinks: LazyListOrStream[Char] = shrinkRoseTree.shrinks.map(_.value)
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
        a1.value shouldEqual b1.value
        a2.value shouldEqual b2.value
        a3.value shouldEqual b3.value
      }
      it("should produce the Float edge value first") {
        import Generator._
        val gen = floatGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: RoseTree[Float], ae1: List[Float], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, ae3, ar3) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val (a4, ae4, ar4) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae3, rnd = ar3)
        val (a5, ae5, ar5) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae4, rnd = ar4)
        val (a6, ae6, ar6) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae5, rnd = ar5)
        val (a7, ae7, ar7) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae6, rnd = ar6)
        val (a8, ae8, ar8) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae7, rnd = ar7)
        val (a9, ae9, ar9) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae8, rnd = ar8)
        val (a10, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae9, rnd = ar9)
        val edges = List(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10).map(_.value)
        edges should contain (Float.NegativeInfinity)
        edges should contain (Float.MinValue)
        edges should contain (-1.0F)
        edges should contain (-Float.MinPositiveValue)
        edges should contain (-0.0F)
        edges should contain (0.0F)
        edges should contain (Float.MinPositiveValue)
        edges should contain (1.0F)
        edges should contain (Float.MaxValue)
        edges should contain (Float.PositiveInfinity)
      }
      it("should produce Float canonical values") {
        import Generator._
        val gen = floatGenerator
        val canonicals = gen.canonicals
        canonicals.map(_.value).toList shouldBe List(-3.0f, 3.0f, -2.0f, 2.0f, -1.0f, 1.0f, 0.0f)
      }
      it("should shrink Floats with an algo towards 0") {
        import GeneratorDrivenPropertyChecks._
        forAll { (shrinkRoseTree: RoseTree[Float]) =>
          val i = shrinkRoseTree.value
          val shrinks: LazyListOrStream[Float] = shrinkRoseTree.shrinks.map(_.value)
          shrinks.distinct.length shouldEqual shrinks.length
          if (i == 0.0f)
            shrinks shouldBe empty
          else {
            shrinks should not be empty
            inspectAll(shrinks) { s =>
              if (i >= 0.0f)
                s should be < i
              else
                s should be > i  
            }  
          }
        }
      }
      it("should shrink Floats by dropping the fraction part then repeatedly 'square-rooting' and negating") {
        import GeneratorDrivenPropertyChecks._
        forAll { (shrinkRoseTree: RoseTree[Float]) =>
          val fv = shrinkRoseTree.value
          val shrinks: LazyListOrStream[Float] = shrinkRoseTree.shrinks.map(_.value)
          shrinks.distinct.length shouldEqual shrinks.length
          if (fv == 0.0f) {
            shrinks shouldBe empty
          } else {
            val n =
              if (fv == Float.PositiveInfinity || fv.isNaN)
                Float.MaxValue
              else if (fv == Float.NegativeInfinity)
                Float.MinValue
              else fv
            if (n > 1.0f)
              shrinks.head should be < 0.0f
            else if (n < -1.0f)
              shrinks.head should be < 0.0f
            import org.scalatest.Inspectors._
            if (!n.isWhole) {
              shrinks.head shouldEqual (if (n > 0.0f) (-n).ceil else n.ceil)
            }
            val revShrinks = shrinks.reverse
            val pairs: LazyListOrStream[(Float, Float)] = revShrinks.zip(revShrinks.tail)
            forAll (pairs) { case (x, y) =>
              assert(x == 0.0f || x == -y || x.abs < y.abs)
            }
          }
        }
      }
      it("should produce shrinkees following constraint determined by filter method") {
        val aGen= Generator.floatGenerator.filter(_ > 5.0)
        val (rs, _, _) = aGen.next(SizeParam(1, 0, 1), List(40.0f), Randomizer.default)
        val shrinkees = rs.shrinks.map(_.value)
        shrinkees should not be empty
        shrinkees.toList shouldBe List(6.0f)
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
        a1.value shouldEqual b1.value
        a2.value shouldEqual b2.value
        a3.value shouldEqual b3.value
      }
      it("should produce the Double edge value first") {
        import Generator._
        val gen = doubleGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: RoseTree[Double], ae1: List[Double], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, ae3, ar3) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val (a4, ae4, ar4) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae3, rnd = ar3)
        val (a5, ae5, ar5) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae4, rnd = ar4)
        val (a6, ae6, ar6) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae5, rnd = ar5)
        val (a7, ae7, ar7) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae6, rnd = ar6)
        val (a8, ae8, ar8) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae7, rnd = ar7)
        val (a9, ae9, ar9) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae8, rnd = ar8)
        val (a10, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae9, rnd = ar9)
        val edges = List(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10).map(_.value)
        edges should contain (Double.NegativeInfinity)
        edges should contain (Double.MinValue)
        edges should contain (-1.0)
        edges should contain (-Double.MinPositiveValue)
        edges should contain (-0.0)
        edges should contain (0.0)
        edges should contain (Double.MinPositiveValue)
        edges should contain (1.0)
        edges should contain (Double.MaxValue)
        edges should contain (Double.PositiveInfinity)
      }
      it("should produce Double canonical values") {
        import Generator._
        val gen = doubleGenerator
        val canonicals = gen.canonicals
        canonicals.map(_.value).toList shouldBe List(-3.0, 3.0, -2.0, 2.0, -1.0, 1.0, 0.0)
      }
      it("should shrink Doubles with an algo towards 0") {
        import GeneratorDrivenPropertyChecks._
        forAll { (shrinkRoseTree: RoseTree[Double]) =>
          val i = shrinkRoseTree.value
          val shrinks: LazyListOrStream[Double] = shrinkRoseTree.shrinks.map(_.value)
          shrinks.distinct.length shouldEqual shrinks.length
          if (i == 0.0)
            shrinks shouldBe empty
          else {
            shrinks should not be empty
            inspectAll(shrinks) { s =>
              if (i >= 0.0)
                s should be < i
              else
                s should be > i  
            }  
          }
        }
      }
      it("should produce shrinkees following constraint determined by filter method") {
        val aGen= Generator.doubleGenerator.filter(_ > 5.0)
        val (rs, _, _) = aGen.next(SizeParam(1, 0, 1), List(40.0), Randomizer.default)
        val shrinkees = rs.shrinks.map(_.value)
        shrinkees should not be empty
        shrinkees.toList shouldBe List(6.0)
      }
    }

    // SKIP-DOTTY-START
    /**
      * Boilerplate reduction for those `(LazyListOrStream[T], Randomizer)` pairs returned
      * from `canonicals()` and `shrink()`
      *
      * @param pair the returned values from the Generator method
      * @tparam T the type of the Generator
      */  
    implicit class GeneratorLazyListOrStreamPairOps[T](iter: LazyListOrStream[T]) {
    // SKIP-DOTTY-END  
      /**
        * Helper method for testing canonicals and shrinks, which should always be
        * "growing".
        *
        * The definition of "growing" means, essentially, "moving further from zero".
        * Sometimes that's in the positive direction (eg, PosInt), sometimes negative
        * (NegFloat), sometimes both (NonZeroInt).
        *
        * This returns Unit, because it's all about the assertion.
        *
        * This is a bit loose and approximate, but sufficient for the various
        * Scalactic types.
        *
        * @param iter an LazyListOrStream over a type, typically a Scalactic type
        * @param conv a conversion function from the Scalactic type to an ordinary Numeric
        * @tparam T the Scalactic type
        * @tparam N the underlying ordered numeric type
        */
      // SKIP-DOTTY-START  
      def shouldGrowWithForGeneratorLazyListOrStreamPair[N: Ordering](conv: T => N)(implicit nOps: Numeric[N]): Unit = {
      // SKIP-DOTTY-END  
      //DOTTY-ONLY extension [T](iter: LazyListOrStream[T]) def shouldGrowWithForGeneratorLazyListOrStreamPair[N: Ordering](conv: T => N)(implicit nOps: Numeric[N]): Unit = {  
        iter.reduce { (last, cur) =>
          // Duplicates not allowed:
          last should not equal cur
          val nLast = nOps.abs(conv(last))
          val nCur = nOps.abs(conv(cur))
          nLast should be >= nCur
          cur
        }
      }
    // SKIP-DOTTY-START  
    }
    // SKIP-DOTTY-END

    // SKIP-DOTTY-START
    implicit class GeneratorRoseTreePairOps[T](pair: (RoseTree[T], Randomizer)) {
    // SKIP-DOTTY-END  
      /**
        * Helper method for testing canonicals and shrinks, which should always be
        * "growing".
        *
        * The definition of "growing" means, essentially, "moving further from zero".
        * Sometimes that's in the positive direction (eg, PosInt), sometimes negative
        * (NegFloat), sometimes both (NonZeroInt).
        *
        * This returns Unit, because it's all about the assertion.
        *
        * This is a bit loose and approximate, but sufficient for the various
        * Scalactic types.
        *
        * @param iter an LazyListOrStream over a type, typically a Scalactic type
        * @param conv a conversion function from the Scalactic type to an ordinary Numeric
        * @tparam T the Scalactic type
        * @tparam N the underlying ordered numeric type
        */
      // SKIP-DOTTY-START  
      def shouldGrowWithForGeneratorRoseTreePair[N: Ordering](conv: T => N)(implicit nOps: Numeric[N]): Unit = {
      // SKIP-DOTTY-END  
      //DOTTY-ONLY extension [T](pair: (RoseTree[T], Randomizer)) def shouldGrowWithForGeneratorRoseTreePair[N: Ordering](conv: T => N)(implicit nOps: Numeric[N]): Unit = {  
        val roseTree: RoseTree[T] = pair._1
        roseTree.shrinks.map(_.value).reduce { (last, cur) =>
          // Duplicates not allowed:
          last should not equal cur
          val nLast = nOps.abs(conv(last))
          val nCur = nOps.abs(conv(cur))
          nLast should be >= nCur
          cur
        }
      }
    // SKIP-DOTTY-START  
    }
    // SKIP-DOTTY-END

    // SKIP-DOTTY-START
    implicit class GeneratorOps[T](gen: Generator[T]) {
    // SKIP-DOTTY-END  
      /**
       * Helper method for testing shrinks, which should always be
       * "growing".
       *
       * The definition of "growing" means, essentially, "moving further from zero".
       * Sometimes that's in the positive direction (eg, PosInt), sometimes negative
       * (NegFloat), sometimes both (NonZeroInt).
       *
       * This returns Unit, because it's all about the assertion.
       *
       * This is a bit loose and approximate, but sufficient for the various
       * Scalactic types.
       *
       * @param iter an LazyListOrStream over a type, typically a Scalactic type
       * @param conv a conversion function from the Scalactic type to an ordinary Numeric
       * @tparam T the Scalactic type
       * @tparam N the underlying ordered numeric type
       */
      // SKIP-DOTTY-START
      def shouldGrowWithForShrink[N: Ordering](conv: T => N)(implicit nOps: Numeric[N], pos: source.Position): Unit = {
      // SKIP-DOTTY-END
      //DOTTY-ONLY extension [T](gen: Generator[T]) def shouldGrowWithForShrink[N: Ordering](conv: T => N)(implicit nOps: Numeric[N]): Unit = {
        val rnd = Randomizer.default
        val maxSize = PosZInt(100)
        val (size, nextRnd) = rnd.choosePosZInt(1, maxSize) // size will be positive because between 1 and 100, inclusive
        val (roseTree, _, _) = gen.next(SizeParam(PosZInt(0), maxSize, size), Nil, nextRnd)
        val shrunken = roseTree.shrinks.map(_.value)
        if (shrunken.length > 0) {
          shrunken.reduce { (last, cur) =>
            // Duplicates not allowed:
            last should not equal cur
            val nLast = nOps.abs(conv(last))
            val nCur = nOps.abs(conv(cur))
            nLast should be >= nCur
            cur
          }
        }
      }
    // SKIP-DOTTY-START  
    }
    // SKIP-DOTTY-END

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
        List(a1.value, a2.value, a3.value, a4.value, a5.value) should contain theSameElementsAs List(b1.value, b2.value, b3.value, b4.value, b5.value)
        a6.value shouldEqual b6.value
        a7.value shouldEqual b7.value
      }
      it("should produce PosInt edge values first in random order") {
        import Generator._
        val gen = posIntGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: RoseTree[PosInt], ae1: List[PosInt], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val edges = List(a1, a2).map(_.value)
        edges should contain (PosInt(1))
        edges should contain (PosInt.MaxValue)
      }

      it("should have legitimate canonicals") {
        import Generator._
        val gen = posIntGenerator
        val rnd = Randomizer.default
        gen.canonicals.shouldGrowWithForGeneratorLazyListOrStreamPair(_.value.value)
      }
      
      it("should shrink PosInts by algo towards 1") {
        import GeneratorDrivenPropertyChecks._
        forAll { (shrinkRoseTree: RoseTree[PosInt]) =>
          val i = shrinkRoseTree.value
          val shrinks: LazyListOrStream[PosInt] = shrinkRoseTree.shrinks.map(_.value)
          shrinks.distinct.length shouldEqual shrinks.length
          if (i.value == 0)
            shrinks shouldBe empty
          else {
            shrinks should not be empty
            inspectAll(shrinks) { s =>
              s.value should be < i.value  
            }
          }
        }
      }

      it("should produce shrinkees following constraint determined by filter method") {
        val aGen= Generator.posIntGenerator.filter(_.value > 5)
        val (rs, _, _) = aGen.next(SizeParam(1, 0, 1), List(PosInt(30)), Randomizer.default)
        val shrinkees = rs.shrinks.map(_.value)
        shrinkees should not be empty
        shrinkees.toList shouldBe List(PosInt(15), PosInt(7))
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
        List(a1.value, a2.value, a3.value, a4.value, a5.value) should contain theSameElementsAs List(b1.value, b2.value, b3.value, b4.value, b5.value)
        a6.value shouldEqual b6.value
        a7.value shouldEqual b7.value
      }
      it("should produce PosZInt edge values first in random order") {
        import Generator._
        val gen = posZIntGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: RoseTree[PosZInt], ae1: List[PosZInt], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val edges = List(a1, a2, a3).map(_.value)
        edges should contain (PosZInt(0))
        edges should contain (PosZInt(1))
        edges should contain (PosZInt.MaxValue)
      }

      it("should have legitimate canonicals") {
        import Generator._
        val gen = posZIntGenerator
        val rnd = Randomizer.default
        gen.canonicals.shouldGrowWithForGeneratorLazyListOrStreamPair(_.value.value)
      }

      it("should shrink PosZInts by algo towards 0") {
        import GeneratorDrivenPropertyChecks._
        forAll { (shrinkRoseTree: RoseTree[PosZInt]) =>
          val i = shrinkRoseTree.value
          val shrinks: List[PosZInt] = shrinkRoseTree.shrinks.map(_.value).toList
          shrinks.distinct.length shouldEqual shrinks.length
          if (i.value == 0)
            shrinks shouldBe empty
          else {
            shrinks should not be empty
            inspectAll(shrinks) { s =>
              s.value should be < i.value  
            }
          }
        }
      }

      it("should produce shrinkees following constraint determined by filter method") {
        val aGen= Generator.posZIntGenerator.filter(_.value > 5)
        val (rs, _, _) = aGen.next(SizeParam(1, 0, 1), List(PosZInt(30)), Randomizer.default)
        val shrinkees = rs.shrinks.map(_.value)
        shrinkees should not be empty
        shrinkees.toList shouldBe List(PosZInt(15), PosZInt(7))
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
        List(a1.value, a2.value, a3.value, a4.value, a5.value) should contain theSameElementsAs List(b1.value, b2.value, b3.value, b4.value, b5.value)
        a6.value shouldEqual b6.value
        a7.value shouldEqual b7.value
      }
      it("should produce PosLong edge values first in random order") {
        import Generator._
        val gen = posLongGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: RoseTree[PosLong], ae1: List[PosLong], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val edges = List(a1, a2).map(_.value)
        edges should contain (PosLong(1L))
        edges should contain (PosLong.MaxValue)
      }

      it("should have legitimate canonicals and shrink") {
        import Generator._
        val gen = posLongGenerator
        val rnd = Randomizer.default
      }

      it("should shrink PosLongs by algo towards 1") {
        import GeneratorDrivenPropertyChecks._
        forAll { (shrinkRoseTree: RoseTree[PosLong]) =>
          val i = shrinkRoseTree.value
          val shrinks: LazyListOrStream[PosLong] = shrinkRoseTree.shrinks.map(_.value)
          shrinks.distinct.length shouldEqual shrinks.length
          if (i.value == 1L)
            shrinks shouldBe empty
          else {
            shrinks should not be empty
            inspectAll(shrinks) { s =>
              s.value should be < i.value  
            }
          }
        }
      }

      it("should produce shrinkees following constraint determined by filter method") {
        val aGen= Generator.posLongGenerator.filter(_.value > 5)
        val (rs, _, _) = aGen.next(SizeParam(1, 0, 1), List(PosLong(30L)), Randomizer.default)
        val shrinkees = rs.shrinks.map(_.value)
        shrinkees should not be empty
        shrinkees.toList shouldBe List(PosLong(15L), PosLong(7L))
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
        List(a1.value, a2.value, a3.value, a4.value, a5.value) should contain theSameElementsAs List(b1.value, b2.value, b3.value, b4.value, b5.value)
        a6.value shouldEqual b6.value
        a7.value shouldEqual b7.value
      }
      it("should produce PosZLong edge values first in random order") {
        import Generator._
        val gen = posZLongGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: RoseTree[PosZLong], ae1: List[PosZLong], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val edges = List(a1, a2, a3).map(_.value)
        edges should contain (PosZLong(0L))
        edges should contain (PosZLong(1L))
        edges should contain (PosZLong.MaxValue)
      }

      it("should have legitimate canonicals and shrink") {
        import Generator._
        val gen = posZLongGenerator
        val rnd = Randomizer.default
        gen.canonicals.shouldGrowWithForGeneratorLazyListOrStreamPair(_.value.value)
      }

      it("should shrink PosZLongs by algo towards 0") {
        import GeneratorDrivenPropertyChecks._
        forAll { (shrinkRoseTree: RoseTree[PosZLong]) =>
          val i = shrinkRoseTree.value
          val shrinks: LazyListOrStream[PosZLong] = shrinkRoseTree.shrinks.map(_.value)
          shrinks.distinct.length shouldEqual shrinks.length
          if (i.value == 0L)
            shrinks shouldBe empty
          else {
            shrinks should not be empty
            inspectAll(shrinks) { s =>
              s.value should be < i.value  
            }
          }
        }
      }

      it("should produce shrinkees following constraint determined by filter method") {
        val aGen= Generator.posZLongGenerator.filter(_.value > 5)
        val (rs, _, _) = aGen.next(SizeParam(1, 0, 1), List(PosZLong(30L)), Randomizer.default)
        val shrinkees = rs.shrinks.map(_.value)
        shrinkees should not be empty
        shrinkees.toList shouldBe List(PosZLong(15L), PosZLong(7L))
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
        List(a1.value, a2.value, a3.value, a4.value, a5.value) should contain theSameElementsAs List(b1.value, b2.value, b3.value, b4.value, b5.value)
        a6.value shouldEqual b6.value
        a7.value shouldEqual b7.value
      }
      it("should produce PosFloat edge values first in random order") {
        import Generator._
        val gen = posFloatGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: RoseTree[PosFloat], ae1: List[PosFloat], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, ae3, ar3) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val (a4, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae3, rnd = ar3)
        val edges = List(a1, a2, a3, a4).map(_.value)
        edges should contain (PosFloat.MinPositiveValue)
        edges should contain (PosFloat(1.0f))
        edges should contain (PosFloat.MaxValue)
        edges should contain (PosFloat.PositiveInfinity)
      }

      it("should have legitimate canonicals and shrink") {
        import Generator._
        val gen = posFloatGenerator
        val rnd = Randomizer.default
        gen.canonicals.shouldGrowWithForGeneratorLazyListOrStreamPair(_.value.value)
        gen.shouldGrowWithForShrink(_.value)
      }

      it("should shrink PosFloat by algo towards 1.0 and positive min value") {
        import GeneratorDrivenPropertyChecks._
        forAll { (shrinkRoseTree: RoseTree[PosFloat]) =>
          val i = shrinkRoseTree.value
          val shrinks: LazyListOrStream[PosFloat] = shrinkRoseTree.shrinks.map(_.value)
          shrinks.distinct.length shouldEqual shrinks.length
          if (i.value == 1.0f || i.value == Float.MinPositiveValue)
            shrinks shouldBe empty
          else {
            shrinks should not be empty
            inspectAll(shrinks) { s =>
              s.value should (be < i.value or equal (1.0))
            }
          }
        }
      }

      it("should produce shrinkees following constraint determined by filter method") {
        val aGen= Generator.posFloatGenerator.filter(_ > 5.0)
        val (rs, _, _) = aGen.next(SizeParam(1, 0, 1), List(PosFloat(40.0f)), Randomizer.default)
        val shrinkees = rs.shrinks.map(_.value)
        shrinkees should not be empty
        shrinkees.toList shouldBe List(PosFloat(6.0f))
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
        List(a1.value, a2.value, a3.value, a4.value, a5.value) should contain theSameElementsAs List(b1.value, b2.value, b3.value, b4.value, b5.value)
        a6.value shouldEqual b6.value
        a7.value shouldEqual b7.value
      }
      it("should produce PosFiniteFloat edge values first in random order") {
        import Generator._
        val gen = posFiniteFloatGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: RoseTree[PosFiniteFloat], ae1: List[PosFiniteFloat], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val edges = List(a1, a2, a3).map(_.value)
        edges should contain (PosFiniteFloat.MinValue)
        edges should contain (PosFiniteFloat(1.0f))
        edges should contain (PosFiniteFloat.MaxValue)
      }

      it("should have legitimate canonicals") {
        import Generator._
        val gen = posFiniteFloatGenerator
        val rnd = Randomizer.default
        gen.canonicals.shouldGrowWithForGeneratorLazyListOrStreamPair(_.value.value)
      }

      it("should shrink PosFiniteFloat by algo towards 1.0 and positive min value") {
        import GeneratorDrivenPropertyChecks._
        forAll { (shrinkRoseTree: RoseTree[PosFiniteFloat]) =>
          val i = shrinkRoseTree.value
          val shrinks: LazyListOrStream[PosFiniteFloat] = shrinkRoseTree.shrinks.map(_.value)
          shrinks.distinct.length shouldEqual shrinks.length
          if (i.value == 1.0f || i.value == Float.MinPositiveValue)
            shrinks shouldBe empty
          else {
            shrinks should not be empty
            inspectAll(shrinks) { s =>
              s.value should (be < i.value or equal (1.0))
            }
          }
        }
      }

      it("should produce shrinkees following constraint determined by filter method") {
        val aGen= Generator.posFiniteFloatGenerator.filter(_ > 5.0)
        val (rs, _, _) = aGen.next(SizeParam(1, 0, 1), List(PosFiniteFloat(40.0f)), Randomizer.default)
        val shrinkees = rs.shrinks.map(_.value)
        shrinkees should not be empty
        shrinkees.toList shouldBe List(PosFiniteFloat(6.0f))
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
        List(a1.value, a2.value, a3.value, a4.value, a5.value) should contain theSameElementsAs List(b1.value, b2.value, b3.value, b4.value, b5.value)
        a6.value shouldEqual b6.value
        a7.value shouldEqual b7.value
      }
      it("should produce PosZFloat edge values first in random order") {
        import Generator._
        val gen = posZFloatGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: RoseTree[PosZFloat], ae1: List[PosZFloat], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, ae3, ar3) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val (a4, ae4, ar4) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae3, rnd = ar3)
        val (a5, ae5, ar5) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae4, rnd = ar4)
        val (a6, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae5, rnd = ar5)
        val edges = List(a1, a2, a3, a4, a5, a6).map(_.value)
        edges should contain (PosZFloat(-0.0f))
        edges should contain (PosZFloat(0.0f))
        edges should contain (PosZFloat.MinPositiveValue)
        edges should contain (PosZFloat(1.0f))
        edges should contain (PosZFloat.MaxValue)
        edges should contain (PosZFloat.PositiveInfinity)
      }

      it("should have legitimate canonicals and shrink") {
        import Generator._
        val gen = posZFloatGenerator
        val rnd = Randomizer.default
        gen.canonicals.shouldGrowWithForGeneratorLazyListOrStreamPair(_.value.value)
        gen.shouldGrowWithForShrink(_.value)
      }

      it("should shrink PosZFloat by algo towards 0") {
        import GeneratorDrivenPropertyChecks._
        forAll { (shrinkRoseTree: RoseTree[PosFloat]) =>
          val i = shrinkRoseTree.value
          val shrinks: LazyListOrStream[PosFloat] = shrinkRoseTree.shrinks.map(_.value)
          shrinks.distinct.length shouldEqual shrinks.length
          if (i.value == 0.0f)
            shrinks shouldBe empty
          else {
            shrinks should not be empty
            inspectAll(shrinks) { s =>
              s.value should (be < i.value or equal (1.0))
            }
          }
        }
      }

      it("should produce shrinkees following constraint determined by filter method") {
        val aGen= Generator.posZFloatGenerator.filter(_ > 5.0)
        val (rs, _, _) = aGen.next(SizeParam(1, 0, 1), List(PosZFloat(40.0f)), Randomizer.default)
        val shrinkees = rs.shrinks.map(_.value)
        shrinkees should not be empty
        shrinkees.toList shouldBe List(PosZFloat(6.0f))
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
        List(a1.value, a2.value, a3.value, a4.value, a5.value) should contain theSameElementsAs List(b1.value, b2.value, b3.value, b4.value, b5.value)
        a6.value shouldEqual b6.value
        a7.value shouldEqual b7.value
      }
      it("should produce PosZFiniteFloat edge values first in random order") {
        import Generator._
        val gen = posZFiniteFloatGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: RoseTree[PosZFiniteFloat], ae1: List[PosZFiniteFloat], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, ae3, ar3) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val (a4, ae4, ar4) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae3, rnd = ar3)
        val (a5, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae4, rnd = ar4)
        val edges = List(a1, a2, a3, a4, a5).map(_.value)
        edges should contain (PosZFiniteFloat(-0.0f))
        edges should contain (PosZFiniteFloat(0.0f))
        edges should contain (PosZFiniteFloat.MinPositiveValue)
        edges should contain (PosZFiniteFloat(1.0f))
        edges should contain (PosZFiniteFloat.MaxValue)
      }

      it("should have legitimate canonicals and shrink") {
        import Generator._
        val gen = posZFiniteFloatGenerator
        val rnd = Randomizer.default
        gen.shouldGrowWithForShrink(_.value)
        gen.canonicals.shouldGrowWithForGeneratorLazyListOrStreamPair(_.value.value)
      }

      it("should shrink PosZFiniteFloat by algo towards 0") {
        import GeneratorDrivenPropertyChecks._
        forAll { (shrinkRoseTree: RoseTree[PosZFiniteFloat]) =>
          val i = shrinkRoseTree.value
          val shrinks: LazyListOrStream[PosZFiniteFloat] = shrinkRoseTree.shrinks.map(_.value)
          shrinks.distinct.length shouldEqual shrinks.length
          if (i.value == 0.0f)
            shrinks shouldBe empty
          else {
            shrinks should not be empty
            inspectAll(shrinks) { s =>
              s.value should (be < i.value or equal (1.0))
            }
          }
        }
      }

      it("should produce shrinkees following constraint determined by filter method") {
        val aGen= Generator.posZFiniteFloatGenerator.filter(_ > 5.0)
        val (rs, _, _) = aGen.next(SizeParam(1, 0, 1), List(PosZFiniteFloat(40.0f)), Randomizer.default)
        val shrinkees = rs.shrinks.map(_.value)
        shrinkees should not be empty
        shrinkees.toList shouldBe List(PosZFiniteFloat(6.0f))
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
        List(a1.value, a2.value, a3.value, a4.value, a5.value) should contain theSameElementsAs List(b1.value, b2.value, b3.value, b4.value, b5.value)
        a6.value shouldEqual b6.value
        a7.value shouldEqual b7.value
      }
      it("should produce PosDouble edge values first in random order") {
        import Generator._
        val gen = posDoubleGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: RoseTree[PosDouble], ae1: List[PosDouble], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, ae3, ar3) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val (a4, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae3, rnd = ar3)
        val edges = List(a1, a2, a3, a4).map(_.value)
        edges should contain (PosDouble.MinPositiveValue)
        edges should contain (PosDouble(1.0))
        edges should contain (PosDouble.MaxValue)
        edges should contain (PosDouble.PositiveInfinity)
      }

      it("should have legitimate canonicals and shrink") {
        import Generator._
        val gen = posDoubleGenerator
        val rnd = Randomizer.default
        gen.shouldGrowWithForShrink(_.value)
        gen.canonicals.shouldGrowWithForGeneratorLazyListOrStreamPair(_.value.value)
      }

      it("should shrink PosDouble by algo towards 0") {
        import GeneratorDrivenPropertyChecks._
        forAll { (shrinkRoseTree: RoseTree[PosDouble]) =>
          val i = shrinkRoseTree.value
          val shrinks: LazyListOrStream[PosDouble] = shrinkRoseTree.shrinks.map(_.value)
          shrinks.distinct.length shouldEqual shrinks.length
          if (i.value == Double.MinPositiveValue)
            shrinks shouldBe empty
          else {
            shrinks should not be empty
            inspectAll(shrinks) { s =>
              s.value should (be < i.value or equal (1.0))
            }
          }
        }
      }

      it("should produce shrinkees following constraint determined by filter method") {
        val aGen= Generator.posDoubleGenerator.filter(_ > 5.0)
        val (rs, _, _) = aGen.next(SizeParam(1, 0, 1), List(PosDouble(40.0)), Randomizer.default)
        val shrinkees = rs.shrinks.map(_.value)
        shrinkees should not be empty
        shrinkees.toList shouldBe List(PosDouble(6.0))
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
        List(a1.value, a2.value, a3.value, a4.value, a5.value) should contain theSameElementsAs List(b1.value, b2.value, b3.value, b4.value, b5.value)
        a6.value shouldEqual b6.value
        a7.value shouldEqual b7.value
      }
      it("should produce PosFiniteDouble edge values first in random order") {
        import Generator._
        val gen = posFiniteDoubleGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: RoseTree[PosFiniteDouble], ae1: List[PosFiniteDouble], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val edges = List(a1, a2, a3).map(_.value)
        edges should contain (PosFiniteDouble.MinValue)
        edges should contain (PosFiniteDouble(1.0))
        edges should contain (PosFiniteDouble.MaxValue)
      }

      it("should have legitimate canonicals and shrink") {
        // TODO: Got: [info]     java.lang.AssertionError: Infinity was not a valid FiniteDouble
        import Generator._
        val gen = posFiniteDoubleGenerator
        val rnd = Randomizer.default
        gen.shouldGrowWithForShrink(_.value)
        gen.canonicals.shouldGrowWithForGeneratorLazyListOrStreamPair(_.value.value)
      }

      it("should shrink PosFiniteDouble by algo towards positive min value") {
        import GeneratorDrivenPropertyChecks._
        forAll { (shrinkRoseTree: RoseTree[PosFiniteDouble]) =>
          val i = shrinkRoseTree.value
          val shrinks: LazyListOrStream[PosFiniteDouble] = shrinkRoseTree.shrinks.map(_.value)
          shrinks.distinct.length shouldEqual shrinks.length
          if (i.value == Float.MinPositiveValue)
            shrinks shouldBe empty
          else {
            shrinks should not be empty
            inspectAll(shrinks) { s =>
              s.value should (be < i.value or equal (1.0))
            }
          }
        }
      }

      it("should produce shrinkees following constraint determined by filter method") {
        val aGen= Generator.posFiniteDoubleGenerator.filter(_ > 5.0)
        val (rs, _, _) = aGen.next(SizeParam(1, 0, 1), List(PosFiniteDouble(40.0)), Randomizer.default)
        val shrinkees = rs.shrinks.map(_.value)
        shrinkees should not be empty
        shrinkees.toList shouldBe List(PosFiniteDouble(6.0))
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
        List(a1.value, a2.value, a3.value, a4.value, a5.value) should contain theSameElementsAs List(b1.value, b2.value, b3.value, b4.value, b5.value)
        a6.value shouldEqual b6.value
        a7.value shouldEqual b7.value
      }
      it("should produce PosZDouble edge values first in random order") {
        import Generator._
        val gen = posZDoubleGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: RoseTree[PosZDouble], ae1: List[PosZDouble], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, ae3, ar3) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val (a4, ae4, ar4) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae3, rnd = ar3)
        val (a5, ae5, ar5) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae4, rnd = ar4)
        val (a6, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae5, rnd = ar5)
        val edges = List(a1, a2, a3, a4, a5, a6).map(_.value)
        edges should contain (PosZDouble(-0.0f))
        edges should contain (PosZDouble(0.0f))
        edges should contain (PosZDouble.MinPositiveValue)
        edges should contain (PosZDouble(1.0f))
        edges should contain (PosZDouble.MaxValue)
        edges should contain (PosZDouble.PositiveInfinity)
      }

      it("should have legitimate canonicals and shrink") {
        import Generator._
        val gen = posZDoubleGenerator
        val rnd = Randomizer.default
        gen.shouldGrowWithForShrink(_.value)
        gen.canonicals.shouldGrowWithForGeneratorLazyListOrStreamPair(_.value.value)
      }

      it("should shrink PosZDouble by algo towards 0") {
        import GeneratorDrivenPropertyChecks._
        forAll { (shrinkRoseTree: RoseTree[PosZDouble]) =>
          val i = shrinkRoseTree.value
          val shrinks: LazyListOrStream[PosZDouble] = shrinkRoseTree.shrinks.map(_.value)
          shrinks.distinct.length shouldEqual shrinks.length
          if (i.value == 0.0)
            shrinks shouldBe empty
          else {
            shrinks should not be empty
            inspectAll(shrinks) { s =>
              s.value should be < i.value  
            }
          }
        }
      }

      it("should produce shrinkees following constraint determined by filter method") {
        val aGen= Generator.posZDoubleGenerator.filter(_ > 5.0)
        val (rs, _, _) = aGen.next(SizeParam(1, 0, 1), List(PosZDouble(40.0)), Randomizer.default)
        val shrinkees = rs.shrinks.map(_.value)
        shrinkees should not be empty
        shrinkees.toList shouldBe List(PosZDouble(6.0))
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
        List(a1.value, a2.value, a3.value, a4.value, a5.value) should contain theSameElementsAs List(b1.value, b2.value, b3.value, b4.value, b5.value)
        a6.value shouldEqual b6.value
        a7.value shouldEqual b7.value
      }
      it("should produce PosZFiniteDouble edge values first in random order") {
        import Generator._
        val gen = posZFiniteDoubleGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: RoseTree[PosZFiniteDouble], ae1: List[PosZFiniteDouble], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, ae3, ar3) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val (a4, ae4, ar4) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae3, rnd = ar3)
        val (a5, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae4, rnd = ar4)
        val edges = List(a1, a2, a3, a4, a5).map(_.value)
        edges should contain (PosZFiniteDouble(-0.0f))
        edges should contain (PosZFiniteDouble(0.0f))
        edges should contain (PosZFiniteDouble.MinPositiveValue)
        edges should contain (PosZFiniteDouble(1.0f))
        edges should contain (PosZFiniteDouble.MaxValue)
      }

      it("should have legitimate canonicals and shrink") {
        import Generator._
        val gen = posZFiniteDoubleGenerator
        val rnd = Randomizer.default
        gen.shouldGrowWithForShrink(_.value)
        gen.canonicals.shouldGrowWithForGeneratorLazyListOrStreamPair(_.value.value)
      }

      it("should shrink PosZFiniteDouble by algo towards 0") {
        import GeneratorDrivenPropertyChecks._
        forAll { (shrinkRoseTree: RoseTree[PosFiniteDouble]) =>
          val i = shrinkRoseTree.value
          val shrinks: LazyListOrStream[PosFiniteDouble] = shrinkRoseTree.shrinks.map(_.value)
          shrinks.distinct.length shouldEqual shrinks.length
          if (i.value == 0.0f)
            shrinks shouldBe empty
          else {
            shrinks should not be empty
            inspectAll(shrinks) { s =>
              s.value should (be < i.value or equal (1.0))
            }
          }
        }
      }

      it("should produce shrinkees following constraint determined by filter method") {
        val aGen= Generator.posZFiniteDoubleGenerator.filter(_ > 5.0)
        val (rs, _, _) = aGen.next(SizeParam(1, 0, 1), List(PosZFiniteDouble(40.0)), Randomizer.default)
        val shrinkees = rs.shrinks.map(_.value)
        shrinkees should not be empty
        shrinkees.toList shouldBe List(PosZFiniteDouble(6.0))
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
        List(a1.value, a2.value, a3.value, a4.value, a5.value) should contain theSameElementsAs List(b1.value, b2.value, b3.value, b4.value, b5.value)
        a6.value shouldEqual b6.value
        a7.value shouldEqual b7.value
      }
      it("should produce NegInt edge values first in random order") {
        import Generator._
        val gen = negIntGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: RoseTree[NegInt], ae1: List[NegInt], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val edges = List(a1, a2).map(_.value)
        edges should contain (NegInt(-1))
        edges should contain (NegInt.MaxValue)
      }

      it("should have legitimate canonicals and shrink") {
        import Generator._
        val gen = negIntGenerator
        val rnd = Randomizer.default
        gen.canonicals.shouldGrowWithForGeneratorLazyListOrStreamPair(_.value.value)
      }

      it("should shrink NegInts by algo towards -1") {
        import GeneratorDrivenPropertyChecks._
        forAll { (shrinkRoseTree: RoseTree[NegInt]) =>
          val i = shrinkRoseTree.value
          val shrinks: LazyListOrStream[NegInt] = shrinkRoseTree.shrinks.map(_.value)
          shrinks.distinct.length shouldEqual shrinks.length
          if (i.value == -1)
            shrinks shouldBe empty
          else {
            shrinks should not be empty
            inspectAll(shrinks) { s =>
              s.value should be > i.value  
            }
          }
        }
      }

      it("should produce shrinkees following constraint determined by filter method") {
        val aGen= Generator.negIntGenerator.filter(_ < -5)
        val (rs, _, _) = aGen.next(SizeParam(1, 0, 1), List(NegInt(-30)), Randomizer.default)
        val shrinkees = rs.shrinks.map(_.value)
        shrinkees should not be empty
        shrinkees.toList shouldBe List(NegInt(-15), NegInt(-7))
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
        List(a1.value, a2.value, a3.value, a4.value, a5.value) should contain theSameElementsAs List(b1.value, b2.value, b3.value, b4.value, b5.value)
        a6.value shouldEqual b6.value
        a7.value shouldEqual b7.value
      }
      it("should produce NegZInt edge values first in random order") {
        import Generator._
        val gen = negZIntGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: RoseTree[NegZInt], ae1: List[NegZInt], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val edges = List(a1, a2, a3).map(_.value)
        edges should contain (NegZInt(0))
        edges should contain (NegZInt(-1))
        edges should contain (NegZInt.MaxValue)
      }

      it("should have legitimate canonicals and shrink") {
        import Generator._
        val gen = negZIntGenerator
        val rnd = Randomizer.default
        gen.canonicals.shouldGrowWithForGeneratorLazyListOrStreamPair(_.value.value)
      }

      it("should shrink NegZInts by algo towards 0") {
        import GeneratorDrivenPropertyChecks._
        forAll { (shrinkRoseTree: RoseTree[NegZInt]) =>
          val i = shrinkRoseTree.value
          val shrinks: LazyListOrStream[NegZInt] = shrinkRoseTree.shrinks.map(_.value)
          shrinks.distinct.length shouldEqual shrinks.length
          if (i.value == 0)
            shrinks shouldBe empty
          else {
            shrinks should not be empty
            inspectAll(shrinks) { s =>
              s.value should be > i.value  
            }
          }
        }
      }

      it("should produce shrinkees following constraint determined by filter method") {
        val aGen= Generator.negZIntGenerator.filter(_ < -5)
        val (rs, _, _) = aGen.next(SizeParam(1, 0, 1), List(NegInt(-30)), Randomizer.default)
        val shrinkees = rs.shrinks.map(_.value)
        shrinkees should not be empty
        shrinkees.toList shouldBe List(NegZInt(-15), NegZInt(-7))
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
        List(a1.value, a2.value, a3.value, a4.value, a5.value) should contain theSameElementsAs List(b1.value, b2.value, b3.value, b4.value, b5.value)
        a6.value shouldEqual b6.value
        a7.value shouldEqual b7.value
      }
      it("should produce NegLong edge values first in random order") {
        import Generator._
        val gen = negLongGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: RoseTree[NegLong], ae1: List[NegLong], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val edges = List(a1, a2).map(_.value)
        edges should contain (NegLong(-1L))
        edges should contain (NegLong.MaxValue)
      }

      it("should have legitimate canonicals and shrink") {
        import Generator._
        val gen = negLongGenerator
        val rnd = Randomizer.default
        gen.canonicals.shouldGrowWithForGeneratorLazyListOrStreamPair(_.value.value)
      }

      it("should shrink NegLongs by algo towards -1") {
        import GeneratorDrivenPropertyChecks._
        forAll { (shrinkRoseTree: RoseTree[NegLong]) =>
          val i = shrinkRoseTree.value
          val shrinks: LazyListOrStream[NegLong] = shrinkRoseTree.shrinks.map(_.value)
          shrinks.distinct.length shouldEqual shrinks.length
          if (i.value == -1L)
            shrinks shouldBe empty
          else {
            shrinks should not be empty
            inspectAll(shrinks) { s =>
              s.value should be > i.value  
            }
          }
        }
      }

      it("should produce shrinkees following constraint determined by filter method") {
        val aGen= Generator.negLongGenerator.filter(_ < -5)
        val (rs, _, _) = aGen.next(SizeParam(1, 0, 1), List(NegLong(-30L)), Randomizer.default)
        val shrinkees = rs.shrinks.map(_.value)
        shrinkees should not be empty
        shrinkees.toList shouldBe List(NegLong(-15L), NegLong(-7L))
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
        List(a1.value, a2.value, a3.value, a4.value, a5.value) should contain theSameElementsAs List(b1.value, b2.value, b3.value, b4.value, b5.value)
        a6.value shouldEqual b6.value
        a7.value shouldEqual b7.value
      }
      it("should produce NegZLong edge values first in random order") {
        import Generator._
        val gen = negZLongGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: RoseTree[NegZLong], ae1: List[NegZLong], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val edges = List(a1, a2, a3).map(_.value)
        edges should contain (NegZLong(0L))
        edges should contain (NegZLong(-1L))
        edges should contain (NegZLong.MaxValue)
      }

      it("should have legitimate canonicals and shrink") {
        import Generator._
        val gen = negZLongGenerator
        val rnd = Randomizer.default
        gen.canonicals.shouldGrowWithForGeneratorLazyListOrStreamPair(_.value.value)
      }

      it("should shrink NegZLongs by algo towards 0") {
        import GeneratorDrivenPropertyChecks._
        forAll { (shrinkRoseTree: RoseTree[NegZLong]) =>
          val i = shrinkRoseTree.value
          val shrinks: LazyListOrStream[NegZLong] = shrinkRoseTree.shrinks.map(_.value)
          shrinks.distinct.length shouldEqual shrinks.length
          if (i.value == 0L)
            shrinks shouldBe empty
          else {
            shrinks should not be empty
            inspectAll(shrinks) { s =>
              s.value should be > i.value  
            }
          }
        }
      }

      it("should produce shrinkees following constraint determined by filter method") {
        val aGen= Generator.negZLongGenerator.filter(_ < -5)
        val (rs, _, _) = aGen.next(SizeParam(1, 0, 1), List(NegZLong(-30L)), Randomizer.default)
        val shrinkees = rs.shrinks.map(_.value)
        shrinkees should not be empty
        shrinkees.toList shouldBe List(NegZLong(-15L), NegZLong(-7L))
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
        List(a1.value, a2.value, a3.value, a4.value, a5.value) should contain theSameElementsAs List(b1.value, b2.value, b3.value, b4.value, b5.value)
        a6.value shouldEqual b6.value
        a7.value shouldEqual b7.value
      }
      it("should produce NegFloat edge values first in random order") {
        import Generator._
        val gen = negFloatGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: RoseTree[NegFloat], ae1: List[NegFloat], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, ae3, ar3) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val (a4, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae3, rnd = ar3)
        val edges = List(a1, a2, a3, a4).map(_.value)
        edges should contain (NegFloat.MaxValue)
        edges should contain (NegFloat(-1.0f))
        edges should contain (NegFloat.MinValue)
        edges should contain (NegFloat.NegativeInfinity)
      }

      it("should have legitimate canonicals and shrink") {
        import Generator._
        val gen = negFloatGenerator
        val rnd = Randomizer.default
        gen.shouldGrowWithForShrink(_.value)
        gen.canonicals.shouldGrowWithForGeneratorLazyListOrStreamPair(_.value.value)
      }

      it("should shrink NegFloat by algo towards 0") {
        import GeneratorDrivenPropertyChecks._
        forAll { (shrinkRoseTree: RoseTree[NegFloat]) =>
          val i = shrinkRoseTree.value
          val shrinks: LazyListOrStream[NegFloat] = shrinkRoseTree.shrinks.map(_.value)
          shrinks.distinct.length shouldEqual shrinks.length
          if (i.value == -Float.MinPositiveValue)
            shrinks shouldBe empty
          else {
            shrinks should not be empty
            inspectAll(shrinks) { s =>
              s.value should (be > i.value or equal(-1.0))
            }
          }
        }
      }

      it("should produce shrinkees following constraint determined by filter method") {
        val aGen= Generator.negFloatGenerator.filter(_ < -5.0f)
        val (rs, _, _) = aGen.next(SizeParam(1, 0, 1), List(NegFloat(-40.0f)), Randomizer.default)
        val shrinkees = rs.shrinks.map(_.value)
        shrinkees should not be empty
        shrinkees.toList shouldBe List(NegFloat(-6.0f))
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
        List(a1.value, a2.value, a3.value, a4.value, a5.value) should contain theSameElementsAs List(b1.value, b2.value, b3.value, b4.value, b5.value)
        a6.value shouldEqual b6.value
        a7.value shouldEqual b7.value
      }
      it("should produce NegFiniteFloat edge values first in random order") {
        import Generator._
        val gen = negFiniteFloatGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: RoseTree[NegFiniteFloat], ae1: List[NegFiniteFloat], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val edges = List(a1, a2, a3).map(_.value)
        edges should contain (NegFiniteFloat(-1.0f))
        edges should contain (NegFiniteFloat.MaxValue)
        edges should contain (NegFiniteFloat.MinValue)
      }

      it("should have legitimate canonicals and shrink") {
        import Generator._
        val gen = negFiniteFloatGenerator
        val rnd = Randomizer.default
        gen.shouldGrowWithForShrink(_.value)
        gen.canonicals.shouldGrowWithForGeneratorLazyListOrStreamPair(_.value.value)
      }

      it("should shrink NegFiniteFloat by algo towards 0") {
        import GeneratorDrivenPropertyChecks._
        forAll { (shrinkRoseTree: RoseTree[NegFiniteFloat]) =>
          val i = shrinkRoseTree.value
          val shrinks: LazyListOrStream[NegFiniteFloat] = shrinkRoseTree.shrinks.map(_.value)
          shrinks.distinct.length shouldEqual shrinks.length
          if (i.value == -Float.MinPositiveValue)
            shrinks shouldBe empty
          else {
            shrinks should not be empty
            // shrink does not mean get smaller, it means get simpler. If a number is between -1.0 and 0.0
            // then we hop to -1.0. Otherwise we go towards zero with whole numbers.
            inspectAll(shrinks) { s =>
              s.value should (be > i.value or equal (-1.0))
            }
          }
        }
      }

      it("should produce shrinkees following constraint determined by filter method") {
        val aGen= Generator.negFiniteFloatGenerator.filter(_ < -5.0f)
        val (rs, _, _) = aGen.next(SizeParam(1, 0, 1), List(NegFiniteFloat(-40.0f)), Randomizer.default)
        val shrinkees = rs.shrinks.map(_.value)
        shrinkees should not be empty
        shrinkees.toList shouldBe List(NegFiniteFloat(-6.0f))
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
        List(a1.value, a2.value, a3.value, a4.value, a5.value) should contain theSameElementsAs List(b1.value, b2.value, b3.value, b4.value, b5.value)
        a6.value shouldEqual b6.value
        a7.value shouldEqual b7.value
      }
      it("should produce NegZFloat edge values first in random order") {
        import Generator._
        val gen = negZFloatGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: RoseTree[NegZFloat], ae1: List[NegZFloat], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, ae3, ar3) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val (a4, ae4, ar4) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae3, rnd = ar3)
        val (a5, ae5, ar5) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae4, rnd = ar4)
        val (a6, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae5, rnd = ar5)
        val edges = List(a1, a2, a3, a4, a5, a6).map(_.value)
        edges should contain (NegZFloat(0.0f))
        edges should contain (NegZFloat(-0.0f))
        edges should contain (NegZFloat.ensuringValid(-Float.MinPositiveValue))
        edges should contain (NegZFloat(-1.0f))
        edges should contain (NegZFloat.MinValue)
        edges should contain (NegZFloat.NegativeInfinity)
      }

      it("should have legitimate canonicals and shrink") {
        import Generator._
        val gen = negZFloatGenerator
        val rnd = Randomizer.default
        gen.shouldGrowWithForShrink(_.value)
        gen.canonicals.shouldGrowWithForGeneratorLazyListOrStreamPair(_.value.value)
      }

      it("should shrink NegZFloat by algo towards 0") {
        import GeneratorDrivenPropertyChecks._
        forAll { (shrinkRoseTree: RoseTree[NegZFloat]) =>
          val i = shrinkRoseTree.value
          val shrinks: LazyListOrStream[NegZFloat] = shrinkRoseTree.shrinks.map(_.value)
          shrinks.distinct.length shouldEqual shrinks.length
          if (i.value == 0.0f)
            shrinks shouldBe empty
          else {
            shrinks should not be empty
            inspectAll(shrinks) { s =>
              s.value should be > i.value  
            }
          }
        }
      }

      it("should produce shrinkees following constraint determined by filter method") {
        val aGen= Generator.negZFloatGenerator.filter(_ < -5.0f)
        val (rs, _, _) = aGen.next(SizeParam(1, 0, 1), List(NegZFloat(-40.0f)), Randomizer.default)
        val shrinkees = rs.shrinks.map(_.value)
        shrinkees should not be empty
        shrinkees.toList shouldBe List(NegZFloat(-6.0f))
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
        List(a1.value, a2.value, a3.value, a4.value, a5.value) should contain theSameElementsAs List(b1.value, b2.value, b3.value, b4.value, b5.value)
        a6.value shouldEqual b6.value
        a7.value shouldEqual b7.value
      }
      it("should produce NegZFiniteFloat edge values first in random order") {
        import Generator._
        val gen = negZFiniteFloatGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: RoseTree[NegZFiniteFloat], ae1: List[NegZFiniteFloat], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, ae3, ar3) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val (a4, ae4, ar4) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae3, rnd = ar3)
        val (a5, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae4, rnd = ar4)
        val edges = List(a1, a2, a3, a4, a5).map(_.value)
        edges should contain (NegZFiniteFloat(0.0f))
        edges should contain (NegZFiniteFloat(-0.0f))
        edges should contain (NegZFiniteFloat.ensuringValid(-Float.MinPositiveValue))
        edges should contain (NegZFiniteFloat(-1.0f))
        edges should contain (NegZFiniteFloat.MinValue)
      }

      it("should have legitimate canonicals and shrink") {
        import Generator._
        val gen = negZFiniteFloatGenerator
        val rnd = Randomizer.default
        gen.shouldGrowWithForShrink(_.value)
        gen.canonicals.shouldGrowWithForGeneratorLazyListOrStreamPair(_.value.value)
      }

      it("should shrink NegZFiniteFloat by algo towards 0") {
        import GeneratorDrivenPropertyChecks._
        forAll { (shrinkRoseTree: RoseTree[NegZFiniteFloat]) =>
          val i = shrinkRoseTree.value
          val shrinks: LazyListOrStream[NegZFiniteFloat] = shrinkRoseTree.shrinks.map(_.value)
          shrinks.distinct.length shouldEqual shrinks.length
          if (i.value == 0.0f)
            shrinks shouldBe empty
          else {
            shrinks should not be empty
            inspectAll(shrinks) { s =>
              s.value should (be > i.value or equal (-1.0))
            }
          }
        }
      }

      it("should produce shrinkees following constraint determined by filter method") {
        val aGen= Generator.negZFiniteFloatGenerator.filter(_ < -5.0f)
        val (rs, _, _) = aGen.next(SizeParam(1, 0, 1), List(NegZFiniteFloat(-40.0f)), Randomizer.default)
        val shrinkees = rs.shrinks.map(_.value)
        shrinkees should not be empty
        shrinkees.toList shouldBe List(NegZFiniteFloat(-6.0f))
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
        List(a1.value, a2.value, a3.value, a4.value, a5.value) should contain theSameElementsAs List(b1.value, b2.value, b3.value, b4.value, b5.value)
        a6.value shouldEqual b6.value
        a7.value shouldEqual b7.value
      }
      it("should produce NegDouble edge values first in random order") {
        import Generator._
        val gen = negDoubleGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: RoseTree[NegDouble], ae1: List[NegDouble], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, ae3, ar3) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val (a4, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae3, rnd = ar3)
        val edges = List(a1, a2, a3, a4).map(_.value)
        edges should contain (NegDouble.MaxValue)
        edges should contain (NegDouble(-1.0f))
        edges should contain (NegDouble.MinValue)
        edges should contain (NegDouble.NegativeInfinity)
      }

      it("should have legitimate canonicals and shrink") {
        import Generator._
        val gen = negDoubleGenerator
        val rnd = Randomizer.default
        gen.shouldGrowWithForShrink(_.value)
        gen.canonicals.shouldGrowWithForGeneratorLazyListOrStreamPair(_.value.value)
      }

      it("should shrink NegDouble by algo towards 0") {
        import GeneratorDrivenPropertyChecks._
        forAll { (shrinkRoseTree: RoseTree[NegDouble]) =>
          val i = shrinkRoseTree.value
          val shrinks: LazyListOrStream[NegDouble] = shrinkRoseTree.shrinks.map(_.value)
          shrinks.distinct.length shouldEqual shrinks.length
          if (i.value == -Double.MinPositiveValue)
            shrinks shouldBe empty
          else {
            shrinks should not be empty
            inspectAll(shrinks) { s =>
              s.value should (be > i.value or equal(-1.0))
            }
          }
        }
      }

      it("should produce shrinkees following constraint determined by filter method") {
        val aGen= Generator.negDoubleGenerator.filter(_ < -5.0)
        val (rs, _, _) = aGen.next(SizeParam(1, 0, 1), List(NegDouble(-40.0)), Randomizer.default)
        val shrinkees = rs.shrinks.map(_.value)
        shrinkees should not be empty
        shrinkees.toList shouldBe List(NegDouble(-6.0))
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
        List(a1.value, a2.value, a3.value, a4.value, a5.value) should contain theSameElementsAs List(b1.value, b2.value, b3.value, b4.value, b5.value)
        a6.value shouldEqual b6.value
        a7.value shouldEqual b7.value
      }
      it("should produce NegFiniteDouble edge values first in random order") {
        import Generator._
        val gen = negFiniteDoubleGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: RoseTree[NegFiniteDouble], ae1: List[NegFiniteDouble], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val edges = List(a1, a2, a3).map(_.value)
        edges should contain (NegFiniteDouble(-1.0))
        edges should contain (NegFiniteDouble.MinValue)
        edges should contain (NegFiniteDouble.MaxValue)
      }

      it("should have legitimate canonicals and shrink") {
        import Generator._
        val gen = negFiniteDoubleGenerator
        val rnd = Randomizer.default
        gen.shouldGrowWithForShrink(_.value)
        gen.canonicals.shouldGrowWithForGeneratorLazyListOrStreamPair(_.value.value)
      }

      it("should shrink NegFiniteDouble by algo towards 0") {
        import GeneratorDrivenPropertyChecks._
        forAll { (shrinkRoseTree: RoseTree[NegFiniteDouble]) =>
          val i = shrinkRoseTree.value
          val shrinks: LazyListOrStream[NegFiniteDouble] = shrinkRoseTree.shrinks.map(_.value)
          shrinks.distinct.length shouldEqual shrinks.length
          if (i.value == -Float.MinPositiveValue)
            shrinks shouldBe empty
          else {
            shrinks should not be empty
            inspectAll(shrinks) { s =>
              s.value should (be > i.value or equal(-1.0))
            }
          }
        }
      }

      it("should produce shrinkees following constraint determined by filter method") {
        val aGen= Generator.negFiniteDoubleGenerator.filter(_ < -5.0)
        val (rs, _, _) = aGen.next(SizeParam(1, 0, 1), List(NegFiniteDouble(-40.0)), Randomizer.default)
        val shrinkees = rs.shrinks.map(_.value)
        shrinkees should not be empty
        shrinkees.toList shouldBe List(NegFiniteDouble(-6.0))
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
        List(a1.value, a2.value, a3.value, a4.value, a5.value) should contain theSameElementsAs List(b1.value, b2.value, b3.value, b4.value, b5.value)
        a6.value shouldEqual b6.value
        a7.value shouldEqual b7.value
      }
      it("should produce NegZDouble edge values first in random order") {
        import Generator._
        val gen = negZDoubleGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: RoseTree[NegZDouble], ae1: List[NegZDouble], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, ae3, ar3) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val (a4, ae4, ar4) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae3, rnd = ar3)
        val (a5, ae5, ar5) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae4, rnd = ar4)
        val (a6, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae5, rnd = ar5)
        val edges = List(a1, a2, a3, a4, a5, a6).map(_.value)
        edges should contain (NegZDouble(0.0f))
        edges should contain (NegZDouble(-0.0f))
        edges should contain (NegZDouble.ensuringValid(-Double.MinPositiveValue))
        edges should contain (NegZDouble(-1.0f))
        edges should contain (NegZDouble.MinValue)
        edges should contain (NegZDouble.NegativeInfinity)
      }

      it("should have legitimate canonicals and shrink") {
        import Generator._
        val gen = negZDoubleGenerator
        val rnd = Randomizer.default
        gen.shouldGrowWithForShrink(_.value)
        gen.canonicals.shouldGrowWithForGeneratorLazyListOrStreamPair(_.value.value)
      }

      it("should shrink NegZDouble by algo towards 0") {
        import GeneratorDrivenPropertyChecks._
        forAll { (shrinkRoseTree: RoseTree[NegZDouble]) =>
          val i = shrinkRoseTree.value
          val shrinks: LazyListOrStream[NegZDouble] = shrinkRoseTree.shrinks.map(_.value)
          shrinks.distinct.length shouldEqual shrinks.length
          if (i.value == 0.0)
            shrinks shouldBe empty
          else {
            shrinks should not be empty
            inspectAll(shrinks) { s =>
              s.value should be > i.value  
            }
          }
        }
      }

      it("should produce shrinkees following constraint determined by filter method") {
        val aGen= Generator.negZDoubleGenerator.filter(_ < -5.0)
        val (rs, _, _) = aGen.next(SizeParam(1, 0, 1), List(NegZDouble(-40.0)), Randomizer.default)
        val shrinkees = rs.shrinks.map(_.value)
        shrinkees should not be empty
        shrinkees.toList shouldBe List(NegZDouble(-6.0))
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
        List(a1.value, a2.value, a3.value, a4.value, a5.value) should contain theSameElementsAs List(b1.value, b2.value, b3.value, b4.value, b5.value)
        a6.value shouldEqual b6.value
        a7.value shouldEqual b7.value
      }
      it("should produce NegZFiniteDouble edge values first in random order") {
        import Generator._
        val gen = negZFiniteDoubleGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: RoseTree[NegZFiniteDouble], ae1: List[NegZFiniteDouble], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, ae3, ar3) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val (a4, ae4, ar4) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae3, rnd = ar3)
        val (a5, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae4, rnd = ar4)
        val edges = List(a1, a2, a3, a4, a5).map(_.value)
        edges should contain (NegZFiniteDouble(0.0))
        edges should contain (NegZFiniteDouble(-0.0))
        edges should contain (NegZFiniteDouble.ensuringValid(-Double.MinPositiveValue))
        edges should contain (NegZFiniteDouble(-1.0))
        edges should contain (NegZFiniteDouble.MinValue)
      }

      it("should have legitimate canonicals and shrink") {
        import Generator._
        val gen = negZFiniteDoubleGenerator
        val rnd = Randomizer.default
        gen.shouldGrowWithForShrink(_.value)
        gen.canonicals.shouldGrowWithForGeneratorLazyListOrStreamPair(_.value.value)
      }

      it("should shrink NegZFiniteDouble by algo towards 0") {
        import GeneratorDrivenPropertyChecks._
        forAll { (shrinkRoseTree: RoseTree[NegZFiniteDouble]) =>
          val i = shrinkRoseTree.value
          val shrinks: LazyListOrStream[NegZFiniteDouble] = shrinkRoseTree.shrinks.map(_.value)
          shrinks.distinct.length shouldEqual shrinks.length
          if (i.value == 0.0)
            shrinks shouldBe empty
          else {
            shrinks should not be empty
            inspectAll(shrinks) { s =>
              s.value should be > i.value  
            }
          }
        }
      }

      it("should produce shrinkees following constraint determined by filter method") {
        val aGen= Generator.negZFiniteDoubleGenerator.filter(_ < -5.0)
        val (rs, _, _) = aGen.next(SizeParam(1, 0, 1), List(NegZFiniteDouble(-40.0)), Randomizer.default)
        val shrinkees = rs.shrinks.map(_.value)
        shrinkees should not be empty
        shrinkees.toList shouldBe List(NegZFiniteDouble(-6.0))
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
        List(a1.value, a2.value, a3.value, a4.value, a5.value) should contain theSameElementsAs List(b1.value, b2.value, b3.value, b4.value, b5.value)
        a6.value shouldEqual b6.value
        a7.value shouldEqual b7.value
      }
      it("should produce NonZeroInt edge values first in random order") {
        import Generator._
        val gen = nonZeroIntGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: RoseTree[NonZeroInt], ae1: List[NonZeroInt], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, ae3, ar3) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val (a4, ae4, ar4) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae3, rnd = ar3)
        val edges = List(a1, a2, a3, a4).map(_.value)
        edges should contain (NonZeroInt(-1))
        edges should contain (NonZeroInt.MaxValue)
        edges should contain (NonZeroInt(1))
        edges should contain (NonZeroInt.MinValue)
      }
      it("should produce NonZeroInt canonical values") {
        import Generator._
        val gen = nonZeroIntGenerator
        val canonicals = gen.canonicals
        canonicals.map(_.value).toList shouldBe List(NonZeroInt(-3), NonZeroInt(3), NonZeroInt(-2), NonZeroInt(2), NonZeroInt(-1), NonZeroInt(1))
      }
      it("should shrink NonZeroInts by repeatedly halving and negating") {
        import GeneratorDrivenPropertyChecks._
        forAll { (shrinkRoseTree: RoseTree[NonZeroInt]) =>
          val i = shrinkRoseTree.value
          val shrinks: LazyListOrStream[NonZeroInt] = shrinkRoseTree.shrinks.map(_.value)
          shrinks.distinct.length shouldEqual shrinks.length
          if (i.value == 1 || i.value == -1)
            shrinks shouldBe empty
          else {
            if (i > 1)
              shrinks.head.value should be <= 1
            else if (i < -1)
              shrinks.head.value should be >= 1
            import org.scalatest.Inspectors._
            val revShrinks = shrinks.reverse
            val pairs: LazyListOrStream[(NonZeroInt, NonZeroInt)] = revShrinks.zip(revShrinks.tail)
            forAll (pairs) { case (x, y) =>
              assert(x == -y || x.value.abs == y.value.abs / 2)
            }
          }
        }
      }

      it("should produce shrinkees following constraint determined by filter method") {
        val aGen= Generator.nonZeroIntGenerator.filter(_ > 5)
        val (rs, _, _) = aGen.next(SizeParam(1, 0, 1), List(NonZeroInt(30)), Randomizer.default)
        val shrinkees = rs.shrinks.map(_.value)
        shrinkees should not be empty
        shrinkees.toList shouldBe List(NonZeroInt(15), NonZeroInt(7))
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
        List(a1.value, a2.value, a3.value, a4.value, a5.value) should contain theSameElementsAs List(b1.value, b2.value, b3.value, b4.value, b5.value)
        a6.value shouldEqual b6.value
        a7.value shouldEqual b7.value
      }
      it("should produce NonZeroLong edge values first in random order") {
        import Generator._
        val gen = nonZeroLongGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: RoseTree[NonZeroLong], ae1: List[NonZeroLong], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, ae3, ar3) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val (a4, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae3, rnd = ar3)
        val edges = List(a1, a2, a3, a4).map(_.value)
        edges should contain (NonZeroLong(-1L))
        edges should contain (NonZeroLong.MaxValue)
        edges should contain (NonZeroLong(1L))
        edges should contain (NonZeroLong.MinValue)
      }

      it("should have legitimate canonicals and shrink") {
        import Generator._
        val gen = nonZeroLongGenerator
        val rnd = Randomizer.default
        gen.canonicals.shouldGrowWithForGeneratorLazyListOrStreamPair(_.value.value)
      }

      it("should shrink NonZeroLongs by algo towards min positive and negative values") {
        import GeneratorDrivenPropertyChecks._
        forAll { (shrinkRoseTree: RoseTree[NonZeroLong]) =>
          val i = shrinkRoseTree.value
          val shrinks: LazyListOrStream[NonZeroLong] = shrinkRoseTree.shrinks.map(_.value)
          shrinks.distinct.length shouldEqual shrinks.length
          if (i.value == 1L || i.value == -1L)
            shrinks shouldBe empty
          else {
            shrinks should not be empty
            inspectAll(shrinks) { s =>
              if (i.value > 0L)
                s.value should be < i.value
              else
                s.value should be > i.value
            }
          }
        }
      }

      it("should produce shrinkees following constraint determined by filter method") {
        val aGen= Generator.nonZeroLongGenerator.filter(_ > 5L)
        val (rs, _, _) = aGen.next(SizeParam(1, 0, 1), List(NonZeroLong(30L)), Randomizer.default)
        val shrinkees = rs.shrinks.map(_.value)
        shrinkees should not be empty
        shrinkees.toList shouldBe List(NonZeroLong(15L), NonZeroLong(7L))
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
        List(a1.value, a2.value, a3.value, a4.value, a5.value) should contain theSameElementsAs List(b1.value, b2.value, b3.value, b4.value, b5.value)
        a6.value shouldEqual b6.value
        a7.value shouldEqual b7.value
      }
      it("should produce NonZeroFloat edge values first in random order") {
        import Generator._
        val gen = nonZeroFloatGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: RoseTree[NonZeroFloat], ae1: List[NonZeroFloat], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, ae3, ar3) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val (a4, ae4, ar4) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae3, rnd = ar3)
        val (a5, ae5, ar5) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae4, rnd = ar4)
        val (a6, ae6, ar6) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae5, rnd = ar5)
        val (a7, ae7, ar7) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae6, rnd = ar6)
        val (a8, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae7, rnd = ar7)
        val edges = List(a1, a2, a3, a4, a5, a6, a7, a8).map(_.value)
        edges should contain (NonZeroFloat.NegativeInfinity)
        edges should contain (NonZeroFloat.MinValue)
        edges should contain (NonZeroFloat(-1.0f))
        edges should contain (-NonZeroFloat.MinPositiveValue)
        edges should contain (NonZeroFloat.MinPositiveValue)
        edges should contain (NonZeroFloat(1.0f))
        edges should contain (NonZeroFloat.MaxValue)
        edges should contain (NonZeroFloat.PositiveInfinity)
      }

      it("should have legitimate canonicals and shrink") {
        import Generator._
        val gen = nonZeroFloatGenerator
        val rnd = Randomizer.default
        gen.shouldGrowWithForShrink(_.value)
        gen.canonicals.shouldGrowWithForGeneratorLazyListOrStreamPair(_.value.value)
      }

      it("should shrink NonZeroFloats with an algo towards min positive or negative value") {
        import GeneratorDrivenPropertyChecks._
        forAll { (shrinkRoseTree: RoseTree[NonZeroFloat]) =>
          val i = shrinkRoseTree.value
          val shrinks: LazyListOrStream[NonZeroFloat] = shrinkRoseTree.shrinks.map(_.value)
          shrinks.distinct.length shouldEqual shrinks.length
          if (i.value == Float.MinPositiveValue || i.value == -Float.MinPositiveValue)
            shrinks shouldBe empty
          else {
            shrinks should not be empty
            inspectAll(shrinks) { s =>
              if (i.value >= 0.0f)
                s.value should (be < i.value or equal(1.0))
              else
                s.value should (be > i.value or equal(-1.0))
            }  
          }
        }
      }

      it("should produce shrinkees following constraint determined by filter method") {
        val aGen= Generator.nonZeroFloatGenerator.filter(_ > 5.0f)
        val (rs, _, _) = aGen.next(SizeParam(1, 0, 1), List(NonZeroFloat(40.0f)), Randomizer.default)
        val shrinkees = rs.shrinks.map(_.value)
        shrinkees should not be empty
        shrinkees.toList shouldBe List(NonZeroFloat(6.0f))
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
        List(a1.value, a2.value, a3.value, a4.value, a5.value) should contain theSameElementsAs List(b1.value, b2.value, b3.value, b4.value, b5.value)
        a6.value shouldEqual b6.value
        a7.value shouldEqual b7.value
      }
      it("should produce NonZeroFiniteFloat edge values first in random order") {
        import Generator._
        val gen = nonZeroFiniteFloatGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: RoseTree[NonZeroFiniteFloat], ae1: List[NonZeroFiniteFloat], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, ae3, ar3) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val (a4, ae4, ar4) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae3, rnd = ar3)
        val (a5, ae5, ar5) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae4, rnd = ar4)
        val (a6, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae5, rnd = ar5)
        val edges = List(a1, a2, a3, a4, a5, a6).map(_.value)
        edges should contain (NonZeroFiniteFloat.MinValue)
        edges should contain (NonZeroFiniteFloat(-1.0f))
        edges should contain (NonZeroFiniteFloat.ensuringValid(-NonZeroFiniteFloat.MinPositiveValue))
        edges should contain (NonZeroFiniteFloat.MinPositiveValue)
        edges should contain (NonZeroFiniteFloat(1.0f))
        edges should contain (NonZeroFiniteFloat.MaxValue)
      }

      it("should have legitimate canonicals and shrink") {
        import Generator._
        val gen = nonZeroFiniteFloatGenerator
        val rnd = Randomizer.default
        gen.shouldGrowWithForShrink(_.value)
        gen.canonicals.shouldGrowWithForGeneratorLazyListOrStreamPair(_.value.value)
      }

      it("should shrink NonZeroFiniteFloats with an algo towards min positive or negative value") {
        import GeneratorDrivenPropertyChecks._
        forAll { (shrinkRoseTree: RoseTree[NonZeroFiniteFloat]) =>
          val i = shrinkRoseTree.value
          val shrinks: LazyListOrStream[NonZeroFiniteFloat] = shrinkRoseTree.shrinks.map(_.value)
          shrinks.distinct.length shouldEqual shrinks.length
          if (i.value == Float.MinPositiveValue || i.value == -Float.MinPositiveValue)
            shrinks shouldBe empty
          else {
            shrinks should not be empty
            inspectAll(shrinks) { s =>
              if (i.value >= 0.0f)
                s.value should (be < i.value or equal(1.0f))
              else
                s.value should (be > i.value or equal(-1.0))
            }  
          }
        }
      }

      it("should produce shrinkees following constraint determined by filter method") {
        val aGen= Generator.nonZeroFiniteFloatGenerator.filter(_ > 5.0f)
        val (rs, _, _) = aGen.next(SizeParam(1, 0, 1), List(NonZeroFiniteFloat(40.0f)), Randomizer.default)
        val shrinkees = rs.shrinks.map(_.value)
        shrinkees should not be empty
        shrinkees.toList shouldBe List(NonZeroFiniteFloat(6.0f))
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
        List(a1.value, a2.value, a3.value, a4.value, a5.value) should contain theSameElementsAs List(b1.value, b2.value, b3.value, b4.value, b5.value)
        a6.value shouldEqual b6.value
        a7.value shouldEqual b7.value
      }
      it("should produce NonZeroDouble edge values first in random order") {
        import Generator._
        val gen = nonZeroDoubleGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: RoseTree[NonZeroDouble], ae1: List[NonZeroDouble], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, ae3, ar3) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val (a4, ae4, ar4) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae3, rnd = ar3)
        val (a5, ae5, ar5) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae4, rnd = ar4)
        val (a6, ae6, ar6) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae5, rnd = ar5)
        val (a7, ae7, ar7) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae6, rnd = ar6)
        val (a8, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae7, rnd = ar7)
        val edges = List(a1, a2, a3, a4, a5, a6, a7, a8).map(_.value)
        edges should contain (NonZeroDouble.NegativeInfinity)
        edges should contain (NonZeroDouble.MinValue)
        edges should contain (NonZeroDouble(-1.0f))
        edges should contain (-NonZeroDouble.MinPositiveValue)
        edges should contain (NonZeroDouble.MinPositiveValue)
        edges should contain (NonZeroDouble(1.0f))
        edges should contain (NonZeroDouble.MaxValue)
        edges should contain (NonZeroDouble.PositiveInfinity)
      }

      it("should have legitimate canonicals and shrink") {
        import Generator._
        val gen = nonZeroDoubleGenerator
        val rnd = Randomizer.default
        gen.shouldGrowWithForShrink(_.value)
        gen.canonicals.shouldGrowWithForGeneratorLazyListOrStreamPair(_.value.value)
      }

      it("should shrink NonZeroDoubles with an algo towards min positive or negative value") {
        import GeneratorDrivenPropertyChecks._
        forAll { (shrinkRoseTree: RoseTree[NonZeroDouble]) =>
          val i = shrinkRoseTree.value
          val shrinks: LazyListOrStream[NonZeroDouble] = shrinkRoseTree.shrinks.map(_.value)
          shrinks.distinct.length shouldEqual shrinks.length
          if (i.value == Double.MinPositiveValue || i.value == -Double.MinPositiveValue)
            shrinks shouldBe empty
          else {
            shrinks should not be empty
            inspectAll(shrinks) { s =>
              if (i.value >= 0.0)
                s.value should (be < i.value or equal(1.0))
              else
                s.value should (be > i.value or equal(-1.0))
            }  
          }
        }
      }

      it("should produce shrinkees following constraint determined by filter method") {
        val aGen= Generator.nonZeroDoubleGenerator.filter(_ > 5.0)
        val (rs, _, _) = aGen.next(SizeParam(1, 0, 1), List(NonZeroDouble(40.0)), Randomizer.default)
        val shrinkees = rs.shrinks.map(_.value)
        shrinkees should not be empty
        shrinkees.toList shouldBe List(NonZeroDouble(6.0))
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
        List(a1.value, a2.value, a3.value, a4.value, a5.value) should contain theSameElementsAs List(b1.value, b2.value, b3.value, b4.value, b5.value)
        a6.value shouldEqual b6.value
        a7.value shouldEqual b7.value
      }
      it("should produce NonZeroFiniteDouble edge values first in random order") {
        import Generator._
        val gen = nonZeroFiniteDoubleGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: RoseTree[NonZeroFiniteDouble], ae1: List[NonZeroFiniteDouble], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, ae3, ar3) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val (a4, ae4, ar4) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae3, rnd = ar3)
        val (a5, ae5, ar5) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae4, rnd = ar4)
        val (a6, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae5, rnd = ar5)
        val edges = List(a1, a2, a3, a4, a5, a6).map(_.value)
        edges should contain (NonZeroFiniteDouble.MinValue)
        edges should contain (NonZeroFiniteDouble(-1.0))
        edges should contain (NonZeroFiniteDouble.ensuringValid(-NonZeroFiniteDouble.MinPositiveValue))
        edges should contain (NonZeroFiniteDouble.MinPositiveValue)
        edges should contain (NonZeroFiniteDouble(1.0))
        edges should contain (NonZeroFiniteDouble.MaxValue)
      }

      it("should have legitimate canonicals and shrink") {
        import Generator._
        val gen = nonZeroFiniteDoubleGenerator
        val rnd = Randomizer.default
        gen.shouldGrowWithForShrink(_.value)
        gen.canonicals.shouldGrowWithForGeneratorLazyListOrStreamPair(_.value.value)
      }

      it("should shrink NonZeroFiniteDoubles with an algo towards min positive or negative value") {
        import GeneratorDrivenPropertyChecks._
        forAll { (shrinkRoseTree: RoseTree[NonZeroFiniteDouble]) =>
          val i = shrinkRoseTree.value
          val shrinks: LazyListOrStream[NonZeroFiniteDouble] = shrinkRoseTree.shrinks.map(_.value)
          shrinks.distinct.length shouldEqual shrinks.length
          if (i.value == Double.MinPositiveValue || i.value == -Double.MinPositiveValue)
            shrinks shouldBe empty
          else {
            shrinks should not be empty
            inspectAll(shrinks) { s =>
              if (i.value >= 0.0)
                s.value should (be < i.value or equal(1.0))
              else
                s.value should (be > i.value or equal(-1.0))
            }  
          }
        }
      }

      it("should produce shrinkees following constraint determined by filter method") {
        val aGen= Generator.nonZeroFiniteDoubleGenerator.filter(_ > 5.0)
        val (rs, _, _) = aGen.next(SizeParam(1, 0, 1), List(NonZeroFiniteDouble(40.0)), Randomizer.default)
        val shrinkees = rs.shrinks.map(_.value)
        shrinkees should not be empty
        shrinkees.toList shouldBe List(NonZeroFiniteDouble(6.0))
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
        List(a1.value, a2.value, a3.value, a4.value, a5.value) should contain theSameElementsAs List(b1.value, b2.value, b3.value, b4.value, b5.value)
        a6.value shouldEqual b6.value
        a7.value shouldEqual b7.value
      }
      it("should produce FiniteFloat edge values first in random order") {
        import Generator._
        val gen = finiteFloatGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: RoseTree[FiniteFloat], ae1: List[FiniteFloat], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, ae3, ar3) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val (a4, ae4, ar4) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae3, rnd = ar3)
        val (a5, ae5, ar5) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae4, rnd = ar4)
        val (a6, ae6, ar6) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae5, rnd = ar5)
        val (a7, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae6, rnd = ar6)
        val edges = List(a1, a2, a3, a4, a5, a6, a7).map(_.value)
        edges should contain (FiniteFloat.MinValue)
        edges should contain (FiniteFloat(-1.0f))
        edges should contain (FiniteFloat.ensuringValid(-FiniteFloat.MinPositiveValue))
        edges should contain (FiniteFloat(0.0f))
        edges should contain (FiniteFloat.MinPositiveValue)
        edges should contain (FiniteFloat(1.0f))
        edges should contain (FiniteFloat.MaxValue)
      }

      it("should have legitimate canonicals and shrink") {
        import Generator._
        val gen = finiteFloatGenerator
        val rnd = Randomizer.default
        gen.canonicals.shouldGrowWithForGeneratorLazyListOrStreamPair(_.value.value)
        gen.shouldGrowWithForShrink(_.value)
      }

      it("should shrink FiniteFloats with an algo towards 0") {
        import GeneratorDrivenPropertyChecks._
        forAll { (shrinkRoseTree: RoseTree[FiniteFloat]) =>
          val i = shrinkRoseTree.value
          val shrinks: LazyListOrStream[FiniteFloat] = shrinkRoseTree.shrinks.map(_.value)
          shrinks.distinct.length shouldEqual shrinks.length
          if (i.value == 0.0f)
            shrinks shouldBe empty
          else {
            inspectAll(shrinks) { s =>
              if (i.value >= 0)
                s.value should be < i.value
              else
                s.value should be > i.value  
            }  
          }
        }
      }

      it("should produce shrinkees following constraint determined by filter method") {
        val aGen= Generator.finiteFloatGenerator.filter(_ > 5.0f)
        val (rs, _, _) = aGen.next(SizeParam(1, 0, 1), List(FiniteFloat(40.0f)), Randomizer.default)
        val shrinkees = rs.shrinks.map(_.value)
        shrinkees should not be empty
        shrinkees.toList shouldBe List(FiniteFloat(6.0f))
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
        List(a1.value, a2.value, a3.value, a4.value, a5.value) should contain theSameElementsAs List(b1.value, b2.value, b3.value, b4.value, b5.value)
        a6.value shouldEqual b6.value
        a7.value shouldEqual b7.value
      }
      it("should produce FiniteDouble edge values first in random order") {
        import Generator._
        val gen = finiteDoubleGenerator
        val (initEdges, ier) = gen.initEdges(10, Randomizer.default)
        val (a1: RoseTree[FiniteDouble], ae1: List[FiniteDouble], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, ae3, ar3) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val (a4, ae4, ar4) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae3, rnd = ar3)
        val (a5, ae5, ar5) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae4, rnd = ar4)
        val (a6, ae6, ar6) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae5, rnd = ar5)
        val (a7, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae6, rnd = ar6)
        val edges = List(a1, a2, a3, a4, a5, a6, a7).map(_.value)
        edges should contain (FiniteDouble.MinValue)
        edges should contain (FiniteDouble(-1.0))
        edges should contain (FiniteDouble.ensuringValid(-FiniteDouble.MinPositiveValue))
        edges should contain (FiniteDouble(0.0))
        edges should contain (FiniteDouble.MinPositiveValue)
        edges should contain (FiniteDouble(1.0))
        edges should contain (FiniteDouble.MaxValue)
      }

      it("should have legitimate canonicals") {
        import Generator._
        val gen = finiteDoubleGenerator
        val rnd = Randomizer.default
        gen.shouldGrowWithForShrink(_.value)
        gen.canonicals.shouldGrowWithForGeneratorLazyListOrStreamPair(_.value.value)
      }

      it("should shrink FiniteDoubles with an algo towards 0") {
        import GeneratorDrivenPropertyChecks._
        forAll { (shrinkRoseTree: RoseTree[FiniteDouble]) =>
          val i = shrinkRoseTree.value
          val shrinks: LazyListOrStream[FiniteDouble] = shrinkRoseTree.shrinks.map(_.value)
          shrinks.distinct.length shouldEqual shrinks.length
          if (i.value == 0.0)
            shrinks shouldBe empty
          else {
            inspectAll(shrinks) { s =>
              if (i.value >= 0.0)
                s.value should be < i.value
              else
                s.value should be > i.value  
            }  
          }
        }
      }

      it("should produce shrinkees following constraint determined by filter method") {
        val aGen= Generator.finiteDoubleGenerator.filter(_ > 5.0)
        val (rs, _, _) = aGen.next(SizeParam(1, 0, 1), List(FiniteDouble(40.0)), Randomizer.default)
        val shrinkees = rs.shrinks.map(_.value)
        shrinkees should not be empty
        shrinkees.toList shouldBe List(FiniteDouble(6.0))
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
        val (a1, ae1, ar1) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = initEdges, rnd = ier)
        val (a2, _, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        List(a1, a2).map(_.value) should contain theSameElementsAs List(NumericChar('0'), NumericChar('9'))
      }

      it("should have legitimate canonicals") {
        import Generator._
        val gen = numericCharGenerator
        val rnd = Randomizer.default
        gen.canonicals.shouldGrowWithForGeneratorLazyListOrStreamPair(_.value.value)
      }

      it("should shrink NumericChars with an algo towards '0'") {
        import GeneratorDrivenPropertyChecks._
        forAll { (shrinkRoseTree: RoseTree[NumericChar]) =>
          val i = shrinkRoseTree.value
          val shrinks: LazyListOrStream[NumericChar] = shrinkRoseTree.shrinks.map(_.value)
          shrinks.distinct.length shouldEqual shrinks.length
          if (i.value == '0')
            shrinks shouldBe empty
          else {
            shrinks should not be empty
            inspectAll(shrinks) { s =>
              s.value.toInt should be < i.value.toInt  
            }  
          }
        }
      }

      it("should produce shrinkees following constraint determined by filter method") {
        val aGen= Generator.numericCharGenerator.filter(_.value.toString.toInt > 5)
        val (rs, _, _) = aGen.next(SizeParam(1, 0, 1), List(NumericChar('9')), Randomizer.default)
        val shrinkees = rs.shrinks.map(_.value)
        shrinkees should not be empty
        shrinkees.toList shouldBe List(NumericChar('8'), NumericChar('7'), NumericChar('6'))
      }
    }

    describe("for Strings") {
      it("should offer a String generator that returns a string whose length equals the passed size") {

        import Generator._
        val gen = stringGenerator

        val (s1, _, r1) = gen.next(szp = SizeParam(PosZInt(0), 100, 0), edges = Nil, rnd = Randomizer(100))
        s1.value.length shouldBe 0

        val (s2, _, r2) = gen.next(szp = SizeParam(PosZInt(0), 100, 3), edges = Nil, rnd = r1)
        s2.value.length shouldBe 3

        val (s3, _, r3) = gen.next(szp = SizeParam(PosZInt(0), 100, 38), edges = Nil, rnd = r2)
        s3.value.length shouldBe 38

        val (s4, _, r4) = gen.next(szp = SizeParam(PosZInt(0), 100, 88), edges = Nil, rnd = r3)
        s4.value.length shouldBe 88

        val (s5, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = Nil, rnd = r4)
        s5.value.length shouldBe 100
      }
      it("should shrink String with an algo towards empty string") {
        import GeneratorDrivenPropertyChecks._
        forAll { (shrinkRoseTree: RoseTree[String]) =>
          val theString = shrinkRoseTree.value
          val shrinks: LazyListOrStream[String] = shrinkRoseTree.shrinks.map(_.value)
          shrinks.distinct.length shouldEqual shrinks.length
          if (theString == "")
            shrinks shouldBe empty
          else {
            shrinks should not be empty
            inspectAll(shrinks) { s =>
              s.length should be < theString.length  
              theString should contain allElementsOf s
            }  
          }
        }
      }
      it("should offer a String generator that offers cononicals based on Char canonicals") {
        import Generator._
        val gen = stringGenerator
        val canonicalsIt = gen.canonicals
        val canonicals: List[String] = canonicalsIt.map(_.value).toList
        canonicals.last shouldBe empty
        import org.scalatest.Inspectors
        Inspectors.forAll (canonicals.init) { (s: String) => s should (be >= "a" and be <= "z") }
      }
      it("should produce shrinkees following constraint determined by filter method") {
        val aGen= Generator.stringGenerator.filter(_.length > 5)
        val (rs, _, _) = aGen.next(SizeParam(1, 0, 1), List("one two three four five"), Randomizer.default)
        val shrinkees = rs.shrinks.map(_.value)
        shrinkees should not be empty
        shrinkees.toList shouldBe List("ee four five", "one two thr", "wo thr")
      }
    }

    describe("for Options") {
      it("should produce Nones with a reasonable frequency") {
        import Generator._
        val gen = optionGenerator[Int]

        val classified = CommonGenerators.classify(1000, gen) {
          case Some(_) => "Some"
          case None => "None"
        }

        classified.portions("None") should be (0.01 +- 0.008)
      }

      it("should use the base type for edges") {
        import Generator._
        val baseGen = intGenerator
        val gen = optionGenerator[Int]

        val rnd = Randomizer.default
        val (intEdges, _) = baseGen.initEdges(100, rnd)
        val (optEdges, _) = gen.initEdges(100, rnd)

        optEdges should contain (None)
        optEdges.filter(_.isDefined).map(_.get) should contain theSameElementsAs intEdges
      }

      it("should use the base type for canonicals") {
        import Generator._
        val baseGen = intGenerator
        val gen = optionGenerator[Int]

        val rnd = Randomizer.default
        val intCanon = baseGen.canonicals
        val optCanonIter = gen.canonicals
        val optCanon = optCanonIter.toList

        optCanon.map(_.value) should contain (None)
        optCanon.map(rt => rt.value).filter(_.isDefined).map(_.get) should contain theSameElementsAs intCanon.map(rt => rt.value).toList
      }

      it("should use the base type for shrinking and also produce None") {
        import org.scalatest.OptionValues._
        import GeneratorDrivenPropertyChecks._
        forAll { (shrinkRoseTree: RoseTree[Option[Int]]) =>
          val optI = shrinkRoseTree.value
          val shrinks: LazyListOrStream[Option[Int]] = shrinkRoseTree.shrinks.map(_.value)
          // shrinks.last shouldBe None
          // Decided to not bother with having None at the end of the shrink line, because it is an edge and
          // one out of every 100 or so regular.
          shrinks.distinct.length shouldEqual shrinks.length
          if (optI.isEmpty)
            shrinks shouldBe empty
          else {
            val i = optI.get
            if (i == 0)
              shrinks shouldBe List(None)
            else {
              if (i > 1)
                shrinks.head.value should be < 0
              else if (i < -1)
                shrinks.head.value should be > 0

              import org.scalatest.Inspectors._
              val revShrinks = shrinks.reverse
              val pairs: LazyListOrStream[(Option[Int], Option[Int])] = revShrinks.zip(revShrinks.tail)
              forAll(pairs) {
                case (Some(x), Some(y)) =>
                  assert(x == 0 || x == -y || x.abs == y.abs / 2)
                case (None, Some(_)) => succeed
                case (Some(_), None) => fail("None was ahead of a Some in shrinks (i.e., before being reversed)")
                case (None, None) => fail("None showed up twice in shrinks")
              }
              shrinks.last shouldBe None
            }
          }
        }
      }

      it("should not try to shrink None") {
        import Generator._
        val gen = optionGenerator[Int]
        val rnd = Randomizer.default

        val (optShrink, _, _) = gen.next(SizeParam(1, 0, 1), List(None), rnd)

        assert(optShrink.shrinks.isEmpty)
      }

      it("should produce shrinkees following constraint determined by filter method") {
        val aGen= Generator.optionGenerator[String].filter(_.nonEmpty)
        val (rs, _, _) = aGen.next(SizeParam(1, 0, 1), List(Some("test")), Randomizer.default)
        val shrinkees = rs.shrinks.map(_.value)
        shrinkees should not contain (None)
      }
    }

    describe("for Ors") {
      it("should use the base types for edges") {
        import Generator._
        import org.scalactic._
        val gGen = intGenerator
        val bGen = stringGenerator
        val gen = orGenerator[Int, String]

        val rnd = Randomizer.default
        val (gEdges, _) = gGen.initEdges(100, rnd)
        val (bEdges, _) = bGen.initEdges(100, rnd)
        val (orEdges, _) = gen.initEdges(100, rnd)

        orEdges should contain theSameElementsAs(gEdges.map(Good(_)) ++ bEdges.map(Bad(_)))
      }

      it("should use the base types for canonicals") {
        import Generator._
        import org.scalactic._
        val gGen = intGenerator
        val bGen = stringGenerator
        val gen = orGenerator[Int, String]

        val rnd = Randomizer.default
        val gCanon = gGen.canonicals
        val bCanon = bGen.canonicals
        val orCanon = gen.canonicals

        orCanon.map(_.value).toList should contain theSameElementsAs((gCanon.map(_.value).map(Good(_)) ++ bCanon.map(_.value).map(Bad(_))).toList)
      }

      it("should produce an appropriate mix of Good and Bad", Flicker) {
        import Generator._
        import org.scalactic._
        val gen = orGenerator[Int, String]

        val classification = CommonGenerators.classify(1000, gen) {
          case Good(_) => "Good"
          case Bad(_) => "Bad"
        }

        // It's arbitrary, but we know that it produces Bad about a quarter of the time:
        classification.percentages("Bad").value should be (25 +- 3)
      }

      it("should use the base types to shrink") {
        import Generator._
        import org.scalactic._
        val gGen = intGenerator
        val bGen = longGenerator
        val gen = orGenerator[Int, Long]

        val rnd = Randomizer.default
        val (gShrink, _, _) = gGen.next(SizeParam(1, 0, 1), List(1000), rnd)
        val (bShrink, _, _) = bGen.next(SizeParam(1, 0, 1), List(2000L), rnd)
        val (orGoodShrink, _, _) = gen.next(SizeParam(1, 0, 1), List(Good(1000)), rnd)
        val (orBadShrink, _, _) = gen.next(SizeParam(1, 0, 1), List(Bad(2000L)), rnd)

        orGoodShrink.shrinks.map(_.value) should contain theSameElementsAs(gShrink.shrinks.map(_.value).map(Good(_)).toList)
        orBadShrink.shrinks.map(_.value) should contain theSameElementsAs(bShrink.shrinks.map(_.value).map(Bad(_)).toList)
      }

      it("should produce shrinkees following constraint determined by filter method") {
        import Generator._
        import org.scalactic._
        
        val gen = orGenerator[Int, Long].filter { 
          case Good(g) => g > 200
          case Bad(b) => b > 200
        }

        val rnd = Randomizer.default
        
        val (orGoodShrink, _, _) = gen.next(SizeParam(1, 0, 1), List(Good(1000)), rnd)
        val orGoodShrinkees = orGoodShrink.shrinks.map(_.value)
        orGoodShrinkees should not be empty
        orGoodShrinkees.toList shouldBe List(Good(500), Good(250))

        val (orBadShrink, _, _) = gen.next(SizeParam(1, 0, 1), List(Bad(2000L)), rnd)
        val orBadShrinkees = orBadShrink.shrinks.map(_.value)
        orBadShrinkees should not be empty
        orBadShrinkees.toList shouldBe List(Bad(1000L), Bad(500L), Bad(250L))
      }
    }

    describe("for Eithers") {
      it("should use the base types for edges") {
        import Generator._
        val rGen = intGenerator
        val lGen = stringGenerator
        val gen = eitherGenerator[String, Int]

        val rnd = Randomizer.default
        val (rEdges, _) = rGen.initEdges(100, rnd)
        val (lEdges, _) = lGen.initEdges(100, rnd)
        val (eitherEdges, _) = gen.initEdges(100, rnd)

        eitherEdges should contain theSameElementsAs(rEdges.map(Right(_)) ++ lEdges.map(Left(_)))
      }

      it("should use the base types for canonicals") {
        import Generator._
        val rGen = intGenerator
        val lGen = stringGenerator
        val gen = eitherGenerator[String, Int]

        val rnd = Randomizer.default
        val rCanon = rGen.canonicals
        val lCanon = lGen.canonicals
        val eitherCanon = gen.canonicals

        eitherCanon.map(_.value).toList should contain theSameElementsAs((rCanon.map(_.value).map(Right(_)) ++ lCanon.map(_.value).map(Left(_))).toList)
      }

      it("should produce an appropriate mix of Right and Left", Flicker) {
        import Generator._
        val gen = eitherGenerator[String, Int]

        val classification = CommonGenerators.classify(1000, gen) {
          case Right(_) => "Right"
          case Left(_) => "Left"
        }

        // It's arbitrary, but we know that it produces Left about a quarter of the time:
        classification.percentages("Left").value should be (25 +- 2)
      }

      // TODO. Why does this not fail? Make sure it is correct.
      it("should use the base types to shrink") {
        import Generator._
        val rGen = intGenerator
        val lGen = longGenerator
        val gen = eitherGenerator[Long, Int]

        val rnd = Randomizer.default
        val (rShrink, _, _) = rGen.next(SizeParam(1, 0, 1), List(1000), rnd)
        val (lShrink, _, _) = lGen.next(SizeParam(1, 0, 1), List(2000L), rnd)
        val (eitherRightShrink, _, _) = gen.next(SizeParam(1, 0, 1), List(Right(1000)), rnd)
        val (eitherLeftShrink, _, _) = gen.next(SizeParam(1, 0, 1), List(Left(2000L)), rnd)

        eitherRightShrink.shrinks.map(_.value) should contain theSameElementsAs(rShrink.shrinks.map(_.value).map(Right(_)).toList)
        eitherLeftShrink.shrinks.map(_.value) should contain theSameElementsAs(lShrink.shrinks.map(_.value).map(Left(_)).toList)
      }

      it("should produce shrinkees following constraint determined by filter method") {
        import Generator._
        import org.scalactic._
        
        val gen = eitherGenerator[Int, Long].filter { 
          case Left(l) => l > 200
          case Right(r) => r > 200
        }

        val rnd = Randomizer.default
        
        val (orLeftShrink, _, _) = gen.next(SizeParam(1, 0, 1), List(Left(1000)), rnd)
        val orLeftShrinkees = orLeftShrink.shrinks.map(_.value)
        orLeftShrinkees should not be empty
        orLeftShrinkees.toList shouldBe List(Left(500), Left(250))

        val (orRightShrink, _, _) = gen.next(SizeParam(1, 0, 1), List(Right(2000L)), rnd)
        val orRightShrinkees = orRightShrink.shrinks.map(_.value)
        orRightShrinkees should not be empty
        orRightShrinkees.toList shouldBe List(Right(1000L), Right(500L), Right(250L))
      }
    }

    import scala.collection.GenTraversable
    import org.scalactic.ColCompatHelper
    /**
      * A common test for how we do shrinking in the collection Generators.
      *
      * Since we generally try to deal with shrink() the same way for collections, we use
      * this common test to make sure it's consistent. Not every collection type manages
      * to use this (because Scala 2.12 collections just aren't that consistent), but
      * it generally works.
      *
      * @param factory the companion object for this collection type
      * @param generator the Generator for this collection type
      * @tparam F the collection type we are testing
      */
    def shrinkByStrategery[F[Int] <: GenTraversable[Int]](factory: ColCompatHelper.Factory[Int, F[Int]])(implicit generator: Generator[F[Int]]): Unit = {  
      import GeneratorDrivenPropertyChecks._
      val intGenerator = Generator.intGenerator
      val intCanonicalsIt = intGenerator.canonicals
      val intCanonicals = intCanonicalsIt.toList
      forAll { (xs: F[Int]) =>
        // pass in List(xs) as only edge case so the generator will generate rose tree with the specified value.
        val (shrinkRoseTree, _, _) = generator.next(SizeParam(1, 0, 1), List(xs), Randomizer.default)
        val shrinks: LazyListOrStream[F[Int]] = shrinkRoseTree.shrinks.map(_.value).reverse
        if (xs.isEmpty)
          shrinks shouldBe empty
        else {
          // First one should be the empty list
          shrinks(0) shouldBe empty

          // Then should come one-element Lists of the canonicals of the type
          val phase2 = shrinks.drop(1).take(intCanonicals.length)
          phase2 shouldEqual (intCanonicals.map(i => ColCompatHelper.newBuilder(factory).+=(i.value).result))

          // Phase 3 should be one-element lists of all distinct values in the value passed to shrink
          // If xs already is a one-element list, then we don't do this, because then xs would appear in the output.
          val xsList = xs.toList
          val xsDistincts = if (xsList.length > 1) xsList.distinct else Nil
          val phase3 = shrinks.drop(1 + intCanonicals.length).take(xsDistincts.length)
          phase3 shouldEqual (xsDistincts.map(i => ColCompatHelper.newBuilder(factory).+=(i).result))

          // Phase 4 should be n-element lists that are prefixes cut in half
          val theHalves = shrinks.drop(1 + intCanonicals.length + xsDistincts.length)
          theHalves should not contain xs // This was a bug I noticed
          if (theHalves.length > 1) {
            import org.scalatest.Inspectors
            val zipped = theHalves.zip(theHalves.tail)
            Inspectors.forAll (zipped) { case (s, t) =>
              s.size should be < t.size
            }
          } else succeed
        }
      }
    }

    import org.scalactic.ColCompatHelper.Factory._

    describe("for Lists") {
      it("should offer a List[T] generator that returns a List[T] whose length equals the passed size") {

        import Generator._
        val gen = listGenerator[Int]

        val (l1, _, r1) = gen.next(szp = SizeParam(PosZInt(0), 0, 0), edges = Nil, rnd = Randomizer(100))
        l1.value.length shouldBe 0

        val (l2, _, r2) = gen.next(szp = SizeParam(PosZInt(3), 0, 3), edges = Nil, rnd = r1)
        l2.value.length shouldBe 3

        val (l3, _, r3) = gen.next(szp = SizeParam(PosZInt(38), 0, 38), edges = Nil, rnd = r2)
        l3.value.length shouldBe 38

        val (l4, _, r4) = gen.next(szp = SizeParam(PosZInt(88), 0, 88), edges = Nil, rnd = r3)
        l4.value.length shouldBe 88

        val (l5, _, _) = gen.next(szp = SizeParam(PosZInt(100), 0, 100), edges = Nil, rnd = r4)
        l5.value.length shouldBe 100
      }
      it("should not exhibit this bug in List shrinking") {
        val lstGen = implicitly[Generator[List[List[Int]]]]
        val xss = List(List(100, 200, 300, 400, 300))
        lstGen.next(SizeParam(1, 0, 1), List(xss), Randomizer.default)._1.shrinks.map(_.value) should not contain xss
      }
      it("should return an empty LazyListOrStream when asked to shrink a List of size 0") {
        val lstGen = implicitly[Generator[List[Int]]]
        val xs = List.empty[Int]
        lstGen.next(SizeParam(1, 0, 1), List(xs), Randomizer.default)._1.shrinks.map(_.value) shouldBe empty
      }
      it("should shrink List with an algo towards empty List") {
        import GeneratorDrivenPropertyChecks._
        forAll { (shrinkRoseTree: RoseTree[List[Int]]) =>
          val i = shrinkRoseTree.value
          val shrinks: LazyListOrStream[List[Int]] = shrinkRoseTree.shrinks.map(_.value)
          shrinks.distinct.length shouldEqual shrinks.length
          if (i.isEmpty)
            shrinks shouldBe empty
          else {
            shrinks should not be empty // This flickers
            inspectAll(shrinks) { s =>
              i should contain allElementsOf s
              s.length should be < i.length  
            }  
          }
        }
      }
      it("should produce shrinkees following size determined by havingSize method") {
        val aGen= Generator.listGenerator[Int].havingSize(5)
        val shrinkees = aGen.next(SizeParam(1, 0, 1), List(List(3, 99)), Randomizer.default)._1.shrinks.map(_.value)
        all(shrinkees) should have size 5
      }
      it("should produce shrinkees following length determined by havingLength method") {
        val aGen= Generator.vectorGenerator[Int].havingLength(5)
        val shrinkees = aGen.next(SizeParam(1, 0, 1), List(Vector(3, 99)), Randomizer.default)._1.shrinks.map(_.value)
        all(shrinkees) should have length 5
      }
      it("should produce shrinkees following sizes determined by havingSizesBetween method") {
        val aGen= Generator.vectorGenerator[Int].havingSizesBetween(2, 5)
        val (v, _, _) = aGen.next(SizeParam(1, 0, 1), List(Vector(3, 99)), Randomizer.default)
        val shrinkees = v.shrinks.map(_.value)
        if (v.value.size >= 4)
          shrinkees should not be empty
        shrinkees.foreach { shrinkee =>
          assert(shrinkee.size >= 2 && shrinkee.size <= 5) 
        }
      }
      it("should produce shrinkees following sizes determined by havingLengthsBetween method") {
        val aGen= Generator.vectorGenerator[Int].havingLengthsBetween(2, 5)
        val (v, _, _) = aGen.next(SizeParam(1, 0, 1), List(Vector(3, 99)), Randomizer.default)
        val shrinkees = v.shrinks.map(_.value)
        if (v.value.length >= 4)
          shrinkees should not be empty
        shrinkees.foreach { shrinkee =>
          assert(shrinkee.length >= 2 && shrinkee.length <= 5) 
        }
      }
      it("should produce shrinkees following sizes determined by havingSizesDeterminedBy method") {
        val aGen= Generator.vectorGenerator[Int].havingSizesDeterminedBy(s => SizeParam(2, 3, 5))
        val (v, _, _) = aGen.next(SizeParam(1, 0, 1), List(Vector(3, 99)), Randomizer.default)
        val shrinkees = v.shrinks.map(_.value)
        if (v.value.size >= 4)
          shrinkees should not be empty
        shrinkees.foreach { shrinkee =>
          assert(shrinkee.size >= 2 && shrinkee.size <= 5) 
        }
      }
      it("should produce shrinkees following sizes determined by havingLengthsDeterminedBy method") {
        val aGen= Generator.vectorGenerator[Int].havingLengthsDeterminedBy(s => SizeParam(2, 3, 5))
        val (v, _, _) = aGen.next(SizeParam(1, 0, 1), List(Vector(3, 99)), Randomizer.default)
        val shrinkees = v.shrinks.map(_.value)
        if (v.value.length >= 4)
          shrinkees should not be empty
        shrinkees.foreach { shrinkee =>
          assert(shrinkee.length >= 2 && shrinkee.length <= 5) 
        }
      }
      it("should return an LazyListOrStream that does not repeat the passed list-to-shink even if that list has a power of 2 length") {
        // Since the last batch of lists produced by the list shrinker start at length 2 and then double in size each time,
        // they lengths will be powers of two: 2, 4, 8, 16, etc... So make sure that if the original length has length 16,
        // for example, that that one doesn't show up in the shrinks output, because it would be the original list-to-shrink.
        val lstGen = implicitly[Generator[List[Int]]]
        val listToShrink = List.fill(16)(99)
        val shrinkees = lstGen.next(SizeParam(1, 0, 1), List(listToShrink), Randomizer.default)._1.shrinks.map(_.value)
        shrinkees.distinct should not contain listToShrink
      }
      it("should offer a list generator whose canonical method uses the canonical method of the underlying T") {
        import GeneratorDrivenPropertyChecks._
        val intGenerator = Generator.intGenerator
        val intCanonicalsIt = intGenerator.canonicals
        val intCanonicals = intCanonicalsIt.map(_.value).toList
        val listOfIntGenerator = Generator.listGenerator[Int]
        val listOfIntCanonicalsIt = listOfIntGenerator.canonicals
        val listOfIntCanonicals = listOfIntCanonicalsIt.map(_.value).toList
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
        val intCanonicalsIt = ints.canonicals
        val function0CanonicalsIt = function0s.canonicals
        val intCanonicals = intCanonicalsIt.map(_.value).toList
        val function0Canonicals = function0CanonicalsIt.map(_.value).toList
        function0Canonicals.map(f => f()) should contain theSameElementsAs intCanonicals
      }
      it("should offer an implicit provider for constant function0's that returns the shrinks of the result type") {
        val ints = Generator.intGenerator
        val function0s = Generator.function0Generator[Int]
        import GeneratorDrivenPropertyChecks._
        forAll (ints) { (i: Int) =>
          val rnd = Randomizer(i)
          val (intShrinksRt, _, rnd1) = ints.next(SizeParam(1, 0, 1), List.empty, rnd)
          val (function0ShrinksRt, _, _) = function0s.next(SizeParam(1, 0, 1), List.empty, rnd)
          val intShrinks = intShrinksRt.shrinks.map(_.value)
          val function0Shrinks = function0ShrinksRt.shrinks.map(_.value)
          function0Shrinks.map(f => f()) should contain theSameElementsAs intShrinks
        }
      }
    }
    describe("for arbitrary Function1s") {
      it("should offer an implicit provider that uses hashCode to tweak a seed and has a pretty toString") {
        val gen = implicitly[Generator[Option[Int] => List[Int]]]
        val sample = gen.sample
        // SKIP-DOTTY-START
        sample.toString should include ("Option[Int]")
        sample.toString should include ("List[Int]")
        // SKIP-DOTTY-END
        //DOTTY-ONLY sample.toString should include ("scala.Option[scala.Int]")
        //DOTTY-ONLY sample.toString should include ("scala.collection.immutable.List[scala.Int]")
      }
    }
    describe("for Tuple2s") {
      it("should offer a tuple2 generator") {
        val gen = implicitly[Generator[(Int, Int)]]
        val intGen = implicitly[Generator[Int]]
        val rnd = Randomizer.default
        val (intRt1, _, intRnd1) = intGen.next(SizeParam(0, 8, 8), Nil, rnd)
        val (intRt2, _, intRnd2) = intGen.next(SizeParam(0, 18, 18), Nil, intRnd1)

        val (tupRt1, _, tupRnd1) = gen.next(SizeParam(0, 18, 18), Nil, rnd)
        
        tupRt1.value._1 shouldEqual intRt1.value
        tupRt1.value._2 shouldEqual intRt2.value

        val shIntRt1 = intRt1.shrinks
        val shIntRt2 = intRt2.shrinks
        val shTupRt1 = tupRt1.shrinks

        val shIntHeadValueX2 = -(shIntRt1.head.value * 2)
        val expected = 
          shIntRt2.map { v2 =>
            (shIntHeadValueX2, v2.value)
          }

        inspectAll(shTupRt1.map(_.value).zip(expected)) { case ((t1, t2), (e1, e2)) =>
          t1 should equal (e1 +- 1)
          t2 should equal (e2)
        } 
      }
      it("should be able to transform a tuple generator to a case class generator") {
        val tupGen: Generator[(String, Int)] = Generator.tuple2Generator[String, Int]
        case class Person(name: String, age: Int)
        val persons = for (tup <- tupGen) yield Person(tup._1, tup._2)
        val (rt, _, _) = persons.next(SizeParam(1, 0, 1), List.empty, Randomizer.default)
        rt.shrinks should not be empty
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
    describe("for Vector[T]s") {
      it("should produce the same Vector[T] values in the same order given the same Randomizer") {
        val aGen= Generator.vectorGenerator[Int]
        val bGen = Generator.vectorGenerator[Int]
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
        List(a1, a2, a3, a4, a5).map(_.value) should contain theSameElementsAs List(b1, b2, b3, b4, b5).map(_.value)
        a6.value shouldEqual b6.value
        a7.value shouldEqual b7.value
      }
      it("should produce Vector[T] edge values first in random order") {
        val gen = Generator.vectorGenerator[Int]
        val (a1: RoseTree[Vector[Int]], ae1: List[Vector[Int]], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = List(Vector.empty[Int], Vector(1, 2), Vector(3, 4, 5)), rnd = Randomizer.default)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val edges = List(a1, a2, a3).map(_.value)
        edges should contain (Vector.empty[Int])
        edges should contain (Vector(1, 2))
        edges should contain (Vector(3, 4, 5))
      }
      it("should produce Vector[T] following size determined by havingSize method") {
        val aGen= Generator.vectorGenerator[Int]
        implicit val sGen: Generator[Vector[Int]] = aGen.havingSize(PosZInt(3))

        import GeneratorDrivenPropertyChecks._

        forAll { (v: Vector[Int]) =>
          v.size shouldBe 3
        }
      }
      it("should produce Vector[T] following length determined by havingLength method") {
        val aGen= Generator.vectorGenerator[Int]
        implicit val sGen: Generator[Vector[Int]] = aGen.havingLength(PosZInt(3))

        import GeneratorDrivenPropertyChecks._

        forAll { (v: Vector[Int]) =>
          v.length shouldBe 3
        }
      }
      it("should produce Vector[T] following sizes determined by havingSizeBetween method") {
        val aGen= Generator.vectorGenerator[Int]
        implicit val sGen: Generator[Vector[Int]] = aGen.havingSizesBetween(PosZInt(3), PosZInt(5))

        import GeneratorDrivenPropertyChecks._

        forAll { (v: Vector[Int]) =>
          v.size should (be >= 3 and be <= 5)
        }
      }
      it("should produce IllegalArgumentException when havingSizesBetween is called with invalid from and to pair") {
        val aGen= Generator.vectorGenerator[Int]
        aGen.havingSizesBetween(PosZInt(3), PosZInt(5))
        assertThrows[IllegalArgumentException] {
          aGen.havingSizesBetween(PosZInt(3), PosZInt(3))
        }
        assertThrows[IllegalArgumentException] {
          aGen.havingSizesBetween(PosZInt(3), PosZInt(2))
        }
      }
      it("should produce Vector[T] following lengths determined by havingLengthBetween method") {
        val aGen= Generator.vectorGenerator[Int]
        implicit val sGen: Generator[Vector[Int]] = aGen.havingLengthsBetween(PosZInt(3), PosZInt(5))

        import GeneratorDrivenPropertyChecks._

        forAll { (v: Vector[Int]) =>
          v.length should (be >= 3 and be <= 5)
        }
      }
      it("should produce IllegalArgumentException when havingLengthBetween is called with invalid from and to pair") {
        val aGen= Generator.vectorGenerator[Int]
        aGen.havingLengthsBetween(PosZInt(3), PosZInt(5))
        assertThrows[IllegalArgumentException] {
          aGen.havingLengthsBetween(PosZInt(3), PosZInt(3))
        }
        assertThrows[IllegalArgumentException] {
          aGen.havingLengthsBetween(PosZInt(3), PosZInt(2))
        }
      }
      it("should produce Vector[T] following sizes determined by havingSizesDeterminedBy method") {
        val aGen= Generator.vectorGenerator[Int]
        implicit val sGen: Generator[Vector[Int]] = aGen.havingSizesDeterminedBy(s => SizeParam(5, 0, 5))

        import GeneratorDrivenPropertyChecks._

        forAll { (v: Vector[Int]) =>
          v.size shouldBe 5
        }
      }
      it("should produce Vector[T] following sizes determined by havingLengthsDeterminedBy method") {
        val aGen= Generator.vectorGenerator[Int]
        implicit val sGen: Generator[Vector[Int]] = aGen.havingLengthsDeterminedBy(s => SizeParam(5, 0, 5))

        import GeneratorDrivenPropertyChecks._

        forAll { (v: Vector[Int]) =>
          v.length shouldBe 5
        }
      }
      it("should not exhibit this bug in Vector shrinking") {
        val vectorGen = implicitly[Generator[Vector[Vector[Int]]]]
        val xss = Vector(Vector(100, 200, 300, 400, 300))
        vectorGen.next(SizeParam(1, 0, 1), List(xss), Randomizer.default)._1.shrinks.map(_.value) should not contain xss
      }
      it("should shrink Vector with an algo towards empty Vector") {
        import GeneratorDrivenPropertyChecks._
        forAll { (shrinkRoseTree: RoseTree[Vector[Int]]) =>
          val i = shrinkRoseTree.value
          val shrinks: LazyListOrStream[Vector[Int]] = shrinkRoseTree.shrinks.map(_.value)
          shrinks.distinct.length shouldEqual shrinks.length
          if (i.isEmpty)
            shrinks shouldBe empty
          else {
            shrinks should not be empty
            inspectAll(shrinks) { s =>
              i should contain allElementsOf s
              s.length should be < i.length  
            }  
          }
        }
      }
      it("should produce shrinkees following size determined by havingSize method") {
        val aGen= Generator.vectorGenerator[Int].havingSize(5)
        val shrinkees = aGen.next(SizeParam(1, 0, 1), List(Vector(3, 99)), Randomizer.default)._1.shrinks.map(_.value)
        all(shrinkees) should have size 5
      }
      it("should produce shrinkees following length determined by havingLength method") {
        val aGen= Generator.vectorGenerator[Int].havingLength(5)
        val shrinkees = aGen.next(SizeParam(1, 0, 1), List(Vector(3, 99)), Randomizer.default)._1.shrinks.map(_.value)
        all(shrinkees) should have length 5
      }
      it("should produce shrinkees following sizes determined by havingSizesBetween method") {
        val aGen= Generator.vectorGenerator[Int].havingSizesBetween(2, 5)
        val (v, _, _) = aGen.next(SizeParam(1, 0, 1), List(Vector(3, 99)), Randomizer.default)
        val shrinkees = v.shrinks.map(_.value)
        if (v.value.size >= 4)
          shrinkees should not be empty
        shrinkees.foreach { shrinkee =>
          assert(shrinkee.size >= 2 && shrinkee.size <= 5) 
        }
      }
      it("should produce shrinkees following sizes determined by havingLengthsBetween method") {
        val aGen= Generator.vectorGenerator[Int].havingLengthsBetween(2, 5)
        val (v, _, _) = aGen.next(SizeParam(1, 0, 1), List(Vector(3, 99)), Randomizer.default)
        val shrinkees = v.shrinks.map(_.value)
        if (v.value.length >= 4)
          shrinkees should not be empty
        shrinkees.foreach { shrinkee =>
          assert(shrinkee.length >= 2 && shrinkee.length <= 5) 
        }
      }
      it("should produce shrinkees following sizes determined by havingSizesDeterminedBy method") {
        val aGen= Generator.vectorGenerator[Int].havingSizesDeterminedBy(s => SizeParam(2, 3, 5))
        val (v, _, _) = aGen.next(SizeParam(1, 0, 1), List(Vector(3, 99)), Randomizer.default)
        val shrinkees = v.shrinks.map(_.value)
        if (v.value.size >= 4)
          shrinkees should not be empty
        shrinkees.foreach { shrinkee =>
          assert(shrinkee.size >= 2 && shrinkee.size <= 5) 
        }
      }
      it("should produce shrinkees following sizes determined by havingLengthsDeterminedBy method") {
        val aGen= Generator.vectorGenerator[Int].havingLengthsDeterminedBy(s => SizeParam(2, 3, 5))
        val (v, _, _) = aGen.next(SizeParam(1, 0, 1), List(Vector(3, 99)), Randomizer.default)
        val shrinkees = v.shrinks.map(_.value)
        if (v.value.length >= 4)
          shrinkees should not be empty
        shrinkees.foreach { shrinkee =>
          assert(shrinkee.length >= 2 && shrinkee.length <= 5) 
        }
      }
      it("should return an empty LazyListOrStream when asked to shrink a Vector of size 0") {
        val lstGen = implicitly[Generator[Vector[Int]]]
        val xs = Vector.empty[Int]
        lstGen.next(SizeParam(1, 0, 1), List(xs), Randomizer.default)._1.shrinks shouldBe empty
      }
      it("should return an LazyListOrStream that does not repeat canonicals when asked to shrink a Vector of size 2 that includes canonicals") {
        val lstGen = implicitly[Generator[Vector[Int]]]
        val shrinkees = lstGen.next(SizeParam(1, 0, 1), List(Vector(3, 99)), Randomizer.default)._1.shrinks.map(_.value)
        shrinkees.distinct should contain theSameElementsAs shrinkees
      }
      it("should return an LazyListOrStream that does not repeat the passed list-to-shink even if that list has a power of 2 length") {
        // Since the last batch of lists produced by the list shrinker start at length 2 and then double in size each time,
        // they lengths will be powers of two: 2, 4, 8, 16, etc... So make sure that if the original length has length 16,
        // for example, that that one doesn't show up in the shrinks output, because it would be the original list-to-shrink.
        val lstGen = implicitly[Generator[Vector[Int]]]
        val listToShrink = Vector.fill(16)(99)
        val shrinkees = lstGen.next(SizeParam(1, 0, 1), List(listToShrink), Randomizer.default)._1.shrinks.map(_.value)
        shrinkees.distinct should not contain listToShrink
      }
      it("should offer a Vector generator whose canonical method uses the canonical method of the underlying T") {
        import GeneratorDrivenPropertyChecks._
        val intGenerator = Generator.intGenerator
        val intCanonicalsIt = intGenerator.canonicals
        val intCanonicals = intCanonicalsIt.map(_.value).toVector
        val listOfIntGenerator = Generator.vectorGenerator[Int]
        val listOfIntCanonicalsIt = listOfIntGenerator.canonicals
        val listOfIntCanonicals = listOfIntCanonicalsIt.map(_.value).toList
        listOfIntCanonicals shouldEqual intCanonicals.map(i => List(i))
      }
    }

    describe("for Set[T]s") {
      it("should produce the same Set[T] values in the same order given the same Randomizer") {
        val aGen= Generator.setGenerator[Int]
        val bGen = Generator.setGenerator[Int]
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
        List(a1, a2, a3, a4, a5).map(_.value) should contain theSameElementsAs List(b1, b2, b3, b4, b5).map(_.value)
        a6.value shouldEqual b6.value
        a7.value shouldEqual b7.value
      }
      it("should produce Set[T] edge values first in random order") {
        val gen = Generator.setGenerator[Int]
        val (a1: RoseTree[Set[Int]], ae1: List[Set[Int]], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = List(Set.empty[Int], Set(1, 2), Set(3, 4, 5)), rnd = Randomizer.default)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val edges = List(a1, a2, a3).map(_.value)
        edges should contain (Set.empty[Int])
        edges should contain (Set(1, 2))
        edges should contain (Set(3, 4, 5))
      }
      it("should produce Set[T] following size determined by havingSize method") {
        val aGen= Generator.setGenerator[Int]
        implicit val sGen: Generator[Set[Int]] = aGen.havingSize(PosZInt(3))

        import GeneratorDrivenPropertyChecks._

        forAll { (s: Set[Int]) =>
          s.size shouldBe 3
        }
      }
      it("should produce Set[T] following sizes determined by havingSizeBetween method") {
        val aGen= Generator.setGenerator[Int]
        implicit val sGen: Generator[Set[Int]] = aGen.havingSizesBetween(PosZInt(3), PosZInt(5))

        import GeneratorDrivenPropertyChecks._

        forAll { (s: Set[Int]) =>
          s.size should (be >= 3 and be <= 5)
        }
      }
      it("should produce IllegalArgumentException when havingSizesBetween is called with invalid from and to pair") {
        val aGen= Generator.setGenerator[Int]
        aGen.havingSizesBetween(PosZInt(3), PosZInt(5))
        assertThrows[IllegalArgumentException] {
          aGen.havingSizesBetween(PosZInt(3), PosZInt(3))
        }
        assertThrows[IllegalArgumentException] {
          aGen.havingSizesBetween(PosZInt(3), PosZInt(2))
        }
      }
      it("should produce Set[T] following sizes determined by havingSizesDeterminedBy method") {
        val aGen= Generator.setGenerator[Int]
        implicit val sGen: Generator[Set[Int]] = aGen.havingSizesDeterminedBy(s => SizeParam(5, 0, 5))

        import GeneratorDrivenPropertyChecks._

        forAll { (s: Set[Int]) =>
          s.size shouldBe 5
        }
      }
      it("should not exhibit this bug in Set shrinking") {
        val setGen = implicitly[Generator[Set[Set[Int]]]]
        val xss = Set(Set(100, 200, 300, 400, 300))
        setGen.next(SizeParam(1, 0, 1), List(xss), Randomizer.default)._1.shrinks.map(_.value) should not contain xss
      }
      it("should shrink Set with an algo towards empty Set") {
        import GeneratorDrivenPropertyChecks._
        forAll { (shrinkRoseTree: RoseTree[Set[Int]]) =>
          val i = shrinkRoseTree.value
          val shrinks: LazyListOrStream[Set[Int]] = shrinkRoseTree.shrinks.map(_.value)
          shrinks.distinct.length shouldEqual shrinks.length
          if (i.isEmpty)
            shrinks shouldBe empty
          else {
            shrinks should not be empty
            inspectAll(shrinks) { s =>
              i should contain allElementsOf s
              s.size should be < i.size  
            }  
          }
        }
      }
      it("should produce shrinkees following size determined by havingSize method") {
        val aGen= Generator.setGenerator[Int].havingSize(5)
        val shrinkees = aGen.next(SizeParam(1, 0, 1), List(Set(3, 99)), Randomizer.default)._1.shrinks.map(_.value)
        all(shrinkees) should have size 5
      }
      it("should produce shrinkees following sizes determined by havingSizesBetween method") {
        val aGen= Generator.setGenerator[Int].havingSizesBetween(2, 5)
        val (v, _, _) = aGen.next(SizeParam(1, 0, 1), List(Set(3, 99)), Randomizer.default)
        val shrinkees = v.shrinks.map(_.value)
        if (v.value.size >= 4)
          shrinkees should not be empty
        shrinkees.foreach { shrinkee =>
          assert(shrinkee.size >= 2 && shrinkee.size <= 5) 
        }
      }
      it("should produce shrinkees following sizes determined by havingSizesDeterminedBy method") {
        val aGen= Generator.setGenerator[Int].havingSizesDeterminedBy(s => SizeParam(2, 3, 5))
        val (v, _, _) = aGen.next(SizeParam(1, 0, 1), List(Set(3, 99)), Randomizer.default)
        val shrinkees = v.shrinks.map(_.value)
        if (v.value.size >= 4)
          shrinkees should not be empty
        shrinkees.foreach { shrinkee =>
          assert(shrinkee.size >= 2 && shrinkee.size <= 5) 
        }
      }
      it("should return an empty LazyListOrStream when asked to shrink a Set of size 0") {
        val lstGen = implicitly[Generator[Set[Int]]]
        val xs = Set.empty[Int]
        lstGen.next(SizeParam(1, 0, 1), List(xs), Randomizer.default)._1.shrinks.map(_.value).toSet shouldBe empty
      }
      it("should return an LazyListOrStream that does not repeat canonicals when asked to shrink a Set of size 2 that includes canonicals") {
        val lstGen = implicitly[Generator[Set[Int]]]
        val shrinkees = lstGen.next(SizeParam(1, 0, 1), List(Set(3, 99)), Randomizer.default)._1.shrinks.map(_.value)
        shrinkees.distinct should contain theSameElementsAs shrinkees
      }
      it("should return an LazyListOrStream that does not repeat the passed set-to-shink even if that set has a power of 2 length") {
        // Since the last batch of lists produced by the list shrinker start at length 2 and then double in size each time,
        // they lengths will be powers of two: 2, 4, 8, 16, etc... So make sure that if the original length has length 16,
        // for example, that that one doesn't show up in the shrinks output, because it would be the original list-to-shrink.
        val lstGen = implicitly[Generator[Set[Int]]]
        val listToShrink: Set[Int] = (Set.empty[Int] /: (1 to 16)) { (set, n) =>
          set + n
        }
        val shrinkees = lstGen.next(SizeParam(1, 0, 1), List(listToShrink), Randomizer.default)._1.shrinks.map(_.value)
        shrinkees.distinct should not contain listToShrink
      }
      it("should offer a Set generator whose canonical method uses the canonical method of the underlying T") {
        import GeneratorDrivenPropertyChecks._
        val intGenerator = Generator.intGenerator
        val intCanonicalsIt = intGenerator.canonicals
        val intCanonicals = intCanonicalsIt.map(_.value).toList
        val listOfIntGenerator = Generator.setGenerator[Int]
        val listOfIntCanonicalsIt = listOfIntGenerator.canonicals
        val listOfIntCanonicals = listOfIntCanonicalsIt.map(_.value).toList
        listOfIntCanonicals shouldEqual intCanonicals.map(i => Set(i))
      }
    }

    describe("for SortedSet[T]s") {
      it("should produce the same SortedSet[T] values in the same order given the same Randomizer") {
        val aGen= Generator.sortedSetGenerator[Int]
        val bGen = Generator.sortedSetGenerator[Int]
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
        List(a1, a2, a3, a4, a5).map(_.value) should contain theSameElementsAs List(b1, b2, b3, b4, b5).map(_.value)
        a6.value shouldEqual b6.value
        a7.value shouldEqual b7.value
      }
      it("should produce SortedSet[T] edge values first in random order") {
        val gen = Generator.sortedSetGenerator[Int]
        val (a1: RoseTree[SortedSet[Int]], ae1: List[SortedSet[Int]], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = List(SortedSet.empty[Int], SortedSet(1, 2), SortedSet(3, 4, 5)), rnd = Randomizer.default)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val edges = List(a1, a2, a3).map(_.value)
        edges should contain (SortedSet.empty[Int])
        edges should contain (SortedSet(1, 2))
        edges should contain (SortedSet(3, 4, 5))
      }
      it("should produce Set[T] following size determined by havingSize method") {
        val aGen= Generator.sortedSetGenerator[Int]
        implicit val sGen: Generator[SortedSet[Int]] = aGen.havingSize(PosZInt(3))

        import GeneratorDrivenPropertyChecks._

        forAll { (s: SortedSet[Int]) =>
          s.size shouldBe 3
        }
      }
      it("should produce Set[T] following sizes determined by havingSizeBetween method") {
        val aGen= Generator.sortedSetGenerator[Int]
        implicit val sGen: Generator[SortedSet[Int]] = aGen.havingSizesBetween(PosZInt(3), PosZInt(5))

        import GeneratorDrivenPropertyChecks._

        forAll { (s: SortedSet[Int]) =>
          s.size should (be >= 3 and be <= 5)
        }
      }
      it("should produce IllegalArgumentException when havingSizesBetween is called with invalid from and to pair") {
        val aGen= Generator.sortedSetGenerator[Int]
        aGen.havingSizesBetween(PosZInt(3), PosZInt(5))
        assertThrows[IllegalArgumentException] {
          aGen.havingSizesBetween(PosZInt(3), PosZInt(3))
        }
        assertThrows[IllegalArgumentException] {
          aGen.havingSizesBetween(PosZInt(3), PosZInt(2))
        }
      }
      it("should produce Set[T] following sizes determined by havingSizesDeterminedBy method") {
        val aGen= Generator.sortedSetGenerator[Int]
        implicit val sGen: Generator[SortedSet[Int]] = aGen.havingSizesDeterminedBy(s => SizeParam(5, 0, 5))

        import GeneratorDrivenPropertyChecks._

        forAll { (s: SortedSet[Int]) =>
          s.size shouldBe 5
        }
      }
      it("should shrink Set with an algo towards empty Set") {
        import GeneratorDrivenPropertyChecks._
        forAll { (shrinkRoseTree: RoseTree[SortedSet[Int]]) =>
          val i = shrinkRoseTree.value
          val shrinks: LazyListOrStream[SortedSet[Int]] = shrinkRoseTree.shrinks.map(_.value)
          shrinks.distinct.length shouldEqual shrinks.length
          if (i.isEmpty)
            shrinks shouldBe empty
          else {
            shrinks should not be empty
            inspectAll(shrinks) { s =>
              i should contain allElementsOf s
              s.size should be < i.size  
            }  
          }
        }
      }
      it("should produce shrinkees following size determined by havingSize method") {
        val aGen= Generator.sortedSetGenerator[Int].havingSize(5)
        val shrinkees = aGen.next(SizeParam(1, 0, 1), List(SortedSet(3, 99)), Randomizer.default)._1.shrinks.map(_.value)
        all(shrinkees) should have size 5
      }
      it("should produce shrinkees following sizes determined by havingSizesBetween method") {
        val aGen= Generator.sortedSetGenerator[Int].havingSizesBetween(2, 5)
        val (v, _, _) = aGen.next(SizeParam(1, 0, 1), List(SortedSet(3, 99)), Randomizer.default)
        val shrinkees = v.shrinks.map(_.value)
        if (v.value.size >= 4)
          shrinkees should not be empty
        shrinkees.foreach { shrinkee =>
          assert(shrinkee.size >= 2 && shrinkee.size <= 5) 
        }
      }
      it("should produce shrinkees following sizes determined by havingSizesDeterminedBy method") {
        val aGen= Generator.sortedSetGenerator[Int].havingSizesDeterminedBy(s => SizeParam(2, 3, 5))
        val (v, _, _) = aGen.next(SizeParam(1, 0, 1), List(SortedSet(3, 99)), Randomizer.default)
        val shrinkees = v.shrinks.map(_.value)
        if (v.value.size >= 4)
          shrinkees should not be empty
        shrinkees.foreach { shrinkee =>
          assert(shrinkee.size >= 2 && shrinkee.size <= 5) 
        }
      }
      it("should return an empty LazyListOrStream when asked to shrink a SortedSet of size 0") {
        val lstGen = implicitly[Generator[SortedSet[Int]]]
        val xs = SortedSet.empty[Int]
        lstGen.next(SizeParam(1, 0, 1), List(xs), Randomizer.default)._1.shrinks.map(_.value).toSet shouldBe empty
      }
      it("should return an LazyListOrStream that does not repeat canonicals when asked to shrink a SortedSet of size 2 that includes canonicals") {
        val lstGen = implicitly[Generator[SortedSet[Int]]]
        val shrinkees = lstGen.next(SizeParam(1, 0, 1), List(SortedSet(3, 99)), Randomizer.default)._1.shrinks.map(_.value)
        shrinkees.distinct should contain theSameElementsAs shrinkees
      }
      it("should return an LazyListOrStream that does not repeat the passed set-to-shink even if that set has a power of 2 length") {
        // Since the last batch of lists produced by the list shrinker start at length 2 and then double in size each time,
        // they lengths will be powers of two: 2, 4, 8, 16, etc... So make sure that if the original length has length 16,
        // for example, that that one doesn't show up in the shrinks output, because it would be the original list-to-shrink.
        val lstGen = implicitly[Generator[SortedSet[Int]]]
        val listToShrink: SortedSet[Int] = (SortedSet.empty[Int] /: (1 to 16)) { (set, n) =>
          set + n
        }
        val shrinkees = lstGen.next(SizeParam(1, 0, 1), List(listToShrink), Randomizer.default)._1.shrinks.map(_.value)
        shrinkees.distinct should not contain listToShrink
      }
      it("should offer a Set generator whose canonical method uses the canonical method of the underlying T") {
        import GeneratorDrivenPropertyChecks._
        val intGenerator = Generator.intGenerator
        val intCanonicalsIt = intGenerator.canonicals
        val intCanonicals = intCanonicalsIt.map(_.value).toList
        val listOfIntGenerator = Generator.sortedSetGenerator[Int]
        val listOfIntCanonicalsIt = listOfIntGenerator.canonicals
        val listOfIntCanonicals = listOfIntCanonicalsIt.map(_.value).toList
        listOfIntCanonicals shouldEqual intCanonicals.map(i => SortedSet(i))
      }
    }

    describe("for Map[K, V]s") {
      it("should produce the same Map[K, V] values in the same order given the same Randomizer") {
        val aGen= Generator.mapGenerator[Int, String]
        val bGen = Generator.mapGenerator[Int, String]
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
        List(a1, a2, a3, a4, a5).map(_.value) should contain theSameElementsAs List(b1, b2, b3, b4, b5).map(_.value)
        a6.value shouldEqual b6.value
        a7.value shouldEqual b7.value
      }
      it("should produce Map[K, V] edge values first in random order") {
        val gen = Generator.mapGenerator[Int, String]
        val (a1: RoseTree[Map[Int, String]], ae1: List[Map[Int, String]], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = List(Map.empty[Int, String], Map(1 -> "one", 2 -> "two"), Map(3 -> "three", 4 -> "four", 5 -> "five")), rnd = Randomizer.default)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val edges = List(a1, a2, a3).map(_.value)
        edges should contain (Map.empty[Int, String])
        edges should contain (Map(1 -> "one", 2 -> "two"))
        edges should contain (Map(3 -> "three", 4 -> "four", 5 -> "five"))
      }
      it("should produce Map[K, V] following size determined by havingSize method") {
        val aGen= Generator.mapGenerator[Int, String]
        implicit val sGen: Generator[Map[Int, String]] = aGen.havingSize(PosZInt(3))

        import GeneratorDrivenPropertyChecks._

        forAll { (s: Map[Int, String]) =>
          s.size shouldBe 3
        }
      }
      it("should produce Map[K, V] following sizes determined by havingSizeBetween method") {
        val aGen= Generator.mapGenerator[Int, String]
        implicit val sGen: Generator[Map[Int, String]] = aGen.havingSizesBetween(PosZInt(3), PosZInt(5))

        import GeneratorDrivenPropertyChecks._

        forAll { (s: Map[Int, String]) =>
          s.size should (be >= 3 and be <= 5)
        }
      }
      it("should produce IllegalArgumentException when havingSizesBetween is called with invalid from and to pair") {
        val aGen= Generator.mapGenerator[Int, String]
        aGen.havingSizesBetween(PosZInt(3), PosZInt(5))
        assertThrows[IllegalArgumentException] {
          aGen.havingSizesBetween(PosZInt(3), PosZInt(3))
        }
        assertThrows[IllegalArgumentException] {
          aGen.havingSizesBetween(PosZInt(3), PosZInt(2))
        }
      }
      it("should produce Map[K, V] following sizes determined by havingSizesDeterminedBy method") {
        val aGen= Generator.mapGenerator[Int, String]
        implicit val sGen: Generator[Map[Int, String]] = aGen.havingSizesDeterminedBy(s => SizeParam(5, 0, 5))

        import GeneratorDrivenPropertyChecks._

        forAll { (s: Map[Int, String]) =>
          s.size shouldBe 5
        }
      }
      it("should shrink Map with an algo towards empty Map") {
        import GeneratorDrivenPropertyChecks._
        forAll { (shrinkRoseTree: RoseTree[Map[Int, String]]) =>
          val i = shrinkRoseTree.value
          val shrinks: LazyListOrStream[Map[Int, String]] = shrinkRoseTree.shrinks.map(_.value)
          shrinks.distinct.length shouldEqual shrinks.length
          if (i.isEmpty)
            shrinks shouldBe empty
          else {
            shrinks should not be empty
            inspectAll(shrinks) { s =>
              i should contain allElementsOf s
              s.size should be < i.size  
            }  
          }
        }
      }
      it("should produce shrinkees following size determined by havingSize method") {
        val aGen= Generator.mapGenerator[Int, String].havingSize(5)
        val shrinkees = aGen.next(SizeParam(1, 0, 1), List(Map(3 -> "three", 99 -> "ninety nine")), Randomizer.default)._1.shrinks.map(_.value)
        all(shrinkees) should have size 5
      }
      it("should produce shrinkees following sizes determined by havingSizesBetween method") {
        val aGen= Generator.mapGenerator[Int, String].havingSizesBetween(2, 5)
        val (v, _, _) = aGen.next(SizeParam(1, 0, 1), List(Map(3 -> "three", 99 -> "ninety nine")), Randomizer.default)
        val shrinkees = v.shrinks.map(_.value)
        if (v.value.size >= 4)
          shrinkees should not be empty
        shrinkees.foreach { shrinkee =>
          assert(shrinkee.size >= 2 && shrinkee.size <= 5) 
        }
      }
      it("should produce shrinkees following sizes determined by havingSizesDeterminedBy method") {
        val aGen= Generator.mapGenerator[Int, String].havingSizesDeterminedBy(s => SizeParam(2, 3, 5))
        val (v, _, _) = aGen.next(SizeParam(1, 0, 1), List(Map(3 -> "three", 99 -> "ninety nine")), Randomizer.default)
        val shrinkees = v.shrinks.map(_.value)
        if (v.value.size >= 4)
          shrinkees should not be empty
        shrinkees.foreach { shrinkee =>
          assert(shrinkee.size >= 2 && shrinkee.size <= 5) 
        }
      }
      it("should return an empty LazyListOrStream when asked to shrink a Map of size 0") {
        val lstGen = implicitly[Generator[Map[PosInt, Int]]]
        val xs = Map.empty[PosInt, Int]
        lstGen.next(SizeParam(1, 0, 1), List(xs), Randomizer.default)._1.shrinks.map(_.value).toSet shouldBe empty
      }
      it("should return an LazyListOrStream that does not repeat canonicals when asked to shrink a Map of size 2 that includes canonicals") {
        val lstGen = implicitly[Generator[Map[PosInt, Int]]]
        val shrinkees = lstGen.next(SizeParam(1, 0, 1), List(Map(PosInt(3) -> 3, PosInt(2) -> 2, PosInt(99) -> 99)), Randomizer.default)._1.shrinks.map(_.value)
        shrinkees.distinct should contain theSameElementsAs shrinkees
      }
      it("should return an LazyListOrStream that does not repeat the passed map-to-shink even if that set has a power of 2 length") {
        // Since the last batch of lists produced by the list shrinker start at length 2 and then double in size each time,
        // they lengths will be powers of two: 2, 4, 8, 16, etc... So make sure that if the original length has length 16,
        // for example, that that one doesn't show up in the shrinks output, because it would be the original list-to-shrink.
        val lstGen = implicitly[Generator[Map[PosInt, Int]]]
        val listToShrink: Map[PosInt, Int] = (Map.empty[PosInt, Int] /: (1 to 16)) { (map, n) =>
          map + (PosInt.ensuringValid(n) -> n)
        }
        val shrinkees = lstGen.next(SizeParam(1, 0, 1), List(listToShrink), Randomizer.default)._1.shrinks.map(_.value)
        shrinkees.distinct should not contain listToShrink
      }
      it("should offer a Map generator whose canonical method uses the canonical method of the underlying types") {
        import GeneratorDrivenPropertyChecks._
        val tupleGenerator = Generator.tuple2Generator[PosInt, Int]
        val tupleCanonicalsIt = tupleGenerator.canonicals
        val tupleCanonicals = tupleCanonicalsIt.map(_.value).toList
        val mapGenerator = Generator.mapGenerator[PosInt, Int]
        val mapCanonicalsIt = mapGenerator.canonicals
        val mapCanonicals = mapCanonicalsIt.map(_.value).toList
        mapCanonicals shouldEqual tupleCanonicals.map(i => Map(i))
      }
    }

    describe("for SortedMaps") {
      it("should produce the same SortedMap[K, V] values in the same order given the same Randomizer") {
        val aGen= Generator.sortedMapGenerator[Int, String]
        val bGen = Generator.sortedMapGenerator[Int, String]
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
        List(a1, a2, a3, a4, a5).map(_.value) should contain theSameElementsAs List(b1, b2, b3, b4, b5).map(_.value)
        a6.value shouldEqual b6.value
        a7.value shouldEqual b7.value
      }
      it("should produce SortedMap[K, V] edge values first in random order") {
        val gen = Generator.sortedMapGenerator[Int, String]
        val (a1: RoseTree[SortedMap[Int, String]], ae1: List[SortedMap[Int, String]], ar1: Randomizer) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = List(SortedMap.empty[Int, String], SortedMap(1 -> "one", 2 -> "two"), SortedMap(3 -> "three", 4 -> "four", 5 -> "five")), rnd = Randomizer.default)
        val (a2, ae2, ar2) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae1, rnd = ar1)
        val (a3, _, _) = gen.next(szp = SizeParam(PosZInt(0), 100, 100), edges = ae2, rnd = ar2)
        val edges = List(a1, a2, a3).map(_.value)
        edges should contain (SortedMap.empty[Int, String])
        edges should contain (SortedMap(1 -> "one", 2 -> "two"))
        edges should contain (SortedMap(3 -> "three", 4 -> "four", 5 -> "five"))
      }
      it("should produce SortedMap[K, V] following size determined by havingSize method") {
        val aGen= Generator.sortedMapGenerator[Int, String]
        implicit val sGen: Generator[SortedMap[Int, String]] = aGen.havingSize(PosZInt(3))

        import GeneratorDrivenPropertyChecks._

        forAll { (s: SortedMap[Int, String]) =>
          s.size shouldBe 3
        }
      }
      it("should produce SortedMap[K, V] following sizes determined by havingSizeBetween method") {
        val aGen= Generator.sortedMapGenerator[Int, String]
        implicit val sGen: Generator[SortedMap[Int, String]] = aGen.havingSizesBetween(PosZInt(3), PosZInt(5))

        import GeneratorDrivenPropertyChecks._

        forAll { (s: SortedMap[Int, String]) =>
          s.size should (be >= 3 and be <= 5)
        }
      }
      it("should produce IllegalArgumentException when havingSizesBetween is called with invalid from and to pair") {
        val aGen= Generator.sortedMapGenerator[Int, String]
        aGen.havingSizesBetween(PosZInt(3), PosZInt(5))
        assertThrows[IllegalArgumentException] {
          aGen.havingSizesBetween(PosZInt(3), PosZInt(3))
        }
        assertThrows[IllegalArgumentException] {
          aGen.havingSizesBetween(PosZInt(3), PosZInt(2))
        }
      }
      it("should produce SortedMap[K, V] following sizes determined by havingSizesDeterminedBy method") {
        val aGen= Generator.sortedMapGenerator[Int, String]
        implicit val sGen: Generator[SortedMap[Int, String]] = aGen.havingSizesDeterminedBy(s => SizeParam(5, 0, 5))

        import GeneratorDrivenPropertyChecks._

        forAll { (s: SortedMap[Int, String]) =>
          s.size shouldBe 5
        }
      }
      it("should shrink SortedMap with an algo towards empty SortedMap") {
        import GeneratorDrivenPropertyChecks._
        forAll { (shrinkRoseTree: RoseTree[SortedMap[Int, String]]) =>
          val i = shrinkRoseTree.value
          val shrinks: LazyListOrStream[SortedMap[Int, String]] = shrinkRoseTree.shrinks.map(_.value)
          shrinks.distinct.length shouldEqual shrinks.length
          if (i.isEmpty)
            shrinks shouldBe empty
          else {
            shrinks should not be empty
            inspectAll(shrinks) { s =>
              i should contain allElementsOf s
              s.size should be < i.size  
            }  
          }
        }
      }
      it("should produce shrinkees following size determined by havingSize method") {
        val aGen= Generator.sortedMapGenerator[Int, String].havingSize(5)
        val shrinkees = aGen.next(SizeParam(1, 0, 1), List(SortedMap(3 -> "three", 99 -> "ninety nine")), Randomizer.default)._1.shrinks.map(_.value)
        all(shrinkees) should have size 5
      }
      it("should produce shrinkees following sizes determined by havingSizesBetween method") {
        val aGen= Generator.sortedMapGenerator[Int, String].havingSizesBetween(2, 5)
        val (v, _, _) = aGen.next(SizeParam(1, 0, 1), List(SortedMap(3 -> "three", 99 -> "ninety nine")), Randomizer.default)
        val shrinkees = v.shrinks.map(_.value)
        if (v.value.size >= 4)
          shrinkees should not be empty
        shrinkees.foreach { shrinkee =>
          assert(shrinkee.size >= 2 && shrinkee.size <= 5) 
        }
      }
      it("should produce shrinkees following sizes determined by havingSizesDeterminedBy method") {
        val aGen= Generator.mapGenerator[Int, String].havingSizesDeterminedBy(s => SizeParam(2, 3, 5))
        val (v, _, _) = aGen.next(SizeParam(1, 0, 1), List(SortedMap(3 -> "three", 99 -> "ninety nine")), Randomizer.default)
        val shrinkees = v.shrinks.map(_.value)
        if (v.value.size >= 4)
          shrinkees should not be empty
        shrinkees.foreach { shrinkee =>
          assert(shrinkee.size >= 2 && shrinkee.size <= 5) 
        }
      }
      it("should return an empty LazyListOrStream when asked to shrink a SortedMap of size 0") {
        val lstGen = implicitly[Generator[SortedMap[PosInt, Int]]]
        val xs = SortedMap.empty[PosInt, Int]
        lstGen.next(SizeParam(1, 0, 1), List(xs), Randomizer.default)._1.shrinks.map(_.value).toSet shouldBe empty
      }
      it("should return an LazyListOrStream that does not repeat canonicals when asked to shrink a SortedMap of size 2 that includes canonicals") {
        val lstGen = implicitly[Generator[SortedMap[PosInt, Int]]]
        val shrinkees = lstGen.next(SizeParam(1, 0, 1), List(SortedMap(PosInt(3) -> 3, PosInt(2) -> 2, PosInt(99) -> 99)), Randomizer.default)._1.shrinks.map(_.value)
        shrinkees.distinct should contain theSameElementsAs shrinkees
      }
      it("should return an LazyListOrStream that does not repeat the passed SortedMap-to-shink even if that SortedMap has a power of 2 length") {
        // Since the last batch of lists produced by the list shrinker start at length 2 and then double in size each time,
        // they lengths will be powers of two: 2, 4, 8, 16, etc... So make sure that if the original length has length 16,
        // for example, that that one doesn't show up in the shrinks output, because it would be the original list-to-shrink.
        val lstGen = implicitly[Generator[SortedMap[PosInt, Int]]]
        val listToShrink: SortedMap[PosInt, Int] = (SortedMap.empty[PosInt, Int] /: (1 to 16)) { (map, n) =>
          map + (PosInt.ensuringValid(n) -> n)
        }
        val shrinkees = lstGen.next(SizeParam(1, 0, 1), List(listToShrink), Randomizer.default)._1.shrinks.map(_.value)
        shrinkees.distinct should not contain listToShrink
      }
      it("should offer a SortedMap generator whose canonical method uses the canonical method of the underlying types") {
        import GeneratorDrivenPropertyChecks._
        val tupleGenerator = Generator.tuple2Generator[PosInt, Int]
        val tupleCanonicalsIt = tupleGenerator.canonicals
        val tupleCanonicals = tupleCanonicalsIt.map(_.value).toList
        val mapGenerator = Generator.sortedMapGenerator[PosInt, Int]
        val mapCanonicalsIt = mapGenerator.canonicals
        val mapCanonicals = mapCanonicalsIt.map(_.value).toList
        mapCanonicals shouldEqual tupleCanonicals.map(i => Map(i))
      }
    }
    it("should be creatable for recursive types") {
      // Based on an example from ScalaCheck: The Definitive Guide
      sealed trait Color extends Product with Serializable
      case object Red extends Color
      case object Green extends Color

      sealed trait Shape extends Product with Serializable { def color: Color }
      case class Line(val color: Color) extends Shape
      case class Circle(val color: Color) extends Shape
      case class Box(val color: Color, boxed: Shape) extends Shape

      import CommonGenerators.{evenly, specificValues}
      val genColor = specificValues(Red, Green)
      val genLine = for { color <- genColor } yield Line(color)
      val genCircle = for { color <- genColor } yield Circle(color)
      
      // SKIP-DOTTY-START
      """
      lazy val genShape = evenly(genLine, genCircle, genBox)
      lazy val genBox: Generator[Box] = for {
        color <- genColor
        shape <- genShape
      } yield Box(color, shape)
      """ should compile
      // SKIP-DOTTY-END
      //DOTTY-ONLY """
      //DOTTY-ONLY lazy val genShape = evenly[Shape](genLine, genCircle, genBox)
      //DOTTY-ONLY lazy val genBox: Generator[Box] = for {
      //DOTTY-ONLY   color <- genColor
      //DOTTY-ONLY   shape <- genShape
      //DOTTY-ONLY } yield Box(color, shape)
      //DOTTY-ONLY """ should compile
    }
  }
}

