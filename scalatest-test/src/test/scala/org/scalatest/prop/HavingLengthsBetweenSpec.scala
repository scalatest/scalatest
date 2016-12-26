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

class HavingLengthsBetweenSpec extends FunSpec with Matchers {
  describe("A HavingLengthsBetween Generator for Lists") {
    describe("where from is 0") {
      it("should offer a List[T] generator that returns a List[T] whose length equals the passed size") {
  
        import Generator._
        import CommonGenerators.lists
        val gen = lists[Int].havingLengthsBetween(0, 88)
  
        val (l1, _, r1) = gen.next(size = 0, edges = Nil, rnd = Randomizer(100))
        l1.length shouldBe 0
  
        val (l2, _, r2) = gen.next(size = 3, edges = Nil, rnd = r1)
        l2.length shouldBe 3
  
        val (l3, _, r3) = gen.next(size = 38, edges = Nil, rnd = r2)
        l3.length shouldBe 38
  
        val (l4, _, r4) = gen.next(size = 88, edges = Nil, rnd = r3)
        l4.length shouldBe 88
  
        an [IllegalArgumentException] should be thrownBy {
          gen.next(size = -1, edges = Nil, rnd = Randomizer(100))
        }
      }
      it("should not exhibit this bug in List shrinking") {
        import CommonGenerators.lists
        val lstGen = lists[List[Int]].havingLengthsBetween(0, 77)
        val xss = List(List(100, 200, 300, 400, 300))
        lstGen.shrink(xss, Randomizer.default)._1.toList should not contain xss
      }
      it("should shrink Lists using strategery") {
        import GeneratorDrivenPropertyChecks._
        val intGenerator = Generator.intGenerator
        val (intCanonicalsIt, _) = intGenerator.canonicals(Randomizer.default)
        val intCanonicals = intCanonicalsIt.toList
        forAll (lists[Int].havingLengthsBetween(0, 78)) { (xs: List[Int]) =>
          val generator = lists[Int]
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
        import CommonGenerators.lists
        val lstGen = lists[Int].havingLengthsBetween(0, 99)
        val xs = List.empty[Int]
        lstGen.shrink(xs, Randomizer.default)._1.toList shouldBe empty
      }
      it("should return an Iterator of the canonicals excluding the given values to shrink when asked to shrink a List of size 1") {
        import CommonGenerators.lists
        val lstGen = lists[Int].havingLengthsBetween(0, 88)
        val canonicalLists = List(0, 1, -1, 2, -2, 3, -3).map(i => List(i))
        val expectedLists = List(List.empty[Int]) ++ canonicalLists
        val nonCanonical = List(99)
        lstGen.shrink(nonCanonical, Randomizer.default)._1.toList should contain theSameElementsAs expectedLists
        val canonical = List(3)
        // Ensure 3 (an Int canonical value) does not show up twice in the output
        lstGen.shrink(canonical, Randomizer.default)._1.toList should contain theSameElementsAs expectedLists
      }
      it("should return an Iterator that does not repeat canonicals when asked to shrink a List of size 2 that includes canonicals") {
        import CommonGenerators.lists
        val lstGen = lists[Int].havingLengthsBetween(0, 66)
        val shrinkees = lstGen.shrink(List(3, 99), Randomizer.default)._1.toList
        shrinkees.distinct should contain theSameElementsAs shrinkees
      }
      it("should return an Iterator that does not repeat the passed list-to-shink even if that list has a power of 2 length") {
        // Since the last batch of lists produced by the list shrinker start at length 2 and then double in size each time,
        // they lengths will be powers of two: 2, 4, 8, 16, etc... So make sure that if the original length has length 16,
        // for example, that that one doesn't show up in the shrinks output, because it would be the original list-to-shrink.
        import CommonGenerators.lists
        val lstGen = lists[Int].havingLengthsBetween(0, 77)
        val listToShrink = List.fill(16)(99)
        val shrinkees = lstGen.shrink(listToShrink, Randomizer.default)._1.toList
        shrinkees.distinct should not contain listToShrink
      }
      it("should offer a list generator whose canonical method uses the canonical method of the underlying T if min is 0 or 1") {
        import GeneratorDrivenPropertyChecks._
        val intGenerator = Generator.intGenerator
        val (intCanonicalsIt, _) = intGenerator.canonicals(Randomizer.default)
        val intCanonicals = intCanonicalsIt.toList
        val listOfIntGenerator = lists[Int].havingLengthsBetween(0, 50)
        val (listOfIntCanonicalsIt, _) = listOfIntGenerator.canonicals(Randomizer.default)
        val listOfIntCanonicals = listOfIntCanonicalsIt.toList
        listOfIntCanonicals shouldEqual intCanonicals.map(i => List(i))
      }
    }
    describe("where from is 1") {
      it("should offer a list generator whose canonical method uses the canonical method of the underlying T if min is 0 or 1") {
        import GeneratorDrivenPropertyChecks._
        val intGenerator = Generator.intGenerator
        val (intCanonicalsIt, _) = intGenerator.canonicals(Randomizer.default)
        val intCanonicals = intCanonicalsIt.toList
        val listOfIntGenerator = lists[Int].havingLengthsBetween(1, 50)
        val (listOfIntCanonicalsIt, _) = listOfIntGenerator.canonicals(Randomizer.default)
        val listOfIntCanonicals = listOfIntCanonicalsIt.toList
        listOfIntCanonicals shouldEqual intCanonicals.map(i => List(i))
      }
    }
  }
}

