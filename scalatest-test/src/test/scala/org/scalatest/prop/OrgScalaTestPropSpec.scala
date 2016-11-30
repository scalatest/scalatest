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
import org.scalatest.WordSpec
import org.scalatest.Matchers
import org.scalatest.exceptions.TestFailedException
import scala.annotation.tailrec

class OrgScalaTestPropSpec extends WordSpec with Matchers {
  "The org.scalatest.prop companion object" should {
    // Maybe in prop.intsBetween a la forAll (intsBetween(0, 10)) { ... }
    "offer an intsBetween method" that {
      "produces Ints between min and max" in {

        import org.scalatest.prop.GeneratorDrivenPropertyChecks._

        val minMaxPairs: Generator[(Int, Int)] = 
          for {
            min <- org.scalatest.prop.intsBetween(Int.MinValue, Int.MaxValue - 1)
            max <- org.scalatest.prop.intsBetween(min, Int.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[Int] = org.scalatest.prop.intsBetween(min, max) 
          val samples = minMaxGen.samples(10)
          import org.scalatest.Inspectors._
          forAll (samples) { i =>
            i should be >= min
            i should be <= max
          }
        }
      }
      "returns a generator whose initEdges method includes min and max" in {

        import org.scalatest.prop.GeneratorDrivenPropertyChecks._

        val minMaxPairs: Generator[(Int, Int)] = 
          for {
            min <- org.scalatest.prop.intsBetween(Int.MinValue, Int.MaxValue - 1)
            max <- org.scalatest.prop.intsBetween(min, Int.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[Int] = org.scalatest.prop.intsBetween(min, max) 
          val (edges, _) = minMaxGen.initEdges(100, Randomizer.default)
          edges should (have length 1 or have length 2)
          edges should contain (min)
          edges should contain (max)
        }
      }
    }
    "offer a values method" that {
      "returns a generator that produces from a given set of objects for any type T" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val values123 = org.scalatest.prop.values(1, 2, 3)
        // forAll (values123) { x => x should be oneOf (1, 2, 3) }
        forAll (values123) { x => x should (be (1) or be (2) or be (3)) }
        forAll (values("nice", "warm", "fireplace")) { x =>
          x should (be ("nice") or be ("warm") or be ("fireplace"))
        }
      }
      "requires at least two values be passed to it" in {
        """values()""" shouldNot compile
        """values(1)""" shouldNot compile
        """values(1, 2)""" should compile
      }
    }
    def samplesForGen[T](genOfT: Generator[T], desiredLength: PosInt, originalRnd: Randomizer): List[T] = {         
      @tailrec                                       
      def samplesLoop(count: Int, rnd: Randomizer, acc: List[T]): List[T] = {
        if (count == desiredLength.value) acc
        else {
          val (size, nextRnd) = rnd.chooseInt(1, 100)
          val (value, _, nextNextRnd) = genOfT.next(size, Nil, rnd) 
          samplesLoop(count + 1, nextNextRnd, value :: acc)
        } 
      }
      samplesLoop(100, originalRnd, Nil)
    }

    "offer a bytes method" that {
      "returns the default implicit generator that produces arbitrary Bytes" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[Byte]]
        val namedGen = org.scalatest.prop.bytes
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples shouldEqual namedGenSamples
      }
    }
    "offer a shorts method" that {
      "returns the default implicit generator that produces arbitrary Shorts" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[Short]]
        val namedGen = org.scalatest.prop.shorts
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples shouldEqual namedGenSamples
      }
    }
    "offer an ints method" that {
      "returns the default implicit generator that produces arbitrary Ints" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[Int]]
        val namedGen = org.scalatest.prop.ints
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples shouldEqual namedGenSamples
      }
    }
    "offer a longs method" that {
      "returns the default implicit generator that produces arbitrary Longs" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[Long]]
        val namedGen = org.scalatest.prop.longs
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples shouldEqual namedGenSamples
      }
    }
    "offer a chars method" that {
      "returns the default implicit generator that produces arbitrary Chars" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[Char]]
        val namedGen = org.scalatest.prop.chars
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples shouldEqual namedGenSamples
      }
    }
    "offer a floats method" that {
      "returns the default implicit generator that produces arbitrary Floats" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[Float]]
        val namedGen = org.scalatest.prop.floats
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples shouldEqual namedGenSamples
      }
    }
    "offer a doubles method" that {
      "returns the default implicit generator that produces arbitrary Doubles" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[Double]]
        val namedGen = org.scalatest.prop.doubles
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples shouldEqual namedGenSamples
      }
    }
    "offer a strings method" that {
      "returns the default implicit generator that produces arbitrary Strings" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[String]]
        val namedGen = org.scalatest.prop.strings
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples shouldEqual namedGenSamples
      }
    }
    "offer a lists method" that {
      "returns the default implicit generator that produces arbitrary Lists" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[List[Int]]]
        val namedGen = org.scalatest.prop.lists[Int]
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples shouldEqual namedGenSamples
      }
    }
    "offer a posInts method" that {
      "returns the default implicit generator that produces arbitrary PosInts" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[PosInt]]
        val namedGen = org.scalatest.prop.posInts
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples shouldEqual namedGenSamples
      }
    }
    "offer a posZInts method" that {
      "returns the default implicit generator that produces arbitrary PosZInts" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[PosZInt]]
        val namedGen = org.scalatest.prop.posZInts
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples shouldEqual namedGenSamples
      }
    }
    "offer a posLongs method" that {
      "returns the default implicit generator that produces arbitrary PosLongs" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[PosLong]]
        val namedGen = org.scalatest.prop.posLongs
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples shouldEqual namedGenSamples
      }
    }
    "offer a posZLongs method" that {
      "returns the default implicit generator that produces arbitrary PosZLongs" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[PosZLong]]
        val namedGen = org.scalatest.prop.posZLongs
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples shouldEqual namedGenSamples
      }
    }
    "offer a posFloats method" that {
      "returns the default implicit generator that produces arbitrary PosFloats" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[PosFloat]]
        val namedGen = org.scalatest.prop.posFloats
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples shouldEqual namedGenSamples
      }
    }
    "offer a posZFloats method" that {
      "returns the default implicit generator that produces arbitrary PosZFloats" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[PosZFloat]]
        val namedGen = org.scalatest.prop.posZFloats
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples shouldEqual namedGenSamples
      }
    }
    "offer a posDoubles method" that {
      "returns the default implicit generator that produces arbitrary PosDoubles" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[PosDouble]]
        val namedGen = org.scalatest.prop.posDoubles
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples shouldEqual namedGenSamples
      }
    }
    "offer a posZDoubles method" that {
      "returns the default implicit generator that produces arbitrary PosZDoubles" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[PosZDouble]]
        val namedGen = org.scalatest.prop.posZDoubles
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples shouldEqual namedGenSamples
      }
    }
    "offer a posIntValues method" that {
      "returns the default implicit generator that produces arbitrary positive Ints" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[PosInt]]
        val namedGen = org.scalatest.prop.posIntValues
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges.map(_.value) shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples.map(_.value) shouldEqual namedGenSamples
      }
    }
    "offer a posZIntValues method" that {
      "returns the default implicit generator that produces arbitrary zero and positive Ints" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[PosZInt]]
        val namedGen = org.scalatest.prop.posZIntValues
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges.map(_.value) shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples.map(_.value) shouldEqual namedGenSamples
      }
    }
    "offer a posLongValues method" that {
      "returns the default implicit generator that produces arbitrary positive Longs" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[PosLong]]
        val namedGen = org.scalatest.prop.posLongValues
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges.map(_.value) shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples.map(_.value) shouldEqual namedGenSamples
      }
    }
    "offer a posZLongValues method" that {
      "returns the default implicit generator that produces arbitrary zero and positive Longs" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[PosZLong]]
        val namedGen = org.scalatest.prop.posZLongValues
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges.map(_.value) shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples.map(_.value) shouldEqual namedGenSamples
      }
    }
    "offer a posFloatValues method" that {
      "returns the default implicit generator that produces arbitrary positive Floats" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[PosFloat]]
        val namedGen = org.scalatest.prop.posFloatValues
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges.map(_.value) shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples.map(_.value) shouldEqual namedGenSamples
      }
    }
    "offer a posZFloatValues method" that {
      "returns the default implicit generator that produces arbitrary zero and positive Floats" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[PosZFloat]]
        val namedGen = org.scalatest.prop.posZFloatValues
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges.map(_.value) shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples.map(_.value) shouldEqual namedGenSamples
      }
    }
    "offer a posDoubleValues method" that {
      "returns the default implicit generator that produces arbitrary positive Doubles" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[PosDouble]]
        val namedGen = org.scalatest.prop.posDoubleValues
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges.map(_.value) shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples.map(_.value) shouldEqual namedGenSamples
      }
    }
    "offer a posZDoubleValues method" that {
      "returns the default implicit generator that produces arbitrary zero and positive Doubles" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[PosZDouble]]
        val namedGen = org.scalatest.prop.posZDoubleValues
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges.map(_.value) shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples.map(_.value) shouldEqual namedGenSamples
      }
    }
    "offer a tuple2s method" that {
      "returns the default implicit generator that produces arbitrary Tuple2s" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[(String, Int)]]
        val namedGen = org.scalatest.prop.tuple2s[String, Int]
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples shouldEqual namedGenSamples
      }
    }
    "offer a function0s method" that {
      "returns the default implicit generator that produces arbitrary Function0s" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[() => Int]]
        val namedGen = org.scalatest.prop.function0s[Int]
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples shouldEqual namedGenSamples
      }
    }
  }
}

