/*
 * Copyright 2001-2016 Artima, Inc.
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
import org.scalatest.Resources

class OrgScalaTestPropSpec extends WordSpec with Matchers {
  "The org.scalatest.prop companion object" should {
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
    "offer a posIntsBetween method" that {
       val PosIntMaxValueMinusOne = PosInt(2147483646)
      "produces PosInts between min and max" in {

        import org.scalatest.prop.GeneratorDrivenPropertyChecks._

        val minMaxPairs: Generator[(PosInt, PosInt)] = 
          for {
            min <- org.scalatest.prop.posIntsBetween(PosInt.MinValue, PosIntMaxValueMinusOne)
            max <- org.scalatest.prop.posIntsBetween(min, PosInt.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[PosInt] = org.scalatest.prop.posIntsBetween(min, max) 
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

        val minMaxPairs: Generator[(PosInt, PosInt)] = 
          for {
            min <- org.scalatest.prop.posIntsBetween(PosInt.MinValue, PosIntMaxValueMinusOne)
            max <- org.scalatest.prop.posIntsBetween(min, PosInt.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[PosInt] = org.scalatest.prop.posIntsBetween(min, max) 
          val (edges, _) = minMaxGen.initEdges(100, Randomizer.default)
          edges should (have length 1 or have length 2)
          edges should contain (min)
          edges should contain (max)
        }
      }
    }
    "offer a specificValues method" that {
      "returns a generator that produces from a given set of specific objects for any type T" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val values123 = org.scalatest.prop.specificValues(1, 2, 3)
        // forAll (values123) { x => x should be oneOf (1, 2, 3) }
        forAll (values123) { x => x should (be (1) or be (2) or be (3)) }
        forAll (specificValues("nice", "warm", "fireplace")) { x =>
          x should (be ("nice") or be ("warm") or be ("fireplace"))
        }
      }
      "requires at least two values be passed to it" in {
        """specificValues()""" shouldNot compile
        """specificValues(1)""" shouldNot compile
        """specificValues(1, 2)""" should compile
      }
    }
    "offer a specificValue method" that {
      "returns a generator that produces from a given single specific object for any type T" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val specificValue42 = org.scalatest.prop.specificValue(42)
        forAll (specificValue42) { x => x shouldBe (42) }
        forAll (specificValue("nice")) { x =>
          x should (be ("nice"))
        }
      }
    }
    "offer a frequency method takes a Map of PosInt weights to generators and produces a generator" that {
      "returns values from each specific generator with a probability determined by the weights" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        // import org.scalatest.prop.specificValue
        val evenInts = for { i <- intsBetween(2, 100000) } yield 2 * i
        val oddInts = for { i <- intsBetween(0, 100000) } yield 2 * i + 1
        val oddAndEvenInts =
          frequency(
            1 -> oddInts,
            2 -> evenInts,
            4 -> specificValue(0)
          )

        val classification: Classification =
          classify(10000, oddAndEvenInts) {
            case 0 => "zero"
            case i if i % 2 == 0 => "even"
            case _ => "odd"
          }
 
        val percentages: Map[String, PosZInt] = classification.percentages

        val totalWeight = 1 + 2 + 4
        percentages("zero").value shouldBe (4.0 / totalWeight * 100).toInt +- 1
        percentages("even").value shouldBe (2.0 / totalWeight * 100).toInt +- 1
        percentages("odd").value shouldBe (1.0 / totalWeight * 100).toInt +- 1
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
      "returns a type that also offers a havingLength method that provides a generator for lists of a specific length" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        forAll (posIntsBetween(1, 100)) { len => 
          forAll (lists[Int].havingLength(len)) { xs => xs.length shouldEqual len.value }
        }
      }
      "returns a type that also offers a havingLengthsBetween method that provides a generator for lists of a specific length" in {

        import org.scalatest.prop.GeneratorDrivenPropertyChecks._

        val minsAndMaxes: Generator[(PosZInt, PosZInt)] =
          for {
            min <- posZIntsBetween(0, 99)
            max <- posZIntsBetween(min.ensuringValid(_ + 1), 100)
          } yield (min, max)

        forAll (minsAndMaxes) { case (min, max) => 
          forAll (lists[Int].havingLengthsBetween(min, max)) { xs => xs.length should (be >= min.value and be <= max.value) }
        }

        the [IllegalArgumentException] thrownBy {
          lists[Int].havingLengthsBetween(2, 1)
        } should have message "requirement failed: " + Resources.fromGreaterThanToHavingLengthsBetween(PosZInt(2), PosZInt(1))
        the [IllegalArgumentException] thrownBy {
          lists[Int].havingLengthsBetween(1, 1)
        } should have message "requirement failed: " + Resources.fromEqualToToHavingLengthsBetween(PosZInt(1))
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
    "offer a tuple3s method" that {
      "returns the default implicit generator that produces arbitrary Tuple3s" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[(String, Int, Long)]]
        val namedGen = org.scalatest.prop.tuple3s[String, Int, Long]
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
    "offer a function1s method" that {
      "should use the implicit provider that uses hashCode to tweak a seed and has a pretty toString" in {
        val implicitGen = implicitly[Generator[Long => Int]]
        val namedGen = org.scalatest.prop.function1s[Long, Int]
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples shouldEqual namedGenSamples
      }
    }
    "offer a function2s method" that {
      "should use the implicit provider that uses hashCode to tweak a seed and has a pretty toString" in {
        val implicitGen = implicitly[Generator[(Long, String) => Int]]
        val namedGen = org.scalatest.prop.function2s[Long, String, Int]
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples shouldEqual namedGenSamples
      }
    }
    "offer a classify method" that {
      "takes a partial function and returns an object containing a Map of String classifications to Int totals" in {
        val classification: Classification =
          classify(1000, ints) {
            case 0 => "zero"
            case i if i > 0 => "positive"
            case _ => "negative"
          }
 
        val totals: Map[String, PosZInt] = classification.totals
        totals should have size 3
        totals should (contain key "zero" and contain key "positive" and contain key "negative")
      }
      "allows users to leave some values unclassified" in {
        val classification: Classification =
          classify(1000, ints) {
            case 0 => "zero"
            case i if i > 0 => "positive"
            // case _ => "negative" will leave negative ones unclassified
          }
 
        val totals: Map[String, PosZInt] = classification.totals
        totals should have size 2
        totals should (contain key "zero" and contain key "positive")
      }
      "offer a method that gives back the total number of trials performed" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        forAll (posIntsBetween(100, 1000)) { (count: PosInt) =>
          val fullClassification: Classification =
            classify(count, ints) {
              case 0 => "zero"
              case i if i > 0 => "positive"
              case _ => "negative"
            }
          fullClassification.totalGenerated shouldEqual count
 
          val partialClassification: Classification =
            classify(count, ints) {
              case 0 => "zero"
              case i if i > 0 => "positive"
              // case _ => "negative" will leave negative ones unclassified
            }
          partialClassification.totalGenerated shouldEqual count
        }
      }
      "offers a method returning a Map of String classification names to Double portions (between 0.0 and 1.0)" in {
        val classification: Classification =
          classify(1000, ints) {
            case 0 => "zero"
            case i if i > 0 => "positive"
            case _ => "negative"
          }
 
        val portions: Map[String, Double] = classification.portions
        portions should have size 3
        portions should (contain key "zero" and contain key "positive" and contain key "negative")
        portions.values.sum shouldEqual 1.0
      }
      "offers a method returning a Map of String classification names to Int percentages (between 0 and 100)" in {
        val classification: Classification =
          classify(1000, ints) {
            case 0 => "zero"
            case i if i > 0 => "positive"
            case _ => "negative"
          }
 
        val percentages: Map[String, PosZInt] = classification.percentages
        percentages should have size 3
        percentages should (contain key "zero" and contain key "positive" and contain key "negative")
        percentages.values.map(_.value).sum shouldEqual 100 +- 1
      }
      "has a toString method that prints percentages and classifications on individual lines" in {
        val classification: Classification =
          classify(1000, ints) {
            case 0 => "zero"
            case i if i > 0 => "positive"
            case _ => "negative"
          }
 
        val output: String = classification.toString
        output.count(_ == '\n') shouldEqual 2
        val lines: List[String] = output.split('\n').toList
        lines.length shouldEqual 3
        all (lines) should contain ('%')
        exactly (1, lines) should include ("zero")
        exactly (1, lines) should include ("positive")
        exactly (1, lines) should include ("negative")
      }
    }
    "offer a gen method" that {
      "produces generators given construct and deconstruct functions for 1 type" in {
        case class Person(age: Int)
        val persons = gen(Person) { p => p.age } (posZIntValues)
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        forAll (persons) { case Person(ag) => ag should be >= 0 } // A contrived property check to do something with the generator
      }
      "produces generators given construct and deconstruct functions for 2 types" in {
        case class Person(name: String, age: Int)
        val persons = gen(Person) { p =>
          (p.name, p.age)
        } (strings, posZIntValues)
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        forAll (persons) { case Person(_, ag) => ag should be >= 0 } // A contrived property check to do something with the generator
      }
      "produces generators given construct and deconstruct functions for 3 types" in {
        case class Person(name: String, age: Int, attr3: Long)
        val persons = gen(Person) { p =>
          (p.name, p.age, p.attr3)
        } (strings, posZIntValues, posZLongValues)
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        forAll (persons) { case Person(_, ag, attr3) =>
          ag should be >= 0
          attr3 should be >= 0L
        } // A contrived property check to do something with the generator
      }
      "produces generators given construct and deconstruct functions for 4 types" in {
        case class Person(name: String, age: Int, attr3: Long, attr4: Double)
        val persons = gen(Person) { p =>
          (p.name, p.age, p.attr3, p.attr4)
        } (strings, posZIntValues, posZLongValues, posZDoubleValues)
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        forAll (persons) { case Person(_, ag, attr3, attr4) =>
          ag should be >= 0
          attr3 should be >= 0L
          attr4 should be >= 0.0
        } // A contrived property check to do something with the generator
      }
      "produces generators given construct and deconstruct functions for 5 types" in {
        case class Person(name: String, age: Int, attr3: Long, attr4: Double, attr5: Float)
        val persons = gen(Person) { p =>
          (p.name, p.age, p.attr3, p.attr4, p.attr5)
        } (strings, posZIntValues, posZLongValues, posZDoubleValues, posZFloatValues)
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        forAll (persons) { case Person(_, ag, attr3, attr4, attr5) =>
          ag should be >= 0
          attr3 should be >= 0L
          attr4 should be >= 0.0
          attr5 should be >= 0.0f
        } // A contrived property check to do something with the generator
      }
      "produces generators given construct and deconstruct functions for 6 types" in {
        case class Person(name: String, age: Int, attr3: Long, attr4: Double, attr5: Float, attr6: Int)
        val persons = gen(Person) { p =>
          (p.name, p.age, p.attr3, p.attr4, p.attr5, p.attr6)
        } (strings, posZIntValues, posZLongValues, posZDoubleValues, posZFloatValues, posZIntValues)
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        forAll (persons) { case Person(_, ag, attr3, attr4, attr5, attr6) =>
          ag should be >= 0
          attr3 should be >= 0L
          attr4 should be >= 0.0
          attr5 should be >= 0.0f
          attr6 should be >= 0
        } // A contrived property check to do something with the generator
      }
      "produces generators given construct and deconstruct functions for 7 types" in {
        case class Person(name: String, age: Int, attr3: Long, attr4: Double, attr5: Float, attr6: Int, attr7: Long)
        val persons = gen(Person) { p =>
          (p.name, p.age, p.attr3, p.attr4, p.attr5, p.attr6, p.attr7)
        } (strings, posZIntValues, posZLongValues, posZDoubleValues, posZFloatValues, posZIntValues, posZLongValues)
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        forAll (persons) { case Person(_, ag, attr3, attr4, attr5, attr6, attr7) =>
          ag should be >= 0
          attr3 should be >= 0L
          attr4 should be >= 0.0
          attr5 should be >= 0.0f
          attr6 should be >= 0
          attr7 should be >= 0L
        } // A contrived property check to do something with the generator
      }
      "produces generators given construct and deconstruct functions for 8 types" in {
        case class Person(name: String, age: Int, attr3: Long, attr4: Double, attr5: Float, attr6: Int, attr7: Long, attr8: Double)
        val persons = gen(Person) { p =>
          (p.name, p.age, p.attr3, p.attr4, p.attr5, p.attr6, p.attr7, p.attr8)
        } (strings, posZIntValues, posZLongValues, posZDoubleValues, posZFloatValues, posZIntValues, posZLongValues, posZDoubleValues)
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        forAll (persons) { case Person(_, ag, attr3, attr4, attr5, attr6, attr7, attr8) =>
          ag should be >= 0
          attr3 should be >= 0L
          attr4 should be >= 0.0
          attr5 should be >= 0.0f
          attr6 should be >= 0
          attr7 should be >= 0L
          attr8 should be >= 0.0
        } // A contrived property check to do something with the generator
      }
      "produces generators given construct and deconstruct functions for 9 types" in {
        case class Person(name: String, age: Int, attr3: Long, attr4: Double, attr5: Float, attr6: Int, attr7: Long, attr8: Double, attr9: Float)
        val persons = gen(Person) { p =>
          (p.name, p.age, p.attr3, p.attr4, p.attr5, p.attr6, p.attr7, p.attr8, p.attr9)
        } (strings, posZIntValues, posZLongValues, posZDoubleValues, posZFloatValues, posZIntValues, posZLongValues, posZDoubleValues, posZFloatValues)
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        forAll (persons) { case Person(_, ag, attr3, attr4, attr5, attr6, attr7, attr8, attr9) =>
          ag should be >= 0
          attr3 should be >= 0L
          attr4 should be >= 0.0
          attr5 should be >= 0.0f
          attr6 should be >= 0
          attr7 should be >= 0L
          attr8 should be >= 0.0
          attr9 should be >= 0.0f
        } // A contrived property check to do something with the generator
      }
      "produces generators given construct and deconstruct functions for 10 types" in {
        case class Person(name: String, age: Int, attr3: Long, attr4: Double, attr5: Float, attr6: Int, attr7: Long, attr8: Double, attr9: Float, attr10: Int)
        val persons = gen(Person) { p =>
          (p.name, p.age, p.attr3, p.attr4, p.attr5, p.attr6, p.attr7, p.attr8, p.attr9, p.attr10)
        } (strings, posZIntValues, posZLongValues, posZDoubleValues, posZFloatValues, posZIntValues, posZLongValues, posZDoubleValues, posZFloatValues, posZIntValues)
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        forAll (persons) { case Person(_, ag, attr3, attr4, attr5, attr6, attr7, attr8, attr9, attr10) =>
          ag should be >= 0
          attr3 should be >= 0L
          attr4 should be >= 0.0
          attr5 should be >= 0.0f
          attr6 should be >= 0
          attr7 should be >= 0L
          attr8 should be >= 0.0
          attr9 should be >= 0.0f
          attr10 should be >= 0
        } // A contrived property check to do something with the generator
      }
      "produces generators given construct and deconstruct functions for 11 types" in {
        case class Person(name: String, age: Int, attr3: Long, attr4: Double, attr5: Float, attr6: Int, attr7: Long, attr8: Double, attr9: Float, attr10: Int, attr11: Long)
        val persons = gen(Person) { p =>
          (p.name, p.age, p.attr3, p.attr4, p.attr5, p.attr6, p.attr7, p.attr8, p.attr9, p.attr10, p.attr11)
        } (strings, posZIntValues, posZLongValues, posZDoubleValues, posZFloatValues, posZIntValues, posZLongValues, posZDoubleValues, posZFloatValues, posZIntValues, posZLongValues)
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        forAll (persons) { case Person(_, ag, attr3, attr4, attr5, attr6, attr7, attr8, attr9, attr10, attr11) =>
          ag should be >= 0
          attr3 should be >= 0L
          attr4 should be >= 0.0
          attr5 should be >= 0.0f
          attr6 should be >= 0
          attr7 should be >= 0L
          attr8 should be >= 0.0
          attr9 should be >= 0.0f
          attr10 should be >= 0
          attr11 should be >= 0L
        } // A contrived property check to do something with the generator
      }
      "produces generators given construct and deconstruct functions for 12 types" in {
        case class Person(name: String, age: Int, attr3: Long, attr4: Double, attr5: Float, attr6: Int, attr7: Long, attr8: Double, attr9: Float, attr10: Int, attr11: Long, attr12: Double)
        val persons = gen(Person) { p =>
          (p.name, p.age, p.attr3, p.attr4, p.attr5, p.attr6, p.attr7, p.attr8, p.attr9, p.attr10, p.attr11, p.attr12)
        } (strings, posZIntValues, posZLongValues, posZDoubleValues, posZFloatValues, posZIntValues, posZLongValues, posZDoubleValues, posZFloatValues, posZIntValues, posZLongValues, posZDoubleValues)
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        forAll (persons) { case Person(_, ag, attr3, attr4, attr5, attr6, attr7, attr8, attr9, attr10, attr11, attr12) =>
          ag should be >= 0
          attr3 should be >= 0L
          attr4 should be >= 0.0
          attr5 should be >= 0.0f
          attr6 should be >= 0
          attr7 should be >= 0L
          attr8 should be >= 0.0
          attr9 should be >= 0.0f
          attr10 should be >= 0
          attr11 should be >= 0L
          attr12 should be >= 0.0
        } // A contrived property check to do something with the generator
      }
      "produces generators given construct and deconstruct functions for 13 types" in {
        case class Person(name: String, age: Int, attr3: Long, attr4: Double, attr5: Float, attr6: Int, attr7: Long, attr8: Double, attr9: Float, attr10: Int, attr11: Long, attr12: Double, attr13: Float)
        val persons = gen(Person) { p =>
          (p.name, p.age, p.attr3, p.attr4, p.attr5, p.attr6, p.attr7, p.attr8, p.attr9, p.attr10, p.attr11, p.attr12, p.attr13)
        } (strings, posZIntValues, posZLongValues, posZDoubleValues, posZFloatValues, posZIntValues, posZLongValues, posZDoubleValues, posZFloatValues, posZIntValues, posZLongValues, posZDoubleValues, posZFloatValues)
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        forAll (persons) { case Person(_, ag, attr3, attr4, attr5, attr6, attr7, attr8, attr9, attr10, attr11, attr12, attr13) =>
          ag should be >= 0
          attr3 should be >= 0L
          attr4 should be >= 0.0
          attr5 should be >= 0.0f
          attr6 should be >= 0
          attr7 should be >= 0L
          attr8 should be >= 0.0
          attr9 should be >= 0.0f
          attr10 should be >= 0
          attr11 should be >= 0L
          attr12 should be >= 0.0
          attr13 should be >= 0.0f
        } // A contrived property check to do something with the generator
      }
      "produces generators given construct and deconstruct functions for 14 types" in {
        case class Person(name: String, age: Int, attr3: Long, attr4: Double, attr5: Float, attr6: Int, attr7: Long, attr8: Double, attr9: Float, attr10: Int, attr11: Long, attr12: Double, attr13: Float, attr14: Int)
        val persons = gen(Person) { p =>
          (p.name, p.age, p.attr3, p.attr4, p.attr5, p.attr6, p.attr7, p.attr8, p.attr9, p.attr10, p.attr11, p.attr12, p.attr13, p.attr14)
        } (strings, posZIntValues, posZLongValues, posZDoubleValues, posZFloatValues, posZIntValues, posZLongValues, posZDoubleValues, posZFloatValues, posZIntValues, posZLongValues, posZDoubleValues, posZFloatValues, posZIntValues)
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        forAll (persons) { case Person(_, ag, attr3, attr4, attr5, attr6, attr7, attr8, attr9, attr10, attr11, attr12, attr13, attr14) =>
          ag should be >= 0
          attr3 should be >= 0L
          attr4 should be >= 0.0
          attr5 should be >= 0.0f
          attr6 should be >= 0
          attr7 should be >= 0L
          attr8 should be >= 0.0
          attr9 should be >= 0.0f
          attr10 should be >= 0
          attr11 should be >= 0L
          attr12 should be >= 0.0
          attr13 should be >= 0.0f
          attr14 should be >= 0
        } // A contrived property check to do something with the generator
      }
      "produces generators given construct and deconstruct functions for 15 types" in {
        case class Person(name: String, age: Int, attr3: Long, attr4: Double, attr5: Float, attr6: Int, attr7: Long, attr8: Double, attr9: Float, attr10: Int, attr11: Long, attr12: Double, attr13: Float, attr14: Int, attr15: Long)
        val persons = gen(Person) { p =>
          (p.name, p.age, p.attr3, p.attr4, p.attr5, p.attr6, p.attr7, p.attr8, p.attr9, p.attr10, p.attr11, p.attr12, p.attr13, p.attr14, p.attr15)
        } (strings, posZIntValues, posZLongValues, posZDoubleValues, posZFloatValues, posZIntValues, posZLongValues, posZDoubleValues, posZFloatValues, posZIntValues, posZLongValues, posZDoubleValues, posZFloatValues, posZIntValues, posZLongValues)
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        forAll (persons) { case Person(_, ag, attr3, attr4, attr5, attr6, attr7, attr8, attr9, attr10, attr11, attr12, attr13, attr14, attr15) =>
          ag should be >= 0
          attr3 should be >= 0L
          attr4 should be >= 0.0
          attr5 should be >= 0.0f
          attr6 should be >= 0
          attr7 should be >= 0L
          attr8 should be >= 0.0
          attr9 should be >= 0.0f
          attr10 should be >= 0
          attr11 should be >= 0L
          attr12 should be >= 0.0
          attr13 should be >= 0.0f
          attr14 should be >= 0
          attr15 should be >= 0L
        } // A contrived property check to do something with the generator
      }
      "produces generators given construct and deconstruct functions for 16 types" in {
        case class Person(name: String, age: Int, attr3: Long, attr4: Double, attr5: Float, attr6: Int, attr7: Long, attr8: Double, attr9: Float, attr10: Int, attr11: Long, attr12: Double, attr13: Float, attr14: Int, attr15: Long,
                          attr16: Double)
        val persons = gen(Person) { p =>
          (p.name, p.age, p.attr3, p.attr4, p.attr5, p.attr6, p.attr7, p.attr8, p.attr9, p.attr10, p.attr11, p.attr12, p.attr13, p.attr14, p.attr15, p.attr16)
        } (strings, posZIntValues, posZLongValues, posZDoubleValues, posZFloatValues, posZIntValues, posZLongValues, posZDoubleValues, posZFloatValues, posZIntValues, posZLongValues, posZDoubleValues, posZFloatValues, posZIntValues, posZLongValues,
          posZDoubleValues)
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        forAll (persons) { case Person(_, ag, attr3, attr4, attr5, attr6, attr7, attr8, attr9, attr10, attr11, attr12, attr13, attr14, attr15, attr16) =>
          ag should be >= 0
          attr3 should be >= 0L
          attr4 should be >= 0.0
          attr5 should be >= 0.0f
          attr6 should be >= 0
          attr7 should be >= 0L
          attr8 should be >= 0.0
          attr9 should be >= 0.0f
          attr10 should be >= 0
          attr11 should be >= 0L
          attr12 should be >= 0.0
          attr13 should be >= 0.0f
          attr14 should be >= 0
          attr15 should be >= 0L
          attr16 should be >= 0.0
        } // A contrived property check to do something with the generator
      }
      "produces generators given construct and deconstruct functions for 17 types" in {
        case class Person(name: String, age: Int, attr3: Long, attr4: Double, attr5: Float, attr6: Int, attr7: Long, attr8: Double, attr9: Float, attr10: Int, attr11: Long, attr12: Double, attr13: Float, attr14: Int, attr15: Long,
                          attr16: Double, attr17: Float)
        val persons = gen(Person) { p =>
          (p.name, p.age, p.attr3, p.attr4, p.attr5, p.attr6, p.attr7, p.attr8, p.attr9, p.attr10, p.attr11, p.attr12, p.attr13, p.attr14, p.attr15, p.attr16, p.attr17)
        } (strings, posZIntValues, posZLongValues, posZDoubleValues, posZFloatValues, posZIntValues, posZLongValues, posZDoubleValues, posZFloatValues, posZIntValues, posZLongValues, posZDoubleValues, posZFloatValues, posZIntValues, posZLongValues,
          posZDoubleValues, posZFloatValues)
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        forAll (persons) { case Person(_, ag, attr3, attr4, attr5, attr6, attr7, attr8, attr9, attr10, attr11, attr12, attr13, attr14, attr15, attr16, attr17) =>
          ag should be >= 0
          attr3 should be >= 0L
          attr4 should be >= 0.0
          attr5 should be >= 0.0f
          attr6 should be >= 0
          attr7 should be >= 0L
          attr8 should be >= 0.0
          attr9 should be >= 0.0f
          attr10 should be >= 0
          attr11 should be >= 0L
          attr12 should be >= 0.0
          attr13 should be >= 0.0f
          attr14 should be >= 0
          attr15 should be >= 0L
          attr16 should be >= 0.0
          attr17 should be >= 0.0f
        } // A contrived property check to do something with the generator
      }
      "produces generators given construct and deconstruct functions for 18 types" in {
        case class Person(name: String, age: Int, attr3: Long, attr4: Double, attr5: Float, attr6: Int, attr7: Long, attr8: Double, attr9: Float, attr10: Int, attr11: Long, attr12: Double, attr13: Float, attr14: Int, attr15: Long,
                          attr16: Double, attr17: Float, attr18: Int)
        val persons = gen(Person) { p =>
          (p.name, p.age, p.attr3, p.attr4, p.attr5, p.attr6, p.attr7, p.attr8, p.attr9, p.attr10, p.attr11, p.attr12, p.attr13, p.attr14, p.attr15, p.attr16, p.attr17, p.attr18)
        } (strings, posZIntValues, posZLongValues, posZDoubleValues, posZFloatValues, posZIntValues, posZLongValues, posZDoubleValues, posZFloatValues, posZIntValues, posZLongValues, posZDoubleValues, posZFloatValues, posZIntValues, posZLongValues,
          posZDoubleValues, posZFloatValues, posZIntValues)
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        forAll (persons) { case Person(_, ag, attr3, attr4, attr5, attr6, attr7, attr8, attr9, attr10, attr11, attr12, attr13, attr14, attr15, attr16, attr17, attr18) =>
          ag should be >= 0
          attr3 should be >= 0L
          attr4 should be >= 0.0
          attr5 should be >= 0.0f
          attr6 should be >= 0
          attr7 should be >= 0L
          attr8 should be >= 0.0
          attr9 should be >= 0.0f
          attr10 should be >= 0
          attr11 should be >= 0L
          attr12 should be >= 0.0
          attr13 should be >= 0.0f
          attr14 should be >= 0
          attr15 should be >= 0L
          attr16 should be >= 0.0
          attr17 should be >= 0.0f
          attr18 should be >= 0
        } // A contrived property check to do something with the generator
      }
      "produces generators given construct and deconstruct functions for 19 types" in {
        case class Person(name: String, age: Int, attr3: Long, attr4: Double, attr5: Float, attr6: Int, attr7: Long, attr8: Double, attr9: Float, attr10: Int, attr11: Long, attr12: Double, attr13: Float, attr14: Int, attr15: Long,
                          attr16: Double, attr17: Float, attr18: Int, attr19: Long)
        val persons = gen(Person) { p =>
          (p.name, p.age, p.attr3, p.attr4, p.attr5, p.attr6, p.attr7, p.attr8, p.attr9, p.attr10, p.attr11, p.attr12, p.attr13, p.attr14, p.attr15, p.attr16, p.attr17, p.attr18, p.attr19)
        } (strings, posZIntValues, posZLongValues, posZDoubleValues, posZFloatValues, posZIntValues, posZLongValues, posZDoubleValues, posZFloatValues, posZIntValues, posZLongValues, posZDoubleValues, posZFloatValues, posZIntValues, posZLongValues,
          posZDoubleValues, posZFloatValues, posZIntValues, posZLongValues)
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        forAll (persons) { case Person(_, ag, attr3, attr4, attr5, attr6, attr7, attr8, attr9, attr10, attr11, attr12, attr13, attr14, attr15, attr16, attr17, attr18, attr19) =>
          ag should be >= 0
          attr3 should be >= 0L
          attr4 should be >= 0.0
          attr5 should be >= 0.0f
          attr6 should be >= 0
          attr7 should be >= 0L
          attr8 should be >= 0.0
          attr9 should be >= 0.0f
          attr10 should be >= 0
          attr11 should be >= 0L
          attr12 should be >= 0.0
          attr13 should be >= 0.0f
          attr14 should be >= 0
          attr15 should be >= 0L
          attr16 should be >= 0.0
          attr17 should be >= 0.0f
          attr18 should be >= 0
          attr19 should be >= 0L
        } // A contrived property check to do something with the generator
      }
      "produces generators given construct and deconstruct functions for 20 types" in {
        case class Person(name: String, age: Int, attr3: Long, attr4: Double, attr5: Float, attr6: Int, attr7: Long, attr8: Double, attr9: Float, attr10: Int, attr11: Long, attr12: Double, attr13: Float, attr14: Int, attr15: Long,
                          attr16: Double, attr17: Float, attr18: Int, attr19: Long, attr20: Double)
        val persons = gen(Person) { p =>
          (p.name, p.age, p.attr3, p.attr4, p.attr5, p.attr6, p.attr7, p.attr8, p.attr9, p.attr10, p.attr11, p.attr12, p.attr13, p.attr14, p.attr15, p.attr16, p.attr17, p.attr18, p.attr19, p.attr20)
        } (strings, posZIntValues, posZLongValues, posZDoubleValues, posZFloatValues, posZIntValues, posZLongValues, posZDoubleValues, posZFloatValues, posZIntValues, posZLongValues, posZDoubleValues, posZFloatValues, posZIntValues, posZLongValues,
          posZDoubleValues, posZFloatValues, posZIntValues, posZLongValues, posZDoubleValues)
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        forAll (persons) { case Person(_, ag, attr3, attr4, attr5, attr6, attr7, attr8, attr9, attr10, attr11, attr12, attr13, attr14, attr15, attr16, attr17, attr18, attr19, attr20) =>
          ag should be >= 0
          attr3 should be >= 0L
          attr4 should be >= 0.0
          attr5 should be >= 0.0f
          attr6 should be >= 0
          attr7 should be >= 0L
          attr8 should be >= 0.0
          attr9 should be >= 0.0f
          attr10 should be >= 0
          attr11 should be >= 0L
          attr12 should be >= 0.0
          attr13 should be >= 0.0f
          attr14 should be >= 0
          attr15 should be >= 0L
          attr16 should be >= 0.0
          attr17 should be >= 0.0f
          attr18 should be >= 0
          attr19 should be >= 0L
          attr20 should be >= 0.0
        } // A contrived property check to do something with the generator
      }
      "produces generators given construct and deconstruct functions for 21 types" in {
        case class Person(name: String, age: Int, attr3: Long, attr4: Double, attr5: Float, attr6: Int, attr7: Long, attr8: Double, attr9: Float, attr10: Int, attr11: Long, attr12: Double, attr13: Float, attr14: Int, attr15: Long,
                          attr16: Double, attr17: Float, attr18: Int, attr19: Long, attr20: Double, attr21: Float)
        val persons = gen(Person) { p =>
          (p.name, p.age, p.attr3, p.attr4, p.attr5, p.attr6, p.attr7, p.attr8, p.attr9, p.attr10, p.attr11, p.attr12, p.attr13, p.attr14, p.attr15, p.attr16, p.attr17, p.attr18, p.attr19, p.attr20, p.attr21)
        } (strings, posZIntValues, posZLongValues, posZDoubleValues, posZFloatValues, posZIntValues, posZLongValues, posZDoubleValues, posZFloatValues, posZIntValues, posZLongValues, posZDoubleValues, posZFloatValues, posZIntValues, posZLongValues,
          posZDoubleValues, posZFloatValues, posZIntValues, posZLongValues, posZDoubleValues, posZFloatValues)
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        forAll (persons) { case Person(_, ag, attr3, attr4, attr5, attr6, attr7, attr8, attr9, attr10, attr11, attr12, attr13, attr14, attr15, attr16, attr17, attr18, attr19, attr20, attr21) =>
          ag should be >= 0
          attr3 should be >= 0L
          attr4 should be >= 0.0
          attr5 should be >= 0.0f
          attr6 should be >= 0
          attr7 should be >= 0L
          attr8 should be >= 0.0
          attr9 should be >= 0.0f
          attr10 should be >= 0
          attr11 should be >= 0L
          attr12 should be >= 0.0
          attr13 should be >= 0.0f
          attr14 should be >= 0
          attr15 should be >= 0L
          attr16 should be >= 0.0
          attr17 should be >= 0.0f
          attr18 should be >= 0
          attr19 should be >= 0L
          attr20 should be >= 0.0
          attr21 should be >= 0.0f
        } // A contrived property check to do something with the generator
      }
      "produces generators given construct and deconstruct functions for 22 types" in {
        case class Person(name: String, age: Int, attr3: Long, attr4: Double, attr5: Float, attr6: Int, attr7: Long, attr8: Double, attr9: Float, attr10: Int, attr11: Long, attr12: Double, attr13: Float, attr14: Int, attr15: Long,
                          attr16: Double, attr17: Float, attr18: Int, attr19: Long, attr20: Double, attr21: Float, attr22: Int)
        val persons = gen(Person) { p =>
          (p.name, p.age, p.attr3, p.attr4, p.attr5, p.attr6, p.attr7, p.attr8, p.attr9, p.attr10, p.attr11, p.attr12, p.attr13, p.attr14, p.attr15, p.attr16, p.attr17, p.attr18, p.attr19, p.attr20, p.attr21, p.attr22)
        } (strings, posZIntValues, posZLongValues, posZDoubleValues, posZFloatValues, posZIntValues, posZLongValues, posZDoubleValues, posZFloatValues, posZIntValues, posZLongValues, posZDoubleValues, posZFloatValues, posZIntValues, posZLongValues,
          posZDoubleValues, posZFloatValues, posZIntValues, posZLongValues, posZDoubleValues, posZFloatValues, posZIntValues)
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        forAll (persons) { case Person(_, ag, attr3, attr4, attr5, attr6, attr7, attr8, attr9, attr10, attr11, attr12, attr13, attr14, attr15, attr16, attr17, attr18, attr19, attr20, attr21, attr22) =>
          ag should be >= 0
          attr3 should be >= 0L
          attr4 should be >= 0.0
          attr5 should be >= 0.0f
          attr6 should be >= 0
          attr7 should be >= 0L
          attr8 should be >= 0.0
          attr9 should be >= 0.0f
          attr10 should be >= 0
          attr11 should be >= 0L
          attr12 should be >= 0.0
          attr13 should be >= 0.0f
          attr14 should be >= 0
          attr15 should be >= 0L
          attr16 should be >= 0.0
          attr17 should be >= 0.0f
          attr18 should be >= 0
          attr19 should be >= 0L
          attr20 should be >= 0.0
          attr21 should be >= 0.0f
          attr22 should be >= 0
        } // A contrived property check to do something with the generator
      }
    }
  }
}

