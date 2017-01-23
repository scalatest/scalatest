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
import org.scalatest.matchers.BeMatcher
import org.scalatest.matchers.MatchResult

class CommonGeneratorsSpec extends WordSpec with Matchers {
  import CommonGenerators._
  "The CommonGenerators object" should {
    "offer a first1000Primes method" that {
      "produces the first 1000 prime numbers a Ints" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._

        def isPrime(n: Int): Boolean = {
          if (n <= 1) false
          else if (n <= 3) true
          else if (n % 2 == 0 || n % 3 == 0) false
          else {
            var i = 5
            while (i * i <= n) {
              if (n % i == 0 || n % (i + 2) == 0)
                return false
              i += 6
            }
            true
          }
        }

        val aPrimeNumber =
          new BeMatcher[Int] {
            def apply(left: Int) =
              MatchResult(
                isPrime(left),
                left.toString + " was not prime",
                left.toString + " was prime"
              )
          }

        forAll (first1000Primes) { i =>
          i shouldBe aPrimeNumber
        }
      }
    }

    "offer a bytesBetween method" that {
      "throws IAE if max is less than min" in {
        val loHiPairs =
          for {
            lo <- bytesBetween(Byte.MinValue, Byte.MaxValue - 1) // Hmm. Using the method to test itself
            hi <- bytesBetween((lo + 1).toByte, Byte.MaxValue)
          } yield (lo, hi)
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        forAll (loHiPairs) { case (lo, hi) =>
          an [IllegalArgumentException] should be thrownBy {
            bytesBetween(hi, lo)
          }
        }
      }
      "produces Bytes between min and max" in {

        import org.scalatest.prop.GeneratorDrivenPropertyChecks._

        val minMaxPairs: Generator[(Byte, Byte)] =
          for {
            min <- bytesBetween(Byte.MinValue, Byte.MaxValue - 1)
            max <- bytesBetween(min, Byte.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[Byte] = bytesBetween(min, max)
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

        val minMaxPairs: Generator[(Byte, Byte)] =
          for {
            min <- bytesBetween(Byte.MinValue, Byte.MaxValue - 1)
            max <- bytesBetween(min, Byte.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[Byte] = bytesBetween(min, max)
          val (edges, _) = minMaxGen.initEdges(100, Randomizer.default)
          edges.length should (be >= 1 or be <= 7)
          edges should contain (min)
          edges should contain (max)
        }
      }
      "returns a generator whose initEdges method includes normal Int edges only if they are between min and max, inclusive" in {

        import org.scalatest.Inspectors._

        val (edges, rnd1) = bytes.initEdges(100, Randomizer.default)
        val sortedEdges = edges.sorted
        val combos = sortedEdges.combinations(2).toList

        def included(from: Byte, to: Byte): List[Byte] = {
          val fromIdx = sortedEdges.indexOf(from)
          val toIdx = sortedEdges.indexOf(to)
          sortedEdges.drop(fromIdx).take(toIdx - fromIdx + 1)
        }

        forAll (combos) { case List(from, to) =>
          val requiredEdges = included(from, to)
          val minMaxGen: Generator[Byte] = bytesBetween(from, to)
          val (edges, _) = minMaxGen.initEdges(100, Randomizer.default)
          edges should contain allElementsOf requiredEdges
          val outOfBoundsEdges = sortedEdges.filter(i => i < from || i > to)
          edges should contain noElementsOf outOfBoundsEdges
        }
      }
    }

    "offer a shortsBetween method" that {
      "throws IAE if max is less than min" in {
        val loHiPairs =
          for {
            lo <- shortsBetween(Short.MinValue, Short.MaxValue - 1) // Hmm. Using the method to test itself
            hi <- shortsBetween((lo + 1).toShort, Short.MaxValue)
          } yield (lo, hi)
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        forAll (loHiPairs) { case (lo, hi) =>
          an [IllegalArgumentException] should be thrownBy {
            shortsBetween(hi, lo)
          }
        }
      }
      "produces Shorts between min and max" in {

        import org.scalatest.prop.GeneratorDrivenPropertyChecks._

        val minMaxPairs: Generator[(Short, Short)] =
          for {
            min <- shortsBetween(Short.MinValue, Short.MaxValue - 1)
            max <- shortsBetween(min, Short.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[Short] = shortsBetween(min, max)
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

        val minMaxPairs: Generator[(Short, Short)] =
          for {
            min <- shortsBetween(Short.MinValue, Short.MaxValue - 1)
            max <- shortsBetween(min, Short.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[Short] = shortsBetween(min, max)
          val (edges, _) = minMaxGen.initEdges(100, Randomizer.default)
          edges.length should (be >= 1 or be <= 7)
          edges should contain (min)
          edges should contain (max)
        }
      }
      "returns a generator whose initEdges method includes normal Int edges only if they are between min and max, inclusive" in {

        import org.scalatest.Inspectors._

        val (edges, rnd1) = shorts.initEdges(100, Randomizer.default)
        val sortedEdges = edges.sorted
        val combos = sortedEdges.combinations(2).toList

        def included(from: Short, to: Short): List[Short] = {
          val fromIdx = sortedEdges.indexOf(from)
          val toIdx = sortedEdges.indexOf(to)
          sortedEdges.drop(fromIdx).take(toIdx - fromIdx + 1)
        }

        forAll (combos) { case List(from, to) =>
          val requiredEdges = included(from, to)
          val minMaxGen: Generator[Short] = shortsBetween(from, to)
          val (edges, _) = minMaxGen.initEdges(100, Randomizer.default)
          edges should contain allElementsOf requiredEdges
          val outOfBoundsEdges = sortedEdges.filter(i => i < from || i > to)
          edges should contain noElementsOf outOfBoundsEdges
        }
      }
    }

    "offer an intsBetween method" that {
      "throws IAE if max is less than min" in {
        val loHiPairs =
          for {
            lo <- intsBetween(Int.MinValue, Int.MaxValue - 1) // Hmm. Using the method to test itself
            hi <- intsBetween(lo + 1, Int.MaxValue)
          } yield (lo, hi)
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        forAll (loHiPairs) { case (lo, hi) =>
          an [IllegalArgumentException] should be thrownBy {
            intsBetween(hi, lo)
          }
        }
      }
      "produces Ints between min and max" in {

        import org.scalatest.prop.GeneratorDrivenPropertyChecks._

        val minMaxPairs: Generator[(Int, Int)] = 
          for {
            min <- intsBetween(Int.MinValue, Int.MaxValue - 1)
            max <- intsBetween(min, Int.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[Int] = intsBetween(min, max) 
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
            min <- intsBetween(Int.MinValue, Int.MaxValue - 1)
            max <- intsBetween(min, Int.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[Int] = intsBetween(min, max) 
          val (edges, _) = minMaxGen.initEdges(100, Randomizer.default)
          edges.length should (be >= 1 or be <= 7)
          edges should contain (min)
          edges should contain (max)
        }
      }
      "returns a generator whose initEdges method includes normal Int edges only if they are between min and max, inclusive" in {

        import org.scalatest.Inspectors._

        val (edges, rnd1) = ints.initEdges(100, Randomizer.default)
        // sortedEdges: List[Int] = List(-2147483648, -1, 0, 1, 2147483647)
        val sortedEdges = edges.sorted
        // res5: List[List[Int]] = List(List(-2147483648, -1), List(-2147483648, 0), List(-2147483648, 1),
        //   List(-2147483648, 2147483647), List(-1, 0), List(-1, 1), List(-1, 2147483647), List(0, 1),
        //   List(0, 2147483647), List(1, 2147483647))
        val combos = sortedEdges.combinations(2).toList

        def included(from: Int, to: Int): List[Int] = {
          val fromIdx = sortedEdges.indexOf(from)
          val toIdx = sortedEdges.indexOf(to)
          sortedEdges.drop(fromIdx).take(toIdx - fromIdx + 1)
        }

        forAll (combos) { case List(from, to) =>
          val requiredEdges = included(from, to)
          val minMaxGen: Generator[Int] = intsBetween(from, to) 
          val (edges, _) = minMaxGen.initEdges(100, Randomizer.default)
          edges should contain allElementsOf requiredEdges
          val outOfBoundsEdges = sortedEdges.filter(i => i < from || i > to)
          edges should contain noElementsOf outOfBoundsEdges
        }
      }
    }

    "offer a longsBetween method" that {
      "throws IAE if max is less than min" in {
        val loHiPairs =
          for {
            lo <- longsBetween(Long.MinValue, Long.MaxValue - 1) // Hmm. Using the method to test itself
            hi <- longsBetween(lo + 1, Long.MaxValue)
          } yield (lo, hi)
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        forAll (loHiPairs) { case (lo, hi) =>
          an [IllegalArgumentException] should be thrownBy {
            longsBetween(hi, lo)
          }
        }
      }
      "produces Longs between min and max" in {

        import org.scalatest.prop.GeneratorDrivenPropertyChecks._

        val minMaxPairs: Generator[(Long, Long)] =
          for {
            min <- longsBetween(Long.MinValue, Long.MaxValue - 1)
            max <- longsBetween(min, Long.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[Long] = longsBetween(min, max)
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

        val minMaxPairs: Generator[(Long, Long)] =
          for {
            min <- longsBetween(Long.MinValue, Long.MaxValue - 1)
            max <- longsBetween(min, Long.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[Long] = longsBetween(min, max)
          val (edges, _) = minMaxGen.initEdges(100, Randomizer.default)
          edges.length should (be >= 1 or be <= 7)
          edges should contain (min)
          edges should contain (max)
        }
      }
      "returns a generator whose initEdges method includes normal Int edges only if they are between min and max, inclusive" in {

        import org.scalatest.Inspectors._

        val (edges, rnd1) = longs.initEdges(100, Randomizer.default)
        val sortedEdges = edges.sorted
        val combos = sortedEdges.combinations(2).toList

        def included(from: Long, to: Long): List[Long] = {
          val fromIdx = sortedEdges.indexOf(from)
          val toIdx = sortedEdges.indexOf(to)
          sortedEdges.drop(fromIdx).take(toIdx - fromIdx + 1)
        }

        forAll (combos) { case List(from, to) =>
          val requiredEdges = included(from, to)
          val minMaxGen: Generator[Long] = longsBetween(from, to)
          val (edges, _) = minMaxGen.initEdges(100, Randomizer.default)
          edges should contain allElementsOf requiredEdges
          val outOfBoundsEdges = sortedEdges.filter(i => i < from || i > to)
          edges should contain noElementsOf outOfBoundsEdges
        }
      }
    }

    "offer a charsBetween method" that {
      "throws IAE if max is less than min" in {
        val loHiPairs =
          for {
            lo <- charsBetween(Char.MinValue, Char.MaxValue - 1) // Hmm. Using the method to test itself
            hi <- charsBetween((lo + 1).toChar, Char.MaxValue)
          } yield (lo, hi)
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        forAll (loHiPairs) { case (lo, hi) =>
          an [IllegalArgumentException] should be thrownBy {
            charsBetween(hi, lo)
          }
        }
      }
      "produces Chars between min and max" in {

        import org.scalatest.prop.GeneratorDrivenPropertyChecks._

        val minMaxPairs: Generator[(Char, Char)] =
          for {
            min <- charsBetween(Char.MinValue, Char.MaxValue - 1)
            max <- charsBetween(min, Char.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[Char] = charsBetween(min, max)
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

        val minMaxPairs: Generator[(Char, Char)] =
          for {
            min <- charsBetween(Char.MinValue, Char.MaxValue - 1)
            max <- charsBetween(min, Char.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[Char] = charsBetween(min, max)
          val (edges, _) = minMaxGen.initEdges(100, Randomizer.default)
          edges.length should (be >= 1 or be <= 7)
          edges should contain (min)
          edges should contain (max)
        }
      }
      "returns a generator whose initEdges method includes normal Char edges only if they are between min and max, inclusive" in {

        import org.scalatest.Inspectors._

        val (edges, rnd1) = chars.initEdges(100, Randomizer.default)
        val sortedEdges = edges.sorted
        val combos = sortedEdges.combinations(2).toList

        def included(from: Char, to: Char): List[Char] = {
          val fromIdx = sortedEdges.indexOf(from)
          val toIdx = sortedEdges.indexOf(to)
          sortedEdges.drop(fromIdx).take(toIdx - fromIdx + 1)
        }

        forAll (combos) { case List(from, to) =>
          val requiredEdges = included(from, to)
          val minMaxGen: Generator[Char] = charsBetween(from, to)
          val (edges, _) = minMaxGen.initEdges(100, Randomizer.default)
          edges should contain allElementsOf requiredEdges
          val outOfBoundsEdges = sortedEdges.filter(i => i < from || i > to)
          edges should contain noElementsOf outOfBoundsEdges
        }
      }
    }

    "offer a floatsBetween method" that {
      "throws IAE if max is less than min" in {
        val loHiPairs =
          for {
            lo <- floatsBetween(Float.MinValue, Float.MaxValue - 1E32f) // Hmm. Using the method to test itself
            hi <- floatsBetween((lo + 1E32f), Float.MaxValue)
          } yield (lo, hi)
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        forAll (loHiPairs) { case (lo, hi) =>
          an[IllegalArgumentException] should be thrownBy {
            floatsBetween(hi, lo)
          }
        }
      }
      "produces Floats between min and max" in {

        import org.scalatest.prop.GeneratorDrivenPropertyChecks._

        val minMaxPairs: Generator[(Float, Float)] =
          for {
            min <- floatsBetween(Float.MinValue, Float.MaxValue - 1E32f)
            max <- floatsBetween(min, Float.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[Float] = floatsBetween(min, max)
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

        val minMaxPairs: Generator[(Float, Float)] =
          for {
            min <- floatsBetween(Float.MinValue, Float.MaxValue - 1E32f)
            max <- floatsBetween(min, Float.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[Float] = floatsBetween(min, max)
          val (edges, _) = minMaxGen.initEdges(100, Randomizer.default)
          edges.length should (be >= 1 or be <= 7)
          edges should contain (min)
          edges should contain (max)
        }
      }
      "returns a generator whose initEdges method includes normal Float edges only if they are between min and max, inclusive" in {

        import org.scalatest.Inspectors._

        val (edges, rnd1) = floats.initEdges(100, Randomizer.default)
        val sortedEdges = edges.sorted
        val combos = sortedEdges.combinations(2).toList

        def included(from: Float, to: Float): List[Float] = {
          val fromIdx = sortedEdges.indexOf(from)
          val toIdx = sortedEdges.indexOf(to)
          sortedEdges.drop(fromIdx).take(toIdx - fromIdx + 1)
        }

        forAll (combos) { case List(from, to) =>
          val requiredEdges = included(from, to)
          val minMaxGen: Generator[Float] = floatsBetween(from, to)
          val (edges, _) = minMaxGen.initEdges(100, Randomizer.default)
          edges should contain allElementsOf requiredEdges
          val outOfBoundsEdges = sortedEdges.filter(i => i < from || i > to)
          edges should contain noElementsOf outOfBoundsEdges
        }
      }
    }

    "offer a doublesBetween method" that {
      "throws IAE if max is less than min" in {
        val loHiPairs =
          for {
            lo <- doublesBetween(Double.MinValue, Double.MaxValue - 1E292) // Hmm. Using the method to test itself
            hi <- doublesBetween((lo + 1E292), Double.MaxValue)
          } yield (lo, hi)
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        forAll (loHiPairs) { case (lo, hi) =>
          an[IllegalArgumentException] should be thrownBy {
            doublesBetween(hi, lo)
          }
        }
      }
      "produces Doubles between min and max" in {

        import org.scalatest.prop.GeneratorDrivenPropertyChecks._

        val minMaxPairs: Generator[(Double, Double)] =
          for {
            min <- doublesBetween(Double.MinValue, Double.MaxValue - 1E292)
            max <- doublesBetween(min, Double.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[Double] = doublesBetween(min, max)
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

        val minMaxPairs: Generator[(Double, Double)] =
          for {
            min <- doublesBetween(Double.MinValue, Double.MaxValue - 1E292)
            max <- doublesBetween(min, Double.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[Double] = doublesBetween(min, max)
          val (edges, _) = minMaxGen.initEdges(100, Randomizer.default)
          edges.length should (be >= 1 or be <= 7)
          edges should contain (min)
          edges should contain (max)
        }
      }
      "returns a generator whose initEdges method includes normal Double edges only if they are between min and max, inclusive" in {

        import org.scalatest.Inspectors._

        val (edges, rnd1) = doubles.initEdges(100, Randomizer.default)
        val sortedEdges = edges.sorted
        val combos = sortedEdges.combinations(2).toList

        def included(from: Double, to: Double): List[Double] = {
          val fromIdx = sortedEdges.indexOf(from)
          val toIdx = sortedEdges.indexOf(to)
          sortedEdges.drop(fromIdx).take(toIdx - fromIdx + 1)
        }

        forAll (combos) { case List(from, to) =>
          val requiredEdges = included(from, to)
          val minMaxGen: Generator[Double] = doublesBetween(from, to)
          val (edges, _) = minMaxGen.initEdges(100, Randomizer.default)
          edges should contain allElementsOf requiredEdges
          val outOfBoundsEdges = sortedEdges.filter(i => i < from || i > to)
          edges should contain noElementsOf outOfBoundsEdges
        }
      }
    }

    "offer a posIntsBetween method" that {
      val PosIntMaxValueMinusOne = PosInt(2147483646)

      "throws IAE if max is less than min" in {
        val loHiPairs =
          for {
            lo <- intsBetween(PosInt.MinValue, PosInt.MaxValue - 1) // Hmm. Using the method to test itself
            hi <- intsBetween(lo + 1, PosInt.MaxValue)
          } yield (lo, hi)
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        forAll (loHiPairs) { case (lo, hi) =>
          an [IllegalArgumentException] should be thrownBy {
            intsBetween(hi, lo)
          }
        }
      }

      "produces PosInts between min and max" in {

        import org.scalatest.prop.GeneratorDrivenPropertyChecks._

        val minMaxPairs: Generator[(PosInt, PosInt)] = 
          for {
            min <- posIntsBetween(PosInt.MinValue, PosIntMaxValueMinusOne)
            max <- posIntsBetween(min, PosInt.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[PosInt] = posIntsBetween(min, max) 
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
            min <- posIntsBetween(PosInt.MinValue, PosIntMaxValueMinusOne)
            max <- posIntsBetween(min, PosInt.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[PosInt] = posIntsBetween(min, max) 
          val (edges, _) = minMaxGen.initEdges(100, Randomizer.default)
          edges should (have length 1 or have length 2)
          edges should contain (min)
          edges should contain (max)
        }
      }
      "returns a generator whose initEdges method includes normal PosInt edges only if they are between min and max, inclusive" in {

        import org.scalatest.Inspectors._

        val (edges, rnd1) = posInts.initEdges(100, Randomizer.default)
        // sortedEdges: List[Int] = List(-2147483648, -1, 0, 1, 2147483647)
        val sortedEdges = edges.sorted
        // res5: List[List[Int]] = List(List(-2147483648, -1), List(-2147483648, 0), List(-2147483648, 1),
        //   List(-2147483648, 2147483647), List(-1, 0), List(-1, 1), List(-1, 2147483647), List(0, 1),
        //   List(0, 2147483647), List(1, 2147483647))
        val combos = sortedEdges.combinations(2).toList

        def included(from: PosInt, to: PosInt): List[PosInt] = {
          val fromIdx = sortedEdges.indexOf(from)
          val toIdx = sortedEdges.indexOf(to)
          sortedEdges.drop(fromIdx).take(toIdx - fromIdx + 1)
        }

        forAll (combos) { case List(from, to) =>
          val requiredEdges = included(from, to)
          val minMaxGen: Generator[PosInt] = posIntsBetween(from, to)
          val (edges, _) = minMaxGen.initEdges(100, Randomizer.default)
          edges should contain allElementsOf requiredEdges
          val outOfBoundsEdges = sortedEdges.filter(i => i < from || i > to)
          edges should contain noElementsOf outOfBoundsEdges
        }
      }
    }

    "offer a posLongsBetween method" that {
      "throws IAE if max is less than min" in {
        val loHiPairs =
          for {
            lo <- posLongsBetween(PosLong.MinValue, PosLong.ensuringValid(PosLong.MaxValue - 1)) // Hmm. Using the method to test itself
            hi <- posLongsBetween(PosLong.ensuringValid(lo + 1), PosLong.MaxValue)
          } yield (lo, hi)
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        forAll (loHiPairs) { case (lo, hi) =>
          an [IllegalArgumentException] should be thrownBy {
            posLongsBetween(hi, lo)
          }
        }
      }
      "produces PosLongs between min and max" in {

        import org.scalatest.prop.GeneratorDrivenPropertyChecks._

        val minMaxPairs: Generator[(PosLong, PosLong)] =
          for {
            min <- posLongsBetween(PosLong.MinValue, PosLong.ensuringValid(PosLong.MaxValue - 1))
            max <- posLongsBetween(min, PosLong.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[PosLong] = posLongsBetween(min, max)
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

        val minMaxPairs: Generator[(PosLong, PosLong)] =
          for {
            min <- posLongsBetween(PosLong.MinValue, PosLong.ensuringValid(PosLong.MaxValue - 1))
            max <- posLongsBetween(min, PosLong.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[PosLong] = posLongsBetween(min, max)
          val (edges, _) = minMaxGen.initEdges(100, Randomizer.default)
          edges.length should (be >= 1 or be <= 7)
          edges should contain (min)
          edges should contain (max)
        }
      }
      "returns a generator whose initEdges method includes normal PosLong edges only if they are between min and max, inclusive" in {

        import org.scalatest.Inspectors._

        val (edges, rnd1) = posLongs.initEdges(100, Randomizer.default)
        val sortedEdges = edges.sorted
        val combos = sortedEdges.combinations(2).toList

        def included(from: PosLong, to: PosLong): List[PosLong] = {
          val fromIdx = sortedEdges.indexOf(from)
          val toIdx = sortedEdges.indexOf(to)
          sortedEdges.drop(fromIdx).take(toIdx - fromIdx + 1)
        }

        forAll (combos) { case List(from, to) =>
          val requiredEdges = included(from, to)
          val minMaxGen: Generator[PosLong] = posLongsBetween(from, to)
          val (edges, _) = minMaxGen.initEdges(100, Randomizer.default)
          edges should contain allElementsOf requiredEdges
          val outOfBoundsEdges = sortedEdges.filter(i => i < from || i > to)
          edges should contain noElementsOf outOfBoundsEdges
        }
      }
    }

    "offer a posFloatsBetween method" that {
      "throws IAE if max is less than min" in {
        val loHiPairs =
          for {
            lo <- posFloatsBetween(PosFloat.MinValue, PosFloat.ensuringValid(PosFloat.MaxValue - 1E32f)) // Hmm. Using the method to test itself
            hi <- posFloatsBetween(PosFloat.ensuringValid(lo + 1E32f), PosFloat.MaxValue)
          } yield (lo, hi)
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        forAll (loHiPairs) { case (lo, hi) =>
          an[IllegalArgumentException] should be thrownBy {
            posFloatsBetween(hi, lo)
          }
        }
      }
      "produces PosFloats between min and max" in {

        import org.scalatest.prop.GeneratorDrivenPropertyChecks._

        val minMaxPairs: Generator[(PosFloat, PosFloat)] =
          for {
            min <- posFloatsBetween(PosFloat.MinValue, PosFloat.ensuringValid(PosFloat.MaxValue - 1E32f))
            max <- posFloatsBetween(min, PosFloat.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[PosFloat] = posFloatsBetween(min, max)
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

        val minMaxPairs: Generator[(PosFloat, PosFloat)] =
          for {
            min <- posFloatsBetween(PosFloat.MinValue, PosFloat.ensuringValid(PosFloat.MaxValue - 1E32f))
            max <- posFloatsBetween(min, PosFloat.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[PosFloat] = posFloatsBetween(min, max)
          val (edges, _) = minMaxGen.initEdges(100, Randomizer.default)
          edges.length should (be >= 1 or be <= 7)
          edges should contain (min)
          edges should contain (max)
        }
      }
      "returns a generator whose initEdges method includes normal PosFloat edges only if they are between min and max, inclusive" in {

        import org.scalatest.Inspectors._

        val (edges, rnd1) = posFloats.initEdges(100, Randomizer.default)
        val sortedEdges = edges.sorted
        val combos = sortedEdges.combinations(2).toList

        def included(from: PosFloat, to: PosFloat): List[PosFloat] = {
          val fromIdx = sortedEdges.indexOf(from)
          val toIdx = sortedEdges.indexOf(to)
          sortedEdges.drop(fromIdx).take(toIdx - fromIdx + 1)
        }

        forAll (combos) { case List(from, to) =>
          val requiredEdges = included(from, to)
          val minMaxGen: Generator[PosFloat] = posFloatsBetween(from, to)
          val (edges, _) = minMaxGen.initEdges(100, Randomizer.default)
          edges should contain allElementsOf requiredEdges
          val outOfBoundsEdges = sortedEdges.filter(i => i < from || i > to)
          edges should contain noElementsOf outOfBoundsEdges
        }
      }
    }

    "offer a posDoublesBetween method" that {
      "throws IAE if max is less than min" in {
        val loHiPairs =
          for {
            lo <- posDoublesBetween(PosDouble.MinValue, PosDouble.ensuringValid(PosDouble.MaxValue - 1E292)) // Hmm. Using the method to test itself
            hi <- posDoublesBetween(PosDouble.ensuringValid(lo + 1E292), PosDouble.MaxValue)
          } yield (lo, hi)
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        forAll (loHiPairs) { case (lo, hi) =>
          an[IllegalArgumentException] should be thrownBy {
            posDoublesBetween(hi, lo)
          }
        }
      }
      "produces PosDoubles between min and max" in {

        import org.scalatest.prop.GeneratorDrivenPropertyChecks._

        val minMaxPairs: Generator[(PosDouble, PosDouble)] =
          for {
            min <- posDoublesBetween(PosDouble.MinValue, PosDouble.ensuringValid(PosDouble.MaxValue - 1E292))
            max <- posDoublesBetween(min, PosDouble.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[PosDouble] = posDoublesBetween(min, max)
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

        val minMaxPairs: Generator[(PosDouble, PosDouble)] =
          for {
            min <- posDoublesBetween(PosDouble.MinValue, PosDouble.ensuringValid(PosDouble.MaxValue - 1E292))
            max <- posDoublesBetween(min, PosDouble.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[PosDouble] = posDoublesBetween(min, max)
          val (edges, _) = minMaxGen.initEdges(100, Randomizer.default)
          edges.length should (be >= 1 or be <= 7)
          edges should contain (min)
          edges should contain (max)
        }
      }

      "returns a generator whose initEdges method includes normal PosDouble edges only if they are between min and max, inclusive" in {

        import org.scalatest.Inspectors._

        val (edges, rnd1) = posDoubles.initEdges(100, Randomizer.default)
        val sortedEdges = edges.sorted
        val combos = sortedEdges.combinations(2).toList

        def included(from: PosDouble, to: PosDouble): List[PosDouble] = {
          val fromIdx = sortedEdges.indexOf(from)
          val toIdx = sortedEdges.indexOf(to)
          sortedEdges.drop(fromIdx).take(toIdx - fromIdx + 1)
        }

        forAll (combos) { case List(from, to) =>
          val requiredEdges = included(from, to)
          val minMaxGen: Generator[PosDouble] = posDoublesBetween(from, to)
          val (edges, _) = minMaxGen.initEdges(100, Randomizer.default)
          edges should contain allElementsOf requiredEdges
          val outOfBoundsEdges = sortedEdges.filter(i => i < from || i > to)
          edges should contain noElementsOf outOfBoundsEdges
        }
      }
    }

    "offer a posZIntsBetween method" that {
      val PosZIntMaxValueMinusOne = PosZInt(2147483646)
      "throws IAE if max is less than min" in {
        val loHiPairs =
          for {
            lo <- intsBetween(PosZInt.MinValue, PosZInt.MaxValue - 1) // Hmm. Using the method to test itself
            hi <- intsBetween(lo + 1, PosZInt.MaxValue)
          } yield (lo, hi)
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        forAll (loHiPairs) { case (lo, hi) =>
          an [IllegalArgumentException] should be thrownBy {
            intsBetween(hi, lo)
          }
        }
      }
      "produces PosZInts between min and max" in {

        import org.scalatest.prop.GeneratorDrivenPropertyChecks._

        val minMaxPairs: Generator[(PosZInt, PosZInt)] =
          for {
            min <- posZIntsBetween(PosZInt.MinValue, PosZIntMaxValueMinusOne)
            max <- posZIntsBetween(min, PosZInt.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[PosZInt] = posZIntsBetween(min, max)
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

        val minMaxPairs: Generator[(PosZInt, PosZInt)] =
          for {
            min <- posZIntsBetween(PosZInt.MinValue, PosZIntMaxValueMinusOne)
            max <- posZIntsBetween(min, PosZInt.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[PosZInt] = posZIntsBetween(min, max)
          val (edges, _) = minMaxGen.initEdges(100, Randomizer.default)
          edges should (have length 1 or have length 2 or have length 3)
          edges should contain (min)
          edges should contain (max)
        }
      }
      "returns a generator whose initEdges method includes normal PosZInt edges only if they are between min and max, inclusive" in {

        import org.scalatest.Inspectors._

        val (edges, rnd1) = posZInts.initEdges(100, Randomizer.default)
        val sortedEdges = edges.sorted
        val combos = sortedEdges.combinations(2).toList

        def included(from: PosZInt, to: PosZInt): List[PosZInt] = {
          val fromIdx = sortedEdges.indexOf(from)
          val toIdx = sortedEdges.indexOf(to)
          sortedEdges.drop(fromIdx).take(toIdx - fromIdx + 1)
        }

        forAll (combos) { case List(from, to) =>
          val requiredEdges = included(from, to)
          val minMaxGen: Generator[PosZInt] = posZIntsBetween(from, to)
          val (edges, _) = minMaxGen.initEdges(100, Randomizer.default)
          edges should contain allElementsOf requiredEdges
          val outOfBoundsEdges = sortedEdges.filter(i => i < from || i > to)
          edges should contain noElementsOf outOfBoundsEdges
        }
      }
    }

    "offer a posZLongsBetween method" that {
      "throws IAE if max is less than min" in {
        val loHiPairs =
          for {
            lo <- posZLongsBetween(PosZLong.MinValue, PosZLong.ensuringValid(PosLong.MaxValue - 1)) // Hmm. Using the method to test itself
            hi <- posZLongsBetween(PosZLong.ensuringValid(lo + 1), PosZLong.MaxValue)
          } yield (lo, hi)
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        forAll (loHiPairs) { case (lo, hi) =>
          an [IllegalArgumentException] should be thrownBy {
            posZLongsBetween(hi, lo)
          }
        }
      }
      "produces PosZLongs between min and max" in {

        import org.scalatest.prop.GeneratorDrivenPropertyChecks._

        val minMaxPairs: Generator[(PosZLong, PosZLong)] =
          for {
            min <- posZLongsBetween(PosZLong.MinValue, PosZLong.ensuringValid(PosLong.MaxValue - 1))
            max <- posZLongsBetween(min, PosZLong.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[PosZLong] = posZLongsBetween(min, max)
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

        val minMaxPairs: Generator[(PosZLong, PosZLong)] =
          for {
            min <- posZLongsBetween(PosZLong.MinValue, PosZLong.ensuringValid(PosZLong.MaxValue - 1))
            max <- posZLongsBetween(min, PosZLong.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[PosZLong] = posZLongsBetween(min, max)
          val (edges, _) = minMaxGen.initEdges(100, Randomizer.default)
          edges.length should (be >= 1 or be <= 7)
          edges should contain (min)
          edges should contain (max)
        }
      }
      "returns a generator whose initEdges method includes normal PosZLong edges only if they are between min and max, inclusive" in {

        import org.scalatest.Inspectors._

        val (edges, rnd1) = posZLongs.initEdges(100, Randomizer.default)
        val sortedEdges = edges.sorted
        val combos = sortedEdges.combinations(2).toList

        def included(from: PosZLong, to: PosZLong): List[PosZLong] = {
          val fromIdx = sortedEdges.indexOf(from)
          val toIdx = sortedEdges.indexOf(to)
          sortedEdges.drop(fromIdx).take(toIdx - fromIdx + 1)
        }

        forAll (combos) { case List(from, to) =>
          val requiredEdges = included(from, to)
          val minMaxGen: Generator[PosZLong] = posZLongsBetween(from, to)
          val (edges, _) = minMaxGen.initEdges(100, Randomizer.default)
          edges should contain allElementsOf requiredEdges
          val outOfBoundsEdges = sortedEdges.filter(i => i < from || i > to)
          edges should contain noElementsOf outOfBoundsEdges
        }
      }
    }

    "offer a posZFloatsBetween method" that {
      "throws IAE if max is less than min" in {
        val loHiPairs =
          for {
            lo <- posZFloatsBetween(PosZFloat.MinValue, PosZFloat.ensuringValid(PosZFloat.MaxValue - 1E32f)) // Hmm. Using the method to test itself
            hi <- posZFloatsBetween(PosZFloat.ensuringValid(lo + 1E32f), PosZFloat.MaxValue)
          } yield (lo, hi)
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        forAll (loHiPairs) { case (lo, hi) =>
          an[IllegalArgumentException] should be thrownBy {
            posZFloatsBetween(hi, lo)
          }
        }
      }
      "produces PosZFloats between min and max" in {

        import org.scalatest.prop.GeneratorDrivenPropertyChecks._

        val minMaxPairs: Generator[(PosZFloat, PosZFloat)] =
          for {
            min <- posZFloatsBetween(PosZFloat.MinValue, PosZFloat.ensuringValid(PosZFloat.MaxValue - 1E32f))
            max <- posZFloatsBetween(min, PosZFloat.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[PosZFloat] = posZFloatsBetween(min, max)
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

        val minMaxPairs: Generator[(PosZFloat, PosZFloat)] =
          for {
            min <- posZFloatsBetween(PosZFloat.MinValue, PosZFloat.ensuringValid(PosZFloat.MaxValue - 1E32f))
            max <- posZFloatsBetween(min, PosZFloat.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[PosZFloat] = posZFloatsBetween(min, max)
          val (edges, _) = minMaxGen.initEdges(100, Randomizer.default)
          edges.length should (be >= 1 or be <= 7)
          edges should contain (min)
          edges should contain (max)
        }
      }
      "returns a generator whose initEdges method includes normal PosFloat edges only if they are between min and max, inclusive" in {

        import org.scalatest.Inspectors._

        val (edges, rnd1) = posZFloats.initEdges(100, Randomizer.default)
        val sortedEdges = edges.sorted
        val combos = sortedEdges.combinations(2).toList

        def included(from: PosZFloat, to: PosZFloat): List[PosZFloat] = {
          val fromIdx = sortedEdges.indexOf(from)
          val toIdx = sortedEdges.indexOf(to)
          sortedEdges.drop(fromIdx).take(toIdx - fromIdx + 1)
        }

        forAll (combos) { case List(from, to) =>
          val requiredEdges = included(from, to)
          val minMaxGen: Generator[PosZFloat] = posZFloatsBetween(from, to)
          val (edges, _) = minMaxGen.initEdges(100, Randomizer.default)
          edges should contain allElementsOf requiredEdges
          val outOfBoundsEdges = sortedEdges.filter(i => i < from || i > to)
          edges should contain noElementsOf outOfBoundsEdges
        }
      }
    }

    "offer a posZDoublesBetween method" that {
      "throws IAE if max is less than min" in {
        val loHiPairs =
          for {
            lo <- posZDoublesBetween(PosZDouble.MinValue, PosZDouble.ensuringValid(PosZDouble.MaxValue - 1E292)) // Hmm. Using the method to test itself
            hi <- posZDoublesBetween(PosZDouble.ensuringValid(lo + 1E292), PosZDouble.MaxValue)
          } yield (lo, hi)
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        forAll (loHiPairs) { case (lo, hi) =>
          an[IllegalArgumentException] should be thrownBy {
            posZDoublesBetween(hi, lo)
          }
        }
      }
      "produces PosDoubles between min and max" in {

        import org.scalatest.prop.GeneratorDrivenPropertyChecks._

        val minMaxPairs: Generator[(PosZDouble, PosZDouble)] =
          for {
            min <- posZDoublesBetween(PosZDouble.MinValue, PosZDouble.ensuringValid(PosZDouble.MaxValue - 1E292))
            max <- posZDoublesBetween(min, PosZDouble.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[PosZDouble] = posZDoublesBetween(min, max)
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

        val minMaxPairs: Generator[(PosZDouble, PosZDouble)] =
          for {
            min <- posZDoublesBetween(PosZDouble.MinValue, PosZDouble.ensuringValid(PosZDouble.MaxValue - 1E292))
            max <- posZDoublesBetween(min, PosZDouble.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[PosZDouble] = posZDoublesBetween(min, max)
          val (edges, _) = minMaxGen.initEdges(100, Randomizer.default)
          edges.length should (be >= 1 or be <= 7)
          edges should contain (min)
          edges should contain (max)
        }
      }

      "returns a generator whose initEdges method includes normal PosZDouble edges only if they are between min and max, inclusive" in {

        import org.scalatest.Inspectors._

        val (edges, rnd1) = posZDoubles.initEdges(100, Randomizer.default)
        val sortedEdges = edges.sorted
        val combos = sortedEdges.combinations(2).toList

        def included(from: PosZDouble, to: PosZDouble): List[PosZDouble] = {
          val fromIdx = sortedEdges.indexOf(from)
          val toIdx = sortedEdges.indexOf(to)
          sortedEdges.drop(fromIdx).take(toIdx - fromIdx + 1)
        }

        forAll (combos) { case List(from, to) =>
          val requiredEdges = included(from, to)
          val minMaxGen: Generator[PosZDouble] = posZDoublesBetween(from, to)
          val (edges, _) = minMaxGen.initEdges(100, Randomizer.default)
          edges should contain allElementsOf requiredEdges
          val outOfBoundsEdges = sortedEdges.filter(i => i < from || i > to)
          edges should contain noElementsOf outOfBoundsEdges
        }
      }
    }

    "offer a negIntsBetween method" that {
      val NegIntMaxValueMinusOne = NegInt(-2)

      "throws IAE if max is less than min" in {
        val loHiPairs =
          for {
            lo <- negIntsBetween(NegInt.MinValue, NegInt.ensuringValid(NegInt.MaxValue - 1)) // Hmm. Using the method to test itself
            hi <- negIntsBetween(NegInt.ensuringValid(lo + 1), NegInt.MaxValue)
          } yield (lo, hi)
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        forAll (loHiPairs) { case (lo, hi) =>
          an [IllegalArgumentException] should be thrownBy {
            negIntsBetween(hi, lo)
          }
        }
      }

      "produces NegInts between min and max" in {

        import org.scalatest.prop.GeneratorDrivenPropertyChecks._

        val minMaxPairs: Generator[(NegInt, NegInt)] =
          for {
            min <- negIntsBetween(NegInt.MinValue, NegIntMaxValueMinusOne)
            max <- negIntsBetween(min, NegInt.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[NegInt] = negIntsBetween(min, max)
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

        val minMaxPairs: Generator[(NegInt, NegInt)] =
          for {
            min <- negIntsBetween(NegInt.MinValue, NegIntMaxValueMinusOne)
            max <- negIntsBetween(min, NegInt.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[NegInt] = negIntsBetween(min, max)
          val (edges, _) = minMaxGen.initEdges(100, Randomizer.default)
          edges should (have length 1 or have length 2)
          edges should contain (min)
          edges should contain (max)
        }
      }
      "returns a generator whose initEdges method includes normal NegInt edges only if they are between min and max, inclusive" in {

        import org.scalatest.Inspectors._

        val (edges, rnd1) = negInts.initEdges(100, Randomizer.default)
        val sortedEdges = edges.sorted
        val combos = sortedEdges.combinations(2).toList

        def included(from: NegInt, to: NegInt): List[NegInt] = {
          val fromIdx = sortedEdges.indexOf(from)
          val toIdx = sortedEdges.indexOf(to)
          sortedEdges.drop(fromIdx).take(toIdx - fromIdx + 1)
        }

        forAll (combos) { case List(from, to) =>
          val requiredEdges = included(from, to)
          val minMaxGen: Generator[NegInt] = negIntsBetween(from, to)
          val (edges, _) = minMaxGen.initEdges(100, Randomizer.default)
          edges should contain allElementsOf requiredEdges
          val outOfBoundsEdges = sortedEdges.filter(i => i < from || i > to)
          edges should contain noElementsOf outOfBoundsEdges
        }
      }
    }

    "offer a negLongsBetween method" that {
      "throws IAE if max is less than min" in {
        val loHiPairs =
          for {
            lo <- negLongsBetween(NegLong.MinValue, NegLong.ensuringValid(NegLong.MaxValue - 1)) // Hmm. Using the method to test itself
            hi <- negLongsBetween(NegLong.ensuringValid(lo + 1), NegLong.MaxValue)
          } yield (lo, hi)
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        forAll (loHiPairs) { case (lo, hi) =>
          an [IllegalArgumentException] should be thrownBy {
            negLongsBetween(hi, lo)
          }
        }
      }
      "produces NegLongs between min and max" in {

        import org.scalatest.prop.GeneratorDrivenPropertyChecks._

        val minMaxPairs: Generator[(NegLong, NegLong)] =
          for {
            min <- negLongsBetween(NegLong.MinValue, NegLong.ensuringValid(NegLong.MaxValue - 1))
            max <- negLongsBetween(min, NegLong.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[NegLong] = negLongsBetween(min, max)
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

        val minMaxPairs: Generator[(NegLong, NegLong)] =
          for {
            min <- negLongsBetween(NegLong.MinValue, NegLong.ensuringValid(NegLong.MaxValue - 1))
            max <- negLongsBetween(min, NegLong.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[NegLong] = negLongsBetween(min, max)
          val (edges, _) = minMaxGen.initEdges(100, Randomizer.default)
          edges.length should (be >= 1 or be <= 7)
          edges should contain (min)
          edges should contain (max)
        }
      }
      "returns a generator whose initEdges method includes normal NegLong edges only if they are between min and max, inclusive" in {

        import org.scalatest.Inspectors._

        val (edges, rnd1) = negLongs.initEdges(100, Randomizer.default)
        val sortedEdges = edges.sorted
        val combos = sortedEdges.combinations(2).toList

        def included(from: NegLong, to: NegLong): List[NegLong] = {
          val fromIdx = sortedEdges.indexOf(from)
          val toIdx = sortedEdges.indexOf(to)
          sortedEdges.drop(fromIdx).take(toIdx - fromIdx + 1)
        }

        forAll (combos) { case List(from, to) =>
          val requiredEdges = included(from, to)
          val minMaxGen: Generator[NegLong] = negLongsBetween(from, to)
          val (edges, _) = minMaxGen.initEdges(100, Randomizer.default)
          edges should contain allElementsOf requiredEdges
          val outOfBoundsEdges = sortedEdges.filter(i => i < from || i > to)
          edges should contain noElementsOf outOfBoundsEdges
        }
      }
    }

    "offer a negFloatsBetween method" that {
      "throws IAE if max is less than min" in {
        val loHiPairs =
          for {
            lo <- negFloatsBetween(NegFloat.MinValue, NegFloat.ensuringValid(NegFloat.MaxValue - 1E32f - 1E32f))
            hi <- negFloatsBetween(NegFloat.ensuringValid(lo + 1E32f), NegFloat.MaxValue)
          } yield (lo, hi)
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        forAll (loHiPairs) { case (lo, hi) =>
          an[IllegalArgumentException] should be thrownBy {
            negFloatsBetween(hi, lo)
          }
        }
      }
      "produces NegFloats between min and max" in {

        import org.scalatest.prop.GeneratorDrivenPropertyChecks._

        val minMaxPairs: Generator[(NegFloat, NegFloat)] =
          for {
            min <- negFloatsBetween(NegFloat.MinValue, NegFloat.ensuringValid(NegFloat.MaxValue - 1E32f))
            max <- negFloatsBetween(min, NegFloat.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[NegFloat] = negFloatsBetween(min, max)
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

        val minMaxPairs: Generator[(NegFloat, NegFloat)] =
          for {
            min <- negFloatsBetween(NegFloat.MinValue, NegFloat.ensuringValid(NegFloat.MaxValue - 1E32f))
            max <- negFloatsBetween(min, NegFloat.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[NegFloat] = negFloatsBetween(min, max)
          val (edges, _) = minMaxGen.initEdges(100, Randomizer.default)
          edges.length should (be >= 1 or be <= 7)
          edges should contain (min)
          edges should contain (max)
        }
      }
      "returns a generator whose initEdges method includes normal NegFloat edges only if they are between min and max, inclusive" in {

        import org.scalatest.Inspectors._

        val (edges, rnd1) = negFloats.initEdges(100, Randomizer.default)
        val sortedEdges = edges.sorted
        val combos = sortedEdges.combinations(2).toList

        def included(from: NegFloat, to: NegFloat): List[NegFloat] = {
          val fromIdx = sortedEdges.indexOf(from)
          val toIdx = sortedEdges.indexOf(to)
          sortedEdges.drop(fromIdx).take(toIdx - fromIdx + 1)
        }

        forAll (combos) { case List(from, to) =>
          val requiredEdges = included(from, to)
          val minMaxGen: Generator[NegFloat] = negFloatsBetween(from, to)
          val (edges, _) = minMaxGen.initEdges(100, Randomizer.default)
          edges should contain allElementsOf requiredEdges
          val outOfBoundsEdges = sortedEdges.filter(i => i < from || i > to)
          edges should contain noElementsOf outOfBoundsEdges
        }
      }
    }

    "offer a negDoublesBetween method" that {
      "throws IAE if max is less than min" in {
        val loHiPairs =
          for {
            lo <- negDoublesBetween(NegDouble.MinValue, NegDouble.ensuringValid(NegDouble.MaxValue - 1E292 - 1E292)) // Hmm. Using the method to test itself
            hi <- negDoublesBetween(NegDouble.ensuringValid(lo + 1E292), NegDouble.MaxValue)
          } yield (lo, hi)
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        forAll (loHiPairs) { case (lo, hi) =>
          an[IllegalArgumentException] should be thrownBy {
            negDoublesBetween(hi, lo)
          }
        }
      }
      "produces NegDoubles between min and max" in {

        import org.scalatest.prop.GeneratorDrivenPropertyChecks._

        val minMaxPairs: Generator[(NegDouble, NegDouble)] =
          for {
            min <- negDoublesBetween(NegDouble.MinValue, NegDouble.ensuringValid(NegDouble.MaxValue - 1E292 - 1E292))
            max <- negDoublesBetween(min, NegDouble.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[NegDouble] = negDoublesBetween(min, max)
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

        val minMaxPairs: Generator[(NegDouble, NegDouble)] =
          for {
            min <- negDoublesBetween(NegDouble.MinValue, NegDouble.ensuringValid(NegDouble.MaxValue - 1E292 - 1E292))
            max <- negDoublesBetween(min, NegDouble.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[NegDouble] = negDoublesBetween(min, max)
          val (edges, _) = minMaxGen.initEdges(100, Randomizer.default)
          edges.length should (be >= 1 or be <= 7)
          edges should contain (min)
          edges should contain (max)
        }
      }

      "returns a generator whose initEdges method includes normal PosDouble edges only if they are between min and max, inclusive" in {

        import org.scalatest.Inspectors._

        val (edges, rnd1) = posDoubles.initEdges(100, Randomizer.default)
        val sortedEdges = edges.sorted
        val combos = sortedEdges.combinations(2).toList

        def included(from: PosDouble, to: PosDouble): List[PosDouble] = {
          val fromIdx = sortedEdges.indexOf(from)
          val toIdx = sortedEdges.indexOf(to)
          sortedEdges.drop(fromIdx).take(toIdx - fromIdx + 1)
        }

        forAll (combos) { case List(from, to) =>
          val requiredEdges = included(from, to)
          val minMaxGen: Generator[PosDouble] = posDoublesBetween(from, to)
          val (edges, _) = minMaxGen.initEdges(100, Randomizer.default)
          edges should contain allElementsOf requiredEdges
          val outOfBoundsEdges = sortedEdges.filter(i => i < from || i > to)
          edges should contain noElementsOf outOfBoundsEdges
        }
      }
    }

    "offer a negZIntsBetween method" that {
      val NegZIntMaxValueMinusOne = NegZInt(-1)
      "throws IAE if max is less than min" in {
        val loHiPairs =
          for {
            lo <- negZIntsBetween(NegZInt.MinValue, NegZInt.ensuringValid(NegZInt.MaxValue - 1))
            hi <- negZIntsBetween(NegZInt.ensuringValid(lo + 1), NegZInt.MaxValue)
          } yield (lo, hi)
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        forAll (loHiPairs) { case (lo, hi) =>
          an [IllegalArgumentException] should be thrownBy {
            negZIntsBetween(hi, lo)
          }
        }
      }
      "produces NegZInts between min and max" in {

        import org.scalatest.prop.GeneratorDrivenPropertyChecks._

        val minMaxPairs: Generator[(NegZInt, NegZInt)] =
          for {
            min <- negZIntsBetween(NegZInt.MinValue, NegZIntMaxValueMinusOne)
            max <- negZIntsBetween(min, NegZInt.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[NegZInt] = negZIntsBetween(min, max)
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

        val minMaxPairs: Generator[(NegZInt, NegZInt)] =
          for {
            min <- negZIntsBetween(NegZInt.MinValue, NegZIntMaxValueMinusOne)
            max <- negZIntsBetween(min, NegZInt.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[NegZInt] = negZIntsBetween(min, max)
          val (edges, _) = minMaxGen.initEdges(100, Randomizer.default)
          edges should (have length 1 or have length 2 or have length 3)
          edges should contain (min)
          edges should contain (max)
        }
      }
      "returns a generator whose initEdges method includes normal NegZInt edges only if they are between min and max, inclusive" in {

        import org.scalatest.Inspectors._

        val (edges, rnd1) = negZInts.initEdges(100, Randomizer.default)
        val sortedEdges = edges.sorted
        val combos = sortedEdges.combinations(2).toList

        def included(from: NegZInt, to: NegZInt): List[NegZInt] = {
          val fromIdx = sortedEdges.indexOf(from)
          val toIdx = sortedEdges.indexOf(to)
          sortedEdges.drop(fromIdx).take(toIdx - fromIdx + 1)
        }

        forAll (combos) { case List(from, to) =>
          val requiredEdges = included(from, to)
          val minMaxGen: Generator[NegZInt] = negZIntsBetween(from, to)
          val (edges, _) = minMaxGen.initEdges(100, Randomizer.default)
          edges should contain allElementsOf requiredEdges
          val outOfBoundsEdges = sortedEdges.filter(i => i < from || i > to)
          edges should contain noElementsOf outOfBoundsEdges
        }
      }
    }

    "offer a negZLongsBetween method" that {
      "throws IAE if max is less than min" in {
        val loHiPairs =
          for {
            lo <- negZLongsBetween(NegZLong.MinValue, NegZLong.ensuringValid(NegLong.MaxValue - 1)) // Hmm. Using the method to test itself
            hi <- negZLongsBetween(NegZLong.ensuringValid(lo + 1), NegZLong.MaxValue)
          } yield (lo, hi)
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        forAll (loHiPairs) { case (lo, hi) =>
          an [IllegalArgumentException] should be thrownBy {
            negZLongsBetween(hi, lo)
          }
        }
      }
      "produces NegZLongs between min and max" in {

        import org.scalatest.prop.GeneratorDrivenPropertyChecks._

        val minMaxPairs: Generator[(NegZLong, NegZLong)] =
          for {
            min <- negZLongsBetween(NegZLong.MinValue, NegZLong.ensuringValid(NegLong.MaxValue - 1))
            max <- negZLongsBetween(min, NegZLong.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[NegZLong] = negZLongsBetween(min, max)
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

        val minMaxPairs: Generator[(NegZLong, NegZLong)] =
          for {
            min <- negZLongsBetween(NegZLong.MinValue, NegZLong.ensuringValid(NegZLong.MaxValue - 1))
            max <- negZLongsBetween(min, NegZLong.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[NegZLong] = negZLongsBetween(min, max)
          val (edges, _) = minMaxGen.initEdges(100, Randomizer.default)
          edges.length should (be >= 1 or be <= 7)
          edges should contain (min)
          edges should contain (max)
        }
      }
      "returns a generator whose initEdges method includes normal NegZLong edges only if they are between min and max, inclusive" in {

        import org.scalatest.Inspectors._

        val (edges, rnd1) = negZLongs.initEdges(100, Randomizer.default)
        val sortedEdges = edges.sorted
        val combos = sortedEdges.combinations(2).toList

        def included(from: NegZLong, to: NegZLong): List[NegZLong] = {
          val fromIdx = sortedEdges.indexOf(from)
          val toIdx = sortedEdges.indexOf(to)
          sortedEdges.drop(fromIdx).take(toIdx - fromIdx + 1)
        }

        forAll (combos) { case List(from, to) =>
          val requiredEdges = included(from, to)
          val minMaxGen: Generator[NegZLong] = negZLongsBetween(from, to)
          val (edges, _) = minMaxGen.initEdges(100, Randomizer.default)
          edges should contain allElementsOf requiredEdges
          val outOfBoundsEdges = sortedEdges.filter(i => i < from || i > to)
          edges should contain noElementsOf outOfBoundsEdges
        }
      }
    }

    "offer a negZFloatsBetween method" that {
      "throws IAE if max is less than min" in {
        val loHiPairs =
          for {
            lo <- negZFloatsBetween(NegZFloat.MinValue, NegZFloat.ensuringValid(NegZFloat.MaxValue - 1E32f - 1E32f)) // Hmm. Using the method to test itself
            hi <- negZFloatsBetween(NegZFloat.ensuringValid(lo + 1E32f), NegZFloat.MaxValue)
          } yield (lo, hi)
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        forAll (loHiPairs) { case (lo, hi) =>
          an[IllegalArgumentException] should be thrownBy {
            negZFloatsBetween(hi, lo)
          }
        }
      }
      "produces NegZFloats between min and max" in {

        import org.scalatest.prop.GeneratorDrivenPropertyChecks._

        val minMaxPairs: Generator[(NegZFloat, NegZFloat)] =
          for {
            min <- negZFloatsBetween(NegZFloat.MinValue, NegZFloat.ensuringValid(NegZFloat.MaxValue - 1E32f - 1E32f))
            max <- negZFloatsBetween(min, NegZFloat.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[NegZFloat] = negZFloatsBetween(min, max)
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

        val minMaxPairs: Generator[(NegZFloat, NegZFloat)] =
          for {
            min <- negZFloatsBetween(NegZFloat.MinValue, NegZFloat.ensuringValid(NegZFloat.MaxValue - 1E32f))
            max <- negZFloatsBetween(min, NegZFloat.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[NegZFloat] = negZFloatsBetween(min, max)
          val (edges, _) = minMaxGen.initEdges(100, Randomizer.default)
          edges.length should (be >= 1 or be <= 7)
          edges should contain (min)
          edges should contain (max)
        }
      }
      "returns a generator whose initEdges method includes normal NegZFloat edges only if they are between min and max, inclusive" in {

        import org.scalatest.Inspectors._

        val (edges, rnd1) = negZFloats.initEdges(100, Randomizer.default)
        val sortedEdges = edges.sorted
        val combos = sortedEdges.combinations(2).toList

        def included(from: NegZFloat, to: NegZFloat): List[NegZFloat] = {
          val fromIdx = sortedEdges.indexOf(from)
          val toIdx = sortedEdges.indexOf(to)
          sortedEdges.drop(fromIdx).take(toIdx - fromIdx + 1)
        }

        forAll (combos) { case List(from, to) =>
          val requiredEdges = included(from, to)
          val minMaxGen: Generator[NegZFloat] = negZFloatsBetween(from, to)
          val (edges, _) = minMaxGen.initEdges(100, Randomizer.default)
          edges should contain allElementsOf requiredEdges
          val outOfBoundsEdges = sortedEdges.filter(i => i < from || i > to)
          edges should contain noElementsOf outOfBoundsEdges
        }
      }
    }

    "offer a negZDoublesBetween method" that {
      "throws IAE if max is less than min" in {
        val loHiPairs =
          for {
            lo <- negZDoublesBetween(NegZDouble.MinValue, NegZDouble.ensuringValid(NegZDouble.MaxValue - 1E292)) // Hmm. Using the method to test itself
            hi <- negZDoublesBetween(NegZDouble.ensuringValid(lo + 1E292), NegZDouble.MaxValue)
          } yield (lo, hi)
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        forAll (loHiPairs) { case (lo, hi) =>
          an[IllegalArgumentException] should be thrownBy {
            negZDoublesBetween(hi, lo)
          }
        }
      }
      "produces NegDoubles between min and max" in {

        import org.scalatest.prop.GeneratorDrivenPropertyChecks._

        val minMaxPairs: Generator[(NegZDouble, NegZDouble)] =
          for {
            min <- negZDoublesBetween(NegZDouble.MinValue, NegZDouble.ensuringValid(NegZDouble.MaxValue - 1E292))
            max <- negZDoublesBetween(min, NegZDouble.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[NegZDouble] = negZDoublesBetween(min, max)
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

        val minMaxPairs: Generator[(NegZDouble, NegZDouble)] =
          for {
            min <- negZDoublesBetween(NegZDouble.MinValue, NegZDouble.ensuringValid(NegZDouble.MaxValue - 1E292))
            max <- negZDoublesBetween(min, NegZDouble.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[NegZDouble] = negZDoublesBetween(min, max)
          val (edges, _) = minMaxGen.initEdges(100, Randomizer.default)
          edges.length should (be >= 1 or be <= 7)
          edges should contain (min)
          edges should contain (max)
        }
      }

      "returns a generator whose initEdges method includes normal NegZDouble edges only if they are between min and max, inclusive" in {

        import org.scalatest.Inspectors._

        val (edges, rnd1) = negZDoubles.initEdges(100, Randomizer.default)
        val sortedEdges = edges.sorted
        val combos = sortedEdges.combinations(2).toList

        def included(from: NegZDouble, to: NegZDouble): List[NegZDouble] = {
          val fromIdx = sortedEdges.indexOf(from)
          val toIdx = sortedEdges.indexOf(to)
          sortedEdges.drop(fromIdx).take(toIdx - fromIdx + 1)
        }

        forAll (combos) { case List(from, to) =>
          val requiredEdges = included(from, to)
          val minMaxGen: Generator[NegZDouble] = negZDoublesBetween(from, to)
          val (edges, _) = minMaxGen.initEdges(100, Randomizer.default)
          edges should contain allElementsOf requiredEdges
          val outOfBoundsEdges = sortedEdges.filter(i => i < from || i > to)
          edges should contain noElementsOf outOfBoundsEdges
        }
      }
    }

    "offer a nonZeroIntsBetween method" that {
      val NonZeroIntMaxValueMinusOne = PosInt(2147483646)

      "throws IAE if max is less than min" in {
        val loHiPairs =
          for {
            lo <- nonZeroIntsBetween(NonZeroInt.MinValue, NonZeroInt.ensuringValid(NonZeroInt.MaxValue - 1))
            hi <- nonZeroIntsBetween(if (lo.value == -1) NonZeroInt(1) else NonZeroInt.ensuringValid(lo + 1), NonZeroInt.MaxValue)
          } yield (lo, hi)
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        forAll (loHiPairs) { case (lo, hi) =>
          an [IllegalArgumentException] should be thrownBy {
            nonZeroIntsBetween(hi, lo)
          }
        }
      }

      "produces NonZeroInts between min and max" in {

        import org.scalatest.prop.GeneratorDrivenPropertyChecks._

        val minMaxPairs: Generator[(NonZeroInt, NonZeroInt)] =
          for {
            min <- nonZeroIntsBetween(NonZeroInt.MinValue, NonZeroIntMaxValueMinusOne)
            max <- nonZeroIntsBetween(min, NonZeroInt.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[NonZeroInt] = nonZeroIntsBetween(min, max)
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

        val minMaxPairs: Generator[(NonZeroInt, NonZeroInt)] =
          for {
            min <- nonZeroIntsBetween(NonZeroInt.MinValue, NonZeroIntMaxValueMinusOne)
            max <- nonZeroIntsBetween(min, NonZeroInt.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[NonZeroInt] = nonZeroIntsBetween(min, max)
          val (edges, _) = minMaxGen.initEdges(100, Randomizer.default)
          edges should (have length 1 or have length 2 or have length 3 or have length 4)
          edges should contain (min)
          edges should contain (max)
        }
      }
      "returns a generator whose initEdges method includes normal NonZeroInt edges only if they are between min and max, inclusive" in {

        import org.scalatest.Inspectors._

        val (edges, rnd1) = nonZeroInts.initEdges(100, Randomizer.default)
        val sortedEdges = edges.sorted
        val combos = sortedEdges.combinations(2).toList

        def included(from: NonZeroInt, to: NonZeroInt): List[NonZeroInt] = {
          val fromIdx = sortedEdges.indexOf(from)
          val toIdx = sortedEdges.indexOf(to)
          sortedEdges.drop(fromIdx).take(toIdx - fromIdx + 1)
        }

        forAll (combos) { case List(from, to) =>
          val requiredEdges = included(from, to)
          val minMaxGen: Generator[NonZeroInt] = nonZeroIntsBetween(from, to)
          val (edges, _) = minMaxGen.initEdges(100, Randomizer.default)
          edges should contain allElementsOf requiredEdges
          val outOfBoundsEdges = sortedEdges.filter(i => i < from || i > to)
          edges should contain noElementsOf outOfBoundsEdges
        }
      }
    }

    "offer a nonZeroLongsBetween method" that {
      "throws IAE if max is less than min" in {
        val loHiPairs =
          for {
            lo <- nonZeroLongsBetween(NonZeroLong.MinValue, NonZeroLong.ensuringValid(NonZeroLong.MaxValue - 1L)) // Hmm. Using the method to test itself
            hi <- nonZeroLongsBetween(if (lo.value == -1L) NonZeroLong(1L) else NonZeroLong.ensuringValid(lo + 1L), NonZeroLong.MaxValue)
          } yield (lo, hi)
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        forAll (loHiPairs) { case (lo, hi) =>
          an [IllegalArgumentException] should be thrownBy {
            nonZeroLongsBetween(hi, lo)
          }
        }
      }
      "produces NonZeroLongs between min and max" in {

        import org.scalatest.prop.GeneratorDrivenPropertyChecks._

        val minMaxPairs: Generator[(NonZeroLong, NonZeroLong)] =
          for {
            min <- nonZeroLongsBetween(NonZeroLong.MinValue, NonZeroLong.ensuringValid(NonZeroLong.MaxValue - 1))
            max <- nonZeroLongsBetween(min, NonZeroLong.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[NonZeroLong] = nonZeroLongsBetween(min, max)
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

        val minMaxPairs: Generator[(NonZeroLong, NonZeroLong)] =
          for {
            min <- nonZeroLongsBetween(NonZeroLong.MinValue, NonZeroLong.ensuringValid(NonZeroLong.MaxValue - 1L))
            max <- nonZeroLongsBetween(min, NonZeroLong.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[NonZeroLong] = nonZeroLongsBetween(min, max)
          val (edges, _) = minMaxGen.initEdges(100, Randomizer.default)
          edges.length should (be >= 1 or be <= 7)
          edges should contain (min)
          edges should contain (max)
        }
      }
      "returns a generator whose initEdges method includes normal NonZeroLong edges only if they are between min and max, inclusive" in {

        import org.scalatest.Inspectors._

        val (edges, rnd1) = nonZeroLongs.initEdges(100, Randomizer.default)
        val sortedEdges = edges.sorted
        val combos = sortedEdges.combinations(2).toList

        def included(from: NonZeroLong, to: NonZeroLong): List[NonZeroLong] = {
          val fromIdx = sortedEdges.indexOf(from)
          val toIdx = sortedEdges.indexOf(to)
          sortedEdges.drop(fromIdx).take(toIdx - fromIdx + 1)
        }

        forAll (combos) { case List(from, to) =>
          val requiredEdges = included(from, to)
          val minMaxGen: Generator[NonZeroLong] = nonZeroLongsBetween(from, to)
          val (edges, _) = minMaxGen.initEdges(100, Randomizer.default)
          edges should contain allElementsOf requiredEdges
          val outOfBoundsEdges = sortedEdges.filter(i => i < from || i > to)
          edges should contain noElementsOf outOfBoundsEdges
        }
      }
    }

    "offer a nonZeroFloatsBetween method" that {
      "throws IAE if max is less than min" in {
        val loHiPairs =
          for {
            lo <- nonZeroFloatsBetween(NonZeroFloat.MinValue, NonZeroFloat.ensuringValid(NonZeroFloat.MaxValue - 1E32f))
            hi <- nonZeroFloatsBetween(if (lo + 1E32f == 0.0f) NonZeroFloat(1.0f) else NonZeroFloat.ensuringValid(lo + 1E32f), NonZeroFloat.MaxValue)
          } yield (lo, hi)
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        forAll (loHiPairs) { case (lo, hi) =>
          an[IllegalArgumentException] should be thrownBy {
            nonZeroFloatsBetween(hi, lo)
          }
        }
      }
      "produces PosFloats between min and max" in {

        import org.scalatest.prop.GeneratorDrivenPropertyChecks._

        val minMaxPairs: Generator[(NonZeroFloat, NonZeroFloat)] =
          for {
            min <- nonZeroFloatsBetween(NonZeroFloat.MinValue, NonZeroFloat.ensuringValid(NonZeroFloat.MaxValue - 1E32f))
            max <- nonZeroFloatsBetween(min, NonZeroFloat.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[NonZeroFloat] = nonZeroFloatsBetween(min, max)
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

        val minMaxPairs: Generator[(NonZeroFloat, NonZeroFloat)] =
          for {
            min <- nonZeroFloatsBetween(NonZeroFloat.MinValue, NonZeroFloat.ensuringValid(NonZeroFloat.MaxValue - 1E32f))
            max <- nonZeroFloatsBetween(min, NonZeroFloat.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[NonZeroFloat] = nonZeroFloatsBetween(min, max)
          val (edges, _) = minMaxGen.initEdges(100, Randomizer.default)
          edges.length should (be >= 1 or be <= 7)
          edges should contain (min)
          edges should contain (max)
        }
      }
      "returns a generator whose initEdges method includes normal NonZeroFloat edges only if they are between min and max, inclusive" in {

        import org.scalatest.Inspectors._

        val (edges, rnd1) = nonZeroFloats.initEdges(100, Randomizer.default)
        val sortedEdges = edges.sorted
        val combos = sortedEdges.combinations(2).toList

        def included(from: NonZeroFloat, to: NonZeroFloat): List[NonZeroFloat] = {
          val fromIdx = sortedEdges.indexOf(from)
          val toIdx = sortedEdges.indexOf(to)
          sortedEdges.drop(fromIdx).take(toIdx - fromIdx + 1)
        }

        forAll (combos) { case List(from, to) =>
          val requiredEdges = included(from, to)
          val minMaxGen: Generator[NonZeroFloat] = nonZeroFloatsBetween(from, to)
          val (edges, _) = minMaxGen.initEdges(100, Randomizer.default)
          edges should contain allElementsOf requiredEdges
          val outOfBoundsEdges = sortedEdges.filter(i => i < from || i > to)
          edges should contain noElementsOf outOfBoundsEdges
        }
      }
    }

    "offer a nonZeroDoublesBetween method" that {
      "throws IAE if max is less than min" in {
        val loHiPairs =
          for {
            lo <- nonZeroDoublesBetween(NonZeroDouble.MinValue, NonZeroDouble.ensuringValid(NonZeroDouble.MaxValue - 1E292))
            hi <- nonZeroDoublesBetween(if (lo + 1E292 == 0.0) NonZeroDouble(1.0) else NonZeroDouble.ensuringValid(lo + 1E292), NonZeroDouble.MaxValue)
          } yield (lo, hi)
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        forAll (loHiPairs) { case (lo, hi) =>
          an[IllegalArgumentException] should be thrownBy {
            nonZeroDoublesBetween(hi, lo)
          }
        }
      }
      "produces NonZeroDoubles between min and max" in {

        import org.scalatest.prop.GeneratorDrivenPropertyChecks._

        val minMaxPairs: Generator[(NonZeroDouble, NonZeroDouble)] =
          for {
            min <- nonZeroDoublesBetween(NonZeroDouble.MinValue, NonZeroDouble.ensuringValid(NonZeroDouble.MaxValue - 1E292))
            max <- nonZeroDoublesBetween(min, NonZeroDouble.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[NonZeroDouble] = nonZeroDoublesBetween(min, max)
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

        val minMaxPairs: Generator[(NonZeroDouble, NonZeroDouble)] =
          for {
            min <- nonZeroDoublesBetween(NonZeroDouble.MinValue, NonZeroDouble.ensuringValid(NonZeroDouble.MaxValue - 1E292))
            max <- nonZeroDoublesBetween(min, NonZeroDouble.MaxValue)
          } yield (min, max)

        forAll (minMaxPairs) { case (min, max) =>
          val minMaxGen: Generator[NonZeroDouble] = nonZeroDoublesBetween(min, max)
          val (edges, _) = minMaxGen.initEdges(100, Randomizer.default)
          edges.length should (be >= 1 or be <= 7)
          edges should contain (min)
          edges should contain (max)
        }
      }

      "returns a generator whose initEdges method includes normal NonZeroDouble edges only if they are between min and max, inclusive" in {

        import org.scalatest.Inspectors._

        val (edges, rnd1) = nonZeroDoubles.initEdges(100, Randomizer.default)
        val sortedEdges = edges.sorted
        val combos = sortedEdges.combinations(2).toList

        def included(from: NonZeroDouble, to: NonZeroDouble): List[NonZeroDouble] = {
          val fromIdx = sortedEdges.indexOf(from)
          val toIdx = sortedEdges.indexOf(to)
          sortedEdges.drop(fromIdx).take(toIdx - fromIdx + 1)
        }

        forAll (combos) { case List(from, to) =>
          val requiredEdges = included(from, to)
          val minMaxGen: Generator[NonZeroDouble] = nonZeroDoublesBetween(from, to)
          val (edges, _) = minMaxGen.initEdges(100, Randomizer.default)
          edges should contain allElementsOf requiredEdges
          val outOfBoundsEdges = sortedEdges.filter(i => i < from || i > to)
          edges should contain noElementsOf outOfBoundsEdges
        }
      }
    }

    "offer a specificValues method" that {
      "returns a generator that produces from a given set of specific objects for any type T" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val values123 = specificValues(1, 2, 3)
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
        val specificValue42 = specificValue(42)
        forAll (specificValue42) { x => x shouldBe (42) }
        forAll (specificValue("nice")) { x =>
          x should (be ("nice"))
        }
      }
    }
    "offer a frequency method that takes a varargs of Int weights to generators and produces a generator" that {
      "returns values from each specific generator with a probability determined by the weights" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        // import specificValue
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
      "throws IllegalArgumentException if passed less than two arguments" in {
        "frequency()" shouldNot compile
        "frequency(1 -> ints)" shouldNot compile
        "frequency(1 -> intsBetween(1, 10), 5 -> specificValue(0))" should compile
      }
    }
    "offer an evenly method that takes a varargs of generators and produces a generator" that {
      "returns values from each specific generator with an equal probability" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        sealed trait Numero extends Product with Serializable {
          val value: Int
        }
        case class Uno(value: Int) extends Numero
        case class Dos(value: Int) extends Numero
        case class Tres(value: Int) extends Numero
	case class Quatro(value: Int) extends Numero
        case class Cinco(value: Int) extends Numero

        val unos = instancesOf(Uno) { uno => uno.value }
        val doses = instancesOf(Dos) { dos => dos.value }
        val treses = instancesOf(Tres) { tres => tres.value }
        val quatros = instancesOf(Quatro) { quatro => quatro.value }
        val cincos = instancesOf(Cinco) { cinco => cinco.value }

        // val numeros: Generator[Numero] = evenly(unos, doses, treses, quatros, cincos)
        val numeros = evenly(unos, doses, treses, quatros, cincos)

        val classification: Classification =
          classify(10000, numeros) {
            case _: Uno => "uno"
            case _: Dos => "dos"
            case _: Tres => "tres"
            case _: Quatro => "quatro"
            case _: Cinco => "cinco"
          }
 
        val percentages: Map[String, PosZInt] = classification.percentages

        percentages("uno").value shouldBe 20 +- 1
        percentages("dos").value shouldBe 20 +- 1
        percentages("tres").value shouldBe 20 +- 1
        percentages("quatro").value shouldBe 20 +- 1
        percentages("cinco").value shouldBe 20 +- 1
      }
      "throws IllegalArgumentException if passed less than two arguments" in {
        "evenly()" shouldNot compile
        "evenly(ints)" shouldNot compile
        "evenly(intsBetween(1, 10), specificValue(0))" should compile
      }
    }
    def samplesForGen[T](genOfT: Generator[T], desiredLength: PosInt, originalRnd: Randomizer): List[T] = {         
      @tailrec                                       
      def samplesLoop(count: Int, rnd: Randomizer, acc: List[T]): List[T] = {
        if (count == desiredLength.value) acc
        else {
          val maxSize = PosZInt(100)
          val (size, nextRnd) = rnd.chooseInt(1, maxSize)
          val (value, _, nextNextRnd) = genOfT.next(PosZInt.ensuringValid(size), maxSize, Nil, rnd) 
          samplesLoop(count + 1, nextNextRnd, value :: acc)
        } 
      }
      samplesLoop(100, originalRnd, Nil)
    }

    "offer a bytes method" that {
      "returns the default implicit generator that produces arbitrary Bytes" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[Byte]]
        val namedGen = bytes
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
        val namedGen = shorts
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
        val namedGen = ints
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
        val namedGen = longs
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
        val namedGen = chars
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
        val namedGen = floats
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
        val namedGen = doubles
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
        val namedGen = strings
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
        val namedGen = lists[Int]
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
      "returns a type that also offers a havingLengthsBetween method that provides a generator for lists of a range of lengths" in {

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
      "returns a type that also offers a havingLengthsDeterminedBy method that provides a generator for lists whose length is determined by a function" in {

        import org.scalatest.prop.GeneratorDrivenPropertyChecks._

        val upperLimits: Generator[PosZInt] = posZIntsBetween(0, 99)

        forAll (upperLimits) { upperLimit => 
          val lengthlimitedLists = lists[Int].havingLengthsDeterminedBy(_ => upperLimit)
          forAll (lengthlimitedLists) { xs => xs.length shouldBe upperLimit.value }
        }
      }
    }
    "offer a posInts method" that {
      "returns the default implicit generator that produces arbitrary PosInts" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[PosInt]]
        val namedGen = posInts
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
        val namedGen = posZInts
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
        val namedGen = posLongs
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
        val namedGen = posZLongs
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
        val namedGen = posFloats
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
        val namedGen = posZFloats
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
        val namedGen = posDoubles
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
        val namedGen = posZDoubles
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
        val namedGen = posIntValues
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
        val namedGen = posZIntValues
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
        val namedGen = posLongValues
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
        val namedGen = posZLongValues
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
        val namedGen = posFloatValues
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
        val namedGen = posZFloatValues
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
        val namedGen = posDoubleValues
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
        val namedGen = posZDoubleValues
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges.map(_.value) shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples.map(_.value) shouldEqual namedGenSamples
      }
    }
    "offer a negInts method" that {
      "returns the default implicit generator that produces arbitrary NegInts" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[NegInt]]
        val namedGen = negInts
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples shouldEqual namedGenSamples
      }
    }
    "offer a negZInts method" that {
      "returns the default implicit generator that produces arbitrary NegZInts" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[NegZInt]]
        val namedGen = negZInts
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples shouldEqual namedGenSamples
      }
    }
    "offer a negLongs method" that {
      "returns the default implicit generator that produces arbitrary NegLongs" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[NegLong]]
        val namedGen = negLongs
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples shouldEqual namedGenSamples
      }
    }
    "offer a negZLongs method" that {
      "returns the default implicit generator that produces arbitrary NegZLongs" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[NegZLong]]
        val namedGen = negZLongs
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples shouldEqual namedGenSamples
      }
    }
    "offer a negFloats method" that {
      "returns the default implicit generator that produces arbitrary NegFloats" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[NegFloat]]
        val namedGen = negFloats
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples shouldEqual namedGenSamples
      }
    }
    "offer a negZFloats method" that {
      "returns the default implicit generator that produces arbitrary NegZFloats" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[NegZFloat]]
        val namedGen = negZFloats
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples shouldEqual namedGenSamples
      }
    }
    "offer a negDoubles method" that {
      "returns the default implicit generator that produces arbitrary NegDoubles" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[NegDouble]]
        val namedGen = negDoubles
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples shouldEqual namedGenSamples
      }
    }
    "offer a negZDoubles method" that {
      "returns the default implicit generator that produces arbitrary NegZDoubles" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[NegZDouble]]
        val namedGen = negZDoubles
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples shouldEqual namedGenSamples
      }
    }
    "offer a negIntValues method" that {
      "returns the default implicit generator that produces arbitrary NegInts" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[NegInt]]
        val namedGen = negIntValues
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges.map(_.value) shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples.map(_.value) shouldEqual namedGenSamples
      }
    }
    "offer a negZIntValues method" that {
      "returns the default implicit generator that produces arbitrary zero and NegZInts" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[NegZInt]]
        val namedGen = negZIntValues
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges.map(_.value) shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples.map(_.value) shouldEqual namedGenSamples
      }
    }
    "offer a negLongValues method" that {
      "returns the default implicit generator that produces arbitrary NegLongs" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[NegLong]]
        val namedGen = negLongValues
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges.map(_.value) shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples.map(_.value) shouldEqual namedGenSamples
      }
    }
    "offer a negZLongValues method" that {
      "returns the default implicit generator that produces arbitrary zero and NegZLongs" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[NegZLong]]
        val namedGen = negZLongValues
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges.map(_.value) shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples.map(_.value) shouldEqual namedGenSamples
      }
    }
    "offer a negFloatValues method" that {
      "returns the default implicit generator that produces arbitrary NegFloats" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[NegFloat]]
        val namedGen = negFloatValues
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges.map(_.value) shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples.map(_.value) shouldEqual namedGenSamples
      }
    }
    "offer a negZFloatValues method" that {
      "returns the default implicit generator that produces arbitrary zero and NegZFloats" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[NegZFloat]]
        val namedGen = negZFloatValues
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges.map(_.value) shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples.map(_.value) shouldEqual namedGenSamples
      }
    }
    "offer a negDoubleValues method" that {
      "returns the default implicit generator that produces arbitrary NegDoubles" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[NegDouble]]
        val namedGen = negDoubleValues
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges.map(_.value) shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples.map(_.value) shouldEqual namedGenSamples
      }
    }
    "offer a negZDoubleValues method" that {
      "returns the default implicit generator that produces arbitrary zero and NegZDoubles" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[NegZDouble]]
        val namedGen = negZDoubleValues
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges.map(_.value) shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples.map(_.value) shouldEqual namedGenSamples
      }
    }
    "offer a nonZeroInts method" that {
      "returns the default implicit generator that produces arbitrary NonZeroInts" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[NonZeroInt]]
        val namedGen = nonZeroInts
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples shouldEqual namedGenSamples
      }
    }
    "offer a nonZeroLongs method" that {
      "returns the default implicit generator that produces arbitrary NonZeroLongs" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[NonZeroLong]]
        val namedGen = nonZeroLongs
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples shouldEqual namedGenSamples
      }
    }
    "offer a nonZeroFloats method" that {
      "returns the default implicit generator that produces arbitrary NonZeroFloats" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[NonZeroFloat]]
        val namedGen = nonZeroFloats
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples shouldEqual namedGenSamples
      }
    }
    "offer a nonZeroDoubles method" that {
      "returns the default implicit generator that produces arbitrary NonZeroDoubles" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[NonZeroDouble]]
        val namedGen = nonZeroDoubles
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples shouldEqual namedGenSamples
      }
    }
    "offer a nonZeroIntValues method" that {
      "returns the default implicit generator that produces arbitrary NonZeroInts" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[NonZeroInt]]
        val namedGen = nonZeroIntValues
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges.map(_.value) shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples.map(_.value) shouldEqual namedGenSamples
      }
    }
    "offer a nonZeroLongValues method" that {
      "returns the default implicit generator that produces arbitrary NonZeroLongs" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[NonZeroLong]]
        val namedGen = nonZeroLongValues
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges.map(_.value) shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples.map(_.value) shouldEqual namedGenSamples
      }
    }
    "offer a nonZeroFloatValues method" that {
      "returns the default implicit generator that produces arbitrary NonZeroFloats" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[NonZeroFloat]]
        val namedGen = nonZeroFloatValues
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges.map(_.value) shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples.map(_.value) shouldEqual namedGenSamples
      }
    }
    "offer a nonZeroDoubleValues method" that {
      "returns the default implicit generator that produces arbitrary NonZeroDoubles" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[NonZeroDouble]]
        val namedGen = nonZeroDoubleValues
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
        val namedGen = tuple2s[String, Int]
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
        val namedGen = tuple3s[String, Int, Long]
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples shouldEqual namedGenSamples
      }
    }
    "offer a tuple4s method" that {
      "returns the default implicit generator that produces arbitrary Tuple4s" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[(String, Int, Long, Float)]]
        val namedGen = tuple4s[String, Int, Long, Float]
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples shouldEqual namedGenSamples
      }
    }
    "offer a tuple5s method" that {
      "returns the default implicit generator that produces arbitrary Tuple5s" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[(String, Int, Long, Float, Double)]]
        val namedGen = tuple5s[String, Int, Long, Float, Double]
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples shouldEqual namedGenSamples
      }
    }
    "offer a tuple6s method" that {
      "returns the default implicit generator that produces arbitrary Tuple6s" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[(String, Int, Long, Float, Double, String)]]
        val namedGen = tuple6s[String, Int, Long, Float, Double, String]
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples shouldEqual namedGenSamples
      }
    }
    "offer a tuple7s method" that {
      "returns the default implicit generator that produces arbitrary Tuple7s" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[(String, Int, Long, Float, Double, String, Int)]]
        val namedGen = tuple7s[String, Int, Long, Float, Double, String, Int]
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples shouldEqual namedGenSamples
      }
    }
    "offer a tuple8s method" that {
      "returns the default implicit generator that produces arbitrary Tuple8s" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[(String, Int, Long, Float, Double, String, Int, Long)]]
        val namedGen = tuple8s[String, Int, Long, Float, Double, String, Int, Long]
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples shouldEqual namedGenSamples
      }
    }
    "offer a tuple9s method" that {
      "returns the default implicit generator that produces arbitrary Tuple9s" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[(String, Int, Long, Float, Double, String, Int, Long, Float)]]
        val namedGen = tuple9s[String, Int, Long, Float, Double, String, Int, Long, Float]
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples shouldEqual namedGenSamples
      }
    }
    "offer a tuple10s method" that {
      "returns the default implicit generator that produces arbitrary Tuple10s" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[(String, Int, Long, Float, Double, String, Int, Long, Float, Double)]]
        val namedGen = tuple10s[String, Int, Long, Float, Double, String, Int, Long, Float, Double]
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples shouldEqual namedGenSamples
      }
    }
    "offer a tuple11s method" that {
      "returns the default implicit generator that produces arbitrary Tuple11s" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[(String, Int, Long, Float, Double, String, Int, Long, Float, Double, String)]]
        val namedGen = tuple11s[String, Int, Long, Float, Double, String, Int, Long, Float, Double, String]
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples shouldEqual namedGenSamples
      }
    }
    "offer a tuple12s method" that {
      "returns the default implicit generator that produces arbitrary Tuple12s" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[(String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int)]]
        val namedGen = tuple12s[String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int]
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples shouldEqual namedGenSamples
      }
    }
    "offer a tuple13s method" that {
      "returns the default implicit generator that produces arbitrary Tuple13s" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[(String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long)]]
        val namedGen = tuple13s[String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long]
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples shouldEqual namedGenSamples
      }
    }
    "offer a tuple14s method" that {
      "returns the default implicit generator that produces arbitrary Tuple14s" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[(String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long, Float)]]
        val namedGen = tuple14s[String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long, Float]
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples shouldEqual namedGenSamples
      }
    }
    "offer a tuple15s method" that {
      "returns the default implicit generator that produces arbitrary Tuple15s" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[(String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long, Float, Double)]]
        val namedGen = tuple15s[String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long, Float, Double]
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples shouldEqual namedGenSamples
      }
    }
    "offer a tuple16s method" that {
      "returns the default implicit generator that produces arbitrary Tuple16s" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[(String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long, Float, Double, String)]]
        val namedGen = tuple16s[String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long, Float, Double, String]
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples shouldEqual namedGenSamples
      }
    }
    "offer a tuple17s method" that {
      "returns the default implicit generator that produces arbitrary Tuple17s" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[(String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int)]]
        val namedGen = tuple17s[String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int]
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples shouldEqual namedGenSamples
      }
    }
    "offer a tuple18s method" that {
      "returns the default implicit generator that produces arbitrary Tuple18s" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[(String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long)]]
        val namedGen = tuple18s[String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long]
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples shouldEqual namedGenSamples
      }
    }
    "offer a tuple19s method" that {
      "returns the default implicit generator that produces arbitrary Tuple19s" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[(String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long, Float)]]
        val namedGen = tuple19s[String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long, Float]
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples shouldEqual namedGenSamples
      }
    }
    "offer a tuple20s method" that {
      "returns the default implicit generator that produces arbitrary Tuple20s" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[(String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long, Float, Double)]]
        val namedGen = tuple20s[String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long, Float, Double]
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples shouldEqual namedGenSamples
      }
    }
    "offer a tuple21s method" that {
      "returns the default implicit generator that produces arbitrary Tuple21s" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[(String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long, Float, Double, String)]]
        val namedGen = tuple21s[String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long, Float, Double, String]
        val rnd = Randomizer.default
        val (implicitGenEdges, _) = implicitGen.initEdges(100, rnd)
        val (namedGenEdges, _) = namedGen.initEdges(100, rnd)
        implicitGenEdges shouldEqual namedGenEdges
        val implicitGenSamples = samplesForGen(implicitGen, 100, rnd)
        val namedGenSamples = samplesForGen(namedGen, 100, rnd)
        implicitGenSamples shouldEqual namedGenSamples
      }
    }
    "offer a tuple22s method" that {
      "returns the default implicit generator that produces arbitrary Tuple22s" in {
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        val implicitGen = implicitly[Generator[(String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int)]]
        val namedGen = tuple22s[String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int]
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
        val namedGen = function0s[Int]
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
        val namedGen = function1s[Long, Int]
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
        val namedGen = function2s[Long, String, Int]
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
        val persons = instancesOf(Person) { p => p.age } (posZIntValues)
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        forAll (persons) { case Person(ag) => ag should be >= 0 } // A contrived property check to do something with the generator
      }
      "produces generators given construct and deconstruct functions for 2 types" in {
        case class Person(name: String, age: Int)
        val persons = instancesOf(Person) { p =>
          (p.name, p.age)
        } (strings, posZIntValues)
        import org.scalatest.prop.GeneratorDrivenPropertyChecks._
        forAll (persons) { case Person(_, ag) => ag should be >= 0 } // A contrived property check to do something with the generator
      }
      "produces generators given construct and deconstruct functions for 3 types" in {
        case class Person(name: String, age: Int, attr3: Long)
        val persons = instancesOf(Person) { p =>
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
        val persons = instancesOf(Person) { p =>
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
        val persons = instancesOf(Person) { p =>
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
        val persons = instancesOf(Person) { p =>
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
        val persons = instancesOf(Person) { p =>
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
        val persons = instancesOf(Person) { p =>
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
        val persons = instancesOf(Person) { p =>
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
        val persons = instancesOf(Person) { p =>
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
        val persons = instancesOf(Person) { p =>
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
        val persons = instancesOf(Person) { p =>
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
        val persons = instancesOf(Person) { p =>
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
        val persons = instancesOf(Person) { p =>
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
        val persons = instancesOf(Person) { p =>
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
        val persons = instancesOf(Person) { p =>
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
        val persons = instancesOf(Person) { p =>
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
        val persons = instancesOf(Person) { p =>
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
        val persons = instancesOf(Person) { p =>
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
        val persons = instancesOf(Person) { p =>
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
        val persons = instancesOf(Person) { p =>
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
        val persons = instancesOf(Person) { p =>
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

