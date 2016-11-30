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
  }
}

