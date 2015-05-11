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
import org.scalatest.exceptions.TestFailedException
import scala.collection.mutable.Buffer

class GenDrivenPropertyChecksSpec extends FunSpec with Matchers {
  describe("GenDrivenPropertyChecks") {
    import GenDrivenPropertyChecks._
    it("should provide a forAll that takes one param") {
      forAll { (i: Int) => 
        i + i shouldEqual i * 2
      }
      a [TestFailedException] should be thrownBy {
        forAll { (i: Int) => 
          i + i shouldEqual i * 3
        }
      }
    }
    it("should provide a forAll that takes one param that invokes the generator with increasing size") {
      val sizesBuf = Buffer.empty[Int]
      implicit val generatorDrivenConfig =
        PropertyCheckConfig(minSuccessful = 10, maxDiscarded = 50, minSize = 0, maxSize = 99)
      implicit val intGen: Gen[Int] =
        new Gen[Int] {
          def next(size: Int, rnd: Rnd): (Int, Rnd) = {
            sizesBuf += size
            rnd.nextIntWithEdges
          }
          override def toString = "Gen[Int] that records size"
        }

      forAll { (i: Int) =>
        i + i shouldEqual i * 2
      }
      val sizes: List[Int] = sizesBuf.toList
      sizes should have size 10
      sizes.head should be (0)
      /*
        scala> val xs = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9).sliding(2).toList
        xs: List[List[Int]] = List(List(0, 1), List(1, 2), List(2, 3), List(3, 4), List(4, 5), List(5, 6), List(6, 7), List(7, 8), List(8, 9))

      */
      val pairs: List[(Int, Int)] = sizes.sliding(2).map(xs => (xs(0), xs(1))).toList
      import org.scalatest.Inspectors._
      forAll (pairs) { case (x, y) => x should be <= y }
    }
    it("should provide a forAll that takes two params") {
      import GenDrivenPropertyChecks._
      forAll { (i: Int, j: Int) => 
        i + j shouldEqual j + i
      }
      a [TestFailedException] should be thrownBy {
        forAll { (i: Int, j: Int) => 
          i + j shouldEqual j * i
        }
      }
    }
    it("should provide a forAll that takes three params") {
      import GenDrivenPropertyChecks._
      forAll { (i: Int, j: Int, k: Int) => 
        i + j + k shouldEqual k + j + i
      }
      a [TestFailedException] should be thrownBy {
        forAll { (i: Int, j: Int, k: Int) => 
          i + j + k shouldEqual k * j * i
        }
      }
    }
  }
}

