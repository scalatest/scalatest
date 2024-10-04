/*
 * Copyright 2001-2024 Artima, Inc.
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

import org.scalatest._
import org.scalactic.anyvals._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/*
It looks like sizeRange = maxSize - minSize
*/
class SizeParamSpec extends AnyWordSpec with Matchers with PropertyChecks {
  "A SizeParam" should {
    "have a minimum, range, and size" in {
      val sp = SizeParam(33, 20, 35)
      sp.minSize shouldBe PosZInt(33)
      sp.sizeRange shouldBe PosZInt(20)
      sp.size shouldBe PosZInt(35)
    }
    "offer a factory method in its companion that throws IAE if the size is invalid" in {
      noException should be thrownBy SizeParam(0, 10, 0)
      noException should be thrownBy SizeParam(0, 10, 10)
      noException should be thrownBy SizeParam(1, 10, 1)
      noException should be thrownBy SizeParam(1, 10, 11)
      an [IllegalArgumentException] should be thrownBy SizeParam(0, 10, 11) // minSize 0, maxSize 10
      an [IllegalArgumentException] should be thrownBy SizeParam(1, 10, 0) // minSize 1, maxSize 11,
      an [IllegalArgumentException] should be thrownBy SizeParam(1, 10, 12)
    }
    "offer a maxSize methods" in {
     
       val params =
        for {
          minSz <- posZInts
          maxSz <- posZIntsBetween(minSz, PosZInt.MaxValue)
          sz <- posZIntsBetween(minSz, maxSz)
        } yield (minSz, maxSz, sz)
  
      forAll (params) { case (minSize, sizeRange, size) => 
        whenever(minSize + sizeRange >= 0) {
          val sp = SizeParam(minSize, sizeRange, size)
          sp.sizeRange shouldBe PosZInt.ensuringValid(sp.maxSize - sp.minSize)
        }
      }
    }
  }
}




