/*
 * Copyright 2001-2025 Artima, Inc.
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
package org.scalatest

import matchers.should.Matchers._
import enablers.Length
import enablers.Size

class Lengthiness(val length: Int) extends AnyVal
class Siziness(val size: Int) extends AnyVal

class AnyValMatchersSpec extends funspec.AnyFunSpec {

  describe("Matchers ") {
    it("should allow me to check for length on a custom AnyVal that has a Length type class available") {
      implicit val lengthOfLengthiness: Length[Lengthiness] = new Length[Lengthiness] { def lengthOf(ln: Lengthiness): Long = ln.length }
      new Lengthiness(7) should have length 7
      new Lengthiness(8) should not have length (7)
    }
    it("should allow me to check for size on a custom AnyVal that has a Size type class available") {
      implicit val sizeOfSiziness: Size[Siziness] = new Size[Siziness] { def sizeOf(sz: Siziness): Long = sz.size }
      new Siziness(7) should have size 7
      new Siziness(8) should not have size (7)
    }
    it("should allow me to check for length on a collection of custom AnyVals for which a Length type class available") {
      implicit val lengthOfLengthiness: Length[Lengthiness] = new Length[Lengthiness] { def lengthOf(ln: Lengthiness): Long = ln.length }
      all (List(new Lengthiness(7), new Lengthiness(7))) should have length 7
      all (List(new Lengthiness(8), new Lengthiness(9))) should not have length (7)
    }
    it("should allow me to check for size on a collection of custom AnyVal for which a Size type class available") {
      implicit val sizeOfSiziness: Size[Siziness] = new Size[Siziness] { def sizeOf(sz: Siziness): Long = sz.size }
      all (List(new Siziness(7), new Siziness(7))) should have size 7
      all (List(new Siziness(8), new Siziness(9))) should not have size (7)
    }
  }
}

