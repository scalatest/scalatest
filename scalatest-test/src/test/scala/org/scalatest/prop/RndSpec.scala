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

class RndSpec extends FunSpec with Matchers {

  describe("A Rnd") {
    it("should offer a nextInt method that produces the same values as java.util.Random given the same seed") {
      val jr = new java.util.Random(100)
      val ja = jr.nextInt()
      val jb = jr.nextInt()
      val jc = jr.nextInt()

      val sr = Rnd(100)
      val (ia, ra) = sr.nextInt
      val (ib, rb) = ra.nextInt
      val (ic, _) = rb.nextInt

      ja shouldEqual ia
      jb shouldEqual ib
      jc shouldEqual ic
    }

    it("should offer a nextDouble method that produces the same values as java.util.Random given the same seed") {
      val jr = new java.util.Random(100)
      val ja = jr.nextDouble()
      val jb = jr.nextDouble()
      val jc = jr.nextDouble()

      val sr = Rnd(100)
      val (ia, ra) = sr.nextDouble
      val (ib, rb) = ra.nextDouble
      val (ic, _) = rb.nextDouble

      ja shouldEqual ia
      jb shouldEqual ib
      jc shouldEqual ic
    }
    it("should offer a nextIntWithEdges method that initially produces Int edge values") {
      val r0 = Rnd(100)
      val (a1, r1) = r0.nextIntWithEdges
      val (a2, r2) = r1.nextIntWithEdges
      val (a3, r3) = r2.nextIntWithEdges
      val (a4, r4) = r3.nextIntWithEdges
      val (a5, _) = r4.nextIntWithEdges
      val edges = List(a1, a2, a3, a4, a5)
      edges should contain (0)
      edges should contain (1)
      edges should contain (-1)
      edges should contain (Int.MaxValue)
      edges should contain (Int.MinValue)
    }
    it("should offer a nextDoubleWithEdges method that initially produces Double edge values") {
      val r0 = Rnd(100)
      val (a1, r1) = r0.nextDoubleWithEdges
      a1 should be (0.0)
    }
  }
}

