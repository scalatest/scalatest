/*
 * Copyright 2001-2014 Artima, Inc.
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
package org.scalactic.numbers

import org.scalatest._
import scala.collection.mutable.WrappedArray
import OptionValues._
import org.scalactic.StrictCheckedEquality._

class PosLongSpec extends Spec with Matchers {

  object `A PosLong` {
    object `should offer a from factory method that` {
      def `returns Some[PosLong] if the passed Long is greater than 0`
      {
        PosLong.from(50L).value.value shouldBe 50
        PosLong.from(100L).value.value shouldBe 100
      }
      def `returns None if the passed Long is NOT greater than 0` {
        PosLong.from(0L) shouldBe None
        PosLong.from(-1L) shouldBe None
        PosLong.from(-99L) shouldBe None
      }
    } 
    def `should have a pretty toString` {
      PosLong.from(42L).value.toString shouldBe "PosLong(42)"
    }
    object `when created with apply method` {

      def `should compile when 8 is passed in`: Unit = {
        "PosLong(8)" should compile
        PosLong(8).value shouldEqual 8
        "PosLong(8L)" should compile
        PosLong(8L).value shouldEqual 8
      }

      def `should not compile when 0 is passed in`: Unit = {
        "PosLong(0)" shouldNot compile
        "PosLong(0L)" shouldNot compile
      }

      def `should not compile when -8 is passed in`: Unit = {
        "PosLong(-8)" shouldNot compile
        "PosLong(-8L)" shouldNot compile
      }

      def `should not compile when x is passed in`: Unit = {
        val a: Int = -8
        "PosLong(a)" shouldNot compile
        val b: Long = -8L
        "PosLong(b)" shouldNot compile
      }
    }
    object `when specified as a plain-old Long` {

      def takesPosLong(pos: PosLong): Long = pos.value

      def `should compile when 8 is passed in`: Unit = {
        "takesPosLong(8)" should compile
        takesPosLong(8) shouldEqual 8
        "takesPosLong(8L)" should compile
        takesPosLong(8L) shouldEqual 8
      }

      def `should not compile when 0 is passed in`: Unit = {
        "takesPosLong(0)" shouldNot compile
        "takesPosLong(0L)" shouldNot compile
      }

      def `should not compile when -8 is passed in`: Unit = {
        "takesPosLong(-8)" shouldNot compile
        "takesPosLong(-8L)" shouldNot compile
      }

      def `should not compile when x is passed in`: Unit = {
        val x: Long = -8
        "takesPosLong(x)" shouldNot compile
        val b: Long = -8L
        "takesPosLong(b)" shouldNot compile
      }
    }
/*
    object `when specified as a plain-old Int` {

      def takesPosLong(posL: PosLong): Long = posL.value

      def `should compile when 8 is passed in`: Unit = {
        "takesPosLong(8)" should compile
        takesPosLong(8) shouldEqual 8L
        "takesPosLong(8L)" should compile
        takesPosLong(8L) shouldEqual 8L
      }

      def `should not compile when 0 is passed in`: Unit = {
        "takesPosLong(0)" shouldNot compile
        "takesPosLong(0L)" shouldNot compile
      }

      def `should not compile when -8 is passed in`: Unit = {
        "takesPosLong(-8)" shouldNot compile
        "takesPosLong(-8L)" shouldNot compile
      }

      def `should not compile when x is passed in`: Unit = {
        val a: Int = -8
        "takesPosLong(a)" shouldNot compile
        val b: Long = -8L
        "takesPosLong(b)" shouldNot compile
      }
    }
*/
/*
    def `should be automatically widened to compatible AnyVal targets` {
      (PosLong.from(3).get: Int) shouldEqual 3 // shouldNot typeCheck
      (PosLong.from(3).get: Long) shouldEqual 3L
      (PosLong.from(3).get: Float) shouldEqual 3.0F
      (PosLong.from(3).get: Double) shouldEqual 3.0
      (PosLong.from(3).get: Poz) shouldEqual Poz.from(3).get
      (PosLong.from(3).get: PozLong) shouldEqual PozLong.from(3L).get
      (PosLong.from(3).get: PozFloat) shouldEqual PozFloat.from(3.0F).get
      (PosLong.from(3).get: PozDouble) shouldEqual PozDouble.from(3.0).get
    }
    object `when a compatible AnyVal is passed to a + method invoked on it` {
      def `should give the same AnyVal type back at compile time, and correct value at runtime` {
        // When adding a "primitive"
        val opInt = PosLong.from(3).get + 3 // should be type Long
        opInt shouldEqual 6L

        val opLong = PosLong.from(3).get + 3L
        opLong shouldEqual 6L

        val opFloat = PosLong.from(3).get + 3.0F
        opFloat shouldEqual 6.0F

        val opDouble = PosLong.from(3).get + 3.0
        opDouble shouldEqual 6.0

        // When adding a *Pos
        val opPos = Pos.from(3).get + Pos.from(3).get
        opPos shouldEqual 6L

        val opPosLong = Pos.from(3).get + PosLong.from(3L).get
        opPosLong shouldEqual 6L

        val opPosFloat = Pos.from(3).get + PosFloat.from(3.0F).get
        opPosFloat shouldEqual 6.0F

        val opPosDouble = Pos.from(3).get + PosDouble.from(3.0).get
        opPosDouble shouldEqual 6.0

        // When adding a *Poz
        val opPoz = Pos.from(3).get + Poz.from(3).get
        opPoz shouldEqual Poz.from(6).get.value

        val opPozLong = Pos.from(3).get + PozLong.from(3L).get
        opPozLong shouldEqual PozLong.from(6L).get.value

        val opPozFloat = Pos.from(3).get + PozFloat.from(3.0F).get
        opPozFloat shouldEqual PozFloat.from(6.0F).get.value

        val opPozDouble = Pos.from(3).get + PozDouble.from(3.0).get
        opPozDouble shouldEqual PozDouble.from(6.0).get.value
      }
    }
*/
  }
}

