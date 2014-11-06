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

class PosFloatSpec extends Spec with Matchers {
  object `A PosFloat` {
    object `should offer a from factory method that` {
      def `returns Some[PosFloat] if the passed Float is greater than 0`
      {
        PosFloat.from(50.23F).value.value shouldBe 50.23F
        PosFloat.from(100.0F).value.value shouldBe 100.0F
      }
      def `returns None if the passed Float is NOT greater than 0` {
        PosFloat.from(0.0F) shouldBe None
        PosFloat.from(-0.00001F) shouldBe None
        PosFloat.from(-99.9F) shouldBe None
      }
    } 
    def `should have a pretty toString` {
      PosFloat.from(42.0F).value.toString shouldBe "PosFloat(42.0)"
    }
    object `when created with apply method` {
  
      def `should compile when 8 is passed in`: Unit = {
        "PosFloat(8)" should compile
        PosFloat(8).value shouldEqual 8
        "PosFloat(8L)" should compile
        PosFloat(8L).value shouldEqual 8
        "PosFloat(8.0F)" should compile
        PosFloat(8.0F).value shouldEqual 8.0F
      }
  
      def `should not compile when 0 is passed in`: Unit = {
        "PosFloat(0)" shouldNot compile
        "PosFloat(0L)" shouldNot compile
        "PosFloat(0.0F)" shouldNot compile
      }
  
      def `should not compile when -8 is passed in`: Unit = {
        "PosFloat(-8)" shouldNot compile
        "PosFloat(-8L)" shouldNot compile
        "PosFloat(-8.0F)" shouldNot compile
      }
      def `should not compile when x is passed in`: Unit = {
        val a: Int = -8
        "PosFloat(a)" shouldNot compile
        val b: Long = -8L
        "PosFloat(b)" shouldNot compile
        val c: Float = -8.0F
        "PosFloat(c)" shouldNot compile
      }
    }
    object `when specified as a plain-old Float` {

      def takesPosFloat(pos: PosFloat): Float = pos.value

      def `should compile when 8 is passed in`: Unit = {
        "takesPosFloat(8)" should compile
        takesPosFloat(8) shouldEqual 8
        "takesPosFloat(8L)" should compile
        takesPosFloat(8L) shouldEqual 8
        "takesPosFloat(8.0F)" should compile
        takesPosFloat(8.0F) shouldEqual 8.0F
      }

      def `should not compile when 0 is passed in`: Unit = {
        "takesPosFloat(0)" shouldNot compile
        "takesPosFloat(0L)" shouldNot compile
        "takesPosFloat(0.0F)" shouldNot compile
      }

      def `should not compile when -8 is passed in`: Unit = {
        "takesPosFloat(-8)" shouldNot compile
        "takesPosFloat(-8L)" shouldNot compile
        "takesPosFloat(-8.0F)" shouldNot compile
      }

      def `should not compile when x is passed in`: Unit = {
        val x: Float = -8
        "takesPosFloat(x)" shouldNot compile
        val b: Float = -8L
        "takesPosFloat(b)" shouldNot compile
        val c: Float = -8.0F
        "takesPosFloat(c)" shouldNot compile
      }
    }
  }
}
  
