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
import org.scalactic.CheckedEquality._

class PozFloatSpec extends Spec with Matchers {
  object `An PozFloat` {
    object `should offer a from factory method that` {
      def `returns Some[PozFloat] if the passed Float is greater than or equal to 0`
      {
        PozFloat.from(0.0f).value.value shouldBe 0.0f
        PozFloat.from(50.23f).value.value shouldBe 50.23f
        PozFloat.from(100.0f).value.value shouldBe 100.0f
      }
      def `returns None if the passed Float is NOT greater than or equal to 0` {
        PozFloat.from(-0.00001f) shouldBe None
        PozFloat.from(-99.9f) shouldBe None
      }
    } 
    def `should have a pretty toString` {
      PozFloat.from(42.0f).value.toString shouldBe "PozFloat(42.0)"
    }
    object `when created with apply method` {
  
      def `should compile when 8 is passed in`: Unit = {
        "PozFloat(8)" should compile
        PozFloat(8).value shouldEqual 8
        "PozFloat(8L)" should compile
        PozFloat(8L).value shouldEqual 8
        "PozFloat(8.0F)" should compile
        PozFloat(8.0F).value shouldEqual 8.0F
      }
  
      def `should compile when 0 is passed in`: Unit = {
        "PozFloat(0)" should compile
        PozFloat(0).value shouldEqual 0
        "PozFloat(0L)" should compile
        PozFloat(0L).value shouldEqual 0
        "PozFloat(0.0F)" should compile
        PozFloat(0.0F).value shouldEqual 0.0F
      }

      def `should not compile when -8 is passed in`: Unit = {
        "PozFloat(-8)" shouldNot compile
        "PozFloat(-8L)" shouldNot compile
        "PozFloat(-8.0F)" shouldNot compile
      }

      def `should not compile when x is passed in`: Unit = {
        val a: Int = -8
        "PozFloat(a)" shouldNot compile
        val b: Long = -8L
        "PozFloat(b)" shouldNot compile
        val c: Float = -8.0F
        "PozFloat(c)" shouldNot compile
      }
    }
    object `when specified as a plain-old Float` {

      def takesPozFloat(pos: PozFloat): Float = pos.value

      def `should compile when 8 is passed in`: Unit = {
        "takesPozFloat(8)" should compile
        takesPozFloat(8) shouldEqual 8
        "takesPozFloat(8L)" should compile
        takesPozFloat(8L) shouldEqual 8L
        "takesPozFloat(8.0F)" should compile
        takesPozFloat(8.0F) shouldEqual 8.0F
      }

      def `should compile when 0 is passed in`: Unit = {
        "takesPozFloat(0)" should compile
        takesPozFloat(0) shouldEqual 0
        "takesPozFloat(0L)" should compile
        takesPozFloat(0L) shouldEqual 0L
        "takesPozFloat(0.0F)" should compile
        takesPozFloat(0.0F) shouldEqual 0.0F
      }

      def `should not compile when -8 is passed in`: Unit = {
        "takesPozFloat(-8)" shouldNot compile
        "takesPozFloat(-8L)" shouldNot compile
        "takesPozFloat(-8.0F)" shouldNot compile
      }

      def `should not compile when x is passed in`: Unit = {
        val x: Float = -8
        "takesPozFloat(x)" shouldNot compile
        val b: Float = -8L
        "takesPozFloat(b)" shouldNot compile
        val c: Float = -8.0F
        "takesPozFloat(c)" shouldNot compile
      }
    }
  }
}

