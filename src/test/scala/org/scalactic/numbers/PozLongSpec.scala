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

class PozLongSpec extends Spec with Matchers {
  object `A PozLong` {
    object `should offer a from factory method that` {
      def `returns Some[PozLong] if the passed Long is greater than or equal to 0` {
        PozLong.from(0L).value.value shouldBe 0
        PozLong.from(50L).value.value shouldBe 50
        PozLong.from(100L).value.value shouldBe 100
      }
      def `returns None if the passed Long is NOT greater than or equal to 0` {
        PozLong.from(-1L) shouldBe None
        PozLong.from(-99L) shouldBe None
      }
    }
    def `should have a pretty toString` {
      PozLong.from(42L).value.toString shouldBe "PozLong(42)"
    }
    object `when created with apply method` {

      def `should compile when 8 is passed in`: Unit = {
        "PozLong(8)" should compile
        PozLong(8).value shouldEqual 8L
        "PozLong(8L)" should compile
        PozLong(8L).value shouldEqual 8L
      }

      def `should compile when 0 is passed in`: Unit = {
        "PozLong(0)" should compile
        PozLong(0).value shouldEqual 0L
        "PozLong(0L)" should compile
        PozLong(0L).value shouldEqual 0L
      }

      def `should not compile when -8 is passed in`: Unit = {
        "PozLong(-8)" shouldNot compile
        "PozLong(-8L)" shouldNot compile
      }

      def `should not compile when x is passed in`: Unit = {
        val a: Int = -8
        "PozLong(a)" shouldNot compile
        val b: Long = -8L
        "PozLong(b)" shouldNot compile
      }
    }
  }
}

