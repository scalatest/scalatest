/*
* Copyright 2001-2014 Artima, Inc.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package org.scalactic.anyvals

import org.scalatest._
import scala.collection.mutable.WrappedArray
import OptionValues._
//import org.scalactic.CheckedEquality._

// Uncomment this test once we get the OddIntMacro
// defined in an earlier compilation run.
/* 
class OddIntSpec extends RefSpec with Matchers {
  object `An OddInt` {
    object `should offer a from factory method that` {
      def `returns Some[OddInt] if the passed Int is odd` {
        OddInt.from(1).value.value shouldBe 1
        OddInt.from(5).value.value shouldBe 5
        OddInt.from(-3).value.value shouldBe 10
      }
      def `returns None if the passed Int is NOT odd` {
        OddInt.from(12) shouldBe None
        OddInt.from(100) shouldBe None
        OddInt.from(-2) shouldBe None
        OddInt.from(0) shouldBe None
      }
    }
    def `should have a pretty toString` {
      OddInt.from(3).value.toString shouldBe "OddInt(3)"
    }
    object `when created with apply method` {

      def `should compile when 7 is passed in`: Unit = {
        "OddInt(7)" should compile
        OddInt(7).value shouldEqual 8
      }

      def `should not compile when 0 is passed in`: Unit = {
        "OddInt(0)" shouldNot compile
      }

      def `should not compile when -8 is passed in`: Unit = {
        "OddInt(-8)" shouldNot compile
      }

      def `should not compile when x is passed in`: Unit = {
        val x: Int = 7
        "OddInt(x)" shouldNot compile
      }
    }
  }
}
*/
