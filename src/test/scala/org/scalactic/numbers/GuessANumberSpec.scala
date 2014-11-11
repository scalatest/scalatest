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
package org.scalactic.numbers

import org.scalatest._
import scala.collection.mutable.WrappedArray
import OptionValues._
import org.scalactic.CheckedEquality._

class GuessANumberSpec extends Spec with Matchers {
  object `A GuessANumber` {
    object `should offer a from factory method that` {
      def `returns Some[GuessANumber] if the passed Int is between 1 and 10` {
        GuessANumber.from(1).value.value shouldBe 1
        GuessANumber.from(5).value.value shouldBe 5
        GuessANumber.from(10).value.value shouldBe 10
      }
      def `returns None if the passed Int is NOT between 1 and 10` {
        GuessANumber.from(11) shouldBe None
        GuessANumber.from(100) shouldBe None
        GuessANumber.from(-1) shouldBe None
        GuessANumber.from(-99) shouldBe None
      }
    }
    def `should have a pretty toString` {
      GuessANumber.from(4).value.toString shouldBe "GuessANumber(4)"
    }
  }
  object `An LGuessANumber` {
    object `should offer a from factory method that` {
      def `returns Some[LGuessANumber] if the passed Long is between 1 and 10` {
        LGuessANumber.from(1L).value.value shouldBe 1L
        LGuessANumber.from(5L).value.value shouldBe 5L
        LGuessANumber.from(10L).value.value shouldBe 10L
      }
      def `returns None if the passed Long is NOT between 1 and 10` {
        LGuessANumber.from(11L) shouldBe None
        LGuessANumber.from(100L) shouldBe None
        LGuessANumber.from(-1L) shouldBe None
        LGuessANumber.from(-99L) shouldBe None
      }
    }
    def `should have a pretty toString` {
      LGuessANumber.from(4L).value.toString shouldBe "LGuessANumber(4)"
    }
  }

  object `An FGuessANumber` {
    object `should offer a from factory method that` {
      def `returns Some[FGuessANumber] if the passed Float is between 1 and 10` {
        FGuessANumber.from(1.0F).value.value shouldBe 1.0F
        FGuessANumber.from(5.0F).value.value shouldBe 5.0F
        FGuessANumber.from(10.0F).value.value shouldBe 10.0F
      }
      def `returns None if the passed Float is NOT between 1 and 10` {
        FGuessANumber.from(10.00001F) shouldBe None
        FGuessANumber.from(100.1F) shouldBe None
        FGuessANumber.from(-.000001F) shouldBe None
        FGuessANumber.from(-9.999F) shouldBe None
      }
    }
    def `should have a pretty toString` {
      FGuessANumber.from(4.0F).value.toString shouldBe "FGuessANumber(4.0)"
      FGuessANumber.from(4.42F).value.toString shouldBe "FGuessANumber(4.42)"
    }
  }
  object `A DGuessANumber` {
    object `should offer a from factory method that` {
      def `returns Some[DGuessANumber] if the passed Double is between 1 and 10` {
        DGuessANumber.from(1.0).value.value shouldBe 1.0
        DGuessANumber.from(5.0).value.value shouldBe 5.0
        DGuessANumber.from(10.0).value.value shouldBe 10.0
      }
      def `returns None if the passed Double is NOT between 1 and 10` {
        DGuessANumber.from(10.000001) shouldBe None
        DGuessANumber.from(100.1) shouldBe None
        DGuessANumber.from(-.000001) shouldBe None
        DGuessANumber.from(-9.999) shouldBe None
      }
    }
    def `should have a pretty toString` {
      DGuessANumber.from(4.0).value.toString shouldBe "DGuessANumber(4.0)"
      DGuessANumber.from(4.42).value.toString shouldBe "DGuessANumber(4.42)"
    }
  }
}

