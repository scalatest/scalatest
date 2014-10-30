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
package org.scalactic.math

import org.scalatest._
import scala.collection.mutable.WrappedArray
import OptionValues._
import org.scalactic.CheckedEquality._

class PercentSpec extends Spec with Matchers {
  object `A Percent` {
    object `should offer a from factory method that` {
      def `returns Some[Percent] if the passed Double is between 0 and 100` {
        Percent.from(0).value.value shouldBe 0
        Percent.from(50).value.value shouldBe 50
        Percent.from(100).value.value shouldBe 100
      }
      def `returns None if the passed Double is NOT between 0 and 100` {
        Percent.from(101) shouldBe None
        Percent.from(1000) shouldBe None
        Percent.from(-1) shouldBe None
        Percent.from(-99) shouldBe None
      }
    } 
    def `should have a pretty toString` {
      Percent.from(42).value.toString shouldBe "Percent(42)"
    }
  }
}

