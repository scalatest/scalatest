/*
 * Copyright 2001-2013 Artima, Inc.
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

import matchers._
import SharedHelpers._

class ThreadNameSpec extends Spec with Matchers {

  object `The Thread name augmenter` {
    def `should add "running X" onto bare thread names` {
      SuiteHelpers.augmentedThreadName("ScalaTest-main", "SomeSpec") shouldBe "ScalaTest-main-running-SomeSpec"
      SuiteHelpers.augmentedThreadName("ScalaTest-run", "SomeSpec") shouldBe "ScalaTest-run-running-SomeSpec"
      SuiteHelpers.augmentedThreadName("ScalaTest-3", "SomeSpec") shouldBe "ScalaTest-3-running-SomeSpec"
    }
    def `should replace "...-running-X" with "...-running-Y"` {
      SuiteHelpers.augmentedThreadName("ScalaTest-main-running-SomeSpec", "OtherSpec") shouldBe "ScalaTest-main-running-OtherSpec"
      SuiteHelpers.augmentedThreadName("ScalaTest-run-running-SomeSpec", "GopherSuite") shouldBe "ScalaTest-run-running-GopherSuite"
      SuiteHelpers.augmentedThreadName("ScalaTest-3-running-SomeSpec", "OtherSpec") shouldBe "ScalaTest-3-running-OtherSpec"
    }
    def `should replace "...-running-X-running-Y" with "...-running-Z"` {
      SuiteHelpers.augmentedThreadName("ScalaTest-main-running-Some-running-Spec", "OtherSpec") shouldBe "ScalaTest-main-running-OtherSpec"
      SuiteHelpers.augmentedThreadName("ScalaTest-run-running-Some-running-Spec", "OtherSpec") shouldBe "ScalaTest-run-running-OtherSpec"
      SuiteHelpers.augmentedThreadName("ScalaTest-3-running-Some-running-Spec", "BunnySpec") shouldBe "ScalaTest-3-running-BunnySpec"
    }
    // This could happen if calling run directly.
    def `should replace a string that does not start with "ScalaTest-" with "ScalaTest-running-Z"` {
      SuiteHelpers.augmentedThreadName("pool-3-thread-5", "OtherSpec") shouldBe "ScalaTest-running-OtherSpec"
      SuiteHelpers.augmentedThreadName("", "AntherSpec") shouldBe "ScalaTest-running-AntherSpec"
      SuiteHelpers.augmentedThreadName("something else", "Elephant") shouldBe "ScalaTest-running-Elephant"
      SuiteHelpers.augmentedThreadName("something-else-running-something", "Elephant") shouldBe "ScalaTest-running-Elephant"
    }
  }
}

