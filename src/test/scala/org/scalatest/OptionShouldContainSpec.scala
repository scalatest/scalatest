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

class OptionShouldContainSpec extends Spec with Matchers with SharedHelpers {
  object `an Option` {
    def `should be usable with contain (value) syntax` {

      val some: Option[String] = Some("hi")
      val none: Option[String] = None

      some should contain ("hi")
      //some should not contain ("ho")
      //none should not contain ("hi")

      val e1 = intercept[TestFailedException] {
        some should contain ("ho")
      }
      e1.message.get should be (Resources("didNotContainExpectedElement", some, "\"ho\""))
      e1.failedCodeFileName.get should be ("OptionShouldContainSpec.scala")
      e1.failedCodeLineNumber.get should be (thisLineNumber - 4)

      val e2 = intercept[TestFailedException] {
        none should contain ("ho")
      }
      e2.message.get should be (Resources("didNotContainExpectedElement", none, "\"ho\""))
      e2.failedCodeFileName.get should be ("OptionShouldContainSpec.scala")
      e2.failedCodeLineNumber.get should be (thisLineNumber - 4)

/*
      val e3 = intercept[TestFailedException] {
        some should not contain ("hi")
      }
      e3.message.get should be (Resources("containedExpectedElement", some, "\"hi\""))
      e3.failedCodeFileName.get should be ("OptionShouldContainSpec.scala")
      e3.failedCodeLineNumber.get should be (thisLineNumber - 4)
*/
    }
  }
}
