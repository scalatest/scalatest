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

import org.scalautils.Equality

class OptionShouldContainOneOfSpec extends Spec with Matchers with SharedHelpers {

  object `an Option` {

    object `when used with contain oneOf (...) syntax` {

      val some: Option[String] = Some("fum")

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        some should newContain newOneOf ("fee", "fie", "foe", "fum")
        val e1 = intercept[TestFailedException] {
          some should newContain newOneOf ("happy", "birthday", "to", "you")
        }
        e1.failedCodeFileName.get should be ("OptionShouldContainOneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources("didNotContainOneOfElements", some, "\"happy\", \"birthday\", \"to\", \"you\""))
      }

      def `should use the implicit Equality in scope` {
        implicit val e = new Equality[String] {
          def areEqual(a: String, b: Any): Boolean = a != b
        }
        some should newContain newOneOf ("happy", "birthday", "to", "you")
        val e2 = intercept[TestFailedException] {
          some should newContain newOneOf ("fum", "fum", "fum", "fum")
        }
      }
    }

    object `when used with not contain oneOf (...) syntax` {

      val some: Option[String] = Some("to")

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        some should not newContain newOneOf ("fee", "fie", "foe", "fum")
        val e1 = intercept[TestFailedException] {
          some should not newContain newOneOf ("happy", "birthday", "to", "you")
        }
        e1.failedCodeFileName.get should be ("OptionShouldContainOneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources("containedOneOfElements", some, "\"happy\", \"birthday\", \"to\", \"you\""))
      }

      def `should use the implicit Equality in scope` {
        implicit val e = new Equality[String] {
          def areEqual(a: String, b: Any): Boolean = a != b
        }
        some should not newContain newOneOf ("to", "to", "to", "to")
        val e2 = intercept[TestFailedException] {
          some should not newContain newOneOf ("fee", "fie", "foe", "fum")
        }
      }
    }

    object `when used with (contain oneOf (...)) syntax` {

      val some: Option[String] = Some("fum")

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        some should (newContain newOneOf ("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          some should (newContain newOneOf ("happy", "birthday", "to", "you"))
        }
        e1.failedCodeFileName.get should be ("OptionShouldContainOneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources("didNotContainOneOfElements", some, "\"happy\", \"birthday\", \"to\", \"you\""))
      }

      def `should use the implicit Equality in scope` {
        implicit val e = new Equality[String] {
          def areEqual(a: String, b: Any): Boolean = a != b
        }
        some should (newContain newOneOf ("happy", "birthday", "to", "you"))
        val e2 = intercept[TestFailedException] {
          some should (newContain newOneOf ("fum", "fum", "fum", "fum"))
        }
      }
    }

    object `when used with (not contain oneOf (...)) syntax` {

      val some: Option[String] = Some("to")

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        some should (not newContain newOneOf ("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          some should (not newContain newOneOf ("happy", "birthday", "to", "you"))
        }
        e1.failedCodeFileName.get should be ("OptionShouldContainOneOfSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message.get should be (Resources("containedOneOfElements", some, "\"happy\", \"birthday\", \"to\", \"you\""))
      }

      def `should use the implicit Equality in scope` {
        implicit val e = new Equality[String] {
          def areEqual(a: String, b: Any): Boolean = a != b
        }
        some should (not newContain newOneOf ("to", "to", "to", "to"))
        val e2 = intercept[TestFailedException] {
          some should (not newContain newOneOf ("fee", "fie", "foe", "fum"))
        }
      }
    }
  }
}
