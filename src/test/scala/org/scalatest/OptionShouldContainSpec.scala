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

class OptionShouldContainSpec extends Spec with Matchers with SharedHelpers {
  object `an Option` {
    def `should be usable with contain (value) syntax` {

      val some: Option[String] = Some("hi")
      val none: Option[String] = None

      some should contain ("hi")
      Some("hi") should contain ("hi")
      some should not contain ("ho")
      Some("hi") should not contain ("ho")
      none should not contain ("hi")

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

      val e3 = intercept[TestFailedException] {
        some should not contain ("hi")
      }
      e3.message.get should be (Resources("containedExpectedElement", some, "\"hi\""))
      e3.failedCodeFileName.get should be ("OptionShouldContainSpec.scala")
      e3.failedCodeLineNumber.get should be (thisLineNumber - 4)

      val e4 = intercept[TestFailedException] {
        Some("hi") should contain ("ho")
      }
      e4.message.get should be (Resources("didNotContainExpectedElement", some, "\"ho\""))
      e4.failedCodeFileName.get should be ("OptionShouldContainSpec.scala")
      e4.failedCodeLineNumber.get should be (thisLineNumber - 4)

      val e5 = intercept[TestFailedException] {
        Some("hi") should not contain ("hi")
      }
      e5.message.get should be (Resources("containedExpectedElement", some, "\"hi\""))
      e5.failedCodeFileName.get should be ("OptionShouldContainSpec.scala")
      e5.failedCodeLineNumber.get should be (thisLineNumber - 4)
    }

    def `should use the implicit Equality in scope with contain (value) syntax` {
      val some: Option[String] = Some("hi")
      some should contain ("hi")
      intercept[TestFailedException] {
        some should contain ("ho")
      }
      implicit val e = new Equality[String] {
        def areEqual(a: String, b: Any): Boolean = a != b
      }
      some should contain ("ho")
      intercept[TestFailedException] {
        some should contain ("hi")
      }
      some should contain ("hi") (decidedForOption by defaultEquality[String])
      intercept[TestFailedException] {
        some should contain ("ho") (decidedForOption by defaultEquality[String])
      }
    }

    def `should be usable with contain oneOf syntax` {
      val some: Option[String] = Some("fum")
      some should newContain newOneOf ("fee", "fie", "foe", "fum")
      val e1 = intercept[TestFailedException] {
        some should newContain newOneOf ("happy", "birthday", "to", "you")
      }
      e1.failedCodeFileName.get should be ("OptionShouldContainSpec.scala")
      e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
      e1.message.get should be (Resources("didNotContainOneOfElements", some, "\"happy\", \"birthday\", \"to\", \"you\""))
      implicit val e = new Equality[String] {
        def areEqual(a: String, b: Any): Boolean = a != b
      }
      some should newContain newOneOf ("happy", "birthday", "to", "you")
      val e2 = intercept[TestFailedException] {
        some should newContain newOneOf ("fum", "fum", "fum", "fum")
      }
    }

    def `should be usable with not contain oneOf syntax` {
      val some: Option[String] = Some("to")
      some should not newContain newOneOf ("fee", "fie", "foe", "fum")
      val e1 = intercept[TestFailedException] {
        some should not newContain newOneOf ("happy", "birthday", "to", "you")
      }
      e1.failedCodeFileName.get should be ("OptionShouldContainSpec.scala")
      e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
      e1.message.get should be (Resources("containedOneOfElements", some, "\"happy\", \"birthday\", \"to\", \"you\""))
      implicit val e = new Equality[String] {
        def areEqual(a: String, b: Any): Boolean = a != b
      }
      some should not newContain newOneOf ("to", "to", "to", "to")
      val e2 = intercept[TestFailedException] {
        some should not newContain newOneOf ("fee", "fie", "foe", "fum")
      }
    }

    def `should be usable with (contain oneOf) syntax` {
      val some: Option[String] = Some("fum")
      some should (newContain newOneOf ("fee", "fie", "foe", "fum"))
      val e1 = intercept[TestFailedException] {
        some should (newContain newOneOf ("happy", "birthday", "to", "you"))
      }
      e1.failedCodeFileName.get should be ("OptionShouldContainSpec.scala")
      e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
      e1.message.get should be (Resources("didNotContainOneOfElements", some, "\"happy\", \"birthday\", \"to\", \"you\""))
      implicit val e = new Equality[String] {
        def areEqual(a: String, b: Any): Boolean = a != b
      }
      some should (newContain newOneOf ("happy", "birthday", "to", "you"))
      val e2 = intercept[TestFailedException] {
        some should (newContain newOneOf ("fum", "fum", "fum", "fum"))
      }
    }
  }

  object `a collection of Options` {
    def `be usable with contain value syntax via inspector shorthands` {

      val some1s: Vector[Option[Int]] = Vector(Some(1), Some(1), Some(1))
      val somes: Vector[Option[Int]] = Vector(Some(1), Some(1), Some(2))
      val nones: Vector[Option[Int]] = Vector(None, None, None)
      val somesNone: Vector[Option[Int]] = Vector(Some(1), Some(1), None)

      all (some1s) should contain (1)
      atLeast (2, somes) should contain (1)
      atMost (2, somes) should contain (2)
      no (somes) should contain (3)
      no (nones) should contain (1)
      no (somesNone) should contain (3)

      val e1 = intercept[TestFailedException] {
        all (somes) should contain (1)
      }
      e1.failedCodeFileName.get should be ("OptionShouldContainSpec.scala")
      e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
      e1.message should be (Some("'all' inspection failed, because: \n" +
                                 "  at index 2, Some(2) did not contain element 1 (OptionShouldContainSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                 "in Vector(Some(1), Some(1), Some(2))"))

      val e2 = intercept[TestFailedException] {
        all (nones) should contain ("ho")
      }
      e2.failedCodeFileName.get should be ("OptionShouldContainSpec.scala")
      e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
      e2.message should be (Some("'all' inspection failed, because: \n" +
                                 "  at index 0, None did not contain element \"ho\" (OptionShouldContainSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                 "in Vector(None, None, None)"))

      val e3 = intercept[TestFailedException] {
        all (somes) should not contain (2)
      }
      e3.failedCodeFileName.get should be ("OptionShouldContainSpec.scala")
      e3.failedCodeLineNumber.get should be (thisLineNumber - 3)
      e3.message should be (Some("'all' inspection failed, because: \n" +
                                 "  at index 2, Some(2) contained element 2 (OptionShouldContainSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                 "in Vector(Some(1), Some(1), Some(2))"))

      val e4 = intercept[TestFailedException] {
        all (somes) should contain (1)
      }
      e4.failedCodeFileName.get should be ("OptionShouldContainSpec.scala")
      e4.failedCodeLineNumber.get should be (thisLineNumber - 3)
      e4.message should be (Some("'all' inspection failed, because: \n" +
                                 "  at index 2, Some(2) did not contain element 1 (OptionShouldContainSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                 "in Vector(Some(1), Some(1), Some(2))"))
    }

    def `should use the implicit Equality in scope with contain (value) syntax` {
      val somes: Vector[Option[String]] = Vector(Some("hi"), Some("hi"), Some("hi"))
      intercept[TestFailedException] {
        all (somes) should contain ("ho")
      }
      implicit val e = new Equality[String] {
        def areEqual(a: String, b: Any): Boolean = a != b
      }
      all (somes) should contain ("ho")
    }
  }
}
