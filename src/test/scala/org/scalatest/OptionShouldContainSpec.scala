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
import org.scalautils.NormalizingEquality
import org.scalautils.StringNormalizations._
import SharedHelpers._

class OptionShouldContainSpec extends Spec with Matchers {

  object `an Option` {

    val some: Option[String] = Some("hi")
    val none: Option[String] = None

    object `when used with contain (value) syntax` {
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        some should contain ("hi")
        Some("hi") should contain ("hi")

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

        val e4 = intercept[TestFailedException] {
          Some("hi") should contain ("ho")
        }
        e4.message.get should be (Resources("didNotContainExpectedElement", some, "\"ho\""))
        e4.failedCodeFileName.get should be ("OptionShouldContainSpec.scala")
        e4.failedCodeLineNumber.get should be (thisLineNumber - 4)
      }
      def `should use the implicit Equality in scope` {
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
      }
      def `should use an explicitly provided Equality` {
        intercept[TestFailedException] {
          some should contain ("HI")
        }
        (some should contain ("HI")) (decided by defaultEquality afterBeing lowerCased)
        (some should contain ("HI")) (after being lowerCased)
        intercept[TestFailedException] {
          (some should contain ("HI ")) (after being lowerCased)
        }
        (some should contain ("HI ")) (after being lowerCased and trimmed)
        implicit val e = new Equality[String] {
          def areEqual(a: String, b: Any): Boolean = a != b
        }
        intercept[TestFailedException] {
          some should contain ("hi")
        }
        (some should contain ("hi")) (decided by defaultEquality[String])
      }
    }

    object `when used with not contain value syntax` {
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        some should not contain "ho"
        Some("hi") should not contain "ho"
        none should not contain "hi"

        val e3 = intercept[TestFailedException] {
          some should not contain "hi"
        }
        e3.message.get should be (Resources("containedExpectedElement", some, "\"hi\""))
        e3.failedCodeFileName.get should be ("OptionShouldContainSpec.scala")
        e3.failedCodeLineNumber.get should be (thisLineNumber - 4)

        val e5 = intercept[TestFailedException] {
          Some("hi") should not contain "hi"
        }
        e5.message.get should be (Resources("containedExpectedElement", some, "\"hi\""))
        e5.failedCodeFileName.get should be ("OptionShouldContainSpec.scala")
        e5.failedCodeLineNumber.get should be (thisLineNumber - 4)
      }
      def `should use the implicit Equality in scope` {
        some should not contain "ho"
        intercept[TestFailedException] {
          some should not contain "hi"
        }
        implicit val e = new Equality[String] {
          def areEqual(a: String, b: Any): Boolean = a != b
        }
        some should not contain "hi"
        intercept[TestFailedException] {
          some should not contain "ho"
        }
      }
      def `should use an explicitly provided Equality` {
        some should not contain "HI"
        some should not contain "HI "
        (some should not contain "HI ") (decided by defaultEquality afterBeing lowerCased)
        (some should not contain "HI ") (after being lowerCased)
        intercept[TestFailedException] {
          (some should not contain "HI") (decided by defaultEquality afterBeing lowerCased)
        }
        intercept[TestFailedException] {
          (some should not contain "HI ") (after being lowerCased and trimmed)
        }
      }
    }

    object `when used with not (contain (value)) syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {

        some should not (contain ("ho"))
        Some("hi") should not (contain ("ho"))
        none should not (contain ("hi"))

        val e3 = intercept[TestFailedException] {
          some should not (contain ("hi"))
        }
        e3.message.get should be (Resources("containedExpectedElement", some, "\"hi\""))
        e3.failedCodeFileName.get should be ("OptionShouldContainSpec.scala")
        e3.failedCodeLineNumber.get should be (thisLineNumber - 4)

        val e5 = intercept[TestFailedException] {
          Some("hi") should not (contain ("hi"))
        }
        e5.message.get should be (Resources("containedExpectedElement", some, "\"hi\""))
        e5.failedCodeFileName.get should be ("OptionShouldContainSpec.scala")
        e5.failedCodeLineNumber.get should be (thisLineNumber - 4)
      }

      def `should use the implicit Equality in scope` {
        some should not (contain ("ho"))
        intercept[TestFailedException] {
          some should not (contain ("hi"))
        }
        implicit val e = new Equality[String] {
          def areEqual(a: String, b: Any): Boolean = a != b
        }
        some should not (contain ("hi"))
        intercept[TestFailedException] {
          some should not (contain ("ho"))
        }
      }
      def `should use an explicitly provided Equality` {
        some should not (contain ("HI"))
        some should not (contain ("HI "))
        (some should not (contain ("HI "))) (decided by defaultEquality afterBeing lowerCased)
        (some should not (contain ("HI "))) (after being lowerCased)
        intercept[TestFailedException] {
          (some should not (contain ("HI"))) (decided by defaultEquality afterBeing lowerCased)
        }
        intercept[TestFailedException] {
          (some should not (contain ("HI "))) (after being lowerCased and trimmed)
        }
      }
    }

    object `when used with (not contain value) syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        some should (not contain "ho")
        Some("hi") should (not contain ("ho"))
        none should (not contain "hi")

        val e3 = intercept[TestFailedException] {
          some should (not contain "hi")
        }
        e3.message.get should be (Resources("containedExpectedElement", some, "\"hi\""))
        e3.failedCodeFileName.get should be ("OptionShouldContainSpec.scala")
        e3.failedCodeLineNumber.get should be (thisLineNumber - 4)

        val e5 = intercept[TestFailedException] {
          Some("hi") should (not contain "hi")
        }
        e5.message.get should be (Resources("containedExpectedElement", some, "\"hi\""))
        e5.failedCodeFileName.get should be ("OptionShouldContainSpec.scala")
        e5.failedCodeLineNumber.get should be (thisLineNumber - 4)
      }

      def `should use the implicit Equality in scope` {
        some should (not contain "ho")
        intercept[TestFailedException] {
          some should (not contain "hi")
        }
        implicit val e = new Equality[String] {
          def areEqual(a: String, b: Any): Boolean = a != b
        }
        some should (not contain "hi")
        intercept[TestFailedException] {
          some should (not contain "ho")
        }
      }
      def `should use an explicitly provided Equality` {
        some should (not contain "HI")
        some should (not contain "HI ")
        (some should (not contain "HI ")) (decided by defaultEquality afterBeing lowerCased)
        (some should (not contain "HI ")) (after being lowerCased)
        intercept[TestFailedException] {
          (some should (not contain "HI")) (decided by defaultEquality afterBeing lowerCased)
        }
        intercept[TestFailedException] {
          (some should (not contain "HI ")) (after being lowerCased and trimmed)
        }
      }
    }
  }

  object `a collection of Options` {

    val some1s: Vector[Option[Int]] = Vector(Some(1), Some(1), Some(1))
    val somes: Vector[Option[Int]] = Vector(Some(1), Some(1), Some(2))
    val nones: Vector[Option[Int]] = Vector(None, None, None)
    val somesNone: Vector[Option[Int]] = Vector(Some(1), Some(1), None)
    val hiSomes: Vector[Option[String]] = Vector(Some("hi"), Some("hi"), Some("hi"))

    object `when used with contain (value) syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {

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
      def `should use the implicit Equality in scope` {
        intercept[TestFailedException] {
          all (hiSomes) should contain ("ho")
        }
        implicit val e = new Equality[String] {
          def areEqual(a: String, b: Any): Boolean = a != b
        }
        all (hiSomes) should contain ("ho")
        intercept[TestFailedException] {
          all (hiSomes) should contain ("hi")
        }
      }
      def `should use an explicitly provided Equality` {
        intercept[TestFailedException] {
          all (somes) should contain ("HI")
        }
        intercept[TestFailedException] {
          all (hiSomes) should contain ("HI ")
        }
        (all (hiSomes) should contain ("HI")) (decided by defaultEquality afterBeing lowerCased)
        (all (hiSomes) should contain ("HI ")) (after being trimmed and lowerCased)
      }
    }
    object `when used with not contain value syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {

        all (some1s) should not contain 2
        atLeast (2, somes) should not contain 2
        atMost (2, somes) should not contain 1
        no (some1s) should not contain 1 // I will recommend against double negatives, but we should test it
        all (nones) should not contain 1
        all (somesNone) should not contain 3

        val e1 = intercept[TestFailedException] {
          all (somes) should not contain 2
        }
        e1.failedCodeFileName.get should be ("OptionShouldContainSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 2, Some(2) contained element 2 (OptionShouldContainSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in Vector(Some(1), Some(1), Some(2))"))

        val e2 = intercept[TestFailedException] {
          atMost (2, nones) should not contain "ho"
        }
        e2.failedCodeFileName.get should be ("OptionShouldContainSpec.scala")
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some("'atMost(2)' inspection failed, because 3 elements satisfied the assertion block at index 0, 1 and 2 in Vector(None, None, None)"))
      }
      def `should use the implicit Equality in scope` {
        all (hiSomes) should not contain "ho"
        intercept[TestFailedException] {
          all (hiSomes) should not contain "hi"
        }
        implicit val e = new Equality[String] {
          def areEqual(a: String, b: Any): Boolean = a != b
        }
        all (hiSomes) should not contain "hi"
        intercept[TestFailedException] {
          all (hiSomes) should not contain "ho"
        }
      }
      def `should use an explicitly provided Equality` {
        all (hiSomes) should not contain "HI"
        all (hiSomes) should not contain "HI "
        intercept[TestFailedException] {
          (all (hiSomes) should not contain "HI") (decided by defaultEquality afterBeing lowerCased)
        }
        intercept[TestFailedException] {
          (all (hiSomes) should not contain "HI ") (after being trimmed and lowerCased)
        }
      }
    }
    object `when used with not (contain (value)) syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {

        all (some1s) should not (contain (2))
        atLeast (2, somes) should not (contain (2))
        atMost (2, somes) should not (contain (1))
        no (some1s) should not (contain (1)) // I will recommend against double negatives, but we should test it
        all (nones) should not (contain (1))
        all (somesNone) should not (contain (3))

        val e1 = intercept[TestFailedException] {
          all (somes) should not (contain (2))
        }
        e1.failedCodeFileName.get should be ("OptionShouldContainSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 2, Some(2) contained element 2 (OptionShouldContainSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in Vector(Some(1), Some(1), Some(2))"))

        val e2 = intercept[TestFailedException] {
          atMost (2, nones) should not (contain ("ho"))
        }
        e2.failedCodeFileName.get should be ("OptionShouldContainSpec.scala")
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some("'atMost(2)' inspection failed, because 3 elements satisfied the assertion block at index 0, 1 and 2 in Vector(None, None, None)"))
      }
      def `should use the implicit Equality in scope` {
        all (hiSomes) should not (contain ("ho"))
        intercept[TestFailedException] {
          all (hiSomes) should not (contain ("hi"))
        }
        implicit val e = new Equality[String] {
          def areEqual(a: String, b: Any): Boolean = a != b
        }
        all (hiSomes) should not (contain ("hi"))
        intercept[TestFailedException] {
          all (hiSomes) should not (contain ("ho"))
        }
      }
      def `should use an explicitly provided Equality` {
        all (hiSomes) should not (contain ("HI"))
        all (hiSomes) should not (contain ("HI "))
        intercept[TestFailedException] {
          (all (hiSomes) should not (contain ("HI"))) (decided by defaultEquality afterBeing lowerCased)
        }
        intercept[TestFailedException] {
          (all (hiSomes) should not (contain ("HI "))) (after being trimmed and lowerCased)
        }
      }
    }
    object `when used with (not contain value) syntax` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {

        all (some1s) should (not contain 2)
        atLeast (2, somes) should (not contain 2)
        atMost (2, somes) should (not contain 1)
        no (some1s) should (not contain 1) // I will recommend against double negatives, but we should test it
        all (nones) should (not contain 1)
        all (somesNone) should (not contain 3)

        val e1 = intercept[TestFailedException] {
          all (somes) should (not contain 2)
        }
        e1.failedCodeFileName.get should be ("OptionShouldContainSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 2, Some(2) contained element 2 (OptionShouldContainSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in Vector(Some(1), Some(1), Some(2))"))

        val e2 = intercept[TestFailedException] {
          atMost (2, nones) should (not contain "ho")
        }
        e2.failedCodeFileName.get should be ("OptionShouldContainSpec.scala")
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some("'atMost(2)' inspection failed, because 3 elements satisfied the assertion block at index 0, 1 and 2 in Vector(None, None, None)"))
      }
      def `should use the implicit Equality in scope` {
        all (hiSomes) should (not contain "ho")
        intercept[TestFailedException] {
          all (hiSomes) should (not contain "hi")
        }
        implicit val e = new Equality[String] {
          def areEqual(a: String, b: Any): Boolean = a != b
        }
        all (hiSomes) should (not contain "hi")
        intercept[TestFailedException] {
          all (hiSomes) should (not contain "ho")
        }
      }
      def `should use an explicitly provided Equality` {
        all (hiSomes) should (not contain "HI")
        all (hiSomes) should (not contain "HI ")
        intercept[TestFailedException] {
          (all (hiSomes) should (not contain "HI")) (decided by defaultEquality afterBeing lowerCased)
        }
        intercept[TestFailedException] {
          (all (hiSomes) should (not contain "HI ")) (after being trimmed and lowerCased)
        }
      }
    }
  }
}
