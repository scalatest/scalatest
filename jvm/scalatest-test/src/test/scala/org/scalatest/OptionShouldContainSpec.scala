/*
 * Copyright 2001-2025 Artima, Inc.
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

import org.scalactic.Equality
import org.scalactic.NormalizingEquality
import org.scalactic.Prettifier
import org.scalactic.StringNormalizations._
import SharedHelpers._
import FailureMessages.decorateToStringValue
import exceptions.TestFailedException
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class OptionShouldContainSpec extends AnyFunSpec with Matchers {

  private val prettifier = Prettifier.default

  describe("an Option") {

    val some: Option[String] = Some("hi")
    val none: Option[String] = None
    val someNull: Option[String] = Some(null)

    describe("when used with contain (value) syntax") {
      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        some should contain ("hi")
        Some("hi") should contain ("hi")

        val e1 = intercept[TestFailedException] {
          some should contain ("ho")
        }
        e1.message.get should be (Resources.didNotContainExpectedElement(decorateToStringValue(prettifier, some), "\"ho\""))
        e1.failedCodeFileName.get should be ("OptionShouldContainSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 4)

        val e2 = intercept[TestFailedException] {
          none should contain ("ho")
        }
        e2.message.get should be (Resources.didNotContainExpectedElement(decorateToStringValue(prettifier, none), "\"ho\""))
        e2.failedCodeFileName.get should be ("OptionShouldContainSpec.scala")
        e2.failedCodeLineNumber.get should be (thisLineNumber - 4)

        val e4 = intercept[TestFailedException] {
          Some("hi") should contain ("ho")
        }
        e4.message.get should be (Resources.didNotContainExpectedElement(decorateToStringValue(prettifier, some), "\"ho\""))
        e4.failedCodeFileName.get should be ("OptionShouldContainSpec.scala")
        e4.failedCodeLineNumber.get should be (thisLineNumber - 4)
      }
      it("should use the implicit Equality in scope") {
        some should contain ("hi")
        intercept[TestFailedException] {
          some should contain ("ho")
        }

        {
          implicit val e = new Equality[String] {
            def areEqual(a: String, b: Any): Boolean = a != b
          }
          some should contain ("ho")
          intercept[TestFailedException] {
            some should contain ("hi")
          }
        }
      }
      it("should use an explicitly provided Equality") {
        intercept[TestFailedException] {
          some should contain ("HI")
        }
        (some should contain ("HI")) (decided by defaultEquality afterBeing lowerCased)
        (some should contain ("HI")) (after being lowerCased)
        intercept[TestFailedException] {
          (some should contain ("HI ")) (after being lowerCased)
        }
        (some should contain ("HI ")) (after being lowerCased and trimmed)
        
        {
          implicit val e = new Equality[String] {
            def areEqual(a: String, b: Any): Boolean = a != b
          }
          intercept[TestFailedException] {
            some should contain ("hi")
          }
          (some should contain ("hi")) (decided by defaultEquality[String])
        }
      }
      it("should do nothing when used with null and LHS contains null value") {
        someNull should contain (null)
      }
      it("should throw TFE with correct stack depth and error message when LHS did not contain null value") {
        val e = intercept[TestFailedException] {
          some should contain (null)
        }
        e.message.get should be (Resources.didNotContainNull(decorateToStringValue(prettifier, some)))
        e.failedCodeFileName.get should be ("OptionShouldContainSpec.scala")
        e.failedCodeLineNumber.get should be (thisLineNumber - 4)
      }
    }

    describe("when used with not contain value syntax") {
      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        some should not contain "ho"
        Some("hi") should not contain "ho"
        none should not contain "hi"

        val e3 = intercept[TestFailedException] {
          some should not contain "hi"
        }
        e3.message.get should be (Resources.containedExpectedElement(decorateToStringValue(prettifier, some), "\"hi\""))
        e3.failedCodeFileName.get should be ("OptionShouldContainSpec.scala")
        e3.failedCodeLineNumber.get should be (thisLineNumber - 4)

        val e5 = intercept[TestFailedException] {
          Some("hi") should not contain "hi"
        }
        e5.message.get should be (Resources.containedExpectedElement(decorateToStringValue(prettifier, some), "\"hi\""))
        e5.failedCodeFileName.get should be ("OptionShouldContainSpec.scala")
        e5.failedCodeLineNumber.get should be (thisLineNumber - 4)
      }
      it("should use the implicit Equality in scope") {
        some should not contain "ho"
        intercept[TestFailedException] {
          some should not contain "hi"
        }

        {
          implicit val e = new Equality[String] {
            def areEqual(a: String, b: Any): Boolean = a != b
          }
          some should not contain "hi"
          intercept[TestFailedException] {
            some should not contain "ho"
          }
        }
      }
      it("should use an explicitly provided Equality") {
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
      it("should do nothing when used with null and LHS did not contain null value") {
        some should not contain (null)
      }
      it("should throw TFE with correct stack depth and error message when LHS contained null value") {
        val e = intercept[TestFailedException] {
          someNull should not contain (null)
        }
        e.message.get should be (Resources.containedNull(decorateToStringValue(prettifier, someNull)))
        e.failedCodeFileName.get should be ("OptionShouldContainSpec.scala")
        e.failedCodeLineNumber.get should be (thisLineNumber - 4)
      }
    }

    describe("when used with not (contain (value)) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {

        some should not (contain ("ho"))
        Some("hi") should not (contain ("ho"))
        none should not (contain ("hi"))

        val e3 = intercept[TestFailedException] {
          some should not (contain ("hi"))
        }
        e3.message.get should be (Resources.containedExpectedElement(decorateToStringValue(prettifier, some), "\"hi\""))
        e3.failedCodeFileName.get should be ("OptionShouldContainSpec.scala")
        e3.failedCodeLineNumber.get should be (thisLineNumber - 4)

        val e5 = intercept[TestFailedException] {
          Some("hi") should not (contain ("hi"))
        }
        e5.message.get should be (Resources.containedExpectedElement(decorateToStringValue(prettifier, some), "\"hi\""))
        e5.failedCodeFileName.get should be ("OptionShouldContainSpec.scala")
        e5.failedCodeLineNumber.get should be (thisLineNumber - 4)
      }

      it("should use the implicit Equality in scope") {
        some should not (contain ("ho"))
        intercept[TestFailedException] {
          some should not (contain ("hi"))
        }

        {
          implicit val e = new Equality[String] {
            def areEqual(a: String, b: Any): Boolean = a != b
          }
          some should not (contain ("hi"))
          intercept[TestFailedException] {
            some should not (contain ("ho"))
          }
        }
      }
      it("should use an explicitly provided Equality") {
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
      it("should do nothing when used with null and LHS did not contain null value") {
        some should not (contain (null))
      }
      it("should throw TFE with correct stack depth and error message when LHS contained null value") {
        val e = intercept[TestFailedException] {
          someNull should not (contain (null))
        }
        e.message.get should be (Resources.containedNull(decorateToStringValue(prettifier, someNull)))
        e.failedCodeFileName.get should be ("OptionShouldContainSpec.scala")
        e.failedCodeLineNumber.get should be (thisLineNumber - 4)
      }
    }

    describe("when used with (not contain value) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        some should (not contain "ho")
        Some("hi") should (not contain ("ho"))
        none should (not contain "hi")

        val e3 = intercept[TestFailedException] {
          some should (not contain "hi")
        }
        e3.message.get should be (Resources.containedExpectedElement(decorateToStringValue(prettifier, some), "\"hi\""))
        e3.failedCodeFileName.get should be ("OptionShouldContainSpec.scala")
        e3.failedCodeLineNumber.get should be (thisLineNumber - 4)

        val e5 = intercept[TestFailedException] {
          Some("hi") should (not contain "hi")
        }
        e5.message.get should be (Resources.containedExpectedElement(decorateToStringValue(prettifier, some), "\"hi\""))
        e5.failedCodeFileName.get should be ("OptionShouldContainSpec.scala")
        e5.failedCodeLineNumber.get should be (thisLineNumber - 4)
      }

      it("should use the implicit Equality in scope") {
        some should (not contain "ho")
        intercept[TestFailedException] {
          some should (not contain "hi")
        }

        {
          implicit val e = new Equality[String] {
            def areEqual(a: String, b: Any): Boolean = a != b
          }
          some should (not contain "hi")
          intercept[TestFailedException] {
            some should (not contain "ho")
          }
        }
      }
      it("should use an explicitly provided Equality") {
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
      it("should do nothing when used with null and LHS did not contain null value") {
        some should (not contain (null))
      }
      it("should throw TFE with correct stack depth and error message when LHS contained null value") {
        val e = intercept[TestFailedException] {
          someNull should (not contain (null))
        }
        e.message.get should be (Resources.containedNull(decorateToStringValue(prettifier, someNull)))
        e.failedCodeFileName.get should be ("OptionShouldContainSpec.scala")
        e.failedCodeLineNumber.get should be (thisLineNumber - 4)
      }
    }

    describe("when used with shouldNot contain value syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        some shouldNot contain ("ho")
        none shouldNot contain ("hi")

        val e3 = intercept[TestFailedException] {
          some shouldNot contain ("hi")
        }
        e3.message.get should be (Resources.containedExpectedElement(decorateToStringValue(prettifier, some), "\"hi\""))
        e3.failedCodeFileName.get should be ("OptionShouldContainSpec.scala")
        e3.failedCodeLineNumber.get should be (thisLineNumber - 4)
      }

      it("should use the implicit Equality in scope") {
        some shouldNot contain ("ho")
        intercept[TestFailedException] {
          some shouldNot contain ("hi")
        }

        {
          implicit val e = new Equality[String] {
            def areEqual(a: String, b: Any): Boolean = a != b
          }
          some shouldNot contain ("hi")
          intercept[TestFailedException] {
            some shouldNot contain ("ho")
          }
        }
      }
      it("should use an explicitly provided Equality") {
        some shouldNot contain ("HI")
        some shouldNot contain ("HI ")
        (some shouldNot contain ("HI ")) (decided by defaultEquality afterBeing lowerCased)
        (some shouldNot contain ("HI ")) (after being lowerCased)
        intercept[TestFailedException] {
          (some shouldNot contain ("HI")) (decided by defaultEquality afterBeing lowerCased)
        }
        intercept[TestFailedException] {
          (some shouldNot contain ("HI ")) (after being lowerCased and trimmed)
        }
      }
      it("should minimize normalization if an implicit NormalizingEquality is in scope") {
        some shouldNot contain ("HI")
        var normalizedInvokedCount = 0

        {
          implicit val e = new NormalizingEquality[String] {
            def normalized(s: String): String = {
              normalizedInvokedCount = normalizedInvokedCount + 1
              s.toLowerCase
            }
            def normalizedCanHandle(b: Any): Boolean = b.isInstanceOf[String]
            def normalizedOrSame(b: Any) =
              b match {
                case s: String => normalized(s)
                case _ => b
              }
          }
          intercept[TestFailedException] {
            some shouldNot contain ("HI")
          }
          normalizedInvokedCount should be (2)
        }
      }
      it("should do nothing when used with null and LHS did not contain null value") {
        some shouldNot contain (null)
      }
      it("should throw TFE with correct stack depth and error message when LHS contained null value") {
        val e = intercept[TestFailedException] {
          someNull shouldNot contain (null)
        }
        e.message.get should be (Resources.containedNull(decorateToStringValue(prettifier, someNull)))
        e.failedCodeFileName.get should be ("OptionShouldContainSpec.scala")
        e.failedCodeLineNumber.get should be (thisLineNumber - 4)
      }
    }

    describe("when used with shouldNot (contain (value)) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {

        some shouldNot (contain ("ho"))
        none shouldNot (contain ("hi"))

        val e3 = intercept[TestFailedException] {
          some shouldNot (contain ("hi"))
        }
        e3.message.get should be (Resources.containedExpectedElement(decorateToStringValue(prettifier, some), "\"hi\""))
        e3.failedCodeFileName.get should be ("OptionShouldContainSpec.scala")
        e3.failedCodeLineNumber.get should be (thisLineNumber - 4)
      }

      it("should use the implicit Equality in scope") {
        some shouldNot (contain ("ho"))
        intercept[TestFailedException] {
          some shouldNot (contain ("hi"))
        }

        {
          implicit val e = new Equality[String] {
            def areEqual(a: String, b: Any): Boolean = a != b
          }
          some shouldNot (contain ("hi"))
          intercept[TestFailedException] {
            some shouldNot (contain ("ho"))
          }
        }
      }
      it("should use an explicitly provided Equality") {
        some shouldNot (contain ("HI"))
        some shouldNot (contain ("HI "))
        (some shouldNot (contain ("HI "))) (decided by defaultEquality afterBeing lowerCased)
        (some shouldNot (contain ("HI "))) (after being lowerCased)
        intercept[TestFailedException] {
          (some shouldNot (contain ("HI"))) (decided by defaultEquality afterBeing lowerCased)
        }
        intercept[TestFailedException] {
          (some shouldNot (contain ("HI"))) (after being lowerCased)
        }
        intercept[TestFailedException] {
          (some shouldNot (contain ("HI "))) (after being lowerCased and trimmed)
        }
      }
      it("should minimize normalization if an implicit NormalizingEquality is in scope") {
        some shouldNot (contain ("HI"))
        var normalizedInvokedCount = 0

        {
          implicit val e = new NormalizingEquality[String] {
            def normalized(s: String): String = {
              normalizedInvokedCount = normalizedInvokedCount + 1
              s.toLowerCase
            }
            def normalizedCanHandle(b: Any): Boolean = b.isInstanceOf[String]
            def normalizedOrSame(b: Any) =
              b match {
                case s: String => normalized(s)
                case _ => b
              }
          }
          intercept[TestFailedException] {
            some shouldNot (contain ("HI"))
          }
          normalizedInvokedCount should be (2)
        }
      }
      it("should do nothing when used with null and LHS did not contain null value") {
        some shouldNot (contain (null))
      }
      it("should throw TFE with correct stack depth and error message when LHS contained null value") {
        val e = intercept[TestFailedException] {
          someNull shouldNot (contain (null))
        }
        e.message.get should be (Resources.containedNull(decorateToStringValue(prettifier, someNull)))
        e.failedCodeFileName.get should be ("OptionShouldContainSpec.scala")
        e.failedCodeLineNumber.get should be (thisLineNumber - 4)
      }
    }
  }

  describe("a collection of Options") {

    val some1s: Vector[Option[Int]] = Vector(Some(1), Some(1), Some(1))
    val somes: Vector[Option[Int]] = Vector(Some(1), Some(1), Some(2))
    val nones: Vector[Option[Int]] = Vector(None, None, None)
    val somesNone: Vector[Option[Int]] = Vector(Some(1), Some(1), None)
    val hiSomes: Vector[Option[String]] = Vector(Some("hi"), Some("hi"), Some("hi"))
    val hiNullSomes: Vector[Option[String]] = Vector(Some(null), Some(null), Some(null))

    describe("when used with contain (value) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {

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

        val e4 = intercept[TestFailedException] {
          all (somes) should contain (1)
        }
        e4.failedCodeFileName.get should be ("OptionShouldContainSpec.scala")
        e4.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e4.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 2, Some(2) did not contain element 1 (OptionShouldContainSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in Vector(Some(1), Some(1), Some(2))"))
      }
      it("should use the implicit Equality in scope") {
        intercept[TestFailedException] {
          all (hiSomes) should contain ("ho")
        }

        {
          implicit val e = new Equality[String] {
            def areEqual(a: String, b: Any): Boolean = a != b
          }
          all (hiSomes) should contain ("ho")
          intercept[TestFailedException] {
            all (hiSomes) should contain ("hi")
          }
        }
      }
      it("should use an explicitly provided Equality") {
        intercept[TestFailedException] {
          all (somes) should contain ("HI")
        }
        intercept[TestFailedException] {
          all (hiSomes) should contain ("HI ")
        }
        (all (hiSomes) should contain ("HI")) (decided by defaultEquality afterBeing lowerCased)
        (all (hiSomes) should contain ("HI ")) (after being trimmed and lowerCased)
      }
      it("should do nothing when used with null and LHS contains null value") {
        all (hiNullSomes) should contain (null)
      }
      it("should throw TFE with correct stack depth and error message when all elements of LHS did not contain null value") {
        val e = intercept[TestFailedException] {
          all (hiSomes) should contain (null)
        }
        e.failedCodeFileName.get should be ("OptionShouldContainSpec.scala")
        e.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 0, " + decorateToStringValue(prettifier, hiSomes(0)) + " did not contain null (OptionShouldContainSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in " + decorateToStringValue(prettifier, hiSomes)))
      }
    }
    describe("when used with not contain value syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {

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
      it("should use the implicit Equality in scope") {
        all (hiSomes) should not contain "ho"
        intercept[TestFailedException] {
          all (hiSomes) should not contain "hi"
        }

        {
          implicit val e = new Equality[String] {
            def areEqual(a: String, b: Any): Boolean = a != b
          }
          all (hiSomes) should not contain "hi"
          intercept[TestFailedException] {
            all (hiSomes) should not contain "ho"
          }
        }
      }
      it("should use an explicitly provided Equality") {
        all (hiSomes) should not contain "HI"
        all (hiSomes) should not contain "HI "
        intercept[TestFailedException] {
          (all (hiSomes) should not contain "HI") (decided by defaultEquality afterBeing lowerCased)
        }
        intercept[TestFailedException] {
          (all (hiSomes) should not contain "HI ") (after being trimmed and lowerCased)
        }
      }
      it("should do nothing when used with null and LHS did not contain null value") {
        all (hiSomes) should not contain (null)
      }
      it("should throw TFE with correct stack depth and error message when one of elements of LHS contained null value") {
        val e = intercept[TestFailedException] {
          all (hiNullSomes) should not contain (null)
        }
        e.failedCodeFileName.get should be ("OptionShouldContainSpec.scala")
        e.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 0, " + decorateToStringValue(prettifier, hiNullSomes(0)) + " contained null (OptionShouldContainSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in " + decorateToStringValue(prettifier, hiNullSomes)))
      }
    }
    describe("when used with not (contain (value)) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {

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
      it("should use the implicit Equality in scope") {
        all (hiSomes) should not (contain ("ho"))
        intercept[TestFailedException] {
          all (hiSomes) should not (contain ("hi"))
        }

        {
          implicit val e = new Equality[String] {
            def areEqual(a: String, b: Any): Boolean = a != b
          }
          all (hiSomes) should not (contain ("hi"))
          intercept[TestFailedException] {
            all (hiSomes) should not (contain ("ho"))
          }
        }
      }
      it("should use an explicitly provided Equality") {
        all (hiSomes) should not (contain ("HI"))
        all (hiSomes) should not (contain ("HI "))
        intercept[TestFailedException] {
          (all (hiSomes) should not (contain ("HI"))) (decided by defaultEquality afterBeing lowerCased)
        }
        intercept[TestFailedException] {
          (all (hiSomes) should not (contain ("HI "))) (after being trimmed and lowerCased)
        }
      }
      it("should do nothing when used with null and LHS did not contain null value") {
        all (hiSomes) should not (contain (null))
      }
      it("should throw TFE with correct stack depth and error message when one of elements of LHS contained null value") {
        val e = intercept[TestFailedException] {
          all (hiNullSomes) should not (contain (null))
        }
        e.failedCodeFileName.get should be ("OptionShouldContainSpec.scala")
        e.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 0, " + decorateToStringValue(prettifier, hiNullSomes(0)) + " contained null (OptionShouldContainSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in " + decorateToStringValue(prettifier, hiNullSomes)))
      }
    }
    describe("when used with (not contain value) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {

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
      it("should use the implicit Equality in scope") {
        all (hiSomes) should (not contain "ho")
        intercept[TestFailedException] {
          all (hiSomes) should (not contain "hi")
        }

        {
          implicit val e = new Equality[String] {
            def areEqual(a: String, b: Any): Boolean = a != b
          }
          all (hiSomes) should (not contain "hi")
          intercept[TestFailedException] {
            all (hiSomes) should (not contain "ho")
          }
        }
      }
      it("should use an explicitly provided Equality") {
        all (hiSomes) should (not contain "HI")
        all (hiSomes) should (not contain "HI ")
        intercept[TestFailedException] {
          (all (hiSomes) should (not contain "HI")) (decided by defaultEquality afterBeing lowerCased)
        }
        intercept[TestFailedException] {
          (all (hiSomes) should (not contain "HI ")) (after being trimmed and lowerCased)
        }
      }
      it("should do nothing when used with null and LHS did not contain null value") {
        all (hiSomes) should (not contain (null))
      }
      it("should throw TFE with correct stack depth and error message when one of elements of LHS contained null value") {
        val e = intercept[TestFailedException] {
          all (hiNullSomes) should (not contain (null))
        }
        e.failedCodeFileName.get should be ("OptionShouldContainSpec.scala")
        e.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 0, " + decorateToStringValue(prettifier, hiNullSomes(0)) + " contained null (OptionShouldContainSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in " + decorateToStringValue(prettifier, hiNullSomes)))
      }
    }

    describe("when used with shouldNot contain value syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {

        all (some1s) shouldNot contain (4)
        atLeast (2, somes) shouldNot contain (2)
        atMost (2, somes) shouldNot contain (1)
        no (some1s) shouldNot contain (1) // I will recommend against double negatives, but we should test it
        all (nones) shouldNot contain (1)
        all (somesNone) shouldNot contain (4)

        val e1 = intercept[TestFailedException] {
          all (somes) shouldNot contain (2)
        }
        e1.failedCodeFileName.get should be ("OptionShouldContainSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 2, " + decorateToStringValue(prettifier, somes(2)) + " contained element 2 (OptionShouldContainSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in " + decorateToStringValue(prettifier, somes)))

        val e2 = intercept[TestFailedException] {
          atMost (2, nones) shouldNot contain ("ho")
        }
        e2.failedCodeFileName.get should be ("OptionShouldContainSpec.scala")
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some("'atMost(2)' inspection failed, because 3 elements satisfied the assertion block at index 0, 1 and 2 in " + decorateToStringValue(prettifier, nones)))
      }
      it("should use the implicit Equality in scope") {
        all (hiSomes) shouldNot contain ("ho")
        intercept[TestFailedException] {
          all (hiSomes) shouldNot contain ("hi")
        }

        {
          implicit val e = new Equality[String] {
            def areEqual(a: String, b: Any): Boolean = a != b
          }
          all (hiSomes) shouldNot contain ("hi")
          intercept[TestFailedException] {
            all (hiSomes) shouldNot contain ("ho")
          }
        }
      }
      it("should use an explicitly provided Equality") {
        all (hiSomes) shouldNot contain ("HI")
        all (hiSomes) shouldNot contain ("HI ")
        intercept[TestFailedException] {
          (all (hiSomes) shouldNot contain ("HI")) (decided by defaultEquality afterBeing lowerCased)
        }
        intercept[TestFailedException] {
          (all (hiSomes) shouldNot contain ("HI ")) (after being trimmed and lowerCased)
        }
      }
      it("should do nothing when used with null and LHS did not contain null value") {
        all (hiSomes) shouldNot contain (null)
      }
      it("should throw TFE with correct stack depth and error message when one of elements of LHS contained null value") {
        val e = intercept[TestFailedException] {
          all (hiNullSomes) shouldNot contain (null)
        }
        e.failedCodeFileName.get should be ("OptionShouldContainSpec.scala")
        e.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 0, " + decorateToStringValue(prettifier, hiNullSomes(0)) + " contained null (OptionShouldContainSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in " + decorateToStringValue(prettifier, hiNullSomes)))
      }
    }

    describe("when used with shouldNot (contain value) syntax") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {

        all (some1s) shouldNot (contain (4))
        atLeast (2, somes) shouldNot (contain (2))
        atMost (2, somes) shouldNot (contain (1))
        no (some1s) shouldNot (contain (1)) // I will recommend against double negatives, but we should test it
        all (nones) shouldNot (contain (1))
        all (somesNone) shouldNot (contain (4))

        val e1 = intercept[TestFailedException] {
          all (somes) shouldNot (contain (2))
        }
        e1.failedCodeFileName.get should be ("OptionShouldContainSpec.scala")
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 2, " + decorateToStringValue(prettifier, somes(2)) + " contained element 2 (OptionShouldContainSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in " + decorateToStringValue(prettifier, somes)))

        val e2 = intercept[TestFailedException] {
          atMost (2, nones) shouldNot (contain ("ho"))
        }
        e2.failedCodeFileName.get should be ("OptionShouldContainSpec.scala")
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some("'atMost(2)' inspection failed, because 3 elements satisfied the assertion block at index 0, 1 and 2 in " + decorateToStringValue(prettifier, nones)))
      }
      it("should use the implicit Equality in scope") {
        all (hiSomes) shouldNot (contain ("ho"))
        intercept[TestFailedException] {
          all (hiSomes) shouldNot (contain ("hi"))
        }

        {
          implicit val e = new Equality[String] {
            def areEqual(a: String, b: Any): Boolean = a != b
          }
          all (hiSomes) shouldNot (contain ("hi"))
          intercept[TestFailedException] {
            all (hiSomes) shouldNot (contain ("ho"))
          }
        }
      }
      it("should use an explicitly provided Equality") {
        all (hiSomes) shouldNot (contain ("HI"))
        all (hiSomes) shouldNot (contain ("HI "))
        intercept[TestFailedException] {
          (all (hiSomes) shouldNot (contain ("HI"))) (decided by defaultEquality afterBeing lowerCased)
        }
        intercept[TestFailedException] {
          (all (hiSomes) shouldNot (contain ("HI "))) (after being trimmed and lowerCased)
        }
      }
      it("should do nothing when used with null and LHS did not contain null value") {
        all (hiSomes) shouldNot (contain (null))
      }
      it("should throw TFE with correct stack depth and error message when one of elements of LHS contained null value") {
        val e = intercept[TestFailedException] {
          all (hiNullSomes) shouldNot (contain (null))
        }
        e.failedCodeFileName.get should be ("OptionShouldContainSpec.scala")
        e.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e.message should be (Some("'all' inspection failed, because: \n" +
          "  at index 0, " + decorateToStringValue(prettifier, hiNullSomes(0)) + " contained null (OptionShouldContainSpec.scala:" + (thisLineNumber - 5) + ") \n" +
          "in " + decorateToStringValue(prettifier, hiNullSomes)))
      }
    }
  }
}
