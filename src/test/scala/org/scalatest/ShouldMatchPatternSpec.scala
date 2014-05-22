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
package org.scalatest

import OptionValues._
import org.scalactic.Prettifier
import SharedHelpers.thisLineNumber
import Matchers._

class ShouldMatchPatternSpec extends FunSpec with OptionValues {

  case class Person(firstName: String, lastName: String)

  val result = Person("Bob", "Mc")
  val result2 = Person("Alice", "Ms")

  def didNotMatchTheGivenPattern(left: Any): String =
    Prettifier.default(left) + " did not match the given pattern"

  def matchedTheGivenPattern(left: Any): String =
    Prettifier.default(left) + " matched the given pattern"
  def equaled(left: Any, right: Any): String =
    FailureMessages("equaled", left, right)

  def didNotEqual(left: Any, right: Any): String =
    FailureMessages("didNotEqual", left, right)

  def wasEqualTo(left: Any, right: Any): String =
    FailureMessages("wasEqualTo", left, right)

  def wasNotEqualTo(left: Any, right: Any): String =
    FailureMessages("wasNotEqualTo", left, right)

  describe("should matchPattern syntax") {

    it("should do nothing when checking the right pattern") {
      result should matchPattern { case Person("Bob", _) => }
    }

    it("should throw TestFailedException with correct error message when checking wrong pattern") {
      val e = intercept[TestFailedException] {
        result should matchPattern { case Person("Alice", _) => }
      }
      e.message.value shouldBe didNotMatchTheGivenPattern(result)
      e.failedCodeFileName should be (Some("ShouldMatchPatternSpec.scala"))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
    }

    it("should fail to compile when RHS of case definition is not empty") {
      ("Person(\"Bob\", \"Mc\") should matchPattern { case Person(\"Bob\", last) =>\n" +
      "  last should startWith(\"Mc\")\n" +
      "}") shouldNot compile
    }

    it("should do nothing if pattern matches and used in a logical-and expression") {
      result should (matchPattern { case Person("Bob", _) => } and (equal (result)))
      result should (equal (result) and (matchPattern { case Person("Bob", _) => }))

      result should (matchPattern { case Person("Bob", _) => } and equal (result))
      result should (equal (result) and matchPattern { case Person("Bob", _) => })

      result should (matchPattern { case Person("Bob", _) => } and (be (result)))
      result should (be (result) and (matchPattern { case Person("Bob", _) => }))

      result should (matchPattern { case Person("Bob", _) => } and be (result))
      result should (be (result) and matchPattern { case Person("Bob", _) => })
    }

    it("should throw TFE with correct stack depth if pattern does not match and used in a logical-and expression") {
      val e1 = intercept[TestFailedException] {
        result should (matchPattern { case Person("Alice", _) => } and (equal (result)))
      }
      e1.message should be (Some(didNotMatchTheGivenPattern(result)))
      e1.failedCodeFileName should be (Some("ShouldMatchPatternSpec.scala"))
      e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))

      val e2 = intercept[TestFailedException] {
        result should (equal (result) and (matchPattern { case Person("Alice", _) => }))
      }
      e2.message should be (Some(equaled(result, result) + ", but " + didNotMatchTheGivenPattern(result)))
      e2.failedCodeFileName should be (Some("ShouldMatchPatternSpec.scala"))
      e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))

      val e3 = intercept[TestFailedException] {
        result should (matchPattern { case Person("Alice", _) => } and equal (result))
      }
      e3.message should be (Some(didNotMatchTheGivenPattern(result)))
      e3.failedCodeFileName should be (Some("ShouldMatchPatternSpec.scala"))
      e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))

      val e4 = intercept[TestFailedException] {
        result should (equal (result) and matchPattern { case Person("Alice", _) => })
      }
      e4.message should be (Some(equaled(result, result) + ", but " + didNotMatchTheGivenPattern(result)))
      e4.failedCodeFileName should be (Some("ShouldMatchPatternSpec.scala"))
      e4.failedCodeLineNumber should be (Some(thisLineNumber - 4))

      val e5 = intercept[TestFailedException] {
        result should (matchPattern { case Person("Alice", _) => } and (be (result)))
      }
      e5.message should be (Some(didNotMatchTheGivenPattern(result)))
      e5.failedCodeFileName should be (Some("ShouldMatchPatternSpec.scala"))
      e5.failedCodeLineNumber should be (Some(thisLineNumber - 4))

      val e6 = intercept[TestFailedException] {
        result should (be (result) and (matchPattern { case Person("Alice", _) => }))
      }
      e6.message should be (Some(wasEqualTo(result, result) + ", but " + didNotMatchTheGivenPattern(result)))
      e6.failedCodeFileName should be (Some("ShouldMatchPatternSpec.scala"))
      e6.failedCodeLineNumber should be (Some(thisLineNumber - 4))

      val e7 = intercept[TestFailedException] {
        result should (matchPattern { case Person("Alice", _) => } and be (result))
      }
      e7.message should be (Some(didNotMatchTheGivenPattern(result)))
      e7.failedCodeFileName should be (Some("ShouldMatchPatternSpec.scala"))
      e7.failedCodeLineNumber should be (Some(thisLineNumber - 4))

      val e8 = intercept[TestFailedException] {
        result should (be (result) and matchPattern { case Person("Alice", _) => })
      }
      e8.message should be (Some(wasEqualTo(result, result) + ", but " + didNotMatchTheGivenPattern(result)))
      e8.failedCodeFileName should be (Some("ShouldMatchPatternSpec.scala"))
      e8.failedCodeLineNumber should be (Some(thisLineNumber - 4))
    }

    it("should fail to compile if RHS of case definition is not empty and used in a logical-and expression") {
      "Person(\"Bob\", \"Mc\") should (matchPattern { case Person(\"Bob\", last) => last should startWith(\"Mc\") } and (equal (result)))" shouldNot compile
      "Person(\"Bob\", \"Mc\") should (equal (result) and (matchPattern { case Person(\"Bob\", last) => last should startWith(\"Mc\") }))" shouldNot compile

      "Person(\"Bob\", \"Mc\") should (matchPattern { case Person(\"Bob\", last) => last should startWith(\"Mc\") } and equal (result))" shouldNot compile
      "Person(\"Bob\", \"Mc\") should (equal (result) and matchPattern { case Person(\"Bob\", last) => last should startWith(\"Mc\") })" shouldNot compile

      "Person(\"Bob\", \"Mc\") should (matchPattern { case Person(\"Bob\", last) => last should startWith(\"Mc\") } and (be (result)))" shouldNot compile
      "Person(\"Bob\", \"Mc\") should (be (result) and (matchPattern { case Person(\"Bob\", last) => last should startWith(\"Mc\") }))" shouldNot compile

      "Person(\"Bob\", \"Mc\") should (matchPattern { case Person(\"Bob\", last) => last should startWith(\"Mc\") } and be (result))" shouldNot compile
      "Person(\"Bob\", \"Mc\") should (be (result) and matchPattern { case Person(\"Bob\", last) => last should startWith(\"Mc\") })" shouldNot compile
    }

    it("should do nothing if pattern matches and used in a logical-or expression") {
      result should (matchPattern { case Person("Bob", _) => } or (equal (result)))
      result should (equal (result) or (matchPattern { case Person("Bob", _) => }))

      result should (matchPattern { case Person("Bob", _) => } or equal (result))
      result should (equal (result) or matchPattern { case Person("Bob", _) => })

      result should (matchPattern { case Person("Bob", _) => } or (be (result)))
      result should (be (result) or (matchPattern { case Person("Bob", _) => }))

      result should (matchPattern { case Person("Bob", _) => } or be (result))
      result should (be (result) or matchPattern { case Person("Bob", _) => })

      result should (matchPattern { case Person("Bob", _) => } or (equal (result)))
      result should (equal (result) or (matchPattern { case Person("Bob", _) => }))

      result should (matchPattern { case Person("Bob", _) => } or equal (result))
      result should (equal (result) or matchPattern { case Person("Bob", _) => })

      result should (matchPattern { case Person("Bob", _) => } or (be (result)))
      result should (be (result) or (matchPattern { case Person("Bob", _) => }))

      result should (matchPattern { case Person("Bob", _) => } or be (result))
      result should (be (result) or matchPattern { case Person("Bob", _) => })

      result should (matchPattern { case Person("Bob", _) => } or (equal (result2)))
      result should (equal (result2) or (matchPattern { case Person("Bob", _) => }))

      result should (matchPattern { case Person("Bob", _) => } or equal (result2))
      result should (equal (result2) or matchPattern { case Person("Bob", _) => })

      result should (matchPattern { case Person("Bob", _) => } or (be (result2)))
      result should (be (result2) or (matchPattern { case Person("Bob", _) => }))

      result should (matchPattern { case Person("Bob", _) => } or be (result2))
      result should (be (result2) or matchPattern { case Person("Bob", _) => })
    }

    it("should throw TFE with correct stack depth if pattern does not match and used in a logical-or expression") {
      val e1 = intercept[TestFailedException] {
        result should (matchPattern { case Person("Alice", _) => } or (equal (result2)))
      }
      e1.message should be (Some(didNotMatchTheGivenPattern(result) + ", and " + didNotEqual(result, result2)))
      e1.failedCodeFileName should be (Some("ShouldMatchPatternSpec.scala"))
      e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))

      val e2 = intercept[TestFailedException] {
        result should (equal (result2) or (matchPattern { case Person("Alice", _) => }))
      }
      e2.message should be (Some(didNotEqual(result, result2) + ", and " + didNotMatchTheGivenPattern(result)))
      e2.failedCodeFileName should be (Some("ShouldMatchPatternSpec.scala"))
      e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))

      val e3 = intercept[TestFailedException] {
        result should (matchPattern { case Person("Alice", _) => } or equal (result2))
      }
      e3.message should be (Some(didNotMatchTheGivenPattern(result) + ", and " + didNotEqual(result, result2)))
      e3.failedCodeFileName should be (Some("ShouldMatchPatternSpec.scala"))
      e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))

      val e4 = intercept[TestFailedException] {
        result should (equal (result2) or matchPattern { case Person("Alice", _) => })
      }
      e4.message should be (Some(didNotEqual(result, result2) + ", and " + didNotMatchTheGivenPattern(result)))
      e4.failedCodeFileName should be (Some("ShouldMatchPatternSpec.scala"))
      e4.failedCodeLineNumber should be (Some(thisLineNumber - 4))

      val e5 = intercept[TestFailedException] {
        result should (matchPattern { case Person("Alice", _) => } or (be (result2)))
      }
      e5.message should be (Some(didNotMatchTheGivenPattern(result) + ", and " + wasNotEqualTo(result, result2)))
      e5.failedCodeFileName should be (Some("ShouldMatchPatternSpec.scala"))
      e5.failedCodeLineNumber should be (Some(thisLineNumber - 4))

      val e6 = intercept[TestFailedException] {
        result should (be (result2) or (matchPattern { case Person("Alice", _) => }))
      }
      e6.message should be (Some(wasNotEqualTo(result, result2) + ", and " + didNotMatchTheGivenPattern(result)))
      e6.failedCodeFileName should be (Some("ShouldMatchPatternSpec.scala"))
      e6.failedCodeLineNumber should be (Some(thisLineNumber - 4))

      val e7 = intercept[TestFailedException] {
        result should (matchPattern { case Person("Alice", _) => } or be (result2))
      }
      e7.message should be (Some(didNotMatchTheGivenPattern(result) + ", and " + wasNotEqualTo(result, result2)))
      e7.failedCodeFileName should be (Some("ShouldMatchPatternSpec.scala"))
      e7.failedCodeLineNumber should be (Some(thisLineNumber - 4))

      val e8 = intercept[TestFailedException] {
        result should (be (result2) or matchPattern { case Person("Alice", _) => })
      }
      e8.message should be (Some(wasNotEqualTo(result, result2) + ", and " + didNotMatchTheGivenPattern(result)))
      e8.failedCodeFileName should be (Some("ShouldMatchPatternSpec.scala"))
      e8.failedCodeLineNumber should be (Some(thisLineNumber - 4))
    }

    it("fail to compile if RHS of case definition is not empty and used in a logical-or expression") {
      "Person(\"Bob\", \"Mc\") should (matchPattern { case Person(\"Bob\", last) => last should startWith(\"Mc\") } or (equal (result)))" shouldNot compile
      "Person(\"Bob\", \"Mc\") should (equal (result) or (matchPattern { case Person(\"Bob\", last) => last should startWith(\"Mc\") }))" shouldNot compile

      "Person(\"Bob\", \"Mc\") should (matchPattern { case Person(\"Bob\", last) => last should startWith(\"Mc\") } or equal (result))" shouldNot compile
      "Person(\"Bob\", \"Mc\") should (equal (result) or matchPattern { case Person(\"Bob\", last) => last should startWith(\"Mc\") })" shouldNot compile

      "Person(\"Bob\", \"Mc\") should (matchPattern { case Person(\"Bob\", last) => last should startWith(\"Mc\") } or (be (result)))" shouldNot compile
      "Person(\"Bob\", \"Mc\") should (be (result) or (matchPattern { case Person(\"Bob\", last) => last should startWith(\"Mc\") }))" shouldNot compile

      "Person(\"Bob\", \"Mc\") should (matchPattern { case Person(\"Bob\", last) => last should startWith(\"Mc\") } or be (result))" shouldNot compile
      "Person(\"Bob\", \"Mc\") should (be (result) or matchPattern { case Person(\"Bob\", last) => last should startWith(\"Mc\") })" shouldNot compile

      "Person(\"Bob\", \"Mc\") should (matchPattern { case Person(\"Bob\", last) => last should startWith(\"Mc\") } or (equal (result)))" shouldNot compile
      "Person(\"Bob\", \"Mc\") should (equal (result) or (matchPattern { case Person(\"Bob\", last) => last should startWith(\"Mc\") }))" shouldNot compile

      "Person(\"Bob\", \"Mc\") should (matchPattern { case Person(\"Bob\", last) => last should startWith(\"Mc\") } or equal (result))" shouldNot compile
      "Person(\"Bob\", \"Mc\") should (equal (result) or matchPattern { case Person(\"Bob\", last) => last should startWith(\"Mc\") })" shouldNot compile

      "Person(\"Bob\", \"Mc\") should (matchPattern { case Person(\"Bob\", last) => last should startWith(\"Mc\") } or (be (result)))" shouldNot compile
      "Person(\"Bob\", \"Mc\") should (be (result) or (matchPattern { case Person(\"Bob\", last) => last should startWith(\"Mc\") }))" shouldNot compile

      "Person(\"Bob\", \"Mc\") should (matchPattern { case Person(\"Bob\", last) => last should startWith(\"Mc\") } or be (result))" shouldNot compile
      "Person(\"Bob\", \"Mc\") should (be (result) or matchPattern { case Person(\"Bob\", last) => last should startWith(\"Mc\") })" shouldNot compile

      "Person(\"Bob\", \"Mc\") should (matchPattern { case Person(\"Bob\", last) => last should startWith(\"Mc\") } or (equal (result2)))" shouldNot compile
      "Person(\"Bob\", \"Mc\") should (equal (result2) or (matchPattern { case Person(\"Bob\", last) => last should startWith(\"Mc\") }))" shouldNot compile

      "Person(\"Bob\", \"Mc\") should (matchPattern { case Person(\"Bob\", last) => last should startWith(\"Mc\") } or equal (result2))" shouldNot compile
      "Person(\"Bob\", \"Mc\") should (equal (result2) or matchPattern { case Person(\"Bob\", last) => last should startWith(\"Mc\") })" shouldNot compile

      "Person(\"Bob\", \"Mc\") should (matchPattern { case Person(\"Bob\", last) => last should startWith(\"Mc\") } or (be (result2)))" shouldNot compile
      "Person(\"Bob\", \"Mc\") should (be (result2) or (matchPattern { case Person(\"Bob\", last) => last should startWith(\"Mc\") }))" shouldNot compile

      "Person(\"Bob\", \"Mc\") should (matchPattern { case Person(\"Bob\", last) => last should startWith(\"Mc\") } or be (result2))" shouldNot compile
      "Person(\"Bob\", \"Mc\") should (be (result2) or matchPattern { case Person(\"Bob\", last) => last should startWith(\"Mc\") })" shouldNot compile
    }
  }

  describe("should not matchPattern syntax") {

    it("should do nothing when checking the wrong pattern") {
      result should not matchPattern { case Person("Alice", _) => }
    }

    it("should throw TestFailedException with correct error message when checking right pattern") {
      val e = intercept[TestFailedException] {
        result should not matchPattern { case Person("Bob", _) => }
      }
      e.message.value shouldBe matchedTheGivenPattern(result)
      e.failedCodeFileName should be (Some("ShouldMatchPatternSpec.scala"))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
    }

    it("should fail to compile when RHS of case definition is not empty") {
      ("Person(\"Bob\", \"Mc\") should not matchPattern { case Person(\"Bob\", last) =>\n" +
        "  last should startWith(\"Mc\")\n" +
        "}") shouldNot compile
    }

    it("should do nothing if pattern does not match and used in a logical-and expression and not") {
      result should (not matchPattern { case Person("Alice", _) => } and (equal (result)))
      result should (equal (result) and (not matchPattern { case Person("Alice", _) => }))

      result should (not matchPattern { case Person("Alice", _) => } and equal (result))
      result should (equal (result) and not matchPattern { case Person("Alice", _) => })

      result should (not matchPattern { case Person("Alice", _) => } and (be (result)))
      result should (be (result) and (not matchPattern { case Person("Alice", _) => }))

      result should (not matchPattern { case Person("Alice", _) => } and be (result))
      result should (be (result) and not matchPattern { case Person("Alice", _) => })
    }

    it("should do nothing if pattern matches and used in a logical-and expression and not") {

      val e1 = intercept[TestFailedException] {
        result should (not matchPattern { case Person("Bob", _) => } and (equal (result)))
      }
      e1.message should be (Some(matchedTheGivenPattern(result)))
      e1.failedCodeFileName should be (Some("ShouldMatchPatternSpec.scala"))
      e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))

      val e2 = intercept[TestFailedException] {
        result should (equal (result) and (not matchPattern { case Person("Bob", _) => }))
      }
      e2.message should be (Some(equaled(result, result) + ", but " + matchedTheGivenPattern(result)))
      e2.failedCodeFileName should be (Some("ShouldMatchPatternSpec.scala"))
      e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))

      val e3 = intercept[TestFailedException] {
        result should (not matchPattern { case Person("Bob", _) => } and equal (result))
      }
      e3.message should be (Some(matchedTheGivenPattern(result)))
      e3.failedCodeFileName should be (Some("ShouldMatchPatternSpec.scala"))
      e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))

      val e4 = intercept[TestFailedException] {
        result should (equal (result) and not matchPattern { case Person("Bob", _) => })
      }
      e4.message should be (Some(equaled(result, result) + ", but " + matchedTheGivenPattern(result)))
      e4.failedCodeFileName should be (Some("ShouldMatchPatternSpec.scala"))
      e4.failedCodeLineNumber should be (Some(thisLineNumber - 4))

      val e5 = intercept[TestFailedException] {
        result should (not matchPattern { case Person("Bob", _) => } and (be (result)))
      }
      e5.message should be (Some(matchedTheGivenPattern(result)))
      e5.failedCodeFileName should be (Some("ShouldMatchPatternSpec.scala"))
      e5.failedCodeLineNumber should be (Some(thisLineNumber - 4))

      val e6 = intercept[TestFailedException] {
        result should (be (result) and (not matchPattern { case Person("Bob", _) => }))
      }
      e6.message should be (Some(wasEqualTo(result, result) + ", but " + matchedTheGivenPattern(result)))
      e6.failedCodeFileName should be (Some("ShouldMatchPatternSpec.scala"))
      e6.failedCodeLineNumber should be (Some(thisLineNumber - 4))

      val e7 = intercept[TestFailedException] {
        result should (not matchPattern { case Person("Bob", _) => } and be (result))
      }
      e7.message should be (Some(matchedTheGivenPattern(result)))
      e7.failedCodeFileName should be (Some("ShouldMatchPatternSpec.scala"))
      e7.failedCodeLineNumber should be (Some(thisLineNumber - 4))

      val e8 = intercept[TestFailedException] {
        result should (be (result) and not matchPattern { case Person("Bob", _) => })
      }
      e8.message should be (Some(wasEqualTo(result, result) + ", but " + matchedTheGivenPattern(result)))
      e8.failedCodeFileName should be (Some("ShouldMatchPatternSpec.scala"))
      e8.failedCodeLineNumber should be (Some(thisLineNumber - 4))
    }

    it("should fail to compile if RHS of case definition is not empty and used in a logical-and expression and not") {
      "Person(\"Bob\", \"Mc\") should (not matchPattern { case Person(\"Alice\", last) => last should startWith(\"Mc\") } and (equal (result)))" shouldNot compile
      "Person(\"Bob\", \"Mc\") should (equal (result) and (not matchPattern { case Person(\"Alice\", _) => last should startWith(\"Mc\") }))" shouldNot compile

      "Person(\"Bob\", \"Mc\") should (not matchPattern { case Person(\"Alice\", last) => last should startWith(\"Mc\") } and equal (result))" shouldNot compile
      "Person(\"Bob\", \"Mc\") should (equal (result) and not matchPattern { case Person(\"Alice\", _) => last should startWith(\"Mc\") })" shouldNot compile

      "Person(\"Bob\", \"Mc\") should (not matchPattern { case Person(\"Alice\", last) => last should startWith(\"Mc\") } and (be (result)))" shouldNot compile
      "Person(\"Bob\", \"Mc\") should (be (result) and (not matchPattern { case Person(\"Alice\", _) => last should startWith(\"Mc\") }))" shouldNot compile

      "Person(\"Bob\", \"Mc\") should (not matchPattern { case Person(\"Alice\", last) => last should startWith(\"Mc\") } and be (result))" shouldNot compile
      "Person(\"Bob\", \"Mc\") should (be (result) and not matchPattern { case Person(\"Alice\", _) => last should startWith(\"Mc\") })" shouldNot compile
    }

    it("should do nothing if pattern does not match and used in a logical-or expression and not") {
      result should (not matchPattern { case Person("Alice", _) => } or (equal (result)))
      result should (equal (result) or (not matchPattern { case Person("Alice", _) => }))

      result should (not matchPattern { case Person("Alice", _) => } or equal (result))
      result should (equal (result) or not matchPattern { case Person("Alice", _) => })

      result should (not matchPattern { case Person("Alice", _) => } or (be (result)))
      result should (be (result) or (not matchPattern { case Person("Alice", _) => }))

      result should (not matchPattern { case Person("Alice", _) => } or be (result))
      result should (be (result) or not matchPattern { case Person("Alice", _) => })

      result should (not matchPattern { case Person("Alice", _) => } or (equal (result)))
      result should (equal (result) or (not matchPattern { case Person("Alice", _) => }))

      result should (not matchPattern { case Person("Alice", _) => } or equal (result))
      result should (equal (result) or not matchPattern { case Person("Alice", _) => })

      result should (not matchPattern { case Person("Alice", _) => } or (be (result)))
      result should (be (result) or (not matchPattern { case Person("Alice", _) => }))

      result should (not matchPattern { case Person("Alice", _) => } or be (result))
      result should (be (result) or not matchPattern { case Person("Alice", _) => })

      result should (not matchPattern { case Person("Alice", _) => } or (equal (result2)))
      result should (equal (result2) or (not matchPattern { case Person("Alice", _) => }))

      result should (not matchPattern { case Person("Alice", _) => } or equal (result2))
      result should (equal (result2) or not matchPattern { case Person("Alice", _) => })

      result should (not matchPattern { case Person("Alice", _) => } or (be (result2)))
      result should (be (result2) or (not matchPattern { case Person("Alice", _) => }))

      result should (not matchPattern { case Person("Alice", _) => } or be (result2))
      result should (be (result2) or not matchPattern { case Person("Alice", _) => })
    }

    it("should throw TFE with correct stack depth if pattern matches and used in a logical-or expression and not") {
      val e1 = intercept[TestFailedException] {
        result should (not matchPattern { case Person("Bob", _) => } or (equal (result2)))
      }
      e1.message should be (Some(matchedTheGivenPattern(result) + ", and " + didNotEqual(result, result2)))
      e1.failedCodeFileName should be (Some("ShouldMatchPatternSpec.scala"))
      e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))

      val e2 = intercept[TestFailedException] {
        result should (equal (result2) or (not matchPattern { case Person("Bob", _) => }))
      }
      e2.message should be (Some(didNotEqual(result, result2) + ", and " + matchedTheGivenPattern(result)))
      e2.failedCodeFileName should be (Some("ShouldMatchPatternSpec.scala"))
      e2.failedCodeLineNumber should be (Some(thisLineNumber - 4))

      val e3 = intercept[TestFailedException] {
        result should (not matchPattern { case Person("Bob", _) => } or equal (result2))
      }
      e3.message should be (Some(matchedTheGivenPattern(result) + ", and " + didNotEqual(result, result2)))
      e3.failedCodeFileName should be (Some("ShouldMatchPatternSpec.scala"))
      e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))

      val e4 = intercept[TestFailedException] {
        result should (equal (result2) or not matchPattern { case Person("Bob", _) => })
      }
      e4.message should be (Some(didNotEqual(result, result2) + ", and " + matchedTheGivenPattern(result)))
      e4.failedCodeFileName should be (Some("ShouldMatchPatternSpec.scala"))
      e4.failedCodeLineNumber should be (Some(thisLineNumber - 4))

      val e5 = intercept[TestFailedException] {
        result should (not matchPattern { case Person("Bob", _) => } or (be (result2)))
      }
      e5.message should be (Some(matchedTheGivenPattern(result) + ", and " + wasNotEqualTo(result, result2)))
      e5.failedCodeFileName should be (Some("ShouldMatchPatternSpec.scala"))
      e5.failedCodeLineNumber should be (Some(thisLineNumber - 4))

      val e6 = intercept[TestFailedException] {
        result should (be (result2) or (not matchPattern { case Person("Bob", _) => }))
      }
      e6.message should be (Some(wasNotEqualTo(result, result2) + ", and " + matchedTheGivenPattern(result)))
      e6.failedCodeFileName should be (Some("ShouldMatchPatternSpec.scala"))
      e6.failedCodeLineNumber should be (Some(thisLineNumber - 4))

      val e7 = intercept[TestFailedException] {
        result should (not matchPattern { case Person("Bob", _) => } or be (result2))
      }
      e7.message should be (Some(matchedTheGivenPattern(result) + ", and " + wasNotEqualTo(result, result2)))
      e7.failedCodeFileName should be (Some("ShouldMatchPatternSpec.scala"))
      e7.failedCodeLineNumber should be (Some(thisLineNumber - 4))

      val e8 = intercept[TestFailedException] {
        result should (be (result2) or not matchPattern { case Person("Bob", _) => })
      }
      e8.message should be (Some(wasNotEqualTo(result, result2) + ", and " + matchedTheGivenPattern(result)))
      e8.failedCodeFileName should be (Some("ShouldMatchPatternSpec.scala"))
      e8.failedCodeLineNumber should be (Some(thisLineNumber - 4))
    }

    it("should fail to compile if RHS of case definition is not empty and used in a logical-or expression and not") {
      "Person(\"Bob\", \"Mc\") should (not matchPattern { case Person(\"Alice\", last) => last should startWith(\"Mc\") } or (equal (result)))" shouldNot compile
      "Person(\"Bob\", \"Mc\") should (equal (result) or (not matchPattern { case Person(\"Alice\", last) => last should startWith(\"Mc\") }))" shouldNot compile

      "Person(\"Bob\", \"Mc\") should (not matchPattern { case Person(\"Alice\", last) => last should startWith(\"Mc\") } or equal (result))" shouldNot compile
      "Person(\"Bob\", \"Mc\") should (equal (result) or not matchPattern { case Person(\"Alice\", last) => last should startWith(\"Mc\") })" shouldNot compile

      "Person(\"Bob\", \"Mc\") should (not matchPattern { case Person(\"Alice\", last) => last should startWith(\"Mc\") } or (be (result)))" shouldNot compile
      "Person(\"Bob\", \"Mc\") should (be (result) or (not matchPattern { case Person(\"Alice\", last) => last should startWith(\"Mc\") }))" shouldNot compile

      "Person(\"Bob\", \"Mc\") should (not matchPattern { case Person(\"Alice\", last) => last should startWith(\"Mc\") } or be (result))" shouldNot compile
      "Person(\"Bob\", \"Mc\") should (be (result) or not matchPattern { case Person(\"Alice\", last) => last should startWith(\"Mc\") })" shouldNot compile

      "Person(\"Bob\", \"Mc\") should (not matchPattern { case Person(\"Alice\", last) => last should startWith(\"Mc\") } or (equal (result)))" shouldNot compile
      "Person(\"Bob\", \"Mc\") should (equal (result) or (not matchPattern { case Person(\"Alice\", last) => last should startWith(\"Mc\") }))" shouldNot compile

      "Person(\"Bob\", \"Mc\") should (not matchPattern { case Person(\"Alice\", last) => last should startWith(\"Mc\") } or equal (result))" shouldNot compile
      "Person(\"Bob\", \"Mc\") should (equal (result) or not matchPattern { case Person(\"Alice\", last) => last should startWith(\"Mc\") })" shouldNot compile

      "Person(\"Bob\", \"Mc\") should (not matchPattern { case Person(\"Alice\", last) => last should startWith(\"Mc\") } or (be (result)))" shouldNot compile
      "Person(\"Bob\", \"Mc\") should (be (result) or (not matchPattern { case Person(\"Alice\", last) => last should startWith(\"Mc\") }))" shouldNot compile

      "Person(\"Bob\", \"Mc\") should (not matchPattern { case Person(\"Alice\", last) => last should startWith(\"Mc\") } or be (result))" shouldNot compile
      "Person(\"Bob\", \"Mc\") should (be (result) or not matchPattern { case Person(\"Alice\", last) => last should startWith(\"Mc\") })" shouldNot compile

      "Person(\"Bob\", \"Mc\") should (not matchPattern { case Person(\"Alice\", last) => last should startWith(\"Mc\") } or (equal (result2)))" shouldNot compile
      "Person(\"Bob\", \"Mc\") should (equal (result2) or (not matchPattern { case Person(\"Alice\", last) => last should startWith(\"Mc\") }))" shouldNot compile

      "Person(\"Bob\", \"Mc\") should (not matchPattern { case Person(\"Alice\", last) => last should startWith(\"Mc\") } or equal (result2))" shouldNot compile
      "Person(\"Bob\", \"Mc\") should (equal (result2) or not matchPattern { case Person(\"Alice\", last) => last should startWith(\"Mc\") })" shouldNot compile

      "Person(\"Bob\", \"Mc\") should (not matchPattern { case Person(\"Alice\", last) => last should startWith(\"Mc\") } or (be (result2)))" shouldNot compile
      "Person(\"Bob\", \"Mc\") should (be (result2) or (not matchPattern { case Person(\"Alice\", last) => last should startWith(\"Mc\") }))" shouldNot compile

      "Person(\"Bob\", \"Mc\") should (not matchPattern { case Person(\"Alice\", last) => last should startWith(\"Mc\") } or be (result2))" shouldNot compile
      "Person(\"Bob\", \"Mc\") should (be (result2) or not matchPattern { case Person(\"Alice\", last) => last should startWith(\"Mc\") })" shouldNot compile
    }

  }

  describe("shouldNot matchPattern syntax") {

    it("should do nothing when checking the wrong pattern") {
      result shouldNot matchPattern { case Person("Alice", _) => }
    }

    it("should throw TestFailedException with correct error message when checking right pattern") {
      val e = intercept[TestFailedException] {
        result should not matchPattern { case Person("Bob", _) => }
      }
      e.message.value shouldBe matchedTheGivenPattern(result)
      e.failedCodeFileName should be (Some("ShouldMatchPatternSpec.scala"))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
    }

    it("should fail to compile when RHS of case definition is not empty") {
      ("result should not matchPattern { case Person(\"Bob\", last) =>\n" +
        "  last should startWith(\"Mc\")\n" +
        "}") shouldNot compile
    }

  }

}