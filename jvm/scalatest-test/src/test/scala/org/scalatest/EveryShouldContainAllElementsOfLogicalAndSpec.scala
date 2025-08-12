/*
* Copyright 2001-2025 Artima, Inc.
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
package org.scalatest

import org.scalactic.{Equality, Every, One, Many, Prettifier}
import org.scalactic.StringNormalizations._
import SharedHelpers._
import FailureMessages.decorateToStringValue
import org.scalatest.matchers.{MatchResult, BeMatcher}
import exceptions.TestFailedException
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class EveryShouldContainAllElementsOfLogicalAndSpec extends AnyFunSpec {

  val prettifier = Prettifier.default

  val invertedListOfStringEquality =
    new Equality[Every[String]] {
      def areEqual(a: Every[String], b: Any): Boolean = a != b
    }

  val upperCaseStringEquality =
    new Equality[String] {
      def areEqual(a: String, b: Any): Boolean = upperCase(a) == upperCase(b)
    }

  private def upperCase(value: Any): Any =
    value match {
      case l: Every[_] => l.map(upperCase(_))
      case s: String => s.toUpperCase
      case c: Char => c.toString.toUpperCase.charAt(0)
      case (s1: String, s2: String) => (s1.toUpperCase, s2.toUpperCase)
      case e: java.util.Map.Entry[_, _] =>
        (e.getKey, e.getValue) match {
          case (k: String, v: String) => Entry(k.toUpperCase, v.toUpperCase)
          case _ => value
        }
      case _ => value
    }

  val upperCaseListOfStringEquality =
    new Equality[Every[String]] {
      def areEqual(a: Every[String], b: Any): Boolean = upperCase(a) == upperCase(b)
    }

  //ADDITIONAL//

  val fileName: String = "EveryShouldContainAllElementsOfLogicalAndSpec.scala"

  describe("an Every") {

    val fumList: Every[String] = Every("fex", "fum", "foe", "fie", "fee")
    val toList: Every[String] = Every("too", "you", "to", "birthday", "happy")

    describe("when used with (contain allElementsOf Seq(..) and contain allElementsOf Seq(..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (contain allElementsOf Seq("fee", "fie", "foe", "fum") and contain allElementsOf Seq("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain allElementsOf Seq("happy", "birthday", "to", "you") and contain allElementsOf Seq("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, FailureMessages.didNotContainAllElementsOf(prettifier, fumList, Seq("happy", "birthday", "to", "you")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (contain allElementsOf Seq("fee", "fie", "foe", "fum") and contain allElementsOf Seq("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e2, FailureMessages.containedAllElementsOf(prettifier, fumList, Seq("fee", "fie", "foe", "fum")) + ", but " + FailureMessages.didNotContainAllElementsOf(prettifier, fumList, Seq("happy", "birthday", "to", "you")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (contain allElementsOf Seq("FEE", "FIE", "FOE", "FUM") and contain allElementsOf Seq("FEE", "FIE", "FUM", "FOE"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain allElementsOf Seq("FEE", "FIE", "FOE", "FAM") and contain allElementsOf Seq("FEE", "FIE", "FUM", "FOE"))
        }
        checkMessageStackDepth(e1, FailureMessages.didNotContainAllElementsOf(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FAM")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (contain allElementsOf Seq("FEE", "FIE", "FOE", "FUM") and (contain allElementsOf Seq("FEE", "FIE", "FAM", "FOE")))
        }
        checkMessageStackDepth(e2, FailureMessages.containedAllElementsOf(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FUM")) + ", but " + FailureMessages.didNotContainAllElementsOf(prettifier, fumList, Seq("FEE", "FIE", "FAM", "FOE")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (contain allElementsOf Seq("FEE", "FIE", "FOE", "FUM") and contain allElementsOf Seq("FEE", "FIE", "FUM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain allElementsOf Seq("FEE", "FIE", "FOE", "FAM") and contain allElementsOf Seq("FEE", "FIE", "FUM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.didNotContainAllElementsOf(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FAM")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (contain allElementsOf Seq("FEE", "FIE", "FOE", "FUM") and contain allElementsOf Seq("FEE", "FIE", "FAM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, FailureMessages.containedAllElementsOf(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FUM")) + ", but " + FailureMessages.didNotContainAllElementsOf(prettifier, fumList, Seq("FEE", "FIE", "FAM", "FOE")), fileName, thisLineNumber - 2)
        (fumList should (contain allElementsOf Seq(" FEE ", " FIE ", " FOE ", " FUM ") and contain allElementsOf Seq(" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }

      it("should allow RHS to contain duplicated value") {
        fumList should (contain allElementsOf Seq("fee", "fie", "foe", "fie", "fum") and contain allElementsOf Seq("fie", "fee", "fum", "foe"))
        fumList should (contain allElementsOf Seq("fie", "fee", "fum", "foe") and contain allElementsOf Seq("fee", "fie", "foe", "fie", "fum"))
      }
    }

    describe("when used with (equal (..) and contain allElementsOf Seq(..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (equal (fumList) and contain allElementsOf Seq("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) and contain allElementsOf Seq("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, FailureMessages.didNotEqual(prettifier, fumList, toList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (equal (fumList) and contain allElementsOf Seq("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e2, FailureMessages.equaled(prettifier, fumList, fumList) + ", but " + FailureMessages.didNotContainAllElementsOf(prettifier, fumList, Seq("happy", "birthday", "to", "you")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (equal (fumList) and contain allElementsOf Seq("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) and contain allElementsOf Seq("FEE", "FIE", "FOE", "FUM"))
        }
        checkMessageStackDepth(e1, FailureMessages.didNotEqual(prettifier, fumList, toList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (equal (fumList) and (contain allElementsOf Seq("FEE", "FIE", "FOE", "FAM")))
        }
        checkMessageStackDepth(e2, FailureMessages.equaled(prettifier, fumList, fumList) + ", but " + FailureMessages.didNotContainAllElementsOf(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FAM")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (equal (toList) and contain allElementsOf Seq("FEE", "FIE", "FOE", "FUM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (equal (toList) and contain allElementsOf Seq("FEE", "FIE", "FOE", "FAM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.equaled(prettifier, fumList, toList) + ", but " + FailureMessages.didNotContainAllElementsOf(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FAM")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (equal (fumList) and contain allElementsOf Seq("FEE", "FIE", "FOE", "FUM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, FailureMessages.didNotEqual(prettifier, fumList, fumList), fileName, thisLineNumber - 2)
        (fumList should (equal (toList) and contain allElementsOf Seq(" FEE ", " FIE ", " FOE ", " FUM "))) (decided by invertedListOfStringEquality, after being lowerCased and trimmed)
      }

      it("should allow RHS to contain duplicated value") {
        fumList should (equal (fumList) and contain allElementsOf Seq("fee", "fie", "foe", "fie", "fum"))
      }
    }

    describe("when used with (be (..) and contain allElementsOf Seq(..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (be (fumList) and contain allElementsOf Seq("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (be (toList) and contain allElementsOf Seq("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, FailureMessages.wasNotEqualTo(prettifier, fumList, toList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (be (fumList) and contain allElementsOf Seq("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e2, FailureMessages.wasEqualTo(prettifier, fumList, fumList) + ", but " + FailureMessages.didNotContainAllElementsOf(prettifier, fumList, Seq("happy", "birthday", "to", "you")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (be (fumList) and contain allElementsOf Seq("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (be (toList) and contain allElementsOf Seq("FEE", "FIE", "FOE", "FUM"))
        }
        checkMessageStackDepth(e1, FailureMessages.wasNotEqualTo(prettifier, fumList, toList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (be (fumList) and (contain allElementsOf Seq("happy", "birthday", "to", "you")))
        }
        checkMessageStackDepth(e2, FailureMessages.wasEqualTo(prettifier, fumList, fumList) + ", but " + FailureMessages.didNotContainAllElementsOf(prettifier, fumList, Seq("happy", "birthday", "to", "you")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (be (fumList) and contain allElementsOf Seq("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (be (fumList) and contain allElementsOf Seq("happy", "birthday", "to", "you"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.wasEqualTo(prettifier, fumList, fumList) + ", but " + FailureMessages.didNotContainAllElementsOf(prettifier, fumList, Seq("happy", "birthday", "to", "you")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (be (toList) and contain allElementsOf Seq("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, FailureMessages.wasNotEqualTo(prettifier, fumList, toList), fileName, thisLineNumber - 2)
        (fumList should (be (fumList) and contain allElementsOf Seq(" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed)
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        fumList should (be (fumList) and contain allElementsOf Seq("fee", "fie", "foe", "fie", "fum"))
      }
    }

    describe("when used with (contain allElementsOf Seq(..) and be (..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (contain allElementsOf Seq("fie", "fee", "fum", "foe") and be (fumList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain allElementsOf Seq("fee", "fie", "foe", "fum") and be (toList))
        }
        checkMessageStackDepth(e1, FailureMessages.containedAllElementsOf(prettifier, fumList, Seq("fee", "fie", "foe", "fum")) + ", but " + FailureMessages.wasNotEqualTo(prettifier, fumList, toList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (contain allElementsOf Seq("happy", "birthday", "to", "you") and be (fumList))
        }
        checkMessageStackDepth(e2, FailureMessages.didNotContainAllElementsOf(prettifier, fumList, Seq("happy", "birthday", "to", "you")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (contain allElementsOf Seq("FIE", "FEE", "FUM", "FOE") and be (fumList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain allElementsOf Seq("FIE", "FEE", "FAM", "FOE") and be (toList))
        }
        checkMessageStackDepth(e1, FailureMessages.didNotContainAllElementsOf(prettifier, fumList, Seq("FIE", "FEE", "FAM", "FOE")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (contain allElementsOf Seq("HAPPY", "BIRTHDAY", "TO", "YOU") and (be (fumList)))
        }
        checkMessageStackDepth(e2, FailureMessages.didNotContainAllElementsOf(prettifier, fumList, Seq("HAPPY", "BIRTHDAY", "TO", "YOU")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (contain allElementsOf Seq("FIE", "FEE", "FUM", "FOE") and be (fumList))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain allElementsOf Seq("FIE", "FEE", "FAM", "FOE") and be (fumList))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.didNotContainAllElementsOf(prettifier, fumList, Seq("FIE", "FEE", "FAM", "FOE")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (contain allElementsOf Seq("FIE", "FEE", "FUM", "FOE") and be (toList))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, FailureMessages.containedAllElementsOf(prettifier, fumList, Seq("FIE", "FEE", "FUM", "FOE")) + ", but " + FailureMessages.wasNotEqualTo(prettifier, fumList, toList), fileName, thisLineNumber - 2)
        (fumList should (contain allElementsOf Seq(" FEE ", " FIE ", " FOE ", " FUM ") and be (fumList))) (after being lowerCased and trimmed)
      }

      it("should allow RHS to contain duplicated value") {
        fumList should (contain allElementsOf Seq("fee", "fie", "foe", "fie", "fum") and be (fumList))
      }
    }

    describe("when used with (not contain allElementsOf xx and not contain allElementsOf xx)") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (not contain allElementsOf (Seq("fee", "fie", "foe", "fuu")) and not contain allElementsOf (Seq("fie", "fee", "fuu", "foe")))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain allElementsOf (Seq("fee", "fie", "foe", "fum")) and not contain allElementsOf (Seq("happy", "birthday", "to", "you")))
        }
        checkMessageStackDepth(e1, FailureMessages.containedAllElementsOf(prettifier, fumList, Seq("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not contain allElementsOf (Seq("happy", "birthday", "to", "you")) and not contain allElementsOf (Seq("fee", "fie", "foe", "fum")))
        }
        checkMessageStackDepth(e2, FailureMessages.didNotContainAllElementsOf(prettifier, fumList, Seq("happy", "birthday", "to", "you")) + ", but " + FailureMessages.containedAllElementsOf(prettifier, fumList, Seq("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (not contain allElementsOf (Seq("FIE", "FEE", "FAM", "FOE")) and not contain allElementsOf (Seq("FIE", "FEE", "FOE", "FAM")))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain allElementsOf (Seq("FIE", "FEE", "FUM", "FOE")) and not contain allElementsOf (Seq("FIE", "FEE", "FOE", "FAM")))
        }
        checkMessageStackDepth(e1, FailureMessages.containedAllElementsOf(prettifier, fumList, Seq("FIE", "FEE", "FUM", "FOE")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not contain allElementsOf (Seq("FIE", "FEE", "FAM", "FOE")) and (not contain allElementsOf (Seq("FIE", "FEE", "FOE", "FUM"))))
        }
        checkMessageStackDepth(e2, FailureMessages.didNotContainAllElementsOf(prettifier, fumList, Seq("FIE", "FEE", "FAM", "FOE")) + ", but " + FailureMessages.containedAllElementsOf(prettifier, fumList, Seq("FIE", "FEE", "FOE", "FUM")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (not contain allElementsOf (Seq("FIE", "FEE", "FAM", "FOE")) and not contain allElementsOf (Seq("FIE", "FEE", "FOE", "FAM")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not contain allElementsOf (Seq("FIE", "FEE", "FAM", "FOE")) and not contain allElementsOf (Seq("FIE", "FEE", "FOE", "FUM")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.didNotContainAllElementsOf(prettifier, fumList, Seq("FIE", "FEE", "FAM", "FOE")) + ", but " + FailureMessages.containedAllElementsOf(prettifier, fumList, Seq("FIE", "FEE", "FOE", "FUM")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (not contain allElementsOf (Seq("FIE", "FEE", "FUM", "FOE")) and not contain allElementsOf (Seq("FIE", "FEE", "FOE", "FAM")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, FailureMessages.containedAllElementsOf(prettifier, fumList, Seq("FIE", "FEE", "FUM", "FOE")), fileName, thisLineNumber - 2)
        (fumList should (contain allElementsOf Seq(" FEE ", " FIE ", " FOE ", " FUM ") and contain allElementsOf Seq(" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }

      it("should allow RHS to contain duplicated value") {
        fumList should (not contain allElementsOf (Seq("fee", "fie", "foe", "fie", "fam")) and not contain allElementsOf (Seq("fie", "fee", "fuu", "foe")))
        fumList should (not contain allElementsOf (Seq("fie", "fee", "fuu", "foe")) and not contain allElementsOf (Seq("fee", "fie", "foe", "fie", "fam")))
      }
    }

    describe("when used with (not equal (..) and not contain allElementsOf Seq(..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (not equal (toList) and not contain allElementsOf (Seq("fie", "fee", "fuu", "foe")))
        val e1 = intercept[TestFailedException] {
          fumList should (not equal (fumList) and not contain allElementsOf (Seq("happy", "birthday", "to", "you")))
        }
        checkMessageStackDepth(e1, FailureMessages.equaled(prettifier, fumList, fumList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not equal (toList) and not contain allElementsOf (Seq("fee", "fie", "foe", "fum")))
        }
        checkMessageStackDepth(e2, FailureMessages.didNotEqual(prettifier, fumList, toList) + ", but " + FailureMessages.containedAllElementsOf(prettifier, fumList, Seq("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (not equal (toList) and not contain allElementsOf (Seq("FIE", "FEE", "FAM", "FOE")))
        val e1 = intercept[TestFailedException] {
          fumList should (not equal (fumList) and not contain allElementsOf (Seq("FIE", "FEE", "FAM", "FOE")))
        }
        checkMessageStackDepth(e1, FailureMessages.equaled(prettifier, fumList, fumList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not equal (toList) and (not contain allElementsOf (Seq("FIE", "FEE", "FUM", "FOE"))))
        }
        checkMessageStackDepth(e2, FailureMessages.didNotEqual(prettifier, fumList, toList) + ", but " + FailureMessages.containedAllElementsOf(prettifier, fumList, Seq("FIE", "FEE", "FUM", "FOE")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (not equal (fumList) and not contain allElementsOf (Seq("FIE", "FEE", "FAM", "FOE")))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not equal (fumList) and not contain allElementsOf (Seq("FIE", "FEE", "FUM", "FOE")))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.didNotEqual(prettifier, fumList, fumList) + ", but " + FailureMessages.containedAllElementsOf(prettifier, fumList, Seq("FIE", "FEE", "FUM", "FOE")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (not equal (toList) and not contain allElementsOf (Seq("FIE", "FEE", "FAM", "FOE")))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, FailureMessages.equaled(prettifier, fumList, toList), fileName, thisLineNumber - 2)
        (fumList should (not contain allElementsOf (Seq(" FEE ", " FIE ", " FOE ", " FUU ")) and not contain allElementsOf (Seq(" FEE ", " FIE ", " FOE ", " FUU ")))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }

      it("should allow RHS to contain duplicated value") {
        fumList should (not equal (toList) and not contain allElementsOf (Seq("fee", "fie", "foe", "fie", "fam")))
      }
    }

    describe("when used with (not be (..) and not contain allElementsOf Seq(..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (not be (toList) and not contain allElementsOf (Seq("fie", "fee", "fuu", "foe")))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) and not contain allElementsOf (Seq("happy", "birthday", "to", "you")))
        }
        checkMessageStackDepth(e1, FailureMessages.wasEqualTo(prettifier, fumList, fumList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not be (toList) and not contain allElementsOf (Seq("fee", "fie", "foe", "fum")))
        }
        checkMessageStackDepth(e2, FailureMessages.wasNotEqualTo(prettifier, fumList, toList) + ", but " + FailureMessages.containedAllElementsOf(prettifier, fumList, Seq("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (not be (toList) and not contain allElementsOf (Seq("FIE", "FEE", "FAM", "FOE")))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) and not contain allElementsOf (Seq("FIE", "FEE", "FAM", "FOE")))
        }
        checkMessageStackDepth(e1, FailureMessages.wasEqualTo(prettifier, fumList, fumList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not be (toList) and (not contain allElementsOf (Seq("FIE", "FEE", "FUM", "FOE"))))
        }
        checkMessageStackDepth(e2, FailureMessages.wasNotEqualTo(prettifier, fumList, toList) + ", but " + FailureMessages.containedAllElementsOf(prettifier, fumList, Seq("FIE", "FEE", "FUM", "FOE")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (not be (toList) and not contain allElementsOf (Seq("FIE", "FEE", "FAM", "FOE")))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not be (toList) and not contain allElementsOf (Seq("FIE", "FEE", "FUM", "FOE")))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.wasNotEqualTo(prettifier, fumList, toList) + ", but " + FailureMessages.containedAllElementsOf(prettifier, fumList, Seq("FIE", "FEE", "FUM", "FOE")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (not be (fumList) and not contain allElementsOf (Seq("FIE", "FEE", "FAM", "FOE")))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, FailureMessages.wasEqualTo(prettifier, fumList, fumList), fileName, thisLineNumber - 2)
        (fumList should (not contain allElementsOf (Seq(" FEE ", " FIE ", " FOE ", " FUU ")) and not contain allElementsOf (Seq(" FEE ", " FIE ", " FOE ", " FUU ")))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }

      it("should allow RHS to contain duplicated value") {
        fumList should (not be (toList) and not contain allElementsOf (Seq("fee", "fie", "foe", "fie", "fam")))
      }
    }

  }

  describe("col of Lists") {

    val list1s: Every[Every[Int]] = Every(Every(3, 2, 1, 0), Every(3, 2, 1, 0), Every(3, 2, 1, 0))
    val lists: Every[Every[Int]] = Every(Every(3, 2, 1, 0), Every(3, 2, 1, 0), Every(8, 4, 3, 2))
    val hiLists: Every[Every[String]] = Every(Every("howdy", "hi", "hello"), Every("howdy", "hi", "hello"), Every("howdy", "hi", "hello"))

    def allErrMsg(index: Int, message: String, lineNumber: Int, left: Any): String =
      "'all' inspection failed, because: \n" +
        "  at index " + index + ", " + message + " (" + fileName + ":" + (lineNumber) + ") \n" +
        "in " + decorateToStringValue(prettifier, left)

    describe("used with contain allElementsOf xx and contain allElementsOf xx") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (contain allElementsOf Seq(3, 2, 1) and contain allElementsOf Seq(1, 3, 2))
        atLeast (2, lists) should (contain allElementsOf Seq(3, 1, 2) and contain allElementsOf Seq(2, 3, 1))
        atMost (2, lists) should (contain allElementsOf Seq(3, 1, 2) and contain allElementsOf Seq(2, 3, 1))
        no (lists) should (contain allElementsOf Seq(3, 6, 9) and contain allElementsOf Seq(3, 4, 5))

        val e1 = intercept[TestFailedException] {
          all (lists) should (contain allElementsOf Seq(1, 2, 3) and contain allElementsOf Seq(1, 3, 2))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(prettifier, Many(8, 4, 3, 2)) + " did not contain all elements of " + decorateToStringValue(prettifier, List(1, 2, 3)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (list1s) should (contain allElementsOf Seq(1, 2, 3) and contain allElementsOf Seq(1, 3, 4))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, Many(3, 2, 1, 0)) + " contained all elements of " + decorateToStringValue(prettifier, List(1, 2, 3)) + ", but " + decorateToStringValue(prettifier, Many(3, 2, 1, 0)) + " did not contain all elements of " + decorateToStringValue(prettifier, List(1, 3, 4)), thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)

        val e4 = intercept[TestFailedException] {
          all (hiLists) should (contain allElementsOf Seq("hi", "hello") and contain allElementsOf Seq("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e4, allErrMsg(0, decorateToStringValue(prettifier, Many("howdy", "hi", "hello")) + " contained all elements of " + decorateToStringValue(prettifier, List("hi", "hello")) + ", but " + decorateToStringValue(prettifier, Many("howdy", "hi", "hello")) + " did not contain all elements of " + decorateToStringValue(prettifier, List("ho", "hey", "howdy")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (contain allElementsOf Seq("HELLO", "HI") and contain allElementsOf Seq("HI", "HELLO"))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (contain allElementsOf Seq("HO", "HELLO") and contain allElementsOf Seq("HI", "HELLO"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, Many("howdy", "hi", "hello")) + " did not contain all elements of " + decorateToStringValue(prettifier, List("HO", "HELLO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (hiLists) should (contain allElementsOf Seq("HELLO", "HI") and contain allElementsOf Seq("HO", "HELLO"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, Many("howdy", "hi", "hello")) + " contained all elements of " + decorateToStringValue(prettifier, List("HELLO", "HI")) + ", but " + decorateToStringValue(prettifier, Many("howdy", "hi", "hello")) + " did not contain all elements of " + decorateToStringValue(prettifier, List("HO", "HELLO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (all (hiLists) should (contain allElementsOf Seq("HELLO", "HI") and contain allElementsOf Seq("HI", "HELLO"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (contain allElementsOf Seq("HO", "HELLO") and contain allElementsOf Seq("HI", "HELLO"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, Many("howdy", "hi", "hello")) + " did not contain all elements of " + decorateToStringValue(prettifier, List("HO", "HELLO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (contain allElementsOf Seq("HELLO", "HI") and contain allElementsOf Seq("HO", "HELLO"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, Many("howdy", "hi", "hello")) + " contained all elements of " + decorateToStringValue(prettifier, List("HELLO", "HI")) + ", but " + decorateToStringValue(prettifier, Many("howdy", "hi", "hello")) + " did not contain all elements of " + decorateToStringValue(prettifier, List("HO", "HELLO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should allow RHS contain duplicated value") {
        all (list1s) should (contain allElementsOf Seq(3, 2, 2, 1) and contain allElementsOf Seq(1, 3, 2))
        all (list1s) should (contain allElementsOf Seq(1, 3, 2) and contain allElementsOf Seq(3, 2, 2, 1))
      }
    }

    describe("when used with (be (..) and contain allElementsOf Seq(..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (be (Many(3, 2, 1, 0)) and contain allElementsOf Seq(1, 3, 2))
        atLeast (2, lists) should (be (Many(3, 2, 1, 0)) and contain allElementsOf Seq(1, 3, 2))
        atMost (2, lists) should (be (Many(3, 2, 1, 0)) and contain allElementsOf Seq(2, 3, 1))
        no (lists) should (be (Many(3, 6, 9)) and contain allElementsOf Seq(3, 4, 5))

        val e1 = intercept[TestFailedException] {
          all (lists) should (be (Many(3, 2, 1, 0)) and contain allElementsOf Seq(1, 3, 2))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(prettifier, Many(8, 4, 3, 2)) + " was not equal to " + decorateToStringValue(prettifier, Many(3, 2, 1, 0)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (list1s) should (be (Many(3, 2, 1, 0)) and contain allElementsOf Seq(2, 3, 8))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, Many(3, 2, 1, 0)) + " was equal to " + decorateToStringValue(prettifier, Many(3, 2, 1, 0)) + ", but " + decorateToStringValue(prettifier, Many(3, 2, 1, 0)) + " did not contain all elements of " + decorateToStringValue(prettifier, List(2, 3, 8)), thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)

        val e3 = intercept[TestFailedException] {
          all (hiLists) should (be (Many("howdy", "hi", "hello")) and contain allElementsOf Seq("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e3, allErrMsg(0, decorateToStringValue(prettifier, Many("howdy", "hi", "hello")) + " was equal to " + decorateToStringValue(prettifier, Many("howdy", "hi", "hello")) + ", but " + decorateToStringValue(prettifier, Many("howdy", "hi", "hello")) + " did not contain all elements of " + decorateToStringValue(prettifier, List("ho", "hey", "howdy")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e4 = intercept[TestFailedException] {
          all (list1s) should (be (Many(3, 2, 1, 0)) and contain allElementsOf Seq(2, 3, 8))
        }
        checkMessageStackDepth(e4, allErrMsg(0, decorateToStringValue(prettifier, Many(3, 2, 1, 0)) + " was equal to " + decorateToStringValue(prettifier, Many(3, 2, 1, 0)) + ", but " + decorateToStringValue(prettifier, Many(3, 2, 1, 0)) + " did not contain all elements of " + decorateToStringValue(prettifier, List(2, 3, 8)), thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (be (Many("howdy", "hi", "hello")) and contain allElementsOf Seq("HELLO", "HI"))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (be (Many("HI", "HELLO")) and contain allElementsOf Seq("HELLO", "HI"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, Many("howdy", "hi", "hello")) + " was not equal to " + decorateToStringValue(prettifier, Many("HI", "HELLO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (hiLists) should (be (Many("howdy", "hi", "hello")) and contain allElementsOf Seq("HO", "HELLO"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, Many("howdy", "hi", "hello")) + " was equal to " + decorateToStringValue(prettifier, Many("howdy", "hi", "hello")) + ", but " + decorateToStringValue(prettifier, Many("howdy", "hi", "hello")) + " did not contain all elements of " + decorateToStringValue(prettifier, List("HO", "HELLO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (all (hiLists) should (be (Many("howdy", "hi", "hello")) and contain allElementsOf Seq("HELLO", "HI"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (be (Many("HI", "HELLO")) and contain allElementsOf Seq("HELLO", "HI"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, Many("howdy", "hi", "hello")) + " was not equal to " + decorateToStringValue(prettifier, Many("HI", "HELLO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (be (Many("howdy", "hi", "hello")) and contain allElementsOf Seq("HO", "HELLO"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, Many("howdy", "hi", "hello")) + " was equal to " + decorateToStringValue(prettifier, Many("howdy", "hi", "hello")) + ", but " + decorateToStringValue(prettifier, Many("howdy", "hi", "hello")) + " did not contain all elements of " + decorateToStringValue(prettifier, List("HO", "HELLO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should allow RHS to contain duplicated value") {
        all (list1s) should (be (Many(3, 2, 1, 0)) and contain allElementsOf Seq(3, 2, 2, 1))
      }
    }

    describe("when used with (not contain allElementsOf xx and not contain allElementsOf xx)") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (not contain allElementsOf (Seq(3, 2, 8)) and not contain allElementsOf (Seq(8, 3, 4)))
        atLeast (2, lists) should (not contain allElementsOf (Seq(3, 8, 5)) and not contain allElementsOf (Seq(8, 3, 4)))
        atMost (2, lists) should (not contain allElementsOf (Seq(2, 4, 3)) and contain allElementsOf (Seq(4, 3, 2)))
        no (list1s) should (not contain allElementsOf (Seq(1, 2, 3)) and not contain allElementsOf (Seq(1, 3, 2)))

        val e1 = intercept[TestFailedException] {
          all (lists) should (not contain allElementsOf (Seq(2, 3, 4)) and not contain allElementsOf (Seq(8, 3, 4)))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(prettifier, Many(8, 4, 3, 2)) + " contained all elements of " + decorateToStringValue(prettifier, List(2, 3, 4)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (lists) should (not contain allElementsOf (Seq(3, 6, 8)) and not contain allElementsOf (Seq(2, 3, 4)))
        }
        checkMessageStackDepth(e2, allErrMsg(2, decorateToStringValue(prettifier, Many(8, 4, 3, 2)) + " did not contain all elements of " + decorateToStringValue(prettifier, List(3, 6, 8)) + ", but " + decorateToStringValue(prettifier, Many(8, 4, 3, 2)) + " contained all elements of " + decorateToStringValue(prettifier, List(2, 3, 4)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e3 = intercept[TestFailedException] {
          all (hiLists) should (not contain allElementsOf (Seq("hello", "hi")) and not contain allElementsOf (Seq("ho", "hey", "howdy")))
        }
        checkMessageStackDepth(e3, allErrMsg(0, decorateToStringValue(prettifier, Many("howdy", "hi", "hello")) + " contained all elements of " + decorateToStringValue(prettifier, List("hello", "hi")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e4 = intercept[TestFailedException] {
          all (hiLists) should (not contain allElementsOf (Seq("ho", "hey", "howdy")) and not contain allElementsOf (Seq("hello", "hi")))
        }
        checkMessageStackDepth(e4, allErrMsg(0, decorateToStringValue(prettifier, Many("howdy", "hi", "hello")) + " did not contain all elements of " + decorateToStringValue(prettifier, List("ho", "hey", "howdy")) + ", but " + decorateToStringValue(prettifier, Many("howdy", "hi", "hello")) + " contained all elements of " + decorateToStringValue(prettifier, List("hello", "hi")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (not contain allElementsOf (Seq("TO", "YOU")) and not contain allElementsOf (Seq("HO", "HE")))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not contain allElementsOf (Seq("HELLO", "HI")) and not contain allElementsOf (Seq("HO", "HE")))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, Many("howdy", "hi", "hello")) + " contained all elements of " + decorateToStringValue(prettifier, List("HELLO", "HI")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (hiLists) should (not contain allElementsOf (Seq("HO", "HE")) and not contain allElementsOf (Seq("HELLO", "HI")))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, Many("howdy", "hi", "hello")) + " did not contain all elements of " + decorateToStringValue(prettifier, List("HO", "HE")) + ", but " + decorateToStringValue(prettifier, Many("howdy", "hi", "hello")) + " contained all elements of " + decorateToStringValue(prettifier, List("HELLO", "HI")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (all (hiLists) should (not contain allElementsOf (Seq("TO", "YOU")) and not contain allElementsOf (Seq("HO", "HE")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not contain allElementsOf (Seq("HELLO", "HI")) and not contain allElementsOf (Seq("HO", "HE")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, Many("howdy", "hi", "hello")) + " contained all elements of " + decorateToStringValue(prettifier, List("HELLO", "HI")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (not contain allElementsOf (Seq("HO", "HE")) and not contain allElementsOf (Seq("HELLO", "HI")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, Many("howdy", "hi", "hello")) + " did not contain all elements of " + decorateToStringValue(prettifier, List("HO", "HE")) + ", but " + decorateToStringValue(prettifier, Many("howdy", "hi", "hello")) + " contained all elements of " + decorateToStringValue(prettifier, List("HELLO", "HI")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should allow RHS to contain duplicated value") {
        all (list1s) should (not contain allElementsOf (Seq(3, 2, 2, 1, 8)) and not contain allElementsOf (Seq(8, 3, 4)))
        all (list1s) should (not contain allElementsOf (Seq(8, 3, 4)) and not contain allElementsOf (Seq(3, 2, 2, 1, 8)))
      }
    }

    describe("when used with (not be (..) and not contain allElementsOf Seq(..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (not be (One(2)) and not contain allElementsOf (Seq(8, 3, 4)))
        atLeast (2, lists) should (not be (One(3)) and not contain allElementsOf (Seq(8, 3, 4)))
        atMost (2, lists) should (not be (Many(4, 3, 2)) and not contain allElementsOf (Seq(3, 4, 2)))
        no (list1s) should (not be (Many(3, 2, 1)) and not contain allElementsOf (Seq(1, 2, 3)))

        val e1 = intercept[TestFailedException] {
          all (lists) should (not be (Many(8, 4, 3, 2)) and not contain allElementsOf (Seq(8, 3, 4)))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(prettifier, Many(8, 4, 3, 2)) + " was equal to " + decorateToStringValue(prettifier, Many(8, 4, 3, 2)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (lists) should (not be (One(3)) and not contain allElementsOf (Seq(2, 3, 4)))
        }
        checkMessageStackDepth(e2, allErrMsg(2, decorateToStringValue(prettifier, Many(8, 4, 3, 2)) + " was not equal to " + decorateToStringValue(prettifier, One(3)) + ", but " + decorateToStringValue(prettifier, Many(8, 4, 3, 2)) + " contained all elements of " + decorateToStringValue(prettifier, List(2, 3, 4)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e3 = intercept[TestFailedException] {
          all (hiLists) should (not be (Many("howdy", "hi", "hello")) and not contain allElementsOf (Seq("ho", "hey", "howdy")))
        }
        checkMessageStackDepth(e3, allErrMsg(0, decorateToStringValue(prettifier, Many("howdy", "hi", "hello")) + " was equal to " + decorateToStringValue(prettifier, Many("howdy", "hi", "hello")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e4 = intercept[TestFailedException] {
          all (hiLists) should (not be (One("ho")) and not contain allElementsOf (Seq("hello", "hi")))
        }
        checkMessageStackDepth(e4, allErrMsg(0, decorateToStringValue(prettifier, Many("howdy", "hi", "hello")) + " was not equal to " + decorateToStringValue(prettifier, One("ho")) + ", but " + decorateToStringValue(prettifier, Many("howdy", "hi", "hello")) + " contained all elements of " + decorateToStringValue(prettifier, List("hello", "hi")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (not be (One("ho")) and not contain allElementsOf (Seq("HO", "HELLO")))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not be (Many("howdy", "hi", "hello")) and not contain allElementsOf (Seq("HELLO", "HI")))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, Many("howdy", "hi", "hello")) + " was equal to " + decorateToStringValue(prettifier, Many("howdy", "hi", "hello")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (hiLists) should (not be (One("ho")) and not contain allElementsOf (Seq("HI", "HELLO")))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, Many("howdy", "hi", "hello")) + " was not equal to " + decorateToStringValue(prettifier, One("ho")) + ", but " + decorateToStringValue(prettifier, Many("howdy", "hi", "hello")) + " contained all elements of " + decorateToStringValue(prettifier, List("HI", "HELLO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (all (hiLists) should (not be (One("ho")) and not contain allElementsOf (Seq("HO", "HELLO")))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not be (Many("howdy", "hi", "hello")) and not contain allElementsOf (Seq("HELLO", "HI")))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, Many("howdy", "hi", "hello")) + " was equal to " + decorateToStringValue(prettifier, Many("howdy", "hi", "hello")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (not be (One("ho")) and not contain allElementsOf (Seq("HI", "HELLO")))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, Many("howdy", "hi", "hello")) + " was not equal to " + decorateToStringValue(prettifier, One("ho")) + ", but " + decorateToStringValue(prettifier, Many("howdy", "hi", "hello")) + " contained all elements of " + decorateToStringValue(prettifier, List("HI", "HELLO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should allow RHS to contain duplicated value") {
        all (list1s) should (not be (One(2)) and not contain allElementsOf (Seq(3, 2, 2, 1, 8)))
      }
    }
  }
}
