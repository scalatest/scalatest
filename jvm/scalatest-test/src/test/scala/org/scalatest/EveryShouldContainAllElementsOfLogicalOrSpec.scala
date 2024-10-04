/*
* Copyright 2001-2024 Artima, Inc.
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

class EveryShouldContainAllElementsOfLogicalOrSpec extends AnyFunSpec {

  private val prettifier = Prettifier.default

  val invertedStringEquality =
    new Equality[String] {
      def areEqual(a: String, b: Any): Boolean = a != b
    }

  val invertedListOfStringEquality =
    new Equality[Every[String]] {
      def areEqual(a: Every[String], b: Any): Boolean = a != b
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

  val upperCaseStringEquality =
    new Equality[String] {
      def areEqual(a: String, b: Any): Boolean = upperCase(a) == upperCase(b)
    }

  //ADDITIONAL//

  val fileName: String = "EveryShouldContainAllElementsOfLogicalOrSpec.scala"

  describe("an Every") {

    val fumList: Every[String] = Every("fex", "fum", "foe", "fie", "fee")
    val toList: Every[String] = Every("too", "you", "to", "birthday", "happy")

    describe("when used with (contain allElementsOf Seq(..) or contain allElementsOf Seq(..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (contain allElementsOf Seq("fee", "fie", "foe", "fum") or contain allElementsOf Seq("fie", "fee", "fum", "foe"))
        fumList should (contain allElementsOf Seq("fee", "fie", "foe", "fam") or contain allElementsOf Seq("fie", "fee", "fum", "foe"))
        fumList should (contain allElementsOf Seq("fee", "fie", "foe", "fum") or contain allElementsOf Seq("fie", "fee", "fam", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain allElementsOf Seq("fee", "fie", "foe", "fam") or contain allElementsOf Seq("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, FailureMessages.didNotContainAllElementsOf(prettifier, fumList, Seq("fee", "fie", "foe", "fam")) + ", and " + FailureMessages.didNotContainAllElementsOf(prettifier, fumList, Seq("happy", "birthday", "to", "you")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (contain allElementsOf Seq("FEE", "FIE", "FOE", "FUM") or contain allElementsOf Seq("FIE", "FEE", "FUM", "FOE"))
        fumList should (contain allElementsOf Seq("FEE", "FIE", "FOE", "FAM") or contain allElementsOf Seq("FIE", "FEE", "FUM", "FOE"))
        fumList should (contain allElementsOf Seq("FEE", "FIE", "FOE", "FUM") or contain allElementsOf Seq("FIE", "FEE", "FAM", "FOE"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain allElementsOf Seq("FEE", "FIE", "FOE", "FAM") or (contain allElementsOf Seq("FIE", "FEE", "FAM", "FOE")))
        }
        checkMessageStackDepth(e1, FailureMessages.didNotContainAllElementsOf(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FAM")) + ", and " + FailureMessages.didNotContainAllElementsOf(prettifier, fumList, Seq("FIE", "FEE", "FAM", "FOE")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (contain allElementsOf Seq("FEE", "FIE", "FOE", "FUM") or contain allElementsOf Seq("FIE", "FEE", "FUM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (contain allElementsOf Seq("FEE", "FIE", "FOE", "FAM") or contain allElementsOf Seq("FIE", "FEE", "FUM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (contain allElementsOf Seq("FEE", "FIE", "FOE", "FUM") or contain allElementsOf Seq("FIE", "FEE", "FAM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain allElementsOf Seq("FEE", "FIE", "FOE", "FAM") or contain allElementsOf Seq("FIE", "FEE", "FAM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.didNotContainAllElementsOf(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FAM")) + ", and " + FailureMessages.didNotContainAllElementsOf(prettifier, fumList, Seq("FIE", "FEE", "FAM", "FOE")), fileName, thisLineNumber - 2)
        (fumList should (contain allElementsOf Seq(" FEE ", " FIE ", " FOE ", " FUM ") or contain allElementsOf Seq(" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }

      it("should allow RHS to contain duplicated value") {
        fumList should (contain allElementsOf Seq("fee", "fie", "foe", "fie", "fum") or contain allElementsOf Seq("fie", "fee", "fum", "foe"))
        fumList should (contain allElementsOf Seq("fie", "fee", "fum", "foe") or contain allElementsOf Seq("fee", "fie", "foe", "fie", "fum"))
      }
    }

    describe("when used with (equal (..) and contain allElementsOf Seq(..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (equal (fumList) or contain allElementsOf Seq("fie", "fee", "fum", "foe"))
        fumList should (equal (toList) or contain allElementsOf Seq("fie", "fee", "fum", "foe"))
        fumList should (equal (fumList) or contain allElementsOf Seq("fie", "fee", "fam", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) or contain allElementsOf Seq("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, FailureMessages.didNotEqual(prettifier, fumList, toList) + ", and " + FailureMessages.didNotContainAllElementsOf(prettifier, fumList, Seq("happy", "birthday", "to", "you")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (equal (fumList) or contain allElementsOf Seq("FIE", "FEE", "FUM", "FOE"))
        fumList should (equal (toList) or contain allElementsOf Seq("FIE", "FEE", "FUM", "FOE"))
        fumList should (equal (fumList) or contain allElementsOf Seq("FIE", "FEE", "FAM", "FOE"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) or (contain allElementsOf Seq("FIE", "FEE", "FAM", "FOE")))
        }
        checkMessageStackDepth(e1, FailureMessages.didNotEqual(prettifier, fumList, toList) + ", and " + FailureMessages.didNotContainAllElementsOf(prettifier, fumList, Seq("FIE", "FEE", "FAM", "FOE")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (equal (toList) or contain allElementsOf Seq("FIE", "FEE", "FUM", "FOE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (equal (fumList) or contain allElementsOf Seq("FIE", "FEE", "FUM", "FOE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (equal (toList) or contain allElementsOf Seq("FIE", "FEE", "FAM", "FOE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (equal (fumList) or contain allElementsOf Seq("FIE", "FEE", "FAM", "FOE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.didNotEqual(prettifier, fumList, fumList) + ", and " + FailureMessages.didNotContainAllElementsOf(prettifier, fumList, Seq("FIE", "FEE", "FAM", "FOE")), fileName, thisLineNumber - 2)
        (fumList should (equal (toList) or contain allElementsOf Seq(" FEE ", " FIE ", " FOE ", " FUM "))) (decided by invertedListOfStringEquality, after being lowerCased and trimmed)
      }

      it("should allow RHS to contain duplicated value") {
        fumList should (equal (fumList) or contain allElementsOf Seq("fee", "fie", "foe", "fie", "fum"))
      }
    }

    describe("when used with (be (..) and contain allElementsOf Seq(..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (be (fumList) or contain allElementsOf Seq("fie", "fee", "fum", "foe"))
        fumList should (be (toList) or contain allElementsOf Seq("fie", "fee", "fum", "foe"))
        fumList should (be (fumList) or contain allElementsOf Seq("fie", "fee", "fam", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (be (toList) or contain allElementsOf Seq("fie", "fee", "fam", "foe"))
        }
        checkMessageStackDepth(e1, FailureMessages.wasNotEqualTo(prettifier, fumList, toList) + ", and " + FailureMessages.didNotContainAllElementsOf(prettifier, fumList, Seq("fie", "fee", "fam", "foe")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (be (fumList) or contain allElementsOf Seq("FIE", "FEE", "FUM", "FOE"))
        fumList should (be (toList) or contain allElementsOf Seq("FIE", "FEE", "FUM", "FOE"))
        fumList should (be (fumList) or contain allElementsOf Seq("FIE", "FEE", "FAM", "FOE"))
        val e1 = intercept[TestFailedException] {
          fumList should (be (toList) or (contain allElementsOf Seq("FIE", "FEE", "FAM", "FOE")))
        }
        checkMessageStackDepth(e1, FailureMessages.wasNotEqualTo(prettifier, fumList, toList) + ", and " + FailureMessages.didNotContainAllElementsOf(prettifier, fumList, Seq("FIE", "FEE", "FAM", "FOE")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (be (fumList) or contain allElementsOf Seq("FIE", "FEE", "FUM", "FOE"))) (decided by upperCaseStringEquality)
        (fumList should (be (toList) or contain allElementsOf Seq("FIE", "FEE", "FUM", "FOE"))) (decided by upperCaseStringEquality)
        (fumList should (be (fumList) or contain allElementsOf Seq("FIE", "FEE", "FAM", "FOE"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (be (toList) or contain allElementsOf Seq("FIE", "FEE", "FAM", "FOE"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.wasNotEqualTo(prettifier, fumList, toList) + ", and " + FailureMessages.didNotContainAllElementsOf(prettifier, fumList, Seq("FIE", "FEE", "FAM", "FOE")), fileName, thisLineNumber - 2)
        (fumList should (be (fumList) or contain allElementsOf Seq(" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed)
      }

      it("should allow RHS to contain duplicated value") {
        fumList should (be (fumList) or contain allElementsOf Seq("fee", "fie", "foe", "fie", "fum"))
      }
    }

    describe("when used with (contain allElementsOf Seq(..) and be (..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (contain allElementsOf Seq("fie", "fee", "fum", "foe") or be (fumList))
        fumList should (contain allElementsOf Seq("fie", "fee", "fam", "foe") or be (fumList))
        fumList should (contain allElementsOf Seq("fie", "fee", "fum", "foe") or be (toList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain allElementsOf Seq("fee", "fie", "foe", "fam") or be (toList))
        }
        checkMessageStackDepth(e1, FailureMessages.didNotContainAllElementsOf(prettifier, fumList, Seq("fee", "fie", "foe", "fam")) + ", and " + FailureMessages.wasNotEqualTo(prettifier, fumList, toList), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (contain allElementsOf Seq("FIE", "FEE", "FUM", "FOE") or be (fumList))
        fumList should (contain allElementsOf Seq("FIE", "FEE", "FAM", "FOE") or be (fumList))
        fumList should (contain allElementsOf Seq("FIE", "FEE", "FUM", "FOE") or be (toList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain allElementsOf Seq("FEE", "FIE", "FOE", "FAM") or be (toList))
        }
        checkMessageStackDepth(e1, FailureMessages.didNotContainAllElementsOf(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FAM")) + ", and " + FailureMessages.wasNotEqualTo(prettifier, fumList, toList), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (contain allElementsOf Seq("FIE", "FEE", "FUM", "FOE") or be (fumList))) (decided by upperCaseStringEquality)
        (fumList should (contain allElementsOf Seq("FIE", "FEE", "FAM", "FOE") or be (fumList))) (decided by upperCaseStringEquality)
        (fumList should (contain allElementsOf Seq("FIE", "FEE", "FUM", "FOE") or be (toList))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain allElementsOf Seq("FEE", "FIE", "FOE", "FAM") or be (toList))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.didNotContainAllElementsOf(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FAM")) + ", and " + FailureMessages.wasNotEqualTo(prettifier, fumList, toList), fileName, thisLineNumber - 2)
        (fumList should (contain allElementsOf Seq(" FEE ", " FIE ", " FOE ", " FUM ") or be (fumList))) (after being lowerCased and trimmed)
      }

      it("should allow RHS to contain duplicated value") {
        fumList should (contain allElementsOf Seq("fee", "fie", "foe", "fie", "fum") or be (fumList))
      }
    }

    describe("when used with (not contain allElementsOf Seq(..) and not contain allElementsOf Seq(..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (not contain allElementsOf (Seq("fee", "fie", "foe", "fuu")) or not contain allElementsOf (Seq("fie", "fee", "fuu", "foe")))
        fumList should (not contain allElementsOf (Seq("fee", "fie", "foe", "fum")) or not contain allElementsOf (Seq("fie", "fee", "fuu", "foe")))
        fumList should (not contain allElementsOf (Seq("fee", "fie", "foe", "fuu")) or not contain allElementsOf (Seq("fie", "fee", "fum", "foe")))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain allElementsOf (Seq("fee", "fie", "foe", "fum")) or not contain allElementsOf (Seq("fie", "fee", "fum", "foe")))
        }
        checkMessageStackDepth(e1, FailureMessages.containedAllElementsOf(prettifier, fumList, Seq("fee", "fie", "foe", "fum")) + ", and " + FailureMessages.containedAllElementsOf(prettifier, fumList, Seq("fie", "fee", "fum", "foe")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (not contain allElementsOf (Seq("FEE", "FIE", "FOE", "FUU")) or not contain allElementsOf (Seq("FIE", "FEE", "FUU", "FOE")))
        fumList should (not contain allElementsOf (Seq("FEE", "FIE", "FOE", "FUM")) or not contain allElementsOf (Seq("FIE", "FEE", "FUU", "FOE")))
        fumList should (not contain allElementsOf (Seq("FEE", "FIE", "FOE", "FUU")) or not contain allElementsOf (Seq("FIE", "FEE", "FUM", "FOE")))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain allElementsOf (Seq("FEE", "FIE", "FOE", "FUM")) or not contain allElementsOf (Seq("FIE", "FEE", "FUM", "FOE")))
        }
        checkMessageStackDepth(e1, FailureMessages.containedAllElementsOf(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FUM")) + ", and " + FailureMessages.containedAllElementsOf(prettifier, fumList, Seq("FIE", "FEE", "FUM", "FOE")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (not contain allElementsOf (Seq("FEE", "FIE", "FOE", "FUU")) or not contain allElementsOf (Seq("FIE", "FEE", "FUU", "FOE")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (not contain allElementsOf (Seq("FEE", "FIE", "FOE", "FUM")) or not contain allElementsOf (Seq("FIE", "FEE", "FUU", "FOE")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (not contain allElementsOf (Seq("FEE", "FIE", "FOE", "FUU")) or not contain allElementsOf (Seq("FIE", "FEE", "FUM", "FOE")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not contain allElementsOf (Seq("FEE", "FIE", "FOE", "FUM")) or not contain allElementsOf (Seq("FIE", "FEE", "FUM", "FOE")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.containedAllElementsOf(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FUM")) + ", and " + FailureMessages.containedAllElementsOf(prettifier, fumList, Seq("FIE", "FEE", "FUM", "FOE")), fileName, thisLineNumber - 2)
        (fumList should (contain allElementsOf Seq(" FEE ", " FIE ", " FOE ", " FUM ") or contain allElementsOf Seq(" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }

      it("should allow RHS to contain duplicated value") {
        fumList should (not contain allElementsOf (Seq("fee", "fie", "foe", "fie", "fum")) or not contain allElementsOf (Seq("fie", "fee", "fuu", "foe")))
        fumList should (not contain allElementsOf (Seq("fie", "fee", "fuu", "foe")) or not contain allElementsOf (Seq("fee", "fie", "foe", "fie", "fum")))
      }
    }

    describe("when used with (not equal (..) and not contain allElementsOf Seq(..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (not equal (toList) or not contain allElementsOf (Seq("fie", "fee", "fuu", "foe")))
        fumList should (not equal (fumList) or not contain allElementsOf (Seq("fie", "fee", "fuu", "foe")))
        fumList should (not equal (toList) or not contain allElementsOf (Seq("fie", "fee", "fum", "foe")))
        val e1 = intercept[TestFailedException] {
          fumList should (not equal (fumList) or not contain allElementsOf (Seq("fee", "fie", "foe", "fum")))
        }
        checkMessageStackDepth(e1, FailureMessages.equaled(prettifier, fumList, fumList) + ", and " + FailureMessages.containedAllElementsOf(prettifier, fumList, Seq("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (not equal (toList) or not contain allElementsOf (Seq("FIE", "FEE", "FUU", "FOE")))
        fumList should (not equal (fumList) or not contain allElementsOf (Seq("FIE", "FEE", "FUU", "FOE")))
        fumList should (not equal (toList) or not contain allElementsOf (Seq("FIE", "FEE", "FUM", "FOE")))
        val e2 = intercept[TestFailedException] {
          fumList should (not equal (fumList) or (not contain allElementsOf (Seq("FEE", "FIE", "FOE", "FUM"))))
        }
        checkMessageStackDepth(e2, FailureMessages.equaled(prettifier, fumList, fumList) + ", and " + FailureMessages.containedAllElementsOf(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (not equal (fumList) or not contain allElementsOf (Seq("FIE", "FEE", "FUU", "FOE")))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (not equal (toList) or not contain allElementsOf (Seq("FIE", "FEE", "FUU", "FOE")))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (not equal (fumList) or not contain allElementsOf (Seq("FIE", "FEE", "FUM", "FOE")))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not equal (toList) or not contain allElementsOf (Seq("FIE", "FEE", "FUM", "FOE")))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.equaled(prettifier, fumList, toList) + ", and " + FailureMessages.containedAllElementsOf(prettifier, fumList, Seq("FIE", "FEE", "FUM", "FOE")), fileName, thisLineNumber - 2)
        (fumList should (not contain allElementsOf (Seq(" FEE ", " FIE ", " FOE ", " FUU ")) or not contain allElementsOf (Seq(" FEE ", " FIE ", " FOE ", " FUU ")))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }

      it("should allow RHS to contain duplicated value") {
        fumList should (not equal (toList) or not contain allElementsOf (Seq("fee", "fie", "foe", "fie", "fum")))
      }
    }

    describe("when used with (not be (..) and not contain allElementsOf Seq(..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (not be (toList) or not contain allElementsOf (Seq("fie", "fee", "fuu", "foe")))
        fumList should (not be (fumList) or not contain allElementsOf (Seq("fie", "fee", "fuu", "foe")))
        fumList should (not be (toList) or not contain allElementsOf (Seq("fee", "fie", "foe", "fum")))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) or not contain allElementsOf (Seq("fee", "fie", "foe", "fum")))
        }
        checkMessageStackDepth(e1, FailureMessages.wasEqualTo(prettifier, fumList, fumList) + ", and " + FailureMessages.containedAllElementsOf(prettifier, fumList, Seq("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (not be (toList) or not contain allElementsOf (Seq("FIE", "FEE", "FUU", "FOE")))
        fumList should (not be (fumList) or not contain allElementsOf (Seq("FIE", "FEE", "FUU", "FOE")))
        fumList should (not be (toList) or not contain allElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) or (not contain allElementsOf (Seq("FEE", "FIE", "FOE", "FUM"))))
        }
        checkMessageStackDepth(e1, FailureMessages.wasEqualTo(prettifier, fumList, fumList) + ", and " + FailureMessages.containedAllElementsOf(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (not be (toList) or not contain allElementsOf (Seq("FIE", "FEE", "FUU", "FOE")))) (decided by upperCaseStringEquality)
        (fumList should (not be (fumList) or not contain allElementsOf (Seq("FIE", "FEE", "FUU", "FOE")))) (decided by upperCaseStringEquality)
        (fumList should (not be (toList) or not contain allElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not be (fumList) or not contain allElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.wasEqualTo(prettifier, fumList, fumList) + ", and " + FailureMessages.containedAllElementsOf(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
        (fumList should (not contain allElementsOf (Seq(" FEE ", " FIE ", " FOE ", " FUU ")) or not contain allElementsOf (Seq(" FEE ", " FIE ", " FOE ", " FUU ")))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }

      it("should allow RHS to contain duplicated value") {
        fumList should (not be (toList) or not contain allElementsOf (Seq("fee", "fie", "foe", "fie", "fum")))
      }
    }

  }

  describe("every of Everys") {

    val list1s: Every[Every[Int]] = Every(Every(3, 2, 1, 0), Every(3, 2, 1, 0), Every(3, 2, 1, 0))
    val lists: Every[Every[Int]] = Every(Every(3, 2, 1, 0), Every(3, 2, 1, 0), Every(8, 4, 3, 2))
    val hiLists: Every[Every[String]] = Every(Every("howdy", "hi", "hello"), Every("howdy", "hi", "hello"), Every("howdy", "hi", "hello"))
    val toLists: Every[Every[String]] = Every(Every("nice", "you", "to"), Every("nice", "you", "to"), Every("nice", "you", "to"))

    def allErrMsg(index: Int, message: String, lineNumber: Int, left: Any): String =
      "'all' inspection failed, because: \n" +
        "  at index " + index + ", " + message + " (" + fileName + ":" + (lineNumber) + ") \n" +
        "in " + decorateToStringValue(prettifier, left)

    describe("when used with (contain allElementsOf Seq(..) and contain allElementsOf Seq(..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (contain allElementsOf Seq(3, 2, 1) or contain allElementsOf Seq(1, 3, 2))
        all (list1s) should (contain allElementsOf Seq(3, 2, 5) or contain allElementsOf Seq(1, 3, 2))
        all (list1s) should (contain allElementsOf Seq(3, 2, 1) or contain allElementsOf Seq(2, 3, 4))

        atLeast (2, lists) should (contain allElementsOf Seq(3, 1, 2) or contain allElementsOf Seq(1, 2, 3))
        atLeast (2, lists) should (contain allElementsOf Seq(3, 6, 5) or contain allElementsOf Seq(1, 3, 2))
        atLeast (2, lists) should (contain allElementsOf Seq(3, 1, 2) or contain allElementsOf Seq(8, 3, 4))

        val e1 = intercept[TestFailedException] {
          all (lists) should (contain allElementsOf Seq(3, 1, 2) or contain allElementsOf Seq(1, 3, 2))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(prettifier, Many(8, 4, 3, 2)) + " did not contain all elements of " + decorateToStringValue(prettifier, List(3, 1, 2)) + ", and " + decorateToStringValue(prettifier, Many(8, 4, 3, 2)) + " did not contain all elements of " + decorateToStringValue(prettifier, List(1, 3, 2)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (contain allElementsOf Seq("HELLO", "HI") or contain allElementsOf Seq("hello", "hi"))
        all (hiLists) should (contain allElementsOf Seq("HELLO", "HO") or contain allElementsOf Seq("hello", "hi"))
        all (hiLists) should (contain allElementsOf Seq("HELLO", "HI") or contain allElementsOf Seq("hello", "ho"))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (contain allElementsOf Seq("HELLO", "HO") or contain allElementsOf Seq("hello", "ho"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, Many("howdy", "hi", "hello")) + " did not contain all elements of " + decorateToStringValue(prettifier, List("HELLO", "HO")) + ", and " + decorateToStringValue(prettifier, Many("howdy", "hi", "hello")) + " did not contain all elements of " + decorateToStringValue(prettifier, List("hello", "ho")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (all (hiLists) should (contain allElementsOf Seq("HELLO", "HI") or contain allElementsOf Seq("hello", "hi"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (contain allElementsOf Seq("HELLO", "HO") or contain allElementsOf Seq("hello", "hi"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (contain allElementsOf Seq("HELLO", "HI") or contain allElementsOf Seq("hello", "ho"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)

        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (contain allElementsOf Seq("HELLO", "HO") or contain allElementsOf Seq("hello", "ho"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, Many("howdy", "hi", "hello")) + " did not contain all elements of " + decorateToStringValue(prettifier, List("HELLO", "HO")) + ", and " + decorateToStringValue(prettifier, Many("howdy", "hi", "hello")) + " did not contain all elements of " + decorateToStringValue(prettifier, List("hello", "ho")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should allow RHS to contain duplicated value") {
        all (list1s) should (contain allElementsOf Seq(3, 2, 2, 1) or contain allElementsOf Seq(1, 3, 2))
        all (list1s) should (contain allElementsOf Seq(1, 3, 2) or contain allElementsOf Seq(3, 2, 2, 1))
      }
    }

    describe("when used with (be (..) and contain allElementsOf Seq(..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (be (Many(3, 2, 1, 0)) or contain allElementsOf Seq(1, 2, 3))
        all (list1s) should (be (Many(2, 3, 4)) or contain allElementsOf Seq(1, 2, 3))
        all (list1s) should (be (Many(3, 2, 1, 0)) or contain allElementsOf Seq(2, 3, 4))

        val e1 = intercept[TestFailedException] {
          all (list1s) should (be (Many(2, 3, 4)) or contain allElementsOf Seq(2, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, Many(3, 2, 1, 0)) + " was not equal to " + decorateToStringValue(prettifier, Many(2, 3, 4)) + ", and " + decorateToStringValue(prettifier, Many(3, 2, 1, 0)) + " did not contain all elements of " + decorateToStringValue(prettifier, List(2, 3, 4)), thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (be (Many("howdy", "hi", "hello")) or contain allElementsOf Seq("HELLO", "HI"))
        all (hiLists) should (be (Many("ho", "hello")) or contain allElementsOf Seq("HELLO", "HI"))
        all (hiLists) should (be (Many("howdy", "hi", "hello")) or contain allElementsOf Seq("HELLO", "HO"))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (be (Many("ho", "hello")) or contain allElementsOf Seq("HELLO", "HO"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, Many("howdy", "hi", "hello")) + " was not equal to " + decorateToStringValue(prettifier, Many("ho", "hello")) + ", and " + decorateToStringValue(prettifier, Many("howdy", "hi", "hello")) + " did not contain all elements of " + decorateToStringValue(prettifier, List("HELLO", "HO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (all (hiLists) should (be (Many("howdy", "hi", "hello")) or contain allElementsOf Seq("HELLO", "HI"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (be (Many("ho", "hello")) or contain allElementsOf Seq("HELLO", "HI"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (be (Many("howdy", "hi", "hello")) or contain allElementsOf Seq("HELLO", "HO"))) (decided by upperCaseStringEquality)

        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (be (Many("ho", "hello")) or contain allElementsOf Seq("HELLO", "HO"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, Many("howdy", "hi", "hello")) + " was not equal to " + decorateToStringValue(prettifier, Many("ho", "hello")) + ", and " + decorateToStringValue(prettifier, Many("howdy", "hi", "hello")) + " did not contain all elements of " + decorateToStringValue(prettifier, List("HELLO", "HO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should allow RHS contain duplicated value") {
        all (list1s) should (be (Many(3, 2, 1, 0)) or contain allElementsOf Seq(1, 2, 2, 3))
      }
    }

    describe("when used with (not contain allElementsOf xx and not contain allElementsOf xx)") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (not contain allElementsOf (Seq(3, 2, 8)) or not contain allElementsOf (Seq(8, 3, 4)))
        all (list1s) should (not contain allElementsOf (Seq(1, 2, 3)) or not contain allElementsOf (Seq(8, 3, 4)))
        all (list1s) should (not contain allElementsOf (Seq(3, 2, 8)) or not contain allElementsOf (Seq(2, 3, 1)))

        val e1 = intercept[TestFailedException] {
          all (lists) should (not contain allElementsOf (Seq(4, 2, 3)) or not contain allElementsOf (Seq(2, 3, 4)))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(prettifier, Many(8, 4, 3, 2)) + " contained all elements of " + decorateToStringValue(prettifier, List(4, 2, 3)) + ", and " + decorateToStringValue(prettifier, Many(8, 4, 3, 2)) + " contained all elements of " + decorateToStringValue(prettifier, List(2, 3, 4)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (not contain allElementsOf (Seq("HELLO", "HO")) or not contain allElementsOf (Seq("hello", "ho")))
        all (hiLists) should (not contain allElementsOf (Seq("HELLO", "HI")) or not contain allElementsOf (Seq("hello", "ho")))
        all (hiLists) should (not contain allElementsOf (Seq("HELLO", "HO")) or not contain allElementsOf (Seq("hello", "hi")))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not contain allElementsOf (Seq("HELLO", "HI")) or not contain allElementsOf (Seq("hello", "hi")))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, Many("howdy", "hi", "hello")) + " contained all elements of " + decorateToStringValue(prettifier, List("HELLO", "HI")) + ", and " + decorateToStringValue(prettifier, Many("howdy", "hi", "hello")) + " contained all elements of " + decorateToStringValue(prettifier, List("hello", "hi")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (all (hiLists) should (not contain allElementsOf (Seq("HELLO", "HO")) or not contain allElementsOf (Seq("hello", "ho")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (not contain allElementsOf (Seq("HELLO", "HI")) or not contain allElementsOf (Seq("hello", "ho")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (not contain allElementsOf (Seq("HELLO", "HO")) or not contain allElementsOf (Seq("hello", "hi")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)

        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not contain allElementsOf (Seq("HELLO", "HI")) or not contain allElementsOf (Seq("hello", "hi")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, Many("howdy", "hi", "hello")) + " contained all elements of " + decorateToStringValue(prettifier, List("HELLO", "HI")) + ", and " + decorateToStringValue(prettifier, Many("howdy", "hi", "hello")) + " contained all elements of " + decorateToStringValue(prettifier, List("hello", "hi")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should allow RHS to contain duplicated value") {
        all (list1s) should (not contain allElementsOf (Seq(1, 2, 2, 3)) or not contain allElementsOf (Seq(8, 3, 4)))
        all (list1s) should (not contain allElementsOf (Seq(8, 3, 4)) or not contain allElementsOf (Seq(1, 2, 2, 3)))
      }
    }

    describe("when used with (not be (...) and not contain allElementsOf Seq(...))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (not be (One(2)) or not contain allElementsOf (Seq(8, 3, 4)))
        all (list1s) should (not be (Many(3, 2, 1, 0)) or not contain allElementsOf (Seq(8, 3, 4)))
        all (list1s) should (not be (One(2)) or not contain allElementsOf (Seq(1, 2, 3)))

        val e1 = intercept[TestFailedException] {
          all (list1s) should (not be (Many(3, 2, 1, 0)) or not contain allElementsOf (Seq(2, 3, 1)))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, Many(3, 2, 1, 0)) + " was equal to " + decorateToStringValue(prettifier, Many(3, 2, 1, 0)) + ", and " + decorateToStringValue(prettifier, Many(3, 2, 1, 0)) + " contained all elements of " + decorateToStringValue(prettifier, List(2, 3, 1)), thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (not be (Many("hello", "ho")) or not contain allElementsOf (Seq("HELLO", "HO")))
        all (hiLists) should (not be (Many("howdy", "hello", "hi")) or not contain allElementsOf (Seq("HELLO", "HO")))
        all (hiLists) should (not be (Many("hello", "ho")) or not contain allElementsOf (Seq("HELLO", "HI")))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not be (Many("howdy", "hi", "hello")) or not contain allElementsOf (Seq("HELLO", "HI")))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, Many("howdy", "hi", "hello")) + " was equal to " + decorateToStringValue(prettifier, Many("howdy", "hi", "hello")) + ", and " + decorateToStringValue(prettifier, Many("howdy", "hi", "hello")) + " contained all elements of " + decorateToStringValue(prettifier, List("HELLO", "HI")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (all (hiLists) should (not be (Many("hello", "ho")) or not contain allElementsOf (Seq("HELLO", "HO")))) (decided by upperCaseStringEquality)
        (all (hiLists) should (not be (Many("howdy", "hello", "hi")) or not contain allElementsOf (Seq("HELLO", "HO")))) (decided by upperCaseStringEquality)
        (all (hiLists) should (not be (Many("hello", "ho")) or not contain allElementsOf (Seq("HELLO", "HI")))) (decided by upperCaseStringEquality)

        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not be (Many("howdy", "hi", "hello")) or not contain allElementsOf (Seq("HELLO", "HI")))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, Many("howdy", "hi", "hello")) + " was equal to " + decorateToStringValue(prettifier, Many("howdy", "hi", "hello")) + ", and " + decorateToStringValue(prettifier, Many("howdy", "hi", "hello")) + " contained all elements of " + decorateToStringValue(prettifier, List("HELLO", "HI")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should allow RHS to contain duplicated value") {
        all (list1s) should (not be (One(2)) or not contain allElementsOf (Seq(1, 2, 2, 3)))
      }
    }
  }
}
