/*
* Copyright 2001-2013 Artima, Inc.
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

import org.scalactic.{Equality, Every, One, Many}
import org.scalactic.StringNormalizations._
import SharedHelpers._
import FailureMessages.decorateToStringValue
import scala.collection.JavaConverters._
import Matchers._
import exceptions.TestFailedException

class EveryShouldContainNoElementsOfLogicalOrSpec extends FunSpec {

  val invertedStringEquality =
    new Equality[String] {
      def areEqual(a: String, b: Any): Boolean = a != b
    }

  val invertedListOfStringEquality =
    new Equality[Every[String]] {
      def areEqual(a: Every[String], b: Any): Boolean = a != b
    }

  val upperCaseStringEquality =
    new Equality[String] {
      def areEqual(a: String, b: Any): Boolean = a.toUpperCase == b
    }

  val upperCaseListOfStringEquality =
    new Equality[Every[String]] {
      def areEqual(a: Every[String], b: Any): Boolean = a.map(_.toUpperCase) == b
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

  //ADDITIONAL//

  val fileName: String = "EveryShouldContainNoElementsOfLogicalOrSpec.scala"

  describe("an Every") {

    val fumList: Every[String] = Every("fum")
    val toList: Every[String] = Every("to")

    describe("when used with (contain noElementsOf Seq(..) or contain noElementsOf Seq(..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (contain noElementsOf Seq("fee", "fie", "foe", "fam") or contain noElementsOf Seq("fie", "fee", "fam", "foe"))
        fumList should (contain noElementsOf Seq("fee", "fie", "foe", "fam") or contain noElementsOf Seq("fie", "fee", "fum", "foe"))
        fumList should (contain noElementsOf Seq("fee", "fie", "foe", "fum") or contain noElementsOf Seq("fie", "fee", "fam", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain noElementsOf Seq("fee", "fie", "foe", "fum") or contain noElementsOf Seq("fie", "fee", "fum", "foe"))
        }
        checkMessageStackDepth(e1, FailureMessages.containedAtLeastOneElementOf(fumList, Seq("fee", "fie", "foe", "fum")) + ", and " + FailureMessages.containedAtLeastOneElementOf(fumList, Seq("fie", "fee", "fum", "foe")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (contain noElementsOf Seq("fee", "fie", "foe", "fum") or contain noElementsOf Seq("fee", "fie", "fum", "foe"))
        fumList should (contain noElementsOf Seq("FEE", "FIE", "FOE", "FUM") or contain noElementsOf Seq("fee", "fie", "fum", "foe"))
        fumList should (contain noElementsOf Seq("fee", "fie", "foe", "fum") or contain noElementsOf Seq("FEE", "FIE", "FUM", "FOE"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain noElementsOf Seq("FEE", "FIE", "FOE", "FUM") or (contain noElementsOf Seq("FEE", "FIE", "FUM", "FOE")))
        }
        checkMessageStackDepth(e1, FailureMessages.containedAtLeastOneElementOf(fumList, Seq("FEE", "FIE", "FOE", "FUM")) + ", and " + FailureMessages.containedAtLeastOneElementOf(fumList, Seq("FEE", "FIE", "FUM", "FOE")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (contain noElementsOf Seq("fee", "fie", "foe", "fum") or contain noElementsOf Seq("fee", "fie", "fum", "foe"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (contain noElementsOf Seq("FEE", "FIE", "FOE", "FUM") or contain noElementsOf Seq("fee", "fie", "fum", "foe"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (contain noElementsOf Seq("fee", "fie", "foe", "fum") or contain noElementsOf Seq("FEE", "FIE", "FUM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain noElementsOf Seq("FEE", "FIE", "FOE", "FUM") or contain noElementsOf Seq("FEE", "FIE", "FUM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.containedAtLeastOneElementOf(fumList, Seq("FEE", "FIE", "FOE", "FUM")) + ", and " + FailureMessages.containedAtLeastOneElementOf(fumList, Seq("FEE", "FIE", "FUM", "FOE")), fileName, thisLineNumber - 2)
        (fumList should (contain noElementsOf Seq(" FEE ", " FIE ", " FOE ", " FAM ") or contain noElementsOf Seq(" FEE ", " FIE ", " FOE ", " FAM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }

      it("should allow RHS to contain duplicated value") {
        fumList should (contain noElementsOf Seq("fee", "fie", "foe", "fie", "fum") or contain noElementsOf Seq("fie", "fee", "fam", "foe"))
        fumList should (contain noElementsOf Seq("fie", "fee", "fam", "foe") or contain noElementsOf Seq("fee", "fie", "foe", "fie", "fum"))
      }
    }

    describe("when used with (equal (..) and contain noElementsOf Seq(..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (equal (fumList) or contain noElementsOf Seq("fie", "fee", "fam", "foe"))
        fumList should (equal (toList) or contain noElementsOf Seq("fie", "fee", "fam", "foe"))
        fumList should (equal (fumList) or contain noElementsOf Seq("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) or contain noElementsOf Seq("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, FailureMessages.didNotEqual(fumList, toList) + ", and " + FailureMessages.containedAtLeastOneElementOf(fumList, Seq("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (equal (fumList) or contain noElementsOf Seq("fie", "fee", "fum", "foe"))
        fumList should (equal (toList) or contain noElementsOf Seq("fie", "fee", "fum", "foe"))
        fumList should (equal (fumList) or contain noElementsOf Seq("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) or (contain noElementsOf Seq("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e1, FailureMessages.didNotEqual(fumList, toList) + ", and " + FailureMessages.containedAtLeastOneElementOf(fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (equal (toList) or contain noElementsOf Seq("fie", "fee", "fum", "foe"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (equal (fumList) or contain noElementsOf Seq("fie", "fee", "fum", "foe"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (equal (toList) or contain noElementsOf Seq("FEE", "FIE", "FOE", "FUM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (equal (fumList) or contain noElementsOf Seq("FEE", "FIE", "FOE", "FUM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.didNotEqual(fumList, fumList) + ", and " + FailureMessages.containedAtLeastOneElementOf(fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
        (fumList should (equal (toList) or contain noElementsOf Seq(" FEE ", " FIE ", " FOE ", " FAM "))) (decided by invertedListOfStringEquality, after being lowerCased and trimmed)
      }

      it("should allow RHS to contain duplicated value") {
        fumList should (equal (fumList) or contain noElementsOf Seq("fee", "fie", "foe", "fie", "fum"))
      }
    }

    describe("when used with (be (..) and contain noElementsOf Seq(..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (be (fumList) or contain noElementsOf Seq("fie", "fee", "fam", "foe"))
        fumList should (be (toList) or contain noElementsOf Seq("fie", "fee", "fam", "foe"))
        fumList should (be (fumList) or contain noElementsOf Seq("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (be (toList) or contain noElementsOf Seq("fie", "fee", "fum", "foe"))
        }
        checkMessageStackDepth(e1, FailureMessages.wasNotEqualTo(fumList, toList) + ", and " + FailureMessages.containedAtLeastOneElementOf(fumList, Seq("fie", "fee", "fum", "foe")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (be (fumList) or contain noElementsOf Seq("fee", "fie", "foe", "fum"))
        fumList should (be (toList) or contain noElementsOf Seq("fee", "fie", "foe", "fum"))
        fumList should (be (fumList) or contain noElementsOf Seq("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (be (toList) or (contain noElementsOf Seq("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e1, FailureMessages.wasNotEqualTo(fumList, toList) + ", and " + FailureMessages.containedAtLeastOneElementOf(fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (be (fumList) or contain noElementsOf Seq("fee", "fie", "foe", "fum"))) (decided by upperCaseStringEquality)
        (fumList should (be (toList) or contain noElementsOf Seq("fee", "fie", "foe", "fum"))) (decided by upperCaseStringEquality)
        (fumList should (be (fumList) or contain noElementsOf Seq("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (be (toList) or contain noElementsOf Seq("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.wasNotEqualTo(fumList, toList) + ", and " + FailureMessages.containedAtLeastOneElementOf(fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
        (fumList should (be (fumList) or contain noElementsOf Seq(" FEE ", " FIE ", " FOE ", " FAM "))) (after being lowerCased and trimmed)
      }

      it("should allow RHS to contain duplicated value") {
        fumList should (be (fumList) or contain noElementsOf Seq("fee", "fie", "foe", "fie", "fum"))
      }
    }

    describe("when used with (contain noElementsOf Seq(..) and be (..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (contain noElementsOf Seq("fie", "fee", "fam", "foe") or be (fumList))
        fumList should (contain noElementsOf Seq("fie", "fee", "fum", "foe") or be (fumList))
        fumList should (contain noElementsOf Seq("fie", "fee", "fam", "foe") or be (toList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain noElementsOf Seq("fie", "fee", "fum", "foe") or be (toList))
        }
        checkMessageStackDepth(e1, FailureMessages.containedAtLeastOneElementOf(fumList, Seq("fie", "fee", "fum", "foe")) + ", and " + FailureMessages.wasNotEqualTo(fumList, toList), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (contain noElementsOf Seq("fee", "fie", "foe", "fum") or be (fumList))
        fumList should (contain noElementsOf Seq("FEE", "FIE", "FOE", "FUM") or be (fumList))
        fumList should (contain noElementsOf Seq("fee", "fie", "foe", "fum") or be (toList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain noElementsOf Seq("FEE", "FIE", "FOE", "FUM") or be (toList))
        }
        checkMessageStackDepth(e1, FailureMessages.containedAtLeastOneElementOf(fumList, Seq("FEE", "FIE", "FOE", "FUM")) + ", and " + FailureMessages.wasNotEqualTo(fumList, toList), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (contain noElementsOf Seq("fee", "fie", "foe", "fum") or be (fumList))) (decided by upperCaseStringEquality)
        (fumList should (contain noElementsOf Seq("FEE", "FIE", "FOE", "FUM") or be (fumList))) (decided by upperCaseStringEquality)
        (fumList should (contain noElementsOf Seq("fee", "fie", "foe", "fum") or be (toList))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain noElementsOf Seq("FEE", "FIE", "FOE", "FUM") or be (toList))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.containedAtLeastOneElementOf(fumList, Seq("FEE", "FIE", "FOE", "FUM")) + ", and " + FailureMessages.wasNotEqualTo(fumList, toList), fileName, thisLineNumber - 2)
        (fumList should (contain noElementsOf Seq(" FEE ", " FIE ", " FOE ", " FUM ") or be (fumList))) (after being lowerCased and trimmed)
      }

      it("should allow RHS to contain duplicated value") {
        fumList should (contain noElementsOf Seq("fee", "fie", "foe", "fie", "fum") or be (fumList))
      }
    }

    describe("when used with (not contain noElementsOf Seq(..) and not contain noElementsOf Seq(..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (not contain noElementsOf (Seq("fee", "fie", "foe", "fum")) or not contain noElementsOf (Seq("fee", "fie", "fum", "foe")))
        fumList should (not contain noElementsOf (Seq("fee", "fie", "foe", "fam")) or not contain noElementsOf (Seq("fee", "fie", "fum", "foe")))
        fumList should (not contain noElementsOf (Seq("fee", "fie", "foe", "fum")) or not contain noElementsOf (Seq("fee", "fie", "fam", "foe")))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain noElementsOf (Seq("fee", "fie", "foe", "fam")) or not contain noElementsOf (Seq("fee", "fie", "fam", "foe")))
        }
        checkMessageStackDepth(e1, FailureMessages.didNotContainAtLeastOneElementOf(fumList, Seq("fee", "fie", "foe", "fam")) + ", and " + FailureMessages.didNotContainAtLeastOneElementOf(fumList, Seq("fee", "fie", "fam", "foe")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (not contain noElementsOf (Seq("FEE", "FIE", "FOE", "FUM")) or not contain noElementsOf (Seq("FEE", "FIE", "FUM", "FOE")))
        fumList should (not contain noElementsOf (Seq("fee", "fie", "foe", "fum")) or not contain noElementsOf (Seq("FEE", "FIE", "FUM", "FOE")))
        fumList should (not contain noElementsOf (Seq("FEE", "FIE", "FOE", "FUM")) or not contain noElementsOf (Seq("fee", "fie", "fum", "foe")))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain noElementsOf (Seq("fee", "fie", "foe", "fum")) or not contain noElementsOf (Seq("fee", "fie", "fum", "foe")))
        }
        checkMessageStackDepth(e1, FailureMessages.didNotContainAtLeastOneElementOf(fumList, Seq("fee", "fie", "foe", "fum")) + ", and " + FailureMessages.didNotContainAtLeastOneElementOf(fumList, Seq("fee", "fie", "fum", "foe")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (not contain noElementsOf (Seq("FEE", "FIE", "FOE", "FUM")) or not contain noElementsOf (Seq("FEE", "FIE", "FUM", "FOE")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (not contain noElementsOf (Seq("fee", "fie", "foe", "fum")) or not contain noElementsOf (Seq("FEE", "FIE", "FUM", "FOE")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (not contain noElementsOf (Seq("FEE", "FIE", "FOE", "FUM")) or not contain noElementsOf (Seq("fee", "fie", "fum", "foe")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not contain noElementsOf (Seq("fee", "fie", "foe", "fum")) or not contain noElementsOf (Seq("fee", "fie", "fum", "foe")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.didNotContainAtLeastOneElementOf(fumList, Seq("fee", "fie", "foe", "fum")) + ", and " + FailureMessages.didNotContainAtLeastOneElementOf(fumList, Seq("fee", "fie", "fum", "foe")), fileName, thisLineNumber - 2)
      }

      it("should allow RHS to contain duplicated value") {
        fumList should (not contain noElementsOf (Seq("fee", "fie", "foe", "fie", "fum")) or not contain noElementsOf (Seq("fee", "fie", "fum", "foe")))
        fumList should (not contain noElementsOf (Seq("fee", "fie", "fum", "foe")) or not contain noElementsOf (Seq("fee", "fie", "foe", "fie", "fum")))
      }
    }

    describe("when used with (not equal (..) and not contain noElementsOf Seq(..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (not equal (toList) or not contain noElementsOf (Seq("fee", "fie", "foe", "fum")))
        fumList should (not equal (fumList) or not contain noElementsOf (Seq("fee", "fie", "foe", "fum")))
        fumList should (not equal (toList) or not contain noElementsOf (Seq("fee", "fie", "foe", "fam")))
        val e1 = intercept[TestFailedException] {
          fumList should (not equal (fumList) or not contain noElementsOf (Seq("fee", "fie", "foe", "fam")))
        }
        checkMessageStackDepth(e1, FailureMessages.equaled(fumList, fumList) + ", and " + FailureMessages.didNotContainAtLeastOneElementOf(fumList, Seq("fee", "fie", "foe", "fam")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (not equal (toList) or not contain noElementsOf (Seq("FIE", "FEE", "FUM", "FOE")))
        fumList should (not equal (fumList) or not contain noElementsOf (Seq("FIE", "FEE", "FUM", "FOE")))
        fumList should (not equal (toList) or not contain noElementsOf (Seq("fie", "fee", "fum", "foe")))
        val e2 = intercept[TestFailedException] {
          fumList should (not equal (fumList) or (not contain noElementsOf (Seq("fie", "fee", "fum", "foe"))))
        }
        checkMessageStackDepth(e2, FailureMessages.equaled(fumList, fumList) + ", and " + FailureMessages.didNotContainAtLeastOneElementOf(fumList, Seq("fie", "fee", "fum", "foe")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (not equal (fumList) or not contain noElementsOf (Seq("FIE", "FEE", "FUM", "FOE")))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (not equal (toList) or not contain noElementsOf (Seq("FIE", "FEE", "FUM", "FOE")))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (not equal (fumList) or not contain noElementsOf (Seq("fie", "fee", "fum", "foe")))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not equal (toList) or not contain noElementsOf (Seq("fie", "fee", "fum", "foe")))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.equaled(fumList, toList) + ", and " + FailureMessages.didNotContainAtLeastOneElementOf(fumList, Seq("fie", "fee", "fum", "foe")), fileName, thisLineNumber - 2)
        (fumList should (not contain noElementsOf (Seq(" FEE ", " FIE ", " FOE ", " FUM ")) or not contain noElementsOf (Seq(" FEE ", " FIE ", " FOE ", " FUM ")))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }

      it("should allow RHS to contain duplicated value") {
        fumList should (not equal (toList) or not contain noElementsOf (Seq("fee", "fie", "foe", "fie", "fum")))
      }
    }

    describe("when used with (not be (..) and not contain noElementsOf Seq(..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (not be (toList) or not contain noElementsOf (Seq("fee", "fie", "foe", "fum")))
        fumList should (not be (fumList) or not contain noElementsOf (Seq("fee", "fie", "foe", "fum")))
        fumList should (not be (toList) or not contain noElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) or not contain noElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e1, FailureMessages.wasEqualTo(fumList, fumList) + ", and " + FailureMessages.didNotContainAtLeastOneElementOf(fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (not be (toList) or not contain noElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))
        fumList should (not be (fumList) or not contain noElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))
        fumList should (not be (toList) or not contain noElementsOf (Seq("fee", "fie", "foe", "fum")))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) or (not contain noElementsOf (Seq("fee", "fie", "foe", "fum"))))
        }
        checkMessageStackDepth(e1, FailureMessages.wasEqualTo(fumList, fumList) + ", and " + FailureMessages.didNotContainAtLeastOneElementOf(fumList, Seq("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (not be (toList) or not contain noElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))) (decided by upperCaseStringEquality)
        (fumList should (not be (fumList) or not contain noElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))) (decided by upperCaseStringEquality)
        (fumList should (not be (toList) or not contain noElementsOf (Seq("fee", "fie", "foe", "fum")))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not be (fumList) or not contain noElementsOf (Seq("fee", "fie", "foe", "fum")))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.wasEqualTo(fumList, fumList) + ", and " + FailureMessages.didNotContainAtLeastOneElementOf(fumList, Seq("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)
        (fumList should (not contain noElementsOf (Seq(" FEE ", " FIE ", " FOE ", " FUM ")) or not contain noElementsOf (Seq(" FEE ", " FIE ", " FOE ", " FUM ")))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }

      it("should allow RHS to contain duplicated value") {
        fumList should (not be (toList) or not contain noElementsOf (Seq("fee", "fie", "foe", "fie", "fum")))
      }
    }

  }

  describe("every of Everys") {

    val list1s: Every[Every[Int]] = Every(Every(1), Every(1), Every(1))
    val lists: Every[Every[Int]] = Every(Every(1), Every(1), Every(2))
    val hiLists: Every[Every[String]] = Every(Every("hi"), Every("hi"), Every("hi"))
    val toLists: Every[Every[String]] = Every(Every("to"), Every("to"), Every("to"))

    def allErrMsg(index: Int, message: String, lineNumber: Int, left: Any): String =
      "'all' inspection failed, because: \n" +
        "  at index " + index + ", " + message + " (" + fileName + ":" + (lineNumber) + ") \n" +
        "in " + decorateToStringValue(left)

    describe("when used with (contain noElementsOf Seq(..) and contain noElementsOf Seq(..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (contain noElementsOf Seq(3, 6, 9) or contain noElementsOf Seq(2, 6, 8))
        all (list1s) should (contain noElementsOf Seq(1, 2, 3) or contain noElementsOf Seq(2, 6, 8))
        all (list1s) should (contain noElementsOf Seq(3, 6, 9) or contain noElementsOf Seq(1, 2, 3))

        atLeast (2, lists) should (contain noElementsOf Seq(2, 6, 8) or contain noElementsOf Seq(3, 6, 9))
        atLeast (2, lists) should (contain noElementsOf Seq(1, 2, 3) or contain noElementsOf Seq(3, 6, 9))
        atLeast (2, lists) should (contain noElementsOf Seq(2, 6, 8) or contain noElementsOf Seq(1, 2, 3))

        val e1 = intercept[TestFailedException] {
          all (lists) should (contain noElementsOf Seq(2, 6, 8) or contain noElementsOf Seq(2, 3, 5))
        }
        checkMessageStackDepth(e1, allErrMsg(2, FailureMessages.containedAtLeastOneElementOf(lists(2), Seq(2, 6, 8)) + ", and " + FailureMessages.containedAtLeastOneElementOf(lists(2), Seq(2, 3, 5)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (contain noElementsOf Seq("hi", "he") or contain noElementsOf Seq("hi", "he"))
        all (hiLists) should (contain noElementsOf Seq("hi", "he") or contain noElementsOf Seq("HI", "HE"))
        all (hiLists) should (contain noElementsOf Seq("HI", "HE") or contain noElementsOf Seq("hi", "he"))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (contain noElementsOf Seq("HI", "HE") or contain noElementsOf Seq("HI", "HE"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, FailureMessages.containedAtLeastOneElementOf(hiLists(0), Seq("HI", "HE")) + ", and " + FailureMessages.containedAtLeastOneElementOf(hiLists(0), Seq("HI", "HE")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (all (hiLists) should (contain noElementsOf Seq("hi", "he") or contain noElementsOf Seq("hi", "he"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (contain noElementsOf Seq("hi", "he") or contain noElementsOf Seq("HI", "HE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (contain noElementsOf Seq("HI", "HE") or contain noElementsOf Seq("hi", "he"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)

        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (contain noElementsOf Seq("HI", "HE") or contain noElementsOf Seq("HI", "HE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, FailureMessages.containedAtLeastOneElementOf(hiLists(0), Seq("HI", "HE")) + ", and " + FailureMessages.containedAtLeastOneElementOf(hiLists(0), Seq("HI", "HE")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should allow RHS to contain duplicated value") {
        all (list1s) should (contain noElementsOf Seq(1, 2, 2, 3) or contain noElementsOf Seq(2, 6, 8))
        all (list1s) should (contain noElementsOf Seq(2, 6, 8) or contain noElementsOf Seq(1, 2, 2, 3))
      }
    }

    describe("when used with (be (..) and contain noElementsOf Seq(..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (be (One(1)) or contain noElementsOf Seq(2, 6, 8))
        all (list1s) should (be (One(2)) or contain noElementsOf Seq(2, 6, 8))
        all (list1s) should (be (One(1)) or contain noElementsOf Seq(1, 2, 3))

        val e1 = intercept[TestFailedException] {
          all (list1s) should (be (One(2)) or contain noElementsOf Seq(1, 2, 3))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(One(1)) + " was not equal to " + decorateToStringValue(One(2)) + ", and " + FailureMessages.containedAtLeastOneElementOf(list1s(0), Seq(1, 2, 3)), thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (be (One("hi")) or contain noElementsOf Seq("hi", "he"))
        all (hiLists) should (be (One("ho")) or contain noElementsOf Seq("hi", "he"))
        all (hiLists) should (be (One("hi")) or contain noElementsOf Seq("HI", "HE"))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (be (One("ho")) or contain noElementsOf Seq("HI", "HE"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(One("hi")) + " was not equal to " + decorateToStringValue(One("ho")) + ", and " + FailureMessages.containedAtLeastOneElementOf(hiLists(0), Seq("HI", "HE")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (all (hiLists) should (be (One("hi")) or contain noElementsOf Seq("hi", "he"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (be (One("ho")) or contain noElementsOf Seq("hi", "he"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (be (One("hi")) or contain noElementsOf Seq("HI", "HE"))) (decided by upperCaseStringEquality)

        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (be (One("ho")) or contain noElementsOf Seq("HI", "HE"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(One("hi")) + " was not equal to " + decorateToStringValue(One("ho")) + ", and " + FailureMessages.containedAtLeastOneElementOf(hiLists(0), Seq("HI", "HE")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should allow RHS to contain duplicated value") {
        all (list1s) should (be (One(1)) or contain noElementsOf Seq(1, 2, 2, 3))
      }
    }

    describe("when used with (not contain noElementsOf Seq(..) and not contain noElementsOf Seq(..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (not contain noElementsOf (Seq(1, 2, 3)) or not contain noElementsOf (Seq(1, 6, 8)))
        all (list1s) should (not contain noElementsOf (Seq(2, 6, 8)) or not contain noElementsOf (Seq(1, 6, 8)))
        all (list1s) should (not contain noElementsOf (Seq(1, 2, 3)) or not contain noElementsOf (Seq(2, 6, 8)))

        val e1 = intercept[TestFailedException] {
          all (lists) should (not contain noElementsOf (Seq(1, 6, 8)) or not contain noElementsOf (Seq(1, 3, 5)))
        }
        checkMessageStackDepth(e1, allErrMsg(2, FailureMessages.didNotContainAtLeastOneElementOf(lists(2), Seq(1, 6, 8)) + ", and " + FailureMessages.didNotContainAtLeastOneElementOf(lists(2), Seq(1, 3, 5)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (not contain noElementsOf (Seq("HI", "HE")) or not contain noElementsOf (Seq("HI", "HE")))
        all (hiLists) should (not contain noElementsOf (Seq("hi", "he")) or not contain noElementsOf (Seq("HI", "HE")))
        all (hiLists) should (not contain noElementsOf (Seq("HI", "HE")) or not contain noElementsOf (Seq("hi", "he")))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not contain noElementsOf (Seq("hi", "he")) or not contain noElementsOf (Seq("hi", "he")))
        }
        checkMessageStackDepth(e1, allErrMsg(0, FailureMessages.didNotContainAtLeastOneElementOf(hiLists(0), Seq("hi", "he")) + ", and " + FailureMessages.didNotContainAtLeastOneElementOf(hiLists(0), Seq("hi", "he")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (all (hiLists) should (not contain noElementsOf (Seq("HI", "HE")) or not contain noElementsOf (Seq("HI", "HE")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (not contain noElementsOf (Seq("hi", "he")) or not contain noElementsOf (Seq("HI", "HE")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (not contain noElementsOf (Seq("HI", "HE")) or not contain noElementsOf (Seq("hi", "he")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)

        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not contain noElementsOf (Seq("hi", "he")) or not contain noElementsOf (Seq("hi", "he")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, FailureMessages.didNotContainAtLeastOneElementOf(hiLists(0), Seq("hi", "he")) + ", and " + FailureMessages.didNotContainAtLeastOneElementOf(hiLists(0), Seq("hi", "he")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should allow RHS to contain duplicated value") {
        all (list1s) should (not contain noElementsOf (Seq(1, 2, 2, 3)) or not contain noElementsOf (Seq(1, 6, 8)))
        all (list1s) should (not contain noElementsOf (Seq(1, 6, 8)) or not contain noElementsOf (Seq(1, 2, 2, 3)))
      }
    }

    describe("when used with (not be (..) and not contain noElementsOf Seq(..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (not be (One(2)) or not contain noElementsOf (Seq(1, 6, 8)))
        all (list1s) should (not be (One(1)) or not contain noElementsOf (Seq(1, 6, 8)))
        all (list1s) should (not be (One(2)) or not contain noElementsOf (Seq(2, 6, 8)))

        val e1 = intercept[TestFailedException] {
          all (list1s) should (not be (One(1)) or not contain noElementsOf (Seq(2, 6, 8)))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(One(1)) + " was equal to " + decorateToStringValue(One(1)) + ", and " + FailureMessages.didNotContainAtLeastOneElementOf(list1s(0), Seq(2, 6, 8)), thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (not be (One("ho")) or not contain noElementsOf (Seq("HI", "HE")))
        all (hiLists) should (not be (One("hi")) or not contain noElementsOf (Seq("HI", "HE")))
        all (hiLists) should (not be (One("ho")) or not contain noElementsOf (Seq("hi", "he")))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not be (One("hi")) or not contain noElementsOf (Seq("hi", "he")))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(One("hi")) + " was equal to " + decorateToStringValue(One("hi")) + ", and " + FailureMessages.didNotContainAtLeastOneElementOf(hiLists(0), Seq("hi", "he")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (all (hiLists) should (not be (One("ho")) or not contain noElementsOf (Seq("HI", "HE")))) (decided by upperCaseStringEquality)
        (all (hiLists) should (not be (One("hi")) or not contain noElementsOf (Seq("HI", "HE")))) (decided by upperCaseStringEquality)
        (all (hiLists) should (not be (One("ho")) or not contain noElementsOf (Seq("hi", "he")))) (decided by upperCaseStringEquality)

        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not be (One("hi")) or not contain noElementsOf (Seq("hi", "he")))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(One("hi")) + " was equal to " + decorateToStringValue(One("hi")) + ", and " + FailureMessages.didNotContainAtLeastOneElementOf(hiLists(0), Seq("hi", "he")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should allow RHS to contain duplicated value") {
        all (list1s) should (not be (One(2)) or not contain noElementsOf (Seq(1, 2, 2, 3)))
      }
    }
  }
}
