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
import FailureMessages._
import exceptions.TestFailedException
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class EveryShouldContainNoElementsOfLogicalAndSpec extends AnyFunSpec {

  private val prettifier = Prettifier.default

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

  val fileName: String = "EveryShouldContainNoElementsOfLogicalAndSpec.scala"

  describe("an Every") {

    val fumList: Every[String] = Every("fum")
    val toList: Every[String] = Every("to")

    describe("when used with (contain noElementsOf Seq(..) and contain noElementsOf Seq(..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (contain noElementsOf Seq("fee", "fie", "foe", "fam") and contain noElementsOf Seq("fie", "fee", "fam", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain noElementsOf Seq("fee", "fie", "foe", "fum") and contain noElementsOf Seq("fie", "fee", "fam", "foe"))
        }
        checkMessageStackDepth(e1, FailureMessages.containedAtLeastOneElementOf(prettifier, fumList, Seq("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (contain noElementsOf Seq("fee", "fie", "foe", "fam") and contain noElementsOf Seq("fie", "fee", "fum", "foe"))
        }
        checkMessageStackDepth(e2, FailureMessages.didNotContainAtLeastOneElementOf(prettifier, fumList, Seq("fee", "fie", "foe", "fam")) + ", but " + FailureMessages.containedAtLeastOneElementOf(prettifier, fumList, Seq("fie", "fee", "fum", "foe")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (contain noElementsOf Seq("fee", "fie", "foe", "fum") and contain noElementsOf Seq("fee", "fie", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain noElementsOf Seq("FEE", "FIE", "FOE", "FUM") and contain noElementsOf Seq("fee", "fie", "fum", "foe"))
        }
        checkMessageStackDepth(e1, FailureMessages.containedAtLeastOneElementOf(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (contain noElementsOf Seq("fee", "fie", "foe", "fum") and (contain noElementsOf Seq("FEE", "FIE", "FUM", "FOE")))
        }
        checkMessageStackDepth(e2, FailureMessages.didNotContainAtLeastOneElementOf(prettifier, fumList, Seq("fee", "fie", "foe", "fum")) + ", but " + FailureMessages.containedAtLeastOneElementOf(prettifier, fumList, Seq("FEE", "FIE", "FUM", "FOE")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (contain noElementsOf Seq("fee", "fie", "foe", "fum") and contain noElementsOf Seq("fee", "fie", "fum", "foe"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain noElementsOf Seq("FEE", "FIE", "FOE", "FUM") and contain noElementsOf Seq("fee", "fie", "fum", "foe"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.containedAtLeastOneElementOf(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (contain noElementsOf Seq("fee", "fie", "foe", "fum") and contain noElementsOf Seq("FEE", "FIE", "FUM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, FailureMessages.didNotContainAtLeastOneElementOf(prettifier, fumList, Seq("fee", "fie", "foe", "fum")) + ", but " + FailureMessages.containedAtLeastOneElementOf(prettifier, fumList, Seq("FEE", "FIE", "FUM", "FOE")), fileName, thisLineNumber - 2)
        (fumList should (contain noElementsOf Seq(" FEE ", " FIE ", " FOE ", " FAM ") and contain noElementsOf Seq(" FEE ", " FIE ", " FOE ", " FAM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }

      it("should allow RHS to contain duplicated value") {
        fumList should (contain noElementsOf Seq("fee", "fie", "foe", "fie", "fam") and contain noElementsOf Seq("fie", "fee", "fam", "foe"))
        fumList should (contain noElementsOf Seq("fie", "fee", "fam", "foe") and contain noElementsOf Seq("fee", "fie", "foe", "fie", "fam"))
      }
    }

    describe("when used with (equal (..) and contain noElementsOf Seq(..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (equal (fumList) and contain noElementsOf Seq("fie", "fee", "fam", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) and contain noElementsOf Seq("fee", "fie", "foe", "fam"))
        }
        checkMessageStackDepth(e1, FailureMessages.didNotEqual(prettifier, fumList, toList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (equal (fumList) and contain noElementsOf Seq("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e2, FailureMessages.equaled(prettifier, fumList, fumList) + ", but " + FailureMessages.containedAtLeastOneElementOf(prettifier, fumList, Seq("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (equal (fumList) and contain noElementsOf Seq("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) and contain noElementsOf Seq("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, FailureMessages.didNotEqual(prettifier, fumList, toList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (equal (fumList) and (contain noElementsOf Seq("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e2, FailureMessages.equaled(prettifier, fumList, fumList) + ", but " + FailureMessages.containedAtLeastOneElementOf(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (equal (toList) and contain noElementsOf Seq("fee", "fie", "foe", "fum"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (equal (fumList) and contain noElementsOf Seq("fee", "fie", "foe", "fum"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.didNotEqual(prettifier, fumList, fumList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (equal (toList) and contain noElementsOf Seq("FEE", "FIE", "FOE", "FUM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, FailureMessages.equaled(prettifier, fumList, toList) + ", but " + FailureMessages.containedAtLeastOneElementOf(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
        (fumList should (equal (toList) and contain noElementsOf Seq(" FEE ", " FIE ", " FOE ", " FAM "))) (decided by invertedListOfStringEquality, after being lowerCased and trimmed)
      }

      it("should allow RHS to contain duplicated value") {
        fumList should (equal (fumList) and contain noElementsOf Seq("fee", "fie", "foe", "fie", "fam"))
      }
    }

    describe("when used with (be (..) and contain noElementsOf Seq(..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (be (fumList) and contain noElementsOf Seq("fie", "fee", "fam", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (be (toList) and contain noElementsOf Seq("fee", "fie", "foe", "fam"))
        }
        checkMessageStackDepth(e1, FailureMessages.wasNotEqualTo(prettifier, fumList, toList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (be (fumList) and contain noElementsOf Seq("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e2, FailureMessages.wasEqualTo(prettifier, fumList, fumList) + ", but " + FailureMessages.containedAtLeastOneElementOf(prettifier, fumList, Seq("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (be (fumList) and contain noElementsOf Seq("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (be (toList) and contain noElementsOf Seq("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, FailureMessages.wasNotEqualTo(prettifier, fumList, toList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (be (fumList) and (contain noElementsOf Seq("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e2, FailureMessages.wasEqualTo(prettifier, fumList, fumList) + ", but " + FailureMessages.containedAtLeastOneElementOf(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (be (fumList) and contain noElementsOf Seq("fee", "fie", "foe", "fum"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (be (fumList) and contain noElementsOf Seq("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.wasEqualTo(prettifier, fumList, fumList) + ", but " + FailureMessages.containedAtLeastOneElementOf(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (be (toList) and contain noElementsOf Seq("fee", "fie", "foe", "fum"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, FailureMessages.wasNotEqualTo(prettifier, fumList, toList), fileName, thisLineNumber - 2)
        (fumList should (be (fumList) and contain noElementsOf Seq(" FEE ", " FIE ", " FOE ", " FAM "))) (after being lowerCased and trimmed)
      }

      it("should allow RHS to contain duplicated value") {
        fumList should (be (fumList) and contain noElementsOf Seq("fee", "fie", "foe", "fie", "fam"))
      }
    }

    describe("when used with (contain noElementsOf Seq(..) and be (..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (contain noElementsOf Seq("fie", "fee", "fam", "foe") and be (fumList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain noElementsOf Seq("fee", "fie", "foe", "fam") and be (toList))
        }
        checkMessageStackDepth(e1, FailureMessages.didNotContainAtLeastOneElementOf(prettifier, fumList, Seq("fee", "fie", "foe", "fam")) + ", but " + FailureMessages.wasNotEqualTo(prettifier, fumList, toList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (contain noElementsOf Seq("fee", "fie", "foe", "fum") and be (fumList))
        }
        checkMessageStackDepth(e2, FailureMessages.containedAtLeastOneElementOf(prettifier, fumList, Seq("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (contain noElementsOf Seq("fee", "fie", "foe", "fum") and be (fumList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain noElementsOf Seq("FEE", "FIE", "FOE", "FUM") and be (toList))
        }
        checkMessageStackDepth(e1, FailureMessages.containedAtLeastOneElementOf(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (contain noElementsOf Seq("FEE", "FIE", "FOE", "FUM") and (be (fumList)))
        }
        checkMessageStackDepth(e2, FailureMessages.containedAtLeastOneElementOf(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (contain noElementsOf Seq("fee", "fie", "foe", "fum") and be (fumList))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain noElementsOf Seq("FEE", "FIE", "FOE", "FUM") and be (fumList))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.containedAtLeastOneElementOf(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (contain noElementsOf Seq("fee", "fie", "foe", "fum") and be (toList))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, FailureMessages.didNotContainAtLeastOneElementOf(prettifier, fumList, Seq("fee", "fie", "foe", "fum")) + ", but " + FailureMessages.wasNotEqualTo(prettifier, fumList, toList), fileName, thisLineNumber - 2)
        (fumList should (contain noElementsOf Seq(" FEE ", " FIE ", " FOE ", " FAM ") and be (fumList))) (after being lowerCased and trimmed)
      }

      it("should allow RHS to contain duplicated value") {
        fumList should (contain noElementsOf Seq("fee", "fie", "foe", "fie", "fam") and be (fumList))
      }
    }

    describe("when used with (not contain noElementsOf Seq(..) and not contain noElementsOf Seq(..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (not contain noElementsOf (Seq("fee", "fie", "foe", "fum")) and not contain noElementsOf (Seq("fee", "fie", "fum", "foe")))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain noElementsOf (Seq("FEE", "FIE", "FOE", "FUM")) and not contain noElementsOf (Seq("fee", "fie", "fum", "foe")))
        }
        checkMessageStackDepth(e1, FailureMessages.didNotContainAtLeastOneElementOf(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not contain noElementsOf (Seq("fee", "fie", "foe", "fum")) and not contain noElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e2, FailureMessages.containedAtLeastOneElementOf(prettifier, fumList, Seq("fee", "fie", "foe", "fum")) + ", but " + FailureMessages.didNotContainAtLeastOneElementOf(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (not contain noElementsOf (Seq("FEE", "FIE", "FOE", "FUM")) and not contain noElementsOf (Seq("FEE", "FIE", "FUM", "FOE")))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain noElementsOf (Seq("fee", "fie", "foe", "fum")) and not contain noElementsOf (Seq("FEE", "FIE", "FUM", "FOE")))
        }
        checkMessageStackDepth(e1, FailureMessages.didNotContainAtLeastOneElementOf(prettifier, fumList, Seq("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          fumList should (not contain noElementsOf (Seq("FEE", "FIE", "FOE", "FUM")) and (not contain noElementsOf (Seq("fee", "fie", "fum", "foe"))))
        }
        checkMessageStackDepth(e2, FailureMessages.containedAtLeastOneElementOf(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FUM")) + ", but " + FailureMessages.didNotContainAtLeastOneElementOf(prettifier, fumList, Seq("fee", "fie", "fum", "foe")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (not contain noElementsOf (Seq("FEE", "FIE", "FOE", "FUM")) and not contain noElementsOf (Seq("FEE", "FIE", "FUM", "FOE")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not contain noElementsOf (Seq("fee", "fie", "foe", "fum")) and not contain noElementsOf (Seq("FEE", "FIE", "FUM", "FOE")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.didNotContainAtLeastOneElementOf(prettifier, fumList, Seq("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (not contain noElementsOf (Seq("FEE", "FIE", "FOE", "FUM")) and not contain noElementsOf (Seq("fee", "fie", "fum", "foe")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, FailureMessages.containedAtLeastOneElementOf(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FUM")) + ", but " + FailureMessages.didNotContainAtLeastOneElementOf(prettifier, fumList, Seq("fee", "fie", "fum", "foe")), fileName, thisLineNumber - 2)
        (fumList should (contain noElementsOf Seq(" FEE ", " FIE ", " FOE ", " FAM ") and contain noElementsOf Seq(" FEE ", " FIE ", " FOE ", " FAM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }

      it("should allow RHS to contain duplicated value") {
        fumList should (not contain noElementsOf (Seq("fee", "fie", "foe", "fie", "fum")) and not contain noElementsOf (Seq("fee", "fie", "fum", "foe")))
        fumList should (not contain noElementsOf (Seq("fee", "fie", "fum", "foe")) and not contain noElementsOf (Seq("fee", "fie", "foe", "fie", "fum")))
      }
    }

    describe("when used with (not equal (..) and not contain noElementsOf Seq(..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (not equal (toList) and not contain noElementsOf (Seq("fee", "fie", "foe", "fum")))
        val e1 = intercept[TestFailedException] {
          fumList should (not equal (fumList) and not contain noElementsOf (Seq("fee", "fie", "foe", "fum")))
        }
        checkMessageStackDepth(e1, FailureMessages.equaled(prettifier, fumList, fumList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not equal (toList) and not contain noElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e2, FailureMessages.didNotEqual(prettifier, fumList, toList) + ", but " + FailureMessages.didNotContainAtLeastOneElementOf(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (not equal (toList) and not contain noElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))
        val e1 = intercept[TestFailedException] {
          fumList should (not equal (fumList) and not contain noElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e1, FailureMessages.equaled(prettifier, fumList, fumList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not equal (toList) and (not contain noElementsOf (Seq("fee", "fie", "foe", "fum"))))
        }
        checkMessageStackDepth(e2, FailureMessages.didNotEqual(prettifier, fumList, toList) + ", but " + FailureMessages.didNotContainAtLeastOneElementOf(prettifier, fumList, Seq("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (not equal (fumList) and not contain noElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not equal (fumList) and not contain noElementsOf (Seq("fee", "fie", "foe", "fum")))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.didNotEqual(prettifier, fumList, fumList) + ", but " + FailureMessages.didNotContainAtLeastOneElementOf(prettifier, fumList, Seq("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (not equal (toList) and not contain noElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, FailureMessages.equaled(prettifier, fumList, toList), fileName, thisLineNumber - 2)
        (fumList should (not contain noElementsOf (Seq(" FEE ", " FIE ", " FOE ", " FUM ")) and not contain noElementsOf (Seq(" FEE ", " FIE ", " FOE ", " FUM ")))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }

      it("should allow RHS to contain duplicated value") {
        fumList should (not equal (toList) and not contain noElementsOf (Seq("fee", "fie", "foe", "fie", "fum")))
      }
    }

    describe("when used with (not be (..) and not contain noElementsOf Seq(..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (not be (toList) and not contain noElementsOf (Seq("fee", "fie", "foe", "fum")))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) and not contain noElementsOf (Seq("fee", "fie", "foe", "fum")))
        }
        checkMessageStackDepth(e1, FailureMessages.wasEqualTo(prettifier, fumList, fumList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not be (toList) and not contain noElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e2, FailureMessages.wasNotEqualTo(prettifier, fumList, toList) + ", but " + FailureMessages.didNotContainAtLeastOneElementOf(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (not be (toList) and not contain noElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) and not contain noElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e1, FailureMessages.wasEqualTo(prettifier, fumList, fumList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not be (toList) and (not contain noElementsOf (Seq("fee", "fie", "foe", "fum"))))
        }
        checkMessageStackDepth(e2, FailureMessages.wasNotEqualTo(prettifier, fumList, toList) + ", but " + FailureMessages.didNotContainAtLeastOneElementOf(prettifier, fumList, Seq("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (not be (toList) and not contain noElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not be (toList) and not contain noElementsOf (Seq("fee", "fie", "foe", "fum")))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.wasNotEqualTo(prettifier, fumList, toList) + ", but " + FailureMessages.didNotContainAtLeastOneElementOf(prettifier, fumList, Seq("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (not be (fumList) and not contain noElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, FailureMessages.wasEqualTo(prettifier, fumList, fumList), fileName, thisLineNumber - 2)
        (fumList should (not contain noElementsOf (Seq(" FEE ", " FIE ", " FOE ", " FUM ")) and not contain noElementsOf (Seq(" FEE ", " FIE ", " FOE ", " FUM ")))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }

      it("should allow RHS to contain duplicated value") {
        fumList should (not be (toList) and not contain noElementsOf (Seq("fee", "fie", "foe", "fie", "fum")))
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
        "in " + decorateToStringValue(prettifier, left)

    describe("when used with (contain noElementsOf Seq(..) and contain noElementsOf Seq(..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (contain noElementsOf Seq(3, 2, 8) and contain noElementsOf Seq(2, 6, 8))
        atLeast (2, lists) should (contain noElementsOf Seq(3, 2, 5) and contain noElementsOf Seq(2, 3, 4))
        atMost (2, lists) should (contain noElementsOf Seq(3, 2, 8) and contain noElementsOf Seq(2, 3, 4))
        no (lists) should (contain noElementsOf Seq(1, 2, 8) and contain noElementsOf Seq(1, 2, 3))

        val e1 = intercept[TestFailedException] {
          all (lists) should (contain noElementsOf Seq(2, 6, 8) and contain noElementsOf Seq(3, 6, 9))
        }
        checkMessageStackDepth(e1, allErrMsg(2, FailureMessages.containedAtLeastOneElementOf(prettifier, lists(2), Seq(2, 6, 8)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (lists) should (contain noElementsOf Seq(3, 6, 9) and contain noElementsOf Seq(2, 6, 8))
        }
        checkMessageStackDepth(e2, allErrMsg(2, FailureMessages.didNotContainAtLeastOneElementOf(prettifier, lists(2), Seq(3, 6, 9)) + ", but " + FailureMessages.containedAtLeastOneElementOf(prettifier, lists(2), Seq(2, 6, 8)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e3 = intercept[TestFailedException] {
          all (hiLists) should (contain noElementsOf Seq("ho", "hello") and contain noElementsOf Seq("hi", "hey", "howdy"))
        }
        checkMessageStackDepth(e3, allErrMsg(0, FailureMessages.didNotContainAtLeastOneElementOf(prettifier, hiLists(0), Seq("ho", "hello")) + ", but " + FailureMessages.containedAtLeastOneElementOf(prettifier, hiLists(0), Seq("hi", "hey", "howdy")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e4 = intercept[TestFailedException] {
          all (lists) should (contain noElementsOf Seq(3, 6, 9) and contain noElementsOf Seq(2, 6, 8))
        }
        checkMessageStackDepth(e4, allErrMsg(2, FailureMessages.didNotContainAtLeastOneElementOf(prettifier, lists(2), Seq(3, 6, 9)) + ", but " + FailureMessages.containedAtLeastOneElementOf(prettifier, lists(2), Seq(2, 6, 8)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (contain noElementsOf Seq("hi", "he") and contain noElementsOf Seq("ho", "he"))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (contain noElementsOf Seq("HI", "HE") and contain noElementsOf Seq("ho", "he"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, FailureMessages.containedAtLeastOneElementOf(prettifier, hiLists(0), Seq("HI", "HE")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (hiLists) should (contain noElementsOf Seq("hi", "he") and contain noElementsOf Seq("HI", "HE"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, FailureMessages.didNotContainAtLeastOneElementOf(prettifier, hiLists(0), Seq("hi", "he")) + ", but " + FailureMessages.containedAtLeastOneElementOf(prettifier, hiLists(0), Seq("HI", "HE")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (all (hiLists) should (contain noElementsOf Seq("hi", "he") and contain noElementsOf Seq("ho", "he"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (contain noElementsOf Seq("HI", "HE") and contain noElementsOf Seq("ho", "he"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, FailureMessages.containedAtLeastOneElementOf(prettifier, hiLists(0), Seq("HI", "HE")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (contain noElementsOf Seq("hi", "he") and contain noElementsOf Seq("HI", "HE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, FailureMessages.didNotContainAtLeastOneElementOf(prettifier, hiLists(0), Seq("hi", "he")) + ", but " + FailureMessages.containedAtLeastOneElementOf(prettifier, hiLists(0), Seq("HI", "HE")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should allow RHS to contain duplicated value") {
        all (list1s) should (contain noElementsOf Seq(8, 2, 2, 3) and contain noElementsOf Seq(2, 6, 8))
        all (list1s) should (contain noElementsOf Seq(2, 6, 8) and contain noElementsOf Seq(8, 2, 2, 3))
      }
    }

    describe("when used with (be (..) and contain noElementsOf Seq(..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (be (One(1)) and contain noElementsOf Seq(2, 3, 4))
        atLeast (2, lists) should (be (One(1)) and contain noElementsOf Seq(2, 3, 4))
        atMost (2, lists) should (be (One(1)) and contain noElementsOf Seq(2, 3, 4))
        no (lists) should (be (One(8)) and contain noElementsOf Seq(1, 2, 3))

        val e1 = intercept[TestFailedException] {
          all (lists) should (be (One(1)) and contain noElementsOf Seq(3, 6, 9))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(prettifier, One(2)) + " was not equal to " + decorateToStringValue(prettifier, One(1)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (list1s) should (be (One(1)) and contain noElementsOf Seq(1, 2, 3))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, One(1)) + " was equal to " + decorateToStringValue(prettifier, One(1)) + ", but " + FailureMessages.containedAtLeastOneElementOf(prettifier, list1s(0), Seq(1, 2, 3)), thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)

        val e3 = intercept[TestFailedException] {
          all (hiLists) should (be (One("hi")) and contain noElementsOf Seq("hi", "hey", "howdy"))
        }
        checkMessageStackDepth(e3, allErrMsg(0, decorateToStringValue(prettifier, One("hi")) + " was equal to " + decorateToStringValue(prettifier, One("hi")) + ", but " + FailureMessages.containedAtLeastOneElementOf(prettifier, hiLists(0), Seq("hi", "hey", "howdy")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e4 = intercept[TestFailedException] {
          all (list1s) should (be (One(1)) and contain noElementsOf Seq(1, 2, 3))
        }
        checkMessageStackDepth(e4, allErrMsg(0, decorateToStringValue(prettifier, One(1)) + " was equal to " + decorateToStringValue(prettifier, One(1)) + ", but " + FailureMessages.containedAtLeastOneElementOf(prettifier, list1s(0), Seq(1, 2, 3)), thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (be (One("hi")) and contain noElementsOf Seq("hi", "he"))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (be (One("ho")) and contain noElementsOf Seq("hi", "he"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, One("hi")) + " was not equal to " + decorateToStringValue(prettifier, One("ho")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (hiLists) should (be (One("hi")) and contain noElementsOf Seq("HI", "HE"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, One("hi")) + " was equal to " + decorateToStringValue(prettifier, One("hi")) + ", but " + FailureMessages.containedAtLeastOneElementOf(prettifier, hiLists(0), Seq("HI", "HE")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (all (hiLists) should (be (One("hi")) and contain noElementsOf Seq("hi", "he"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (be (One("ho")) and contain noElementsOf Seq("hi", "he"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, One("hi")) + " was not equal to " + decorateToStringValue(prettifier, One("ho")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (be (One("hi")) and contain noElementsOf Seq("HI", "HE"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, One("hi")) + " was equal to " + decorateToStringValue(prettifier, One("hi")) + ", but " + FailureMessages.containedAtLeastOneElementOf(prettifier, hiLists(0), Seq("HI", "HE")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should allow RHS to contain duplicated value") {
        all (list1s) should (be (One(1)) and contain noElementsOf Seq(8, 2, 2, 3))
      }
    }

    describe("when used with (not contain noElementsOf Seq(..) and not contain noElementsOf Seq(..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (not contain noElementsOf (Seq(1, 2, 8)) and not contain noElementsOf (Seq(1, 6, 8)))
        atLeast (2, lists) should (not contain noElementsOf (Seq(1, 6, 8)) and not contain noElementsOf (Seq(1, 3, 8)))
        atMost (2, lists) should (not contain noElementsOf (Seq(1, 3, 8)) and contain noElementsOf (Seq(1, 6, 8)))
        no (lists) should (not contain noElementsOf (Seq(3, 6, 9)) and not contain noElementsOf (Seq(5, 7, 9)))

        val e1 = intercept[TestFailedException] {
          all (lists) should (not contain noElementsOf (Seq(1, 6, 8)) and not contain noElementsOf (Seq(1, 2, 3)))
        }
        checkMessageStackDepth(e1, allErrMsg(2, FailureMessages.didNotContainAtLeastOneElementOf(prettifier, lists(2), Seq(1, 6, 8)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (lists) should (not contain noElementsOf (Seq(1, 2, 3)) and not contain noElementsOf (Seq(1, 6, 8)))
        }
        checkMessageStackDepth(e2, allErrMsg(2, FailureMessages.containedAtLeastOneElementOf(prettifier, lists(2), Seq(1, 2, 3)) + ", but " + FailureMessages.didNotContainAtLeastOneElementOf(prettifier, lists(2), Seq(1, 6, 8)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e3 = intercept[TestFailedException] {
          all (hiLists) should (not contain noElementsOf (Seq("ho", "hello")) and not contain noElementsOf (Seq("hi", "hey", "howdy")))
        }
        checkMessageStackDepth(e3, allErrMsg(0, FailureMessages.didNotContainAtLeastOneElementOf(prettifier, hiLists(0), Seq("ho", "hello")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e4 = intercept[TestFailedException] {
          all (hiLists) should (not contain noElementsOf (Seq("hi", "hey", "howdy")) and not contain noElementsOf (Seq("ho", "hello")))
        }
        checkMessageStackDepth(e4, allErrMsg(0, FailureMessages.containedAtLeastOneElementOf(prettifier, hiLists(0), Seq("hi", "hey", "howdy")) + ", but " + FailureMessages.didNotContainAtLeastOneElementOf(prettifier, hiLists(0), Seq("ho", "hello")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (not contain noElementsOf (Seq("HI", "HE")) and not contain noElementsOf (Seq("HI", "HE")))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not contain noElementsOf (Seq("hi", "he")) and not contain noElementsOf (Seq("HI", "HE")))
        }
        checkMessageStackDepth(e1, allErrMsg(0, FailureMessages.didNotContainAtLeastOneElementOf(prettifier, hiLists(0), Seq("hi", "he")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (hiLists) should (not contain noElementsOf (Seq("HI", "HE")) and not contain noElementsOf (Seq("hi", "he")))
        }
        checkMessageStackDepth(e2, allErrMsg(0, FailureMessages.containedAtLeastOneElementOf(prettifier, hiLists(0), Seq("HI", "HE")) + ", but " + FailureMessages.didNotContainAtLeastOneElementOf(prettifier, hiLists(0), Seq("hi", "he")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (all (hiLists) should (not contain noElementsOf (Seq("HI", "HE")) and not contain noElementsOf (Seq("HI", "HE")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not contain noElementsOf (Seq("hi", "he")) and not contain noElementsOf (Seq("HI", "HE")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, FailureMessages.didNotContainAtLeastOneElementOf(prettifier, hiLists(0), Seq("hi", "he")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (not contain noElementsOf (Seq("HI", "HE")) and not contain noElementsOf (Seq("hi", "he")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, FailureMessages.containedAtLeastOneElementOf(prettifier, hiLists(0), Seq("HI", "HE")) + ", but " + FailureMessages.didNotContainAtLeastOneElementOf(prettifier, hiLists(0), Seq("hi", "he")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should allow RHS to contain duplicated value") {
        all (list1s) should (not contain noElementsOf (Seq(1, 2, 2, 3)) and not contain noElementsOf (Seq(1, 6, 8)))
        all (list1s) should (not contain noElementsOf (Seq(1, 6, 8)) and not contain noElementsOf (Seq(1, 2, 2, 3)))
      }
    }

    describe("when used with (not be (..) and not contain oneOf (..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (not be (One(2)) and not contain noElementsOf (Seq(1, 2, 3)))
        atLeast (2, lists) should (not be (One(3)) and not contain noElementsOf (Seq(1, 6, 8)))
        atMost (2, lists) should (not be (One(3)) and contain noElementsOf (Seq(1, 6, 8)))
        no (list1s) should (not be (One(1)) and not contain noElementsOf (Seq(3, 6, 9)))

        val e1 = intercept[TestFailedException] {
          all (lists) should (not be (One(2)) and not contain noElementsOf (Seq(1, 2, 3)))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(prettifier, One(2)) + " was equal to " + decorateToStringValue(prettifier, One(2)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (lists) should (not be (One(3)) and not contain noElementsOf (Seq(1, 6, 8)))
        }
        checkMessageStackDepth(e2, allErrMsg(2, decorateToStringValue(prettifier, One(2)) + " was not equal to " + decorateToStringValue(prettifier, One(3)) + ", but " + FailureMessages.didNotContainAtLeastOneElementOf(prettifier, lists(2), Seq(1, 6, 8)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e3 = intercept[TestFailedException] {
          all (hiLists) should (not be (One("hi")) and not contain noElementsOf (Seq("hi", "hey", "howdy")))
        }
        checkMessageStackDepth(e3, allErrMsg(0, decorateToStringValue(prettifier, One("hi")) + " was equal to " + decorateToStringValue(prettifier, One("hi")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e4 = intercept[TestFailedException] {
          all (hiLists) should (not be (One("ho")) and not contain noElementsOf (Seq("ho", "hello")))
        }
        checkMessageStackDepth(e4, allErrMsg(0, decorateToStringValue(prettifier, One("hi")) + " was not equal to " + decorateToStringValue(prettifier, One("ho")) + ", but " + FailureMessages.didNotContainAtLeastOneElementOf(prettifier, hiLists(0), Seq("ho", "hello")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (not be (One("ho")) and not contain noElementsOf (Seq("HI", "HE")))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not be (One("hi")) and not contain noElementsOf (Seq("HI", "HE")))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, One("hi")) + " was equal to " + decorateToStringValue(prettifier, One("hi")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (hiLists) should (not be (One("ho")) and not contain noElementsOf (Seq("hi", "he")))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, One("hi")) + " was not equal to " + decorateToStringValue(prettifier, One("ho")) + ", but " + FailureMessages.didNotContainAtLeastOneElementOf(prettifier, hiLists(0), Seq("hi", "he")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (all (hiLists) should (not be (One("ho")) and not contain noElementsOf (Seq("HI", "HE")))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not be (One("hi")) and not contain noElementsOf (Seq("HI", "HE")))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, One("hi")) + " was equal to " + decorateToStringValue(prettifier, One("hi")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (not be (One("ho")) and not contain noElementsOf (Seq("hi", "he")))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, One("hi")) + " was not equal to " + decorateToStringValue(prettifier, One("ho")) + ", but " + FailureMessages.didNotContainAtLeastOneElementOf(prettifier, hiLists(0), Seq("hi", "he")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should allow RHS to contain duplicated value") {
        all (list1s) should (not be (One(2)) and not contain noElementsOf (Seq(1, 2, 2, 3)))
      }
    }
  }
}
