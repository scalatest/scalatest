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
import exceptions.TestFailedException
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class EveryShouldContainInOrderOnlyLogicalAndSpec extends AnyFunSpec {

  private val prettifier = Prettifier.default

  //ADDITIONAL//

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

  val fileName: String = "EveryShouldContainInOrderOnlyLogicalAndSpec.scala"

  describe("an Every") {

    val fumList: Every[String] = Every("fum", "fum", "foe", "fie", "fie", "fee")
    val toList: Every[String] = Every("you", "you", "to", "birthday", "happy", "happy")

    describe("when used with (contain inOrderOnly xx and contain inOrderOnly xx)") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (contain inOrderOnly ("fum", "foe", "fie", "fee") and contain inOrderOnly ("fum", "foe", "fie", "fee"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain inOrderOnly ("fee", "fie", "foe", "fum") and contain inOrderOnly ("fum", "foe", "fie", "fee"))
        }
        checkMessageStackDepth(e1, Resources.didNotContainInOrderOnlyElements(decorateToStringValue(prettifier, fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (contain inOrderOnly ("fum", "foe", "fie", "fee") and contain inOrderOnly ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e2, Resources.containedInOrderOnlyElements(decorateToStringValue(prettifier, fumList), "\"fum\", \"foe\", \"fie\", \"fee\"") + ", but " + Resources.didNotContainInOrderOnlyElements(decorateToStringValue(prettifier, fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (contain inOrderOnly ("FUM", "FOE", "FIE", "FEE") and contain inOrderOnly ("FUM", "FOE", "FIE", "FEE"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain inOrderOnly ("FEE", "FIE", "FOE", "FUM") and contain inOrderOnly ("FUM", "FOE", "FIE", "FEE"))
        }
        checkMessageStackDepth(e1, Resources.didNotContainInOrderOnlyElements(decorateToStringValue(prettifier, fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (contain inOrderOnly ("FUM", "FOE", "FIE", "FEE") and (contain inOrderOnly ("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e2, Resources.containedInOrderOnlyElements(decorateToStringValue(prettifier, fumList), "\"FUM\", \"FOE\", \"FIE\", \"FEE\"") + ", but " + Resources.didNotContainInOrderOnlyElements(decorateToStringValue(prettifier, fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (contain inOrderOnly ("FUM", "FOE", "FIE", "FEE") and contain inOrderOnly ("FUM", "FOE", "FIE", "FEE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain inOrderOnly ("FEE", "FIE", "FOE", "FUM") and contain inOrderOnly ("FUM", "FOE", "FIE", "FEE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.didNotContainInOrderOnlyElements(decorateToStringValue(prettifier, fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (contain inOrderOnly ("FUM", "FOE", "FIE", "FEE") and contain inOrderOnly ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources.containedInOrderOnlyElements(decorateToStringValue(prettifier, fumList), "\"FUM\", \"FOE\", \"FIE\", \"FEE\"") + ", but " + Resources.didNotContainInOrderOnlyElements(decorateToStringValue(prettifier, fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
        (fumList should (contain inOrderOnly (" FUM ", " FOE ", " FIE ", " FEE ") and contain inOrderOnly (" FUM ", " FOE ", " FIE ", " FEE "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (contain inOrderOnly ("fee", "fie", "foe", "fie", "fum") and contain inOrderOnly ("fum", "foe", "fie", "fee"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.inOrderOnlyDuplicate))

        val e2 = intercept[exceptions.NotAllowedException] {
          fumList should (contain inOrderOnly ("fum", "foe", "fie", "fee") and contain inOrderOnly ("fee", "fie", "foe", "fie", "fum"))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources.inOrderOnlyDuplicate))
      }
    }

    describe("when used with (equal xx and contain inOrderOnly xx)") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (equal (fumList) and contain inOrderOnly ("fum", "foe", "fie", "fee"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) and contain inOrderOnly ("fum", "foe", "fie", "fee"))
        }
        checkMessageStackDepth(e1, Resources.didNotEqual(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (equal (fumList) and contain inOrderOnly ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e2, Resources.equaled(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)) + ", but " + Resources.didNotContainInOrderOnlyElements(decorateToStringValue(prettifier, fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (equal (fumList) and contain inOrderOnly ("FUM", "FOE", "FIE", "FEE"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) and contain inOrderOnly ("FUM", "FOE", "FIE", "FEE"))
        }
        checkMessageStackDepth(e1, Resources.didNotEqual(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (equal (fumList) and (contain inOrderOnly ("FEE", "FIE", "FOE", "FAM")))
        }
        checkMessageStackDepth(e2, Resources.equaled(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)) + ", but " + Resources.didNotContainInOrderOnlyElements(decorateToStringValue(prettifier, fumList), "\"FEE\", \"FIE\", \"FOE\", \"FAM\""), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (equal (toList) and contain inOrderOnly ("FUM", "FOE", "FIE", "FEE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (equal (toList) and contain inOrderOnly ("FEE", "FIE", "FOE", "FUM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.equaled(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)) + ", but " + Resources.didNotContainInOrderOnlyElements(decorateToStringValue(prettifier, fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (equal (fumList) and contain inOrderOnly ("FUM", "FOE", "FIE", "FEE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources.didNotEqual(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)), fileName, thisLineNumber - 2)
        (fumList should (equal (toList) and contain inOrderOnly (" FUM ", " FOE ", " FIE ", " FEE "))) (decided by invertedListOfStringEquality, after being lowerCased and trimmed)
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (equal (fumList) and contain inOrderOnly ("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.inOrderOnlyDuplicate))
      }
    }

    describe("when used with (be xx and contain inOrderOnly xx)") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (be (fumList) and contain inOrderOnly ("fum", "foe", "fie", "fee"))
        val e1 = intercept[TestFailedException] {
          fumList should (be (toList) and contain inOrderOnly ("fum", "foe", "fie", "fee"))
        }
        checkMessageStackDepth(e1, Resources.wasNotEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (be (fumList) and contain inOrderOnly ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e2, Resources.wasEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)) + ", but " + Resources.didNotContainInOrderOnlyElements(decorateToStringValue(prettifier, fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (be (fumList) and contain inOrderOnly ("FUM", "FOE", "FIE", "FEE"))
        val e1 = intercept[TestFailedException] {
          fumList should (be (toList) and contain inOrderOnly ("FUM", "FOE", "FIE", "FEE"))
        }
        checkMessageStackDepth(e1, Resources.wasNotEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (be (fumList) and (contain inOrderOnly ("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e2, Resources.wasEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)) + ", but " + Resources.didNotContainInOrderOnlyElements(decorateToStringValue(prettifier, fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (be (fumList) and contain inOrderOnly ("FUM", "FOE", "FIE", "FEE"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (be (fumList) and contain inOrderOnly ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.wasEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)) + ", but " + Resources.didNotContainInOrderOnlyElements(decorateToStringValue(prettifier, fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (be (toList) and contain inOrderOnly ("FUM", "FOE", "FIE", "FEE"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources.wasNotEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)), fileName, thisLineNumber - 2)
        (fumList should (be (fumList) and contain inOrderOnly (" FUM ", " FOE ", " FIE ", " FEE "))) (after being lowerCased and trimmed)
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (be (fumList) and contain inOrderOnly ("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.inOrderOnlyDuplicate))
      }
    }

    describe("when used with (contain inOrderOnly xx and be xx)") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (contain inOrderOnly ("fum", "foe", "fie", "fee") and be (fumList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain inOrderOnly ("fum", "foe", "fie", "fee") and be (toList))
        }
        checkMessageStackDepth(e1, Resources.containedInOrderOnlyElements(decorateToStringValue(prettifier, fumList), "\"fum\", \"foe\", \"fie\", \"fee\"") + ", but " + Resources.wasNotEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (contain inOrderOnly ("fee", "fie", "foe", "fum") and be (fumList))
        }
        checkMessageStackDepth(e2, Resources.didNotContainInOrderOnlyElements(decorateToStringValue(prettifier, fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (contain inOrderOnly ("FUM", "FOE", "FIE", "FEE") and be (fumList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain inOrderOnly ("FEE", "FIE", "FOE", "FUM") and be (toList))
        }
        checkMessageStackDepth(e1, Resources.didNotContainInOrderOnlyElements(decorateToStringValue(prettifier, fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (contain inOrderOnly ("HAPPY", "BIRTHDAY", "TO", "YOU") and (be (fumList)))
        }
        checkMessageStackDepth(e2, Resources.didNotContainInOrderOnlyElements(decorateToStringValue(prettifier, fumList), "\"HAPPY\", \"BIRTHDAY\", \"TO\", \"YOU\""), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (contain inOrderOnly ("FUM", "FOE", "FIE", "FEE") and be (fumList))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain inOrderOnly ("FEE", "FIE", "FOE", "FUM") and be (fumList))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.didNotContainInOrderOnlyElements(decorateToStringValue(prettifier, fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (contain inOrderOnly ("FUM", "FOE", "FIE", "FEE") and be (toList))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources.containedInOrderOnlyElements(decorateToStringValue(prettifier, fumList), "\"FUM\", \"FOE\", \"FIE\", \"FEE\"") + ", but " + Resources.wasNotEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)), fileName, thisLineNumber - 2)
        (fumList should (contain inOrderOnly (" FUM ", " FOE ", " FIE ", " FEE ") and be (fumList))) (after being lowerCased and trimmed)
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (contain inOrderOnly ("fee", "fie", "foe", "fie", "fum") and be (fumList))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.inOrderOnlyDuplicate))
      }
    }

    describe("when used with (not contain inOrderOnly xx and not contain inOrderOnly xx)") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (not contain inOrderOnly ("fee", "fie", "foe", "fum") and not contain inOrderOnly ("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain inOrderOnly ("fum", "foe", "fie", "fee") and not contain inOrderOnly ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, Resources.containedInOrderOnlyElements(decorateToStringValue(prettifier, fumList), "\"fum\", \"foe\", \"fie\", \"fee\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not contain inOrderOnly ("fee", "fie", "foe", "fum") and not contain inOrderOnly ("fum", "foe", "fie", "fee"))
        }
        checkMessageStackDepth(e2, Resources.didNotContainInOrderOnlyElements(decorateToStringValue(prettifier, fumList), "\"fee\", \"fie\", \"foe\", \"fum\"") + ", but " + Resources.containedInOrderOnlyElements(decorateToStringValue(prettifier, fumList), "\"fum\", \"foe\", \"fie\", \"fee\""), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (not contain inOrderOnly ("FEE", "FIE", "FOE", "FUM") and not contain inOrderOnly ("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain inOrderOnly ("FUM", "FOE", "FIE", "FEE") and not contain inOrderOnly ("FEE", "FIE", "FOE", "FUM"))
        }
        checkMessageStackDepth(e1, Resources.containedInOrderOnlyElements(decorateToStringValue(prettifier, fumList), "\"FUM\", \"FOE\", \"FIE\", \"FEE\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not contain inOrderOnly ("FEE", "FIE", "FOE", "FUM") and (not contain inOrderOnly ("FUM", "FOE", "FIE", "FEE")))
        }
        checkMessageStackDepth(e2, Resources.didNotContainInOrderOnlyElements(decorateToStringValue(prettifier, fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\"") + ", but " + Resources.containedInOrderOnlyElements(decorateToStringValue(prettifier, fumList), "\"FUM\", \"FOE\", \"FIE\", \"FEE\""), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (not contain inOrderOnly ("FEE", "FIE", "FOE", "FUM") and not contain inOrderOnly ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not contain inOrderOnly ("FEE", "FIE", "FOE", "FUM") and not contain inOrderOnly ("FUM", "FOE", "FIE", "FEE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.didNotContainInOrderOnlyElements(decorateToStringValue(prettifier, fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\"") + ", but " + Resources.containedInOrderOnlyElements(decorateToStringValue(prettifier, fumList), "\"FUM\", \"FOE\", \"FIE\", \"FEE\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (not contain inOrderOnly ("FUM", "FOE", "FIE", "FEE") and not contain inOrderOnly ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources.containedInOrderOnlyElements(decorateToStringValue(prettifier, fumList), "\"FUM\", \"FOE\", \"FIE\", \"FEE\""), fileName, thisLineNumber - 2)
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not contain inOrderOnly ("fee", "fie", "foe", "fie", "fum") and not contain inOrderOnly ("fee", "fie", "foe", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.inOrderOnlyDuplicate))

        val e2 = intercept[exceptions.NotAllowedException] {
          fumList should (not contain inOrderOnly ("fee", "fie", "foe", "fum") and not contain inOrderOnly ("fee", "fie", "foe", "fie", "fum"))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources.inOrderOnlyDuplicate))
      }
    }

    describe("when used with (not equal xx and not contain inOrderOnly xx)") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (not equal (toList) and not contain inOrderOnly ("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (not equal (fumList) and not contain inOrderOnly ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources.equaled(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not equal (toList) and not contain inOrderOnly ("fum", "foe", "fie", "fee"))
        }
        checkMessageStackDepth(e2, Resources.didNotEqual(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)) + ", but " + Resources.containedInOrderOnlyElements(decorateToStringValue(prettifier, fumList), "\"fum\", \"foe\", \"fie\", \"fee\""), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (not equal (toList) and not contain inOrderOnly ("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (not equal (fumList) and not contain inOrderOnly ("FEE", "FIE", "FOE", "FUM"))
        }
        checkMessageStackDepth(e1, Resources.equaled(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not equal (toList) and (not contain inOrderOnly ("FUM", "FOE", "FIE", "FEE")))
        }
        checkMessageStackDepth(e2, Resources.didNotEqual(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)) + ", but " + Resources.containedInOrderOnlyElements(decorateToStringValue(prettifier, fumList), "\"FUM\", \"FOE\", \"FIE\", \"FEE\""), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (not equal (fumList) and not contain inOrderOnly ("FEE", "FIE", "FOE", "FUM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not equal (fumList) and not contain inOrderOnly ("FUM", "FOE", "FIE", "FEE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.didNotEqual(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)) + ", but " + Resources.containedInOrderOnlyElements(decorateToStringValue(prettifier, fumList), "\"FUM\", \"FOE\", \"FIE\", \"FEE\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (not equal (toList) and not contain inOrderOnly ("FEE", "FIE", "FOE", "FUM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources.equaled(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)), fileName, thisLineNumber - 2)
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not equal (toList) and not contain inOrderOnly ("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.inOrderOnlyDuplicate))
      }
    }

    describe("when used with (not be xx and not contain inOrderOnly xx)") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (not be (toList) and not contain inOrderOnly ("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) and not contain inOrderOnly ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources.wasEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not be (toList) and not contain inOrderOnly ("fum", "foe", "fie", "fee"))
        }
        checkMessageStackDepth(e2, Resources.wasNotEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)) + ", but " + Resources.containedInOrderOnlyElements(decorateToStringValue(prettifier, fumList), "\"fum\", \"foe\", \"fie\", \"fee\""), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (not be (toList) and not contain inOrderOnly ("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) and not contain inOrderOnly ("FEE", "FIE", "FOE", "FUM"))
        }
        checkMessageStackDepth(e1, Resources.wasEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not be (toList) and (not contain inOrderOnly ("FUM", "FOE", "FIE", "FEE")))
        }
        checkMessageStackDepth(e2, Resources.wasNotEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)) + ", but " + Resources.containedInOrderOnlyElements(decorateToStringValue(prettifier, fumList), "\"FUM\", \"FOE\", \"FIE\", \"FEE\""), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (not be (toList) and not contain inOrderOnly ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not be (toList) and not contain inOrderOnly ("FUM", "FOE", "FIE", "FEE"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.wasNotEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)) + ", but " + Resources.containedInOrderOnlyElements(decorateToStringValue(prettifier, fumList), "\"FUM\", \"FOE\", \"FIE\", \"FEE\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (not be (fumList) and not contain inOrderOnly ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources.wasEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)), fileName, thisLineNumber - 2)
        (fumList should (not contain inOrderOnly (" FEE ", " FIE ", " FOE ", " FUU ") and not contain inOrderOnly (" FEE ", " FIE ", " FOE ", " FUU "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not be (toList) and not contain inOrderOnly ("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.inOrderOnlyDuplicate))
      }
    }
  }

  describe("every of Everys") {

    val list1s: Every[Every[Int]] = Every(Every(1, 2, 2, 3), Every(1, 2, 2, 3), Every(1, 2, 2, 3))
    val lists: Every[Every[Int]] = Every(Every(1, 2, 3, 3), Every(1, 2, 3, 3), Every(2, 3, 4))
    val hiLists: Every[Every[String]] = Every(Every("hi", "hello"), Every("hi", "hello"), Every("hi", "hello"))

    def allErrMsg(index: Int, message: String, lineNumber: Int, left: Any): String =
      "'all' inspection failed, because: \n" +
        "  at index " + index + ", " + message + " (" + fileName + ":" + (lineNumber) + ") \n" +
        "in " + decorateToStringValue(prettifier, left)

    describe("used with contain inOrderOnly xx and contain inOrderOnly xx") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (contain inOrderOnly (1, 2, 3) and contain inOrderOnly (1, 2, 3))
        atLeast (2, lists) should (contain inOrderOnly (1, 2, 3) and contain inOrderOnly (1, 2, 3))
        atMost (2, lists) should (contain inOrderOnly (1, 2, 3) and contain inOrderOnly (1, 2, 3))
        no (lists) should (contain inOrderOnly (3, 6, 9) and contain inOrderOnly (3, 4, 5))

        val e1 = intercept[TestFailedException] {
          all (lists) should (contain inOrderOnly (1, 2, 3) and contain inOrderOnly (1, 2, 3))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(prettifier, Many(2, 3, 4)) + " did not contain only " + "(1, 2, 3)" + " in order", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (list1s) should (contain inOrderOnly (1, 2, 3) and contain inOrderOnly (1, 3, 4))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, Many(1, 2, 2, 3)) + " contained only " + "(1, 2, 3)" + " in order" + ", but " + decorateToStringValue(prettifier, Many(1, 2, 2, 3)) + " did not contain only " + "(1, 3, 4)" + " in order", thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)

        val e3 = intercept[TestFailedException] {
          all (hiLists) should (contain inOrderOnly ("hi", "hello") and contain inOrderOnly ("hello", "hi"))
        }
        checkMessageStackDepth(e3, allErrMsg(0, decorateToStringValue(prettifier, Many("hi", "hello")) + " contained only " + "(\"hi\", \"hello\")" + " in order" + ", but " + decorateToStringValue(prettifier, Many("hi", "hello")) + " did not contain only " + "(\"hello\", \"hi\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (contain inOrderOnly ("HI", "HELLO") and contain inOrderOnly ("HI", "HELLO"))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (contain inOrderOnly ("HO", "HELLO") and contain inOrderOnly ("HI", "HELLO"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, Many("hi", "hello")) + " did not contain only " + "(\"HO\", \"HELLO\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (hiLists) should (contain inOrderOnly ("HI", "HELLO") and contain inOrderOnly ("HELLO", "HI"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, Many("hi", "hello")) + " contained only " + "(\"HI\", \"HELLO\")" + " in order" + ", but " + decorateToStringValue(prettifier, Many("hi", "hello")) + " did not contain only " + "(\"HELLO\", \"HI\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (all (hiLists) should (contain inOrderOnly ("HI", "HELLO") and contain inOrderOnly ("HI", "HELLO"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (contain inOrderOnly ("HO", "HELLO") and contain inOrderOnly ("HI", "HELLO"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, Many("hi", "hello")) + " did not contain only " + "(\"HO\", \"HELLO\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (contain inOrderOnly ("HI", "HELLO") and contain inOrderOnly ("HELLO", "HI"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, Many("hi", "hello")) + " contained only " + "(\"HI\", \"HELLO\")" + " in order" + ", but " + decorateToStringValue(prettifier, Many("hi", "hello")) + " did not contain only " + "(\"HELLO\", \"HI\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (contain inOrderOnly (1, 2, 2, 3) and contain inOrderOnly (1, 2, 3))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.inOrderOnlyDuplicate))

        val e2 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (contain inOrderOnly (1, 2, 3) and contain inOrderOnly (1, 2, 2, 3))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources.inOrderOnlyDuplicate))
      }
    }

    describe("when used with (be xx and contain inOrderOnly xx)") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (be (Many(1, 2, 2, 3)) and contain inOrderOnly (1, 2, 3))
        atLeast (2, lists) should (be (Many(1, 2, 3, 3)) and contain inOrderOnly (1, 2, 3))
        atMost (2, lists) should (be (Many(3, 2, 1)) and contain inOrderOnly (1, 2, 3))
        no (lists) should (be (Many(3, 6, 9)) and contain inOrderOnly (3, 4, 5))

        val e1 = intercept[TestFailedException] {
          all (lists) should (be (Many(1, 2, 3, 3)) and contain inOrderOnly (1, 2, 3))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(prettifier, Many(2, 3, 4)) + " was not equal to " + decorateToStringValue(prettifier, Many(1, 2, 3, 3)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (list1s) should (be (Many(1, 2, 2, 3)) and contain inOrderOnly (2, 3, 8))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, Many(1, 2, 2, 3)) + " was equal to " + decorateToStringValue(prettifier, Many(1, 2, 2, 3)) + ", but " + decorateToStringValue(prettifier, Many(1, 2, 2, 3)) + " did not contain only " + "(2, 3, 8)" + " in order", thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)

        val e3 = intercept[TestFailedException] {
          all (hiLists) should (be (Many("hi", "hello")) and contain inOrderOnly ("hello", "hi"))
        }
        checkMessageStackDepth(e3, allErrMsg(0, decorateToStringValue(prettifier, Many("hi", "hello")) + " was equal to " + decorateToStringValue(prettifier, Many("hi", "hello")) + ", but " + decorateToStringValue(prettifier, Many("hi", "hello")) + " did not contain only " + "(\"hello\", \"hi\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e4 = intercept[TestFailedException] {
          all (list1s) should (be (Many(1, 2, 2, 3)) and contain inOrderOnly (2, 3, 8))
        }
        checkMessageStackDepth(e4, allErrMsg(0, decorateToStringValue(prettifier, Many(1, 2, 2, 3)) + " was equal to " + decorateToStringValue(prettifier, Many(1, 2, 2, 3)) + ", but " + decorateToStringValue(prettifier, Many(1, 2, 2, 3)) + " did not contain only " + "(2, 3, 8)" + " in order", thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (be (Many("hi", "hello")) and contain inOrderOnly ("HI", "HELLO"))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (be (Many("HI", "HELLO")) and contain inOrderOnly ("HI", "HELLO"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, Many("hi", "hello")) + " was not equal to " + decorateToStringValue(prettifier, Many("HI", "HELLO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (hiLists) should (be (Many("hi", "hello")) and contain inOrderOnly ("HELLO", "HI"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, Many("hi", "hello")) + " was equal to " + decorateToStringValue(prettifier, Many("hi", "hello")) + ", but " + decorateToStringValue(prettifier, Many("hi", "hello")) + " did not contain only " + "(\"HELLO\", \"HI\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (all (hiLists) should (be (Many("hi", "hello")) and contain inOrderOnly ("HI", "HELLO"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (be (Many("HI", "HELLO")) and contain inOrderOnly ("HI", "HELLO"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, Many("hi", "hello")) + " was not equal to " + decorateToStringValue(prettifier, Many("HI", "HELLO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (be (Many("hi", "hello")) and contain inOrderOnly ("HELLO", "HI"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, Many("hi", "hello")) + " was equal to " + decorateToStringValue(prettifier, Many("hi", "hello")) + ", but " + decorateToStringValue(prettifier, Many("hi", "hello")) + " did not contain only " + "(\"HELLO\", \"HI\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (be (Many(1, 2, 2, 3)) and contain inOrderOnly (1, 2, 2, 3))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.inOrderOnlyDuplicate))
      }
    }

    describe("when used with (not contain inOrderOnly xx and not contain inOrderOnly xx)") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (not contain inOrderOnly (3, 2, 8) and not contain inOrderOnly (8, 3, 4))
        atLeast (2, lists) should (not contain inOrderOnly (3, 8, 5) and not contain inOrderOnly (8, 3, 4))
        atMost (2, lists) should (not contain inOrderOnly (2, 4, 3) and contain inOrderOnly (2, 3, 4))
        no (list1s) should (not contain inOrderOnly (1, 2, 3) and not contain inOrderOnly (1, 2, 3))

        val e1 = intercept[TestFailedException] {
          all (lists) should (not contain inOrderOnly (2, 3, 4) and not contain inOrderOnly (8, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(prettifier, Many(2, 3, 4)) + " contained only " + "(2, 3, 4)" + " in order", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (lists) should (not contain inOrderOnly (3, 6, 8) and not contain inOrderOnly (2, 3, 4))
        }
        checkMessageStackDepth(e2, allErrMsg(2, decorateToStringValue(prettifier, Many(2, 3, 4)) + " did not contain only " + "(3, 6, 8)" + " in order" + ", but " + decorateToStringValue(prettifier, Many(2, 3, 4)) + " contained only " + "(2, 3, 4)" + " in order", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e3 = intercept[TestFailedException] {
          all (hiLists) should (not contain inOrderOnly ("hi", "hello") and not contain inOrderOnly ("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e3, allErrMsg(0, decorateToStringValue(prettifier, Many("hi", "hello")) + " contained only " + "(\"hi\", \"hello\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e4 = intercept[TestFailedException] {
          all (hiLists) should (not contain inOrderOnly ("ho", "hey", "howdy") and not contain inOrderOnly ("hi", "hello"))
        }
        checkMessageStackDepth(e4, allErrMsg(0, decorateToStringValue(prettifier, Many("hi", "hello")) + " did not contain only " + "(\"ho\", \"hey\", \"howdy\")" + " in order" + ", but " + decorateToStringValue(prettifier, Many("hi", "hello")) + " contained only " + "(\"hi\", \"hello\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (not contain inOrderOnly ("HELLO", "HI") and not contain inOrderOnly ("HELLO", "HO"))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not contain inOrderOnly ("HI", "HELLO") and not contain inOrderOnly ("HO", "HE"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, Many("hi", "hello")) + " contained only " + "(\"HI\", \"HELLO\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (hiLists) should (not contain inOrderOnly ("HELLO", "HI") and not contain inOrderOnly ("HI", "HELLO"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, Many("hi", "hello")) + " did not contain only " + "(\"HELLO\", \"HI\")" + " in order" + ", but " + decorateToStringValue(prettifier, Many("hi", "hello")) + " contained only " + "(\"HI\", \"HELLO\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (all (hiLists) should (not contain inOrderOnly ("HELLO", "HI") and not contain inOrderOnly ("HELLO", "HO"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not contain inOrderOnly ("HI", "HELLO") and not contain inOrderOnly ("HO", "HE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, Many("hi", "hello")) + " contained only " + "(\"HI\", \"HELLO\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (not contain inOrderOnly ("HELLO", "HI") and not contain inOrderOnly ("HI", "HELLO"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, Many("hi", "hello")) + " did not contain only " + "(\"HELLO\", \"HI\")" + " in order" + ", but " + decorateToStringValue(prettifier, Many("hi", "hello")) + " contained only " + "(\"HI\", \"HELLO\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not contain inOrderOnly (1, 2, 2, 3) and not contain inOrderOnly (8, 3, 4))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.inOrderOnlyDuplicate))

        val e2 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not contain inOrderOnly (8, 3, 4) and not contain inOrderOnly (1, 2, 2, 3))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources.inOrderOnlyDuplicate))
      }
    }

    describe("when used with (not be xx and not contain inOrderOnly xx)") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (not be (One(2)) and not contain inOrderOnly (8, 3, 4))
        atLeast (2, lists) should (not be (One(3)) and not contain inOrderOnly (8, 3, 4))
        atMost (2, lists) should (not be (Many(2, 3, 4)) and not contain inOrderOnly (2, 3, 4))
        no (list1s) should (not be (Many(1, 2, 2, 3)) and not contain inOrderOnly (1, 2, 3))

        val e1 = intercept[TestFailedException] {
          all (lists) should (not be (Many(2, 3, 4)) and not contain inOrderOnly (8, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(prettifier, Many(2, 3, 4)) + " was equal to " + decorateToStringValue(prettifier, Many(2, 3, 4)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (lists) should (not be (One(3)) and not contain inOrderOnly (2, 3, 4))
        }
        checkMessageStackDepth(e2, allErrMsg(2, decorateToStringValue(prettifier, Many(2, 3, 4)) + " was not equal to " + decorateToStringValue(prettifier, One(3)) + ", but " + decorateToStringValue(prettifier, Many(2, 3, 4)) + " contained only " + "(2, 3, 4)" + " in order", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e3 = intercept[TestFailedException] {
          all (hiLists) should (not be (Many("hi", "hello")) and not contain inOrderOnly ("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e3, allErrMsg(0, decorateToStringValue(prettifier, Many("hi", "hello")) + " was equal to " + decorateToStringValue(prettifier, Many("hi", "hello")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e4 = intercept[TestFailedException] {
          all (hiLists) should (not be (One("ho")) and not contain inOrderOnly ("hi", "hello"))
        }
        checkMessageStackDepth(e4, allErrMsg(0, decorateToStringValue(prettifier, Many("hi", "hello")) + " was not equal to " + decorateToStringValue(prettifier, One("ho")) + ", but " + decorateToStringValue(prettifier, Many("hi", "hello")) + " contained only " + "(\"hi\", \"hello\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (not be (One("ho")) and not contain inOrderOnly ("HO", "HELLO"))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not be (Many("hi", "hello")) and not contain inOrderOnly ("HELLO", "HI"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, Many("hi", "hello")) + " was equal to " + decorateToStringValue(prettifier, Many("hi", "hello")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (hiLists) should (not be (One("ho")) and not contain inOrderOnly ("HI", "HELLO"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, Many("hi", "hello")) + " was not equal to " + decorateToStringValue(prettifier, One("ho")) + ", but " + decorateToStringValue(prettifier, Many("hi", "hello")) + " contained only " + "(\"HI\", \"HELLO\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (all (hiLists) should (not be (One("ho")) and not contain inOrderOnly ("HO", "HELLO"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not be (Many("hi", "hello")) and not contain inOrderOnly ("HELLO", "HI"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, Many("hi", "hello")) + " was equal to " + decorateToStringValue(prettifier, Many("hi", "hello")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (not be (One("ho")) and not contain inOrderOnly ("HI", "HELLO"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, Many("hi", "hello")) + " was not equal to " + decorateToStringValue(prettifier, One("ho")) + ", but " + decorateToStringValue(prettifier, Many("hi", "hello")) + " contained only " + "(\"HI\", \"HELLO\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not be (One(2)) and not contain inOrderOnly (1, 2, 2, 3))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.inOrderOnlyDuplicate))
      }
    }
  }
}
