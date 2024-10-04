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
import exceptions.TestFailedException
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class EveryShouldContainInOrderElementsOfLogicalAndSpec extends AnyFunSpec {

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

  val fileName: String = "EveryShouldContainInOrderElementsOfLogicalAndSpec.scala"

  describe("an Every") {

    val fumList: Every[String] = Every("fex", "fum", "fum", "foe", "fie", "fie", "fee")
    val toList: Every[String] = Every("too", "you", "you", "to", "birthday", "happy", "happy")

    describe("when used with (contain inOrderElementsOf xx and contain inOrderElementsOf xx)") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (contain inOrderElementsOf Seq("fum", "foe", "fie", "fee") and contain inOrderElementsOf Seq("fum", "foe", "fie", "fee"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain inOrderElementsOf Seq("fee", "fie", "foe", "fum") and contain inOrderElementsOf Seq("fum", "foe", "fie", "fee"))
        }
        checkMessageStackDepth(e1, FailureMessages.didNotContainAllElementsOfInOrder(prettifier, fumList, Seq("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (contain inOrderElementsOf Seq("fum", "foe", "fie", "fee") and contain inOrderElementsOf Seq("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e2, FailureMessages.containedAllElementsOfInOrder(prettifier, fumList, Seq("fum", "foe", "fie", "fee")) + ", but " + FailureMessages.didNotContainAllElementsOfInOrder(prettifier, fumList, Seq("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE") and contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain inOrderElementsOf Seq("FEE", "FIE", "FOE", "FUM") and contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE"))
        }
        checkMessageStackDepth(e1, FailureMessages.didNotContainAllElementsOfInOrder(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE") and (contain inOrderElementsOf Seq("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e2, FailureMessages.containedAllElementsOfInOrder(prettifier, fumList, Seq("FUM", "FOE", "FIE", "FEE")) + ", but " + FailureMessages.didNotContainAllElementsOfInOrder(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE") and contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain inOrderElementsOf Seq("FEE", "FIE", "FOE", "FUM") and contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.didNotContainAllElementsOfInOrder(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE") and contain inOrderElementsOf Seq("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, FailureMessages.containedAllElementsOfInOrder(prettifier, fumList, Seq("FUM", "FOE", "FIE", "FEE")) + ", but " + FailureMessages.didNotContainAllElementsOfInOrder(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
        (fumList should (contain inOrderElementsOf Seq(" FUM ", " FOE ", " FIE ", " FEE ") and contain inOrderElementsOf Seq(" FUM ", " FOE ", " FIE ", " FEE "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }

      it("should do nothing when RHS contain duplicated value") {
        fumList should (contain inOrderElementsOf Seq("fum", "fum", "foe", "fie", "fee") and contain inOrderElementsOf Seq("fum", "foe", "fie", "fee"))
        fumList should (contain inOrderElementsOf Seq("fum", "foe", "fie", "fee") and contain inOrderElementsOf Seq("fum", "fum", "foe", "fie", "fee"))
      }
    }

    describe("when used with (equal xx and contain inOrderElementsOf xx)") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (equal (fumList) and contain inOrderElementsOf Seq("fum", "foe", "fie", "fee"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) and contain inOrderElementsOf Seq("fum", "foe", "fie", "fee"))
        }
        checkMessageStackDepth(e1, FailureMessages.didNotEqual(prettifier, fumList, toList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (equal (fumList) and contain inOrderElementsOf Seq("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e2, FailureMessages.equaled(prettifier, fumList, fumList) + ", but " + FailureMessages.didNotContainAllElementsOfInOrder(prettifier, fumList, Seq("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (equal (fumList) and contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) and contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE"))
        }
        checkMessageStackDepth(e1, FailureMessages.didNotEqual(prettifier, fumList, toList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (equal (fumList) and (contain inOrderElementsOf Seq("FEE", "FIE", "FOE", "FAM")))
        }
        checkMessageStackDepth(e2, FailureMessages.equaled(prettifier, fumList, fumList) + ", but " + FailureMessages.didNotContainAllElementsOfInOrder(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FAM")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (equal (toList) and contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (equal (toList) and contain inOrderElementsOf Seq("FEE", "FIE", "FOE", "FUM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.equaled(prettifier, fumList, toList) + ", but " + FailureMessages.didNotContainAllElementsOfInOrder(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (equal (fumList) and contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, FailureMessages.didNotEqual(prettifier, fumList, fumList), fileName, thisLineNumber - 2)
        (fumList should (equal (toList) and contain inOrderElementsOf Seq(" FUM ", " FOE ", " FIE ", " FEE "))) (decided by invertedListOfStringEquality, after being lowerCased and trimmed)
      }

      it("should do nothing when RHS contain duplicated value") {
        fumList should (equal (fumList) and contain inOrderElementsOf Seq("fum", "fum", "foe", "fie", "fee"))
      }
    }

    describe("when used with (be xx and contain inOrderElementsOf xx)") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (be (fumList) and contain inOrderElementsOf Seq("fum", "foe", "fie", "fee"))
        val e1 = intercept[TestFailedException] {
          fumList should (be (toList) and contain inOrderElementsOf Seq("fum", "foe", "fie", "fee"))
        }
        checkMessageStackDepth(e1, FailureMessages.wasNotEqualTo(prettifier, fumList, toList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (be (fumList) and contain inOrderElementsOf Seq("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e2, FailureMessages.wasEqualTo(prettifier, fumList, fumList) + ", but " + FailureMessages.didNotContainAllElementsOfInOrder(prettifier, fumList, Seq("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (be (fumList) and contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE"))
        val e1 = intercept[TestFailedException] {
          fumList should (be (toList) and contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE"))
        }
        checkMessageStackDepth(e1, FailureMessages.wasNotEqualTo(prettifier, fumList, toList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (be (fumList) and (contain inOrderElementsOf Seq("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e2, FailureMessages.wasEqualTo(prettifier, fumList, fumList) + ", but " + FailureMessages.didNotContainAllElementsOfInOrder(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (be (fumList) and contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (be (fumList) and contain inOrderElementsOf Seq("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.wasEqualTo(prettifier, fumList, fumList) + ", but " + FailureMessages.didNotContainAllElementsOfInOrder(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (be (toList) and contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, FailureMessages.wasNotEqualTo(prettifier, fumList, toList), fileName, thisLineNumber - 2)
        (fumList should (be (fumList) and contain inOrderElementsOf Seq(" FUM ", " FOE ", " FIE ", " FEE "))) (after being lowerCased and trimmed)
      }

      it("should do nothing when RHS contain duplicated value") {
        fumList should (be (fumList) and contain inOrderElementsOf Seq("fum", "fum", "foe", "fie", "fee"))
      }
    }

    describe("when used with (contain inOrderElementsOf xx and be xx)") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (contain inOrderElementsOf Seq("fum", "foe", "fie", "fee") and be (fumList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain inOrderElementsOf Seq("fum", "foe", "fie", "fee") and be (toList))
        }
        checkMessageStackDepth(e1, FailureMessages.containedAllElementsOfInOrder(prettifier, fumList, Seq("fum", "foe", "fie", "fee")) + ", but " + FailureMessages.wasNotEqualTo(prettifier, fumList, toList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (contain inOrderElementsOf Seq("fee", "fie", "foe", "fum") and be (fumList))
        }
        checkMessageStackDepth(e2, FailureMessages.didNotContainAllElementsOfInOrder(prettifier, fumList, Seq("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE") and be (fumList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain inOrderElementsOf Seq("FEE", "FIE", "FOE", "FUM") and be (toList))
        }
        checkMessageStackDepth(e1, FailureMessages.didNotContainAllElementsOfInOrder(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (contain inOrderElementsOf Seq("HAPPY", "BIRTHDAY", "TO", "YOU") and (be (fumList)))
        }
        checkMessageStackDepth(e2, FailureMessages.didNotContainAllElementsOfInOrder(prettifier, fumList, Seq("HAPPY", "BIRTHDAY", "TO", "YOU")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE") and be (fumList))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain inOrderElementsOf Seq("FEE", "FIE", "FOE", "FUM") and be (fumList))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.didNotContainAllElementsOfInOrder(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE") and be (toList))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, FailureMessages.containedAllElementsOfInOrder(prettifier, fumList, Seq("FUM", "FOE", "FIE", "FEE")) + ", but " + FailureMessages.wasNotEqualTo(prettifier, fumList, toList), fileName, thisLineNumber - 2)
        (fumList should (contain inOrderElementsOf Seq(" FUM ", " FOE ", " FIE ", " FEE ") and be (fumList))) (after being lowerCased and trimmed)
      }

      it("should do nothing when RHS contain duplicated value") {
        fumList should (contain inOrderElementsOf Seq("fum", "fum", "foe", "fie", "fee") and be (fumList))
      }
    }

    describe("when used with (not contain inOrderElementsOf xx and not contain inOrderElementsOf xx)") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (not contain inOrderElementsOf (Seq("fee", "fie", "foe", "fum")) and not contain inOrderElementsOf (Seq("fee", "fie", "foe", "fum")))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain inOrderElementsOf (Seq("fum", "foe", "fie", "fee")) and not contain inOrderElementsOf (Seq("happy", "birthday", "to", "you")))
        }
        checkMessageStackDepth(e1, FailureMessages.containedAllElementsOfInOrder(prettifier, fumList, Seq("fum", "foe", "fie", "fee")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not contain inOrderElementsOf (Seq("fee", "fie", "foe", "fum")) and not contain inOrderElementsOf (Seq("fum", "foe", "fie", "fee")))
        }
        checkMessageStackDepth(e2, FailureMessages.didNotContainAllElementsOfInOrder(prettifier, fumList, Seq("fee", "fie", "foe", "fum")) + ", but " + FailureMessages.containedAllElementsOfInOrder(prettifier, fumList, Seq("fum", "foe", "fie", "fee")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (not contain inOrderElementsOf (Seq("FEE", "FIE", "FOE", "FUM")) and not contain inOrderElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain inOrderElementsOf (Seq("FUM", "FOE", "FIE", "FEE")) and not contain inOrderElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e1, FailureMessages.containedAllElementsOfInOrder(prettifier, fumList, Seq("FUM", "FOE", "FIE", "FEE")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not contain inOrderElementsOf (Seq("FEE", "FIE", "FOE", "FUM")) and (not contain inOrderElementsOf (Seq("FUM", "FOE", "FIE", "FEE"))))
        }
        checkMessageStackDepth(e2, FailureMessages.didNotContainAllElementsOfInOrder(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FUM")) + ", but " + FailureMessages.containedAllElementsOfInOrder(prettifier, fumList, Seq("FUM", "FOE", "FIE", "FEE")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (not contain inOrderElementsOf (Seq("FEE", "FIE", "FOE", "FUM")) and not contain inOrderElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not contain inOrderElementsOf (Seq("FEE", "FIE", "FOE", "FUM")) and not contain inOrderElementsOf (Seq("FUM", "FOE", "FIE", "FEE")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.didNotContainAllElementsOfInOrder(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FUM")) + ", but " + FailureMessages.containedAllElementsOfInOrder(prettifier, fumList, Seq("FUM", "FOE", "FIE", "FEE")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (not contain inOrderElementsOf (Seq("FUM", "FOE", "FIE", "FEE")) and not contain inOrderElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, FailureMessages.containedAllElementsOfInOrder(prettifier, fumList, Seq("FUM", "FOE", "FIE", "FEE")), fileName, thisLineNumber - 2)
      }

      it("should do nothing when RHS contain duplicated value") {
        fumList should (not contain inOrderElementsOf (Seq("fee", "fie", "foe", "fie", "fum")) and not contain inOrderElementsOf (Seq("fee", "fie", "foe", "fum")))
        fumList should (not contain inOrderElementsOf (Seq("fee", "fie", "foe", "fum")) and not contain inOrderElementsOf (Seq("fee", "fie", "foe", "fie", "fum")))
      }
    }

    describe("when used with (not equal xx and not contain inOrderElementsOf xx)") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (not equal (toList) and not contain inOrderElementsOf (Seq("fee", "fie", "foe", "fum")))
        val e1 = intercept[TestFailedException] {
          fumList should (not equal (fumList) and not contain inOrderElementsOf (Seq("fee", "fie", "foe", "fum")))
        }
        checkMessageStackDepth(e1, FailureMessages.equaled(prettifier, fumList, fumList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not equal (toList) and not contain inOrderElementsOf (Seq("fum", "foe", "fie", "fee")))
        }
        checkMessageStackDepth(e2, FailureMessages.didNotEqual(prettifier, fumList, toList) + ", but " + FailureMessages.containedAllElementsOfInOrder(prettifier, fumList, Seq("fum", "foe", "fie", "fee")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (not equal (toList) and not contain inOrderElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))
        val e1 = intercept[TestFailedException] {
          fumList should (not equal (fumList) and not contain inOrderElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e1, FailureMessages.equaled(prettifier, fumList, fumList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not equal (toList) and (not contain inOrderElementsOf (Seq("FUM", "FOE", "FIE", "FEE"))))
        }
        checkMessageStackDepth(e2, FailureMessages.didNotEqual(prettifier, fumList, toList) + ", but " + FailureMessages.containedAllElementsOfInOrder(prettifier, fumList, Seq("FUM", "FOE", "FIE", "FEE")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (not equal (fumList) and not contain inOrderElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not equal (fumList) and not contain inOrderElementsOf (Seq("FUM", "FOE", "FIE", "FEE")))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.didNotEqual(prettifier, fumList, fumList) + ", but " + FailureMessages.containedAllElementsOfInOrder(prettifier, fumList, Seq("FUM", "FOE", "FIE", "FEE")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (not equal (toList) and not contain inOrderElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, FailureMessages.equaled(prettifier, fumList, toList), fileName, thisLineNumber - 2)
      }

      it("should do nothing when RHS contain duplicated value") {
        fumList should (not equal (toList) and not contain inOrderElementsOf (Seq("fee", "fie", "foe", "fie", "fum")))
      }
    }

    describe("when used with (not be xx and not contain inOrderElementsOf xx)") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (not be (toList) and not contain inOrderElementsOf (Seq("fee", "fie", "foe", "fum")))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) and not contain inOrderElementsOf (Seq("fee", "fie", "foe", "fum")))
        }
        checkMessageStackDepth(e1, FailureMessages.wasEqualTo(prettifier, fumList, fumList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not be (toList) and not contain inOrderElementsOf (Seq("fum", "foe", "fie", "fee")))
        }
        checkMessageStackDepth(e2, FailureMessages.wasNotEqualTo(prettifier, fumList, toList) + ", but " + FailureMessages.containedAllElementsOfInOrder(prettifier, fumList, Seq("fum", "foe", "fie", "fee")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (not be (toList) and not contain inOrderElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) and not contain inOrderElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e1, FailureMessages.wasEqualTo(prettifier, fumList, fumList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not be (toList) and (not contain inOrderElementsOf (Seq("FUM", "FOE", "FIE", "FEE"))))
        }
        checkMessageStackDepth(e2, FailureMessages.wasNotEqualTo(prettifier, fumList, toList) + ", but " + FailureMessages.containedAllElementsOfInOrder(prettifier, fumList, Seq("FUM", "FOE", "FIE", "FEE")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (not be (toList) and not contain inOrderElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not be (toList) and not contain inOrderElementsOf (Seq("FUM", "FOE", "FIE", "FEE")))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.wasNotEqualTo(prettifier, fumList, toList) + ", but " + FailureMessages.containedAllElementsOfInOrder(prettifier, fumList, Seq("FUM", "FOE", "FIE", "FEE")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (not be (fumList) and not contain inOrderElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, FailureMessages.wasEqualTo(prettifier, fumList, fumList), fileName, thisLineNumber - 2)
        (fumList should (not contain inOrderElementsOf (Seq(" FEE ", " FIE ", " FOE ", " FUU ")) and not contain inOrderElementsOf (Seq(" FEE ", " FIE ", " FOE ", " FUU ")))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }

      it("should do nothing when RHS contain duplicated value") {
        fumList should (not be (toList) and not contain inOrderElementsOf (Seq("fee", "fie", "foe", "fie", "fum")))
      }
    }
  }

  describe("every of Everys") {

    val list1s: Every[Every[Int]] = Every(Every(0, 1, 2, 2, 3), Every(0, 1, 2, 2, 3), Every(0, 1, 2, 2, 3))
    val lists: Every[Every[Int]] = Every(Every(0, 1, 2, 3, 3), Every(0, 1, 2, 3, 3), Every(8, 2, 3, 4))
    val hiLists: Every[Every[String]] = Every(Every("he", "hi", "hello"), Every("he", "hi", "hello"), Every("he", "hi", "hello"))

    def allErrMsg(index: Int, message: String, lineNumber: Int, left: Any): String =
      "'all' inspection failed, because: \n" +
        "  at index " + index + ", " + message + " (" + fileName + ":" + (lineNumber) + ") \n" +
        "in " + decorateToStringValue(prettifier, left)

    describe("used with contain inOrderElementsOf xx and contain inOrderElementsOf xx") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (contain inOrderElementsOf Seq(1, 2, 3) and contain inOrderElementsOf Seq(1, 2, 3))
        atLeast (2, lists) should (contain inOrderElementsOf Seq(1, 2, 3) and contain inOrderElementsOf Seq(1, 2, 3))
        atMost (2, lists) should (contain inOrderElementsOf Seq(1, 2, 3) and contain inOrderElementsOf Seq(1, 2, 3))
        no (lists) should (contain inOrderElementsOf Seq(3, 6, 9) and contain inOrderElementsOf Seq(3, 4, 5))

        val e1 = intercept[TestFailedException] {
          all (lists) should (contain inOrderElementsOf Seq(1, 2, 3) and contain inOrderElementsOf Seq(1, 2, 3))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(prettifier, lists(2)) + " did not contain all elements of " + decorateToStringValue(prettifier, Seq(1, 2, 3)) + " in order", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (list1s) should (contain inOrderElementsOf Seq(1, 2, 3) and contain inOrderElementsOf Seq(1, 3, 4))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, list1s(0)) + " contained all elements of " + decorateToStringValue(prettifier, Seq(1, 2, 3)) + " in order" + ", but " + decorateToStringValue(prettifier, list1s(0)) + " did not contain all elements of " + decorateToStringValue(prettifier, Seq(1, 3, 4)) + " in order", thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)

        val e3 = intercept[TestFailedException] {
          all (hiLists) should (contain inOrderElementsOf Seq("hi", "hello") and contain inOrderElementsOf Seq("hello", "hi"))
        }
        checkMessageStackDepth(e3, allErrMsg(0, decorateToStringValue(prettifier, hiLists(0)) + " contained all elements of " + decorateToStringValue(prettifier, Seq("hi", "hello")) + " in order" + ", but " + decorateToStringValue(prettifier, hiLists(0)) + " did not contain all elements of " + decorateToStringValue(prettifier, Seq("hello", "hi")) + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (contain inOrderElementsOf Seq("HI", "HELLO") and contain inOrderElementsOf Seq("HI", "HELLO"))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (contain inOrderElementsOf Seq("HO", "HELLO") and contain inOrderElementsOf Seq("HI", "HELLO"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, hiLists(0)) + " did not contain all elements of " + decorateToStringValue(prettifier, Seq("HO", "HELLO")) + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (hiLists) should (contain inOrderElementsOf Seq("HI", "HELLO") and contain inOrderElementsOf Seq("HELLO", "HI"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, hiLists(0)) + " contained all elements of " + decorateToStringValue(prettifier, Seq("HI", "HELLO")) + " in order" + ", but " + decorateToStringValue(prettifier, hiLists(0)) + " did not contain all elements of " + decorateToStringValue(prettifier, Seq("HELLO", "HI")) + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (all (hiLists) should (contain inOrderElementsOf Seq("HI", "HELLO") and contain inOrderElementsOf Seq("HI", "HELLO"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (contain inOrderElementsOf Seq("HO", "HELLO") and contain inOrderElementsOf Seq("HI", "HELLO"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, hiLists(0)) + " did not contain all elements of " + decorateToStringValue(prettifier, Seq("HO", "HELLO")) + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (contain inOrderElementsOf Seq("HI", "HELLO") and contain inOrderElementsOf Seq("HELLO", "HI"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, hiLists(0)) + " contained all elements of " + decorateToStringValue(prettifier, Seq("HI", "HELLO")) + " in order" + ", but " + decorateToStringValue(prettifier, hiLists(0)) + " did not contain all elements of " + decorateToStringValue(prettifier, Seq("HELLO", "HI")) + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should do nothing when RHS contain duplicated value") {
        all (list1s) should (contain inOrderElementsOf Seq(1, 2, 2, 3) and contain inOrderElementsOf Seq(1, 2, 3))
        all (list1s) should (contain inOrderElementsOf Seq(1, 2, 3) and contain inOrderElementsOf Seq(1, 2, 2, 3))
      }
    }

    describe("when used with (be xx and contain inOrderElementsOf xx)") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (be (Many(0, 1, 2, 2, 3)) and contain inOrderElementsOf Seq(1, 2, 3))
        atLeast (2, lists) should (be (Many(0, 1, 2, 3, 3)) and contain inOrderElementsOf Seq(1, 2, 3))
        atMost (2, lists) should (be (Many(3, 2, 1)) and contain inOrderElementsOf Seq(1, 2, 3))
        no (lists) should (be (Many(3, 6, 9)) and contain inOrderElementsOf Seq(3, 4, 5))

        val e1 = intercept[TestFailedException] {
          all (lists) should (be (Many(0, 1, 2, 3, 3)) and contain inOrderElementsOf Seq(1, 2, 3))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(prettifier, lists(2)) + " was not equal to " + decorateToStringValue(prettifier, Many(0, 1, 2, 3, 3)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (list1s) should (be (Many(0, 1, 2, 2, 3)) and contain inOrderElementsOf Seq(2, 3, 8))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, list1s(0)) + " was equal to " + decorateToStringValue(prettifier, Many(0, 1, 2, 2, 3)) + ", but " + decorateToStringValue(prettifier, list1s(0)) + " did not contain all elements of " + decorateToStringValue(prettifier, Seq(2, 3, 8)) + " in order", thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)

        val e3 = intercept[TestFailedException] {
          all (hiLists) should (be (Many("he", "hi", "hello")) and contain inOrderElementsOf Seq("hello", "hi"))
        }
        checkMessageStackDepth(e3, allErrMsg(0, decorateToStringValue(prettifier, hiLists(0)) + " was equal to " + decorateToStringValue(prettifier, Many("he", "hi", "hello")) + ", but " + decorateToStringValue(prettifier, hiLists(0)) + " did not contain all elements of " + decorateToStringValue(prettifier, Seq("hello", "hi")) + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e4 = intercept[TestFailedException] {
          all (list1s) should (be (Many(0, 1, 2, 2, 3)) and contain inOrderElementsOf Seq(2, 3, 8))
        }
        checkMessageStackDepth(e4, allErrMsg(0, decorateToStringValue(prettifier, list1s(0)) + " was equal to " + decorateToStringValue(prettifier, Many(0, 1, 2, 2, 3)) + ", but " + decorateToStringValue(prettifier, list1s(0)) + " did not contain all elements of " + decorateToStringValue(prettifier, Seq(2, 3, 8)) + " in order", thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (be (Many("he", "hi", "hello")) and contain inOrderElementsOf Seq("HI", "HELLO"))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (be (Many("HI", "HELLO")) and contain inOrderElementsOf Seq("HI", "HELLO"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, hiLists(0)) + " was not equal to " + decorateToStringValue(prettifier, Many("HI", "HELLO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (hiLists) should (be (Many("he", "hi", "hello")) and contain inOrderElementsOf Seq("HELLO", "HI"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, hiLists(0)) + " was equal to " + decorateToStringValue(prettifier, Many("he", "hi", "hello")) + ", but " + decorateToStringValue(prettifier, hiLists(0)) + " did not contain all elements of " + decorateToStringValue(prettifier, Seq("HELLO", "HI")) + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (all (hiLists) should (be (Many("he", "hi", "hello")) and contain inOrderElementsOf Seq("HI", "HELLO"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (be (Many("HI", "HELLO")) and contain inOrderElementsOf Seq("HI", "HELLO"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, hiLists(0)) + " was not equal to " + decorateToStringValue(prettifier, Many("HI", "HELLO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (be (Many("he", "hi", "hello")) and contain inOrderElementsOf Seq("HELLO", "HI"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, hiLists(0)) + " was equal to " + decorateToStringValue(prettifier, Many("he", "hi", "hello")) + ", but " + decorateToStringValue(prettifier, hiLists(0)) + " did not contain all elements of " + decorateToStringValue(prettifier, Seq("HELLO", "HI")) + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should do nothing when RHS contain duplicated value") {
        all (list1s) should (be (Many(0, 1, 2, 2, 3)) and contain inOrderElementsOf Seq(1, 2, 2, 3))
      }
    }

    describe("when used with (not contain inOrderElementsOf xx and not contain inOrderElementsOf xx)") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (not contain inOrderElementsOf (Seq(3, 2, 8)) and not contain inOrderElementsOf (Seq(8, 3, 4)))
        atLeast (2, lists) should (not contain inOrderElementsOf (Seq(3, 8, 5)) and not contain inOrderElementsOf (Seq(8, 3, 4)))
        atMost (2, lists) should (not contain inOrderElementsOf (Seq(2, 4, 3)) and contain inOrderElementsOf (Seq(2, 3, 4)))
        no (list1s) should (not contain inOrderElementsOf (Seq(1, 2, 3)) and not contain inOrderElementsOf (Seq(1, 2, 3)))

        val e1 = intercept[TestFailedException] {
          all (lists) should (not contain inOrderElementsOf (Seq(2, 3, 4)) and not contain inOrderElementsOf (Seq(8, 3, 4)))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(prettifier, lists(2)) + " contained all elements of " + decorateToStringValue(prettifier, Seq(2, 3, 4)) + " in order", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (lists) should (not contain inOrderElementsOf (Seq(3, 6, 8)) and not contain inOrderElementsOf (Seq(2, 3, 4)))
        }
        checkMessageStackDepth(e2, allErrMsg(2, decorateToStringValue(prettifier, lists(2)) + " did not contain all elements of " + decorateToStringValue(prettifier, Seq(3, 6, 8)) + " in order" + ", but " + decorateToStringValue(prettifier, lists(2)) + " contained all elements of " + decorateToStringValue(prettifier, Seq(2, 3, 4)) + " in order", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e3 = intercept[TestFailedException] {
          all (hiLists) should (not contain inOrderElementsOf (Seq("hi", "hello")) and not contain inOrderElementsOf (Seq("ho", "hey", "howdy")))
        }
        checkMessageStackDepth(e3, allErrMsg(0, decorateToStringValue(prettifier, hiLists(0)) + " contained all elements of " + decorateToStringValue(prettifier, Seq("hi", "hello")) + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e4 = intercept[TestFailedException] {
          all (hiLists) should (not contain inOrderElementsOf (Seq("ho", "hey", "howdy")) and not contain inOrderElementsOf (Seq("hi", "hello")))
        }
        checkMessageStackDepth(e4, allErrMsg(0, decorateToStringValue(prettifier, hiLists(0)) + " did not contain all elements of " + decorateToStringValue(prettifier, Seq("ho", "hey", "howdy")) + " in order" + ", but " + decorateToStringValue(prettifier, hiLists(0)) + " contained all elements of " + decorateToStringValue(prettifier, Seq("hi", "hello")) + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (not contain inOrderElementsOf (Seq("HELLO", "HI")) and not contain inOrderElementsOf (Seq("HELLO", "HO")))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not contain inOrderElementsOf (Seq("HI", "HELLO")) and not contain inOrderElementsOf (Seq("HO", "HE")))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, hiLists(0)) + " contained all elements of " + decorateToStringValue(prettifier, Seq("HI", "HELLO")) + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (hiLists) should (not contain inOrderElementsOf (Seq("HELLO", "HI")) and not contain inOrderElementsOf (Seq("HI", "HELLO")))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, hiLists(0)) + " did not contain all elements of " + decorateToStringValue(prettifier, Seq("HELLO", "HI")) + " in order" + ", but " + decorateToStringValue(prettifier, hiLists(0)) + " contained all elements of " + decorateToStringValue(prettifier, Seq("HI", "HELLO")) + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (all (hiLists) should (not contain inOrderElementsOf (Seq("HELLO", "HI")) and not contain inOrderElementsOf (Seq("HELLO", "HO")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not contain inOrderElementsOf (Seq("HI", "HELLO")) and not contain inOrderElementsOf (Seq("HO", "HE")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, hiLists(0)) + " contained all elements of " + decorateToStringValue(prettifier, Seq("HI", "HELLO")) + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (not contain inOrderElementsOf (Seq("HELLO", "HI")) and not contain inOrderElementsOf (Seq("HI", "HELLO")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, hiLists(0)) + " did not contain all elements of " + decorateToStringValue(prettifier, Seq("HELLO", "HI")) + " in order" + ", but " + decorateToStringValue(prettifier, hiLists(0)) + " contained all elements of " + decorateToStringValue(prettifier, Seq("HI", "HELLO")) + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should do nothing when RHS contain duplicated value") {
        all (list1s) should (not contain inOrderElementsOf (Seq(1, 3, 3, 8)) and not contain inOrderElementsOf (Seq(8, 3, 4)))
        all (list1s) should (not contain inOrderElementsOf (Seq(1, 3, 3, 8)) and not contain inOrderElementsOf (Seq(8, 3, 4)))
      }
    }

    describe("when used with (not be xx and not contain inOrderElementsOf xx)") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (not be (One(2)) and not contain inOrderElementsOf (Seq(8, 3, 4)))
        atLeast (2, lists) should (not be (One(3)) and not contain inOrderElementsOf (Seq(8, 3, 4)))
        atMost (2, lists) should (not be (Many(8, 2, 3, 4)) and not contain inOrderElementsOf (Seq(2, 3, 4)))
        no (list1s) should (not be (Many(0, 1, 2, 2, 3)) and not contain inOrderElementsOf (Seq(1, 2, 3)))

        val e1 = intercept[TestFailedException] {
          all (lists) should (not be (Many(8, 2, 3, 4)) and not contain inOrderElementsOf (Seq(8, 3, 4)))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(prettifier, lists(2)) + " was equal to " + decorateToStringValue(prettifier, Many(8, 2, 3, 4)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (lists) should (not be (One(3)) and not contain inOrderElementsOf (Seq(2, 3, 4)))
        }
        checkMessageStackDepth(e2, allErrMsg(2, decorateToStringValue(prettifier, lists(2)) + " was not equal to " + decorateToStringValue(prettifier, One(3)) + ", but " + decorateToStringValue(prettifier, lists(2)) + " contained all elements of " + decorateToStringValue(prettifier, Seq(2, 3, 4)) + " in order", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e3 = intercept[TestFailedException] {
          all (hiLists) should (not be (Many("he", "hi", "hello")) and not contain inOrderElementsOf (Seq("ho", "hey", "howdy")))
        }
        checkMessageStackDepth(e3, allErrMsg(0, decorateToStringValue(prettifier, hiLists(0)) + " was equal to " + decorateToStringValue(prettifier, Many("he", "hi", "hello")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e4 = intercept[TestFailedException] {
          all (hiLists) should (not be (One("ho")) and not contain inOrderElementsOf (Seq("hi", "hello")))
        }
        checkMessageStackDepth(e4, allErrMsg(0, decorateToStringValue(prettifier, hiLists(0)) + " was not equal to " + decorateToStringValue(prettifier, One("ho")) + ", but " + decorateToStringValue(prettifier, hiLists(0)) + " contained all elements of " + decorateToStringValue(prettifier, Seq("hi", "hello")) + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (not be (One("ho")) and not contain inOrderElementsOf (Seq("HO", "HELLO")))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not be (Many("he", "hi", "hello")) and not contain inOrderElementsOf (Seq("HELLO", "HI")))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, hiLists(0)) + " was equal to " + decorateToStringValue(prettifier, Many("he", "hi", "hello")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (hiLists) should (not be (One("ho")) and not contain inOrderElementsOf (Seq("HI", "HELLO")))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, hiLists(0)) + " was not equal to " + decorateToStringValue(prettifier, One("ho")) + ", but " + decorateToStringValue(prettifier, hiLists(0)) + " contained all elements of " + decorateToStringValue(prettifier, Seq("HI", "HELLO")) + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (all (hiLists) should (not be (One("ho")) and not contain inOrderElementsOf (Seq("HO", "HELLO")))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not be (Many("he", "hi", "hello")) and not contain inOrderElementsOf (Seq("HELLO", "HI")))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, hiLists(0)) + " was equal to " + decorateToStringValue(prettifier, Many("he", "hi", "hello")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (not be (One("ho")) and not contain inOrderElementsOf (Seq("HI", "HELLO")))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, hiLists(0)) + " was not equal to " + decorateToStringValue(prettifier, One("ho")) + ", but " + decorateToStringValue(prettifier, hiLists(0)) + " contained all elements of " + decorateToStringValue(prettifier, Seq("HI", "HELLO")) + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should do nothing when RHS contain duplicated value") {
        all (list1s) should (not be (One(2)) and not contain inOrderElementsOf (Seq(1, 3, 3, 8)))
      }
    }
  }
}
