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

class EveryShouldContainInOrderElementsOfLogicalOrSpec extends AnyFunSpec {

  private val prettifier = Prettifier.default

  //ADDITIONAL//

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

  val fileName: String = "EveryShouldContainInOrderElementsOfLogicalOrSpec.scala"

  describe("an Every") {

    val fumList: Every[String] = Every("fex", "fum", "fum", "foe", "fie", "fee", "fee", "fee")
    val toList: Every[String] = Every("too", "you", "to", "to", "birthday", "birthday", "happy")

    describe("when used with (contain inOrderElementsOf xx or contain inOrderElementsOf xx)") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (contain inOrderElementsOf Seq("fum", "foe", "fie", "fee") or contain inOrderElementsOf Seq("fum", "foe", "fie", "fee"))
        fumList should (contain inOrderElementsOf Seq("fee", "fie", "foe", "fum") or contain inOrderElementsOf Seq("fum", "foe", "fie", "fee"))
        fumList should (contain inOrderElementsOf Seq("fum", "foe", "fie", "fee") or contain inOrderElementsOf Seq("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain inOrderElementsOf Seq("fee", "fie", "foe", "fum") or contain inOrderElementsOf Seq("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, FailureMessages.didNotContainAllElementsOfInOrder(prettifier, fumList, Seq("fee", "fie", "foe", "fum")) + ", and " + FailureMessages.didNotContainAllElementsOfInOrder(prettifier, fumList, Seq("happy", "birthday", "to", "you")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE") or contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE"))
        fumList should (contain inOrderElementsOf Seq("FEE", "FIE", "FOE", "FUM") or contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE"))
        fumList should (contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE") or contain inOrderElementsOf Seq("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain inOrderElementsOf Seq("FEE", "FIE", "FOE", "FUM") or (contain inOrderElementsOf Seq("FIE", "FEE", "FAM", "FOE")))
        }
        checkMessageStackDepth(e1, FailureMessages.didNotContainAllElementsOfInOrder(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FUM")) + ", and " + FailureMessages.didNotContainAllElementsOfInOrder(prettifier, fumList, Seq("FIE", "FEE", "FAM", "FOE")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE") or contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (contain inOrderElementsOf Seq("FEE", "FIE", "FOE", "FUM") or contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE") or contain inOrderElementsOf Seq("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain inOrderElementsOf Seq("FEE", "FIE", "FOE", "FUM") or contain inOrderElementsOf Seq("FIE", "FEE", "FAM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.didNotContainAllElementsOfInOrder(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FUM")) + ", and " + FailureMessages.didNotContainAllElementsOfInOrder(prettifier, fumList, Seq("FIE", "FEE", "FAM", "FOE")), fileName, thisLineNumber - 2)
        (fumList should (contain inOrderElementsOf Seq(" FUM ", " FOE ", " FIE ", " FEE ") or contain inOrderElementsOf Seq(" FUM ", " FOE ", " FIE ", " FEE "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }

      it("should do nothing when RHS contain duplicated value") {
        fumList should (contain inOrderElementsOf Seq("fee", "fie", "foe", "fie", "fum") or contain inOrderElementsOf Seq("fum", "foe", "fie", "fee"))
        fumList should (contain inOrderElementsOf Seq("fum", "foe", "fie", "fee") or contain inOrderElementsOf Seq("fee", "fie", "foe", "fie", "fum"))
      }
    }

    describe("when used with (equal xx and contain inOrderElementsOf xx)") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (equal (fumList) or contain inOrderElementsOf Seq("fum", "foe", "fie", "fee"))
        fumList should (equal (toList) or contain inOrderElementsOf Seq("fum", "foe", "fie", "fee"))
        fumList should (equal (fumList) or contain inOrderElementsOf Seq("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) or contain inOrderElementsOf Seq("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, FailureMessages.didNotEqual(prettifier, fumList, toList) + ", and " + FailureMessages.didNotContainAllElementsOfInOrder(prettifier, fumList, Seq("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (equal (fumList) or contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE"))
        fumList should (equal (toList) or contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE"))
        fumList should (equal (fumList) or contain inOrderElementsOf Seq("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) or (contain inOrderElementsOf Seq("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e1, FailureMessages.didNotEqual(prettifier, fumList, toList) + ", and " + FailureMessages.didNotContainAllElementsOfInOrder(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (equal (toList) or contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (equal (fumList) or contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (equal (toList) or contain inOrderElementsOf Seq("FEE", "FIE", "FOE", "FUM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (equal (fumList) or contain inOrderElementsOf Seq("FEE", "FIE", "FOE", "FUM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.didNotEqual(prettifier, fumList, fumList) + ", and " + FailureMessages.didNotContainAllElementsOfInOrder(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
        (fumList should (equal (toList) or contain inOrderElementsOf Seq(" FEE ", " FIE ", " FOE ", " FUM "))) (decided by invertedListOfStringEquality, after being lowerCased and trimmed)
      }

      it("should do nothing when RHS contain duplicated value") {
        fumList should (equal (fumList) or contain inOrderElementsOf Seq("fee", "fie", "foe", "fie", "fum"))
      }
    }

    describe("when used with (be xx and contain inOrderElementsOf xx)") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (be (fumList) or contain inOrderElementsOf Seq("fum", "foe", "fie", "fee"))
        fumList should (be (toList) or contain inOrderElementsOf Seq("fum", "foe", "fie", "fee"))
        fumList should (be (fumList) or contain inOrderElementsOf Seq("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (be (toList) or contain inOrderElementsOf Seq("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, FailureMessages.wasNotEqualTo(prettifier, fumList, toList) + ", and " + FailureMessages.didNotContainAllElementsOfInOrder(prettifier, fumList, Seq("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (be (fumList) or contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE"))
        fumList should (be (toList) or contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE"))
        fumList should (be (fumList) or contain inOrderElementsOf Seq("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (be (toList) or (contain inOrderElementsOf Seq("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e1, FailureMessages.wasNotEqualTo(prettifier, fumList, toList) + ", and " + FailureMessages.didNotContainAllElementsOfInOrder(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (be (fumList) or contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE"))) (decided by upperCaseStringEquality)
        (fumList should (be (toList) or contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE"))) (decided by upperCaseStringEquality)
        (fumList should (be (fumList) or contain inOrderElementsOf Seq("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (be (toList) or contain inOrderElementsOf Seq("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.wasNotEqualTo(prettifier, fumList, toList) + ", and " + FailureMessages.didNotContainAllElementsOfInOrder(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
        (fumList should (be (fumList) or contain inOrderElementsOf Seq(" FUM ", " FOE ", " FIE ", " FEE "))) (after being lowerCased and trimmed)
      }

      it("should do nothing when RHS contain duplicated value") {
        fumList should (be (fumList) or contain inOrderElementsOf Seq("fee", "fie", "foe", "fie", "fum"))
      }
    }

    describe("when used with (contain inOrderElementsOf xx and be xx)") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (contain inOrderElementsOf Seq("fum", "foe", "fie", "fee") or be (fumList))
        fumList should (contain inOrderElementsOf Seq("fee", "fie", "foe", "fum") or be (fumList))
        fumList should (contain inOrderElementsOf Seq("fum", "foe", "fie", "fee") or be (toList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain inOrderElementsOf Seq("fee", "fie", "foe", "fum") or be (toList))
        }
        checkMessageStackDepth(e1, FailureMessages.didNotContainAllElementsOfInOrder(prettifier, fumList, Seq("fee", "fie", "foe", "fum")) + ", and " + FailureMessages.wasNotEqualTo(prettifier, fumList, toList), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE") or be (fumList))
        fumList should (contain inOrderElementsOf Seq("FEE", "FIE", "FOE", "FUM") or be (fumList))
        fumList should (contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE") or be (toList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain inOrderElementsOf Seq("FEE", "FIE", "FOE", "FUM") or be (toList))
        }
        checkMessageStackDepth(e1, FailureMessages.didNotContainAllElementsOfInOrder(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FUM")) + ", and " + FailureMessages.wasNotEqualTo(prettifier, fumList, toList), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE") or be (fumList))) (decided by upperCaseStringEquality)
        (fumList should (contain inOrderElementsOf Seq("FEE", "FIE", "FOE", "FUM") or be (fumList))) (decided by upperCaseStringEquality)
        (fumList should (contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE") or be (toList))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain inOrderElementsOf Seq("FEE", "FIE", "FOE", "FUM") or be (toList))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.didNotContainAllElementsOfInOrder(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FUM")) + ", and " + FailureMessages.wasNotEqualTo(prettifier, fumList, toList), fileName, thisLineNumber - 2)
        (fumList should (contain inOrderElementsOf Seq(" FUM ", " FOE ", " FIE ", " FEE ") or be (fumList))) (after being lowerCased and trimmed)
      }

      it("should do nothing when RHS contain duplicated value") {
        fumList should (contain inOrderElementsOf Seq("fee", "fie", "foe", "fie", "fum") or be (fumList))
      }
    }

    describe("when used with (not contain inOrderElementsOf xx and not contain inOrderElementsOf xx)") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (not contain inOrderElementsOf (Seq("fee", "fie", "foe", "fum")) or not contain inOrderElementsOf (Seq("fee", "fie", "foe", "fum")))
        fumList should (not contain inOrderElementsOf (Seq("fum", "foe", "fie", "fee")) or not contain inOrderElementsOf (Seq("fee", "fie", "foe", "fum")))
        fumList should (not contain inOrderElementsOf (Seq("fee", "fie", "foe", "fum")) or not contain inOrderElementsOf (Seq("fum", "foe", "fie", "fee")))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain inOrderElementsOf (Seq("fum", "foe", "fie", "fee")) or not contain inOrderElementsOf (Seq("fum", "foe", "fie", "fee")))
        }
        checkMessageStackDepth(e1, FailureMessages.containedAllElementsOfInOrder(prettifier, fumList, Seq("fum", "foe", "fie", "fee")) + ", and " + FailureMessages.containedAllElementsOfInOrder(prettifier, fumList, Seq("fum", "foe", "fie", "fee")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (not contain inOrderElementsOf (Seq("FEE", "FIE", "FOE", "FUM")) or not contain inOrderElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))
        fumList should (not contain inOrderElementsOf (Seq("FUM", "FOE", "FIE", "FEE")) or not contain inOrderElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))
        fumList should (not contain inOrderElementsOf (Seq("FEE", "FIE", "FOE", "FUM")) or not contain inOrderElementsOf (Seq("FUM", "FOE", "FIE", "FEE")))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain inOrderElementsOf (Seq("FUM", "FOE", "FIE", "FEE")) or not contain inOrderElementsOf (Seq("FUM", "FOE", "FIE", "FEE")))
        }
        checkMessageStackDepth(e1, FailureMessages.containedAllElementsOfInOrder(prettifier, fumList, Seq("FUM", "FOE", "FIE", "FEE")) + ", and " + FailureMessages.containedAllElementsOfInOrder(prettifier, fumList, Seq("FUM", "FOE", "FIE", "FEE")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (not contain inOrderElementsOf (Seq("FEE", "FIE", "FOE", "FUM")) or not contain inOrderElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (not contain inOrderElementsOf (Seq("FUM", "FOE", "FIE", "FEE")) or not contain inOrderElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (not contain inOrderElementsOf (Seq("FEE", "FIE", "FOE", "FUM")) or not contain inOrderElementsOf (Seq("FUM", "FOE", "FIE", "FEE")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not contain inOrderElementsOf (Seq("FUM", "FOE", "FIE", "FEE")) or not contain inOrderElementsOf (Seq("FUM", "FOE", "FIE", "FEE")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.containedAllElementsOfInOrder(prettifier, fumList, Seq("FUM", "FOE", "FIE", "FEE")) + ", and " + FailureMessages.containedAllElementsOfInOrder(prettifier, fumList, Seq("FUM", "FOE", "FIE", "FEE")), fileName, thisLineNumber - 2)
      }

      it("should do nothing when RHS contain duplicated value") {
        fumList should (not contain inOrderElementsOf (Seq("fee", "fie", "foe", "fie", "fum")) or not contain inOrderElementsOf (Seq("fee", "fie", "foe", "fum")))
        fumList should (not contain inOrderElementsOf (Seq("fee", "fie", "foe", "fum")) or not contain inOrderElementsOf (Seq("fee", "fie", "foe", "fie", "fum")))
      }
    }

    describe("when used with (not equal xx and not contain inOrderElementsOf xx)") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (not equal (toList) or not contain inOrderElementsOf (Seq("fee", "fie", "foe", "fum")))
        fumList should (not equal (fumList) or not contain inOrderElementsOf (Seq("fee", "fie", "foe", "fum")))
        fumList should (not equal (toList) or not contain inOrderElementsOf (Seq("fum", "foe", "fie", "fee")))
        val e1 = intercept[TestFailedException] {
          fumList should (not equal (fumList) or not contain inOrderElementsOf (Seq("fum", "foe", "fie", "fee")))
        }
        checkMessageStackDepth(e1, FailureMessages.equaled(prettifier, fumList, fumList) + ", and " + FailureMessages.containedAllElementsOfInOrder(prettifier, fumList, Seq("fum", "foe", "fie", "fee")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (not equal (toList) or not contain inOrderElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))
        fumList should (not equal (fumList) or not contain inOrderElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))
        fumList should (not equal (toList) or not contain inOrderElementsOf (Seq("FUM", "FOE", "FIE", "FEE")))
        val e2 = intercept[TestFailedException] {
          fumList should (not equal (fumList) or (not contain inOrderElementsOf (Seq("FUM", "FOE", "FIE", "FEE"))))
        }
        checkMessageStackDepth(e2, FailureMessages.equaled(prettifier, fumList, fumList) + ", and " + FailureMessages.containedAllElementsOfInOrder(prettifier, fumList, Seq("FUM", "FOE", "FIE", "FEE")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (not equal (fumList) or not contain inOrderElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (not equal (toList) or not contain inOrderElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (not equal (fumList) or not contain inOrderElementsOf (Seq("FUM", "FOE", "FIE", "FEE")))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not equal (toList) or not contain inOrderElementsOf (Seq("FUM", "FOE", "FIE", "FEE")))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.equaled(prettifier, fumList, toList) + ", and " + FailureMessages.containedAllElementsOfInOrder(prettifier, fumList, Seq("FUM", "FOE", "FIE", "FEE")), fileName, thisLineNumber - 2)
      }

      it("should do nothing when RHS contain duplicated value") {
        fumList should (not equal (toList) or not contain inOrderElementsOf (Seq("fee", "fie", "foe", "fie", "fum")))
      }
    }

    describe("when used with (not be xx and not contain inOrderElementsOf xx)") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (not be (toList) or not contain inOrderElementsOf (Seq("fee", "fie", "foe", "fum")))
        fumList should (not be (fumList) or not contain inOrderElementsOf (Seq("fee", "fie", "foe", "fum")))
        fumList should (not be (toList) or not contain inOrderElementsOf (Seq("fum", "foe", "fie", "fee")))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) or not contain inOrderElementsOf (Seq("fum", "foe", "fie", "fee")))
        }
        checkMessageStackDepth(e1, FailureMessages.wasEqualTo(prettifier, fumList, fumList) + ", and " + FailureMessages.containedAllElementsOfInOrder(prettifier, fumList, Seq("fum", "foe", "fie", "fee")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (not be (toList) or not contain inOrderElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))
        fumList should (not be (fumList) or not contain inOrderElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))
        fumList should (not be (toList) or not contain inOrderElementsOf (Seq("FUM", "FOE", "FIE", "FEE")))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) or (not contain inOrderElementsOf (Seq("FUM", "FOE", "FIE", "FEE"))))
        }
        checkMessageStackDepth(e1, FailureMessages.wasEqualTo(prettifier, fumList, fumList) + ", and " + FailureMessages.containedAllElementsOfInOrder(prettifier, fumList, Seq("FUM", "FOE", "FIE", "FEE")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (not be (toList) or not contain inOrderElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))) (decided by upperCaseStringEquality)
        (fumList should (not be (fumList) or not contain inOrderElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))) (decided by upperCaseStringEquality)
        (fumList should (not be (toList) or not contain inOrderElementsOf (Seq("FUM", "FOE", "FIE", "FEE")))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not be (fumList) or not contain inOrderElementsOf (Seq("FUM", "FOE", "FIE", "FEE")))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.wasEqualTo(prettifier, fumList, fumList) + ", and " + FailureMessages.containedAllElementsOfInOrder(prettifier, fumList, Seq("FUM", "FOE", "FIE", "FEE")), fileName, thisLineNumber - 2)
        (fumList should (not contain inOrderElementsOf (Seq(" FEE ", " FIE ", " FOE ", " FUU ")) or not contain inOrderElementsOf (Seq(" FEE ", " FIE ", " FOE ", " FUU ")))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }

      it("should do nothing when RHS contain duplicated value") {
        fumList should (not be (toList) or not contain inOrderElementsOf (Seq("fee", "fie", "foe", "fie", "fum")))
      }
    }
  }

  describe("every of Everys") {

    val list1s: Every[Every[Int]] = Every(Every(0, 1, 2, 2, 3), Every(0, 1, 2, 2, 3), Every(0, 1, 2, 2, 3))
    val lists: Every[Every[Int]] = Every(Every(0, 1, 2, 2, 3), Every(0, 1, 1, 2, 3, 3), Every(8, 2, 3, 4))
    val hiLists: Every[Every[String]] = Every(Every("he", "hi", "hello"), Every("he", "hi", "hello"), Every("he", "hi", "hello"))
    val toLists: Every[Every[String]] = Every(Every("nice", "you", "to"), Every("nice", "you", "to"), Every("nice", "you", "to"))

    def allErrMsg(index: Int, message: String, lineNumber: Int, left: Any): String =
      "'all' inspection failed, because: \n" +
        "  at index " + index + ", " + message + " (" + fileName + ":" + (lineNumber) + ") \n" +
        "in " + decorateToStringValue(prettifier, left)

    describe("when used with (contain inOrderElementsOf xx and contain inOrderElementsOf xx)") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (contain inOrderElementsOf Seq(1, 2, 3) or contain inOrderElementsOf Seq(1, 2, 3))
        all (list1s) should (contain inOrderElementsOf Seq(3, 2, 5) or contain inOrderElementsOf Seq(1, 2, 3))
        all (list1s) should (contain inOrderElementsOf Seq(1, 2, 3) or contain inOrderElementsOf Seq(2, 3, 4))

        atLeast (2, lists) should (contain inOrderElementsOf Seq(1, 2, 3) or contain inOrderElementsOf Seq(1, 2, 3))
        atLeast (2, lists) should (contain inOrderElementsOf Seq(3, 6, 5) or contain inOrderElementsOf Seq(1, 2, 3))
        atLeast (2, lists) should (contain inOrderElementsOf Seq(1, 2, 3) or contain inOrderElementsOf Seq(8, 3, 4))

        val e1 = intercept[TestFailedException] {
          all (lists) should (contain inOrderElementsOf Seq(1, 2, 3) or contain inOrderElementsOf Seq(1, 2, 3))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(prettifier, lists(2)) + " did not contain all elements of " + decorateToStringValue(prettifier, Seq(1, 2, 3)) + " in order" + ", and " + decorateToStringValue(prettifier, lists(2)) + " did not contain all elements of " + decorateToStringValue(prettifier, Seq(1, 2, 3)) + " in order", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (contain inOrderElementsOf Seq("HI", "HELLO") or contain inOrderElementsOf Seq("hi", "hello"))
        all (hiLists) should (contain inOrderElementsOf Seq("HELLO", "HO") or contain inOrderElementsOf Seq("hi", "hello"))
        all (hiLists) should (contain inOrderElementsOf Seq("HI", "HELLO") or contain inOrderElementsOf Seq("hello", "ho"))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (contain inOrderElementsOf Seq("HELLO", "HO") or contain inOrderElementsOf Seq("hello", "ho"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, hiLists(0)) + " did not contain all elements of " + decorateToStringValue(prettifier, Seq("HELLO", "HO")) + " in order" + ", and " + decorateToStringValue(prettifier, hiLists(0)) + " did not contain all elements of " + decorateToStringValue(prettifier, Seq("hello", "ho")) + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (all (hiLists) should (contain inOrderElementsOf Seq("HI", "HELLO") or contain inOrderElementsOf Seq("hi", "hello"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (contain inOrderElementsOf Seq("HELLO", "HO") or contain inOrderElementsOf Seq("hi", "hello"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (contain inOrderElementsOf Seq("HI", "HELLO") or contain inOrderElementsOf Seq("hello", "ho"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)

        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (contain inOrderElementsOf Seq("HELLO", "HO") or contain inOrderElementsOf Seq("hello", "ho"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, hiLists(0)) + " did not contain all elements of " + decorateToStringValue(prettifier, Seq("HELLO", "HO")) + " in order" + ", and " + decorateToStringValue(prettifier, hiLists(0)) + " did not contain all elements of " + decorateToStringValue(prettifier, Seq("hello", "ho")) + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should do nothing when RHS contain duplicated value") {
        all (list1s) should (contain inOrderElementsOf Seq(1, 2, 2, 3) or contain inOrderElementsOf Seq(1, 2, 3))
        all (list1s) should (contain inOrderElementsOf Seq(1, 2, 3) or contain inOrderElementsOf Seq(1, 2, 2, 3))
      }
    }

    describe("when used with (be xx and contain inOrderElementsOf xx)") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (be (Many(0, 1, 2, 2, 3)) or contain inOrderElementsOf Seq(1, 2, 3))
        all (list1s) should (be (Many(8, 2, 3, 4)) or contain inOrderElementsOf Seq(1, 2, 3))
        all (list1s) should (be (Many(0, 1, 2, 2, 3)) or contain inOrderElementsOf Seq(2, 3, 4))

        val e1 = intercept[TestFailedException] {
          all (list1s) should (be (Many(8, 2, 3, 4)) or contain inOrderElementsOf Seq(2, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, list1s(0)) + " was not equal to " + decorateToStringValue(prettifier, Many(8, 2, 3, 4)) + ", and " + decorateToStringValue(prettifier, list1s(0)) + " did not contain all elements of " + decorateToStringValue(prettifier, Seq(2, 3, 4)) + " in order", thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (be (Many("he", "hi", "hello")) or contain inOrderElementsOf Seq("HI", "HELLO"))
        all (hiLists) should (be (Many("ho", "hello")) or contain inOrderElementsOf Seq("HI", "HELLO"))
        all (hiLists) should (be (Many("he", "hi", "hello")) or contain inOrderElementsOf Seq("HELLO", "HI"))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (be (Many("ho", "hello")) or contain inOrderElementsOf Seq("HELLO", "HI"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, hiLists(0)) + " was not equal to " + decorateToStringValue(prettifier, Many("ho", "hello")) + ", and " + decorateToStringValue(prettifier, hiLists(0)) + " did not contain all elements of " + decorateToStringValue(prettifier, Seq("HELLO", "HI")) + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (all (hiLists) should (be (Many("he", "hi", "hello")) or contain inOrderElementsOf Seq("HI", "HELLO"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (be (Many("ho", "hello")) or contain inOrderElementsOf Seq("HI", "HELLO"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (be (Many("he", "hi", "hello")) or contain inOrderElementsOf Seq("HELLO", "HI"))) (decided by upperCaseStringEquality)

        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (be (Many("ho", "hello")) or contain inOrderElementsOf Seq("HELLO", "HI"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, hiLists(0)) + " was not equal to " + decorateToStringValue(prettifier, Many("ho", "hello")) + ", and " + decorateToStringValue(prettifier, hiLists(0)) + " did not contain all elements of " + decorateToStringValue(prettifier, Seq("HELLO", "HI")) + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should do nothing when RHS contain duplicated value") {
        all (list1s) should (be (Many(0, 1, 2, 2, 3)) or contain inOrderElementsOf Seq(1, 2, 2, 3))
      }
    }

    describe("when used with (not contain inOrderElementsOf xx and not contain inOrderElementsOf xx)") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (not contain inOrderElementsOf (Seq(3, 2, 8)) or not contain inOrderElementsOf (Seq(8, 3, 4)))
        all (list1s) should (not contain inOrderElementsOf (Seq(1, 2, 3)) or not contain inOrderElementsOf (Seq(8, 3, 4)))
        all (list1s) should (not contain inOrderElementsOf (Seq(3, 2, 8)) or not contain inOrderElementsOf (Seq(1, 2, 3)))

        val e1 = intercept[TestFailedException] {
          all (lists) should (not contain inOrderElementsOf (Seq(2, 3, 4)) or not contain inOrderElementsOf (Seq(2, 3, 4)))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(prettifier, lists(2)) + " contained all elements of " + decorateToStringValue(prettifier, Seq(2, 3, 4)) + " in order" + ", and " + decorateToStringValue(prettifier, lists(2)) + " contained all elements of " + decorateToStringValue(prettifier, Seq(2, 3, 4)) + " in order", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (not contain inOrderElementsOf (Seq("HELLO", "HI")) or not contain inOrderElementsOf (Seq("hello", "hi")))
        all (hiLists) should (not contain inOrderElementsOf (Seq("HI", "HELLO")) or not contain inOrderElementsOf (Seq("hello", "hi")))
        all (hiLists) should (not contain inOrderElementsOf (Seq("HELLO", "HI")) or not contain inOrderElementsOf (Seq("hi", "hello")))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not contain inOrderElementsOf (Seq("HI", "HELLO")) or not contain inOrderElementsOf (Seq("hi", "hello")))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, hiLists(0)) + " contained all elements of " + decorateToStringValue(prettifier, Seq("HI", "HELLO")) + " in order" + ", and " + decorateToStringValue(prettifier, hiLists(0)) + " contained all elements of " + decorateToStringValue(prettifier, Seq("hi", "hello")) + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (all (hiLists) should (not contain inOrderElementsOf (Seq("HELLO", "HI")) or not contain inOrderElementsOf (Seq("hello", "hi")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (not contain inOrderElementsOf (Seq("HI", "HELLO")) or not contain inOrderElementsOf (Seq("hello", "hi")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (not contain inOrderElementsOf (Seq("HELLO", "HI")) or not contain inOrderElementsOf (Seq("hi", "hello")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)

        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not contain inOrderElementsOf (Seq("HI", "HELLO")) or not contain inOrderElementsOf (Seq("hi", "hello")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, hiLists(0)) + " contained all elements of " + decorateToStringValue(prettifier, Seq("HI", "HELLO")) + " in order" + ", and " + decorateToStringValue(prettifier, hiLists(0)) + " contained all elements of " + decorateToStringValue(prettifier, Seq("hi", "hello")) + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should do nothing when RHS contain duplicated value") {
        all (list1s) should (not contain inOrderElementsOf (Seq(1, 2, 2, 3)) or not contain inOrderElementsOf (Seq(8, 3, 4)))
        all (list1s) should (not contain inOrderElementsOf (Seq(8, 3, 4)) or not contain inOrderElementsOf (Seq(1, 2, 2, 3)))
      }
    }

    describe("when used with (not be xx and not contain inOrderElementsOf xx)") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (not be (One(2)) or not contain inOrderElementsOf (Seq(8, 3, 4)))
        all (list1s) should (not be (Many(0, 1, 2, 2, 3)) or not contain inOrderElementsOf (Seq(8, 3, 4)))
        all (list1s) should (not be (One(2)) or not contain inOrderElementsOf (Seq(1, 2, 3)))

        val e1 = intercept[TestFailedException] {
          all (list1s) should (not be (Many(0, 1, 2, 2, 3)) or not contain inOrderElementsOf (Seq(1, 2, 3)))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, list1s(0)) + " was equal to " + decorateToStringValue(prettifier, Many(0, 1, 2, 2, 3)) + ", and " + decorateToStringValue(prettifier, list1s(0)) + " contained all elements of " + decorateToStringValue(prettifier, Seq(1, 2, 3)) + " in order", thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (not be (Many("hello", "ho")) or not contain inOrderElementsOf (Seq("HELLO", "HO")))
        all (hiLists) should (not be (Many("he", "hi", "hello")) or not contain inOrderElementsOf (Seq("HELLO", "HO")))
        all (hiLists) should (not be (Many("hello", "ho")) or not contain inOrderElementsOf (Seq("HI", "HELLO")))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not be (Many("he", "hi", "hello")) or not contain inOrderElementsOf (Seq("HI", "HELLO")))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, hiLists(0)) + " was equal to " + decorateToStringValue(prettifier, Many("he", "hi", "hello")) + ", and " + decorateToStringValue(prettifier, hiLists(0)) + " contained all elements of " + decorateToStringValue(prettifier, Seq("HI", "HELLO")) + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (all (hiLists) should (not be (Many("hello", "ho")) or not contain inOrderElementsOf (Seq("HELLO", "HO")))) (decided by upperCaseStringEquality)
        (all (hiLists) should (not be (Many("he", "hi", "hello")) or not contain inOrderElementsOf (Seq("HELLO", "HO")))) (decided by upperCaseStringEquality)
        (all (hiLists) should (not be (Many("hello", "ho")) or not contain inOrderElementsOf (Seq("HI", "HELLO")))) (decided by upperCaseStringEquality)

        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not be (Many("he", "hi", "hello")) or not contain inOrderElementsOf (Seq("HI", "HELLO")))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, hiLists(0)) + " was equal to " + decorateToStringValue(prettifier, Many("he", "hi", "hello")) + ", and " + decorateToStringValue(prettifier, hiLists(0)) + " contained all elements of " + decorateToStringValue(prettifier, Seq("HI", "HELLO")) + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should do nothing when RHS contain duplicated value") {
        all (list1s) should (not be (One(2)) or not contain inOrderElementsOf (Seq(1, 2, 2, 3)))
      }
    }
  }
}
