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
import scala.collection.mutable.ListBuffer
import exceptions.TestFailedException
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

class EveryShouldContainTheSameElementsInOrderAsLogicalOrSpec extends AnyFreeSpec {

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

  val fileName: String = "EveryShouldContainTheSameElementsInOrderAsLogicalOrSpec.scala"

  "an Every" - {

    val fumList: Every[String] = Every("fum", "foe", "fie", "fee")
    val toList: Every[String] = Every("you", "to", "birthday", "happy")

    "when used with (contain theSameElementsInOrderAs xx or contain theSameElementsInOrderAs xx)" - {

      "should do nothing if valid, else throw a TFE with an appropriate error message" in {
        fumList should (contain theSameElementsInOrderAs ListBuffer("fum", "foe", "fie", "fee") or contain theSameElementsInOrderAs ListBuffer("fum", "foe", "fie", "fee"))
        fumList should (contain theSameElementsInOrderAs ListBuffer("fee", "fie", "foe", "fum") or contain theSameElementsInOrderAs ListBuffer("fum", "foe", "fie", "fee"))
        fumList should (contain theSameElementsInOrderAs ListBuffer("fum", "foe", "fie", "fee") or contain theSameElementsInOrderAs ListBuffer("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain theSameElementsInOrderAs ListBuffer("fee", "fie", "foe", "fum") or contain theSameElementsInOrderAs ListBuffer("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, Resources.didNotContainSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("fee", "fie", "foe", "fum"))) + ", and " + Resources.didNotContainSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("happy", "birthday", "to", "you"))), fileName, thisLineNumber - 2)
      }

      "should use the implicit Equality in scope" in {
        implicit val ise = upperCaseStringEquality
        fumList should (contain theSameElementsInOrderAs ListBuffer("FUM", "FOE", "FIE", "FEE") or contain theSameElementsInOrderAs ListBuffer("FUM", "FOE", "FIE", "FEE"))
        fumList should (contain theSameElementsInOrderAs ListBuffer("FEE", "FIE", "FOE", "FUM") or contain theSameElementsInOrderAs ListBuffer("FUM", "FOE", "FIE", "FEE"))
        fumList should (contain theSameElementsInOrderAs ListBuffer("FUM", "FOE", "FIE", "FEE") or contain theSameElementsInOrderAs ListBuffer("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain theSameElementsInOrderAs ListBuffer("FEE", "FIE", "FOE", "FUM") or (contain theSameElementsInOrderAs ListBuffer("FIE", "FEE", "FAM", "FOE")))
        }
        checkMessageStackDepth(e1, Resources.didNotContainSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("FEE", "FIE", "FOE", "FUM"))) + ", and " + Resources.didNotContainSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("FIE", "FEE", "FAM", "FOE"))), fileName, thisLineNumber - 2)
      }

      "should use an explicitly provided Equality" in {
        (fumList should (contain theSameElementsInOrderAs ListBuffer("FUM", "FOE", "FIE", "FEE") or contain theSameElementsInOrderAs ListBuffer("FUM", "FOE", "FIE", "FEE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (contain theSameElementsInOrderAs ListBuffer("FEE", "FIE", "FOE", "FUM") or contain theSameElementsInOrderAs ListBuffer("FUM", "FOE", "FIE", "FEE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (contain theSameElementsInOrderAs ListBuffer("FUM", "FOE", "FIE", "FEE") or contain theSameElementsInOrderAs ListBuffer("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain theSameElementsInOrderAs ListBuffer("FEE", "FIE", "FOE", "FUM") or contain theSameElementsInOrderAs ListBuffer("FIE", "FEE", "FAM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.didNotContainSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("FEE", "FIE", "FOE", "FUM"))) + ", and " + Resources.didNotContainSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("FIE", "FEE", "FAM", "FOE"))), fileName, thisLineNumber - 2)
        (fumList should (contain theSameElementsInOrderAs ListBuffer(" FUM ", " FOE ", " FIE ", " FEE ") or contain theSameElementsInOrderAs ListBuffer(" FUM ", " FOE ", " FIE ", " FEE "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
    }

    "when used with (equal xx and contain theSameElementsInOrderAs xx)" - {

      "should do nothing if valid, else throw a TFE with an appropriate error message" in {
        fumList should (equal (fumList) or contain theSameElementsInOrderAs ListBuffer("fum", "foe", "fie", "fee"))
        fumList should (equal (toList) or contain theSameElementsInOrderAs ListBuffer("fum", "foe", "fie", "fee"))
        fumList should (equal (fumList) or contain theSameElementsInOrderAs ListBuffer("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) or contain theSameElementsInOrderAs ListBuffer("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources.didNotEqual(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)) + ", and " + Resources.didNotContainSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("fee", "fie", "foe", "fum"))), fileName, thisLineNumber - 2)
      }

      "should use the implicit Equality in scope" in {
        implicit val ise = upperCaseStringEquality
        fumList should (equal (fumList) or contain theSameElementsInOrderAs ListBuffer("FUM", "FOE", "FIE", "FEE"))
        fumList should (equal (toList) or contain theSameElementsInOrderAs ListBuffer("FUM", "FOE", "FIE", "FEE"))
        fumList should (equal (fumList) or contain theSameElementsInOrderAs ListBuffer("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) or (contain theSameElementsInOrderAs ListBuffer("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e1, Resources.didNotEqual(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)) + ", and " + Resources.didNotContainSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("FEE", "FIE", "FOE", "FUM"))), fileName, thisLineNumber - 2)
      }

      "should use an explicitly provided Equality" in {
        (fumList should (equal (toList) or contain theSameElementsInOrderAs ListBuffer("FUM", "FOE", "FIE", "FEE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (equal (fumList) or contain theSameElementsInOrderAs ListBuffer("FUM", "FOE", "FIE", "FEE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (equal (toList) or contain theSameElementsInOrderAs (ListBuffer("FEE", "FIE", "FOE", "FUM")))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (equal (fumList) or contain theSameElementsInOrderAs (ListBuffer("FEE", "FIE", "FOE", "FUM")))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.didNotEqual(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)) + ", and " + Resources.didNotContainSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("FEE", "FIE", "FOE", "FUM"))), fileName, thisLineNumber - 2)
        (fumList should (equal (toList) or contain theSameElementsInOrderAs ListBuffer(" FEE ", " FIE ", " FOE ", " FUM "))) (decided by invertedListOfStringEquality, after being lowerCased and trimmed)
      }
    }

    "when used with (be xx and contain theSameElementsInOrderAs xx)" - {

      "should do nothing if valid, else throw a TFE with an appropriate error message" in {
        fumList should (be (fumList) or contain theSameElementsInOrderAs ListBuffer("fum", "foe", "fie", "fee"))
        fumList should (be (toList) or contain theSameElementsInOrderAs ListBuffer("fum", "foe", "fie", "fee"))
        fumList should (be (fumList) or contain theSameElementsInOrderAs ListBuffer("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (be (toList) or contain theSameElementsInOrderAs ListBuffer("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources.wasNotEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)) + ", and " + Resources.didNotContainSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("fee", "fie", "foe", "fum"))), fileName, thisLineNumber - 2)
      }

      "should use the implicit Equality in scope" in {
        implicit val ise = upperCaseStringEquality
        fumList should (be (fumList) or contain theSameElementsInOrderAs ListBuffer("FUM", "FOE", "FIE", "FEE"))
        fumList should (be (toList) or contain theSameElementsInOrderAs ListBuffer("FUM", "FOE", "FIE", "FEE"))
        fumList should (be (fumList) or contain theSameElementsInOrderAs ListBuffer("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (be (toList) or (contain theSameElementsInOrderAs ListBuffer("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e1, Resources.wasNotEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)) + ", and " + Resources.didNotContainSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("FEE", "FIE", "FOE", "FUM"))), fileName, thisLineNumber - 2)
      }

      "should use an explicitly provided Equality" in {
        (fumList should (be (fumList) or contain theSameElementsInOrderAs ListBuffer("FUM", "FOE", "FIE", "FEE"))) (decided by upperCaseStringEquality)
        (fumList should (be (toList) or contain theSameElementsInOrderAs ListBuffer("FUM", "FOE", "FIE", "FEE"))) (decided by upperCaseStringEquality)
        (fumList should (be (fumList) or contain theSameElementsInOrderAs ListBuffer("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (be (toList) or contain theSameElementsInOrderAs ListBuffer("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.wasNotEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)) + ", and " + Resources.didNotContainSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("FEE", "FIE", "FOE", "FUM"))), fileName, thisLineNumber - 2)
        (fumList should (be (fumList) or contain theSameElementsInOrderAs ListBuffer(" FUM ", " FOE ", " FIE ", " FEE "))) (after being lowerCased and trimmed)
      }
    }

    "when used with (contain theSameElementsInOrderAs xx and be xx)" - {

      "should do nothing if valid, else throw a TFE with an appropriate error message" in {
        fumList should (contain theSameElementsInOrderAs ListBuffer("fum", "foe", "fie", "fee") or be (fumList))
        fumList should (contain theSameElementsInOrderAs ListBuffer("fee", "fie", "foe", "fum") or be (fumList))
        fumList should (contain theSameElementsInOrderAs ListBuffer("fum", "foe", "fie", "fee") or be (toList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain theSameElementsInOrderAs ListBuffer("fee", "fie", "foe", "fum") or be (toList))
        }
        checkMessageStackDepth(e1, Resources.didNotContainSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("fee", "fie", "foe", "fum"))) + ", and " + Resources.wasNotEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)), fileName, thisLineNumber - 2)
      }

      "should use the implicit Equality in scope" in {
        implicit val ise = upperCaseStringEquality
        fumList should (contain theSameElementsInOrderAs ListBuffer("FUM", "FOE", "FIE", "FEE") or be (fumList))
        fumList should (contain theSameElementsInOrderAs ListBuffer("FEE", "FIE", "FOE", "FUM") or be (fumList))
        fumList should (contain theSameElementsInOrderAs ListBuffer("FUM", "FOE", "FIE", "FEE") or be (toList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain theSameElementsInOrderAs ListBuffer("FEE", "FIE", "FOE", "FUM") or be (toList))
        }
        checkMessageStackDepth(e1, Resources.didNotContainSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("FEE", "FIE", "FOE", "FUM"))) + ", and " + Resources.wasNotEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)), fileName, thisLineNumber - 2)
      }

      "should use an explicitly provided Equality" in {
        (fumList should (contain theSameElementsInOrderAs ListBuffer("FUM", "FOE", "FIE", "FEE") or be (fumList))) (decided by upperCaseStringEquality)
        (fumList should (contain theSameElementsInOrderAs ListBuffer("FEE", "FIE", "FOE", "FUM") or be (fumList))) (decided by upperCaseStringEquality)
        (fumList should (contain theSameElementsInOrderAs ListBuffer("FUM", "FOE", "FIE", "FEE") or be (toList))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain theSameElementsInOrderAs ListBuffer("FEE", "FIE", "FOE", "FUM") or be (toList))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.didNotContainSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("FEE", "FIE", "FOE", "FUM"))) + ", and " + Resources.wasNotEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)), fileName, thisLineNumber - 2)
        (fumList should (contain theSameElementsInOrderAs ListBuffer(" FUM ", " FOE ", " FIE ", " FEE ") or be (fumList))) (after being lowerCased and trimmed)
      }
    }

    "when used with (not contain theSameElementsInOrderAs xx and not contain theSameElementsInOrderAs xx)" - {

      "should do nothing if valid, else throw a TFE with an appropriate error message" in {
        fumList should (not contain theSameElementsInOrderAs (ListBuffer("fee", "fie", "foe", "fum")) or not contain theSameElementsInOrderAs (ListBuffer("fee", "fie", "foe", "fum")))
        fumList should (not contain theSameElementsInOrderAs (ListBuffer("fum", "foe", "fie", "fee")) or not contain theSameElementsInOrderAs (ListBuffer("fee", "fie", "foe", "fum")))
        fumList should (not contain theSameElementsInOrderAs (ListBuffer("fee", "fie", "foe", "fum")) or not contain theSameElementsInOrderAs (ListBuffer("fum", "foe", "fie", "fee")))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain theSameElementsInOrderAs (ListBuffer("fum", "foe", "fie", "fee")) or not contain theSameElementsInOrderAs (ListBuffer("fum", "foe", "fie", "fee")))
        }
        checkMessageStackDepth(e1, Resources.containedSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("fum", "foe", "fie", "fee"))) + ", and " + Resources.containedSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("fum", "foe", "fie", "fee"))), fileName, thisLineNumber - 2)
      }

      "should use the implicit Equality in scope" in {
        implicit val ise = upperCaseStringEquality
        fumList should (not contain theSameElementsInOrderAs (ListBuffer("FEE", "FIE", "FOE", "FUM")) or not contain theSameElementsInOrderAs (ListBuffer("FEE", "FIE", "FOE", "FUM")))
        fumList should (not contain theSameElementsInOrderAs (ListBuffer("FUM", "FOE", "FIE", "FEE")) or not contain theSameElementsInOrderAs (ListBuffer("FEE", "FIE", "FOE", "FUM")))
        fumList should (not contain theSameElementsInOrderAs (ListBuffer("FEE", "FIE", "FOE", "FUM")) or not contain theSameElementsInOrderAs (ListBuffer("FUM", "FOE", "FIE", "FEE")))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain theSameElementsInOrderAs (ListBuffer("FUM", "FOE", "FIE", "FEE")) or not contain theSameElementsInOrderAs (ListBuffer("FUM", "FOE", "FIE", "FEE")))
        }
        checkMessageStackDepth(e1, Resources.containedSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("FUM", "FOE", "FIE", "FEE"))) + ", and " + Resources.containedSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("FUM", "FOE", "FIE", "FEE"))), fileName, thisLineNumber - 2)
      }

      "should use an explicitly provided Equality" in {
        (fumList should (not contain theSameElementsInOrderAs (ListBuffer("FEE", "FIE", "FOE", "FUM")) or not contain theSameElementsInOrderAs (ListBuffer("FEE", "FIE", "FOE", "FUM")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (not contain theSameElementsInOrderAs (ListBuffer("FUM", "FOE", "FIE", "FEE")) or not contain theSameElementsInOrderAs (ListBuffer("FEE", "FIE", "FOE", "FUM")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (not contain theSameElementsInOrderAs (ListBuffer("FEE", "FIE", "FOE", "FUM")) or not contain theSameElementsInOrderAs (ListBuffer("FUM", "FOE", "FIE", "FEE")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not contain theSameElementsInOrderAs (ListBuffer("FUM", "FOE", "FIE", "FEE")) or not contain theSameElementsInOrderAs (ListBuffer("FUM", "FOE", "FIE", "FEE")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.containedSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("FUM", "FOE", "FIE", "FEE"))) + ", and " + Resources.containedSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("FUM", "FOE", "FIE", "FEE"))), fileName, thisLineNumber - 2)
      }
    }

    "when used with (not equal xx and not contain theSameElementsInOrderAs xx)" - {

      "should do nothing if valid, else throw a TFE with an appropriate error message" in {
        fumList should (not equal (toList) or not contain theSameElementsInOrderAs (ListBuffer("fee", "fie", "foe", "fum")))
        fumList should (not equal (fumList) or not contain theSameElementsInOrderAs (ListBuffer("fee", "fie", "foe", "fum")))
        fumList should (not equal (toList) or not contain theSameElementsInOrderAs (ListBuffer("fum", "foe", "fie", "fee")))
        val e1 = intercept[TestFailedException] {
          fumList should (not equal (fumList) or not contain theSameElementsInOrderAs (ListBuffer("fum", "foe", "fie", "fee")))
        }
        checkMessageStackDepth(e1, Resources.equaled(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)) + ", and " + Resources.containedSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("fum", "foe", "fie", "fee"))), fileName, thisLineNumber - 2)
      }

      "should use the implicit Equality in scope" in {
        implicit val ise = upperCaseStringEquality
        fumList should (not equal (toList) or not contain theSameElementsInOrderAs (ListBuffer("FEE", "FIE", "FOE", "FUM")))
        fumList should (not equal (fumList) or not contain theSameElementsInOrderAs (ListBuffer("FEE", "FIE", "FOE", "FUM")))
        fumList should (not equal (toList) or not contain theSameElementsInOrderAs (ListBuffer("FUM", "FOE", "FIE", "FEE")))
        val e2 = intercept[TestFailedException] {
          fumList should (not equal (fumList) or (not contain theSameElementsInOrderAs (ListBuffer("FUM", "FOE", "FIE", "FEE"))))
        }
        checkMessageStackDepth(e2, Resources.equaled(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)) + ", and " + Resources.containedSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("FUM", "FOE", "FIE", "FEE"))), fileName, thisLineNumber - 2)
      }

      "should use an explicitly provided Equality" in {
        (fumList should (not equal (fumList) or not contain theSameElementsInOrderAs (ListBuffer("FEE", "FIE", "FOE", "FUM")))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (not equal (toList) or not contain theSameElementsInOrderAs (ListBuffer("FEE", "FIE", "FOE", "FUM")))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (not equal (fumList) or not contain theSameElementsInOrderAs (ListBuffer("FUM", "FOE", "FIE", "FEE")))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not equal (toList) or not contain theSameElementsInOrderAs (ListBuffer("FUM", "FOE", "FIE", "FEE")))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.equaled(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)) + ", and " + Resources.containedSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("FUM", "FOE", "FIE", "FEE"))), fileName, thisLineNumber - 2)
      }
    }

    "when used with (not be xx and not contain theSameElementsInOrderAs xx)" - {

      "should do nothing if valid, else throw a TFE with an appropriate error message" in {
        fumList should (not be (toList) or not contain theSameElementsInOrderAs (ListBuffer("fee", "fie", "foe", "fum")))
        fumList should (not be (fumList) or not contain theSameElementsInOrderAs (ListBuffer("fee", "fie", "foe", "fum")))
        fumList should (not be (toList) or not contain theSameElementsInOrderAs (ListBuffer("fum", "foe", "fie", "fee")))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) or not contain theSameElementsInOrderAs (ListBuffer("fum", "foe", "fie", "fee")))
        }
        checkMessageStackDepth(e1, Resources.wasEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)) + ", and " + Resources.containedSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("fum", "foe", "fie", "fee"))), fileName, thisLineNumber - 2)
      }

      "should use the implicit Equality in scope" in {
        implicit val ise = upperCaseStringEquality
        fumList should (not be (toList) or not contain theSameElementsInOrderAs (ListBuffer("FEE", "FIE", "FOE", "FUM")))
        fumList should (not be (fumList) or not contain theSameElementsInOrderAs (ListBuffer("FEE", "FIE", "FOE", "FUM")))
        fumList should (not be (toList) or not contain theSameElementsInOrderAs (ListBuffer("FUM", "FOE", "FIE", "FEE")))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) or (not contain theSameElementsInOrderAs (ListBuffer("FUM", "FOE", "FIE", "FEE"))))
        }
        checkMessageStackDepth(e1, Resources.wasEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)) + ", and " + Resources.containedSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("FUM", "FOE", "FIE", "FEE"))), fileName, thisLineNumber - 2)
      }

      "should use an explicitly provided Equality" in {
        (fumList should (not be (toList) or not contain theSameElementsInOrderAs (ListBuffer("FEE", "FIE", "FOE", "FUM")))) (decided by upperCaseStringEquality)
        (fumList should (not be (fumList) or not contain theSameElementsInOrderAs (ListBuffer("FEE", "FIE", "FOE", "FUM")))) (decided by upperCaseStringEquality)
        (fumList should (not be (toList) or not contain theSameElementsInOrderAs (ListBuffer("FUM", "FOE", "FIE", "FEE")))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not be (fumList) or not contain theSameElementsInOrderAs (ListBuffer("FUM", "FOE", "FIE", "FEE")))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.wasEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)) + ", and " + Resources.containedSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("FUM", "FOE", "FIE", "FEE"))), fileName, thisLineNumber - 2)
        (fumList should (not contain theSameElementsInOrderAs (Set(" FEE ", " FIE ", " FOE ", " FUU ")) or not contain theSameElementsInOrderAs (Set(" FEE ", " FIE ", " FOE ", " FUU ")))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
    }

  }

  "every of Everys" - {

    val list1s: Every[Every[Int]] = Every(Every(1, 2, 3), Every(1, 2, 3), Every(1, 2, 3))
    val lists: Every[Every[Int]] = Every(Every(1, 2, 3), Every(1, 2, 3), Every(2, 3, 4))
    val hiLists: Every[Every[String]] = Every(Every("hi", "hello"), Every("hi", "hello"), Every("hi", "hello"))
    val toLists: Every[Every[String]] = Every(Every("you", "to"), Every("you", "to"), Every("you", "to"))

    def allErrMsg(index: Int, message: String, lineNumber: Int, left: Any): String =
      "'all' inspection failed, because: \n" +
        "  at index " + index + ", " + message + " (" + fileName + ":" + (lineNumber) + ") \n" +
        "in " + decorateToStringValue(prettifier, left)

    "when used with (contain theSameElementsInOrderAs xx and contain theSameElementsInOrderAs xx)" - {

      "should do nothing if valid, else throw a TFE with an appropriate error message" in {
        all (list1s) should (contain theSameElementsInOrderAs ListBuffer(1, 2, 3) or contain theSameElementsInOrderAs ListBuffer(1, 2, 3))
        all (list1s) should (contain theSameElementsInOrderAs ListBuffer(3, 2, 5) or contain theSameElementsInOrderAs ListBuffer(1, 2, 3))
        all (list1s) should (contain theSameElementsInOrderAs ListBuffer(1, 2, 3) or contain theSameElementsInOrderAs ListBuffer(2, 3, 4))

        atLeast (2, lists) should (contain theSameElementsInOrderAs ListBuffer(1, 2, 3) or contain theSameElementsInOrderAs ListBuffer(1, 2, 3))
        atLeast (2, lists) should (contain theSameElementsInOrderAs ListBuffer(3, 6, 5) or contain theSameElementsInOrderAs ListBuffer(1, 2, 3))
        atLeast (2, lists) should (contain theSameElementsInOrderAs ListBuffer(1, 2, 3) or contain theSameElementsInOrderAs ListBuffer(8, 3, 4))

        val e1 = intercept[TestFailedException] {
          all (lists) should (contain theSameElementsInOrderAs ListBuffer(1, 2, 3) or contain theSameElementsInOrderAs ListBuffer(1, 2, 3))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(prettifier, Many(2, 3, 4)) + " did not contain the same elements in the same (iterated) order as " + decorateToStringValue(prettifier, ListBuffer(1, 2, 3)) + ", and " + decorateToStringValue(prettifier, Many(2, 3, 4)) + " did not contain the same elements in the same (iterated) order as " + decorateToStringValue(prettifier, ListBuffer(1, 2, 3)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
      }

      "should use the implicit Equality in scope" in {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (contain theSameElementsInOrderAs ListBuffer("HI", "HELLO") or contain theSameElementsInOrderAs ListBuffer("hi", "hello"))
        all (hiLists) should (contain theSameElementsInOrderAs ListBuffer("HELLO", "HO") or contain theSameElementsInOrderAs ListBuffer("hi", "hello"))
        all (hiLists) should (contain theSameElementsInOrderAs ListBuffer("HI", "HELLO") or contain theSameElementsInOrderAs ListBuffer("hello", "ho"))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (contain theSameElementsInOrderAs ListBuffer("HELLO", "HO") or contain theSameElementsInOrderAs ListBuffer("hello", "ho"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, Many("hi", "hello")) + " did not contain the same elements in the same (iterated) order as " + decorateToStringValue(prettifier, ListBuffer("HELLO", "HO")) + ", and " + decorateToStringValue(prettifier, Many("hi", "hello")) + " did not contain the same elements in the same (iterated) order as " + decorateToStringValue(prettifier, ListBuffer("hello", "ho")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      "should use an explicitly provided Equality" in {
        (all (hiLists) should (contain theSameElementsInOrderAs ListBuffer("HI", "HELLO") or contain theSameElementsInOrderAs ListBuffer("hi", "hello"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (contain theSameElementsInOrderAs ListBuffer("HELLO", "HO") or contain theSameElementsInOrderAs ListBuffer("hi", "hello"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (contain theSameElementsInOrderAs ListBuffer("HI", "HELLO") or contain theSameElementsInOrderAs ListBuffer("hello", "ho"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)

        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (contain theSameElementsInOrderAs ListBuffer("HELLO", "HO") or contain theSameElementsInOrderAs ListBuffer("hello", "ho"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, Many("hi", "hello")) + " did not contain the same elements in the same (iterated) order as " + decorateToStringValue(prettifier, ListBuffer("HELLO", "HO")) + ", and " + decorateToStringValue(prettifier, Many("hi", "hello")) + " did not contain the same elements in the same (iterated) order as " + decorateToStringValue(prettifier, ListBuffer("hello", "ho")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
    }

    "when used with (be xx and contain theSameElementsInOrderAs xx)" - {

      "should do nothing if valid, else throw a TFE with an appropriate error message" in {
        all (list1s) should (be (Many(1, 2, 3)) or contain theSameElementsInOrderAs ListBuffer(1, 2, 3))
        all (list1s) should (be (Many(2, 3, 4)) or contain theSameElementsInOrderAs ListBuffer(1, 2, 3))
        all (list1s) should (be (Many(1, 2, 3)) or contain theSameElementsInOrderAs ListBuffer(2, 3, 4))

        val e1 = intercept[TestFailedException] {
          all (list1s) should (be (Many(2, 3, 4)) or contain theSameElementsInOrderAs ListBuffer(2, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, Many(1, 2, 3)) + " was not equal to " + decorateToStringValue(prettifier, Many(2, 3, 4)) + ", and " + decorateToStringValue(prettifier, Many(1, 2, 3)) + " did not contain the same elements in the same (iterated) order as " + decorateToStringValue(prettifier, ListBuffer(2, 3, 4)), thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
      }

      "should use the implicit Equality in scope" in {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (be (Many("hi", "hello")) or contain theSameElementsInOrderAs ListBuffer("HI", "HELLO"))
        all (hiLists) should (be (Many("ho", "hello")) or contain theSameElementsInOrderAs ListBuffer("HI", "HELLO"))
        all (hiLists) should (be (Many("hi", "hello")) or contain theSameElementsInOrderAs ListBuffer("HELLO", "HI"))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (be (Many("ho", "hello")) or contain theSameElementsInOrderAs ListBuffer("HELLO", "HI"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, Many("hi", "hello")) + " was not equal to " + decorateToStringValue(prettifier, Many("ho", "hello")) + ", and " + decorateToStringValue(prettifier, Many("hi", "hello")) + " did not contain the same elements in the same (iterated) order as " + decorateToStringValue(prettifier, ListBuffer("HELLO", "HI")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      "should use an explicitly provided Equality" in {
        (all (hiLists) should (be (Many("hi", "hello")) or contain theSameElementsInOrderAs ListBuffer("HI", "HELLO"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (be (Many("ho", "hello")) or contain theSameElementsInOrderAs ListBuffer("HI", "HELLO"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (be (Many("hi", "hello")) or contain theSameElementsInOrderAs ListBuffer("HELLO", "HI"))) (decided by upperCaseStringEquality)

        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (be (Many("ho", "hello")) or contain theSameElementsInOrderAs ListBuffer("HELLO", "HI"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, Many("hi", "hello")) + " was not equal to " + decorateToStringValue(prettifier, Many("ho", "hello")) + ", and " + decorateToStringValue(prettifier, Many("hi", "hello")) + " did not contain the same elements in the same (iterated) order as " + decorateToStringValue(prettifier, ListBuffer("HELLO", "HI")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
    }

    "when used with (not contain theSameElementsInOrderAs xx and not contain theSameElementsInOrderAs xx)" - {

      "should do nothing if valid, else throw a TFE with an appropriate error message" in {
        all (list1s) should (not contain theSameElementsInOrderAs (ListBuffer(3, 2, 8)) or not contain theSameElementsInOrderAs (ListBuffer(8, 3, 4)))
        all (list1s) should (not contain theSameElementsInOrderAs (ListBuffer(1, 2, 3)) or not contain theSameElementsInOrderAs (ListBuffer(8, 3, 4)))
        all (list1s) should (not contain theSameElementsInOrderAs (ListBuffer(3, 2, 8)) or not contain theSameElementsInOrderAs (ListBuffer(1, 2, 3)))

        val e1 = intercept[TestFailedException] {
          all (lists) should (not contain theSameElementsInOrderAs (ListBuffer(2, 3, 4)) or not contain theSameElementsInOrderAs (ListBuffer(2, 3, 4)))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(prettifier, Many(2, 3, 4)) + " contained the same elements in the same (iterated) order as " + decorateToStringValue(prettifier, ListBuffer(2, 3, 4)) + ", and " + decorateToStringValue(prettifier, Many(2, 3, 4)) + " contained the same elements in the same (iterated) order as " + decorateToStringValue(prettifier, ListBuffer(2, 3, 4)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
      }

      "should use the implicit Equality in scope" in {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (not contain theSameElementsInOrderAs (ListBuffer("HELLO", "HI")) or not contain theSameElementsInOrderAs (ListBuffer("hello", "hi")))
        all (hiLists) should (not contain theSameElementsInOrderAs (ListBuffer("HI", "HELLO")) or not contain theSameElementsInOrderAs (ListBuffer("hello", "hi")))
        all (hiLists) should (not contain theSameElementsInOrderAs (ListBuffer("HELLO", "HI")) or not contain theSameElementsInOrderAs (ListBuffer("hi", "hello")))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not contain theSameElementsInOrderAs (ListBuffer("HI", "HELLO")) or not contain theSameElementsInOrderAs (ListBuffer("hi", "hello")))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, Many("hi", "hello")) + " contained the same elements in the same (iterated) order as " + decorateToStringValue(prettifier, ListBuffer("HI", "HELLO")) + ", and " + decorateToStringValue(prettifier, Many("hi", "hello")) + " contained the same elements in the same (iterated) order as " + decorateToStringValue(prettifier, ListBuffer("hi", "hello")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      "should use an explicitly provided Equality" in {
        (all (hiLists) should (not contain theSameElementsInOrderAs (ListBuffer("HELLO", "HI")) or not contain theSameElementsInOrderAs (ListBuffer("hello", "hi")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (not contain theSameElementsInOrderAs (ListBuffer("HI", "HELLO")) or not contain theSameElementsInOrderAs (ListBuffer("hello", "hi")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (not contain theSameElementsInOrderAs (ListBuffer("HELLO", "HI")) or not contain theSameElementsInOrderAs (ListBuffer("hi", "hello")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)

        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not contain theSameElementsInOrderAs (ListBuffer("HI", "HELLO")) or not contain theSameElementsInOrderAs (ListBuffer("hi", "hello")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, Many("hi", "hello")) + " contained the same elements in the same (iterated) order as " + decorateToStringValue(prettifier, ListBuffer("HI", "HELLO")) + ", and " + decorateToStringValue(prettifier, Many("hi", "hello")) + " contained the same elements in the same (iterated) order as " + decorateToStringValue(prettifier, ListBuffer("hi", "hello")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
    }

    "when used with (not be xx and not contain theSameElementsInOrderAs xx)" - {

      "should do nothing if valid, else throw a TFE with an appropriate error message" in {
        all (list1s) should (not be (One(2)) or not contain theSameElementsInOrderAs (ListBuffer(8, 3, 4)))
        all (list1s) should (not be (Many(1, 2, 3)) or not contain theSameElementsInOrderAs (ListBuffer(8, 3, 4)))
        all (list1s) should (not be (One(2)) or not contain theSameElementsInOrderAs (ListBuffer(1, 2, 3)))

        val e1 = intercept[TestFailedException] {
          all (list1s) should (not be (Many(1, 2, 3)) or not contain theSameElementsInOrderAs (ListBuffer(1, 2, 3)))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, Many(1, 2, 3)) + " was equal to " + decorateToStringValue(prettifier, Many(1, 2, 3)) + ", and " + decorateToStringValue(prettifier, Many(1, 2, 3)) + " contained the same elements in the same (iterated) order as " + decorateToStringValue(prettifier, ListBuffer(1, 2, 3)), thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
      }

      "should use the implicit Equality in scope" in {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (not be (Many("hello", "ho")) or not contain theSameElementsInOrderAs (ListBuffer("HELLO", "HO")))
        all (hiLists) should (not be (Many("hi", "hello")) or not contain theSameElementsInOrderAs (ListBuffer("HELLO", "HO")))
        all (hiLists) should (not be (Many("hello", "ho")) or not contain theSameElementsInOrderAs (ListBuffer("HI", "HELLO")))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not be (Many("hi", "hello")) or not contain theSameElementsInOrderAs (ListBuffer("HI", "HELLO")))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, Many("hi", "hello")) + " was equal to " + decorateToStringValue(prettifier, Many("hi", "hello")) + ", and " + decorateToStringValue(prettifier, Many("hi", "hello")) + " contained the same elements in the same (iterated) order as " + decorateToStringValue(prettifier, ListBuffer("HI", "HELLO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      "should use an explicitly provided Equality" in {
        (all (hiLists) should (not be (Many("hello", "ho")) or not contain theSameElementsInOrderAs (ListBuffer("HELLO", "HO")))) (decided by upperCaseStringEquality)
        (all (hiLists) should (not be (Many("hi", "hello")) or not contain theSameElementsInOrderAs (ListBuffer("HELLO", "HO")))) (decided by upperCaseStringEquality)
        (all (hiLists) should (not be (Many("hello", "ho")) or not contain theSameElementsInOrderAs (ListBuffer("HI", "HELLO")))) (decided by upperCaseStringEquality)

        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not be (Many("hi", "hello")) or not contain theSameElementsInOrderAs (ListBuffer("HI", "HELLO")))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, Many("hi", "hello")) + " was equal to " + decorateToStringValue(prettifier, Many("hi", "hello")) + ", and " + decorateToStringValue(prettifier, Many("hi", "hello")) + " contained the same elements in the same (iterated) order as " + decorateToStringValue(prettifier, ListBuffer("HI", "HELLO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
    }
  }
}
