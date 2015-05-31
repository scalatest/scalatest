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
import org.scalactic.Uniformity
import org.scalactic.StringNormalizations._
import SharedHelpers._
import FailureMessages.decorateToStringValue
import Matchers._
import exceptions.TestFailedException

class EveryShouldContainTheSameElementsAsLogicalAndSpec extends FunSpec {

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

  val fileName: String = "EveryShouldContainTheSameElementsAsLogicalAndSpec.scala"

  describe("an Every") {

    val fumList: Every[String] = Every("fum", "foe", "fie", "fee")
    val toList: Every[String] = Every("you", "to", "birthday", "happy")

    describe("when used with (contain theSameElementsAs (..) and contain theSameElementsAs (..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (contain theSameElementsAs Set("fee", "fie", "foe", "fum") and contain theSameElementsAs Set("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain theSameElementsAs Set("happy", "birthday", "to", "you") and contain theSameElementsAs Set("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources.didNotContainSameElements(decorateToStringValue(fumList), decorateToStringValue(Set("happy", "birthday", "to", "you"))), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (contain theSameElementsAs Set("fee", "fie", "foe", "fum") and contain theSameElementsAs Set("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e2, Resources.containedSameElements(decorateToStringValue(fumList), decorateToStringValue(Set("fee", "fie", "foe", "fum"))) + ", but " + Resources.didNotContainSameElements(decorateToStringValue(fumList), decorateToStringValue(Set("happy", "birthday", "to", "you"))), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (contain theSameElementsAs Set("FEE", "FIE", "FOE", "FUM") and contain theSameElementsAs Set("FEE", "FIE", "FUM", "FOE"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain theSameElementsAs Set("FEE", "FIE", "FOE", "FAM") and contain theSameElementsAs Set("FEE", "FIE", "FUM", "FOE"))
        }
        checkMessageStackDepth(e1, Resources.didNotContainSameElements(decorateToStringValue(fumList), decorateToStringValue(Set("FEE", "FIE", "FOE", "FAM"))), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (contain theSameElementsAs Set("FEE", "FIE", "FOE", "FUM") and (contain theSameElementsAs Set("FEE", "FIE", "FAM", "FOE")))
        }
        checkMessageStackDepth(e2, Resources.containedSameElements(decorateToStringValue(fumList), decorateToStringValue(Set("FEE", "FIE", "FOE", "FUM"))) + ", but " + Resources.didNotContainSameElements(decorateToStringValue(fumList), decorateToStringValue(Set("FEE", "FIE", "FAM", "FOE"))), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (contain theSameElementsAs Set("FEE", "FIE", "FOE", "FUM") and contain theSameElementsAs Set("FEE", "FIE", "FUM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain theSameElementsAs Set("FEE", "FIE", "FOE", "FAM") and contain theSameElementsAs Set("FEE", "FIE", "FUM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.didNotContainSameElements(decorateToStringValue(fumList), decorateToStringValue(Set("FEE", "FIE", "FOE", "FAM"))), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (contain theSameElementsAs Set("FEE", "FIE", "FOE", "FUM") and contain theSameElementsAs Set("FEE", "FIE", "FAM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources.containedSameElements(decorateToStringValue(fumList), decorateToStringValue(Set("FEE", "FIE", "FOE", "FUM"))) + ", but " + Resources.didNotContainSameElements(decorateToStringValue(fumList), decorateToStringValue(Set("FEE", "FIE", "FAM", "FOE"))), fileName, thisLineNumber - 2)
        (fumList should (contain theSameElementsAs Set(" FEE ", " FIE ", " FOE ", " FUM ") and contain theSameElementsAs Set(" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
    }

    describe("when used with (equal (..) and contain theSameElementsAs (..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (equal (fumList) and contain theSameElementsAs Set("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) and contain theSameElementsAs Set("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources.didNotEqual(decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (equal (fumList) and contain theSameElementsAs Set("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e2, Resources.equaled(decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", but " + Resources.didNotContainSameElements(decorateToStringValue(fumList), decorateToStringValue(Set("happy", "birthday", "to", "you"))), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (equal (fumList) and contain theSameElementsAs Set("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) and contain theSameElementsAs Set("FEE", "FIE", "FOE", "FUM"))
        }
        checkMessageStackDepth(e1, Resources.didNotEqual(decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (equal (fumList) and (contain theSameElementsAs Set("FEE", "FIE", "FOE", "FAM")))
        }
        checkMessageStackDepth(e2, Resources.equaled(decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", but " + Resources.didNotContainSameElements(decorateToStringValue(fumList), decorateToStringValue(Set("FEE", "FIE", "FOE", "FAM"))), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (equal (toList) and contain theSameElementsAs Set("FEE", "FIE", "FOE", "FUM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (equal (toList) and contain theSameElementsAs Set("FEE", "FIE", "FOE", "FAM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.equaled(decorateToStringValue(fumList), decorateToStringValue(toList)) + ", but " + Resources.didNotContainSameElements(decorateToStringValue(fumList), decorateToStringValue(Set("FEE", "FIE", "FOE", "FAM"))), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (equal (fumList) and contain theSameElementsAs Set("FEE", "FIE", "FOE", "FUM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources.didNotEqual(decorateToStringValue(fumList), decorateToStringValue(fumList)), fileName, thisLineNumber - 2)
        (fumList should (equal (toList) and contain theSameElementsAs Set(" FEE ", " FIE ", " FOE ", " FUM "))) (decided by invertedListOfStringEquality, after being lowerCased and trimmed)
      }
    }

    describe("when used with (be (..) and contain theSameElementsAs (..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (be (fumList) and contain theSameElementsAs Set("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (be (toList) and contain theSameElementsAs Set("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources.wasNotEqualTo(decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (be (fumList) and contain theSameElementsAs Set("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e2, Resources.wasEqualTo(decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", but " + Resources.didNotContainSameElements(decorateToStringValue(fumList), decorateToStringValue(Set("happy", "birthday", "to", "you"))), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (be (fumList) and contain theSameElementsAs Set("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (be (toList) and contain theSameElementsAs Set("FEE", "FIE", "FOE", "FUM"))
        }
        checkMessageStackDepth(e1, Resources.wasNotEqualTo(decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (be (fumList) and (contain theSameElementsAs Set("happy", "birthday", "to", "you")))
        }
        checkMessageStackDepth(e2, Resources.wasEqualTo(decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", but " + Resources.didNotContainSameElements(decorateToStringValue(fumList), decorateToStringValue(Set("happy", "birthday", "to", "you"))), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (be (fumList) and contain theSameElementsAs Set("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (be (fumList) and contain theSameElementsAs Set("happy", "birthday", "to", "you"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.wasEqualTo(decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", but " + Resources.didNotContainSameElements(decorateToStringValue(fumList), decorateToStringValue(Set("happy", "birthday", "to", "you"))), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (be (toList) and contain theSameElementsAs Set("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources.wasNotEqualTo(decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        (fumList should (be (fumList) and contain theSameElementsAs Set(" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed)
      }
    }

    describe("when used with (contain theSameElementsAs (..) and be (..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (contain theSameElementsAs Set("fie", "fee", "fum", "foe") and be (fumList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain theSameElementsAs Set("fee", "fie", "foe", "fum") and be (toList))
        }
        checkMessageStackDepth(e1, Resources.containedSameElements(decorateToStringValue(fumList), decorateToStringValue(Set("fee", "fie", "foe", "fum"))) + ", but " + Resources.wasNotEqualTo(decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (contain theSameElementsAs Set("happy", "birthday", "to", "you") and be (fumList))
        }
        checkMessageStackDepth(e2, Resources.didNotContainSameElements(decorateToStringValue(fumList), decorateToStringValue(Set("happy", "birthday", "to", "you"))), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (contain theSameElementsAs Set("FIE", "FEE", "FUM", "FOE") and be (fumList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain theSameElementsAs Set("FIE", "FEE", "FAM", "FOE") and be (toList))
        }
        checkMessageStackDepth(e1, Resources.didNotContainSameElements(decorateToStringValue(fumList), decorateToStringValue(Set("FIE", "FEE", "FAM", "FOE"))), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (contain theSameElementsAs Set("HAPPY", "BIRTHDAY", "TO", "YOU") and (be (fumList)))
        }
        checkMessageStackDepth(e2, Resources.didNotContainSameElements(decorateToStringValue(fumList), decorateToStringValue(Set("HAPPY", "BIRTHDAY", "TO", "YOU"))), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (contain theSameElementsAs Set("FIE", "FEE", "FUM", "FOE") and be (fumList))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain theSameElementsAs Set("FIE", "FEE", "FAM", "FOE") and be (fumList))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.didNotContainSameElements(decorateToStringValue(fumList), decorateToStringValue(Set("FIE", "FEE", "FAM", "FOE"))), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (contain theSameElementsAs Set("FIE", "FEE", "FUM", "FOE") and be (toList))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources.containedSameElements(decorateToStringValue(fumList), decorateToStringValue(Set("FIE", "FEE", "FUM", "FOE"))) + ", but " + Resources.wasNotEqualTo(decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        (fumList should (contain theSameElementsAs Set(" FEE ", " FIE ", " FOE ", " FUM ") and be (fumList))) (after being lowerCased and trimmed)
      }
    }

    describe("when used with (not contain theSameElementsAs xx and not contain theSameElementsAs xx)") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (not contain theSameElementsAs (Set("fee", "fie", "foe", "fuu")) and not contain theSameElementsAs (Set("fie", "fee", "fuu", "foe")))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain theSameElementsAs (Set("fee", "fie", "foe", "fum")) and not contain theSameElementsAs (Set("happy", "birthday", "to", "you")))
        }
        checkMessageStackDepth(e1, Resources.containedSameElements(decorateToStringValue(fumList), decorateToStringValue(Set("fee", "fie", "foe", "fum"))), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not contain theSameElementsAs (Set("happy", "birthday", "to", "you")) and not contain theSameElementsAs (Set("fee", "fie", "foe", "fum")))
        }
        checkMessageStackDepth(e2, Resources.didNotContainSameElements(decorateToStringValue(fumList), decorateToStringValue(Set("happy", "birthday", "to", "you"))) + ", but " + Resources.containedSameElements(decorateToStringValue(fumList), decorateToStringValue(Set("fee", "fie", "foe", "fum"))), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (not contain theSameElementsAs (Set("FIE", "FEE", "FAM", "FOE")) and not contain theSameElementsAs (Set("FIE", "FEE", "FOE", "FAM")))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain theSameElementsAs (Set("FIE", "FEE", "FUM", "FOE")) and not contain theSameElementsAs (Set("FIE", "FEE", "FOE", "FAM")))
        }
        checkMessageStackDepth(e1, Resources.containedSameElements(decorateToStringValue(fumList), decorateToStringValue(Set("FIE", "FEE", "FUM", "FOE"))), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not contain theSameElementsAs (Set("FIE", "FEE", "FAM", "FOE")) and (not contain theSameElementsAs (Set("FIE", "FEE", "FOE", "FUM"))))
        }
        checkMessageStackDepth(e2, Resources.didNotContainSameElements(decorateToStringValue(fumList), decorateToStringValue(Set("FIE", "FEE", "FAM", "FOE"))) + ", but " + Resources.containedSameElements(decorateToStringValue(fumList), decorateToStringValue(Set("FIE", "FEE", "FOE", "FUM"))), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (not contain theSameElementsAs (Set("FIE", "FEE", "FAM", "FOE")) and not contain theSameElementsAs (Set("FIE", "FEE", "FOE", "FAM")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not contain theSameElementsAs (Set("FIE", "FEE", "FAM", "FOE")) and not contain theSameElementsAs (Set("FIE", "FEE", "FOE", "FUM")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.didNotContainSameElements(decorateToStringValue(fumList), decorateToStringValue(Set("FIE", "FEE", "FAM", "FOE"))) + ", but " + Resources.containedSameElements(decorateToStringValue(fumList), decorateToStringValue(Set("FIE", "FEE", "FOE", "FUM"))), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (not contain theSameElementsAs (Set("FIE", "FEE", "FUM", "FOE")) and not contain theSameElementsAs (Set("FIE", "FEE", "FOE", "FAM")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources.containedSameElements(decorateToStringValue(fumList), decorateToStringValue(Set("FIE", "FEE", "FUM", "FOE"))), fileName, thisLineNumber - 2)
        (fumList should (contain theSameElementsAs Set(" FEE ", " FIE ", " FOE ", " FUM ") and contain theSameElementsAs Set(" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
    }

    describe("when used with (not equal (..) and not contain theSameElementsAs (..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (not equal (toList) and not contain theSameElementsAs (Set("fie", "fee", "fuu", "foe")))
        val e1 = intercept[TestFailedException] {
          fumList should (not equal (fumList) and not contain theSameElementsAs (Set("happy", "birthday", "to", "you")))
        }
        checkMessageStackDepth(e1, Resources.equaled(decorateToStringValue(fumList), decorateToStringValue(fumList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not equal (toList) and not contain theSameElementsAs (Set("fee", "fie", "foe", "fum")))
        }
        checkMessageStackDepth(e2, Resources.didNotEqual(decorateToStringValue(fumList), decorateToStringValue(toList)) + ", but " + Resources.containedSameElements(decorateToStringValue(fumList), decorateToStringValue(Set("fee", "fie", "foe", "fum"))), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (not equal (toList) and not contain theSameElementsAs (Set("FIE", "FEE", "FAM", "FOE")))
        val e1 = intercept[TestFailedException] {
          fumList should (not equal (fumList) and not contain theSameElementsAs (Set("FIE", "FEE", "FAM", "FOE")))
        }
        checkMessageStackDepth(e1, Resources.equaled(decorateToStringValue(fumList), decorateToStringValue(fumList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not equal (toList) and (not contain theSameElementsAs (Set("FIE", "FEE", "FUM", "FOE"))))
        }
        checkMessageStackDepth(e2, Resources.didNotEqual(decorateToStringValue(fumList), decorateToStringValue(toList)) + ", but " + Resources.containedSameElements(decorateToStringValue(fumList), decorateToStringValue(Set("FIE", "FEE", "FUM", "FOE"))), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (not equal (fumList) and not contain theSameElementsAs (Set("FIE", "FEE", "FAM", "FOE")))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not equal (fumList) and not contain theSameElementsAs (Set("FIE", "FEE", "FUM", "FOE")))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.didNotEqual(decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", but " + Resources.containedSameElements(decorateToStringValue(fumList), decorateToStringValue(Set("FIE", "FEE", "FUM", "FOE"))), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (not equal (toList) and not contain theSameElementsAs (Set("FIE", "FEE", "FAM", "FOE")))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources.equaled(decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        (fumList should (not contain theSameElementsAs (Set(" FEE ", " FIE ", " FOE ", " FUU ")) and not contain theSameElementsAs (Set(" FEE ", " FIE ", " FOE ", " FUU ")))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
    }

    describe("when used with (not be (..) and not contain theSameElementsAs (..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (not be (toList) and not contain theSameElementsAs (Set("fie", "fee", "fuu", "foe")))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) and not contain theSameElementsAs (Set("happy", "birthday", "to", "you")))
        }
        checkMessageStackDepth(e1, Resources.wasEqualTo(decorateToStringValue(fumList), decorateToStringValue(fumList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not be (toList) and not contain theSameElementsAs (Set("fee", "fie", "foe", "fum")))
        }
        checkMessageStackDepth(e2, Resources.wasNotEqualTo(decorateToStringValue(fumList), decorateToStringValue(toList)) + ", but " + Resources.containedSameElements(decorateToStringValue(fumList), decorateToStringValue(Set("fee", "fie", "foe", "fum"))), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (not be (toList) and not contain theSameElementsAs (Set("FIE", "FEE", "FAM", "FOE")))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) and not contain theSameElementsAs (Set("FIE", "FEE", "FAM", "FOE")))
        }
        checkMessageStackDepth(e1, Resources.wasEqualTo(decorateToStringValue(fumList), decorateToStringValue(fumList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not be (toList) and (not contain theSameElementsAs (Set("FIE", "FEE", "FUM", "FOE"))))
        }
        checkMessageStackDepth(e2, Resources.wasNotEqualTo(decorateToStringValue(fumList), decorateToStringValue(toList)) + ", but " + Resources.containedSameElements(decorateToStringValue(fumList), decorateToStringValue(Set("FIE", "FEE", "FUM", "FOE"))), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (not be (toList) and not contain theSameElementsAs (Set("FIE", "FEE", "FAM", "FOE")))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not be (toList) and not contain theSameElementsAs (Set("FIE", "FEE", "FUM", "FOE")))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.wasNotEqualTo(decorateToStringValue(fumList), decorateToStringValue(toList)) + ", but " + Resources.containedSameElements(decorateToStringValue(fumList), decorateToStringValue(Set("FIE", "FEE", "FUM", "FOE"))), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (not be (fumList) and not contain theSameElementsAs (Set("FIE", "FEE", "FAM", "FOE")))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources.wasEqualTo(decorateToStringValue(fumList), decorateToStringValue(fumList)), fileName, thisLineNumber - 2)
        (fumList should (not contain theSameElementsAs (Set(" FEE ", " FIE ", " FOE ", " FUU ")) and not contain theSameElementsAs (Set(" FEE ", " FIE ", " FOE ", " FUU ")))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
    }

  }

  describe("every of Everys") {

    val list1s: Every[Every[Int]] = Every(Every(3, 2, 1), Every(3, 2, 1), Every(3, 2, 1))
    val lists: Every[Every[Int]] = Every(Every(3, 2, 1), Every(3, 2, 1), Every(4, 3, 2))
    val hiLists: Every[Every[String]] = Every(Every("hi", "hello"), Every("hi", "hello"), Every("hi", "hello"))

    def allErrMsg(index: Int, message: String, lineNumber: Int, left: Any): String =
      "'all' inspection failed, because: \n" +
        "  at index " + index + ", " + message + " (" + fileName + ":" + (lineNumber) + ") \n" +
        "in " + decorateToStringValue(left)

    describe("used with contain theSameElementsAs xx and contain theSameElementsAs xx") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (contain theSameElementsAs Set(3, 2, 1) and contain theSameElementsAs Set(1, 3, 2))
        atLeast (2, lists) should (contain theSameElementsAs Set(3, 1, 2) and contain theSameElementsAs Set(2, 3, 1))
        atMost (2, lists) should (contain theSameElementsAs Set(3, 1, 2) and contain theSameElementsAs Set(2, 3, 1))
        no (lists) should (contain theSameElementsAs Set(3, 6, 9) and contain theSameElementsAs Set(3, 4, 5))

        val e1 = intercept[TestFailedException] {
          all (lists) should (contain theSameElementsAs Set(1, 2, 3) and contain theSameElementsAs Set(1, 3, 2))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(Many(4, 3, 2)) + " did not contain the same elements as " + decorateToStringValue(Set(1, 2, 3)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (list1s) should (contain theSameElementsAs Set(1, 2, 3) and contain theSameElementsAs Set(1, 3, 4))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(Many(3, 2, 1)) + " contained the same elements as " + decorateToStringValue(Set(1, 2, 3)) + ", but " + decorateToStringValue(Many(3, 2, 1)) + " did not contain the same elements as " + decorateToStringValue(Set(1, 3, 4)), thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)

        val e3 = intercept[TestFailedException] {
          all (hiLists) should (contain theSameElementsAs Set("hi", "hello") and contain theSameElementsAs Set("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e3, allErrMsg(0, decorateToStringValue(Many("hi", "hello")) + " contained the same elements as " + decorateToStringValue(Set("hi", "hello")) + ", but " + decorateToStringValue(Many("hi", "hello")) + " did not contain the same elements as " + decorateToStringValue(Set("ho", "hey", "howdy")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (contain theSameElementsAs Set("HELLO", "HI") and contain theSameElementsAs Set("HI", "HELLO"))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (contain theSameElementsAs Set("HO", "HELLO") and contain theSameElementsAs Set("HI", "HELLO"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(Many("hi", "hello")) + " did not contain the same elements as " + decorateToStringValue(Set("HO", "HELLO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (hiLists) should (contain theSameElementsAs Set("HELLO", "HI") and contain theSameElementsAs Set("HO", "HELLO"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(Many("hi", "hello")) + " contained the same elements as " + decorateToStringValue(Set("HELLO", "HI")) + ", but " + decorateToStringValue(Many("hi", "hello")) + " did not contain the same elements as " + decorateToStringValue(Set("HO", "HELLO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (all (hiLists) should (contain theSameElementsAs Set("HELLO", "HI") and contain theSameElementsAs Set("HI", "HELLO"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (contain theSameElementsAs Set("HO", "HELLO") and contain theSameElementsAs Set("HI", "HELLO"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(Many("hi", "hello")) + " did not contain the same elements as " + decorateToStringValue(Set("HO", "HELLO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (contain theSameElementsAs Set("HELLO", "HI") and contain theSameElementsAs Set("HO", "HELLO"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(Many("hi", "hello")) + " contained the same elements as " + decorateToStringValue(Set("HELLO", "HI")) + ", but " + decorateToStringValue(Many("hi", "hello")) + " did not contain the same elements as " + decorateToStringValue(Set("HO", "HELLO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
    }

    describe("when used with (be (..) and contain theSameElementsAs (..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (be (Many(3, 2, 1)) and contain theSameElementsAs Set(1, 3, 2))
        atLeast (2, lists) should (be (Many(3, 2, 1)) and contain theSameElementsAs Set(1, 3, 2))
        atMost (2, lists) should (be (Many(3, 2, 1)) and contain theSameElementsAs Set(2, 3, 1))
        no (lists) should (be (Many(3, 6, 9)) and contain theSameElementsAs Set(3, 4, 5))

        val e1 = intercept[TestFailedException] {
          all (lists) should (be (Many(3, 2, 1)) and contain theSameElementsAs Set(1, 3, 2))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(Many(4, 3, 2)) + " was not equal to " + decorateToStringValue(Many(3, 2, 1)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (list1s) should (be (Many(3, 2, 1)) and contain theSameElementsAs Set(2, 3, 8))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(Many(3, 2, 1)) + " was equal to " + decorateToStringValue(Many(3, 2, 1)) + ", but " + decorateToStringValue(Many(3, 2, 1)) + " did not contain the same elements as " + decorateToStringValue(Set(2, 3, 8)), thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)

        val e3 = intercept[TestFailedException] {
          all (hiLists) should (be (Many("hi", "hello")) and contain theSameElementsAs Set("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e3, allErrMsg(0, decorateToStringValue(Many("hi", "hello")) + " was equal to " + decorateToStringValue(Many("hi", "hello")) + ", but " + decorateToStringValue(Many("hi", "hello")) + " did not contain the same elements as " + decorateToStringValue(Set("ho", "hey", "howdy")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e4 = intercept[TestFailedException] {
          all (list1s) should (be (Many(3, 2, 1)) and contain theSameElementsAs Set(2, 3, 8))
        }
        checkMessageStackDepth(e4, allErrMsg(0, decorateToStringValue(Many(3, 2, 1)) + " was equal to " + decorateToStringValue(Many(3, 2, 1)) + ", but " + decorateToStringValue(Many(3, 2, 1)) + " did not contain the same elements as " + decorateToStringValue(Set(2, 3, 8)), thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (be (Many("hi", "hello")) and contain theSameElementsAs Set("HELLO", "HI"))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (be (Many("HI", "HELLO")) and contain theSameElementsAs Set("HELLO", "HI"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(Many("hi", "hello")) + " was not equal to " + decorateToStringValue(Many("HI", "HELLO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (hiLists) should (be (Many("hi", "hello")) and contain theSameElementsAs Set("HO", "HELLO"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(Many("hi", "hello")) + " was equal to " + decorateToStringValue(Many("hi", "hello")) + ", but " + decorateToStringValue(Many("hi", "hello")) + " did not contain the same elements as " + decorateToStringValue(Set("HO", "HELLO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (all (hiLists) should (be (Many("hi", "hello")) and contain theSameElementsAs Set("HELLO", "HI"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (be (Many("HI", "HELLO")) and contain theSameElementsAs Set("HELLO", "HI"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(Many("hi", "hello")) + " was not equal to " + decorateToStringValue(Many("HI", "HELLO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (be (Many("hi", "hello")) and contain theSameElementsAs Set("HO", "HELLO"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(Many("hi", "hello")) + " was equal to " + decorateToStringValue(Many("hi", "hello")) + ", but " + decorateToStringValue(Many("hi", "hello")) + " did not contain the same elements as " + decorateToStringValue(Set("HO", "HELLO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
    }

    describe("when used with (not contain theSameElementsAs xx and not contain theSameElementsAs xx)") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (not contain theSameElementsAs (Set(3, 2, 8)) and not contain theSameElementsAs (Set(8, 3, 4)))
        atLeast (2, lists) should (not contain theSameElementsAs (Set(3, 8, 5)) and not contain theSameElementsAs (Set(8, 3, 4)))
        atMost (2, lists) should (not contain theSameElementsAs (Set(2, 4, 3)) and contain theSameElementsAs (Set(4, 3, 2)))
        no (list1s) should (not contain theSameElementsAs (Set(1, 2, 3)) and not contain theSameElementsAs (Set(1, 3, 2)))

        val e1 = intercept[TestFailedException] {
          all (lists) should (not contain theSameElementsAs (Set(2, 3, 4)) and not contain theSameElementsAs (Set(8, 3, 4)))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(Many(4, 3, 2)) + " contained the same elements as " + decorateToStringValue(Set(2, 3, 4)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (lists) should (not contain theSameElementsAs (Set(3, 6, 8)) and not contain theSameElementsAs (Set(2, 3, 4)))
        }
        checkMessageStackDepth(e2, allErrMsg(2, decorateToStringValue(Many(4, 3, 2)) + " did not contain the same elements as " + decorateToStringValue(Set(3, 6, 8)) + ", but " + decorateToStringValue(Many(4, 3, 2)) + " contained the same elements as " + decorateToStringValue(Set(2, 3, 4)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e3 = intercept[TestFailedException] {
          all (hiLists) should (not contain theSameElementsAs (Set("hello", "hi")) and not contain theSameElementsAs (Set("ho", "hey", "howdy")))
        }
        checkMessageStackDepth(e3, allErrMsg(0, decorateToStringValue(Many("hi", "hello")) + " contained the same elements as " + decorateToStringValue(Set("hello", "hi")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e4 = intercept[TestFailedException] {
          all (hiLists) should (not contain theSameElementsAs (Set("ho", "hey", "howdy")) and not contain theSameElementsAs (Set("hello", "hi")))
        }
        checkMessageStackDepth(e4, allErrMsg(0, decorateToStringValue(Many("hi", "hello")) + " did not contain the same elements as " + decorateToStringValue(Set("ho", "hey", "howdy")) + ", but " + decorateToStringValue(Many("hi", "hello")) + " contained the same elements as " + decorateToStringValue(Set("hello", "hi")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (not contain theSameElementsAs (Set("HI")) and not contain theSameElementsAs (Set("HO")))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not contain theSameElementsAs (Set("HELLO", "HI")) and not contain theSameElementsAs (Set("HO")))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(Many("hi", "hello")) + " contained the same elements as " + decorateToStringValue(Set("HELLO", "HI")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (hiLists) should (not contain theSameElementsAs (Set("HI")) and not contain theSameElementsAs (Set("HELLO", "HI")))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(Many("hi", "hello")) + " did not contain the same elements as " + decorateToStringValue(Set("HI")) + ", but " + decorateToStringValue(Many("hi", "hello")) + " contained the same elements as " + decorateToStringValue(Set("HELLO", "HI")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (all (hiLists) should (not contain theSameElementsAs (Set("HI")) and not contain theSameElementsAs (Set("HO")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not contain theSameElementsAs (Set("HELLO", "HI")) and not contain theSameElementsAs (Set("HO")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(Many("hi", "hello")) + " contained the same elements as " + decorateToStringValue(Set("HELLO", "HI")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (not contain theSameElementsAs (Set("HI")) and not contain theSameElementsAs (Set("HELLO", "HI")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(Many("hi", "hello")) + " did not contain the same elements as " + decorateToStringValue(Set("HI")) + ", but " + decorateToStringValue(Many("hi", "hello")) + " contained the same elements as " + decorateToStringValue(Set("HELLO", "HI")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
    }

    describe("when used with (not be (..) and not contain theSameElementsAs (..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (not be (One(2)) and not contain theSameElementsAs (Set(8, 3, 4)))
        atLeast (2, lists) should (not be (One(3)) and not contain theSameElementsAs (Set(8, 3, 4)))
        atMost (2, lists) should (not be (Many(4, 3, 2)) and not contain theSameElementsAs (Set(3, 4, 2)))
        no (list1s) should (not be (Many(3, 2, 1)) and not contain theSameElementsAs (Set(1, 2, 3)))

        val e1 = intercept[TestFailedException] {
          all (lists) should (not be (Many(4, 3, 2)) and not contain theSameElementsAs (Set(8, 3, 4)))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(Many(4, 3, 2)) + " was equal to " + decorateToStringValue(Many(4, 3, 2)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (lists) should (not be (One(3)) and not contain theSameElementsAs (Set(2, 3, 4)))
        }
        checkMessageStackDepth(e2, allErrMsg(2, decorateToStringValue(Many(4, 3, 2)) + " was not equal to " + decorateToStringValue(One(3)) + ", but " + decorateToStringValue(Many(4, 3, 2)) + " contained the same elements as " + decorateToStringValue(Set(2, 3, 4)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e3 = intercept[TestFailedException] {
          all (hiLists) should (not be (Many("hi", "hello")) and not contain theSameElementsAs (Set("ho", "hey", "howdy")))
        }
        checkMessageStackDepth(e3, allErrMsg(0, decorateToStringValue(Many("hi", "hello")) + " was equal to " + decorateToStringValue(Many("hi", "hello")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e4 = intercept[TestFailedException] {
          all (hiLists) should (not be (One("ho")) and not contain theSameElementsAs (Set("hello", "hi")))
        }
        checkMessageStackDepth(e4, allErrMsg(0, decorateToStringValue(Many("hi", "hello")) + " was not equal to " + decorateToStringValue(One("ho")) + ", but " + decorateToStringValue(Many("hi", "hello")) + " contained the same elements as " + decorateToStringValue(Set("hello", "hi")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (not be (One("ho")) and not contain theSameElementsAs (Set("HO", "HELLO")))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not be (Many("hi", "hello")) and not contain theSameElementsAs (Set("HELLO", "HI")))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(Many("hi", "hello")) + " was equal to " + decorateToStringValue(Many("hi", "hello")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (hiLists) should (not be (One("ho")) and not contain theSameElementsAs (Set("HI", "HELLO")))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(Many("hi", "hello")) + " was not equal to " + decorateToStringValue(One("ho")) + ", but " + decorateToStringValue(Many("hi", "hello")) + " contained the same elements as " + decorateToStringValue(Set("HI", "HELLO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (all (hiLists) should (not be (One("ho")) and not contain theSameElementsAs (Set("HO", "HELLO")))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not be (Many("hi", "hello")) and not contain theSameElementsAs (Set("HELLO", "HI")))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(Many("hi", "hello")) + " was equal to " + decorateToStringValue(Many("hi", "hello")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (not be (One("ho")) and not contain theSameElementsAs (Set("HI", "HELLO")))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(Many("hi", "hello")) + " was not equal to " + decorateToStringValue(One("ho")) + ", but " + decorateToStringValue(Many("hi", "hello")) + " contained the same elements as " + decorateToStringValue(Set("HI", "HELLO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
    }
  }
}
