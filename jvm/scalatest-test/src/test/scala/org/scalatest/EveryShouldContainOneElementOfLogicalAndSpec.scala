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

class EveryShouldContainOneElementOfLogicalAndSpec extends AnyFunSpec {

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

  val fileName: String = "EveryShouldContainOneElementOfLogicalAndSpec.scala"

  describe("an Every") {

    val fumList: Every[String] = Every("fum")
    val toList: Every[String] = Every("to")

    describe("when used with (contain oneElementOf (..) and contain oneElementOf (..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (contain oneElementOf Seq("fee", "fie", "foe", "fum") and contain oneElementOf Seq("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain oneElementOf Seq("happy", "birthday", "to", "you") and contain oneElementOf Seq("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, FailureMessages.didNotContainOneElementOf(prettifier, fumList, Seq("happy", "birthday", "to", "you")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (contain oneElementOf Seq("fee", "fie", "foe", "fum") and contain oneElementOf Seq("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e2, FailureMessages.containedOneElementOf(prettifier, fumList, Seq("fee", "fie", "foe", "fum")) + ", but " + FailureMessages.didNotContainOneElementOf(prettifier, fumList, Seq("happy", "birthday", "to", "you")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (contain oneElementOf Seq("FEE", "FIE", "FOE", "FUM") and contain oneElementOf Seq("FIE", "FEE", "FUM", "FOE"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain oneElementOf Seq("fee", "fie", "foe", "fum") and contain oneElementOf Seq("FEE", "FIE", "FOE", "FUM"))
        }
        checkMessageStackDepth(e1, FailureMessages.didNotContainOneElementOf(prettifier, fumList, Seq("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (contain oneElementOf Seq("FEE", "FIE", "FOE", "FUM") and (contain oneElementOf Seq("fee", "fie", "foe", "fum")))
        }
        checkMessageStackDepth(e2, FailureMessages.containedOneElementOf(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FUM")) + ", but " + FailureMessages.didNotContainOneElementOf(prettifier, fumList, Seq("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (contain oneElementOf Seq("FEE", "FIE", "FOE", "FUM") and contain oneElementOf Seq("FIE", "FEE", "FUM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain oneElementOf Seq("fee", "fie", "foe", "fum") and contain oneElementOf Seq("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.didNotContainOneElementOf(prettifier, fumList, Seq("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (contain oneElementOf Seq("FEE", "FIE", "FOE", "FUM") and contain oneElementOf Seq("fee", "fie", "foe", "fum"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, FailureMessages.containedOneElementOf(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FUM")) + ", but " + FailureMessages.didNotContainOneElementOf(prettifier, fumList, Seq("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)
        (fumList should (contain oneElementOf Seq(" FEE ", " FIE ", " FOE ", " FUM ") and contain oneElementOf Seq(" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }

      it("should allow RHS to contain duplicated value") {
        fumList should (contain oneElementOf Seq("fee", "fie", "foe", "fie", "fum") and contain oneElementOf Seq("fie", "fee", "fum", "foe"))
        fumList should (contain oneElementOf Seq("fie", "fee", "fum", "foe") and contain oneElementOf Seq("fee", "fie", "foe", "fie", "fum"))
      }
    }

    describe("when used with (equal (..) and contain oneElementOf (..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (equal (fumList) and contain oneElementOf Seq("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) and contain oneElementOf Seq("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, FailureMessages.didNotEqual(prettifier, fumList, toList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (equal (fumList) and contain oneElementOf Seq("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e2, FailureMessages.equaled(prettifier, fumList, fumList) + ", but " + FailureMessages.didNotContainOneElementOf(prettifier, fumList, Seq("happy", "birthday", "to", "you")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (equal (fumList) and contain oneElementOf Seq("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) and contain oneElementOf Seq("FEE", "FIE", "FOE", "FUM"))
        }
        checkMessageStackDepth(e1, FailureMessages.didNotEqual(prettifier, fumList, toList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (equal (fumList) and (contain oneElementOf Seq("fee", "fie", "foe", "fum")))
        }
        checkMessageStackDepth(e2, FailureMessages.equaled(prettifier, fumList, fumList) + ", but " + FailureMessages.didNotContainOneElementOf(prettifier, fumList, Seq("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (equal (toList) and contain oneElementOf Seq("FEE", "FIE", "FOE", "FUM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (equal (fumList) and contain oneElementOf Seq("FEE", "FIE", "FOE", "FUM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.didNotEqual(prettifier, fumList, fumList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (equal (toList) and contain oneElementOf Seq("fee", "fie", "foe", "fum"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, FailureMessages.equaled(prettifier, fumList, toList) + ", but " + FailureMessages.didNotContainOneElementOf(prettifier, fumList, Seq("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)
        (fumList should (equal (toList) and contain oneElementOf Seq(" FEE ", " FIE ", " FOE ", " FUM "))) (decided by invertedListOfStringEquality, after being lowerCased and trimmed)
      }

      it("should allow RHS to contain duplicated value") {
        fumList should (equal (fumList) and contain oneElementOf Seq("fee", "fie", "foe", "fie", "fum"))
      }
    }

    describe("when used with (be (..) and contain oneElementOf (..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (be (fumList) and contain oneElementOf Seq("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (be (toList) and contain oneElementOf Seq("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, FailureMessages.wasNotEqualTo(prettifier, fumList, toList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (be (fumList) and contain oneElementOf Seq("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e2, FailureMessages.wasEqualTo(prettifier, fumList, fumList) + ", but " + FailureMessages.didNotContainOneElementOf(prettifier, fumList, Seq("happy", "birthday", "to", "you")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (be (fumList) and contain oneElementOf Seq("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (be (toList) and contain oneElementOf Seq("FEE", "FIE", "FOE", "FUM"))
        }
        checkMessageStackDepth(e1, FailureMessages.wasNotEqualTo(prettifier, fumList, toList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (be (fumList) and (contain oneElementOf Seq("fee", "fie", "foe", "fum")))
        }
        checkMessageStackDepth(e2, FailureMessages.wasEqualTo(prettifier, fumList, fumList) + ", but " + FailureMessages.didNotContainOneElementOf(prettifier, fumList, Seq("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (be (fumList) and contain oneElementOf Seq("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (be (fumList) and contain oneElementOf Seq("fee", "fie", "foe", "fum"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.wasEqualTo(prettifier, fumList, fumList) + ", but " + FailureMessages.didNotContainOneElementOf(prettifier, fumList, Seq("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (be (toList) and contain oneElementOf Seq("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, FailureMessages.wasNotEqualTo(prettifier, fumList, toList), fileName, thisLineNumber - 2)
        (fumList should (be (fumList) and contain oneElementOf Seq(" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed)
      }

      it("should allow RHS to contain duplicated value") {
        fumList should (be (fumList) and contain oneElementOf Seq("fee", "fie", "foe", "fie", "fum"))
      }
    }

    describe("when used with (contain oneElementOf (..) and be (..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (contain oneElementOf Seq("fie", "fee", "fum", "foe") and be (fumList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain oneElementOf Seq("fee", "fie", "foe", "fum") and be (toList))
        }
        checkMessageStackDepth(e1, FailureMessages.containedOneElementOf(prettifier, fumList, Seq("fee", "fie", "foe", "fum")) + ", but " + FailureMessages.wasNotEqualTo(prettifier, fumList, toList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (contain oneElementOf Seq("FEE", "FIE", "FOE", "FUM") and be (fumList))
        }
        checkMessageStackDepth(e2, FailureMessages.didNotContainOneElementOf(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (contain oneElementOf Seq("FEE", "FIE", "FOE", "FUM") and be (fumList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain oneElementOf Seq("fie", "fee", "fum", "foe") and be (toList))
        }
        checkMessageStackDepth(e1, FailureMessages.didNotContainOneElementOf(prettifier, fumList, Seq("fie", "fee", "fum", "foe")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (contain oneElementOf Seq("fie", "fee", "fum", "foe") and (be (fumList)))
        }
        checkMessageStackDepth(e2, FailureMessages.didNotContainOneElementOf(prettifier, fumList, Seq("fie", "fee", "fum", "foe")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (contain oneElementOf Seq("FEE", "FIE", "FOE", "FUM") and be (fumList))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain oneElementOf Seq("fie", "fee", "fum", "foe") and be (fumList))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.didNotContainOneElementOf(prettifier, fumList, Seq("fie", "fee", "fum", "foe")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (contain oneElementOf Seq("FEE", "FIE", "FOE", "FUM") and be (toList))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, FailureMessages.containedOneElementOf(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FUM")) + ", but " + FailureMessages.wasNotEqualTo(prettifier, fumList, toList), fileName, thisLineNumber - 2)
        (fumList should (contain oneElementOf Seq(" FEE ", " FIE ", " FOE ", " FUM ") and be (fumList))) (after being lowerCased and trimmed)
      }

      it("should allow RHS to contain duplicated value") {
        fumList should (contain oneElementOf Seq("fee", "fie", "foe", "fie", "fum") and be (fumList))
      }
    }

    describe("when used with (not contain oneElementOf (..) and not contain oneElementOf (..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (not contain oneElementOf (Seq("FEE", "FIE", "FOE", "FUM")) and not contain oneElementOf (Seq("FEE", "FIE", "FOE", "FUM")))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain oneElementOf (Seq("fee", "fie", "foe", "fum")) and not contain oneElementOf (Seq("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e1, FailureMessages.containedOneElementOf(prettifier, fumList, Seq("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not contain oneElementOf (Seq("FEE", "FIE", "FOE", "FUM")) and not contain oneElementOf (Seq("fee", "fie", "foe", "fum")))
        }
        checkMessageStackDepth(e2, FailureMessages.didNotContainOneElementOf(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FUM")) + ", but " + FailureMessages.containedOneElementOf(prettifier, fumList, Seq("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (not contain oneElementOf (Seq("fee", "fie", "foe", "fum")) and not contain oneElementOf (Seq("fee", "fie", "foe", "fum")))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain oneElementOf (Seq("FEE", "FIE", "FOE", "FUM")) and not contain oneElementOf (Seq("fee", "fie", "foe", "fum")))
        }

        val e2 = intercept[TestFailedException] {
          fumList should (not contain oneElementOf (Seq("fee", "fie", "foe", "fum")) and (not contain oneElementOf (Seq("FEE", "FIE", "FOE", "FUM"))))
        }
        checkMessageStackDepth(e2, FailureMessages.didNotContainOneElementOf(prettifier, fumList, Seq("fee", "fie", "foe", "fum")) + ", but " + FailureMessages.containedOneElementOf(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (not contain oneElementOf (Seq("fee", "fie", "foe", "fum")) and not contain oneElementOf (Seq("fee", "fie", "foe", "fum")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not contain oneElementOf (Seq("FEE", "FIE", "FOE", "FUM")) and not contain oneElementOf (Seq("fee", "fie", "foe", "fum")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.containedOneElementOf(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (not contain oneElementOf (Seq("fee", "fie", "foe", "fum")) and not contain oneElementOf (Seq("FEE", "FIE", "FOE", "FUM")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, FailureMessages.didNotContainOneElementOf(prettifier, fumList, Seq("fee", "fie", "foe", "fum")) + ", but " + FailureMessages.containedOneElementOf(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
        (fumList should (contain oneElementOf Seq(" FEE ", " FIE ", " FOE ", " FUM ") and contain oneElementOf Seq(" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }

      it("should allow RHS to contain duplicated value") {
        fumList should (not contain oneElementOf (Seq("fee", "fie", "foe", "fie", "fam")) and not contain oneElementOf (Seq("FEE", "FIE", "FOE", "FUM")))
        fumList should (not contain oneElementOf (Seq("FEE", "FIE", "FOE", "FUM")) and not contain oneElementOf (Seq("fee", "fie", "foe", "fie", "fam")))
      }
    }

    describe("when used with (not equal (..) and not contain oneElementOf (..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (not equal (toList) and not contain oneElementOf (Seq("FEE", "FIE", "FOE", "FUM")))
        val e1 = intercept[TestFailedException] {
          fumList should (not equal (fumList) and not contain oneElementOf (Seq("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e1, FailureMessages.equaled(prettifier, fumList, fumList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not equal (toList) and not contain oneElementOf (Seq("fee", "fie", "foe", "fum")))
        }
        checkMessageStackDepth(e2, FailureMessages.didNotEqual(prettifier, fumList, toList) + ", but " + FailureMessages.containedOneElementOf(prettifier, fumList, Seq("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (not equal (toList) and not contain oneElementOf (Seq("fee", "fie", "foe", "fum")))
        val e1 = intercept[TestFailedException] {
          fumList should (not equal (fumList) and not contain oneElementOf (Seq("fee", "fie", "foe", "fum")))
        }
        checkMessageStackDepth(e1, FailureMessages.equaled(prettifier, fumList, fumList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not equal (toList) and (not contain oneElementOf (Seq("FEE", "FIE", "FOE", "FUM"))))
        }
        checkMessageStackDepth(e2, FailureMessages.didNotEqual(prettifier, fumList, toList) + ", but " + FailureMessages.containedOneElementOf(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (not equal (fumList) and not contain oneElementOf (Seq("fee", "fie", "foe", "fum")))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not equal (fumList) and not contain oneElementOf (Seq("FEE", "FIE", "FOE", "FUM")))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.didNotEqual(prettifier, fumList, fumList) + ", but " + FailureMessages.containedOneElementOf(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (not equal (toList) and not contain oneElementOf (Seq("fee", "fie", "foe", "fum")))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, FailureMessages.equaled(prettifier, fumList, toList), fileName, thisLineNumber - 2)
        (fumList should (not contain oneElementOf (Seq(" FEE ", " FIE ", " FOE ", " FUU ")) and not contain oneElementOf (Seq(" FEE ", " FIE ", " FOE ", " FUU ")))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }

      it("should allow RHS to contain duplicated value") {
        fumList should (not equal (toList) and not contain oneElementOf (Seq("fee", "fie", "foe", "fie", "fam")))
      }
    }

    describe("when used with (not be (..) and not contain oneElementOf (..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (not be (toList) and not contain oneElementOf (Seq("FEE", "FIE", "FOE", "FUM")))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) and not contain oneElementOf (Seq("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e1, FailureMessages.wasEqualTo(prettifier, fumList, fumList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not be (toList) and not contain oneElementOf (Seq("fee", "fie", "foe", "fum")))
        }
        checkMessageStackDepth(e2, FailureMessages.wasNotEqualTo(prettifier, fumList, toList) + ", but " + FailureMessages.containedOneElementOf(prettifier, fumList, Seq("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (not be (toList) and not contain oneElementOf (Seq("fee", "fie", "foe", "fum")))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) and not contain oneElementOf (Seq("fee", "fie", "foe", "fum")))
        }
        checkMessageStackDepth(e1, FailureMessages.wasEqualTo(prettifier, fumList, fumList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not be (toList) and (not contain oneElementOf (Seq("FEE", "FIE", "FOE", "FUM"))))
        }
        checkMessageStackDepth(e2, FailureMessages.wasNotEqualTo(prettifier, fumList, toList) + ", but " + FailureMessages.containedOneElementOf(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (not be (toList) and not contain oneElementOf (Seq("fee", "fie", "foe", "fum")))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not be (toList) and not contain oneElementOf (Seq("FEE", "FIE", "FOE", "FUM")))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.wasNotEqualTo(prettifier, fumList, toList) + ", but " + FailureMessages.containedOneElementOf(prettifier, fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (not be (fumList) and not contain oneElementOf (Seq("fee", "fie", "foe", "fum")))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, FailureMessages.wasEqualTo(prettifier, fumList, fumList), fileName, thisLineNumber - 2)
        (fumList should (not contain oneElementOf (Seq(" FEE ", " FIE ", " FOE ", " FUU ")) and not contain oneElementOf (Seq(" FEE ", " FIE ", " FOE ", " FUU ")))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }

      it("should allow RHS to contain duplicated value") {
        fumList should (not be (toList) and not contain oneElementOf (Seq("fee", "fie", "foe", "fie", "fam")))
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

    describe("when used with (contain oneElementOf (..) and contain oneElementOf (..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (contain oneElementOf Seq(3, 2, 1) and contain oneElementOf Seq(1, 3, 4))
        atLeast (2, lists) should (contain oneElementOf Seq(3, 1, 5) and contain oneElementOf Seq(1, 3, 4))
        atMost (2, lists) should (contain oneElementOf Seq(3, 2, 8) and contain oneElementOf Seq(2, 3, 4))
        no (lists) should (contain oneElementOf Seq(3, 6, 9) and contain oneElementOf Seq(3, 4, 5))

        val e1 = intercept[TestFailedException] {
          all (lists) should (contain oneElementOf Seq(1, 6, 8) and contain oneElementOf Seq(1, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(2, FailureMessages.didNotContainOneElementOf(prettifier, lists(2), Seq(1, 6, 8)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (lists) should (contain oneElementOf Seq(1, 2, 8) and contain oneElementOf Seq(1, 3, 4))
        }
        checkMessageStackDepth(e2, allErrMsg(2, FailureMessages.containedOneElementOf(prettifier, lists(2), Seq(1, 2, 8)) + ", but " + FailureMessages.didNotContainOneElementOf(prettifier, lists(2), Seq(1, 3, 4)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e3 = intercept[TestFailedException] {
          all (hiLists) should (contain oneElementOf Seq("hi", "hello") and contain oneElementOf Seq("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e3, allErrMsg(0, FailureMessages.containedOneElementOf(prettifier, hiLists(0), Seq("hi", "hello")) + ", but " + FailureMessages.didNotContainOneElementOf(prettifier, hiLists(0), Seq("ho", "hey", "howdy")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e4 = intercept[TestFailedException] {
          all (lists) should (contain oneElementOf Seq(1, 2, 8) and contain oneElementOf Seq(1, 3, 4))
        }
        checkMessageStackDepth(e4, allErrMsg(2, FailureMessages.containedOneElementOf(prettifier, lists(2), Seq(1, 2, 8)) + ", but " + FailureMessages.didNotContainOneElementOf(prettifier, lists(2), Seq(1, 3, 4)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (contain oneElementOf Seq("HI", "HE") and contain oneElementOf Seq("HI", "HE"))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (contain oneElementOf Seq("hi", "he") and contain oneElementOf Seq("HI", "HE"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, FailureMessages.didNotContainOneElementOf(prettifier, hiLists(0), Seq("hi", "he")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (hiLists) should (contain oneElementOf Seq("HI", "HE") and contain oneElementOf Seq("hi", "he"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, FailureMessages.containedOneElementOf(prettifier, hiLists(0), Seq("HI", "HE")) + ", but " + FailureMessages.didNotContainOneElementOf(prettifier, hiLists(0), Seq("hi", "he")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (all (hiLists) should (contain oneElementOf Seq("HI", "HE") and contain oneElementOf Seq("HI", "HE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (contain oneElementOf Seq("hi", "he") and contain oneElementOf Seq("HI", "HE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, FailureMessages.didNotContainOneElementOf(prettifier, hiLists(0), Seq("hi", "he")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (contain oneElementOf Seq("HI", "HE") and contain oneElementOf Seq("hi", "he"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, FailureMessages.containedOneElementOf(prettifier, hiLists(0), Seq("HI", "HE")) + ", but " + FailureMessages.didNotContainOneElementOf(prettifier, hiLists(0), Seq("hi", "he")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should allow RHS to contain duplicated value") {
        all (list1s) should (contain oneElementOf Seq(3, 2, 2, 1) and contain oneElementOf Seq(1, 3, 4))
        all (list1s) should (contain oneElementOf Seq(1, 3, 4) and contain oneElementOf Seq(3, 2, 2, 1))
      }
    }

    describe("when used with (be (..) and contain oneElementOf (..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (be (One(1)) and contain oneElementOf Seq(1, 3, 4))
        atLeast (2, lists) should (be (One(1)) and contain oneElementOf Seq(1, 3, 4))
        atMost (2, lists) should (be (One(1)) and contain oneElementOf Seq(2, 3, 4))
        no (lists) should (be (One(8)) and contain oneElementOf Seq(3, 4, 5))

        val e1 = intercept[TestFailedException] {
          all (lists) should (be (One(1)) and contain oneElementOf Seq(1, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(prettifier, One(2)) + " was not equal to " + decorateToStringValue(prettifier, One(1)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (list1s) should (be (One(1)) and contain oneElementOf Seq(2, 3, 8))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, One(1)) + " was equal to " + decorateToStringValue(prettifier, One(1)) + ", but " + FailureMessages.didNotContainOneElementOf(prettifier, list1s(0), Seq(2, 3, 8)), thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)

        val e3 = intercept[TestFailedException] {
          all (hiLists) should (be (One("hi")) and contain oneElementOf Seq("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e3, allErrMsg(0, decorateToStringValue(prettifier, One("hi")) + " was equal to " + decorateToStringValue(prettifier, One("hi")) + ", but " + FailureMessages.didNotContainOneElementOf(prettifier, hiLists(0), Seq("ho", "hey", "howdy")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e4 = intercept[TestFailedException] {
          all (list1s) should (be (One(1)) and contain oneElementOf Seq(2, 3, 8))
        }
        checkMessageStackDepth(e4, allErrMsg(0, decorateToStringValue(prettifier, One(1)) + " was equal to " + decorateToStringValue(prettifier, One(1)) + ", but " + FailureMessages.didNotContainOneElementOf(prettifier, list1s(0), Seq(2, 3, 8)), thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (be (One("hi")) and contain oneElementOf Seq("HI", "HE"))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (be (One("ho")) and contain oneElementOf Seq("hi", "he"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, One("hi")) + " was not equal to " + decorateToStringValue(prettifier, One("ho")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (hiLists) should (be (One("hi")) and contain oneElementOf Seq("hi", "he"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, One("hi")) + " was equal to " + decorateToStringValue(prettifier, One("hi")) + ", but " + FailureMessages.didNotContainOneElementOf(prettifier, hiLists(0), Seq("hi", "he")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (all (hiLists) should (be (One("hi")) and contain oneElementOf Seq("HI", "HE"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (be (One("ho")) and contain oneElementOf Seq("hi", "he"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, One("hi")) + " was not equal to " + decorateToStringValue(prettifier, One("ho")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (be (One("hi")) and contain oneElementOf Seq("hi", "he"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, One("hi")) + " was equal to " + decorateToStringValue(prettifier, One("hi")) + ", but " + FailureMessages.didNotContainOneElementOf(prettifier, hiLists(0), Seq("hi", "he")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should allow RHS to contain duplicated value") {
        all (list1s) should (be (One(1)) and contain oneElementOf Seq(3, 2, 2, 1))
      }
    }

    describe("when used with (not contain oneElementOf (..) and not contain oneElementOf (..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (not contain oneElementOf (Seq(3, 2, 8)) and not contain oneElementOf (Seq(8, 3, 4)))
        atLeast (2, lists) should (not contain oneElementOf (Seq(3, 8, 5)) and not contain oneElementOf (Seq(8, 3, 4)))
        atMost (2, lists) should (not contain oneElementOf (Seq(3, 6, 8)) and contain oneElementOf (Seq(5, 3, 4)))
        no (lists) should (not contain oneElementOf (Seq(1, 2, 9)) and not contain oneElementOf (Seq(2, 1, 5)))

        val e1 = intercept[TestFailedException] {
          all (lists) should (not contain oneElementOf (Seq(2, 6, 8)) and not contain oneElementOf (Seq(2, 3, 4)))
        }
        checkMessageStackDepth(e1, allErrMsg(2, FailureMessages.containedOneElementOf(prettifier, lists(2), Seq(2, 6, 8)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (lists) should (not contain oneElementOf (Seq(3, 6, 8)) and not contain oneElementOf (Seq(2, 3, 4)))
        }
        checkMessageStackDepth(e2, allErrMsg(2, FailureMessages.didNotContainOneElementOf(prettifier, lists(2), Seq(3, 6, 8)) + ", but " + FailureMessages.containedOneElementOf(prettifier, lists(2), Seq(2, 3, 4)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e3 = intercept[TestFailedException] {
          all (hiLists) should (not contain oneElementOf (Seq("hi", "hello")) and not contain oneElementOf (Seq("ho", "hey", "howdy")))
        }
        checkMessageStackDepth(e3, allErrMsg(0, FailureMessages.containedOneElementOf(prettifier, hiLists(0), Seq("hi", "hello")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e4 = intercept[TestFailedException] {
          all (hiLists) should (not contain oneElementOf (Seq("ho", "hey", "howdy")) and not contain oneElementOf (Seq("hi", "hello")))
        }
        checkMessageStackDepth(e4, allErrMsg(0, FailureMessages.didNotContainOneElementOf(prettifier, hiLists(0), Seq("ho", "hey", "howdy")) + ", but " + FailureMessages.containedOneElementOf(prettifier, hiLists(0), Seq("hi", "hello")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (not contain oneElementOf (Seq("hi", "he")) and not contain oneElementOf (Seq("hi", "he")))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not contain oneElementOf (Seq("HI", "HE")) and not contain oneElementOf (Seq("HO", "HEY", "HOWDY")))
        }
        checkMessageStackDepth(e1, allErrMsg(0, FailureMessages.containedOneElementOf(prettifier, hiLists(0), Seq("HI", "HE")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (hiLists) should (not contain oneElementOf (Seq("HO", "HEY", "HOWDY")) and not contain oneElementOf (Seq("HI", "HE")))
        }
        checkMessageStackDepth(e2, allErrMsg(0, FailureMessages.didNotContainOneElementOf(prettifier, hiLists(0), Seq("HO", "HEY", "HOWDY")) + ", but " + FailureMessages.containedOneElementOf(prettifier, hiLists(0), Seq("HI", "HE")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (all (hiLists) should (not contain oneElementOf (Seq("hi", "he")) and not contain oneElementOf (Seq("hi", "he")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not contain oneElementOf (Seq("HI", "HE")) and not contain oneElementOf (Seq("HO", "HEY", "HOWDY")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, FailureMessages.containedOneElementOf(prettifier, hiLists(0), Seq("HI", "HE")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (not contain oneElementOf (Seq("HO", "HEY", "HOWDY")) and not contain oneElementOf (Seq("HI", "HE")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, FailureMessages.didNotContainOneElementOf(prettifier, hiLists(0), Seq("HO", "HEY", "HOWDY")) + ", but " + FailureMessages.containedOneElementOf(prettifier, hiLists(0), Seq("HI", "HE")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should allow RHS to contain duplicated value") {
        all (list1s) should (not contain oneElementOf (Seq(3, 2, 2, 8)) and not contain oneElementOf (Seq(8, 3, 4)))
        all (list1s) should (not contain oneElementOf (Seq(8, 3, 4)) and not contain oneElementOf (Seq(3, 2, 2, 8)))
      }
    }

    describe("when used with (not be (..) and not contain oneElementOf (..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (not be (One(2)) and not contain oneElementOf (Seq(8, 3, 4)))
        atLeast (2, lists) should (not be (One(3)) and not contain oneElementOf (Seq(8, 3, 4)))
        atMost (2, lists) should (not be (One(3)) and contain oneElementOf Seq(5, 3, 4))
        no (list1s) should (not be (One(1)) and not contain oneElementOf (Seq(2, 1, 5)))

        val e1 = intercept[TestFailedException] {
          all (lists) should (not be (One(2)) and not contain oneElementOf (Seq(2, 3, 4)))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(prettifier, One(2)) + " was equal to " + decorateToStringValue(prettifier, One(2)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (lists) should (not be (One(3)) and not contain oneElementOf (Seq(2, 3, 4)))
        }
        checkMessageStackDepth(e2, allErrMsg(2, decorateToStringValue(prettifier, One(2)) + " was not equal to " + decorateToStringValue(prettifier, One(3)) + ", but " + FailureMessages.containedOneElementOf(prettifier, lists(2), Seq(2, 3, 4)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e3 = intercept[TestFailedException] {
          all (hiLists) should (not be (One("hi")) and not contain oneElementOf (Seq("ho", "hey", "howdy")))
        }
        checkMessageStackDepth(e3, allErrMsg(0, decorateToStringValue(prettifier, One("hi")) + " was equal to " + decorateToStringValue(prettifier, One("hi")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e4 = intercept[TestFailedException] {
          all (hiLists) should (not be (One("ho")) and not contain oneElementOf (Seq("hi", "hello")))
        }
        checkMessageStackDepth(e4, allErrMsg(0, decorateToStringValue(prettifier, One("hi")) + " was not equal to " + decorateToStringValue(prettifier, One("ho")) + ", but " + FailureMessages.containedOneElementOf(prettifier, hiLists(0), Seq("hi", "hello")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (not be (One("ho")) and not contain oneElementOf (Seq("hi", "he")))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not be (One("hi")) and not contain oneElementOf (Seq("HI", "HE")))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, One("hi")) + " was equal to " + decorateToStringValue(prettifier, One("hi")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (hiLists) should (not be (One("ho")) and not contain oneElementOf (Seq("HI", "HE")))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, One("hi")) + " was not equal to " + decorateToStringValue(prettifier, One("ho")) + ", but " + FailureMessages.containedOneElementOf(prettifier, hiLists(0), Seq("HI", "HE")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (all (hiLists) should (not be (One("ho")) and not contain oneElementOf (Seq("hi", "he")))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not be (One("hi")) and not contain oneElementOf (Seq("HI", "HE")))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, One("hi")) + " was equal to " + decorateToStringValue(prettifier, One("hi")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (not be (One("ho")) and not contain oneElementOf (Seq("HI", "HE")))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, One("hi")) + " was not equal to " + decorateToStringValue(prettifier, One("ho")) + ", but " + FailureMessages.containedOneElementOf(prettifier, hiLists(0), Seq("HI", "HE")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should allow RHS to contain duplicated value") {
        all (list1s) should (not be (One(2)) and not contain oneElementOf (Seq(3, 2, 2, 8)))
      }
    }
  }
}
