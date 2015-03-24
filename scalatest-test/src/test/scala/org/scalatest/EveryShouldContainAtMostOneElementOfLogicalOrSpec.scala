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

import org.scalactic.{Equality, Every, One, Many, Entry}
import org.scalactic.Uniformity
import org.scalactic.StringNormalizations._
import SharedHelpers._
import FailureMessages.decorateToStringValue
import Matchers._
import exceptions.TestFailedException

class EveryShouldContainAtMostOneElementOfLogicalOrSpec extends FreeSpec {

  val upperCaseStringEquality =
    new Equality[String] {
      def areEqual(a: String, b: Any): Boolean = upperCase(a) == upperCase(b)
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

  //ADDITIONAL//

  val fileName: String = "EveryShouldContainAtMostOneElementOfLogicalOrSpec.scala"

  "an Every" - {

    val fumList: Every[String] = Every("fum", "foe")
    val toList: Every[String] = Every("to", "you")

    "when used with (contain atMostOneElementOf Seq(...) or contain atMostOneElementOf Seq(...)) syntax" - {

      "should do nothing if valid, else throw a TFE with an appropriate error message" in {
        fumList should (contain atMostOneElementOf Seq("fee", "fie", "foe", "fam") or contain atMostOneElementOf Seq("fie", "fee", "fam", "foe"))
        fumList should (contain atMostOneElementOf Seq("fee", "fie", "foe", "fam") or contain atMostOneElementOf Seq("fie", "fee", "fum", "foe"))
        fumList should (contain atMostOneElementOf Seq("fee", "fie", "foe", "fum") or contain atMostOneElementOf Seq("fie", "fee", "fam", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain atMostOneElementOf Seq("fee", "fie", "foe", "fum") or contain atMostOneElementOf Seq("fie", "fee", "fum", "foe"))
        }
        checkMessageStackDepth(e1, FailureMessages.didNotContainAtMostOneElementOf(fumList, Seq("fee", "fie", "foe", "fum")) + ", and " + FailureMessages.didNotContainAtMostOneElementOf(fumList, Seq("fie", "fee", "fum", "foe")), fileName, thisLineNumber - 2)
      }

      "should use the implicit Equality in scope" in {
        implicit val ise = upperCaseStringEquality
        fumList should (contain atMostOneElementOf Seq("FEE", "FIE", "FOE", "FAM") or contain atMostOneElementOf Seq("FIE", "FEE", "FAM", "FOE"))
        fumList should (contain atMostOneElementOf Seq("FEE", "FIE", "FOE", "FAM") or contain atMostOneElementOf Seq("FIE", "FEE", "FUM", "FOE"))
        fumList should (contain atMostOneElementOf Seq("FEE", "FIE", "FOE", "FUM") or contain atMostOneElementOf Seq("FIE", "FEE", "FAM", "FOE"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain atMostOneElementOf Seq("FEE", "FIE", "FOE", "FUM") or (contain atMostOneElementOf Seq("FIE", "FEE", "FUM", "FOE")))
        }
        checkMessageStackDepth(e1, FailureMessages.didNotContainAtMostOneElementOf(fumList, Seq("FEE", "FIE", "FOE", "FUM")) + ", and " + FailureMessages.didNotContainAtMostOneElementOf(fumList, Seq("FIE", "FEE", "FUM", "FOE")), fileName, thisLineNumber - 2)
      }

      "should use an explicitly provided Equality" in {
        (fumList should (contain atMostOneElementOf Seq("FEE", "FIE", "FOE", "FAM") or contain atMostOneElementOf Seq("FIE", "FEE", "FAM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (contain atMostOneElementOf Seq("FEE", "FIE", "FOE", "FAM") or contain atMostOneElementOf Seq("FIE", "FEE", "FUM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (contain atMostOneElementOf Seq("FEE", "FIE", "FOE", "FUM") or contain atMostOneElementOf Seq("FIE", "FEE", "FAM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain atMostOneElementOf Seq("FEE", "FIE", "FOE", "FUM") or contain atMostOneElementOf Seq("FIE", "FEE", "FUM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.didNotContainAtMostOneElementOf(fumList, Seq("FEE", "FIE", "FOE", "FUM")) + ", and " + FailureMessages.didNotContainAtMostOneElementOf(fumList, Seq("FIE", "FEE", "FUM", "FOE")), fileName, thisLineNumber - 2)
        (fumList should (contain atMostOneElementOf Seq(" FEE ", " FIE ", " FOE ", " FAM ") or contain atMostOneElementOf Seq(" FEE ", " FIE ", " FOE ", " FAM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }

      "should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value" in {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (contain atMostOneElementOf Seq("fee", "fie", "foe", "fie", "fum") or contain atMostOneElementOf Seq("fie", "fee", "fam", "foe"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages.atMostOneElementOfDuplicate))

        val e2 = intercept[exceptions.NotAllowedException] {
          fumList should (contain atMostOneElementOf Seq("fie", "fee", "fam", "foe") or contain atMostOneElementOf Seq("fee", "fie", "foe", "fie", "fum"))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(FailureMessages.atMostOneElementOfDuplicate))
      }
    }

    "when used with (equal (...) and contain oneOf (...)) syntax" - {

      "should do nothing if valid, else throw a TFE with an appropriate error message" in {
        fumList should (equal (fumList) or contain atMostOneElementOf Seq("fie", "fee", "fam", "foe"))
        fumList should (equal (toList) or contain atMostOneElementOf Seq("fie", "fee", "fam", "foe"))
        fumList should (equal (fumList) or contain atMostOneElementOf Seq("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) or contain atMostOneElementOf Seq("fie", "fee", "fum", "foe"))
        }
        checkMessageStackDepth(e1, FailureMessages.didNotEqual(fumList, toList) + ", and " + FailureMessages.didNotContainAtMostOneElementOf(fumList, Seq("fie", "fee", "fum", "foe")), fileName, thisLineNumber - 2)
      }

      "should use the implicit Equality in scope" in {
        implicit val ise = upperCaseStringEquality
        fumList should (equal (fumList) or contain atMostOneElementOf Seq("FIE", "FEE", "FAM", "FOE"))
        fumList should (equal (toList) or contain atMostOneElementOf Seq("FIE", "FEE", "FAM", "FOE"))
        fumList should (equal (fumList) or contain atMostOneElementOf Seq("FIE", "FEE", "FUM", "FOE"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) or (contain atMostOneElementOf Seq("FIE", "FEE", "FUM", "FOE")))
        }
        checkMessageStackDepth(e1, FailureMessages.didNotEqual(fumList, toList) + ", and " + FailureMessages.didNotContainAtMostOneElementOf(fumList, Seq("FIE", "FEE", "FUM", "FOE")), fileName, thisLineNumber - 2)
      }

      "should use an explicitly provided Equality" in {
        (fumList should (equal (toList) or contain atMostOneElementOf Seq("FIE", "FEE", "FAM", "FOE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (equal (fumList) or contain atMostOneElementOf Seq("FIE", "FEE", "FAM", "FOE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (equal (toList) or contain atMostOneElementOf Seq("FIE", "FEE", "FUM", "FOE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (equal (fumList) or contain atMostOneElementOf Seq("FIE", "FEE", "FUM", "FOE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.didNotEqual(fumList, fumList) + ", and " + FailureMessages.didNotContainAtMostOneElementOf(fumList, Seq("FIE", "FEE", "FUM", "FOE")), fileName, thisLineNumber - 2)
        (fumList should (equal (toList) or contain atMostOneElementOf Seq(" FEE ", " FIE ", " FOE ", " FAM "))) (decided by invertedListOfStringEquality, after being lowerCased and trimmed)
      }

      "should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value" in {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (equal (fumList) or contain atMostOneElementOf Seq("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages.atMostOneElementOfDuplicate))
      }
    }

    "when used with (be (...) and contain theMostOneOf (...)) syntax" - {

      "should do nothing if valid, else throw a TFE with an appropriate error message" in {
        fumList should (be_== (fumList) or contain atMostOneElementOf Seq("fie", "fee", "fam", "foe"))
        fumList should (be_== (toList) or contain atMostOneElementOf Seq("fie", "fee", "fam", "foe"))
        fumList should (be_== (fumList) or contain atMostOneElementOf Seq("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (be_== (toList) or contain atMostOneElementOf Seq("fie", "fee", "fum", "foe"))
        }
        checkMessageStackDepth(e1, FailureMessages.wasNotEqualTo(fumList, toList) + ", and " + FailureMessages.didNotContainAtMostOneElementOf(fumList, Seq("fie", "fee", "fum", "foe")), fileName, thisLineNumber - 2)
      }

      "should use the implicit Equality in scope" in {
        implicit val ise = upperCaseStringEquality
        fumList should (be_== (fumList) or contain atMostOneElementOf Seq("FIE", "FEE", "FAM", "FOE"))
        fumList should (be_== (toList) or contain atMostOneElementOf Seq("FIE", "FEE", "FAM", "FOE"))
        fumList should (be_== (fumList) or contain atMostOneElementOf Seq("FIE", "FEE", "FUM", "FOE"))
        val e1 = intercept[TestFailedException] {
          fumList should (be_== (toList) or (contain atMostOneElementOf Seq("FIE", "FEE", "FUM", "FOE")))
        }
        checkMessageStackDepth(e1, FailureMessages.wasNotEqualTo(fumList, toList) + ", and " + FailureMessages.didNotContainAtMostOneElementOf(fumList, Seq("FIE", "FEE", "FUM", "FOE")), fileName, thisLineNumber - 2)
      }

      "should use an explicitly provided Equality" in {
        (fumList should (be_== (fumList) or contain atMostOneElementOf Seq("FIE", "FEE", "FAM", "FOE"))) (decided by upperCaseStringEquality)
        (fumList should (be_== (toList) or contain atMostOneElementOf Seq("FIE", "FEE", "FAM", "FOE"))) (decided by upperCaseStringEquality)
        (fumList should (be_== (fumList) or contain atMostOneElementOf Seq("FIE", "FEE", "FUM", "FOE"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (be_== (toList) or contain atMostOneElementOf Seq("FIE", "FEE", "FUM", "FOE"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.wasNotEqualTo(fumList, toList) + ", and " + FailureMessages.didNotContainAtMostOneElementOf(fumList, Seq("FIE", "FEE", "FUM", "FOE")), fileName, thisLineNumber - 2)
        (fumList should (be_== (fumList) or contain atMostOneElementOf Seq(" FEE ", " FIE ", " FOE ", " FAM "))) (after being lowerCased and trimmed)
      }

      "should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value" in {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (be_== (fumList) or contain atMostOneElementOf Seq("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be_== (Some(FailureMessages.atMostOneElementOfDuplicate))
      }
    }

    "when used with (contain oneOf (...) and be (...)) syntax" - {

      "should do nothing if valid, else throw a TFE with an appropriate error message" in {
        fumList should (contain atMostOneElementOf Seq("fie", "fee", "fam", "foe") or be_== (fumList))
        fumList should (contain atMostOneElementOf Seq("fie", "fee", "fum", "foe") or be_== (fumList))
        fumList should (contain atMostOneElementOf Seq("fie", "fee", "fam", "foe") or be_== (toList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain atMostOneElementOf Seq("fee", "fie", "foe", "fum") or be_== (toList))
        }
        checkMessageStackDepth(e1, FailureMessages.didNotContainAtMostOneElementOf(fumList, Seq("fee", "fie", "foe", "fum")) + ", and " + FailureMessages.wasNotEqualTo(fumList, toList), fileName, thisLineNumber - 2)
      }

      "should use the implicit Equality in scope" in {
        implicit val ise = upperCaseStringEquality
        fumList should (contain atMostOneElementOf Seq("FIE", "FEE", "FAM", "FOE") or be_== (fumList))
        fumList should (contain atMostOneElementOf Seq("FIE", "FEE", "FUM", "FOE") or be_== (fumList))
        fumList should (contain atMostOneElementOf Seq("FIE", "FEE", "FAM", "FOE") or be_== (toList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain atMostOneElementOf Seq("FEE", "FIE", "FOE", "FUM") or be_== (toList))
        }
        checkMessageStackDepth(e1, FailureMessages.didNotContainAtMostOneElementOf(fumList, Seq("FEE", "FIE", "FOE", "FUM")) + ", and " + FailureMessages.wasNotEqualTo(fumList, toList), fileName, thisLineNumber - 2)
      }

      "should use an explicitly provided Equality" in {
        (fumList should (contain atMostOneElementOf Seq("FIE", "FEE", "FAM", "FOE") or be_== (fumList))) (decided by upperCaseStringEquality)
        (fumList should (contain atMostOneElementOf Seq("FIE", "FEE", "FUM", "FOE") or be_== (fumList))) (decided by upperCaseStringEquality)
        (fumList should (contain atMostOneElementOf Seq("FIE", "FEE", "FAM", "FOE") or be_== (toList))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain atMostOneElementOf Seq("FEE", "FIE", "FOE", "FUM") or be_== (toList))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.didNotContainAtMostOneElementOf(fumList, Seq("FEE", "FIE", "FOE", "FUM")) + ", and " + FailureMessages.wasNotEqualTo(fumList, toList), fileName, thisLineNumber - 2)
        (fumList should (contain atMostOneElementOf Seq(" FEE ", " FIE ", " FOE ", " FaM ") or be_== (fumList))) (after being lowerCased and trimmed)
      }

      "should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value" in {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (contain atMostOneElementOf Seq("fee", "fie", "foe", "fie", "fum") or be_== (fumList))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages.atMostOneElementOfDuplicate))
      }
    }

    "when used with (not contain atMostOneElementOf Seq(...) and not contain atMostOneElementOf Seq(...)) syntax" - {

      "should do nothing if valid, else throw a TFE with an appropriate error message" in {
        fumList should (not contain atMostOneElementOf (Seq("fee", "fie", "foe", "fum")) or not contain atMostOneElementOf (Seq("fie", "fee", "fum", "foe")))
        fumList should (not contain atMostOneElementOf (Seq("fee", "fie", "foe", "fuu")) or not contain atMostOneElementOf (Seq("fie", "fee", "fum", "foe")))
        fumList should (not contain atMostOneElementOf (Seq("fee", "fie", "foe", "fum")) or not contain atMostOneElementOf (Seq("fie", "fee", "fuu", "foe")))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain atMostOneElementOf (Seq("fee", "fie", "foe", "fuu")) or not contain atMostOneElementOf (Seq("fee", "fie", "foe", "fuu")))
        }
        checkMessageStackDepth(e1, FailureMessages.containedAtMostOneElementOf(fumList, Seq("fee", "fie", "foe", "fuu")) + ", and " + FailureMessages.containedAtMostOneElementOf(fumList, Seq("fee", "fie", "foe", "fuu")), fileName, thisLineNumber - 2)
      }

      "should use the implicit Equality in scope" in {
        implicit val ise = upperCaseStringEquality
        fumList should (not contain atMostOneElementOf (Seq("FEE", "FIE", "FOE", "FUM")) or not contain atMostOneElementOf (Seq("FIE", "FEE", "FUM", "FOE")))
        fumList should (not contain atMostOneElementOf (Seq("FEE", "FIE", "FOE", "FUU")) or not contain atMostOneElementOf (Seq("FIE", "FEE", "FUM", "FOE")))
        fumList should (not contain atMostOneElementOf (Seq("FEE", "FIE", "FOE", "FUM")) or not contain atMostOneElementOf (Seq("FIE", "FEE", "FUU", "FOE")))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain atMostOneElementOf (Seq("FEE", "FIE", "FOE", "FUU")) or not contain atMostOneElementOf (Seq("FEE", "FIE", "FOE", "FUU")))
        }
        checkMessageStackDepth(e1, FailureMessages.containedAtMostOneElementOf(fumList, Seq("FEE", "FIE", "FOE", "FUU")) + ", and " + FailureMessages.containedAtMostOneElementOf(fumList, Seq("FEE", "FIE", "FOE", "FUU")), fileName, thisLineNumber - 2)
      }

      "should use an explicitly provided Equality" in {
        (fumList should (not contain atMostOneElementOf (Seq("FEE", "FIE", "FOE", "FUM")) or not contain atMostOneElementOf (Seq("FIE", "FEE", "FUM", "FOE")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (not contain atMostOneElementOf (Seq("FEE", "FIE", "FOE", "FUU")) or not contain atMostOneElementOf (Seq("FIE", "FEE", "FUM", "FOE")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (not contain atMostOneElementOf (Seq("FEE", "FIE", "FOE", "FUM")) or not contain atMostOneElementOf (Seq("FIE", "FEE", "FUU", "FOE")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not contain atMostOneElementOf (Seq("FEE", "FIE", "FOE", "FUU")) or not contain atMostOneElementOf (Seq("FEE", "FIE", "FOE", "FUU")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.containedAtMostOneElementOf(fumList, Seq("FEE", "FIE", "FOE", "FUU")) + ", and " + FailureMessages.containedAtMostOneElementOf(fumList, Seq("FEE", "FIE", "FOE", "FUU")), fileName, thisLineNumber - 2)
        (fumList should (contain atMostOneElementOf Seq(" FEE ", " FIE ", " FOE ", " FAM ") or contain atMostOneElementOf Seq(" FEE ", " FIE ", " FOE ", " FAM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }

      "should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value" in {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not contain atMostOneElementOf (Seq("fee", "fie", "foe", "fie", "fum")) or not contain atMostOneElementOf (Seq("fie", "fee", "fum", "foe")))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages.atMostOneElementOfDuplicate))

        val e2 = intercept[exceptions.NotAllowedException] {
          fumList should (not contain atMostOneElementOf (Seq("fie", "fee", "fum", "foe")) or not contain atMostOneElementOf (Seq("fee", "fie", "foe", "fie", "fum")))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(FailureMessages.atMostOneElementOfDuplicate))
      }
    }

    "when used with (not equal (...) and not contain oneOf (...)) syntax" - {

      "should do nothing if valid, else throw a TFE with an appropriate error message" in {
        fumList should (not equal (toList) or not contain atMostOneElementOf (Seq("fie", "fee", "fum", "foe")))
        fumList should (not equal (fumList) or not contain atMostOneElementOf (Seq("fie", "fee", "fum", "foe")))
        fumList should (not equal (toList) or not contain atMostOneElementOf (Seq("fie", "fee", "fuu", "foe")))
        val e1 = intercept[TestFailedException] {
          fumList should (not equal (fumList) or not contain atMostOneElementOf (Seq("fee", "fie", "foe", "fuu")))
        }
        checkMessageStackDepth(e1, FailureMessages.equaled(fumList, fumList) + ", and " + FailureMessages.containedAtMostOneElementOf(fumList, Seq("fee", "fie", "foe", "fuu")), fileName, thisLineNumber - 2)
      }

      "should use the implicit Equality in scope" in {
        implicit val ise = upperCaseStringEquality
        fumList should (not equal (toList) or not contain atMostOneElementOf (Seq("FIE", "FEE", "FUM", "FOE")))
        fumList should (not equal (fumList) or not contain atMostOneElementOf (Seq("FIE", "FEE", "FUM", "FOE")))
        fumList should (not equal (toList) or not contain atMostOneElementOf (Seq("FIE", "FEE", "FUU", "FOE")))
        val e2 = intercept[TestFailedException] {
          fumList should (not equal (fumList) or (not contain atMostOneElementOf (Seq("FEE", "FIE", "FOE", "FUU"))))
        }
        checkMessageStackDepth(e2, FailureMessages.equaled(fumList, fumList) + ", and " + FailureMessages.containedAtMostOneElementOf(fumList, Seq("FEE", "FIE", "FOE", "FUU")), fileName, thisLineNumber - 2)
      }

      "should use an explicitly provided Equality" in {
        (fumList should (not equal (fumList) or not contain atMostOneElementOf (Seq("FIE", "FEE", "FUM", "FOE")))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (not equal (toList) or not contain atMostOneElementOf (Seq("FIE", "FEE", "FUM", "FOE")))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (not equal (fumList) or not contain atMostOneElementOf (Seq("FIE", "FEE", "FUU", "FOE")))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not equal (toList) or not contain atMostOneElementOf (Seq("FEE", "FIE", "FOE", "FUU")))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.equaled(fumList, toList) + ", and " + FailureMessages.containedAtMostOneElementOf(fumList, Seq("FEE", "FIE", "FOE", "FUU")), fileName, thisLineNumber - 2)
        (fumList should (not contain atMostOneElementOf (Seq(" FEE ", " FIE ", " FOE ", " FUM ")) or not contain atMostOneElementOf (Seq(" FEE ", " FIE ", " FOE ", " FUM ")))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }

      "should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value" in {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not equal (toList) or not contain atMostOneElementOf (Seq("fee", "fie", "foe", "fie", "fum")))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages.atMostOneElementOfDuplicate))
      }
    }

    "when used with (not be (...) and not contain oneOf (...)) syntax" - {

      "should do nothing if valid, else throw a TFE with an appropriate error message" in {
        fumList should (not be_== (toList) or not contain atMostOneElementOf (Seq("fie", "fee", "fum", "foe")))
        fumList should (not be_== (fumList) or not contain atMostOneElementOf (Seq("fie", "fee", "fum", "foe")))
        fumList should (not be_== (toList) or not contain atMostOneElementOf (Seq("fee", "fie", "foe", "fuu")))
        val e1 = intercept[TestFailedException] {
          fumList should (not be_== (fumList) or not contain atMostOneElementOf (Seq("fee", "fie", "foe", "fuu")))
        }
        checkMessageStackDepth(e1, FailureMessages.wasEqualTo(fumList, fumList) + ", and " + FailureMessages.containedAtMostOneElementOf(fumList, Seq("fee", "fie", "foe", "fuu")), fileName, thisLineNumber - 2)
      }

      "should use the implicit Equality in scope" in {
        implicit val ise = upperCaseStringEquality
        fumList should (not be_== (toList) or not contain atMostOneElementOf (Seq("FIE", "FEE", "FUM", "FOE")))
        fumList should (not be_== (fumList) or not contain atMostOneElementOf (Seq("FIE", "FEE", "FUM", "FOE")))
        fumList should (not be_== (toList) or not contain atMostOneElementOf (Seq("FIE", "FEE", "FUU", "FOE")))
        val e1 = intercept[TestFailedException] {
          fumList should (not be_== (fumList) or (not contain atMostOneElementOf (Seq("FEE", "FIE", "FOE", "FUU"))))
        }
        checkMessageStackDepth(e1, FailureMessages.wasEqualTo(fumList, fumList) + ", and " + FailureMessages.containedAtMostOneElementOf(fumList, Seq("FEE", "FIE", "FOE", "FUU")), fileName, thisLineNumber - 2)
      }

      "should use an explicitly provided Equality" in {
        (fumList should (not be_== (toList) or not contain atMostOneElementOf (Seq("FIE", "FEE", "FUM", "FOE")))) (decided by upperCaseStringEquality)
        (fumList should (not be_== (fumList) or not contain atMostOneElementOf (Seq("FIE", "FEE", "FUM", "FOE")))) (decided by upperCaseStringEquality)
        (fumList should (not be_== (toList) or not contain atMostOneElementOf (Seq("FIE", "FEE", "FUU", "FOE")))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not be_== (fumList) or not contain atMostOneElementOf (Seq("FEE", "FIE", "FOE", "FUU")))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages.wasEqualTo(fumList, fumList) + ", and " + FailureMessages.containedAtMostOneElementOf(fumList, Seq("FEE", "FIE", "FOE", "FUU")), fileName, thisLineNumber - 2)
        (fumList should (not contain atMostOneElementOf (Seq(" FEE ", " FIE ", " FOE ", " FUM ")) or not contain atMostOneElementOf (Seq(" FEE ", " FIE ", " FOE ", " FUM ")))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }

      "should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value" in {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not be_== (toList) or not contain atMostOneElementOf (Seq("fee", "fie", "foe", "fie", "fum")))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages.atMostOneElementOfDuplicate))
      }
    }

  }

  "every of Everys" - {

    val list1s: Every[Every[Int]] = Every(Every(1, 2), Every(1, 2), Every(1, 2))
    val lists: Every[Every[Int]] = Every(Every(1, 2), Every(1, 2), Every(2, 3))
    val hiLists: Every[Every[String]] = Every(Every("hi", "he"), Every("hi", "he"), Every("hi", "he"))
    val toLists: Every[Every[String]] = Every(Every("to", "you"), Every("to", "you"), Every("to", "you"))

    def allErrMsg(index: Int, message: String, lineNumber: Int, left: Any): String =
      "'all' inspection failed, because: \n" +
        "  at index " + index + ", " + message + " (" + fileName + ":" + (lineNumber) + ") \n" +
        "in " + decorateToStringValue(left)

    "when used with (contain oneOf (..) and contain oneOf (..)) syntax" - {

      "should do nothing if valid, else throw a TFE with an appropriate error message" in {
        all (list1s) should (contain atMostOneElementOf Seq(1, 6, 8) or contain atMostOneElementOf Seq(1, 3, 4))
        all (list1s) should (contain atMostOneElementOf Seq(1, 2, 3) or contain atMostOneElementOf Seq(1, 3, 4))
        all (list1s) should (contain atMostOneElementOf Seq(1, 6, 8) or contain atMostOneElementOf Seq(1, 2, 3))

        atLeast (2, lists) should (contain atMostOneElementOf Seq(3, 2, 5) or contain atMostOneElementOf Seq(2, 3, 4))
        atLeast (2, lists) should (contain atMostOneElementOf Seq(1, 2, 3) or contain atMostOneElementOf Seq(2, 3, 4))
        atLeast (2, lists) should (contain atMostOneElementOf Seq(3, 2, 5) or contain atMostOneElementOf Seq(1, 2, 3))

        val e1 = intercept[TestFailedException] {
          all (lists) should (contain atMostOneElementOf Seq(2, 3, 4) or contain atMostOneElementOf Seq(4, 3, 2))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(lists(2)) + " did not contain at most one element of " + decorateToStringValue(Seq(2, 3, 4)) + ", and " + decorateToStringValue(lists(2)) + " did not contain at most one element of " + decorateToStringValue(Seq(4, 3, 2)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
      }

      "should use the implicit Equality in scope" in {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (contain atMostOneElementOf Seq("HI", "HO") or contain atMostOneElementOf Seq("HO", "HI"))
        all (hiLists) should (contain atMostOneElementOf Seq("HI", "HE") or contain atMostOneElementOf Seq("HO", "HI"))
        all (hiLists) should (contain atMostOneElementOf Seq("HI", "HO") or contain atMostOneElementOf Seq("HI", "HE"))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (contain atMostOneElementOf Seq("HI", "HE") or contain atMostOneElementOf Seq("HE", "HI"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(hiLists(0)) + " did not contain at most one element of " + decorateToStringValue(Seq("HI", "HE")) + ", and " + decorateToStringValue(hiLists(0)) + " did not contain at most one element of " + decorateToStringValue(Seq("HE", "HI")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      "should use an explicitly provided Equality" in {
        (all (hiLists) should (contain atMostOneElementOf Seq("HI", "HO") or contain atMostOneElementOf Seq("HO", "HI"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (contain atMostOneElementOf Seq("HI", "HE") or contain atMostOneElementOf Seq("HO", "HI"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (contain atMostOneElementOf Seq("HI", "HO") or contain atMostOneElementOf Seq("HI", "HE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)

        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (contain atMostOneElementOf Seq("HI", "HE") or contain atMostOneElementOf Seq("HE", "HI"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(hiLists(0)) + " did not contain at most one element of " + decorateToStringValue(Seq("HI", "HE")) + ", and " + decorateToStringValue(hiLists(0)) + " did not contain at most one element of " + decorateToStringValue(Seq("HE", "HI")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      "should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value" in {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (contain atMostOneElementOf Seq(1, 2, 2, 3) or contain atMostOneElementOf Seq(1, 3, 4))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages.atMostOneElementOfDuplicate))

        val e2 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (contain atMostOneElementOf Seq(1, 3, 4) or contain atMostOneElementOf Seq(1, 2, 2, 3))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(FailureMessages.atMostOneElementOfDuplicate))
      }
    }

    "when used with (be (...) and contain oneOf (...)) syntax" - {

      "should do nothing if valid, else throw a TFE with an appropriate error message" in {
        all (list1s) should (be_== (Many(1, 2)) or contain atMostOneElementOf Seq(1, 6, 8))
        all (list1s) should (be_== (Many(2, 3)) or contain atMostOneElementOf Seq(1, 6, 8))
        all (list1s) should (be_== (Many(1, 2)) or contain atMostOneElementOf Seq(1, 2, 3))

        val e1 = intercept[TestFailedException] {
          all (list1s) should (be_== (Many(2, 3)) or contain atMostOneElementOf Seq(1, 2, 3))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(list1s(0)) + " was not equal to " + decorateToStringValue(Many(2, 3)) + ", and " + decorateToStringValue(list1s(0)) + " did not contain at most one element of " + decorateToStringValue(Seq(1, 2, 3)), thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
      }

      "should use the implicit Equality in scope" in {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (be_== (Many("hi", "he")) or contain atMostOneElementOf Seq("HI", "HO"))
        all (hiLists) should (be_== (One("HO")) or contain atMostOneElementOf Seq("HI", "HO"))
        all (hiLists) should (be_== (Many("hi", "he")) or contain atMostOneElementOf Seq("HI", "HE"))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (be_== (One("HO")) or contain atMostOneElementOf Seq("HI", "HE"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(hiLists(0)) + " was not equal to " + decorateToStringValue(One("HO")) + ", and " + decorateToStringValue(hiLists(0)) + " did not contain at most one element of " + decorateToStringValue(Seq("HI", "HE")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      "should use an explicitly provided Equality" in {
        (all (hiLists) should (be_== (Many("hi", "he")) or contain atMostOneElementOf Seq("HI", "HO"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (be_== (One("HO")) or contain atMostOneElementOf Seq("HI", "HO"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (be_== (Many("hi", "he")) or contain atMostOneElementOf Seq("HI", "HE"))) (decided by upperCaseStringEquality)

        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (be_== (One("HO")) or contain atMostOneElementOf Seq("HI", "HE"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(hiLists(0)) + " was not equal to " + decorateToStringValue(One("HO")) + ", and " + decorateToStringValue(hiLists(0)) + " did not contain at most one element of " + decorateToStringValue(Seq("HI", "HE")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      "should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value" in {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (be_== (Many(1, 2)) or contain atMostOneElementOf Seq(1, 2, 2, 3))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages.atMostOneElementOfDuplicate))
      }
    }

    "when used with (not contain oneOf (..) and not contain oneOf (..))" - {

      "should do nothing if valid, else throw a TFE with an appropriate error message" in {
        all (list1s) should (not contain atMostOneElementOf (Seq(3, 2, 1)) or not contain atMostOneElementOf (Seq(1, 2, 3)))
        all (list1s) should (not contain atMostOneElementOf (Seq(1, 2, 8)) or not contain atMostOneElementOf (Seq(1, 2, 3)))
        all (list1s) should (not contain atMostOneElementOf (Seq(3, 2, 1)) or not contain atMostOneElementOf (Seq(1, 2, 8)))

        val e1 = intercept[TestFailedException] {
          all (lists) should (not contain atMostOneElementOf (Seq(1, 2, 8)) or not contain atMostOneElementOf (Seq(8, 2, 1)))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(lists(2)) + " contained at most one element of " + decorateToStringValue(Seq(1, 2, 8)) + ", and " + decorateToStringValue(lists(2)) + " contained at most one element of " + decorateToStringValue(Seq(8, 2, 1)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
      }

      "should use the implicit Equality in scope" in {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (not contain atMostOneElementOf (Seq("HI", "HE")) or not contain atMostOneElementOf (Seq("HE", "HI")))
        all (hiLists) should (not contain atMostOneElementOf (Seq("hi", "he")) or not contain atMostOneElementOf (Seq("HE", "HI")))
        all (hiLists) should (not contain atMostOneElementOf (Seq("HI", "HE")) or not contain atMostOneElementOf (Seq("hi", "he")))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not contain atMostOneElementOf (Seq("HE", "HEY", "HOWDY")) or not contain atMostOneElementOf (Seq("HE", "HEY", "HOWDY")))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(hiLists(0)) + " contained at most one element of " + decorateToStringValue(Seq("HE", "HEY", "HOWDY")) + ", and " + decorateToStringValue(hiLists(0)) + " contained at most one element of " + decorateToStringValue(Seq("HE", "HEY", "HOWDY")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      "should use an explicitly provided Equality" in {
        (all (hiLists) should (not contain atMostOneElementOf (Seq("HI", "HE")) or not contain atMostOneElementOf (Seq("HE", "HI")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (not contain atMostOneElementOf (Seq("hi", "he")) or not contain atMostOneElementOf (Seq("HE", "HI")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (not contain atMostOneElementOf (Seq("HI", "HE")) or not contain atMostOneElementOf (Seq("hi", "he")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)

        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not contain atMostOneElementOf (Seq("HE", "HEY", "HOWDY")) or not contain atMostOneElementOf (Seq("HE", "HEY", "HOWDY")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(hiLists(0)) + " contained at most one element of " + decorateToStringValue(Seq("HE", "HEY", "HOWDY")) + ", and " + decorateToStringValue(hiLists(0)) + " contained at most one element of " + decorateToStringValue(Seq("HE", "HEY", "HOWDY")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      "should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value" in {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not contain atMostOneElementOf (Seq(1, 2, 2, 3)) or not contain atMostOneElementOf (Seq(1, 2, 3)))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages.atMostOneElementOfDuplicate))

        val e2 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not contain atMostOneElementOf (Seq(1, 2, 3)) or not contain atMostOneElementOf (Seq(1, 2, 2, 3)))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(FailureMessages.atMostOneElementOfDuplicate))
      }
    }

    "when used with (not be (...) and not contain atMostOneElementOf Seq(...))" - {

      "should do nothing if valid, else throw a TFE with an appropriate error message" in {
        all (list1s) should (not be_== (Many(2, 3)) or not contain atMostOneElementOf (Seq(1, 2, 3)))
        all (list1s) should (not be_== (Many(1, 2)) or not contain atMostOneElementOf (Seq(1, 2, 3)))
        all (list1s) should (not be_== (Many(2, 3)) or not contain atMostOneElementOf (Seq(2, 3, 4)))

        val e1 = intercept[TestFailedException] {
          all (list1s) should (not be_== (Many(1, 2)) or not contain atMostOneElementOf (Seq(2, 3, 4)))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(list1s(0)) + " was equal to " + decorateToStringValue(Many(1, 2)) + ", and " + decorateToStringValue(list1s(0)) + " contained at most one element of " + decorateToStringValue(Seq(2, 3, 4)), thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
      }

      "should use the implicit Equality in scope" in {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (not be_== (One("ho")) or not contain atMostOneElementOf (Seq("HI", "HE")))
        all (hiLists) should (not be_== (Many("hi", "he")) or not contain atMostOneElementOf (Seq("HE", "HI")))
        all (hiLists) should (not be_== (One("ho")) or not contain atMostOneElementOf (Seq("HI", "HE")))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not be_== (Many("hi", "he")) or not contain atMostOneElementOf (Seq("HI", "HO")))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(hiLists(0)) + " was equal to " + decorateToStringValue(Many("hi", "he")) + ", and " + decorateToStringValue(hiLists(0)) + " contained at most one element of " + decorateToStringValue(Seq("HI", "HO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      "should use an explicitly provided Equality" in {
        (all (hiLists) should (not be_== (One("ho")) or not contain atMostOneElementOf (Seq("HI", "HE")))) (decided by upperCaseStringEquality)
        (all (hiLists) should (not be_== (Many("hi", "he")) or not contain atMostOneElementOf (Seq("HE", "HI")))) (decided by upperCaseStringEquality)
        (all (hiLists) should (not be_== (One("ho")) or not contain atMostOneElementOf (Seq("HI", "HE")))) (decided by upperCaseStringEquality)

        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not be_== (Many("hi", "he")) or not contain atMostOneElementOf (Seq("HI", "HO")))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(hiLists(0)) + " was equal to " + decorateToStringValue(Many("hi", "he")) + ", and " + decorateToStringValue(hiLists(0)) + " contained at most one element of " + decorateToStringValue(Seq("HI", "HO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      "should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value" in {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not be_== (Many(2, 3)) or not contain atMostOneElementOf (Seq(1, 2, 2, 3)))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages.atMostOneElementOfDuplicate))
      }
    }
  }
}
