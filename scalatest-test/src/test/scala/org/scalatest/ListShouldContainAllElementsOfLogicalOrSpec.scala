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

import org.scalactic.Equality
import org.scalactic.Uniformity
import org.scalactic.Entry
import org.scalactic.StringNormalizations._
import SharedHelpers._
import FailureMessages.decorateToStringValue
import Matchers._
import exceptions.TestFailedException

class ListShouldContainAllElementsOfLogicalOrSpec extends FunSpec {

  val invertedStringEquality =
    new Equality[String] {
      def areEqual(a: String, b: Any): Boolean = a != b
    }

  val invertedListOfStringEquality =
    new Equality[List[String]] {
      def areEqual(a: List[String], b: Any): Boolean = a != b
    }

  private def upperCase(value: Any): Any =
    value match {
      case l: List[_] => l.map(upperCase(_))
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

  val fileName: String = "ListShouldContainAllElementsOfLogicalOrSpec.scala"

  describe("a List") {

    val fumList: List[String] = List("fex", "fum", "foe", "fie", "fee")
    val toList: List[String] = List("too", "you", "to", "birthday", "happy")

    describe("when used with (contain allElementsOf Seq(..) or contain allElementsOf Seq(..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (contain allElementsOf Seq("fee", "fie", "foe", "fum") or contain allElementsOf Seq("fie", "fee", "fum", "foe"))
        fumList should (contain allElementsOf Seq("fee", "fie", "foe", "fam") or contain allElementsOf Seq("fie", "fee", "fum", "foe"))
        fumList should (contain allElementsOf Seq("fee", "fie", "foe", "fum") or contain allElementsOf Seq("fie", "fee", "fam", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain allElementsOf Seq("fee", "fie", "foe", "fam") or contain allElementsOf Seq("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, FailureMessages("didNotContainAllElementsOf", fumList, Seq("fee", "fie", "foe", "fam")) + ", and " + FailureMessages("didNotContainAllElementsOf", fumList, Seq("happy", "birthday", "to", "you")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (contain allElementsOf Seq("FEE", "FIE", "FOE", "FUM") or contain allElementsOf Seq("FIE", "FEE", "FUM", "FOE"))
        fumList should (contain allElementsOf Seq("FEE", "FIE", "FOE", "FAM") or contain allElementsOf Seq("FIE", "FEE", "FUM", "FOE"))
        fumList should (contain allElementsOf Seq("FEE", "FIE", "FOE", "FUM") or contain allElementsOf Seq("FIE", "FEE", "FAM", "FOE"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain allElementsOf Seq("FEE", "FIE", "FOE", "FAM") or (contain allElementsOf Seq("FIE", "FEE", "FAM", "FOE")))
        }
        checkMessageStackDepth(e1, FailureMessages("didNotContainAllElementsOf", fumList, Seq("FEE", "FIE", "FOE", "FAM")) + ", and " + FailureMessages("didNotContainAllElementsOf", fumList, Seq("FIE", "FEE", "FAM", "FOE")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (contain allElementsOf Seq("FEE", "FIE", "FOE", "FUM") or contain allElementsOf Seq("FIE", "FEE", "FUM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (contain allElementsOf Seq("FEE", "FIE", "FOE", "FAM") or contain allElementsOf Seq("FIE", "FEE", "FUM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (contain allElementsOf Seq("FEE", "FIE", "FOE", "FUM") or contain allElementsOf Seq("FIE", "FEE", "FAM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain allElementsOf Seq("FEE", "FIE", "FOE", "FAM") or contain allElementsOf Seq("FIE", "FEE", "FAM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages("didNotContainAllElementsOf", fumList, Seq("FEE", "FIE", "FOE", "FAM")) + ", and " + FailureMessages("didNotContainAllElementsOf", fumList, Seq("FIE", "FEE", "FAM", "FOE")), fileName, thisLineNumber - 2)
        (fumList should (contain allElementsOf Seq(" FEE ", " FIE ", " FOE ", " FUM ") or contain allElementsOf Seq(" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (contain allElementsOf Seq("fee", "fie", "foe", "fie", "fum") or contain allElementsOf Seq("fie", "fee", "fum", "foe"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("allElementsOfDuplicate")))

        val e2 = intercept[exceptions.NotAllowedException] {
          fumList should (contain allElementsOf Seq("fie", "fee", "fum", "foe") or contain allElementsOf Seq("fee", "fie", "foe", "fie", "fum"))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(FailureMessages("allElementsOfDuplicate")))
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
        checkMessageStackDepth(e1, FailureMessages("didNotEqual", fumList, toList) + ", and " + FailureMessages("didNotContainAllElementsOf", fumList, Seq("happy", "birthday", "to", "you")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (equal (fumList) or contain allElementsOf Seq("FIE", "FEE", "FUM", "FOE"))
        fumList should (equal (toList) or contain allElementsOf Seq("FIE", "FEE", "FUM", "FOE"))
        fumList should (equal (fumList) or contain allElementsOf Seq("FIE", "FEE", "FAM", "FOE"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) or (contain allElementsOf Seq("FIE", "FEE", "FAM", "FOE")))
        }
        checkMessageStackDepth(e1, FailureMessages("didNotEqual", fumList, toList) + ", and " + FailureMessages("didNotContainAllElementsOf", fumList, Seq("FIE", "FEE", "FAM", "FOE")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (equal (toList) or contain allElementsOf Seq("FIE", "FEE", "FUM", "FOE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (equal (fumList) or contain allElementsOf Seq("FIE", "FEE", "FUM", "FOE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (equal (toList) or contain allElementsOf Seq("FIE", "FEE", "FAM", "FOE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (equal (fumList) or contain allElementsOf Seq("FIE", "FEE", "FAM", "FOE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages("didNotEqual", fumList, fumList) + ", and " + FailureMessages("didNotContainAllElementsOf", fumList, Seq("FIE", "FEE", "FAM", "FOE")), fileName, thisLineNumber - 2)
        (fumList should (equal (toList) or contain allElementsOf Seq(" FEE ", " FIE ", " FOE ", " FUM "))) (decided by invertedListOfStringEquality, after being lowerCased and trimmed)
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (equal (fumList) or contain allElementsOf Seq("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("allElementsOfDuplicate")))
      }
    }

    describe("when used with (be (..) and contain allElementsOf Seq(..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (be_== (fumList) or contain allElementsOf Seq("fie", "fee", "fum", "foe"))
        fumList should (be_== (toList) or contain allElementsOf Seq("fie", "fee", "fum", "foe"))
        fumList should (be_== (fumList) or contain allElementsOf Seq("fie", "fee", "fam", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (be_== (toList) or contain allElementsOf Seq("fie", "fee", "fam", "foe"))
        }
        checkMessageStackDepth(e1, FailureMessages("wasNotEqualTo", fumList, toList) + ", and " + FailureMessages("didNotContainAllElementsOf", fumList, Seq("fie", "fee", "fam", "foe")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (be_== (fumList) or contain allElementsOf Seq("FIE", "FEE", "FUM", "FOE"))
        fumList should (be_== (toList) or contain allElementsOf Seq("FIE", "FEE", "FUM", "FOE"))
        fumList should (be_== (fumList) or contain allElementsOf Seq("FIE", "FEE", "FAM", "FOE"))
        val e1 = intercept[TestFailedException] {
          fumList should (be_== (toList) or (contain allElementsOf Seq("FIE", "FEE", "FAM", "FOE")))
        }
        checkMessageStackDepth(e1, FailureMessages("wasNotEqualTo", fumList, toList) + ", and " + FailureMessages("didNotContainAllElementsOf", fumList, Seq("FIE", "FEE", "FAM", "FOE")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (be_== (fumList) or contain allElementsOf Seq("FIE", "FEE", "FUM", "FOE"))) (decided by upperCaseStringEquality)
        (fumList should (be_== (toList) or contain allElementsOf Seq("FIE", "FEE", "FUM", "FOE"))) (decided by upperCaseStringEquality)
        (fumList should (be_== (fumList) or contain allElementsOf Seq("FIE", "FEE", "FAM", "FOE"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (be_== (toList) or contain allElementsOf Seq("FIE", "FEE", "FAM", "FOE"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages("wasNotEqualTo", fumList, toList) + ", and " + FailureMessages("didNotContainAllElementsOf", fumList, Seq("FIE", "FEE", "FAM", "FOE")), fileName, thisLineNumber - 2)
        (fumList should (be_== (fumList) or contain allElementsOf Seq(" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed)
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (be_== (fumList) or contain allElementsOf Seq("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("allElementsOfDuplicate")))
      }
    }

    describe("when used with (contain allElementsOf Seq(..) and be (..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (contain allElementsOf Seq("fie", "fee", "fum", "foe") or be_== (fumList))
        fumList should (contain allElementsOf Seq("fie", "fee", "fam", "foe") or be_== (fumList))
        fumList should (contain allElementsOf Seq("fie", "fee", "fum", "foe") or be_== (toList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain allElementsOf Seq("fee", "fie", "foe", "fam") or be_== (toList))
        }
        checkMessageStackDepth(e1, FailureMessages("didNotContainAllElementsOf", fumList, Seq("fee", "fie", "foe", "fam")) + ", and " + FailureMessages("wasNotEqualTo", fumList, toList), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (contain allElementsOf Seq("FIE", "FEE", "FUM", "FOE") or be_== (fumList))
        fumList should (contain allElementsOf Seq("FIE", "FEE", "FAM", "FOE") or be_== (fumList))
        fumList should (contain allElementsOf Seq("FIE", "FEE", "FUM", "FOE") or be_== (toList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain allElementsOf Seq("FEE", "FIE", "FOE", "FAM") or be_== (toList))
        }
        checkMessageStackDepth(e1, FailureMessages("didNotContainAllElementsOf", fumList, Seq("FEE", "FIE", "FOE", "FAM")) + ", and " + FailureMessages("wasNotEqualTo", fumList, toList), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (contain allElementsOf Seq("FIE", "FEE", "FUM", "FOE") or be_== (fumList))) (decided by upperCaseStringEquality)
        (fumList should (contain allElementsOf Seq("FIE", "FEE", "FAM", "FOE") or be_== (fumList))) (decided by upperCaseStringEquality)
        (fumList should (contain allElementsOf Seq("FIE", "FEE", "FUM", "FOE") or be_== (toList))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain allElementsOf Seq("FEE", "FIE", "FOE", "FAM") or be_== (toList))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages("didNotContainAllElementsOf", fumList, Seq("FEE", "FIE", "FOE", "FAM")) + ", and " + FailureMessages("wasNotEqualTo", fumList, toList), fileName, thisLineNumber - 2)
        (fumList should (contain allElementsOf Seq(" FEE ", " FIE ", " FOE ", " FUM ") or be_== (fumList))) (after being lowerCased and trimmed)
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (contain allElementsOf Seq("fee", "fie", "foe", "fie", "fum") or be_== (fumList))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("allElementsOfDuplicate")))
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
        checkMessageStackDepth(e1, FailureMessages("containedAllElementsOf", fumList, Seq("fee", "fie", "foe", "fum")) + ", and " + FailureMessages("containedAllElementsOf", fumList, Seq("fie", "fee", "fum", "foe")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (not contain allElementsOf (Seq("FEE", "FIE", "FOE", "FUU")) or not contain allElementsOf (Seq("FIE", "FEE", "FUU", "FOE")))
        fumList should (not contain allElementsOf (Seq("FEE", "FIE", "FOE", "FUM")) or not contain allElementsOf (Seq("FIE", "FEE", "FUU", "FOE")))
        fumList should (not contain allElementsOf (Seq("FEE", "FIE", "FOE", "FUU")) or not contain allElementsOf (Seq("FIE", "FEE", "FUM", "FOE")))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain allElementsOf (Seq("FEE", "FIE", "FOE", "FUM")) or not contain allElementsOf (Seq("FIE", "FEE", "FUM", "FOE")))
        }
        checkMessageStackDepth(e1, FailureMessages("containedAllElementsOf", fumList, Seq("FEE", "FIE", "FOE", "FUM")) + ", and " + FailureMessages("containedAllElementsOf", fumList, Seq("FIE", "FEE", "FUM", "FOE")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (not contain allElementsOf (Seq("FEE", "FIE", "FOE", "FUU")) or not contain allElementsOf (Seq("FIE", "FEE", "FUU", "FOE")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (not contain allElementsOf (Seq("FEE", "FIE", "FOE", "FUM")) or not contain allElementsOf (Seq("FIE", "FEE", "FUU", "FOE")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (not contain allElementsOf (Seq("FEE", "FIE", "FOE", "FUU")) or not contain allElementsOf (Seq("FIE", "FEE", "FUM", "FOE")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not contain allElementsOf (Seq("FEE", "FIE", "FOE", "FUM")) or not contain allElementsOf (Seq("FIE", "FEE", "FUM", "FOE")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages("containedAllElementsOf", fumList, Seq("FEE", "FIE", "FOE", "FUM")) + ", and " + FailureMessages("containedAllElementsOf", fumList, Seq("FIE", "FEE", "FUM", "FOE")), fileName, thisLineNumber - 2)
        (fumList should (contain allElementsOf Seq(" FEE ", " FIE ", " FOE ", " FUM ") or contain allElementsOf Seq(" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not contain allElementsOf (Seq("fee", "fie", "foe", "fie", "fum")) or not contain allElementsOf (Seq("fie", "fee", "fuu", "foe")))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("allElementsOfDuplicate")))

        val e2 = intercept[exceptions.NotAllowedException] {
          fumList should (not contain allElementsOf (Seq("fie", "fee", "fuu", "foe")) or not contain allElementsOf (Seq("fee", "fie", "foe", "fie", "fum")))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(FailureMessages("allElementsOfDuplicate")))
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
        checkMessageStackDepth(e1, FailureMessages("equaled", fumList, fumList) + ", and " + FailureMessages("containedAllElementsOf", fumList, Seq("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (not equal (toList) or not contain allElementsOf (Seq("FIE", "FEE", "FUU", "FOE")))
        fumList should (not equal (fumList) or not contain allElementsOf (Seq("FIE", "FEE", "FUU", "FOE")))
        fumList should (not equal (toList) or not contain allElementsOf (Seq("FIE", "FEE", "FUM", "FOE")))
        val e2 = intercept[TestFailedException] {
          fumList should (not equal (fumList) or (not contain allElementsOf (Seq("FEE", "FIE", "FOE", "FUM"))))
        }
        checkMessageStackDepth(e2, FailureMessages("equaled", fumList, fumList) + ", and " + FailureMessages("containedAllElementsOf", fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (not equal (fumList) or not contain allElementsOf (Seq("FIE", "FEE", "FUU", "FOE")))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (not equal (toList) or not contain allElementsOf (Seq("FIE", "FEE", "FUU", "FOE")))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (not equal (fumList) or not contain allElementsOf (Seq("FIE", "FEE", "FUM", "FOE")))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not equal (toList) or not contain allElementsOf (Seq("FIE", "FEE", "FUM", "FOE")))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages("equaled", fumList, toList) + ", and " + FailureMessages("containedAllElementsOf", fumList, Seq("FIE", "FEE", "FUM", "FOE")), fileName, thisLineNumber - 2)
        (fumList should (not contain allElementsOf (Seq(" FEE ", " FIE ", " FOE ", " FUU ")) or not contain allElementsOf (Seq(" FEE ", " FIE ", " FOE ", " FUU ")))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not equal (toList) or not contain allElementsOf (Seq("fee", "fie", "foe", "fie", "fum")))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("allElementsOfDuplicate")))
      }
    }

    describe("when used with (not be (..) and not contain allElementsOf Seq(..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (not be_== (toList) or not contain allElementsOf (Seq("fie", "fee", "fuu", "foe")))
        fumList should (not be_== (fumList) or not contain allElementsOf (Seq("fie", "fee", "fuu", "foe")))
        fumList should (not be_== (toList) or not contain allElementsOf (Seq("fee", "fie", "foe", "fum")))
        val e1 = intercept[TestFailedException] {
          fumList should (not be_== (fumList) or not contain allElementsOf (Seq("fee", "fie", "foe", "fum")))
        }
        checkMessageStackDepth(e1, FailureMessages("wasEqualTo", fumList, fumList) + ", and " + FailureMessages("containedAllElementsOf", fumList, Seq("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (not be_== (toList) or not contain allElementsOf (Seq("FIE", "FEE", "FUU", "FOE")))
        fumList should (not be_== (fumList) or not contain allElementsOf (Seq("FIE", "FEE", "FUU", "FOE")))
        fumList should (not be_== (toList) or not contain allElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))
        val e1 = intercept[TestFailedException] {
          fumList should (not be_== (fumList) or (not contain allElementsOf (Seq("FEE", "FIE", "FOE", "FUM"))))
        }
        checkMessageStackDepth(e1, FailureMessages("wasEqualTo", fumList, fumList) + ", and " + FailureMessages("containedAllElementsOf", fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (not be_== (toList) or not contain allElementsOf (Seq("FIE", "FEE", "FUU", "FOE")))) (decided by upperCaseStringEquality)
        (fumList should (not be_== (fumList) or not contain allElementsOf (Seq("FIE", "FEE", "FUU", "FOE")))) (decided by upperCaseStringEquality)
        (fumList should (not be_== (toList) or not contain allElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not be_== (fumList) or not contain allElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages("wasEqualTo", fumList, fumList) + ", and " + FailureMessages("containedAllElementsOf", fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
        (fumList should (not contain allElementsOf (Seq(" FEE ", " FIE ", " FOE ", " FUU ")) or not contain allElementsOf (Seq(" FEE ", " FIE ", " FOE ", " FUU ")))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not be_== (toList) or not contain allElementsOf (Seq("fee", "fie", "foe", "fie", "fum")))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("allElementsOfDuplicate")))
      }
    }

  }

  describe("collection of Lists") {

    val list1s: Vector[List[Int]] = Vector(List(3, 2, 1, 0), List(3, 2, 1, 0), List(3, 2, 1, 0))
    val lists: Vector[List[Int]] = Vector(List(3, 2, 1, 0), List(3, 2, 1, 0), List(8, 4, 3, 2))
    val nils: Vector[List[Int]] = Vector(Nil, Nil, Nil)
    val listsNil: Vector[List[Int]] = Vector(List(3, 2, 1, 0), List(3, 2, 1, 0), Nil)
    val hiLists: Vector[List[String]] = Vector(List("howdy", "hi", "hello"), List("howdy", "hi", "hello"), List("howdy", "hi", "hello"))
    val toLists: Vector[List[String]] = Vector(List("nice", "you", "to"), List("nice", "you", "to"), List("nice", "you", "to"))

    def allErrMsg(index: Int, message: String, lineNumber: Int, left: Any): String =
      "'all' inspection failed, because: \n" +
        "  at index " + index + ", " + message + " (" + fileName + ":" + (lineNumber) + ") \n" +
        "in " + decorateToStringValue(left)

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
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(lists(2)) + " did not contain all elements of " + decorateToStringValue(Seq(3, 1, 2)) + ", and " + decorateToStringValue(lists(2)) + " did not contain all elements of " + decorateToStringValue(Seq(1, 3, 2)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (contain allElementsOf Seq("HELLO", "HI") or contain allElementsOf Seq("hello", "hi"))
        all (hiLists) should (contain allElementsOf Seq("HELLO", "HO") or contain allElementsOf Seq("hello", "hi"))
        all (hiLists) should (contain allElementsOf Seq("HELLO", "HI") or contain allElementsOf Seq("hello", "ho"))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (contain allElementsOf Seq("HELLO", "HO") or contain allElementsOf Seq("hello", "ho"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(hiLists(0)) + " did not contain all elements of " + decorateToStringValue(Seq("HELLO", "HO")) + ", and " + decorateToStringValue(hiLists(0)) + " did not contain all elements of " + decorateToStringValue(Seq("hello", "ho")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (all (hiLists) should (contain allElementsOf Seq("HELLO", "HI") or contain allElementsOf Seq("hello", "hi"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (contain allElementsOf Seq("HELLO", "HO") or contain allElementsOf Seq("hello", "hi"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (contain allElementsOf Seq("HELLO", "HI") or contain allElementsOf Seq("hello", "ho"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)

        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (contain allElementsOf Seq("HELLO", "HO") or contain allElementsOf Seq("hello", "ho"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(hiLists(0)) + " did not contain all elements of " + decorateToStringValue(Seq("HELLO", "HO")) + ", and " + decorateToStringValue(hiLists(0)) + " did not contain all elements of " + decorateToStringValue(Seq("hello", "ho")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (contain allElementsOf Seq(3, 2, 2, 1) or contain allElementsOf Seq(1, 3, 2))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("allElementsOfDuplicate")))

        val e2 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (contain allElementsOf Seq(1, 3, 2) or contain allElementsOf Seq(3, 2, 2, 1))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(FailureMessages("allElementsOfDuplicate")))
      }
    }

    describe("when used with (be (..) and contain allElementsOf Seq(..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (be_== (List(3, 2, 1, 0)) or contain allElementsOf Seq(1, 2, 3))
        all (list1s) should (be_== (List(2, 3, 4)) or contain allElementsOf Seq(1, 2, 3))
        all (list1s) should (be_== (List(3, 2, 1, 0)) or contain allElementsOf Seq(2, 3, 4))

        val e1 = intercept[TestFailedException] {
          all (list1s) should (be_== (List(2, 3, 4)) or contain allElementsOf Seq(2, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(list1s(0)) + " was not equal to " + decorateToStringValue(List(2, 3, 4)) + ", and " + decorateToStringValue(list1s(0)) + " did not contain all elements of " + decorateToStringValue(Seq(2, 3, 4)), thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (be_== (List("howdy", "hi", "hello")) or contain allElementsOf Seq("HELLO", "HI"))
        all (hiLists) should (be_== (List("ho", "hello")) or contain allElementsOf Seq("HELLO", "HI"))
        all (hiLists) should (be_== (List("howdy", "hi", "hello")) or contain allElementsOf Seq("HELLO", "HO"))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (be_== (List("ho", "hello")) or contain allElementsOf Seq("HELLO", "HO"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(hiLists(0)) + " was not equal to " + decorateToStringValue(List("ho", "hello")) + ", and " + decorateToStringValue(hiLists(0)) + " did not contain all elements of " + decorateToStringValue(Seq("HELLO", "HO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (all (hiLists) should (be_== (List("howdy", "hi", "hello")) or contain allElementsOf Seq("HELLO", "HI"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (be_== (List("ho", "hello")) or contain allElementsOf Seq("HELLO", "HI"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (be_== (List("howdy", "hi", "hello")) or contain allElementsOf Seq("HELLO", "HO"))) (decided by upperCaseStringEquality)

        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (be_== (List("ho", "hello")) or contain allElementsOf Seq("HELLO", "HO"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(hiLists(0)) + " was not equal to " + decorateToStringValue(List("ho", "hello")) + ", and " + decorateToStringValue(hiLists(0)) + " did not contain all elements of " + decorateToStringValue(Seq("HELLO", "HO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (be_== (List(3, 2, 1, 0)) or contain allElementsOf Seq(1, 2, 2, 3))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("allElementsOfDuplicate")))
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
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(lists(2)) + " contained all elements of " + decorateToStringValue(Seq(4, 2, 3)) + ", and " + decorateToStringValue(lists(2)) + " contained all elements of " + decorateToStringValue(Seq(2, 3, 4)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (not contain allElementsOf (Seq("HELLO", "HO")) or not contain allElementsOf (Seq("hello", "ho")))
        all (hiLists) should (not contain allElementsOf (Seq("HELLO", "HI")) or not contain allElementsOf (Seq("hello", "ho")))
        all (hiLists) should (not contain allElementsOf (Seq("HELLO", "HO")) or not contain allElementsOf (Seq("hello", "hi")))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not contain allElementsOf (Seq("HELLO", "HI")) or not contain allElementsOf (Seq("hello", "hi")))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(hiLists(0)) + " contained all elements of " + decorateToStringValue(Seq("HELLO", "HI")) + ", and " + decorateToStringValue(hiLists(0)) + " contained all elements of " + decorateToStringValue(Seq("hello", "hi")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (all (hiLists) should (not contain allElementsOf (Seq("HELLO", "HO")) or not contain allElementsOf (Seq("hello", "ho")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (not contain allElementsOf (Seq("HELLO", "HI")) or not contain allElementsOf (Seq("hello", "ho")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (not contain allElementsOf (Seq("HELLO", "HO")) or not contain allElementsOf (Seq("hello", "hi")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)

        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not contain allElementsOf (Seq("HELLO", "HI")) or not contain allElementsOf (Seq("hello", "hi")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(hiLists(0)) + " contained all elements of " + decorateToStringValue(Seq("HELLO", "HI")) + ", and " + decorateToStringValue(hiLists(0)) + " contained all elements of " + decorateToStringValue(Seq("hello", "hi")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not contain allElementsOf (Seq(1, 2, 2, 3)) or not contain allElementsOf (Seq(8, 3, 4)))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("allElementsOfDuplicate")))

        val e2 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not contain allElementsOf (Seq(8, 3, 4)) or not contain allElementsOf (Seq(1, 2, 2, 3)))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(FailureMessages("allElementsOfDuplicate")))
      }
    }

    describe("when used with (not be (...) and not contain allElementsOf Seq(...))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (not be_== (List(2)) or not contain allElementsOf (Seq(8, 3, 4)))
        all (list1s) should (not be_== (List(3, 2, 1, 0)) or not contain allElementsOf (Seq(8, 3, 4)))
        all (list1s) should (not be_== (List(2)) or not contain allElementsOf (Seq(1, 2, 3)))

        val e1 = intercept[TestFailedException] {
          all (list1s) should (not be_== (List(3, 2, 1, 0)) or not contain allElementsOf (Seq(2, 3, 1)))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(list1s(0)) + " was equal to " + decorateToStringValue(List(3, 2, 1, 0)) + ", and " + decorateToStringValue(list1s(0)) + " contained all elements of " + decorateToStringValue(Seq(2, 3, 1)), thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (not be_== (List("hello", "ho")) or not contain allElementsOf (Seq("HELLO", "HO")))
        all (hiLists) should (not be_== (List("howdy", "hello", "hi")) or not contain allElementsOf (Seq("HELLO", "HO")))
        all (hiLists) should (not be_== (List("hello", "ho")) or not contain allElementsOf (Seq("HELLO", "HI")))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not be_== (List("howdy", "hi", "hello")) or not contain allElementsOf (Seq("HELLO", "HI")))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(hiLists(0)) + " was equal to " + decorateToStringValue(List("howdy", "hi", "hello")) + ", and " + decorateToStringValue(hiLists(0)) + " contained all elements of " + decorateToStringValue(Seq("HELLO", "HI")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (all (hiLists) should (not be_== (List("hello", "ho")) or not contain allElementsOf (Seq("HELLO", "HO")))) (decided by upperCaseStringEquality)
        (all (hiLists) should (not be_== (List("howdy", "hello", "hi")) or not contain allElementsOf (Seq("HELLO", "HO")))) (decided by upperCaseStringEquality)
        (all (hiLists) should (not be_== (List("hello", "ho")) or not contain allElementsOf (Seq("HELLO", "HI")))) (decided by upperCaseStringEquality)

        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not be_== (List("howdy", "hi", "hello")) or not contain allElementsOf (Seq("HELLO", "HI")))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(hiLists(0)) + " was equal to " + decorateToStringValue(List("howdy", "hi", "hello")) + ", and " + decorateToStringValue(hiLists(0)) + " contained all elements of " + decorateToStringValue(Seq("HELLO", "HI")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not be_== (List(2)) or not contain allElementsOf (Seq(1, 2, 2, 3)))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("allElementsOfDuplicate")))
      }
    }
  }
}
