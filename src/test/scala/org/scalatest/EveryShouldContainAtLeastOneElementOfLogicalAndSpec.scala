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
import org.scalatest.matchers.{MatchResult, BeMatcher}

class EveryShouldContainAtLeastOneElementOfLogicalAndSpec extends FunSpec {

  val upperCaseStringEquality =
    new Equality[String] {
      def areEqual(a: String, b: Any): Boolean = upperCase(a) == b
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

  val fileName: String = "EveryShouldContainAtLeastOneElementOfLogicalAndSpec.scala"

  describe("an Every") {

    val fumList: Every[String] = Every("fum")
    val toList: Every[String] = Every("to")

    describe("when used with (contain atLeastOneElementOf Seq(..) and contain atLeastOneElementOf Seq(..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (contain atLeastOneElementOf Seq("fee", "fie", "foe", "fum") and contain atLeastOneElementOf Seq("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain atLeastOneElementOf Seq("happy", "birthday", "to", "you") and contain atLeastOneElementOf Seq("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, FailureMessages("didNotContainAtLeastOneElementOf", fumList, Seq("happy", "birthday", "to", "you")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (contain atLeastOneElementOf Seq("fee", "fie", "foe", "fum") and contain atLeastOneElementOf Seq("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e2, FailureMessages("containedAtLeastOneElementOf", fumList, Seq("fee", "fie", "foe", "fum")) + ", but " + FailureMessages("didNotContainAtLeastOneElementOf", fumList, Seq("happy", "birthday", "to", "you")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (contain atLeastOneElementOf Seq("FEE", "FIE", "FOE", "FUM") and contain atLeastOneElementOf Seq("FEE", "FIE", "FUM", "FOE"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain atLeastOneElementOf Seq("fum", "foe") and contain atLeastOneElementOf Seq("FEE", "FIE", "FOE", "FUM"))
        }
        checkMessageStackDepth(e1, FailureMessages("didNotContainAtLeastOneElementOf", fumList, Seq("fum", "foe")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (contain atLeastOneElementOf Seq("FEE", "FIE", "FOE", "FUM") and (contain atLeastOneElementOf Seq("fum", "foe")))
        }
        checkMessageStackDepth(e2, FailureMessages("containedAtLeastOneElementOf", fumList, Seq("FEE", "FIE", "FOE", "FUM")) + ", but " + FailureMessages("didNotContainAtLeastOneElementOf", fumList, Seq("fum", "foe")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (contain atLeastOneElementOf Seq("FEE", "FIE", "FOE", "FUM") and contain atLeastOneElementOf Seq("FEE", "FIE", "FUM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain atLeastOneElementOf Seq("FEE", "FIE", "FOE", "FUM") and contain atLeastOneElementOf Seq("fum", "foe"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages("containedAtLeastOneElementOf", fumList, Seq("FEE", "FIE", "FOE", "FUM")) + ", but " + FailureMessages("didNotContainAtLeastOneElementOf", fumList, Seq("fum", "foe")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (contain atLeastOneElementOf Seq("fum", "foe") and contain atLeastOneElementOf Seq("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, FailureMessages("didNotContainAtLeastOneElementOf", fumList, Seq("fum", "foe")), fileName, thisLineNumber - 2)
        (fumList should (contain atLeastOneElementOf Seq(" FEE ", " FIE ", " FOE ", " FUM ") and contain atLeastOneElementOf Seq(" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (contain atLeastOneElementOf Seq("fee", "fie", "foe", "fie", "fum") and contain atLeastOneElementOf Seq("fie", "fee", "fum", "foe"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("atLeastOneElementOfDuplicate")))

        val e2 = intercept[exceptions.NotAllowedException] {
          fumList should (contain atLeastOneElementOf Seq("fee", "fie", "foe", "fum") and contain atLeastOneElementOf Seq("fee", "fie", "foe", "fie", "fum"))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(FailureMessages("atLeastOneElementOfDuplicate")))
      }
    }

    describe("when used with (equal (..) and contain atLeastOneElementOf Seq(..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (equal (fumList) and contain atLeastOneElementOf Seq("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) and contain atLeastOneElementOf Seq("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, FailureMessages("didNotEqual", fumList, toList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (equal (fumList) and contain atLeastOneElementOf Seq("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e2, FailureMessages("equaled", fumList, fumList) + ", but " + FailureMessages("didNotContainAtLeastOneElementOf", fumList, Seq("happy", "birthday", "to", "you")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (equal (fumList) and contain atLeastOneElementOf Seq("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) and contain atLeastOneElementOf Seq("FEE", "FIE", "FOE", "FUM"))
        }
        checkMessageStackDepth(e1, FailureMessages("didNotEqual", fumList, toList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (equal (fumList) and (contain atLeastOneElementOf Seq("fum", "foe")))
        }
        checkMessageStackDepth(e2, FailureMessages("equaled", fumList, fumList) + ", but " + FailureMessages("didNotContainAtLeastOneElementOf", fumList, Seq("fum", "foe")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (equal (fumList) and contain atLeastOneElementOf Seq("FEE", "FIE", "FOE", "FUM"))) (decided by defaultEquality[Every[String]], decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (equal (fumList) and contain atLeastOneElementOf Seq("fum", "foe"))) (decided by defaultEquality[Every[String]], decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages("equaled", fumList, fumList) + ", but " + FailureMessages("didNotContainAtLeastOneElementOf", fumList, Seq("fum", "foe")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (equal (toList) and contain atLeastOneElementOf Seq("FEE", "FIE", "FOE", "FUM"))) (decided by defaultEquality[Every[String]], decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, FailureMessages("didNotEqual", fumList, toList), fileName, thisLineNumber - 2)
        (fumList should (equal (fumList) and contain atLeastOneElementOf Seq(" FEE ", " FIE ", " FOE ", " FUM "))) (decided by defaultEquality[Every[String]], after being lowerCased and trimmed)
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (equal (fumList) and contain atLeastOneElementOf Seq("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("atLeastOneElementOfDuplicate")))
      }
    }

    describe("when used with (be (..) and contain atLeastOneElementOf Seq(..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (be_== (fumList) and contain atLeastOneElementOf Seq("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (be_== (toList) and contain atLeastOneElementOf Seq("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, FailureMessages("wasNotEqualTo", fumList, toList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (be_== (fumList) and contain atLeastOneElementOf Seq("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e2, FailureMessages("wasEqualTo", fumList, fumList) + ", but " + FailureMessages("didNotContainAtLeastOneElementOf", fumList, Seq("happy", "birthday", "to", "you")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (be_== (fumList) and contain atLeastOneElementOf Seq("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (be_== (toList) and contain atLeastOneElementOf Seq("FEE", "FIE", "FOE", "FUM"))
        }
        checkMessageStackDepth(e1, FailureMessages("wasNotEqualTo", fumList, toList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (be_== (fumList) and (contain atLeastOneElementOf Seq("fum", "foe")))
        }
        checkMessageStackDepth(e2, FailureMessages("wasEqualTo", fumList, fumList) + ", but " + FailureMessages("didNotContainAtLeastOneElementOf", fumList, Seq("fum", "foe")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (be_== (fumList) and contain atLeastOneElementOf Seq("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (be_== (fumList) and contain atLeastOneElementOf Seq("fum", "foe"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages("wasEqualTo", fumList, fumList) + ", but " + FailureMessages("didNotContainAtLeastOneElementOf", fumList, Seq("fum", "foe")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (be_== (toList) and contain atLeastOneElementOf Seq("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, FailureMessages("wasNotEqualTo", fumList, toList), fileName, thisLineNumber - 2)
        (fumList should (be_== (fumList) and contain atLeastOneElementOf Seq(" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed)
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (be_== (fumList) and contain atLeastOneElementOf Seq("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be_== (Some(FailureMessages("atLeastOneElementOfDuplicate")))
      }
    }

    describe("when used with (contain atLeastOneElementOf Seq(..) and be (..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (contain atLeastOneElementOf Seq("fie", "fee", "fum", "foe") and be_== (fumList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain atLeastOneElementOf Seq("fee", "fie", "foe", "fum") and be_== (toList))
        }
        checkMessageStackDepth(e1, FailureMessages("containedAtLeastOneElementOf", fumList, Seq("fee", "fie", "foe", "fum")) + ", but " + FailureMessages("wasNotEqualTo", fumList, toList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (contain atLeastOneElementOf Seq("happy", "birthday", "to", "you") and be_== (fumList))
        }
        checkMessageStackDepth(e2, FailureMessages("didNotContainAtLeastOneElementOf", fumList, Seq("happy", "birthday", "to", "you")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (contain atLeastOneElementOf Seq("FEE", "FIE", "FOE", "FUM") and be_== (fumList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain atLeastOneElementOf Seq("fum", "foe") and be_== (toList))
        }
        checkMessageStackDepth(e1, FailureMessages("didNotContainAtLeastOneElementOf", fumList, Seq("fum", "foe")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (contain atLeastOneElementOf Seq("fum", "foe") and (be_== (fumList)))
        }
        checkMessageStackDepth(e2, FailureMessages("didNotContainAtLeastOneElementOf", fumList, Seq("fum", "foe")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (contain atLeastOneElementOf Seq("FEE", "FIE", "FOE", "FUM") and be_== (fumList))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain atLeastOneElementOf Seq("fum", "foe") and be_== (fumList))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages("didNotContainAtLeastOneElementOf", fumList, Seq("fum", "foe")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (contain atLeastOneElementOf Seq("FEE", "FIE", "FOE", "FUM") and be_== (toList))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, FailureMessages("containedAtLeastOneElementOf", fumList, Seq("FEE", "FIE", "FOE", "FUM")) + ", but " + FailureMessages("wasNotEqualTo", fumList, toList), fileName, thisLineNumber - 2)
        (fumList should (contain atLeastOneElementOf Seq(" FEE ", " FIE ", " FOE ", " FUM ") and be_== (fumList))) (after being lowerCased and trimmed)
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (contain atLeastOneElementOf Seq("fee", "fie", "foe", "fie", "fum") and be_== (fumList))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("atLeastOneElementOfDuplicate")))
      }
    }

    describe("when used with (not contain atLeastOneElementOf Seq(..) and not contain atLeastOneElementOf Seq(..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (not contain atLeastOneElementOf (Seq("fee", "fie", "foe", "fuu")) and not contain atLeastOneElementOf (Seq("fie", "fee", "fuu", "foe")))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain atLeastOneElementOf (Seq("fee", "fie", "foe", "fum")) and not contain atLeastOneElementOf (Seq("happy", "birthday", "to", "you")))
        }
        checkMessageStackDepth(e1, FailureMessages("containedAtLeastOneElementOf", fumList, Seq("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not contain atLeastOneElementOf (Seq("happy", "birthday", "to", "you")) and not contain atLeastOneElementOf (Seq("fee", "fie", "foe", "fum")))
        }
        checkMessageStackDepth(e2, FailureMessages("didNotContainAtLeastOneElementOf", fumList, Seq("happy", "birthday", "to", "you")) + ", but " + FailureMessages("containedAtLeastOneElementOf", fumList, Seq("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (not contain atLeastOneElementOf (Seq("fum", "foe")) and not contain atLeastOneElementOf (Seq("fum", "foe")))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain atLeastOneElementOf (Seq("FEE", "FIE", "FOE", "FUM")) and not contain atLeastOneElementOf (Seq("fum", "foe")))
        }
        checkMessageStackDepth(e1, FailureMessages("containedAtLeastOneElementOf", fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not contain atLeastOneElementOf (Seq("fum", "foe")) and (not contain atLeastOneElementOf (Seq("FEE", "FIE", "FOE", "FUM"))))
        }
        checkMessageStackDepth(e2, FailureMessages("didNotContainAtLeastOneElementOf", fumList, Seq("fum", "foe")) + ", but " + FailureMessages("containedAtLeastOneElementOf", fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (not contain atLeastOneElementOf (Seq("fum", "foe")) and not contain atLeastOneElementOf (Seq("fum", "foe")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not contain atLeastOneElementOf (Seq("fum", "foe")) and not contain atLeastOneElementOf (Seq("FEE", "FIE", "FOE", "FUM")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages("didNotContainAtLeastOneElementOf", fumList, Seq("fum", "foe")) + ", but " + FailureMessages("containedAtLeastOneElementOf", fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (not contain atLeastOneElementOf (Seq("FEE", "FIE", "FOE", "FUM")) and not contain atLeastOneElementOf (Seq("fum", "foe")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, FailureMessages("containedAtLeastOneElementOf", fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
        (fumList should (contain atLeastOneElementOf Seq(" FEE ", " FIE ", " FOE ", " FUM ") and contain atLeastOneElementOf Seq(" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not contain atLeastOneElementOf (Seq("fee", "fie", "foe", "fie", "fum")) and not contain atLeastOneElementOf (Seq("fie", "fee", "fuu", "foe")))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("atLeastOneElementOfDuplicate")))

        val e2 = intercept[exceptions.NotAllowedException] {
          fumList should (not contain atLeastOneElementOf (Seq("fie", "fee", "fuu", "foe")) and not contain atLeastOneElementOf (Seq("fee", "fie", "foe", "fie", "fum")))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(FailureMessages("atLeastOneElementOfDuplicate")))
      }
    }

    describe("when used with (not equal (..) and not contain atLeastOneElementOf Seq(..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (not equal (toList) and not contain atLeastOneElementOf (Seq("fie", "fee", "fuu", "foe")))
        val e1 = intercept[TestFailedException] {
          fumList should (not equal (fumList) and not contain atLeastOneElementOf (Seq("happy", "birthday", "to", "you")))
        }
        checkMessageStackDepth(e1, FailureMessages("equaled", fumList, fumList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not equal (toList) and not contain atLeastOneElementOf (Seq("fee", "fie", "foe", "fum")))
        }
        checkMessageStackDepth(e2, FailureMessages("didNotEqual", fumList, toList) + ", but " + FailureMessages("containedAtLeastOneElementOf", fumList, Seq("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (not equal (toList) and not contain atLeastOneElementOf (Seq("fum", "foe")))
        val e1 = intercept[TestFailedException] {
          fumList should (not equal (fumList) and not contain atLeastOneElementOf (Seq("fum", "foe")))
        }
        checkMessageStackDepth(e1, FailureMessages("equaled", fumList, fumList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not equal (toList) and (not contain atLeastOneElementOf (Seq("FEE", "FIE", "FOE", "FUM"))))
        }
        checkMessageStackDepth(e2, FailureMessages("didNotEqual", fumList, toList) + ", but " + FailureMessages("containedAtLeastOneElementOf", fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (not equal (toList) and not contain atLeastOneElementOf (Seq("fum", "foe")))) (decided by defaultEquality[Every[String]], decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not equal (toList) and not contain atLeastOneElementOf (Seq("FEE", "FIE", "FOE", "FUM")))) (decided by defaultEquality[Every[String]], decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages("didNotEqual", fumList, toList) + ", but " + FailureMessages("containedAtLeastOneElementOf", fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (not equal (fumList) and not contain atLeastOneElementOf (Seq("fum", "foe")))) (decided by defaultEquality[Every[String]], decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, FailureMessages("equaled", fumList, fumList), fileName, thisLineNumber - 2)
        (fumList should (not contain atLeastOneElementOf (Seq(" FEE ", " FIE ", " FOE ", " FUU ")) and not contain atLeastOneElementOf (Seq(" FEE ", " FIE ", " FOE ", " FUU ")))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not equal (toList) and not contain atLeastOneElementOf (Seq("fee", "fie", "foe", "fie", "fum")))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("atLeastOneElementOfDuplicate")))
      }
    }

    describe("when used with (not be (..) and not contain atLeastOneElementOf Seq(..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (not be (toList) and not contain atLeastOneElementOf (Seq("fie", "fee", "fuu", "foe")))
        val e1 = intercept[TestFailedException] {
          fumList should (not be_== (fumList) and not contain atLeastOneElementOf (Seq("happy", "birthday", "to", "you")))
        }
        checkMessageStackDepth(e1, FailureMessages("wasEqualTo", fumList, fumList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not be_== (toList) and not contain atLeastOneElementOf (Seq("fee", "fie", "foe", "fum")))
        }
        checkMessageStackDepth(e2, FailureMessages("wasNotEqualTo", fumList, toList) + ", but " + FailureMessages("containedAtLeastOneElementOf", fumList, Seq("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (not be_== (toList) and not contain atLeastOneElementOf (Seq("fum", "foe")))
        val e1 = intercept[TestFailedException] {
          fumList should (not be_== (fumList) and not contain atLeastOneElementOf (Seq("fum", "foe")))
        }
        checkMessageStackDepth(e1, FailureMessages("wasEqualTo", fumList, fumList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not be_== (toList) and (not contain atLeastOneElementOf (Seq("FEE", "FIE", "FOE", "FUM"))))
        }
        checkMessageStackDepth(e2, FailureMessages("wasNotEqualTo", fumList, toList) + ", but " + FailureMessages("containedAtLeastOneElementOf", fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (not be_== (toList) and not contain atLeastOneElementOf (Seq("fum", "foe")))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not be_== (toList) and not contain atLeastOneElementOf (Seq("FEE", "FIE", "FOE", "FUM")))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages("wasNotEqualTo", fumList, toList) + ", but " + FailureMessages("containedAtLeastOneElementOf", fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (not be_== (fumList) and not contain atLeastOneElementOf (Seq("fum", "foe")))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, FailureMessages("wasEqualTo", fumList, fumList), fileName, thisLineNumber - 2)
        (fumList should (not contain atLeastOneElementOf (Seq(" FEE ", " FIE ", " FOE ", " FUU ")) and not contain atLeastOneElementOf (Seq(" FEE ", " FIE ", " FOE ", " FUU ")))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not be_== (toList) and not contain atLeastOneElementOf (Seq("fee", "fie", "foe", "fie", "fum")))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("atLeastOneElementOfDuplicate")))
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

    describe("when used with (contain atLeastOneElementOf Seq(..) and contain atLeastOneElementOf Seq(..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (contain atLeastOneElementOf Seq(3, 2, 1) and contain atLeastOneElementOf Seq(1, 3, 4))
        atLeast (2, lists) should (contain atLeastOneElementOf Seq(3, 1, 5) and contain atLeastOneElementOf Seq(1, 3, 4))
        atMost (2, lists) should (contain atLeastOneElementOf Seq(3, 2, 8) and contain atLeastOneElementOf Seq(2, 3, 4))
        no (lists) should (contain atLeastOneElementOf Seq(3, 6, 9) and contain atLeastOneElementOf Seq(3, 4, 5))

        val e1 = intercept[TestFailedException] {
          all (lists) should (contain atLeastOneElementOf Seq(1, 6, 8) and contain atLeastOneElementOf Seq(1, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(lists(2)) + " did not contain at least one element of " + decorateToStringValue(Seq(1, 6, 8)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (lists) should (contain atLeastOneElementOf Seq(1, 2, 8) and contain atLeastOneElementOf Seq(1, 3, 4))
        }
        checkMessageStackDepth(e2, allErrMsg(2, decorateToStringValue(lists(2)) + " contained at least one element of " + decorateToStringValue(Seq(1, 2, 8)) + ", but " + decorateToStringValue(lists(2)) + " did not contain at least one element of " + decorateToStringValue(Seq(1, 3, 4)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e3 = intercept[TestFailedException] {
          all (hiLists) should (contain atLeastOneElementOf Seq("hi", "hello") and contain atLeastOneElementOf Seq("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e3, allErrMsg(0, decorateToStringValue(hiLists(0)) + " contained at least one element of " + decorateToStringValue(Seq("hi", "hello")) + ", but " + decorateToStringValue(hiLists(0)) + " did not contain at least one element of " + decorateToStringValue(Seq("ho", "hey", "howdy")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e4 = intercept[TestFailedException] {
          all (lists) should (contain atLeastOneElementOf Seq(1, 2, 8) and contain atLeastOneElementOf Seq(1, 3, 4))
        }
        checkMessageStackDepth(e4, allErrMsg(2, decorateToStringValue(lists(2)) + " contained at least one element of " + decorateToStringValue(Seq(1, 2, 8)) + ", but " + decorateToStringValue(lists(2)) + " did not contain at least one element of " + decorateToStringValue(Seq(1, 3, 4)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (contain atLeastOneElementOf Seq("HI", "HE") and contain atLeastOneElementOf Seq("HI", "HE"))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (contain atLeastOneElementOf Seq("hi", "he") and contain atLeastOneElementOf Seq("HI", "HE"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(hiLists(0)) + " did not contain at least one element of " + decorateToStringValue(Seq("hi", "he")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (hiLists) should (contain atLeastOneElementOf Seq("HI", "HE") and contain atLeastOneElementOf Seq("hi", "he"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(hiLists(0)) + " contained at least one element of " + decorateToStringValue(Seq("HI", "HE")) + ", but " + decorateToStringValue(hiLists(0)) + " did not contain at least one element of " + decorateToStringValue(Seq("hi", "he")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (all (hiLists) should (contain atLeastOneElementOf Seq("HI", "HE") and contain atLeastOneElementOf Seq("HI", "HE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (contain atLeastOneElementOf Seq("hi", "he") and contain atLeastOneElementOf Seq("HI", "HE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(hiLists(0)) + " did not contain at least one element of " + decorateToStringValue(Seq("hi", "he")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (contain atLeastOneElementOf Seq("HI", "HE") and contain atLeastOneElementOf Seq("hi", "he"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(hiLists(0)) + " contained at least one element of " + decorateToStringValue(Seq("HI", "HE")) + ", but " + decorateToStringValue(One("hi")) + " did not contain at least one element of " + decorateToStringValue(Seq("hi", "he")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (contain atLeastOneElementOf Seq(3, 2, 2, 1) and contain atLeastOneElementOf Seq(1, 3, 4))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("atLeastOneElementOfDuplicate")))

        val e2 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (contain atLeastOneElementOf Seq(1, 3, 4) and contain atLeastOneElementOf Seq(3, 2, 2, 1))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(FailureMessages("atLeastOneElementOfDuplicate")))
      }
    }

    describe("when used with (be (..) and contain atLeastOneElementOf Seq(..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (be_== (One(1)) and contain atLeastOneElementOf Seq(1, 3, 4))
        atLeast (2, lists) should (be_== (One(1)) and contain atLeastOneElementOf Seq(1, 3, 4))
        atMost (2, lists) should (be_== (One(1)) and contain atLeastOneElementOf Seq(2, 3, 4))
        no (lists) should (be_== (One(8)) and contain atLeastOneElementOf Seq(3, 4, 5))

        val e1 = intercept[TestFailedException] {
          all (lists) should (be_== (One(1)) and contain atLeastOneElementOf Seq(1, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(One(2)) + " was not equal to " + decorateToStringValue(One(1)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (list1s) should (be_== (One(1)) and contain atLeastOneElementOf Seq(2, 3, 8))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(One(1)) + " was equal to " + decorateToStringValue(One(1)) + ", but " + decorateToStringValue(list1s(0)) + " did not contain at least one element of " + decorateToStringValue(Seq(2, 3, 8)), thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)

        val e3 = intercept[TestFailedException] {
          all (hiLists) should (be_== (One("hi")) and contain atLeastOneElementOf Seq("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e3, allErrMsg(0, decorateToStringValue(One("hi")) + " was equal to " + decorateToStringValue(One("hi")) + ", but " + decorateToStringValue(hiLists(0)) + " did not contain at least one element of " + decorateToStringValue(Seq("ho", "hey", "howdy")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e4 = intercept[TestFailedException] {
          all (list1s) should (be_== (One(1)) and contain atLeastOneElementOf Seq(2, 3, 8))
        }
        checkMessageStackDepth(e4, allErrMsg(0, decorateToStringValue(One(1)) + " was equal to " + decorateToStringValue(One(1)) + ", but " + decorateToStringValue(list1s(0)) + " did not contain at least one element of " + decorateToStringValue(Seq(2, 3, 8)), thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (be_== (One("hi")) and contain atLeastOneElementOf Seq("HI", "HE"))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (be_== (One("ho")) and contain atLeastOneElementOf Seq("HI", "HE"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(One("hi")) + " was not equal to " + decorateToStringValue(One("ho")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (hiLists) should (be_== (One("hi")) and contain atLeastOneElementOf Seq("hi", "he"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(One("hi")) + " was equal to " + decorateToStringValue(One("hi")) + ", but " + decorateToStringValue(hiLists(0)) + " did not contain at least one element of " + decorateToStringValue(Seq("hi", "he")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (all (hiLists) should (be_== (One("hi")) and contain atLeastOneElementOf Seq("HI", "HE"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (be_== (One("ho")) and contain atLeastOneElementOf Seq("HI", "HE"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(One("hi")) + " was not equal to " + decorateToStringValue(One("ho")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (be_== (One("hi")) and contain atLeastOneElementOf Seq("hi", "he"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(One("hi")) + " was equal to " + decorateToStringValue(One("hi")) + ", but " + decorateToStringValue(hiLists(0)) + " did not contain at least one element of " + decorateToStringValue(Seq("hi", "he")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (be_== (One(1)) and contain atLeastOneElementOf Seq(3, 2, 2, 1))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("atLeastOneElementOfDuplicate")))
      }
    }

    describe("when used with (not contain atLeastOneElementOf Seq(..) and not contain atLeastOneElementOf Seq(..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (not contain atLeastOneElementOf (Seq(3, 2, 8)) and not contain atLeastOneElementOf (Seq(8, 3, 4)))
        atLeast (2, lists) should (not contain atLeastOneElementOf (Seq(3, 8, 5)) and not contain atLeastOneElementOf (Seq(8, 3, 4)))
        atMost (2, lists) should (not contain atLeastOneElementOf (Seq(3, 6, 8)) and contain atLeastOneElementOf (Seq(5, 3, 4)))
        no (lists) should (not contain atLeastOneElementOf (Seq(1, 2, 9)) and not contain atLeastOneElementOf (Seq(2, 1, 5)))

        val e1 = intercept[TestFailedException] {
          all (lists) should (not contain atLeastOneElementOf (Seq(2, 6, 8)) and not contain atLeastOneElementOf (Seq(2, 3, 4)))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(lists(2)) + " contained at least one element of " + decorateToStringValue(Seq(2, 6, 8)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (lists) should (not contain atLeastOneElementOf (Seq(3, 6, 8)) and not contain atLeastOneElementOf (Seq(2, 3, 4)))
        }
        checkMessageStackDepth(e2, allErrMsg(2, decorateToStringValue(lists(2)) + " did not contain at least one element of " + decorateToStringValue(Seq(3, 6, 8)) + ", but " + decorateToStringValue(lists(2)) + " contained at least one element of " + decorateToStringValue(Seq(2, 3, 4)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e3 = intercept[TestFailedException] {
          all (hiLists) should (not contain atLeastOneElementOf (Seq("hi", "hello")) and not contain atLeastOneElementOf (Seq("ho", "hey", "howdy")))
        }
        checkMessageStackDepth(e3, allErrMsg(0, decorateToStringValue(hiLists(0)) + " contained at least one element of " + decorateToStringValue(Seq("hi", "hello")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e4 = intercept[TestFailedException] {
          all (hiLists) should (not contain atLeastOneElementOf (Seq("ho", "hey", "howdy")) and not contain atLeastOneElementOf (Seq("hi", "hello")))
        }
        checkMessageStackDepth(e4, allErrMsg(0, decorateToStringValue(hiLists(0)) + " did not contain at least one element of " + decorateToStringValue(Seq("ho", "hey", "howdy")) + ", but " + decorateToStringValue(hiLists(0)) + " contained at least one element of " + decorateToStringValue(Seq("hi", "hello")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (not contain atLeastOneElementOf (Seq("hi", "he")) and not contain atLeastOneElementOf (Seq("hi", "he")))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not contain atLeastOneElementOf (Seq("HI", "HE")) and not contain atLeastOneElementOf (Seq("hi", "he")))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(hiLists(0)) + " contained at least one element of " + decorateToStringValue(Seq("HI", "HE")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (hiLists) should (not contain atLeastOneElementOf (Seq("hi", "he")) and not contain atLeastOneElementOf (Seq("HI", "HE")))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(hiLists(0)) + " did not contain at least one element of " + decorateToStringValue(Seq("hi", "he")) + ", but " + decorateToStringValue(hiLists(0)) + " contained at least one element of " + decorateToStringValue(Seq("HI", "HE")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (all (hiLists) should (not contain atLeastOneElementOf (Seq("hi", "he")) and not contain atLeastOneElementOf (Seq("hi", "he")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not contain atLeastOneElementOf (Seq("HI", "HE")) and not contain atLeastOneElementOf (Seq("hi", "he")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(hiLists(0)) + " contained at least one element of " + decorateToStringValue(Seq("HI", "HE")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (not contain atLeastOneElementOf (Seq("hi", "he")) and not contain atLeastOneElementOf (Seq("HI", "HE")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(hiLists(0)) + " did not contain at least one element of " + decorateToStringValue(Seq("hi", "he")) + ", but " + decorateToStringValue(hiLists(0)) + " contained at least one element of " + decorateToStringValue(Seq("HI", "HE")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not contain atLeastOneElementOf (Seq(3, 2, 2, 1)) and not contain atLeastOneElementOf (Seq(8, 3, 4)))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("atLeastOneElementOfDuplicate")))

        val e2 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not contain atLeastOneElementOf (Seq(8, 3, 4)) and not contain atLeastOneElementOf (Seq(3, 2, 2, 1)))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(FailureMessages("atLeastOneElementOfDuplicate")))
      }
    }

    describe("when used with (not be (..) and not contain atLeastOneElementOf Seq(..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (not be_== (One(2)) and not contain atLeastOneElementOf (Seq(8, 3, 4)))
        atLeast (2, lists) should (not be_== (One(3)) and not contain atLeastOneElementOf (Seq(8, 3, 4)))
        atMost (2, lists) should (not be_== (One(3)) and contain atLeastOneElementOf (Seq(5, 3, 4)))
        no (list1s) should (not be_== (One(1)) and not contain atLeastOneElementOf (Seq(2, 1, 5)))

        val e1 = intercept[TestFailedException] {
          all (lists) should (not be_== (One(2)) and not contain atLeastOneElementOf (Seq(2, 3, 4)))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(One(2)) + " was equal to " + decorateToStringValue(One(2)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (lists) should (not be_== (One(3)) and not contain atLeastOneElementOf (Seq(2, 3, 4)))
        }
        checkMessageStackDepth(e2, allErrMsg(2, decorateToStringValue(One(2)) + " was not equal to " + decorateToStringValue(One(3)) + ", but " + decorateToStringValue(lists(2)) + " contained at least one element of " + decorateToStringValue(Seq(2, 3, 4)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e3 = intercept[TestFailedException] {
          all (hiLists) should (not be_== (One("hi")) and not contain atLeastOneElementOf (Seq("ho", "hey", "howdy")))
        }
        checkMessageStackDepth(e3, allErrMsg(0, decorateToStringValue(One("hi")) + " was equal to " + decorateToStringValue(One("hi")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e4 = intercept[TestFailedException] {
          all (hiLists) should (not be_== (One("ho")) and not contain atLeastOneElementOf (Seq("hi", "hello")))
        }
        checkMessageStackDepth(e4, allErrMsg(0, decorateToStringValue(One("hi")) + " was not equal to " + decorateToStringValue(One("ho")) + ", but " + decorateToStringValue(hiLists(0)) + " contained at least one element of " + decorateToStringValue(Seq("hi", "hello")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (not be_== (One("ho")) and not contain atLeastOneElementOf (Seq("hi", "he")))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not be_== (One("hi")) and not contain atLeastOneElementOf (Seq("hi", "he")))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(One("hi")) + " was equal to " + decorateToStringValue(One("hi")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (hiLists) should (not be_== (One("ho")) and not contain atLeastOneElementOf (Seq("HI", "HE")))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(One("hi")) + " was not equal to " + decorateToStringValue(One("ho")) + ", but " + decorateToStringValue(hiLists(0)) + " contained at least one element of " + decorateToStringValue(Seq("HI", "HE")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (all (hiLists) should (not be_== (One("ho")) and not contain atLeastOneElementOf (Seq("hi", "he")))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not be_== (One("hi")) and not contain atLeastOneElementOf (Seq("hi", "he")))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(One("hi")) + " was equal to " + decorateToStringValue(One("hi")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (not be_== (One("ho")) and not contain atLeastOneElementOf (Seq("HI", "HE")))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(One("hi")) + " was not equal to " + decorateToStringValue(One("ho")) + ", but " + decorateToStringValue(hiLists(0)) + " contained at least one element of " + decorateToStringValue(Seq("HI", "HE")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not be_== (One(2)) and not contain atLeastOneElementOf (Seq(3, 2, 2, 1)))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("atLeastOneElementOfDuplicate")))
      }
    }
  }
}
