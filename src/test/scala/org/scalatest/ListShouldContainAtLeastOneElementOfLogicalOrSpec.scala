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

class ListShouldContainAtLeastOneElementOfLogicalOrSpec extends FreeSpec {

  val upperCaseStringEquality =
    new Equality[String] {
      def areEqual(a: String, b: Any): Boolean = upperCase(a) == b
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

  //ADDITIONAL//

  val fileName: String = "ListShouldContainAtLeastOneElementOfLogicalOrSpec.scala"

  "a List" - {

    val fumList: List[String] = List("fum")
    val toList: List[String] = List("to")

    "when used with (contain atLeastOneElementOf (...) or contain atLeastOneElementOf (...)) syntax" - {

      "should do nothing if valid, else throw a TFE with an appropriate error message" in {
        fumList should (contain atLeastOneElementOf Seq("fee", "fie", "foe", "fum") or contain atLeastOneElementOf Seq("fie", "fee", "fum", "foe"))
        fumList should (contain atLeastOneElementOf Seq("fee", "fie", "foe", "fam") or contain atLeastOneElementOf Seq("fie", "fee", "fum", "foe"))
        fumList should (contain atLeastOneElementOf Seq("fee", "fie", "foe", "fum") or contain atLeastOneElementOf Seq("fie", "fee", "fam", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain atLeastOneElementOf Seq("fee", "fie", "foe", "fam") or contain atLeastOneElementOf Seq("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, FailureMessages("didNotContainAtLeastOneElementOf", fumList, Seq("fee", "fie", "foe", "fam")) + ", and " + FailureMessages("didNotContainAtLeastOneElementOf", fumList, Seq("happy", "birthday", "to", "you")), fileName, thisLineNumber - 2)
      }

      "should use the implicit Equality in scope" in {
        implicit val ise = upperCaseStringEquality
        fumList should (contain atLeastOneElementOf Seq("FIE", "FEE", "FUM", "FOE") or contain atLeastOneElementOf Seq("FIE", "FEE", "FOE", "FUM"))
        fumList should (contain atLeastOneElementOf Seq("fie", "fee", "fum", "foe") or contain atLeastOneElementOf Seq("FIE", "FEE", "FOE", "FUM"))
        fumList should (contain atLeastOneElementOf Seq("FIE", "FEE", "FUM", "FOE") or contain atLeastOneElementOf Seq("fie", "fee", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain atLeastOneElementOf Seq("fum", "foe") or (contain atLeastOneElementOf Seq("fie", "fee", "foe", "fum")))
        }
        checkMessageStackDepth(e1, FailureMessages("didNotContainAtLeastOneElementOf", fumList, Seq("fum", "foe")) + ", and " + FailureMessages("didNotContainAtLeastOneElementOf", fumList, Seq("fie", "fee", "foe", "fum")), fileName, thisLineNumber - 2)
      }

      "should use an explicitly provided Equality" in {
        (fumList should (contain atLeastOneElementOf Seq("FIE", "FEE", "FUM", "FOE") or contain atLeastOneElementOf Seq("FIE", "FEE", "FOE", "FUM"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (contain atLeastOneElementOf Seq("fie", "fee", "fum", "foe") or contain atLeastOneElementOf Seq("FIE", "FEE", "FOE", "FUM"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (contain atLeastOneElementOf Seq("FIE", "FEE", "FUM", "FOE") or contain atLeastOneElementOf Seq("fie", "fee", "foe", "fum"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain atLeastOneElementOf Seq("fum", "foe") or contain atLeastOneElementOf Seq("fie", "fee", "foe", "fum"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages("didNotContainAtLeastOneElementOf", fumList, Seq("fum", "foe")) + ", and " + FailureMessages("didNotContainAtLeastOneElementOf", fumList, Seq("fie", "fee", "foe", "fum")), fileName, thisLineNumber - 2)
        (fumList should (contain atLeastOneElementOf Seq(" FEE ", " FIE ", " FOE ", " FUM ") or contain atLeastOneElementOf Seq(" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }

      "should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value" in {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (contain atLeastOneElementOf Seq("fee", "fie", "foe", "fie", "fum") or contain atLeastOneElementOf Seq("fie", "fee", "fum", "foe"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("atLeastOneElementOfDuplicate")))

        val e2 = intercept[exceptions.NotAllowedException] {
          fumList should (contain atLeastOneElementOf Seq("fie", "fee", "fum", "foe") or contain atLeastOneElementOf Seq("fee", "fie", "foe", "fie", "fum"))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(FailureMessages("atLeastOneElementOfDuplicate")))
      }
    }

    "when used with (equal (...) and contain atLeastOneElementOf (...)) syntax" - {

      "should do nothing if valid, else throw a TFE with an appropriate error message" in {
        fumList should (equal (fumList) or contain atLeastOneElementOf Seq("fie", "fee", "fum", "foe"))
        fumList should (equal (toList) or contain atLeastOneElementOf Seq("fie", "fee", "fum", "foe"))
        fumList should (equal (fumList) or contain atLeastOneElementOf Seq("fie", "fee", "fam", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) or contain atLeastOneElementOf Seq("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, FailureMessages("didNotEqual", fumList, toList) + ", and " + FailureMessages("didNotContainAtLeastOneElementOf", fumList, Seq("happy", "birthday", "to", "you")), fileName, thisLineNumber - 2)
      }

      "should use the implicit Equality in scope" in {
        implicit val ise = upperCaseStringEquality
        fumList should (equal (fumList) or contain atLeastOneElementOf Seq("FEE", "FIE", "FOE", "FUM"))
        fumList should (equal (toList) or contain atLeastOneElementOf Seq("FEE", "FIE", "FOE", "FUM"))
        fumList should (equal (fumList) or contain atLeastOneElementOf Seq("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) or (contain atLeastOneElementOf Seq("fum", "foe")))
        }
        checkMessageStackDepth(e1, FailureMessages("didNotEqual", fumList, toList) + ", and " + FailureMessages("didNotContainAtLeastOneElementOf", fumList, Seq("fum", "foe")), fileName, thisLineNumber - 2)
      }

      "should use an explicitly provided Equality" in {
        (fumList should (equal (fumList) or contain atLeastOneElementOf Seq("FEE", "FIE", "FOE", "FUM"))) (decided by defaultEquality[List[String]], decided by upperCaseStringEquality)
        (fumList should (equal (toList) or contain atLeastOneElementOf Seq("FEE", "FIE", "FOE", "FUM"))) (decided by defaultEquality[List[String]], decided by upperCaseStringEquality)
        (fumList should (equal (fumList) or contain atLeastOneElementOf Seq("fum", "foe"))) (decided by defaultEquality[List[String]], decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (equal (toList) or contain atLeastOneElementOf Seq("fum", "foe"))) (decided by defaultEquality[List[String]], decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages("didNotEqual", fumList, toList) + ", and " + FailureMessages("didNotContainAtLeastOneElementOf", fumList, Seq("fum", "foe")), fileName, thisLineNumber - 2)
        (fumList should (equal (toList) or contain atLeastOneElementOf Seq(" FEE ", " FIE ", " FOE ", " FUM "))) (decided by defaultEquality[List[String]], after being lowerCased and trimmed)
      }

      "should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value" in {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (equal (fumList) or contain atLeastOneElementOf Seq("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("atLeastOneElementOfDuplicate")))
      }
    }

    "when used with (be (...) and contain atLeastOneElementOf (...)) syntax" - {

      "should do nothing if valid, else throw a TFE with an appropriate error message" in {
        fumList should (be_== (fumList) or contain atLeastOneElementOf Seq("fie", "fee", "fum", "foe"))
        fumList should (be_== (toList) or contain atLeastOneElementOf Seq("fie", "fee", "fum", "foe"))
        fumList should (be_== (fumList) or contain atLeastOneElementOf Seq("fie", "fee", "fam", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (be_== (toList) or contain atLeastOneElementOf Seq("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, FailureMessages("wasNotEqualTo", fumList, toList) + ", and " + FailureMessages("didNotContainAtLeastOneElementOf", fumList, Seq("happy", "birthday", "to", "you")), fileName, thisLineNumber - 2)
      }

      "should use the implicit Equality in scope" in {
        implicit val ise = upperCaseStringEquality
        fumList should (be_== (fumList) or contain atLeastOneElementOf Seq("FIE", "FEE", "FUM", "FOE"))
        fumList should (be_== (toList) or contain atLeastOneElementOf Seq("FIE", "FEE", "FUM", "FOE"))
        fumList should (be_== (fumList) or contain atLeastOneElementOf Seq("fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (be_== (toList) or (contain atLeastOneElementOf Seq("fum", "foe")))
        }
        checkMessageStackDepth(e1, FailureMessages("wasNotEqualTo", fumList, toList) + ", and " + FailureMessages("didNotContainAtLeastOneElementOf", fumList, Seq("fum", "foe")), fileName, thisLineNumber - 2)
      }

      "should use an explicitly provided Equality" in {
        (fumList should (be_== (fumList) or contain atLeastOneElementOf Seq("FIE", "FEE", "FUM", "FOE"))) (decided by upperCaseStringEquality)
        (fumList should (be_== (toList) or contain atLeastOneElementOf Seq("FIE", "FEE", "FUM", "FOE"))) (decided by upperCaseStringEquality)
        (fumList should (be_== (fumList) or contain atLeastOneElementOf Seq("fum", "foe"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (be_== (toList) or contain atLeastOneElementOf Seq("fum", "foe"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages("wasNotEqualTo", fumList, toList) + ", and " + FailureMessages("didNotContainAtLeastOneElementOf", fumList, Seq("fum", "foe")), fileName, thisLineNumber - 2)
        (fumList should (be_== (fumList) or contain atLeastOneElementOf Seq(" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed)
      }

      "should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value" in {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (be_== (fumList) or contain atLeastOneElementOf Seq("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("atLeastOneElementOfDuplicate")))
      }
    }

    "when used with (contain atLeastOneElementOf (...) and be (...)) syntax" - {

      "should do nothing if valid, else throw a TFE with an appropriate error message" in {
        fumList should (contain atLeastOneElementOf Seq("fie", "fee", "fum", "foe") or be_== (fumList))
        fumList should (contain atLeastOneElementOf Seq("fie", "fee", "fam", "foe") or be_== (fumList))
        fumList should (contain atLeastOneElementOf Seq("fie", "fee", "fum", "foe") or be_== (toList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain atLeastOneElementOf Seq("fee", "fie", "foe", "fam") or be_== (toList))
        }
        checkMessageStackDepth(e1, FailureMessages("didNotContainAtLeastOneElementOf", fumList, Seq("fee", "fie", "foe", "fam")) + ", and " + FailureMessages("wasNotEqualTo", fumList, toList), fileName, thisLineNumber - 2)
      }

      "should use the implicit Equality in scope" in {
        implicit val ise = upperCaseStringEquality
        fumList should (contain atLeastOneElementOf Seq("FIE", "FEE", "FUM", "FOE") or be_== (fumList))
        fumList should (contain atLeastOneElementOf Seq("fum", "foe") or be_== (fumList))
        fumList should (contain atLeastOneElementOf Seq("FIE", "FEE", "FUM", "FOE") or be_== (toList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain atLeastOneElementOf Seq("fum", "foe") or be_== (toList))
        }
        checkMessageStackDepth(e1, FailureMessages("didNotContainAtLeastOneElementOf", fumList, Seq("fum", "foe")) + ", and " + FailureMessages("wasNotEqualTo", fumList, toList), fileName, thisLineNumber - 2)
      }

      "should use an explicitly provided Equality" in {
        (fumList should (contain atLeastOneElementOf Seq("FIE", "FEE", "FUM", "FOE") or be_== (fumList))) (decided by upperCaseStringEquality)
        (fumList should (contain atLeastOneElementOf Seq("fum", "foe") or be_== (fumList))) (decided by upperCaseStringEquality)
        (fumList should (contain atLeastOneElementOf Seq("FIE", "FEE", "FUM", "FOE") or be_== (toList))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain atLeastOneElementOf Seq("fum", "foe") or be_== (toList))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages("didNotContainAtLeastOneElementOf", fumList, Seq("fum", "foe")) + ", and " + FailureMessages("wasNotEqualTo", fumList, toList), fileName, thisLineNumber - 2)
        (fumList should (contain atLeastOneElementOf Seq(" FEE ", " FIE ", " FOE ", " FUM ") or be_== (fumList))) (after being lowerCased and trimmed)
      }

      "should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value" in {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (contain atLeastOneElementOf Seq("fee", "fie", "foe", "fie", "fum") or be_== (fumList))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("atLeastOneElementOfDuplicate")))
      }
    }

    "when used with (not contain atLeastOneElementOf (...) and not contain atLeastOneElementOf (...)) syntax" - {

      "should do nothing if valid, else throw a TFE with an appropriate error message" in {
        fumList should (not contain atLeastOneElementOf (Seq("fee", "fie", "foe", "fuu")) or not contain atLeastOneElementOf (Seq("fie", "fee", "fuu", "foe")))
        fumList should (not contain atLeastOneElementOf (Seq("fee", "fie", "foe", "fum")) or not contain atLeastOneElementOf (Seq("fie", "fee", "fuu", "foe")))
        fumList should (not contain atLeastOneElementOf (Seq("fee", "fie", "foe", "fuu")) or not contain atLeastOneElementOf (Seq("fie", "fee", "fum", "foe")))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain atLeastOneElementOf (Seq("fee", "fie", "foe", "fum")) or not contain atLeastOneElementOf (Seq("fee", "fie", "foe", "fum")))
        }
        checkMessageStackDepth(e1, FailureMessages("containedAtLeastOneElementOf", fumList, Seq("fee", "fie", "foe", "fum")) + ", and " + FailureMessages("containedAtLeastOneElementOf", fumList, Seq("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)
      }

      "should use the implicit Equality in scope" in {
        implicit val ise = upperCaseStringEquality
        fumList should (not contain atLeastOneElementOf (Seq("fum", "foe")) or not contain atLeastOneElementOf (Seq("fum", "foe")))
        fumList should (not contain atLeastOneElementOf (Seq("FEE", "FIE", "FOE", "FUM")) or not contain atLeastOneElementOf (Seq("fum", "foe")))
        fumList should (not contain atLeastOneElementOf (Seq("fum", "foe")) or not contain atLeastOneElementOf (Seq("FEE", "FIE", "FOE", "FUM")))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain atLeastOneElementOf (Seq("FEE", "FIE", "FOE", "FUM")) or not contain atLeastOneElementOf (Seq("FEE", "FIE", "FUM", "FOE")))
        }
        checkMessageStackDepth(e1, FailureMessages("containedAtLeastOneElementOf", fumList, Seq("FEE", "FIE", "FOE", "FUM")) + ", and " + FailureMessages("containedAtLeastOneElementOf", fumList, Seq("FEE", "FIE", "FUM", "FOE")), fileName, thisLineNumber - 2)
      }

      "should use an explicitly provided Equality" in {
        (fumList should (not contain atLeastOneElementOf (Seq("fum", "foe")) or not contain atLeastOneElementOf (Seq("fum", "foe")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (not contain atLeastOneElementOf (Seq("FIE", "FEE", "FUM", "FOE")) or not contain atLeastOneElementOf (Seq("fum", "foe")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (not contain atLeastOneElementOf (Seq("fum", "foe")) or not contain atLeastOneElementOf (Seq("FIE", "FEE", "FUM", "FOE")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not contain atLeastOneElementOf (Seq("FEE", "FIE", "FOE", "FUM")) or not contain atLeastOneElementOf (Seq("FEE", "FIE", "FUM", "FOE")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages("containedAtLeastOneElementOf", fumList, Seq("FEE", "FIE", "FOE", "FUM")) + ", and " + FailureMessages("containedAtLeastOneElementOf", fumList, Seq("FEE", "FIE", "FUM", "FOE")), fileName, thisLineNumber - 2)
        (fumList should (contain atLeastOneElementOf (Seq(" FEE ", " FIE ", " FOE ", " FUM ")) or contain atLeastOneElementOf (Seq(" FEE ", " FIE ", " FOE ", " FUM ")))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }

      "should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value" in {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not contain atLeastOneElementOf (Seq("fee", "fie", "foe", "fie", "fum")) or not contain atLeastOneElementOf (Seq("fie", "fee", "fuu", "foe")))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("atLeastOneElementOfDuplicate")))

        val e2 = intercept[exceptions.NotAllowedException] {
          fumList should (not contain atLeastOneElementOf (Seq("fie", "fee", "fuu", "foe")) or not contain atLeastOneElementOf (Seq("fee", "fie", "foe", "fie", "fum")))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(FailureMessages("atLeastOneElementOfDuplicate")))
      }
    }

    "when used with (not equal (...) and not contain atLeastOneElementOf (...)) syntax" - {

      "should do nothing if valid, else throw a TFE with an appropriate error message" in {
        fumList should (not equal (toList) or not contain atLeastOneElementOf (Seq("fie", "fee", "fuu", "foe")))
        fumList should (not equal (fumList) or not contain atLeastOneElementOf (Seq("fie", "fee", "fuu", "foe")))
        fumList should (not equal (toList) or not contain atLeastOneElementOf (Seq("fie", "fee", "fum", "foe")))
        val e1 = intercept[TestFailedException] {
          fumList should (not equal (fumList) or not contain atLeastOneElementOf (Seq("fee", "fie", "foe", "fum")))
        }
        checkMessageStackDepth(e1, FailureMessages("equaled", fumList, fumList) + ", and " + FailureMessages("containedAtLeastOneElementOf", fumList, Seq("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)
      }

      "should use the implicit Equality in scope" in {
        implicit val ise = upperCaseStringEquality
        fumList should (not equal (toList) or not contain atLeastOneElementOf (Seq("fum", "foe")))
        fumList should (not equal (fumList) or not contain atLeastOneElementOf (Seq("fum", "foe")))
        fumList should (not equal (toList) or not contain atLeastOneElementOf (Seq("FEE", "FIE", "FOE", "FUM")))
        val e2 = intercept[TestFailedException] {
          fumList should (not equal (fumList) or (not contain atLeastOneElementOf (Seq("FEE", "FIE", "FOE", "FUM"))))
        }
        checkMessageStackDepth(e2, FailureMessages("equaled", fumList, fumList) + ", and " + FailureMessages("containedAtLeastOneElementOf", fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
      }

      "should use an explicitly provided Equality" in {
        (fumList should (not equal (toList) or not contain atLeastOneElementOf (Seq("fum", "foe")))) (decided by defaultEquality[List[String]], decided by upperCaseStringEquality)
        (fumList should (not equal (fumList) or not contain atLeastOneElementOf (Seq("fum", "foe")))) (decided by defaultEquality[List[String]], decided by upperCaseStringEquality)
        (fumList should (not equal (toList) or not contain atLeastOneElementOf (Seq("FEE", "FIE", "FOE", "FUM")))) (decided by defaultEquality[List[String]], decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not equal (fumList) or not contain atLeastOneElementOf (Seq("FEE", "FIE", "FOE", "FUM")))) (decided by defaultEquality[List[String]], decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages("equaled", fumList, fumList) + ", and " + FailureMessages("containedAtLeastOneElementOf", fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
        (fumList should (not contain atLeastOneElementOf (Seq(" FEE ", " FIE ", " FOE ", " FUU ")) or not contain atLeastOneElementOf (Seq(" FEE ", " FIE ", " FOE ", " FUU ")))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }

      "should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value" in {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not equal (toList) or not contain atLeastOneElementOf (Seq("fee", "fie", "foe", "fie", "fum")))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("atLeastOneElementOfDuplicate")))
      }
    }

    "when used with (not be (...) and not contain atLeastOneElementOf (...)) syntax" - {

      "should do nothing if valid, else throw a TFE with an appropriate error message" in {
        fumList should (not be_== (toList) or not contain atLeastOneElementOf (Seq("fie", "fee", "fuu", "foe")))
        fumList should (not be_== (fumList) or not contain atLeastOneElementOf (Seq("fie", "fee", "fuu", "foe")))
        fumList should (not be_== (toList) or not contain atLeastOneElementOf (Seq("fee", "fie", "foe", "fum")))
        val e1 = intercept[TestFailedException] {
          fumList should (not be_== (fumList) or not contain atLeastOneElementOf (Seq("fee", "fie", "foe", "fum")))
        }
        checkMessageStackDepth(e1, FailureMessages("wasEqualTo", fumList, fumList) + ", and " + FailureMessages("containedAtLeastOneElementOf", fumList, Seq("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)
      }

      "should use the implicit Equality in scope" in {
        implicit val ise = upperCaseStringEquality
        fumList should (not be_== (toList) or not contain atLeastOneElementOf (Seq("fum", "foe")))
        fumList should (not be_== (fumList) or not contain atLeastOneElementOf (Seq("fum", "foe")))
        fumList should (not be_== (toList) or not contain atLeastOneElementOf (Seq("FEE", "FIE", "FOE", "FUM")))
        val e1 = intercept[TestFailedException] {
          fumList should (not be_== (fumList) or (not contain atLeastOneElementOf (Seq("FEE", "FIE", "FOE", "FUM"))))
        }
        checkMessageStackDepth(e1, FailureMessages("wasEqualTo", fumList, fumList) + ", and " + FailureMessages("containedAtLeastOneElementOf", fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
      }

      "should use an explicitly provided Equality" in {
        (fumList should (not be_== (toList) or not contain atLeastOneElementOf (Seq("fum", "foe")))) (decided by upperCaseStringEquality)
        (fumList should (not be_== (fumList) or not contain atLeastOneElementOf (Seq("fum", "foe")))) (decided by upperCaseStringEquality)
        (fumList should (not be_== (toList) or not contain atLeastOneElementOf (Seq("FEE", "FIE", "FOE", "FUM")))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not be_== (fumList) or not contain atLeastOneElementOf (Seq("FEE", "FIE", "FOE", "FUM")))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages("wasEqualTo", fumList, fumList) + ", and " + FailureMessages("containedAtLeastOneElementOf", fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
        (fumList should (not contain atLeastOneElementOf (Seq(" FEE ", " FIE ", " FOE ", " FUU ")) or not contain atLeastOneElementOf (Seq(" FEE ", " FIE ", " FOE ", " FUU ")))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }

      "should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value" in {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not be_== (toList) or not contain atLeastOneElementOf (Seq("fee", "fie", "foe", "fie", "fum")))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("atLeastOneElementOfDuplicate")))
      }
    }

  }

  "collection of Lists" - {

    val list1s: Vector[List[Int]] = Vector(List(1), List(1), List(1))
    val lists: Vector[List[Int]] = Vector(List(1), List(1), List(2))
    val nils: Vector[List[Int]] = Vector(Nil, Nil, Nil)
    val listsNil: Vector[List[Int]] = Vector(List(1), List(1), Nil)
    val hiLists: Vector[List[String]] = Vector(List("hi"), List("hi"), List("hi"))
    val toLists: Vector[List[String]] = Vector(List("to"), List("to"), List("to"))

    def allErrMsg(index: Int, message: String, lineNumber: Int, left: Any): String =
      "'all' inspection failed, because: \n" +
        "  at index " + index + ", " + message + " (" + fileName + ":" + (lineNumber) + ") \n" +
        "in " + decorateToStringValue(left)

    "when used with (contain atLeastOneElementOf (..) and contain atLeastOneElementOf (..)) syntax" - {

      "should do nothing if valid, else throw a TFE with an appropriate error message" in {
        all (list1s) should (contain atLeastOneElementOf Seq(3, 2, 1) or contain atLeastOneElementOf Seq(1, 3, 4))
        all (list1s) should (contain atLeastOneElementOf Seq(3, 2, 5) or contain atLeastOneElementOf Seq(1, 3, 4))
        all (list1s) should (contain atLeastOneElementOf Seq(3, 2, 1) or contain atLeastOneElementOf Seq(2, 3, 4))

        atLeast (2, lists) should (contain atLeastOneElementOf Seq(3, 1, 5) or contain atLeastOneElementOf Seq(1, 3, 4))
        atLeast (2, lists) should (contain atLeastOneElementOf Seq(3, 6, 5) or contain atLeastOneElementOf Seq(1, 3, 4))
        atLeast (2, lists) should (contain atLeastOneElementOf Seq(3, 1, 5) or contain atLeastOneElementOf Seq(8, 3, 4))

        val e1 = intercept[TestFailedException] {
          all (lists) should (contain atLeastOneElementOf Seq(6, 7, 8) or contain atLeastOneElementOf Seq(1, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(lists(2)) + " did not contain at least one element of " + decorateToStringValue(Seq(6, 7, 8)) + ", and " + decorateToStringValue(lists(2)) + " did not contain at least one element of " + decorateToStringValue(Seq(1, 3, 4)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
      }

      "should use the implicit Equality in scope" in {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (contain atLeastOneElementOf Seq("HI", "HE") or contain atLeastOneElementOf Seq("HI", "HE"))
        all (hiLists) should (contain atLeastOneElementOf Seq("hi", "he") or contain atLeastOneElementOf Seq("HI", "HE"))
        all (hiLists) should (contain atLeastOneElementOf Seq("HI", "HE") or contain atLeastOneElementOf Seq("hi", "he"))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (contain atLeastOneElementOf Seq("hi", "he") or contain atLeastOneElementOf Seq("hi", "he"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(hiLists(0)) + " did not contain at least one element of " + decorateToStringValue(Seq("hi", "he")) + ", and " + decorateToStringValue(hiLists(0)) + " did not contain at least one element of " + decorateToStringValue(Seq("hi", "he")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      "should use an explicitly provided Equality" in {
        (all (hiLists) should (contain atLeastOneElementOf Seq("HI", "HE") or contain atLeastOneElementOf Seq("HI", "HE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (contain atLeastOneElementOf Seq("hi", "he") or contain atLeastOneElementOf Seq("HI", "HE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (contain atLeastOneElementOf Seq("HI", "HE") or contain atLeastOneElementOf Seq("hi", "he"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)

        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (contain atLeastOneElementOf Seq("hi", "he") or contain atLeastOneElementOf Seq("hi", "he"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(hiLists(0)) + " did not contain at least one element of " + decorateToStringValue(Seq("hi", "he")) + ", and " + decorateToStringValue(hiLists(0)) + " did not contain at least one element of " + decorateToStringValue(Seq("hi", "he")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      "should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value" in {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (contain atLeastOneElementOf Seq(3, 2, 2, 1) or contain atLeastOneElementOf Seq(1, 3, 4))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("atLeastOneElementOfDuplicate")))

        val e2 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (contain atLeastOneElementOf Seq(1, 3, 4) or contain atLeastOneElementOf Seq(3, 2, 2, 1))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(FailureMessages("atLeastOneElementOfDuplicate")))
      }
    }

    "when used with (be (...) and contain atLeastOneElementOf (...)) syntax" - {

      "should do nothing if valid, else throw a TFE with an appropriate error message" in {
        all (list1s) should (be_== (List(1)) or contain atLeastOneElementOf Seq(1, 3, 4))
        all (list1s) should (be_== (List(2)) or contain atLeastOneElementOf Seq(1, 3, 4))
        all (list1s) should (be_== (List(1)) or contain atLeastOneElementOf Seq(2, 3, 4))

        val e1 = intercept[TestFailedException] {
          all (list1s) should (be_== (List(2)) or contain atLeastOneElementOf Seq(2, 3, 8))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List(1)) + " was not equal to " + decorateToStringValue(List(2)) + ", and " + decorateToStringValue(list1s(0)) + " did not contain at least one element of " + decorateToStringValue(Seq(2, 3, 8)), thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
      }

      "should use the implicit Equality in scope" in {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (be_== (List("hi")) or contain atLeastOneElementOf Seq("HI", "HE"))
        all (hiLists) should (be_== (List("ho")) or contain atLeastOneElementOf Seq("HI", "HE"))
        all (hiLists) should (be_== (List("hi")) or contain atLeastOneElementOf Seq("hi", "he"))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (be_== (List("ho")) or contain atLeastOneElementOf Seq("hi", "he"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi")) + " was not equal to " + decorateToStringValue(List("ho")) + ", and " + decorateToStringValue(List("hi")) + " did not contain at least one element of " + decorateToStringValue(Seq("hi", "he")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      "should use an explicitly provided Equality" in {
        (all (hiLists) should (be_== (List("hi")) or contain atLeastOneElementOf Seq("HI", "HE"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (be_== (List("ho")) or contain atLeastOneElementOf Seq("HI", "HE"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (be_== (List("hi")) or contain atLeastOneElementOf Seq("hi", "he"))) (decided by upperCaseStringEquality)

        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (be_== (List("ho")) or contain atLeastOneElementOf Seq("hi", "he"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi")) + " was not equal to " + decorateToStringValue(List("ho")) + ", and " + decorateToStringValue(hiLists(0)) + " did not contain at least one element of " + decorateToStringValue(Seq("hi", "he")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      "should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value" in {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (be_== (List(1)) or contain atLeastOneElementOf Seq(3, 2, 2, 1))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("atLeastOneElementOfDuplicate")))
      }
    }

    "when used with (not contain atLeastOneElementOf (..) and not contain atLeastOneElementOf (..))" - {

      "should do nothing if valid, else throw a TFE with an appropriate error message" in {
        all (list1s) should (not contain atLeastOneElementOf (Seq(3, 2, 8)) or not contain atLeastOneElementOf (Seq(8, 3, 4)))
        all (list1s) should (not contain atLeastOneElementOf (Seq(1, 2, 8)) or not contain atLeastOneElementOf (Seq(8, 3, 4)))
        all (list1s) should (not contain atLeastOneElementOf (Seq(3, 2, 8)) or not contain atLeastOneElementOf (Seq(8, 3, 1)))

        val e1 = intercept[TestFailedException] {
          all (lists) should (not contain atLeastOneElementOf (Seq(2, 6, 8)) or not contain atLeastOneElementOf (Seq(2, 3, 4)))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(List(2)) + " contained at least one element of " + decorateToStringValue(Seq(2, 6, 8)) + ", and " + decorateToStringValue(List(2)) + " contained at least one element of " + decorateToStringValue(Seq(2, 3, 4)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
      }

      "should use the implicit Equality in scope" in {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (not contain atLeastOneElementOf (Seq("hi", "he")) or not contain atLeastOneElementOf (Seq("hi", "he")))
        all (hiLists) should (not contain atLeastOneElementOf (Seq("HI", "HE")) or not contain atLeastOneElementOf (Seq("hi", "he")))
        all (hiLists) should (not contain atLeastOneElementOf (Seq("hi", "he")) or not contain atLeastOneElementOf (Seq("HI", "HE")))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not contain atLeastOneElementOf (Seq("HI", "HE")) or not contain atLeastOneElementOf (Seq("HI", "HE")))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(hiLists(0)) + " contained at least one element of " + decorateToStringValue(Seq("HI", "HE")) + ", and " + decorateToStringValue(hiLists(0)) + " contained at least one element of " + decorateToStringValue(Seq("HI", "HE")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      "should use an explicitly provided Equality" in {
        (all (hiLists) should (not contain atLeastOneElementOf (Seq("hi", "he")) or not contain atLeastOneElementOf (Seq("hi", "he")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (not contain atLeastOneElementOf (Seq("HI", "HE")) or not contain atLeastOneElementOf (Seq("hi", "he")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (not contain atLeastOneElementOf (Seq("hi", "he")) or not contain atLeastOneElementOf (Seq("HI", "HE")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)

        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not contain atLeastOneElementOf (Seq("HI", "HE")) or not contain atLeastOneElementOf (Seq("HI", "HE")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi")) + " contained at least one element of " + decorateToStringValue(Seq("HI", "HE")) + ", and " + decorateToStringValue(List("hi")) + " contained at least one element of " + decorateToStringValue(Seq("HI", "HE")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      "should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value" in {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not contain atLeastOneElementOf (Seq(3, 2, 2, 1)) or not contain atLeastOneElementOf (Seq(8, 3, 4)))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("atLeastOneElementOfDuplicate")))

        val e2 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not contain atLeastOneElementOf (Seq(8, 3, 4)) or not contain atLeastOneElementOf (Seq(3, 2, 2, 1)))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(FailureMessages("atLeastOneElementOfDuplicate")))
      }
    }

    "when used with (not be (...) and not contain atLeastOneElementOf (...))" - {

      "should do nothing if valid, else throw a TFE with an appropriate error message" in {
        all (list1s) should (not be_== (List(2)) or not contain atLeastOneElementOf (Seq(8, 3, 4)))
        all (list1s) should (not be_== (List(1)) or not contain atLeastOneElementOf (Seq(8, 3, 4)))
        all (list1s) should (not be_== (List(2)) or not contain atLeastOneElementOf (Seq(8, 3, 1)))

        val e1 = intercept[TestFailedException] {
          all (list1s) should (not be_== (List(1)) or not contain atLeastOneElementOf (Seq(2, 3, 1)))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List(1)) + " was equal to " + decorateToStringValue(List(1)) + ", and " + decorateToStringValue(list1s(0)) + " contained at least one element of " + decorateToStringValue(Seq(2, 3, 1)), thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
      }

      "should use the implicit Equality in scope" in {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (not be_== (List("ho")) or not contain atLeastOneElementOf (Seq("hi", "he")))
        all (hiLists) should (not be_== (List("hi")) or not contain atLeastOneElementOf (Seq("hi", "he")))
        all (hiLists) should (not be_== (List("ho")) or not contain atLeastOneElementOf (Seq("HI", "HE")))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not be_== (List("hi")) or not contain atLeastOneElementOf (Seq("HI", "HE")))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi")) + " was equal to " + decorateToStringValue(List("hi")) + ", and " + decorateToStringValue(hiLists(0)) + " contained at least one element of " + decorateToStringValue(Seq("HI", "HE")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      "should use an explicitly provided Equality" in {
        (all (hiLists) should (not be_== (List("ho")) or not contain atLeastOneElementOf (Seq("hi", "he")))) (decided by upperCaseStringEquality)
        (all (hiLists) should (not be_== (List("hi")) or not contain atLeastOneElementOf (Seq("hi", "he")))) (decided by upperCaseStringEquality)
        (all (hiLists) should (not be_== (List("ho")) or not contain atLeastOneElementOf (Seq("HI", "HE")))) (decided by upperCaseStringEquality)

        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not be_== (List("hi")) or not contain atLeastOneElementOf (Seq("HI", "HE")))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi")) + " was equal to " + decorateToStringValue(List("hi")) + ", and " + decorateToStringValue(hiLists(0)) + " contained at least one element of " + decorateToStringValue(Seq("HI", "HE")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      "should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value" in {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not be_== (List(2)) or not contain atLeastOneElementOf (Seq(3, 2, 2, 1)))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("atLeastOneElementOfDuplicate")))
      }
    }
  }
}
