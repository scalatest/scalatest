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
import org.scalactic.StringNormalizations._
import SharedHelpers._
import FailureMessages.decorateToStringValue
import Matchers._

class ListShouldContainInOrderElementsOfLogicalOrSpec extends Spec {

  //ADDITIONAL//

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

  val fileName: String = "ListShouldContainInOrderElementsOfLogicalOrSpec.scala"

  object `a List` {

    val fumList: List[String] = List("fex", "fum", "fum", "foe", "fie", "fee", "fee", "fee")
    val toList: List[String] = List("too", "you", "to", "to", "birthday", "birthday", "happy")

    object `when used with (contain inOrderElementsOf xx or contain inOrderElementsOf xx)` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (contain inOrderElementsOf Seq("fum", "foe", "fie", "fee") or contain inOrderElementsOf Seq("fum", "foe", "fie", "fee"))
        fumList should (contain inOrderElementsOf Seq("fee", "fie", "foe", "fum") or contain inOrderElementsOf Seq("fum", "foe", "fie", "fee"))
        fumList should (contain inOrderElementsOf Seq("fum", "foe", "fie", "fee") or contain inOrderElementsOf Seq("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain inOrderElementsOf Seq("fee", "fie", "foe", "fum") or contain inOrderElementsOf Seq("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, FailureMessages("didNotContainAllElementsOfInOrder", fumList, Seq("fee", "fie", "foe", "fum")) + ", and " + FailureMessages("didNotContainAllElementsOfInOrder", fumList, Seq("happy", "birthday", "to", "you")), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE") or contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE"))
        fumList should (contain inOrderElementsOf Seq("FEE", "FIE", "FOE", "FUM") or contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE"))
        fumList should (contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE") or contain inOrderElementsOf Seq("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain inOrderElementsOf Seq("FEE", "FIE", "FOE", "FUM") or (contain inOrderElementsOf Seq("FIE", "FEE", "FAM", "FOE")))
        }
        checkMessageStackDepth(e1, FailureMessages("didNotContainAllElementsOfInOrder", fumList, Seq("FEE", "FIE", "FOE", "FUM")) + ", and " + FailureMessages("didNotContainAllElementsOfInOrder", fumList, Seq("FIE", "FEE", "FAM", "FOE")), fileName, thisLineNumber - 2)
      }

      def `should use an explicitly provided Equality` {
        (fumList should (contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE") or contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (contain inOrderElementsOf Seq("FEE", "FIE", "FOE", "FUM") or contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE") or contain inOrderElementsOf Seq("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain inOrderElementsOf Seq("FEE", "FIE", "FOE", "FUM") or contain inOrderElementsOf Seq("FIE", "FEE", "FAM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages("didNotContainAllElementsOfInOrder", fumList, Seq("FEE", "FIE", "FOE", "FUM")) + ", and " + FailureMessages("didNotContainAllElementsOfInOrder", fumList, Seq("FIE", "FEE", "FAM", "FOE")), fileName, thisLineNumber - 2)
        (fumList should (contain inOrderElementsOf Seq(" FUM ", " FOE ", " FIE ", " FEE ") or contain inOrderElementsOf Seq(" FUM ", " FOE ", " FIE ", " FEE "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }

      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (contain inOrderElementsOf Seq("fee", "fie", "foe", "fie", "fum") or contain inOrderElementsOf Seq("fum", "foe", "fie", "fee"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("inOrderElementsOfDuplicate")))

        val e2 = intercept[exceptions.NotAllowedException] {
          fumList should (contain inOrderElementsOf Seq("fum", "foe", "fie", "fee") or contain inOrderElementsOf Seq("fee", "fie", "foe", "fie", "fum"))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(FailureMessages("inOrderElementsOfDuplicate")))
      }
    }

    object `when used with (equal xx and contain inOrderElementsOf xx)` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (equal (fumList) or contain inOrderElementsOf Seq("fum", "foe", "fie", "fee"))
        fumList should (equal (toList) or contain inOrderElementsOf Seq("fum", "foe", "fie", "fee"))
        fumList should (equal (fumList) or contain inOrderElementsOf Seq("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) or contain inOrderElementsOf Seq("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, FailureMessages("didNotEqual", fumList, toList) + ", and " + FailureMessages("didNotContainAllElementsOfInOrder", fumList, Seq("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (equal (fumList) or contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE"))
        fumList should (equal (toList) or contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE"))
        fumList should (equal (fumList) or contain inOrderElementsOf Seq("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) or (contain inOrderElementsOf Seq("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e1, FailureMessages("didNotEqual", fumList, toList) + ", and " + FailureMessages("didNotContainAllElementsOfInOrder", fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
      }

      def `should use an explicitly provided Equality` {
        (fumList should (equal (toList) or contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (equal (fumList) or contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (equal (toList) or contain inOrderElementsOf Seq("FEE", "FIE", "FOE", "FUM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (equal (fumList) or contain inOrderElementsOf Seq("FEE", "FIE", "FOE", "FUM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages("didNotEqual", fumList, fumList) + ", and " + FailureMessages("didNotContainAllElementsOfInOrder", fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
        (fumList should (equal (toList) or contain inOrderElementsOf Seq(" FEE ", " FIE ", " FOE ", " FUM "))) (decided by invertedListOfStringEquality, after being lowerCased and trimmed)
      }

      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (equal (fumList) or contain inOrderElementsOf Seq("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("inOrderElementsOfDuplicate")))
      }
    }

    object `when used with (be xx and contain inOrderElementsOf xx)` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (be_== (fumList) or contain inOrderElementsOf Seq("fum", "foe", "fie", "fee"))
        fumList should (be_== (toList) or contain inOrderElementsOf Seq("fum", "foe", "fie", "fee"))
        fumList should (be_== (fumList) or contain inOrderElementsOf Seq("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (be_== (toList) or contain inOrderElementsOf Seq("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, FailureMessages("wasNotEqualTo", fumList, toList) + ", and " + FailureMessages("didNotContainAllElementsOfInOrder", fumList, Seq("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (be_== (fumList) or contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE"))
        fumList should (be_== (toList) or contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE"))
        fumList should (be_== (fumList) or contain inOrderElementsOf Seq("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (be_== (toList) or (contain inOrderElementsOf Seq("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e1, FailureMessages("wasNotEqualTo", fumList, toList) + ", and " + FailureMessages("didNotContainAllElementsOfInOrder", fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
      }

      def `should use an explicitly provided Equality` {
        (fumList should (be_== (fumList) or contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE"))) (decided by upperCaseStringEquality)
        (fumList should (be_== (toList) or contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE"))) (decided by upperCaseStringEquality)
        (fumList should (be_== (fumList) or contain inOrderElementsOf Seq("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (be_== (toList) or contain inOrderElementsOf Seq("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages("wasNotEqualTo", fumList, toList) + ", and " + FailureMessages("didNotContainAllElementsOfInOrder", fumList, Seq("FEE", "FIE", "FOE", "FUM")), fileName, thisLineNumber - 2)
        (fumList should (be_== (fumList) or contain inOrderElementsOf Seq(" FUM ", " FOE ", " FIE ", " FEE "))) (after being lowerCased and trimmed)
      }

      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (be_== (fumList) or contain inOrderElementsOf Seq("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("inOrderElementsOfDuplicate")))
      }
    }

    object `when used with (contain inOrderElementsOf xx and be xx)` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (contain inOrderElementsOf Seq("fum", "foe", "fie", "fee") or be_== (fumList))
        fumList should (contain inOrderElementsOf Seq("fee", "fie", "foe", "fum") or be_== (fumList))
        fumList should (contain inOrderElementsOf Seq("fum", "foe", "fie", "fee") or be_== (toList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain inOrderElementsOf Seq("fee", "fie", "foe", "fum") or be_== (toList))
        }
        checkMessageStackDepth(e1, FailureMessages("didNotContainAllElementsOfInOrder", fumList, Seq("fee", "fie", "foe", "fum")) + ", and " + FailureMessages("wasNotEqualTo", fumList, toList), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE") or be_== (fumList))
        fumList should (contain inOrderElementsOf Seq("FEE", "FIE", "FOE", "FUM") or be_== (fumList))
        fumList should (contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE") or be_== (toList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain inOrderElementsOf Seq("FEE", "FIE", "FOE", "FUM") or be_== (toList))
        }
        checkMessageStackDepth(e1, FailureMessages("didNotContainAllElementsOfInOrder", fumList, Seq("FEE", "FIE", "FOE", "FUM")) + ", and " + FailureMessages("wasNotEqualTo", fumList, toList), fileName, thisLineNumber - 2)
      }

      def `should use an explicitly provided Equality` {
        (fumList should (contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE") or be_== (fumList))) (decided by upperCaseStringEquality)
        (fumList should (contain inOrderElementsOf Seq("FEE", "FIE", "FOE", "FUM") or be_== (fumList))) (decided by upperCaseStringEquality)
        (fumList should (contain inOrderElementsOf Seq("FUM", "FOE", "FIE", "FEE") or be_== (toList))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain inOrderElementsOf Seq("FEE", "FIE", "FOE", "FUM") or be_== (toList))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages("didNotContainAllElementsOfInOrder", fumList, Seq("FEE", "FIE", "FOE", "FUM")) + ", and " + FailureMessages("wasNotEqualTo", fumList, toList), fileName, thisLineNumber - 2)
        (fumList should (contain inOrderElementsOf Seq(" FUM ", " FOE ", " FIE ", " FEE ") or be_== (fumList))) (after being lowerCased and trimmed)
      }

      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (contain inOrderElementsOf Seq("fee", "fie", "foe", "fie", "fum") or be_== (fumList))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("inOrderElementsOfDuplicate")))
      }
    }

    object `when used with (not contain inOrderElementsOf xx and not contain inOrderElementsOf xx)` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (not contain inOrderElementsOf (Seq("fee", "fie", "foe", "fum")) or not contain inOrderElementsOf (Seq("fee", "fie", "foe", "fum")))
        fumList should (not contain inOrderElementsOf (Seq("fum", "foe", "fie", "fee")) or not contain inOrderElementsOf (Seq("fee", "fie", "foe", "fum")))
        fumList should (not contain inOrderElementsOf (Seq("fee", "fie", "foe", "fum")) or not contain inOrderElementsOf (Seq("fum", "foe", "fie", "fee")))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain inOrderElementsOf (Seq("fum", "foe", "fie", "fee")) or not contain inOrderElementsOf (Seq("fum", "foe", "fie", "fee")))
        }
        checkMessageStackDepth(e1, FailureMessages("containedAllElementsOfInOrder", fumList, Seq("fum", "foe", "fie", "fee")) + ", and " + FailureMessages("containedAllElementsOfInOrder", fumList, Seq("fum", "foe", "fie", "fee")), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (not contain inOrderElementsOf (Seq("FEE", "FIE", "FOE", "FUM")) or not contain inOrderElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))
        fumList should (not contain inOrderElementsOf (Seq("FUM", "FOE", "FIE", "FEE")) or not contain inOrderElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))
        fumList should (not contain inOrderElementsOf (Seq("FEE", "FIE", "FOE", "FUM")) or not contain inOrderElementsOf (Seq("FUM", "FOE", "FIE", "FEE")))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain inOrderElementsOf (Seq("FUM", "FOE", "FIE", "FEE")) or not contain inOrderElementsOf (Seq("FUM", "FOE", "FIE", "FEE")))
        }
        checkMessageStackDepth(e1, FailureMessages("containedAllElementsOfInOrder", fumList, Seq("FUM", "FOE", "FIE", "FEE")) + ", and " + FailureMessages("containedAllElementsOfInOrder", fumList, Seq("FUM", "FOE", "FIE", "FEE")), fileName, thisLineNumber - 2)
      }

      def `should use an explicitly provided Equality` {
        (fumList should (not contain inOrderElementsOf (Seq("FEE", "FIE", "FOE", "FUM")) or not contain inOrderElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (not contain inOrderElementsOf (Seq("FUM", "FOE", "FIE", "FEE")) or not contain inOrderElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (not contain inOrderElementsOf (Seq("FEE", "FIE", "FOE", "FUM")) or not contain inOrderElementsOf (Seq("FUM", "FOE", "FIE", "FEE")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not contain inOrderElementsOf (Seq("FUM", "FOE", "FIE", "FEE")) or not contain inOrderElementsOf (Seq("FUM", "FOE", "FIE", "FEE")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages("containedAllElementsOfInOrder", fumList, Seq("FUM", "FOE", "FIE", "FEE")) + ", and " + FailureMessages("containedAllElementsOfInOrder", fumList, Seq("FUM", "FOE", "FIE", "FEE")), fileName, thisLineNumber - 2)
      }

      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not contain inOrderElementsOf (Seq("fee", "fie", "foe", "fie", "fum")) or not contain inOrderElementsOf (Seq("fee", "fie", "foe", "fum")))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("inOrderElementsOfDuplicate")))

        val e2 = intercept[exceptions.NotAllowedException] {
          fumList should (not contain inOrderElementsOf (Seq("fee", "fie", "foe", "fum")) or not contain inOrderElementsOf (Seq("fee", "fie", "foe", "fie", "fum")))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(FailureMessages("inOrderElementsOfDuplicate")))
      }
    }

    object `when used with (not equal xx and not contain inOrderElementsOf xx)` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (not equal (toList) or not contain inOrderElementsOf (Seq("fee", "fie", "foe", "fum")))
        fumList should (not equal (fumList) or not contain inOrderElementsOf (Seq("fee", "fie", "foe", "fum")))
        fumList should (not equal (toList) or not contain inOrderElementsOf (Seq("fum", "foe", "fie", "fee")))
        val e1 = intercept[TestFailedException] {
          fumList should (not equal (fumList) or not contain inOrderElementsOf (Seq("fum", "foe", "fie", "fee")))
        }
        checkMessageStackDepth(e1, FailureMessages("equaled", fumList, fumList) + ", and " + FailureMessages("containedAllElementsOfInOrder", fumList, Seq("fum", "foe", "fie", "fee")), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (not equal (toList) or not contain inOrderElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))
        fumList should (not equal (fumList) or not contain inOrderElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))
        fumList should (not equal (toList) or not contain inOrderElementsOf (Seq("FUM", "FOE", "FIE", "FEE")))
        val e2 = intercept[TestFailedException] {
          fumList should (not equal (fumList) or (not contain inOrderElementsOf (Seq("FUM", "FOE", "FIE", "FEE"))))
        }
        checkMessageStackDepth(e2, FailureMessages("equaled", fumList, fumList) + ", and " + FailureMessages("containedAllElementsOfInOrder", fumList, Seq("FUM", "FOE", "FIE", "FEE")), fileName, thisLineNumber - 2)
      }

      def `should use an explicitly provided Equality` {
        (fumList should (not equal (fumList) or not contain inOrderElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (not equal (toList) or not contain inOrderElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (not equal (fumList) or not contain inOrderElementsOf (Seq("FUM", "FOE", "FIE", "FEE")))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not equal (toList) or not contain inOrderElementsOf (Seq("FUM", "FOE", "FIE", "FEE")))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages("equaled", fumList, toList) + ", and " + FailureMessages("containedAllElementsOfInOrder", fumList, Seq("FUM", "FOE", "FIE", "FEE")), fileName, thisLineNumber - 2)
      }

      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not equal (toList) or not contain inOrderElementsOf (Seq("fee", "fie", "foe", "fie", "fum")))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("inOrderElementsOfDuplicate")))
      }
    }

    object `when used with (not be xx and not contain inOrderElementsOf xx)` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (not be_== (toList) or not contain inOrderElementsOf (Seq("fee", "fie", "foe", "fum")))
        fumList should (not be_== (fumList) or not contain inOrderElementsOf (Seq("fee", "fie", "foe", "fum")))
        fumList should (not be_== (toList) or not contain inOrderElementsOf (Seq("fum", "foe", "fie", "fee")))
        val e1 = intercept[TestFailedException] {
          fumList should (not be_== (fumList) or not contain inOrderElementsOf (Seq("fum", "foe", "fie", "fee")))
        }
        checkMessageStackDepth(e1, FailureMessages("wasEqualTo", fumList, fumList) + ", and " + FailureMessages("containedAllElementsOfInOrder", fumList, Seq("fum", "foe", "fie", "fee")), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (not be_== (toList) or not contain inOrderElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))
        fumList should (not be_== (fumList) or not contain inOrderElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))
        fumList should (not be_== (toList) or not contain inOrderElementsOf (Seq("FUM", "FOE", "FIE", "FEE")))
        val e1 = intercept[TestFailedException] {
          fumList should (not be_== (fumList) or (not contain inOrderElementsOf (Seq("FUM", "FOE", "FIE", "FEE"))))
        }
        checkMessageStackDepth(e1, FailureMessages("wasEqualTo", fumList, fumList) + ", and " + FailureMessages("containedAllElementsOfInOrder", fumList, Seq("FUM", "FOE", "FIE", "FEE")), fileName, thisLineNumber - 2)
      }

      def `should use an explicitly provided Equality` {
        (fumList should (not be_== (toList) or not contain inOrderElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))) (decided by upperCaseStringEquality)
        (fumList should (not be_== (fumList) or not contain inOrderElementsOf (Seq("FEE", "FIE", "FOE", "FUM")))) (decided by upperCaseStringEquality)
        (fumList should (not be_== (toList) or not contain inOrderElementsOf (Seq("FUM", "FOE", "FIE", "FEE")))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not be_== (fumList) or not contain inOrderElementsOf (Seq("FUM", "FOE", "FIE", "FEE")))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages("wasEqualTo", fumList, fumList) + ", and " + FailureMessages("containedAllElementsOfInOrder", fumList, Seq("FUM", "FOE", "FIE", "FEE")), fileName, thisLineNumber - 2)
        (fumList should (not contain inOrderElementsOf (Seq(" FEE ", " FIE ", " FOE ", " FUU ")) or not contain inOrderElementsOf (Seq(" FEE ", " FIE ", " FOE ", " FUU ")))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }

      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not be_== (toList) or not contain inOrderElementsOf (Seq("fee", "fie", "foe", "fie", "fum")))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("inOrderElementsOfDuplicate")))
      }
    }
  }

  object `collection of Lists` {

    val list1s: Vector[List[Int]] = Vector(List(0, 1, 2, 2, 3), List(0, 1, 2, 2, 3), List(0, 1, 2, 2, 3))
    val lists: Vector[List[Int]] = Vector(List(0, 1, 2, 2, 3), List(0, 1, 1, 2, 3, 3), List(8, 2, 3, 4))
    val nils: Vector[List[Int]] = Vector(Nil, Nil, Nil)
    val listsNil: Vector[List[Int]] = Vector(List(0, 1, 2, 3), List(0, 1, 2, 3), Nil)
    val hiLists: Vector[List[String]] = Vector(List("he", "hi", "hello"), List("he", "hi", "hello"), List("he", "hi", "hello"))
    val toLists: Vector[List[String]] = Vector(List("nice", "you", "to"), List("nice", "you", "to"), List("nice", "you", "to"))

    def allErrMsg(index: Int, message: String, lineNumber: Int, left: Any): String =
      "'all' inspection failed, because: \n" +
        "  at index " + index + ", " + message + " (" + fileName + ":" + (lineNumber) + ") \n" +
        "in " + decorateToStringValue(left)

    object `when used with (contain inOrderElementsOf xx and contain inOrderElementsOf xx)` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (contain inOrderElementsOf Seq(1, 2, 3) or contain inOrderElementsOf Seq(1, 2, 3))
        all (list1s) should (contain inOrderElementsOf Seq(3, 2, 5) or contain inOrderElementsOf Seq(1, 2, 3))
        all (list1s) should (contain inOrderElementsOf Seq(1, 2, 3) or contain inOrderElementsOf Seq(2, 3, 4))

        atLeast (2, lists) should (contain inOrderElementsOf Seq(1, 2, 3) or contain inOrderElementsOf Seq(1, 2, 3))
        atLeast (2, lists) should (contain inOrderElementsOf Seq(3, 6, 5) or contain inOrderElementsOf Seq(1, 2, 3))
        atLeast (2, lists) should (contain inOrderElementsOf Seq(1, 2, 3) or contain inOrderElementsOf Seq(8, 3, 4))

        val e1 = intercept[TestFailedException] {
          all (lists) should (contain inOrderElementsOf Seq(1, 2, 3) or contain inOrderElementsOf Seq(1, 2, 3))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(lists(2)) + " did not contain all elements of " + decorateToStringValue(Seq(1, 2, 3)) + " in order" + ", and " + decorateToStringValue(lists(2)) + " did not contain all elements of " + decorateToStringValue(Seq(1, 2, 3)) + " in order", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (contain inOrderElementsOf Seq("HI", "HELLO") or contain inOrderElementsOf Seq("hi", "hello"))
        all (hiLists) should (contain inOrderElementsOf Seq("HELLO", "HO") or contain inOrderElementsOf Seq("hi", "hello"))
        all (hiLists) should (contain inOrderElementsOf Seq("HI", "HELLO") or contain inOrderElementsOf Seq("hello", "ho"))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (contain inOrderElementsOf Seq("HELLO", "HO") or contain inOrderElementsOf Seq("hello", "ho"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(hiLists(0)) + " did not contain all elements of " + decorateToStringValue(Seq("HELLO", "HO")) + " in order" + ", and " + decorateToStringValue(hiLists(0)) + " did not contain all elements of " + decorateToStringValue(Seq("hello", "ho")) + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      def `should use an explicitly provided Equality` {
        (all (hiLists) should (contain inOrderElementsOf Seq("HI", "HELLO") or contain inOrderElementsOf Seq("hi", "hello"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (contain inOrderElementsOf Seq("HELLO", "HO") or contain inOrderElementsOf Seq("hi", "hello"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (contain inOrderElementsOf Seq("HI", "HELLO") or contain inOrderElementsOf Seq("hello", "ho"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)

        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (contain inOrderElementsOf Seq("HELLO", "HO") or contain inOrderElementsOf Seq("hello", "ho"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(hiLists(0)) + " did not contain all elements of " + decorateToStringValue(Seq("HELLO", "HO")) + " in order" + ", and " + decorateToStringValue(hiLists(0)) + " did not contain all elements of " + decorateToStringValue(Seq("hello", "ho")) + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (contain inOrderElementsOf Seq(1, 2, 2, 3) or contain inOrderElementsOf Seq(1, 2, 3))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("inOrderElementsOfDuplicate")))

        val e2 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (contain inOrderElementsOf Seq(1, 2, 3) or contain inOrderElementsOf Seq(1, 2, 2, 3))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(FailureMessages("inOrderElementsOfDuplicate")))
      }
    }

    object `when used with (be xx and contain inOrderElementsOf xx)` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (be_== (List(0, 1, 2, 2, 3)) or contain inOrderElementsOf Seq(1, 2, 3))
        all (list1s) should (be_== (List(8, 2, 3, 4)) or contain inOrderElementsOf Seq(1, 2, 3))
        all (list1s) should (be_== (List(0, 1, 2, 2, 3)) or contain inOrderElementsOf Seq(2, 3, 4))

        val e1 = intercept[TestFailedException] {
          all (list1s) should (be_== (List(8, 2, 3, 4)) or contain inOrderElementsOf Seq(2, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(list1s(0)) + " was not equal to " + decorateToStringValue(List(8, 2, 3, 4)) + ", and " + decorateToStringValue(list1s(0)) + " did not contain all elements of " + decorateToStringValue(Seq(2, 3, 4)) + " in order", thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (be_== (List("he", "hi", "hello")) or contain inOrderElementsOf Seq("HI", "HELLO"))
        all (hiLists) should (be_== (List("ho", "hello")) or contain inOrderElementsOf Seq("HI", "HELLO"))
        all (hiLists) should (be_== (List("he", "hi", "hello")) or contain inOrderElementsOf Seq("HELLO", "HI"))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (be_== (List("ho", "hello")) or contain inOrderElementsOf Seq("HELLO", "HI"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(hiLists(0)) + " was not equal to " + decorateToStringValue(List("ho", "hello")) + ", and " + decorateToStringValue(hiLists(0)) + " did not contain all elements of " + decorateToStringValue(Seq("HELLO", "HI")) + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      def `should use an explicitly provided Equality` {
        (all (hiLists) should (be_== (List("he", "hi", "hello")) or contain inOrderElementsOf Seq("HI", "HELLO"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (be_== (List("ho", "hello")) or contain inOrderElementsOf Seq("HI", "HELLO"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (be_== (List("he", "hi", "hello")) or contain inOrderElementsOf Seq("HELLO", "HI"))) (decided by upperCaseStringEquality)

        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (be_== (List("ho", "hello")) or contain inOrderElementsOf Seq("HELLO", "HI"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(hiLists(0)) + " was not equal to " + decorateToStringValue(List("ho", "hello")) + ", and " + decorateToStringValue(hiLists(0)) + " did not contain all elements of " + decorateToStringValue(Seq("HELLO", "HI")) + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (be_== (List(0, 1, 2, 2, 3)) or contain inOrderElementsOf Seq(1, 2, 2, 3))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("inOrderElementsOfDuplicate")))
      }
    }

    object `when used with (not contain inOrderElementsOf xx and not contain inOrderElementsOf xx)` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (not contain inOrderElementsOf (Seq(3, 2, 8)) or not contain inOrderElementsOf (Seq(8, 3, 4)))
        all (list1s) should (not contain inOrderElementsOf (Seq(1, 2, 3)) or not contain inOrderElementsOf (Seq(8, 3, 4)))
        all (list1s) should (not contain inOrderElementsOf (Seq(3, 2, 8)) or not contain inOrderElementsOf (Seq(1, 2, 3)))

        val e1 = intercept[TestFailedException] {
          all (lists) should (not contain inOrderElementsOf (Seq(2, 3, 4)) or not contain inOrderElementsOf (Seq(2, 3, 4)))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(lists(2)) + " contained all elements of " + decorateToStringValue(Seq(2, 3, 4)) + " in order" + ", and " + decorateToStringValue(lists(2)) + " contained all elements of " + decorateToStringValue(Seq(2, 3, 4)) + " in order", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (not contain inOrderElementsOf (Seq("HELLO", "HI")) or not contain inOrderElementsOf (Seq("hello", "hi")))
        all (hiLists) should (not contain inOrderElementsOf (Seq("HI", "HELLO")) or not contain inOrderElementsOf (Seq("hello", "hi")))
        all (hiLists) should (not contain inOrderElementsOf (Seq("HELLO", "HI")) or not contain inOrderElementsOf (Seq("hi", "hello")))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not contain inOrderElementsOf (Seq("HI", "HELLO")) or not contain inOrderElementsOf (Seq("hi", "hello")))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(hiLists(0)) + " contained all elements of " + decorateToStringValue(Seq("HI", "HELLO")) + " in order" + ", and " + decorateToStringValue(hiLists(0)) + " contained all elements of " + decorateToStringValue(Seq("hi", "hello")) + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      def `should use an explicitly provided Equality` {
        (all (hiLists) should (not contain inOrderElementsOf (Seq("HELLO", "HI")) or not contain inOrderElementsOf (Seq("hello", "hi")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (not contain inOrderElementsOf (Seq("HI", "HELLO")) or not contain inOrderElementsOf (Seq("hello", "hi")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (not contain inOrderElementsOf (Seq("HELLO", "HI")) or not contain inOrderElementsOf (Seq("hi", "hello")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)

        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not contain inOrderElementsOf (Seq("HI", "HELLO")) or not contain inOrderElementsOf (Seq("hi", "hello")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(hiLists(0)) + " contained all elements of " + decorateToStringValue(Seq("HI", "HELLO")) + " in order" + ", and " + decorateToStringValue(hiLists(0)) + " contained all elements of " + decorateToStringValue(Seq("hi", "hello")) + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not contain inOrderElementsOf (Seq(1, 2, 2, 3)) or not contain inOrderElementsOf (Seq(8, 3, 4)))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("inOrderElementsOfDuplicate")))

        val e2 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not contain inOrderElementsOf (Seq(8, 3, 4)) or not contain inOrderElementsOf (Seq(1, 2, 2, 3)))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(FailureMessages("inOrderElementsOfDuplicate")))
      }
    }

    object `when used with (not be xx and not contain inOrderElementsOf xx)` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (not be_== (List(2)) or not contain inOrderElementsOf (Seq(8, 3, 4)))
        all (list1s) should (not be_== (List(0, 1, 2, 2, 3)) or not contain inOrderElementsOf (Seq(8, 3, 4)))
        all (list1s) should (not be_== (List(2)) or not contain inOrderElementsOf (Seq(1, 2, 3)))

        val e1 = intercept[TestFailedException] {
          all (list1s) should (not be_== (List(0, 1, 2, 2, 3)) or not contain inOrderElementsOf (Seq(1, 2, 3)))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(list1s(0)) + " was equal to " + decorateToStringValue(List(0, 1, 2, 2, 3)) + ", and " + decorateToStringValue(list1s(0)) + " contained all elements of " + decorateToStringValue(Seq(1, 2, 3)) + " in order", thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (not be_== (List("hello", "ho")) or not contain inOrderElementsOf (Seq("HELLO", "HO")))
        all (hiLists) should (not be_== (List("he", "hi", "hello")) or not contain inOrderElementsOf (Seq("HELLO", "HO")))
        all (hiLists) should (not be_== (List("hello", "ho")) or not contain inOrderElementsOf (Seq("HI", "HELLO")))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not be_== (List("he", "hi", "hello")) or not contain inOrderElementsOf (Seq("HI", "HELLO")))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(hiLists(0)) + " was equal to " + decorateToStringValue(List("he", "hi", "hello")) + ", and " + decorateToStringValue(hiLists(0)) + " contained all elements of " + decorateToStringValue(Seq("HI", "HELLO")) + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      def `should use an explicitly provided Equality` {
        (all (hiLists) should (not be_== (List("hello", "ho")) or not contain inOrderElementsOf (Seq("HELLO", "HO")))) (decided by upperCaseStringEquality)
        (all (hiLists) should (not be_== (List("he", "hi", "hello")) or not contain inOrderElementsOf (Seq("HELLO", "HO")))) (decided by upperCaseStringEquality)
        (all (hiLists) should (not be_== (List("hello", "ho")) or not contain inOrderElementsOf (Seq("HI", "HELLO")))) (decided by upperCaseStringEquality)

        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not be_== (List("he", "hi", "hello")) or not contain inOrderElementsOf (Seq("HI", "HELLO")))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(hiLists(0)) + " was equal to " + decorateToStringValue(List("he", "hi", "hello")) + ", and " + decorateToStringValue(hiLists(0)) + " contained all elements of " + decorateToStringValue(Seq("HI", "HELLO")) + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not be_== (List(2)) or not contain inOrderElementsOf (Seq(1, 2, 2, 3)))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("inOrderElementsOfDuplicate")))
      }
    }
  }
}
