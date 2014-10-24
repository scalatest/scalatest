/*
* Copyright 2001-2014 Artima, Inc.
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

class ListShouldContainAllElementsOfLogicalAndSpec extends Spec {

  val invertedListOfStringEquality =
    new Equality[List[String]] {
      def areEqual(a: List[String], b: Any): Boolean = a != b
    }

  val upperCaseStringEquality =
    new Equality[String] {
      def areEqual(a: String, b: Any): Boolean = upperCase(a) == upperCase(b)
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

  val upperCaseListOfStringEquality =
    new Equality[List[String]] {
      def areEqual(a: List[String], b: Any): Boolean = upperCase(a) == upperCase(b)
    }

  //ADDITIONAL//

  val fileName: String = "ListShouldContainAllElementsOfLogicalAndSpec.scala"

  object `a List` {

    val fumList: List[String] = List("fex", "fum", "foe", "fie", "fee")
    val toList: List[String] = List("too", "you", "to", "birthday", "happy")

    object `when used with (contain allElementsOf Seq(..) and contain allElementsOf Seq(..))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (contain allElementsOf Seq("fee", "fie", "foe", "fum") and contain allElementsOf Seq("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain allElementsOf Seq("happy", "birthday", "to", "you") and contain allElementsOf Seq("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, FailureMessages("didNotContainAllElementsOf", fumList, Seq("happy", "birthday", "to", "you")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (contain allElementsOf Seq("fee", "fie", "foe", "fum") and contain allElementsOf Seq("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e2, FailureMessages("containedAllElementsOf", fumList, Seq("fee", "fie", "foe", "fum")) + ", but " + FailureMessages("didNotContainAllElementsOf", fumList, Seq("happy", "birthday", "to", "you")), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (contain allElementsOf Seq("FEE", "FIE", "FOE", "FUM") and contain allElementsOf Seq("FEE", "FIE", "FUM", "FOE"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain allElementsOf Seq("FEE", "FIE", "FOE", "FAM") and contain allElementsOf Seq("FEE", "FIE", "FUM", "FOE"))
        }
        checkMessageStackDepth(e1, FailureMessages("didNotContainAllElementsOf", fumList, Seq("FEE", "FIE", "FOE", "FAM")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (contain allElementsOf Seq("FEE", "FIE", "FOE", "FUM") and (contain allElementsOf Seq("FEE", "FIE", "FAM", "FOE")))
        }
        checkMessageStackDepth(e2, FailureMessages("containedAllElementsOf", fumList, Seq("FEE", "FIE", "FOE", "FUM")) + ", but " + FailureMessages("didNotContainAllElementsOf", fumList, Seq("FEE", "FIE", "FAM", "FOE")), fileName, thisLineNumber - 2)
      }

      def `should use an explicitly provided Equality` {
        (fumList should (contain allElementsOf Seq("FEE", "FIE", "FOE", "FUM") and contain allElementsOf Seq("FEE", "FIE", "FUM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain allElementsOf Seq("FEE", "FIE", "FOE", "FAM") and contain allElementsOf Seq("FEE", "FIE", "FUM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages("didNotContainAllElementsOf", fumList, Seq("FEE", "FIE", "FOE", "FAM")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (contain allElementsOf Seq("FEE", "FIE", "FOE", "FUM") and contain allElementsOf Seq("FEE", "FIE", "FAM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, FailureMessages("containedAllElementsOf", fumList, Seq("FEE", "FIE", "FOE", "FUM")) + ", but " + FailureMessages("didNotContainAllElementsOf", fumList, Seq("FEE", "FIE", "FAM", "FOE")), fileName, thisLineNumber - 2)
        (fumList should (contain allElementsOf Seq(" FEE ", " FIE ", " FOE ", " FUM ") and contain allElementsOf Seq(" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }

      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (contain allElementsOf Seq("fee", "fie", "foe", "fie", "fum") and contain allElementsOf Seq("fie", "fee", "fum", "foe"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("allElementsOfDuplicate")))

        val e2 = intercept[exceptions.NotAllowedException] {
          fumList should (contain allElementsOf Seq("fie", "fee", "fum", "foe") and contain allElementsOf Seq("fee", "fie", "foe", "fie", "fum"))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(FailureMessages("allElementsOfDuplicate")))
      }
    }

    object `when used with (equal (..) and contain allElementsOf Seq(..))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (equal (fumList) and contain allElementsOf Seq("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) and contain allElementsOf Seq("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, FailureMessages("didNotEqual", fumList, toList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (equal (fumList) and contain allElementsOf Seq("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e2, FailureMessages("equaled", fumList, fumList) + ", but " + FailureMessages("didNotContainAllElementsOf", fumList, Seq("happy", "birthday", "to", "you")), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (equal (fumList) and contain allElementsOf Seq("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) and contain allElementsOf Seq("FEE", "FIE", "FOE", "FUM"))
        }
        checkMessageStackDepth(e1, FailureMessages("didNotEqual", fumList, toList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (equal (fumList) and (contain allElementsOf Seq("FEE", "FIE", "FOE", "FAM")))
        }
        checkMessageStackDepth(e2, FailureMessages("equaled", fumList, fumList) + ", but " + FailureMessages("didNotContainAllElementsOf", fumList, Seq("FEE", "FIE", "FOE", "FAM")), fileName, thisLineNumber - 2)
      }

      def `should use an explicitly provided Equality` {
        (fumList should (equal (toList) and contain allElementsOf Seq("FEE", "FIE", "FOE", "FUM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (equal (toList) and contain allElementsOf Seq("FEE", "FIE", "FOE", "FAM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages("equaled", fumList, toList) + ", but " + FailureMessages("didNotContainAllElementsOf", fumList, Seq("FEE", "FIE", "FOE", "FAM")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (equal (fumList) and contain allElementsOf Seq("FEE", "FIE", "FOE", "FUM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, FailureMessages("didNotEqual", fumList, fumList), fileName, thisLineNumber - 2)
        (fumList should (equal (toList) and contain allElementsOf Seq(" FEE ", " FIE ", " FOE ", " FUM "))) (decided by invertedListOfStringEquality, after being lowerCased and trimmed)
      }

      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (equal (fumList) and contain allElementsOf Seq("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("allElementsOfDuplicate")))
      }
    }

    object `when used with (be (..) and contain allElementsOf Seq(..))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (be_== (fumList) and contain allElementsOf Seq("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (be_== (toList) and contain allElementsOf Seq("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, FailureMessages("wasNotEqualTo", fumList, toList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (be_== (fumList) and contain allElementsOf Seq("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e2, FailureMessages("wasEqualTo", fumList, fumList) + ", but " + FailureMessages("didNotContainAllElementsOf", fumList, Seq("happy", "birthday", "to", "you")), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (be_== (fumList) and contain allElementsOf Seq("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (be_== (toList) and contain allElementsOf Seq("FEE", "FIE", "FOE", "FUM"))
        }
        checkMessageStackDepth(e1, FailureMessages("wasNotEqualTo", fumList, toList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (be_== (fumList) and (contain allElementsOf Seq("happy", "birthday", "to", "you")))
        }
        checkMessageStackDepth(e2, FailureMessages("wasEqualTo", fumList, fumList) + ", but " + FailureMessages("didNotContainAllElementsOf", fumList, Seq("happy", "birthday", "to", "you")), fileName, thisLineNumber - 2)
      }

      def `should use an explicitly provided Equality` {
        (fumList should (be_== (fumList) and contain allElementsOf Seq("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (be_== (fumList) and contain allElementsOf Seq("happy", "birthday", "to", "you"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages("wasEqualTo", fumList, fumList) + ", but " + FailureMessages("didNotContainAllElementsOf", fumList, Seq("happy", "birthday", "to", "you")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (be_== (toList) and contain allElementsOf Seq("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, FailureMessages("wasNotEqualTo", fumList, toList), fileName, thisLineNumber - 2)
        (fumList should (be_== (fumList) and contain allElementsOf Seq(" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed)
      }

      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (be_== (fumList) and contain allElementsOf Seq("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("allElementsOfDuplicate")))
      }
    }

    object `when used with (contain allElementsOf Seq(..) and be (..))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (contain allElementsOf Seq("fie", "fee", "fum", "foe") and be_== (fumList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain allElementsOf Seq("fee", "fie", "foe", "fum") and be_== (toList))
        }
        checkMessageStackDepth(e1, FailureMessages("containedAllElementsOf", fumList, Seq("fee", "fie", "foe", "fum")) + ", but " + FailureMessages("wasNotEqualTo", fumList, toList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (contain allElementsOf Seq("happy", "birthday", "to", "you") and be_== (fumList))
        }
        checkMessageStackDepth(e2, FailureMessages("didNotContainAllElementsOf", fumList, Seq("happy", "birthday", "to", "you")), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (contain allElementsOf Seq("FIE", "FEE", "FUM", "FOE") and be_== (fumList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain allElementsOf Seq("FIE", "FEE", "FAM", "FOE") and be_== (toList))
        }
        checkMessageStackDepth(e1, FailureMessages("didNotContainAllElementsOf", fumList, Seq("FIE", "FEE", "FAM", "FOE")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (contain allElementsOf Seq("HAPPY", "BIRTHDAY", "TO", "YOU") and (be_== (fumList)))
        }
        checkMessageStackDepth(e2, FailureMessages("didNotContainAllElementsOf", fumList, Seq("HAPPY", "BIRTHDAY", "TO", "YOU")), fileName, thisLineNumber - 2)
      }

      def `should use an explicitly provided Equality` {
        (fumList should (contain allElementsOf Seq("FIE", "FEE", "FUM", "FOE") and be_== (fumList))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain allElementsOf Seq("FIE", "FEE", "FAM", "FOE") and be_== (fumList))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages("didNotContainAllElementsOf", fumList, Seq("FIE", "FEE", "FAM", "FOE")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (contain allElementsOf Seq("FIE", "FEE", "FUM", "FOE") and be_== (toList))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, FailureMessages("containedAllElementsOf", fumList, Seq("FIE", "FEE", "FUM", "FOE")) + ", but " + FailureMessages("wasNotEqualTo", fumList, toList), fileName, thisLineNumber - 2)
        (fumList should (contain allElementsOf Seq(" FEE ", " FIE ", " FOE ", " FUM ") and be_== (fumList))) (after being lowerCased and trimmed)
      }

      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (contain allElementsOf Seq("fee", "fie", "foe", "fie", "fum") and be_== (fumList))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("allElementsOfDuplicate")))
      }
    }

    object `when used with (not contain allElementsOf xx and not contain allElementsOf xx)` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (not contain allElementsOf (Seq("fee", "fie", "foe", "fuu")) and not contain allElementsOf (Seq("fie", "fee", "fuu", "foe")))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain allElementsOf (Seq("fee", "fie", "foe", "fum")) and not contain allElementsOf (Seq("happy", "birthday", "to", "you")))
        }
        checkMessageStackDepth(e1, FailureMessages("containedAllElementsOf", fumList, Seq("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not contain allElementsOf (Seq("happy", "birthday", "to", "you")) and not contain allElementsOf (Seq("fee", "fie", "foe", "fum")))
        }
        checkMessageStackDepth(e2, FailureMessages("didNotContainAllElementsOf", fumList, Seq("happy", "birthday", "to", "you")) + ", but " + FailureMessages("containedAllElementsOf", fumList, Seq("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (not contain allElementsOf (Seq("FIE", "FEE", "FAM", "FOE")) and not contain allElementsOf (Seq("FIE", "FEE", "FOE", "FAM")))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain allElementsOf (Seq("FIE", "FEE", "FUM", "FOE")) and not contain allElementsOf (Seq("FIE", "FEE", "FOE", "FAM")))
        }
        checkMessageStackDepth(e1, FailureMessages("containedAllElementsOf", fumList, Seq("FIE", "FEE", "FUM", "FOE")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not contain allElementsOf (Seq("FIE", "FEE", "FAM", "FOE")) and (not contain allElementsOf (Seq("FIE", "FEE", "FOE", "FUM"))))
        }
        checkMessageStackDepth(e2, FailureMessages("didNotContainAllElementsOf", fumList, Seq("FIE", "FEE", "FAM", "FOE")) + ", but " + FailureMessages("containedAllElementsOf", fumList, Seq("FIE", "FEE", "FOE", "FUM")), fileName, thisLineNumber - 2)
      }

      def `should use an explicitly provided Equality` {
        (fumList should (not contain allElementsOf (Seq("FIE", "FEE", "FAM", "FOE")) and not contain allElementsOf (Seq("FIE", "FEE", "FOE", "FAM")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not contain allElementsOf (Seq("FIE", "FEE", "FAM", "FOE")) and not contain allElementsOf (Seq("FIE", "FEE", "FOE", "FUM")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages("didNotContainAllElementsOf", fumList, Seq("FIE", "FEE", "FAM", "FOE")) + ", but " + FailureMessages("containedAllElementsOf", fumList, Seq("FIE", "FEE", "FOE", "FUM")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (not contain allElementsOf (Seq("FIE", "FEE", "FUM", "FOE")) and not contain allElementsOf (Seq("FIE", "FEE", "FOE", "FAM")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, FailureMessages("containedAllElementsOf", fumList, Seq("FIE", "FEE", "FUM", "FOE")), fileName, thisLineNumber - 2)
        (fumList should (contain allElementsOf Seq(" FEE ", " FIE ", " FOE ", " FUM ") and contain allElementsOf Seq(" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }

      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not contain allElementsOf (Seq("fee", "fie", "foe", "fie", "fum")) and not contain allElementsOf (Seq("fie", "fee", "fuu", "foe")))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("allElementsOfDuplicate")))

        val e2 = intercept[exceptions.NotAllowedException] {
          fumList should (not contain allElementsOf (Seq("fie", "fee", "fuu", "foe")) and not contain allElementsOf (Seq("fee", "fie", "foe", "fie", "fum")))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(FailureMessages("allElementsOfDuplicate")))
      }
    }

    object `when used with (not equal (..) and not contain allElementsOf Seq(..))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (not equal (toList) and not contain allElementsOf (Seq("fie", "fee", "fuu", "foe")))
        val e1 = intercept[TestFailedException] {
          fumList should (not equal (fumList) and not contain allElementsOf (Seq("happy", "birthday", "to", "you")))
        }
        checkMessageStackDepth(e1, FailureMessages("equaled", fumList, fumList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not equal (toList) and not contain allElementsOf (Seq("fee", "fie", "foe", "fum")))
        }
        checkMessageStackDepth(e2, FailureMessages("didNotEqual", fumList, toList) + ", but " + FailureMessages("containedAllElementsOf", fumList, Seq("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (not equal (toList) and not contain allElementsOf (Seq("FIE", "FEE", "FAM", "FOE")))
        val e1 = intercept[TestFailedException] {
          fumList should (not equal (fumList) and not contain allElementsOf (Seq("FIE", "FEE", "FAM", "FOE")))
        }
        checkMessageStackDepth(e1, FailureMessages("equaled", fumList, fumList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not equal (toList) and (not contain allElementsOf (Seq("FIE", "FEE", "FUM", "FOE"))))
        }
        checkMessageStackDepth(e2, FailureMessages("didNotEqual", fumList, toList) + ", but " + FailureMessages("containedAllElementsOf", fumList, Seq("FIE", "FEE", "FUM", "FOE")), fileName, thisLineNumber - 2)
      }

      def `should use an explicitly provided Equality` {
        (fumList should (not equal (fumList) and not contain allElementsOf (Seq("FIE", "FEE", "FAM", "FOE")))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not equal (fumList) and not contain allElementsOf (Seq("FIE", "FEE", "FUM", "FOE")))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages("didNotEqual", fumList, fumList) + ", but " + FailureMessages("containedAllElementsOf", fumList, Seq("FIE", "FEE", "FUM", "FOE")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (not equal (toList) and not contain allElementsOf (Seq("FIE", "FEE", "FAM", "FOE")))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, FailureMessages("equaled", fumList, toList), fileName, thisLineNumber - 2)
        (fumList should (not contain allElementsOf (Seq(" FEE ", " FIE ", " FOE ", " FUU ")) and not contain allElementsOf (Seq(" FEE ", " FIE ", " FOE ", " FUU ")))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }

      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not equal (toList) and not contain allElementsOf (Seq("fee", "fie", "foe", "fie", "fum")))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("allElementsOfDuplicate")))
      }
    }

    object `when used with (not be (..) and not contain allElementsOf Seq(..))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (not be_== (toList) and not contain allElementsOf (Seq("fie", "fee", "fuu", "foe")))
        val e1 = intercept[TestFailedException] {
          fumList should (not be_== (fumList) and not contain allElementsOf (Seq("happy", "birthday", "to", "you")))
        }
        checkMessageStackDepth(e1, FailureMessages("wasEqualTo", fumList, fumList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not be_== (toList) and not contain allElementsOf (Seq("fee", "fie", "foe", "fum")))
        }
        checkMessageStackDepth(e2, FailureMessages("wasNotEqualTo", fumList, toList) + ", but " + FailureMessages("containedAllElementsOf", fumList, Seq("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (not be_== (toList) and not contain allElementsOf (Seq("FIE", "FEE", "FAM", "FOE")))
        val e1 = intercept[TestFailedException] {
          fumList should (not be_== (fumList) and not contain allElementsOf (Seq("FIE", "FEE", "FAM", "FOE")))
        }
        checkMessageStackDepth(e1, FailureMessages("wasEqualTo", fumList, fumList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not be_== (toList) and (not contain allElementsOf (Seq("FIE", "FEE", "FUM", "FOE"))))
        }
        checkMessageStackDepth(e2, FailureMessages("wasNotEqualTo", fumList, toList) + ", but " + FailureMessages("containedAllElementsOf", fumList, Seq("FIE", "FEE", "FUM", "FOE")), fileName, thisLineNumber - 2)
      }

      def `should use an explicitly provided Equality` {
        (fumList should (not be_== (toList) and not contain allElementsOf (Seq("FIE", "FEE", "FAM", "FOE")))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not be_== (toList) and not contain allElementsOf (Seq("FIE", "FEE", "FUM", "FOE")))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, FailureMessages("wasNotEqualTo", fumList, toList) + ", but " + FailureMessages("containedAllElementsOf", fumList, Seq("FIE", "FEE", "FUM", "FOE")), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (not be_== (fumList) and not contain allElementsOf (Seq("FIE", "FEE", "FAM", "FOE")))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, FailureMessages("wasEqualTo", fumList, fumList), fileName, thisLineNumber - 2)
        (fumList should (not contain allElementsOf (Seq(" FEE ", " FIE ", " FOE ", " FUU ")) and not contain allElementsOf (Seq(" FEE ", " FIE ", " FOE ", " FUU ")))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }

      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not be_== (toList) and not contain allElementsOf (Seq("fee", "fie", "foe", "fie", "fum")))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("allElementsOfDuplicate")))
      }
    }

  }

  object `col of Lists` {

    val list1s: Vector[List[Int]] = Vector(List(3, 2, 1, 0), List(3, 2, 1, 0), List(3, 2, 1, 0))
    val lists: Vector[List[Int]] = Vector(List(3, 2, 1, 0), List(3, 2, 1, 0), List(8, 4, 3, 2))
    val nils: Vector[List[Int]] = Vector(Nil, Nil, Nil)
    val listsNil: Vector[List[Int]] = Vector(List(3, 2, 1, 0), List(3, 2, 1, 0), Nil)
    val hiLists: Vector[List[String]] = Vector(List("howdy", "hi", "hello"), List("howdy", "hi", "hello"), List("howdy", "hi", "hello"))

    def allErrMsg(index: Int, message: String, lineNumber: Int, left: Any): String =
      "'all' inspection failed, because: \n" +
        "  at index " + index + ", " + message + " (" + fileName + ":" + (lineNumber) + ") \n" +
        "in " + decorateToStringValue(left)

    object `used with contain allElementsOf xx and contain allElementsOf xx` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (contain allElementsOf Seq(3, 2, 1) and contain allElementsOf Seq(1, 3, 2))
        atLeast (2, lists) should (contain allElementsOf Seq(3, 1, 2) and contain allElementsOf Seq(2, 3, 1))
        atMost (2, lists) should (contain allElementsOf Seq(3, 1, 2) and contain allElementsOf Seq(2, 3, 1))
        no (lists) should (contain allElementsOf Seq(3, 6, 9) and contain allElementsOf Seq(3, 4, 5))

        val e1 = intercept[TestFailedException] {
          all (lists) should (contain allElementsOf Seq(1, 2, 3) and contain allElementsOf Seq(1, 3, 2))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(lists(2)) + " did not contain all elements of " + decorateToStringValue(Seq(1, 2, 3)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (list1s) should (contain allElementsOf Seq(1, 2, 3) and contain allElementsOf Seq(1, 3, 4))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(list1s(0)) + " contained all elements of " + decorateToStringValue(Seq(1, 2, 3)) + ", but " + decorateToStringValue(list1s(0)) + " did not contain all elements of " + decorateToStringValue(Seq(1, 3, 4)), thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)

        val e4 = intercept[TestFailedException] {
          all (hiLists) should (contain allElementsOf Seq("hi", "hello") and contain allElementsOf Seq("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e4, allErrMsg(0, decorateToStringValue(hiLists(0)) + " contained all elements of " + decorateToStringValue(Seq("hi", "hello")) + ", but " + decorateToStringValue(hiLists(0)) + " did not contain all elements of " + decorateToStringValue(Seq("ho", "hey", "howdy")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (contain allElementsOf Seq("HELLO", "HI") and contain allElementsOf Seq("HI", "HELLO"))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (contain allElementsOf Seq("HO", "HELLO") and contain allElementsOf Seq("HI", "HELLO"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(hiLists(0)) + " did not contain all elements of " + decorateToStringValue(Seq("HO", "HELLO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (hiLists) should (contain allElementsOf Seq("HELLO", "HI") and contain allElementsOf Seq("HO", "HELLO"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(hiLists(0)) + " contained all elements of " + decorateToStringValue(Seq("HELLO", "HI")) + ", but " + decorateToStringValue(hiLists(0)) + " did not contain all elements of " + decorateToStringValue(Seq("HO", "HELLO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      def `should use an explicitly provided Equality` {
        (all (hiLists) should (contain allElementsOf Seq("HELLO", "HI") and contain allElementsOf Seq("HI", "HELLO"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (contain allElementsOf Seq("HO", "HELLO") and contain allElementsOf Seq("HI", "HELLO"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(hiLists(0)) + " did not contain all elements of " + decorateToStringValue(Seq("HO", "HELLO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (contain allElementsOf Seq("HELLO", "HI") and contain allElementsOf Seq("HO", "HELLO"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(hiLists(0)) + " contained all elements of " + decorateToStringValue(Seq("HELLO", "HI")) + ", but " + decorateToStringValue(hiLists(0)) + " did not contain all elements of " + decorateToStringValue(Seq("HO", "HELLO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (contain allElementsOf Seq(3, 2, 2, 1) and contain allElementsOf Seq(1, 3, 2))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("allElementsOfDuplicate")))

        val e2 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (contain allElementsOf Seq(1, 3, 2) and contain allElementsOf Seq(3, 2, 2, 1))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(FailureMessages("allElementsOfDuplicate")))
      }
    }

    object `when used with (be (..) and contain allElementsOf Seq(..))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (be_== (List(3, 2, 1, 0)) and contain allElementsOf Seq(1, 3, 2))
        atLeast (2, lists) should (be_== (List(3, 2, 1, 0)) and contain allElementsOf Seq(1, 3, 2))
        atMost (2, lists) should (be_== (List(3, 2, 1, 0)) and contain allElementsOf Seq(2, 3, 1))
        no (lists) should (be_== (List(3, 6, 9)) and contain allElementsOf Seq(3, 4, 5))
        no (nils) should (be_== (List(1, 6, 8)) and contain allElementsOf Seq(1, 3, 4))
        no (listsNil) should (be_== (List(2, 6, 8)) and contain allElementsOf Seq(3, 4, 5))

        val e1 = intercept[TestFailedException] {
          all (lists) should (be_== (List(3, 2, 1, 0)) and contain allElementsOf Seq(1, 3, 2))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(lists(2)) + " was not equal to " + decorateToStringValue(List(3, 2, 1, 0)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (list1s) should (be_== (List(3, 2, 1, 0)) and contain allElementsOf Seq(2, 3, 8))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(list1s(0)) + " was equal to " + decorateToStringValue(List(3, 2, 1, 0)) + ", but " + decorateToStringValue(list1s(0)) + " did not contain all elements of " + decorateToStringValue(Seq(2, 3, 8)), thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)

        val e3 = intercept[TestFailedException] {
          all (nils) should (be_== (List("hey")) and contain allElementsOf Seq("hello", "hi"))
        }
        checkMessageStackDepth(e3, allErrMsg(0, decorateToStringValue(nils(0)) + " was not equal to " + decorateToStringValue(List("hey")), thisLineNumber - 2, nils), fileName, thisLineNumber - 2)

        val e4 = intercept[TestFailedException] {
          all (hiLists) should (be_== (List("howdy", "hi", "hello")) and contain allElementsOf Seq("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e4, allErrMsg(0, decorateToStringValue(hiLists(0)) + " was equal to " + decorateToStringValue(List("howdy", "hi", "hello")) + ", but " + decorateToStringValue(hiLists(0)) + " did not contain all elements of " + decorateToStringValue(Seq("ho", "hey", "howdy")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e5 = intercept[TestFailedException] {
          all (listsNil) should (be_== (List(3, 2, 1, 0)) and contain allElementsOf Seq(1, 3, 2))
        }
        checkMessageStackDepth(e5, allErrMsg(2, decorateToStringValue(Nil) + " was not equal to " + decorateToStringValue(List(3, 2, 1, 0)), thisLineNumber - 2, listsNil), fileName, thisLineNumber - 2)

        val e6 = intercept[TestFailedException] {
          all (list1s) should (be_== (List(3, 2, 1, 0)) and contain allElementsOf Seq(2, 3, 8))
        }
        checkMessageStackDepth(e6, allErrMsg(0, decorateToStringValue(list1s(0)) + " was equal to " + decorateToStringValue(List(3, 2, 1, 0)) + ", but " + decorateToStringValue(list1s(0)) + " did not contain all elements of " + decorateToStringValue(Seq(2, 3, 8)), thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (be_== (List("howdy", "hi", "hello")) and contain allElementsOf Seq("HELLO", "HI"))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (be_== (List("HI", "HELLO")) and contain allElementsOf Seq("HELLO", "HI"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(hiLists(0)) + " was not equal to " + decorateToStringValue(List("HI", "HELLO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (hiLists) should (be_== (List("howdy", "hi", "hello")) and contain allElementsOf Seq("HO", "HELLO"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(hiLists(0)) + " was equal to " + decorateToStringValue(List("howdy", "hi", "hello")) + ", but " + decorateToStringValue(hiLists(0)) + " did not contain all elements of " + decorateToStringValue(Seq("HO", "HELLO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      def `should use an explicitly provided Equality` {
        (all (hiLists) should (be_== (List("howdy", "hi", "hello")) and contain allElementsOf Seq("HELLO", "HI"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (be_== (List("HI", "HELLO")) and contain allElementsOf Seq("HELLO", "HI"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(hiLists(0)) + " was not equal to " + decorateToStringValue(List("HI", "HELLO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (be_== (List("howdy", "hi", "hello")) and contain allElementsOf Seq("HO", "HELLO"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(hiLists(0)) + " was equal to " + decorateToStringValue(List("howdy", "hi", "hello")) + ", but " + decorateToStringValue(hiLists(0)) + " did not contain all elements of " + decorateToStringValue(Seq("HO", "HELLO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (be_== (List(3, 2, 1, 0)) and contain allElementsOf Seq(3, 2, 2, 1))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("allElementsOfDuplicate")))
      }
    }

    object `when used with (not contain allElementsOf xx and not contain allElementsOf xx)` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (not contain allElementsOf (Seq(3, 2, 8)) and not contain allElementsOf (Seq(8, 3, 4)))
        atLeast (2, lists) should (not contain allElementsOf (Seq(3, 8, 5)) and not contain allElementsOf (Seq(8, 3, 4)))
        atMost (2, lists) should (not contain allElementsOf (Seq(2, 4, 3)) and contain allElementsOf (Seq(4, 3, 2)))
        no (list1s) should (not contain allElementsOf (Seq(1, 2, 3)) and not contain allElementsOf (Seq(1, 3, 2)))

        val e1 = intercept[TestFailedException] {
          all (lists) should (not contain allElementsOf (Seq(2, 3, 4)) and not contain allElementsOf (Seq(8, 3, 4)))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(lists(2)) + " contained all elements of " + decorateToStringValue(Seq(2, 3, 4)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (lists) should (not contain allElementsOf (Seq(3, 6, 8)) and not contain allElementsOf (Seq(2, 3, 4)))
        }
        checkMessageStackDepth(e2, allErrMsg(2, decorateToStringValue(lists(2)) + " did not contain all elements of " + decorateToStringValue(Seq(3, 6, 8)) + ", but " + decorateToStringValue(lists(2)) + " contained all elements of " + decorateToStringValue(Seq(2, 3, 4)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e3 = intercept[TestFailedException] {
          all (hiLists) should (not contain allElementsOf (Seq("hello", "hi")) and not contain allElementsOf (Seq("ho", "hey", "howdy")))
        }
        checkMessageStackDepth(e3, allErrMsg(0, decorateToStringValue(hiLists(0)) + " contained all elements of " + decorateToStringValue(Seq("hello", "hi")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e4 = intercept[TestFailedException] {
          all (hiLists) should (not contain allElementsOf (Seq("ho", "hey", "howdy")) and not contain allElementsOf (Seq("hello", "hi")))
        }
        checkMessageStackDepth(e4, allErrMsg(0, decorateToStringValue(hiLists(0)) + " did not contain all elements of " + decorateToStringValue(Seq("ho", "hey", "howdy")) + ", but " + decorateToStringValue(hiLists(0)) + " contained all elements of " + decorateToStringValue(Seq("hello", "hi")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (not contain allElementsOf (Seq("TO", "YOU")) and not contain allElementsOf (Seq("HO", "HE")))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not contain allElementsOf (Seq("HELLO", "HI")) and not contain allElementsOf (Seq("HO", "HE")))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(hiLists(0)) + " contained all elements of " + decorateToStringValue(Seq("HELLO", "HI")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (hiLists) should (not contain allElementsOf (Seq("HO", "HE")) and not contain allElementsOf (Seq("HELLO", "HI")))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(hiLists(0)) + " did not contain all elements of " + decorateToStringValue(Seq("HO", "HE")) + ", but " + decorateToStringValue(hiLists(0)) + " contained all elements of " + decorateToStringValue(Seq("HELLO", "HI")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      def `should use an explicitly provided Equality` {
        (all (hiLists) should (not contain allElementsOf (Seq("TO", "YOU")) and not contain allElementsOf (Seq("HO", "HE")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not contain allElementsOf (Seq("HELLO", "HI")) and not contain allElementsOf (Seq("HO", "HE")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(hiLists(0)) + " contained all elements of " + decorateToStringValue(Seq("HELLO", "HI")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (not contain allElementsOf (Seq("HO", "HE")) and not contain allElementsOf (Seq("HELLO", "HI")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(hiLists(0)) + " did not contain all elements of " + decorateToStringValue(Seq("HO", "HE")) + ", but " + decorateToStringValue(hiLists(0)) + " contained all elements of " + decorateToStringValue(Seq("HELLO", "HI")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not contain allElementsOf (Seq(3, 2, 2, 1)) and not contain allElementsOf (Seq(8, 3, 4)))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("allElementsOfDuplicate")))

        val e2 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not contain allElementsOf (Seq(8, 3, 4)) and not contain allElementsOf (Seq(3, 2, 2, 1)))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(FailureMessages("allElementsOfDuplicate")))
      }
    }

    object `when used with (not be (..) and not contain allElementsOf Seq(..))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (not be_== (List(2)) and not contain allElementsOf (Seq(8, 3, 4)))
        atLeast (2, lists) should (not be_== (List(3)) and not contain allElementsOf (Seq(8, 3, 4)))
        atMost (2, lists) should (not be_== (List(4, 3, 2)) and not contain allElementsOf (Seq(3, 4, 2)))
        no (list1s) should (not be_== (List(3, 2, 1)) and not contain allElementsOf (Seq(1, 2, 3)))

        val e1 = intercept[TestFailedException] {
          all (lists) should (not be_== (List(8, 4, 3, 2)) and not contain allElementsOf (Seq(8, 3, 4)))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(lists(2)) + " was equal to " + decorateToStringValue(List(8, 4, 3, 2)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (lists) should (not be_== (List(3)) and not contain allElementsOf (Seq(2, 3, 4)))
        }
        checkMessageStackDepth(e2, allErrMsg(2, decorateToStringValue(lists(2)) + " was not equal to " + decorateToStringValue(List(3)) + ", but " + decorateToStringValue(lists(2)) + " contained all elements of " + decorateToStringValue(Seq(2, 3, 4)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e3 = intercept[TestFailedException] {
          all (hiLists) should (not be_== (List("howdy", "hi", "hello")) and not contain allElementsOf (Seq("ho", "hey", "howdy")))
        }
        checkMessageStackDepth(e3, allErrMsg(0, decorateToStringValue(hiLists(0)) + " was equal to " + decorateToStringValue(List("howdy", "hi", "hello")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e4 = intercept[TestFailedException] {
          all (hiLists) should (not be_== (List("ho")) and not contain allElementsOf (Seq("hello", "hi")))
        }
        checkMessageStackDepth(e4, allErrMsg(0, decorateToStringValue(hiLists(0)) + " was not equal to " + decorateToStringValue(List("ho")) + ", but " + decorateToStringValue(hiLists(0)) + " contained all elements of " + decorateToStringValue(Seq("hello", "hi")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (not be_== (List("ho")) and not contain allElementsOf (Seq("HO", "HELLO")))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not be_== (List("howdy", "hi", "hello")) and not contain allElementsOf (Seq("HELLO", "HI")))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(hiLists(0)) + " was equal to " + decorateToStringValue(List("howdy", "hi", "hello")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (hiLists) should (not be_== (List("ho")) and not contain allElementsOf (Seq("HI", "HELLO")))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(hiLists(0)) + " was not equal to " + decorateToStringValue(List("ho")) + ", but " + decorateToStringValue(hiLists(0)) + " contained all elements of " + decorateToStringValue(Seq("HI", "HELLO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      def `should use an explicitly provided Equality` {
        (all (hiLists) should (not be_== (List("ho")) and not contain allElementsOf (Seq("HO", "HELLO")))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not be_== (List("howdy", "hi", "hello")) and not contain allElementsOf (Seq("HELLO", "HI")))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(hiLists(0)) + " was equal to " + decorateToStringValue(List("howdy", "hi", "hello")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (not be_== (List("ho")) and not contain allElementsOf (Seq("HI", "HELLO")))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(hiLists(0)) + " was not equal to " + decorateToStringValue(List("ho")) + ", but " + decorateToStringValue(hiLists(0)) + " contained all elements of " + decorateToStringValue(Seq("HI", "HELLO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not be_== (List(2)) and not contain allElementsOf (Seq(3, 2, 2, 1)))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(FailureMessages("allElementsOfDuplicate")))
      }
    }
  }
}
