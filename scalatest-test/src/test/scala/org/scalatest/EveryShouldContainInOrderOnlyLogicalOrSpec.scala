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
import org.scalactic.StringNormalizations._
import SharedHelpers._
import FailureMessages.decorateToStringValue
import Matchers._
import exceptions.TestFailedException

class EveryShouldContainInOrderOnlyLogicalOrSpec extends Spec {

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

  val fileName: String = "EveryShouldContainInOrderOnlyLogicalOrSpec.scala"

  object `an Every` {

    val fumList: Every[String] = Every("fum", "fum", "foe", "fie", "fee", "fee", "fee")
    val toList: Every[String] = Every("you", "to", "to", "birthday", "birthday", "happy")

    object `when used with (contain inOrderOnly xx or contain inOrderOnly xx)` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (contain inOrderOnly ("fum", "foe", "fie", "fee") or contain inOrderOnly ("fum", "foe", "fie", "fee"))
        fumList should (contain inOrderOnly ("fee", "fie", "foe", "fum") or contain inOrderOnly ("fum", "foe", "fie", "fee"))
        fumList should (contain inOrderOnly ("fum", "foe", "fie", "fee") or contain inOrderOnly ("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain inOrderOnly ("fee", "fie", "foe", "fum") or contain inOrderOnly ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, Resources("didNotContainInOrderOnlyElements", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\"") + ", and " + Resources("didNotContainInOrderOnlyElements", decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (contain inOrderOnly ("FUM", "FOE", "FIE", "FEE") or contain inOrderOnly ("FUM", "FOE", "FIE", "FEE"))
        fumList should (contain inOrderOnly ("FEE", "FIE", "FOE", "FUM") or contain inOrderOnly ("FUM", "FOE", "FIE", "FEE"))
        fumList should (contain inOrderOnly ("FUM", "FOE", "FIE", "FEE") or contain inOrderOnly ("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain inOrderOnly ("FEE", "FIE", "FOE", "FUM") or (contain inOrderOnly ("FIE", "FEE", "FAM", "FOE")))
        }
        checkMessageStackDepth(e1, Resources("didNotContainInOrderOnlyElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\"") + ", and " + Resources("didNotContainInOrderOnlyElements", decorateToStringValue(fumList), "\"FIE\", \"FEE\", \"FAM\", \"FOE\""), fileName, thisLineNumber - 2)
      }

      def `should use an explicitly provided Equality` {
        (fumList should (contain inOrderOnly ("FUM", "FOE", "FIE", "FEE") or contain inOrderOnly ("FUM", "FOE", "FIE", "FEE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (contain inOrderOnly ("FEE", "FIE", "FOE", "FUM") or contain inOrderOnly ("FUM", "FOE", "FIE", "FEE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (contain inOrderOnly ("FUM", "FOE", "FIE", "FEE") or contain inOrderOnly ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain inOrderOnly ("FEE", "FIE", "FOE", "FUM") or contain inOrderOnly ("FIE", "FEE", "FAM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotContainInOrderOnlyElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\"") + ", and " + Resources("didNotContainInOrderOnlyElements", decorateToStringValue(fumList), "\"FIE\", \"FEE\", \"FAM\", \"FOE\""), fileName, thisLineNumber - 2)
        (fumList should (contain inOrderOnly (" FUM ", " FOE ", " FIE ", " FEE ") or contain inOrderOnly (" FUM ", " FOE ", " FIE ", " FEE "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }

      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (contain inOrderOnly ("fee", "fie", "foe", "fie", "fum") or contain inOrderOnly ("fum", "foe", "fie", "fee"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("inOrderOnlyDuplicate")))

        val e2 = intercept[exceptions.NotAllowedException] {
          fumList should (contain inOrderOnly ("fum", "foe", "fie", "fee") or contain inOrderOnly ("fee", "fie", "foe", "fie", "fum"))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources("inOrderOnlyDuplicate")))
      }
    }

    object `when used with (equal xx and contain inOrderOnly xx)` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (equal (fumList) or contain inOrderOnly ("fum", "foe", "fie", "fee"))
        fumList should (equal (toList) or contain inOrderOnly ("fum", "foe", "fie", "fee"))
        fumList should (equal (fumList) or contain inOrderOnly ("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) or contain inOrderOnly ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("didNotContainInOrderOnlyElements", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (equal (fumList) or contain inOrderOnly ("FUM", "FOE", "FIE", "FEE"))
        fumList should (equal (toList) or contain inOrderOnly ("FUM", "FOE", "FIE", "FEE"))
        fumList should (equal (fumList) or contain inOrderOnly ("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) or (contain inOrderOnly ("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("didNotContainInOrderOnlyElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
      }

      def `should use an explicitly provided Equality` {
        (fumList should (equal (toList) or contain inOrderOnly ("FUM", "FOE", "FIE", "FEE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (equal (fumList) or contain inOrderOnly ("FUM", "FOE", "FIE", "FEE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (equal (toList) or contain inOrderOnly ("FEE", "FIE", "FOE", "FUM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (equal (fumList) or contain inOrderOnly ("FEE", "FIE", "FOE", "FUM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("didNotContainInOrderOnlyElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
        (fumList should (equal (toList) or contain inOrderOnly (" FEE ", " FIE ", " FOE ", " FUM "))) (decided by invertedListOfStringEquality, after being lowerCased and trimmed)
      }

      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (equal (fumList) or contain inOrderOnly ("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("inOrderOnlyDuplicate")))
      }
    }

    object `when used with (be xx and contain inOrderOnly xx)` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (be_== (fumList) or contain inOrderOnly ("fum", "foe", "fie", "fee"))
        fumList should (be_== (toList) or contain inOrderOnly ("fum", "foe", "fie", "fee"))
        fumList should (be_== (fumList) or contain inOrderOnly ("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (be_== (toList) or contain inOrderOnly ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources("wasNotEqualTo", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("didNotContainInOrderOnlyElements", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (be_== (fumList) or contain inOrderOnly ("FUM", "FOE", "FIE", "FEE"))
        fumList should (be_== (toList) or contain inOrderOnly ("FUM", "FOE", "FIE", "FEE"))
        fumList should (be_== (fumList) or contain inOrderOnly ("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (be_== (toList) or (contain inOrderOnly ("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e1, Resources("wasNotEqualTo", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("didNotContainInOrderOnlyElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
      }

      def `should use an explicitly provided Equality` {
        (fumList should (be_== (fumList) or contain inOrderOnly ("FUM", "FOE", "FIE", "FEE"))) (decided by upperCaseStringEquality)
        (fumList should (be_== (toList) or contain inOrderOnly ("FUM", "FOE", "FIE", "FEE"))) (decided by upperCaseStringEquality)
        (fumList should (be_== (fumList) or contain inOrderOnly ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (be_== (toList) or contain inOrderOnly ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("wasNotEqualTo", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("didNotContainInOrderOnlyElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
        (fumList should (be_== (fumList) or contain inOrderOnly (" FUM ", " FOE ", " FIE ", " FEE "))) (after being lowerCased and trimmed)
      }

      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (be_== (fumList) or contain inOrderOnly ("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("inOrderOnlyDuplicate")))
      }
    }

    object `when used with (contain inOrderOnly xx and be xx)` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (contain inOrderOnly ("fum", "foe", "fie", "fee") or be_== (fumList))
        fumList should (contain inOrderOnly ("fee", "fie", "foe", "fum") or be_== (fumList))
        fumList should (contain inOrderOnly ("fum", "foe", "fie", "fee") or be_== (toList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain inOrderOnly ("fee", "fie", "foe", "fum") or be_== (toList))
        }
        checkMessageStackDepth(e1, Resources("didNotContainInOrderOnlyElements", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\"") + ", and " + Resources("wasNotEqualTo", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (contain inOrderOnly ("FUM", "FOE", "FIE", "FEE") or be_== (fumList))
        fumList should (contain inOrderOnly ("FEE", "FIE", "FOE", "FUM") or be_== (fumList))
        fumList should (contain inOrderOnly ("FUM", "FOE", "FIE", "FEE") or be_== (toList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain inOrderOnly ("FEE", "FIE", "FOE", "FUM") or be_== (toList))
        }
        checkMessageStackDepth(e1, Resources("didNotContainInOrderOnlyElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\"") + ", and " + Resources("wasNotEqualTo", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
      }

      def `should use an explicitly provided Equality` {
        (fumList should (contain inOrderOnly ("FUM", "FOE", "FIE", "FEE") or be_== (fumList))) (decided by upperCaseStringEquality)
        (fumList should (contain inOrderOnly ("FEE", "FIE", "FOE", "FUM") or be_== (fumList))) (decided by upperCaseStringEquality)
        (fumList should (contain inOrderOnly ("FUM", "FOE", "FIE", "FEE") or be_== (toList))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain inOrderOnly ("FEE", "FIE", "FOE", "FUM") or be_== (toList))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotContainInOrderOnlyElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\"") + ", and " + Resources("wasNotEqualTo", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        (fumList should (contain inOrderOnly (" FUM ", " FOE ", " FIE ", " FEE ") or be_== (fumList))) (after being lowerCased and trimmed)
      }

      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (contain inOrderOnly ("fee", "fie", "foe", "fie", "fum") or be_== (fumList))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("inOrderOnlyDuplicate")))
      }
    }

    object `when used with (not contain inOrderOnly xx and not contain inOrderOnly xx)` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (not contain inOrderOnly ("fee", "fie", "foe", "fum") or not contain inOrderOnly ("fee", "fie", "foe", "fum"))
        fumList should (not contain inOrderOnly ("fum", "foe", "fie", "fee") or not contain inOrderOnly ("fee", "fie", "foe", "fum"))
        fumList should (not contain inOrderOnly ("fee", "fie", "foe", "fum") or not contain inOrderOnly ("fum", "foe", "fie", "fee"))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain inOrderOnly ("fum", "foe", "fie", "fee") or not contain inOrderOnly ("fum", "foe", "fie", "fee"))
        }
        checkMessageStackDepth(e1, Resources("containedInOrderOnlyElements", decorateToStringValue(fumList), "\"fum\", \"foe\", \"fie\", \"fee\"") + ", and " + Resources("containedInOrderOnlyElements", decorateToStringValue(fumList), "\"fum\", \"foe\", \"fie\", \"fee\""), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (not contain inOrderOnly ("FEE", "FIE", "FOE", "FUM") or not contain inOrderOnly ("FEE", "FIE", "FOE", "FUM"))
        fumList should (not contain inOrderOnly ("FUM", "FOE", "FIE", "FEE") or not contain inOrderOnly ("FEE", "FIE", "FOE", "FUM"))
        fumList should (not contain inOrderOnly ("FEE", "FIE", "FOE", "FUM") or not contain inOrderOnly ("FUM", "FOE", "FIE", "FEE"))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain inOrderOnly ("FUM", "FOE", "FIE", "FEE") or not contain inOrderOnly ("FUM", "FOE", "FIE", "FEE"))
        }
        checkMessageStackDepth(e1, Resources("containedInOrderOnlyElements", decorateToStringValue(fumList), "\"FUM\", \"FOE\", \"FIE\", \"FEE\"") + ", and " + Resources("containedInOrderOnlyElements", decorateToStringValue(fumList), "\"FUM\", \"FOE\", \"FIE\", \"FEE\""), fileName, thisLineNumber - 2)
      }

      def `should use an explicitly provided Equality` {
        (fumList should (not contain inOrderOnly ("FEE", "FIE", "FOE", "FUM") or not contain inOrderOnly ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (not contain inOrderOnly ("FUM", "FOE", "FIE", "FEE") or not contain inOrderOnly ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (not contain inOrderOnly ("FEE", "FIE", "FOE", "FUM") or not contain inOrderOnly ("FUM", "FOE", "FIE", "FEE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not contain inOrderOnly ("FUM", "FOE", "FIE", "FEE") or not contain inOrderOnly ("FUM", "FOE", "FIE", "FEE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("containedInOrderOnlyElements", decorateToStringValue(fumList), "\"FUM\", \"FOE\", \"FIE\", \"FEE\"") + ", and " + Resources("containedInOrderOnlyElements", decorateToStringValue(fumList), "\"FUM\", \"FOE\", \"FIE\", \"FEE\""), fileName, thisLineNumber - 2)
      }

      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not contain inOrderOnly ("fee", "fie", "foe", "fie", "fum") or not contain inOrderOnly ("fee", "fie", "foe", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("inOrderOnlyDuplicate")))

        val e2 = intercept[exceptions.NotAllowedException] {
          fumList should (not contain inOrderOnly ("fee", "fie", "foe", "fum") or not contain inOrderOnly ("fee", "fie", "foe", "fie", "fum"))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources("inOrderOnlyDuplicate")))
      }
    }

    object `when used with (not equal xx and not contain inOrderOnly xx)` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (not equal (toList) or not contain inOrderOnly ("fee", "fie", "foe", "fum"))
        fumList should (not equal (fumList) or not contain inOrderOnly ("fee", "fie", "foe", "fum"))
        fumList should (not equal (toList) or not contain inOrderOnly ("fum", "foe", "fie", "fee"))
        val e1 = intercept[TestFailedException] {
          fumList should (not equal (fumList) or not contain inOrderOnly ("fum", "foe", "fie", "fee"))
        }
        checkMessageStackDepth(e1, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("containedInOrderOnlyElements", decorateToStringValue(fumList), "\"fum\", \"foe\", \"fie\", \"fee\""), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (not equal (toList) or not contain inOrderOnly ("FEE", "FIE", "FOE", "FUM"))
        fumList should (not equal (fumList) or not contain inOrderOnly ("FEE", "FIE", "FOE", "FUM"))
        fumList should (not equal (toList) or not contain inOrderOnly ("FUM", "FOE", "FIE", "FEE"))
        val e2 = intercept[TestFailedException] {
          fumList should (not equal (fumList) or (not contain inOrderOnly ("FUM", "FOE", "FIE", "FEE")))
        }
        checkMessageStackDepth(e2, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("containedInOrderOnlyElements", decorateToStringValue(fumList), "\"FUM\", \"FOE\", \"FIE\", \"FEE\""), fileName, thisLineNumber - 2)
      }

      def `should use an explicitly provided Equality` {
        (fumList should (not equal (fumList) or not contain inOrderOnly ("FEE", "FIE", "FOE", "FUM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (not equal (toList) or not contain inOrderOnly ("FEE", "FIE", "FOE", "FUM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (not equal (fumList) or not contain inOrderOnly ("FUM", "FOE", "FIE", "FEE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not equal (toList) or not contain inOrderOnly ("FUM", "FOE", "FIE", "FEE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("containedInOrderOnlyElements", decorateToStringValue(fumList), "\"FUM\", \"FOE\", \"FIE\", \"FEE\""), fileName, thisLineNumber - 2)
      }

      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not equal (toList) or not contain inOrderOnly ("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("inOrderOnlyDuplicate")))
      }
    }

    object `when used with (not be xx and not contain inOrderOnly xx)` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (not be_== (toList) or not contain inOrderOnly ("fee", "fie", "foe", "fum"))
        fumList should (not be_== (fumList) or not contain inOrderOnly ("fee", "fie", "foe", "fum"))
        fumList should (not be_== (toList) or not contain inOrderOnly ("fum", "foe", "fie", "fee"))
        val e1 = intercept[TestFailedException] {
          fumList should (not be_== (fumList) or not contain inOrderOnly ("fum", "foe", "fie", "fee"))
        }
        checkMessageStackDepth(e1, Resources("wasEqualTo", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("containedInOrderOnlyElements", decorateToStringValue(fumList), "\"fum\", \"foe\", \"fie\", \"fee\""), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (not be_== (toList) or not contain inOrderOnly ("FEE", "FIE", "FOE", "FUM"))
        fumList should (not be_== (fumList) or not contain inOrderOnly ("FEE", "FIE", "FOE", "FUM"))
        fumList should (not be_== (toList) or not contain inOrderOnly ("FUM", "FOE", "FIE", "FEE"))
        val e1 = intercept[TestFailedException] {
          fumList should (not be_== (fumList) or (not contain inOrderOnly ("FUM", "FOE", "FIE", "FEE")))
        }
        checkMessageStackDepth(e1, Resources("wasEqualTo", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("containedInOrderOnlyElements", decorateToStringValue(fumList), "\"FUM\", \"FOE\", \"FIE\", \"FEE\""), fileName, thisLineNumber - 2)
      }

      def `should use an explicitly provided Equality` {
        (fumList should (not be_== (toList) or not contain inOrderOnly ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        (fumList should (not be_== (fumList) or not contain inOrderOnly ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        (fumList should (not be_== (toList) or not contain inOrderOnly ("FUM", "FOE", "FIE", "FEE"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not be_== (fumList) or not contain inOrderOnly ("FUM", "FOE", "FIE", "FEE"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("wasEqualTo", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("containedInOrderOnlyElements", decorateToStringValue(fumList), "\"FUM\", \"FOE\", \"FIE\", \"FEE\""), fileName, thisLineNumber - 2)
        (fumList should (not contain inOrderOnly (" FEE ", " FIE ", " FOE ", " FUU ") or not contain inOrderOnly (" FEE ", " FIE ", " FOE ", " FUU "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }

      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not be_== (toList) or not contain inOrderOnly ("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("inOrderOnlyDuplicate")))
      }
    }
  }

  object `every of Everys` {

    val list1s: Every[Every[Int]] = Every(Every(1, 2, 2, 3), Every(1, 2, 2, 3), Every(1, 2, 2, 3))
    val lists: Every[Every[Int]] = Every(Every(1, 2, 2, 3), Every(1, 1, 2, 3, 3), Every(2, 3, 4))
    val hiLists: Every[Every[String]] = Every(Every("hi", "hello"), Every("hi", "hello"), Every("hi", "hello"))
    val toLists: Every[Every[String]] = Every(Every("you", "to"), Every("you", "to"), Every("you", "to"))

    def allErrMsg(index: Int, message: String, lineNumber: Int, left: Any): String =
      "'all' inspection failed, because: \n" +
        "  at index " + index + ", " + message + " (" + fileName + ":" + (lineNumber) + ") \n" +
        "in " + decorateToStringValue(left)

    object `when used with (contain inOrderOnly xx and contain inOrderOnly xx)` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (contain inOrderOnly (1, 2, 3) or contain inOrderOnly (1, 2, 3))
        all (list1s) should (contain inOrderOnly (3, 2, 5) or contain inOrderOnly (1, 2, 3))
        all (list1s) should (contain inOrderOnly (1, 2, 3) or contain inOrderOnly (2, 3, 4))

        atLeast (2, lists) should (contain inOrderOnly (1, 2, 3) or contain inOrderOnly (1, 2, 3))
        atLeast (2, lists) should (contain inOrderOnly (3, 6, 5) or contain inOrderOnly (1, 2, 3))
        atLeast (2, lists) should (contain inOrderOnly (1, 2, 3) or contain inOrderOnly (8, 3, 4))

        val e1 = intercept[TestFailedException] {
          all (lists) should (contain inOrderOnly (1, 2, 3) or contain inOrderOnly (1, 2, 3))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(Many(2, 3, 4)) + " did not contain only " + "(1, 2, 3)" + " in order" + ", and " + decorateToStringValue(Many(2, 3, 4)) + " did not contain only " + "(1, 2, 3)" + " in order", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (contain inOrderOnly ("HI", "HELLO") or contain inOrderOnly ("hi", "hello"))
        all (hiLists) should (contain inOrderOnly ("HELLO", "HO") or contain inOrderOnly ("hi", "hello"))
        all (hiLists) should (contain inOrderOnly ("HI", "HELLO") or contain inOrderOnly ("hello", "ho"))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (contain inOrderOnly ("HELLO", "HO") or contain inOrderOnly ("hello", "ho"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(Many("hi", "hello")) + " did not contain only " + "(\"HELLO\", \"HO\")" + " in order" + ", and " + decorateToStringValue(Many("hi", "hello")) + " did not contain only " + "(\"hello\", \"ho\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      def `should use an explicitly provided Equality` {
        (all (hiLists) should (contain inOrderOnly ("HI", "HELLO") or contain inOrderOnly ("hi", "hello"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (contain inOrderOnly ("HELLO", "HO") or contain inOrderOnly ("hi", "hello"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (contain inOrderOnly ("HI", "HELLO") or contain inOrderOnly ("hello", "ho"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)

        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (contain inOrderOnly ("HELLO", "HO") or contain inOrderOnly ("hello", "ho"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(Many("hi", "hello")) + " did not contain only " + "(\"HELLO\", \"HO\")" + " in order" + ", and " + decorateToStringValue(Many("hi", "hello")) + " did not contain only " + "(\"hello\", \"ho\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (contain inOrderOnly (1, 2, 2, 3) or contain inOrderOnly (1, 2, 3))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("inOrderOnlyDuplicate")))

        val e2 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (contain inOrderOnly (1, 2, 3) or contain inOrderOnly (1, 2, 2, 3))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources("inOrderOnlyDuplicate")))
      }
    }

    object `when used with (be xx and contain inOrderOnly xx)` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (be_== (Many(1, 2, 2, 3)) or contain inOrderOnly (1, 2, 3))
        all (list1s) should (be_== (Many(2, 3, 4)) or contain inOrderOnly (1, 2, 3))
        all (list1s) should (be_== (Many(1, 2, 2, 3)) or contain inOrderOnly (2, 3, 4))

        val e1 = intercept[TestFailedException] {
          all (list1s) should (be_== (Many(2, 3, 4)) or contain inOrderOnly (2, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(Many(1, 2, 2, 3)) + " was not equal to " + decorateToStringValue(Many(2, 3, 4)) + ", and " + decorateToStringValue(Many(1, 2, 2, 3)) + " did not contain only " + "(2, 3, 4)" + " in order", thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (be_== (Many("hi", "hello")) or contain inOrderOnly ("HI", "HELLO"))
        all (hiLists) should (be_== (Many("ho", "hello")) or contain inOrderOnly ("HI", "HELLO"))
        all (hiLists) should (be_== (Many("hi", "hello")) or contain inOrderOnly ("HELLO", "HI"))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (be_== (Many("ho", "hello")) or contain inOrderOnly ("HELLO", "HI"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(Many("hi", "hello")) + " was not equal to " + decorateToStringValue(Many("ho", "hello")) + ", and " + decorateToStringValue(Many("hi", "hello")) + " did not contain only " + "(\"HELLO\", \"HI\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      def `should use an explicitly provided Equality` {
        (all (hiLists) should (be_== (Many("hi", "hello")) or contain inOrderOnly ("HI", "HELLO"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (be_== (Many("ho", "hello")) or contain inOrderOnly ("HI", "HELLO"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (be_== (Many("hi", "hello")) or contain inOrderOnly ("HELLO", "HI"))) (decided by upperCaseStringEquality)

        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (be_== (Many("ho", "hello")) or contain inOrderOnly ("HELLO", "HI"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(Many("hi", "hello")) + " was not equal to " + decorateToStringValue(Many("ho", "hello")) + ", and " + decorateToStringValue(Many("hi", "hello")) + " did not contain only " + "(\"HELLO\", \"HI\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (be_== (Many(1, 2, 2, 3)) or contain inOrderOnly (1, 2, 2, 3))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("inOrderOnlyDuplicate")))
      }
    }

    object `when used with (not contain inOrderOnly xx and not contain inOrderOnly xx)` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (not contain inOrderOnly (3, 2, 8) or not contain inOrderOnly (8, 3, 4))
        all (list1s) should (not contain inOrderOnly (1, 2, 3) or not contain inOrderOnly (8, 3, 4))
        all (list1s) should (not contain inOrderOnly (3, 2, 8) or not contain inOrderOnly (1, 2, 3))

        val e1 = intercept[TestFailedException] {
          all (lists) should (not contain inOrderOnly (2, 3, 4) or not contain inOrderOnly (2, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(Many(2, 3, 4)) + " contained only " + "(2, 3, 4)" + " in order" + ", and " + decorateToStringValue(Many(2, 3, 4)) + " contained only " + "(2, 3, 4)" + " in order", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (not contain inOrderOnly ("HELLO", "HI") or not contain inOrderOnly ("hello", "hi"))
        all (hiLists) should (not contain inOrderOnly ("HI", "HELLO") or not contain inOrderOnly ("hello", "hi"))
        all (hiLists) should (not contain inOrderOnly ("HELLO", "HI") or not contain inOrderOnly ("hi", "hello"))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not contain inOrderOnly ("HI", "HELLO") or not contain inOrderOnly ("hi", "hello"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(Many("hi", "hello")) + " contained only " + "(\"HI\", \"HELLO\")" + " in order" + ", and " + decorateToStringValue(Many("hi", "hello")) + " contained only " + "(\"hi\", \"hello\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      def `should use an explicitly provided Equality` {
        (all (hiLists) should (not contain inOrderOnly ("HELLO", "HI") or not contain inOrderOnly ("hello", "hi"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (not contain inOrderOnly ("HI", "HELLO") or not contain inOrderOnly ("hello", "hi"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (not contain inOrderOnly ("HELLO", "HI") or not contain inOrderOnly ("hi", "hello"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)

        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not contain inOrderOnly ("HI", "HELLO") or not contain inOrderOnly ("hi", "hello"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(Many("hi", "hello")) + " contained only " + "(\"HI\", \"HELLO\")" + " in order" + ", and " + decorateToStringValue(Many("hi", "hello")) + " contained only " + "(\"hi\", \"hello\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not contain inOrderOnly (1, 2, 2, 3) or not contain inOrderOnly (8, 3, 4))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("inOrderOnlyDuplicate")))

        val e2 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not contain inOrderOnly (8, 3, 4) or not contain inOrderOnly (1, 2, 2, 3))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources("inOrderOnlyDuplicate")))
      }
    }

    object `when used with (not be xx and not contain inOrderOnly xx)` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (not be_== (One(2)) or not contain inOrderOnly (8, 3, 4))
        all (list1s) should (not be_== (Many(1, 2, 2, 3)) or not contain inOrderOnly (8, 3, 4))
        all (list1s) should (not be_== (One(2)) or not contain inOrderOnly (1, 2, 3))

        val e1 = intercept[TestFailedException] {
          all (list1s) should (not be_== (Many(1, 2, 2, 3)) or not contain inOrderOnly (1, 2, 3))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(Many(1, 2, 2, 3)) + " was equal to " + decorateToStringValue(Many(1, 2, 2, 3)) + ", and " + decorateToStringValue(Many(1, 2, 2, 3)) + " contained only " + "(1, 2, 3)" + " in order", thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (not be_== (Many("hello", "ho")) or not contain inOrderOnly ("HELLO", "HO"))
        all (hiLists) should (not be_== (Many("hi", "hello")) or not contain inOrderOnly ("HELLO", "HO"))
        all (hiLists) should (not be_== (Many("hello", "ho")) or not contain inOrderOnly ("HI", "HELLO"))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not be_== (Many("hi", "hello")) or not contain inOrderOnly ("HI", "HELLO"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(Many("hi", "hello")) + " was equal to " + decorateToStringValue(Many("hi", "hello")) + ", and " + decorateToStringValue(Many("hi", "hello")) + " contained only " + "(\"HI\", \"HELLO\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      def `should use an explicitly provided Equality` {
        (all (hiLists) should (not be_== (Many("hello", "ho")) or not contain inOrderOnly ("HELLO", "HO"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (not be_== (Many("hi", "hello")) or not contain inOrderOnly ("HELLO", "HO"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (not be_== (Many("hello", "ho")) or not contain inOrderOnly ("HI", "HELLO"))) (decided by upperCaseStringEquality)

        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not be_== (Many("hi", "hello")) or not contain inOrderOnly ("HI", "HELLO"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(Many("hi", "hello")) + " was equal to " + decorateToStringValue(Many("hi", "hello")) + ", and " + decorateToStringValue(Many("hi", "hello")) + " contained only " + "(\"HI\", \"HELLO\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not be_== (One(2)) or not contain inOrderOnly (1, 2, 2, 3))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("inOrderOnlyDuplicate")))
      }
    }
  }
}
