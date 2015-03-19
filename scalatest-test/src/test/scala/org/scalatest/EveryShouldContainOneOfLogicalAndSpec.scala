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
import org.scalactic.StringNormalizations._
import SharedHelpers._
import FailureMessages.decorateToStringValue
import scala.collection.JavaConverters._
import Matchers._
import exceptions.TestFailedException

class EveryShouldContainOneOfLogicalAndSpec extends Spec {

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

  val fileName: String = "EveryShouldContainOneOfLogicalAndSpec.scala"

  object `an Every` {

    val fumList: Every[String] = Every("fum")
    val toList: Every[String] = Every("to")

    object `when used with (contain oneOf (..) and contain oneOf (..))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (contain oneOf ("fee", "fie", "foe", "fum") and contain oneOf("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain oneOf ("happy", "birthday", "to", "you") and contain oneOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources.didNotContainOneOfElements(decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (contain oneOf ("fee", "fie", "foe", "fum") and contain oneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e2, Resources.containedOneOfElements(decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\"") + ", but " + Resources.didNotContainOneOfElements(decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (contain oneOf ("FEE", "FIE", "FOE", "FUM") and contain oneOf ("FIE", "FEE", "FUM", "FOE"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain oneOf ("fee", "fie", "foe", "fum") and contain oneOf ("FEE", "FIE", "FOE", "FUM"))
        }
        checkMessageStackDepth(e1, Resources.didNotContainOneOfElements(decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (contain oneOf ("FEE", "FIE", "FOE", "FUM") and (contain oneOf ("fee", "fie", "foe", "fum")))
        }
        checkMessageStackDepth(e2, Resources.containedOneOfElements(decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\"") + ", but " + Resources.didNotContainOneOfElements(decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }

      def `should use an explicitly provided Equality` {
        (fumList should (contain oneOf ("FEE", "FIE", "FOE", "FUM") and contain oneOf ("FIE", "FEE", "FUM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain oneOf ("fee", "fie", "foe", "fum") and contain oneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.didNotContainOneOfElements(decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (contain oneOf ("FEE", "FIE", "FOE", "FUM") and contain oneOf ("fee", "fie", "foe", "fum"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources.containedOneOfElements(decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\"") + ", but " + Resources.didNotContainOneOfElements(decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
        (fumList should (contain oneOf (" FEE ", " FIE ", " FOE ", " FUM ") and contain oneOf (" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }

      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (contain oneOf ("fee", "fie", "foe", "fie", "fum") and contain oneOf("fie", "fee", "fum", "foe"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.oneOfDuplicate))

        val e2 = intercept[exceptions.NotAllowedException] {
          fumList should (contain oneOf ("fie", "fee", "fum", "foe") and contain oneOf("fee", "fie", "foe", "fie", "fum"))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources.oneOfDuplicate))
      }
    }

    object `when used with (equal (..) and contain oneOf (..))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (equal (fumList) and contain oneOf("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) and contain oneOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources.didNotEqual(decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (equal (fumList) and contain oneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e2, Resources.equaled(decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", but " + Resources.didNotContainOneOfElements(decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (equal (fumList) and contain oneOf ("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) and contain oneOf ("FEE", "FIE", "FOE", "FUM"))
        }
        checkMessageStackDepth(e1, Resources.didNotEqual(decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (equal (fumList) and (contain oneOf ("fee", "fie", "foe", "fum")))
        }
        checkMessageStackDepth(e2, Resources.equaled(decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", but " + Resources.didNotContainOneOfElements(decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }

      def `should use an explicitly provided Equality` {
        (fumList should (equal (toList) and contain oneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (equal (fumList) and contain oneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.didNotEqual(decorateToStringValue(fumList), decorateToStringValue(fumList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (equal (toList) and contain oneOf ("fee", "fie", "foe", "fum"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources.equaled(decorateToStringValue(fumList), decorateToStringValue(toList)) + ", but " + Resources.didNotContainOneOfElements(decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
        (fumList should (equal (toList) and contain oneOf (" FEE ", " FIE ", " FOE ", " FUM "))) (decided by invertedListOfStringEquality, after being lowerCased and trimmed)
      }

      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (equal (fumList) and contain oneOf("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.oneOfDuplicate))
      }
    }

    object `when used with (be (..) and contain oneOf (..))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (be (fumList) and contain oneOf("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (be (toList) and contain oneOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources.wasNotEqualTo(decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (be (fumList) and contain oneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e2, Resources.wasEqualTo(decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", but " + Resources.didNotContainOneOfElements(decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (be (fumList) and contain oneOf ("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (be (toList) and contain oneOf ("FEE", "FIE", "FOE", "FUM"))
        }
        checkMessageStackDepth(e1, Resources.wasNotEqualTo(decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (be (fumList) and (contain oneOf ("fee", "fie", "foe", "fum")))
        }
        checkMessageStackDepth(e2, Resources.wasEqualTo(decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", but " + Resources.didNotContainOneOfElements(decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }

      def `should use an explicitly provided Equality` {
        (fumList should (be (fumList) and contain oneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (be (fumList) and contain oneOf ("fee", "fie", "foe", "fum"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.wasEqualTo(decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", but " + Resources.didNotContainOneOfElements(decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (be (toList) and contain oneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources.wasNotEqualTo(decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        (fumList should (be (fumList) and contain oneOf (" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed)
      }

      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (be (fumList) and contain oneOf("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.oneOfDuplicate))
      }
    }

    object `when used with (contain oneOf (..) and be (..))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (contain oneOf("fie", "fee", "fum", "foe") and be (fumList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain oneOf ("fee", "fie", "foe", "fum") and be (toList))
        }
        checkMessageStackDepth(e1, Resources.containedOneOfElements(decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\"") + ", but " + Resources.wasNotEqualTo(decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (contain oneOf ("FEE", "FIE", "FOE", "FUM") and be (fumList))
        }
        checkMessageStackDepth(e2, Resources.didNotContainOneOfElements(decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (contain oneOf ("FEE", "FIE", "FOE", "FUM") and be (fumList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain oneOf ("fie", "fee", "fum", "foe") and be (toList))
        }
        checkMessageStackDepth(e1, Resources.didNotContainOneOfElements(decorateToStringValue(fumList), "\"fie\", \"fee\", \"fum\", \"foe\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (contain oneOf ("fie", "fee", "fum", "foe") and (be (fumList)))
        }
        checkMessageStackDepth(e2, Resources.didNotContainOneOfElements(decorateToStringValue(fumList), "\"fie\", \"fee\", \"fum\", \"foe\""), fileName, thisLineNumber - 2)
      }

      def `should use an explicitly provided Equality` {
        (fumList should (contain oneOf ("FEE", "FIE", "FOE", "FUM") and be (fumList))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain oneOf ("fie", "fee", "fum", "foe") and be (fumList))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.didNotContainOneOfElements(decorateToStringValue(fumList), "\"fie\", \"fee\", \"fum\", \"foe\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (contain oneOf ("FEE", "FIE", "FOE", "FUM") and be (toList))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources.containedOneOfElements(decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\"") + ", but " + Resources.wasNotEqualTo(decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        (fumList should (contain oneOf (" FEE ", " FIE ", " FOE ", " FUM ") and be (fumList))) (after being lowerCased and trimmed)
      }

      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (contain oneOf("fee", "fie", "foe", "fie", "fum") and be (fumList))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.oneOfDuplicate))
      }
    }

    object `when used with (not contain oneOf (..) and not contain oneOf (..))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (not contain oneOf ("FEE", "FIE", "FOE", "FUM") and not contain oneOf("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain oneOf ("fee", "fie", "foe", "fum") and not contain oneOf ("FEE", "FIE", "FOE", "FUM"))
        }
        checkMessageStackDepth(e1, Resources.containedOneOfElements(decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not contain oneOf ("FEE", "FIE", "FOE", "FUM") and not contain oneOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e2, Resources.didNotContainOneOfElements(decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\"") + ", but " + Resources.containedOneOfElements(decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (not contain oneOf ("fee", "fie", "foe", "fum") and not contain oneOf ("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain oneOf ("FEE", "FIE", "FOE", "FUM") and not contain oneOf ("fee", "fie", "foe", "fum"))
        }

        val e2 = intercept[TestFailedException] {
          fumList should (not contain oneOf ("fee", "fie", "foe", "fum") and (not contain oneOf ("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e2, Resources.didNotContainOneOfElements(decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\"") + ", but " + Resources.containedOneOfElements(decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
      }

      def `should use an explicitly provided Equality` {
        (fumList should (not contain oneOf ("fee", "fie", "foe", "fum") and not contain oneOf ("fee", "fie", "foe", "fum"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not contain oneOf ("FEE", "FIE", "FOE", "FUM") and not contain oneOf ("fee", "fie", "foe", "fum"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.containedOneOfElements(decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (not contain oneOf ("fee", "fie", "foe", "fum") and not contain oneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources.didNotContainOneOfElements(decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\"") + ", but " + Resources.containedOneOfElements(decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
        (fumList should (contain oneOf (" FEE ", " FIE ", " FOE ", " FUM ") and contain oneOf (" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }

      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not contain oneOf ("fee", "fie", "foe", "fie", "fum") and not contain oneOf("FEE", "FIE", "FOE", "FUM"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.oneOfDuplicate))

        val e2 = intercept[exceptions.NotAllowedException] {
          fumList should (not contain oneOf ("FEE", "FIE", "FOE", "FUM") and not contain oneOf("fee", "fie", "foe", "fie", "fum"))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources.oneOfDuplicate))
      }
    }

    object `when used with (not equal (..) and not contain oneOf (..))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (not equal (toList) and not contain oneOf("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (not equal (fumList) and not contain oneOf ("FEE", "FIE", "FOE", "FUM"))
        }
        checkMessageStackDepth(e1, Resources.equaled(decorateToStringValue(fumList), decorateToStringValue(fumList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not equal (toList) and not contain oneOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e2, Resources.didNotEqual(decorateToStringValue(fumList), decorateToStringValue(toList)) + ", but " + Resources.containedOneOfElements(decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (not equal (toList) and not contain oneOf ("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (not equal (fumList) and not contain oneOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources.equaled(decorateToStringValue(fumList), decorateToStringValue(fumList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not equal (toList) and (not contain oneOf ("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e2, Resources.didNotEqual(decorateToStringValue(fumList), decorateToStringValue(toList)) + ", but " + Resources.containedOneOfElements(decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
      }

      def `should use an explicitly provided Equality` {
        (fumList should (not equal (fumList) and not contain oneOf ("fee", "fie", "foe", "fum"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not equal (fumList) and not contain oneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.didNotEqual(decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", but " + Resources.containedOneOfElements(decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (not equal (toList) and not contain oneOf ("fee", "fie", "foe", "fum"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources.equaled(decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        (fumList should (not contain oneOf (" FEE ", " FIE ", " FOE ", " FUU ") and not contain oneOf (" FEE ", " FIE ", " FOE ", " FUU "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }

      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not equal (toList) and not contain oneOf("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.oneOfDuplicate))
      }
    }

    object `when used with (not be (..) and not contain oneOf (..))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (not be (toList) and not contain oneOf("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) and not contain oneOf ("FEE", "FIE", "FOE", "FUM"))
        }
        checkMessageStackDepth(e1, Resources.wasEqualTo(decorateToStringValue(fumList), decorateToStringValue(fumList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not be (toList) and not contain oneOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e2, Resources.wasNotEqualTo(decorateToStringValue(fumList), decorateToStringValue(toList)) + ", but " + Resources.containedOneOfElements(decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (not be (toList) and not contain oneOf ("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) and not contain oneOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources.wasEqualTo(decorateToStringValue(fumList), decorateToStringValue(fumList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not be (toList) and (not contain oneOf ("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e2, Resources.wasNotEqualTo(decorateToStringValue(fumList), decorateToStringValue(toList)) + ", but " + Resources.containedOneOfElements(decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
      }

      def `should use an explicitly provided Equality` {
        (fumList should (not be (toList) and not contain oneOf ("fee", "fie", "foe", "fum"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not be (toList) and not contain oneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.wasNotEqualTo(decorateToStringValue(fumList), decorateToStringValue(toList)) + ", but " + Resources.containedOneOfElements(decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (not be (fumList) and not contain oneOf ("fee", "fie", "foe", "fum"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources.wasEqualTo(decorateToStringValue(fumList), decorateToStringValue(fumList)), fileName, thisLineNumber - 2)
        (fumList should (not contain oneOf (" FEE ", " FIE ", " FOE ", " FUU ") and not contain oneOf (" FEE ", " FIE ", " FOE ", " FUU "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }

      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not be (toList) and not contain oneOf("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.oneOfDuplicate))
      }
    }

  }

  object `every of Everys` {

    val list1s: Every[Every[Int]] = Every(Every(1), Every(1), Every(1))
    val lists: Every[Every[Int]] = Every(Every(1), Every(1), Every(2))
    val hiLists: Every[Every[String]] = Every(Every("hi"), Every("hi"), Every("hi"))
    val toLists: Every[Every[String]] = Every(Every("to"), Every("to"), Every("to"))

    def allErrMsg(index: Int, message: String, lineNumber: Int, left: Any): String =
      "'all' inspection failed, because: \n" +
        "  at index " + index + ", " + message + " (" + fileName + ":" + (lineNumber) + ") \n" +
        "in " + decorateToStringValue(left)

    object `when used with (contain oneOf (..) and contain oneOf (..))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (contain oneOf (3, 2, 1) and contain oneOf (1, 3, 4))
        atLeast (2, lists) should (contain oneOf (3, 1, 5) and contain oneOf (1, 3, 4))
        atMost (2, lists) should (contain oneOf (3, 2, 8) and contain oneOf (2, 3, 4))
        no (lists) should (contain oneOf (3, 6, 9) and contain oneOf (3, 4, 5))

        val e1 = intercept[TestFailedException] {
          all (lists) should (contain oneOf (1, 6, 8) and contain oneOf (1, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(2, FailureMessages.didNotContainOneOfElements(lists(2), UnquotedString("1, 6, 8")), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (lists) should (contain oneOf (1, 2, 8) and contain oneOf (1, 3, 4))
        }
        checkMessageStackDepth(e2, allErrMsg(2, FailureMessages.containedOneOfElements(lists(2), UnquotedString("1, 2, 8")) + ", but " + FailureMessages.didNotContainOneOfElements(lists(2), UnquotedString("1, 3, 4")), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e3 = intercept[TestFailedException] {
          all (hiLists) should (contain oneOf ("hi", "hello") and contain oneOf ("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e3, allErrMsg(0, FailureMessages.containedOneOfElements(hiLists(0), UnquotedString("\"hi\", \"hello\"")) + ", but " + FailureMessages.didNotContainOneOfElements(hiLists(0), UnquotedString("\"ho\", \"hey\", \"howdy\"")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e4 = intercept[TestFailedException] {
          all (lists) should (contain oneOf (1, 2, 8) and contain oneOf (1, 3, 4))
        }
        checkMessageStackDepth(e4, allErrMsg(2, FailureMessages.containedOneOfElements(lists(2), UnquotedString("1, 2, 8")) + ", but " + FailureMessages.didNotContainOneOfElements(lists(2), UnquotedString("1, 3, 4")), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (contain oneOf ("HI", "HE") and contain oneOf ("HI", "HE"))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (contain oneOf ("hi", "he") and contain oneOf ("HI", "HE"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, FailureMessages.didNotContainOneOfElements(hiLists(0), UnquotedString("\"hi\", \"he\"")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (hiLists) should (contain oneOf ("HI", "HE") and contain oneOf ("hi", "he"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, FailureMessages.containedOneOfElements(hiLists(0), UnquotedString("\"HI\", \"HE\"")) + ", but " + FailureMessages.didNotContainOneOfElements(hiLists(0), UnquotedString("\"hi\", \"he\"")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      def `should use an explicitly provided Equality` {
        (all (hiLists) should (contain oneOf ("HI", "HE") and contain oneOf ("HI", "HE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (contain oneOf ("hi", "he") and contain oneOf ("HI", "HE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, FailureMessages.didNotContainOneOfElements(hiLists(0), UnquotedString("\"hi\", \"he\"")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (contain oneOf ("HI", "HE") and contain oneOf ("hi", "he"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, FailureMessages.containedOneOfElements(hiLists(0), UnquotedString("\"HI\", \"HE\"")) + ", but " + FailureMessages.didNotContainOneOfElements(hiLists(0), UnquotedString("\"hi\", \"he\"")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (contain oneOf (3, 2, 2, 1) and contain oneOf (1, 3, 4))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.oneOfDuplicate))

        val e2 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (contain oneOf (1, 3, 4) and contain oneOf (3, 2, 2, 1))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources.oneOfDuplicate))
      }
    }

    object `when used with (be (..) and contain oneOf (..))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (be (One(1)) and contain oneOf (1, 3, 4))
        atLeast (2, lists) should (be (One(1)) and contain oneOf (1, 3, 4))
        atMost (2, lists) should (be (One(1)) and contain oneOf (2, 3, 4))
        no (lists) should (be (One(8)) and contain oneOf (3, 4, 5))

        val e1 = intercept[TestFailedException] {
          all (lists) should (be (One(1)) and contain oneOf (1, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(One(2)) + " was not equal to " + decorateToStringValue(One(1)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (list1s) should (be (One(1)) and contain oneOf (2, 3, 8))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(One(1)) + " was equal to " + decorateToStringValue(One(1)) + ", but " + FailureMessages.didNotContainOneOfElements(list1s(0), UnquotedString("2, 3, 8")), thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)

        val e3 = intercept[TestFailedException] {
          all (hiLists) should (be (One("hi")) and contain oneOf ("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e3, allErrMsg(0, decorateToStringValue(One("hi")) + " was equal to " + decorateToStringValue(One("hi")) + ", but " + FailureMessages.didNotContainOneOfElements(hiLists(0), UnquotedString("\"ho\", \"hey\", \"howdy\"")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e4 = intercept[TestFailedException] {
          all (list1s) should (be (One(1)) and contain oneOf (2, 3, 8))
        }
        checkMessageStackDepth(e4, allErrMsg(0, decorateToStringValue(One(1)) + " was equal to " + decorateToStringValue(One(1)) + ", but " + FailureMessages.didNotContainOneOfElements(list1s(0), UnquotedString("2, 3, 8")), thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (be (One("hi")) and contain oneOf ("HI", "HE"))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (be (One("ho")) and contain oneOf ("hi", "he"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(One("hi")) + " was not equal to " + decorateToStringValue(One("ho")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (hiLists) should (be (One("hi")) and contain oneOf ("hi", "he"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(One("hi")) + " was equal to " + decorateToStringValue(One("hi")) + ", but " + FailureMessages.didNotContainOneOfElements(hiLists(0), UnquotedString("\"hi\", \"he\"")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      def `should use an explicitly provided Equality` {
        (all (hiLists) should (be (One("hi")) and contain oneOf ("HI", "HE"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (be (One("ho")) and contain oneOf ("hi", "he"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(One("hi")) + " was not equal to " + decorateToStringValue(One("ho")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (be (One("hi")) and contain oneOf ("hi", "he"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(One("hi")) + " was equal to " + decorateToStringValue(One("hi")) + ", but " + FailureMessages.didNotContainOneOfElements(hiLists(0), UnquotedString("\"hi\", \"he\"")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (be (One(1)) and contain oneOf (3, 2, 2, 1))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.oneOfDuplicate))
      }
    }

    object `when used with (not contain oneOf (..) and not contain oneOf (..))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (not contain oneOf (3, 2, 8) and not contain oneOf (8, 3, 4))
        atLeast (2, lists) should (not contain oneOf (3, 8, 5) and not contain oneOf (8, 3, 4))
        atMost (2, lists) should (not contain oneOf (3, 6, 8) and contain oneOf (5, 3, 4))
        no (lists) should (not contain oneOf (1, 2, 9) and not contain oneOf (2, 1, 5))

        val e1 = intercept[TestFailedException] {
          all (lists) should (not contain oneOf (2, 6, 8) and not contain oneOf (2, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(2, FailureMessages.containedOneOfElements(lists(2), UnquotedString("2, 6, 8")), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (lists) should (not contain oneOf (3, 6, 8) and not contain oneOf (2, 3, 4))
        }
        checkMessageStackDepth(e2, allErrMsg(2, FailureMessages.didNotContainOneOfElements(lists(2), UnquotedString("3, 6, 8")) + ", but " + FailureMessages.containedOneOfElements(lists(2), UnquotedString("2, 3, 4")), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e3 = intercept[TestFailedException] {
          all (hiLists) should (not contain oneOf ("hi", "hello") and not contain oneOf ("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e3, allErrMsg(0, FailureMessages.containedOneOfElements(hiLists(0), UnquotedString("\"hi\", \"hello\"")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e4 = intercept[TestFailedException] {
          all (hiLists) should (not contain oneOf ("ho", "hey", "howdy") and not contain oneOf ("hi", "hello"))
        }
        checkMessageStackDepth(e4, allErrMsg(0, FailureMessages.didNotContainOneOfElements(hiLists(0), UnquotedString("\"ho\", \"hey\", \"howdy\"")) + ", but " + FailureMessages.containedOneOfElements(hiLists(0), UnquotedString("\"hi\", \"hello\"")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (not contain oneOf ("hi", "he") and not contain oneOf ("hi", "he"))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not contain oneOf ("HI", "HE") and not contain oneOf ("HO", "HEY", "HOWDY"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, FailureMessages.containedOneOfElements(hiLists(0), UnquotedString("\"HI\", \"HE\"")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (hiLists) should (not contain oneOf ("HO", "HEY", "HOWDY") and not contain oneOf ("HI", "HE"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, FailureMessages.didNotContainOneOfElements(hiLists(0), UnquotedString("\"HO\", \"HEY\", \"HOWDY\"")) + ", but " + FailureMessages.containedOneOfElements(hiLists(0), UnquotedString("\"HI\", \"HE\"")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      def `should use an explicitly provided Equality` {
        (all (hiLists) should (not contain oneOf ("hi", "he") and not contain oneOf ("hi", "he"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not contain oneOf ("HI", "HE") and not contain oneOf ("HO", "HEY", "HOWDY"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, FailureMessages.containedOneOfElements(hiLists(0), UnquotedString("\"HI\", \"HE\"")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (not contain oneOf ("HO", "HEY", "HOWDY") and not contain oneOf ("HI", "HE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, FailureMessages.didNotContainOneOfElements(hiLists(0), UnquotedString("\"HO\", \"HEY\", \"HOWDY\"")) + ", but " + FailureMessages.containedOneOfElements(hiLists(0), UnquotedString("\"HI\", \"HE\"")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not contain oneOf (3, 2, 2, 1) and not contain oneOf (8, 3, 4))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.oneOfDuplicate))

        val e2 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not contain oneOf (8, 3, 4) and not contain oneOf (3, 2, 2, 1))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources.oneOfDuplicate))
      }
    }

    object `when used with (not be (..) and not contain oneOf (..))` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (not be (One(2)) and not contain oneOf (8, 3, 4))
        atLeast (2, lists) should (not be (One(3)) and not contain oneOf (8, 3, 4))
        atMost (2, lists) should (not be (One(3)) and contain oneOf (5, 3, 4))
        no (list1s) should (not be (One(1)) and not contain oneOf (2, 1, 5))

        val e1 = intercept[TestFailedException] {
          all (lists) should (not be (One(2)) and not contain oneOf (2, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(One(2)) + " was equal to " + decorateToStringValue(One(2)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (lists) should (not be (One(3)) and not contain oneOf (2, 3, 4))
        }
        checkMessageStackDepth(e2, allErrMsg(2, decorateToStringValue(One(2)) + " was not equal to " + decorateToStringValue(One(3)) + ", but " + FailureMessages.containedOneOfElements(lists(2), UnquotedString("2, 3, 4")), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)

        val e3 = intercept[TestFailedException] {
          all (hiLists) should (not be (One("hi")) and not contain oneOf ("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e3, allErrMsg(0, decorateToStringValue(One("hi")) + " was equal to " + decorateToStringValue(One("hi")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e4 = intercept[TestFailedException] {
          all (hiLists) should (not be (One("ho")) and not contain oneOf ("hi", "hello"))
        }
        checkMessageStackDepth(e4, allErrMsg(0, decorateToStringValue(One("hi")) + " was not equal to " + decorateToStringValue(One("ho")) + ", but " + FailureMessages.containedOneOfElements(hiLists(0), UnquotedString("\"hi\", \"hello\"")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (not be (One("ho")) and not contain oneOf ("hi", "he"))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not be (One("hi")) and not contain oneOf ("HI", "HE"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(One("hi")) + " was equal to " + decorateToStringValue(One("hi")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (hiLists) should (not be (One("ho")) and not contain oneOf ("HI", "HE"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(One("hi")) + " was not equal to " + decorateToStringValue(One("ho")) + ", but " + FailureMessages.containedOneOfElements(hiLists(0), UnquotedString("\"HI\", \"HE\"")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      def `should use an explicitly provided Equality` {
        (all (hiLists) should (not be (One("ho")) and not contain oneOf ("hi", "he"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not be (One("hi")) and not contain oneOf ("HI", "HE"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(One("hi")) + " was equal to " + decorateToStringValue(One("hi")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (not be (One("ho")) and not contain oneOf ("HI", "HE"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(One("hi")) + " was not equal to " + decorateToStringValue(One("ho")) + ", but " + FailureMessages.containedOneOfElements(hiLists(0), UnquotedString("\"HI\", \"HE\"")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not be (One(2)) and not contain oneOf (3, 2, 2, 1))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.oneOfDuplicate))
      }
    }
  }
}
