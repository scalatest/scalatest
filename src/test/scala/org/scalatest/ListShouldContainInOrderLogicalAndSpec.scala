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

import org.scalautils.Equality
import org.scalautils.Normalization
import org.scalautils.StringNormalizations._
import SharedHelpers._
import FailureMessages.decorateToStringValue

class ListShouldContainInOrderLogicalAndSpec extends Spec with Matchers {

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
      case _ => value
    }
  
  val upperCaseListOfStringEquality = 
    new Equality[List[String]] {
      def areEqual(a: List[String], b: Any): Boolean = upperCase(a) == upperCase(b)
    }

  //ADDITIONAL//

  val fileName: String = "ListShouldContainInOrderLogicalAndSpec.scala"

  object `a List` {

    val fumList: List[String] = List("fex", "fum", "fum", "foe", "fie", "fie", "fee")
    val toList: List[String] = List("too", "you", "you", "to", "birthday", "happy", "happy")

    object `when used with (contain inOrder xx and contain inOrder xx)` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (newContain newInOrder ("fum", "foe", "fie", "fee") and newContain newInOrder ("fum", "foe", "fie", "fee"))
        val e1 = intercept[TestFailedException] {
          fumList should (newContain newInOrder ("fee", "fie", "foe", "fum") and newContain newInOrder ("fum", "foe", "fie", "fee"))
        }
        checkMessageStackDepth(e1, Resources("didNotContainAllOfElementsInOrder", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (newContain newInOrder ("fum", "foe", "fie", "fee") and newContain newInOrder ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e2, Resources("containedAllOfElementsInOrder", decorateToStringValue(fumList), "\"fum\", \"foe\", \"fie\", \"fee\"") + ", but " + Resources("didNotContainAllOfElementsInOrder", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (newContain newInOrder ("FUM", "FOE", "FIE", "FEE") and newContain newInOrder ("FUM", "FOE", "FIE", "FEE"))
        val e1 = intercept[TestFailedException] {
          fumList should (newContain newInOrder ("FEE", "FIE", "FOE", "FUM") and newContain newInOrder ("FUM", "FOE", "FIE", "FEE"))
        }
        checkMessageStackDepth(e1, Resources("didNotContainAllOfElementsInOrder", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (newContain newInOrder ("FUM", "FOE", "FIE", "FEE") and (newContain newInOrder ("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e2, Resources("containedAllOfElementsInOrder", decorateToStringValue(fumList), "\"FUM\", \"FOE\", \"FIE\", \"FEE\"") + ", but " + Resources("didNotContainAllOfElementsInOrder", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
      }

      def `should use an explicitly provided Equality` {
        (fumList should (newContain newInOrder ("FUM", "FOE", "FIE", "FEE") and newContain newInOrder ("FUM", "FOE", "FIE", "FEE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (newContain newInOrder ("FEE", "FIE", "FOE", "FUM") and newContain newInOrder ("FUM", "FOE", "FIE", "FEE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotContainAllOfElementsInOrder", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (newContain newInOrder ("FUM", "FOE", "FIE", "FEE") and newContain newInOrder ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources("containedAllOfElementsInOrder", decorateToStringValue(fumList), "\"FUM\", \"FOE\", \"FIE\", \"FEE\"") + ", but " + Resources("didNotContainAllOfElementsInOrder", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
        (fumList should (newContain newInOrder (" FUM ", " FOE ", " FIE ", " FEE ") and newContain newInOrder (" FUM ", " FOE ", " FIE ", " FEE "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
    }

    object `when used with (equal xx and contain inOrder xx)` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (equal (fumList) and newContain newInOrder ("fum", "foe", "fie", "fee"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) and newContain newInOrder ("fum", "foe", "fie", "fee"))
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (equal (fumList) and newContain newInOrder ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e2, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", but " + Resources("didNotContainAllOfElementsInOrder", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (equal (fumList) and newContain newInOrder ("FUM", "FOE", "FIE", "FEE"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) and newContain newInOrder ("FUM", "FOE", "FIE", "FEE"))
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (equal (fumList) and (newContain newInOrder ("FEE", "FIE", "FOE", "FAM")))
        }
        checkMessageStackDepth(e2, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", but " + Resources("didNotContainAllOfElementsInOrder", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FAM\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (equal (toList) and newContain newInOrder ("FUM", "FOE", "FIE", "FEE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (equal (toList) and newContain newInOrder ("FEE", "FIE", "FOE", "FUM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", but " + Resources("didNotContainAllOfElementsInOrder", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (equal (fumList) and newContain newInOrder ("FUM", "FOE", "FIE", "FEE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(fumList)), fileName, thisLineNumber - 2)
        (fumList should (equal (toList) and newContain newInOrder (" FUM ", " FOE ", " FIE ", " FEE "))) (decided by invertedListOfStringEquality, after being lowerCased and trimmed)
      }
    }
    
    object `when used with (legacyEqual xx and contain inOrder xx)` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (legacyEqual (fumList) and newContain newInOrder ("fum", "foe", "fie", "fee"))
        val e1 = intercept[TestFailedException] {
          fumList should (legacyEqual (toList) and newContain newInOrder ("fum", "foe", "fie", "fee"))
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (legacyEqual (fumList) and newContain newInOrder ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e2, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", but " + Resources("didNotContainAllOfElementsInOrder", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (legacyEqual (fumList) and newContain newInOrder ("FUM", "FOE", "FIE", "FEE"))
        val e1 = intercept[TestFailedException] {
          fumList should (legacyEqual (toList) and newContain newInOrder ("FUM", "FOE", "FIE", "FEE"))
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (legacyEqual (fumList) and (newContain newInOrder ("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e2, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", but " + Resources("didNotContainAllOfElementsInOrder", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (legacyEqual (fumList) and newContain newInOrder ("FUM", "FOE", "FIE", "FEE"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (legacyEqual (fumList) and newContain newInOrder ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", but " + Resources("didNotContainAllOfElementsInOrder", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (legacyEqual (toList) and newContain newInOrder ("FUM", "FOE", "FIE", "FEE"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        (fumList should (legacyEqual (fumList) and newContain newInOrder (" FUM ", " FOE ", " FIE ", " FEE "))) (after being lowerCased and trimmed)
      }
    }

    object `when used with (contain inOrder xx and legacyEqual xx)` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (newContain newInOrder ("fum", "foe", "fie", "fee") and legacyEqual (fumList))
        val e1 = intercept[TestFailedException] {
          fumList should (newContain newInOrder ("fum", "foe", "fie", "fee") and legacyEqual (toList))
        }
        checkMessageStackDepth(e1, Resources("containedAllOfElementsInOrder", decorateToStringValue(fumList), "\"fum\", \"foe\", \"fie\", \"fee\"") + ", but " + Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (newContain newInOrder ("fee", "fie", "foe", "fum") and legacyEqual (fumList))
        }
        checkMessageStackDepth(e2, Resources("didNotContainAllOfElementsInOrder", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (newContain newInOrder ("FUM", "FOE", "FIE", "FEE") and legacyEqual (fumList))
        val e1 = intercept[TestFailedException] {
          fumList should (newContain newInOrder ("FEE", "FIE", "FOE", "FUM") and legacyEqual (toList))
        }
        checkMessageStackDepth(e1, Resources("didNotContainAllOfElementsInOrder", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (newContain newInOrder ("HAPPY", "BIRTHDAY", "TO", "YOU") and (legacyEqual (fumList)))
        }
        checkMessageStackDepth(e2, Resources("didNotContainAllOfElementsInOrder", decorateToStringValue(fumList), "\"HAPPY\", \"BIRTHDAY\", \"TO\", \"YOU\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (newContain newInOrder ("FUM", "FOE", "FIE", "FEE") and legacyEqual (fumList))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (newContain newInOrder ("FEE", "FIE", "FOE", "FUM") and legacyEqual (fumList))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotContainAllOfElementsInOrder", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (newContain newInOrder ("FUM", "FOE", "FIE", "FEE") and legacyEqual (toList))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources("containedAllOfElementsInOrder", decorateToStringValue(fumList), "\"FUM\", \"FOE\", \"FIE\", \"FEE\"") + ", but " + Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        (fumList should (newContain newInOrder (" FUM ", " FOE ", " FIE ", " FEE ") and legacyEqual (fumList))) (after being lowerCased and trimmed)
      }
    }
    
    object `when used with (not contain inOrder xx and not contain inOrder xx)` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (not newContain newInOrder ("fee", "fie", "foe", "fum") and not newContain newInOrder ("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (not newContain newInOrder ("fum", "foe", "fie", "fee") and not newContain newInOrder ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, Resources("containedAllOfElementsInOrder", decorateToStringValue(fumList), "\"fum\", \"foe\", \"fie\", \"fee\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not newContain newInOrder ("fee", "fie", "foe", "fum") and not newContain newInOrder ("fum", "foe", "fie", "fee"))
        }
        checkMessageStackDepth(e2, Resources("didNotContainAllOfElementsInOrder", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\"") + ", but " + Resources("containedAllOfElementsInOrder", decorateToStringValue(fumList), "\"fum\", \"foe\", \"fie\", \"fee\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (not newContain newInOrder ("FEE", "FIE", "FOE", "FUM") and not newContain newInOrder ("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (not newContain newInOrder ("FUM", "FOE", "FIE", "FEE") and not newContain newInOrder ("FEE", "FIE", "FOE", "FUM"))
        }
        checkMessageStackDepth(e1, Resources("containedAllOfElementsInOrder", decorateToStringValue(fumList), "\"FUM\", \"FOE\", \"FIE\", \"FEE\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not newContain newInOrder ("FEE", "FIE", "FOE", "FUM") and (not newContain newInOrder ("FUM", "FOE", "FIE", "FEE")))
        }
        checkMessageStackDepth(e2, Resources("didNotContainAllOfElementsInOrder", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\"") + ", but " + Resources("containedAllOfElementsInOrder", decorateToStringValue(fumList), "\"FUM\", \"FOE\", \"FIE\", \"FEE\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (not newContain newInOrder ("FEE", "FIE", "FOE", "FUM") and not newContain newInOrder ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not newContain newInOrder ("FEE", "FIE", "FOE", "FUM") and not newContain newInOrder ("FUM", "FOE", "FIE", "FEE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotContainAllOfElementsInOrder", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\"") + ", but " + Resources("containedAllOfElementsInOrder", decorateToStringValue(fumList), "\"FUM\", \"FOE\", \"FIE\", \"FEE\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (not newContain newInOrder ("FUM", "FOE", "FIE", "FEE") and not newContain newInOrder ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources("containedAllOfElementsInOrder", decorateToStringValue(fumList), "\"FUM\", \"FOE\", \"FIE\", \"FEE\""), fileName, thisLineNumber - 2)
      }
    }
    
    object `when used with (not equal xx and not contain inOrder xx)` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (not equal (toList) and not newContain newInOrder ("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (not equal (fumList) and not newContain newInOrder ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(fumList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not equal (toList) and not newContain newInOrder ("fum", "foe", "fie", "fee"))
        }
        checkMessageStackDepth(e2, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", but " + Resources("containedAllOfElementsInOrder", decorateToStringValue(fumList), "\"fum\", \"foe\", \"fie\", \"fee\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (not equal (toList) and not newContain newInOrder ("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (not equal (fumList) and not newContain newInOrder ("FEE", "FIE", "FOE", "FUM"))
        }
        checkMessageStackDepth(e1, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(fumList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not equal (toList) and (not newContain newInOrder ("FUM", "FOE", "FIE", "FEE")))
        }
        checkMessageStackDepth(e2, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", but " + Resources("containedAllOfElementsInOrder", decorateToStringValue(fumList), "\"FUM\", \"FOE\", \"FIE\", \"FEE\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (not equal (fumList) and not newContain newInOrder ("FEE", "FIE", "FOE", "FUM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not equal (fumList) and not newContain newInOrder ("FUM", "FOE", "FIE", "FEE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", but " + Resources("containedAllOfElementsInOrder", decorateToStringValue(fumList), "\"FUM\", \"FOE\", \"FIE\", \"FEE\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (not equal (toList) and not newContain newInOrder ("FEE", "FIE", "FOE", "FUM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
      }
    }
    
    object `when used with (not be xx and not contain inOrder xx)` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (not be (toList) and not newContain newInOrder ("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) and not newContain newInOrder ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources("wasEqualTo", decorateToStringValue(fumList), decorateToStringValue(fumList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not be (toList) and not newContain newInOrder ("fum", "foe", "fie", "fee"))
        }
        checkMessageStackDepth(e2, Resources("wasNotEqualTo", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", but " + Resources("containedAllOfElementsInOrder", decorateToStringValue(fumList), "\"fum\", \"foe\", \"fie\", \"fee\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (not be (toList) and not newContain newInOrder ("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) and not newContain newInOrder ("FEE", "FIE", "FOE", "FUM"))
        }
        checkMessageStackDepth(e1, Resources("wasEqualTo", decorateToStringValue(fumList), decorateToStringValue(fumList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not be (toList) and (not newContain newInOrder ("FUM", "FOE", "FIE", "FEE")))
        }
        checkMessageStackDepth(e2, Resources("wasNotEqualTo", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", but " + Resources("containedAllOfElementsInOrder", decorateToStringValue(fumList), "\"FUM\", \"FOE\", \"FIE\", \"FEE\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (not be (toList) and not newContain newInOrder ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not be (toList) and not newContain newInOrder ("FUM", "FOE", "FIE", "FEE"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("wasNotEqualTo", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", but " + Resources("containedAllOfElementsInOrder", decorateToStringValue(fumList), "\"FUM\", \"FOE\", \"FIE\", \"FEE\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (not be (fumList) and not newContain newInOrder ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources("wasEqualTo", decorateToStringValue(fumList), decorateToStringValue(fumList)), fileName, thisLineNumber - 2)
        (fumList should (not newContain newInOrder (" FEE ", " FIE ", " FOE ", " FUU ") and not newContain newInOrder (" FEE ", " FIE ", " FOE ", " FUU "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
    }
  }
  
  object `col of Lists` {
    
    val list1s: Vector[List[Int]] = Vector(List(0, 1, 2, 2, 3), List(0, 1, 2, 2, 3), List(0, 1, 2, 2, 3))
    val lists: Vector[List[Int]] = Vector(List(0, 1, 2, 3, 3), List(0, 1, 2, 3, 3), List(8, 2, 3, 4))
    val nils: Vector[List[Int]] = Vector(Nil, Nil, Nil)
    val listsNil: Vector[List[Int]] = Vector(List(0, 1, 2, 3), List(0, 1, 2, 3), Nil)
    val hiLists: Vector[List[String]] = Vector(List("he", "hi", "hello"), List("he", "hi", "hello"), List("he", "hi", "hello"))
    
    def allErrMsg(index: Int, message: String, lineNumber: Int, left: Any): String = 
      "'all' inspection failed, because: \n" +
      "  at index " + index + ", " + message + " (" + fileName + ":" + (lineNumber) + ") \n" +
      "in " + decorateToStringValue(left)
    
    object `used with contain inOrder xx and contain inOrder xx` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (newContain newInOrder (1, 2, 3) and newContain newInOrder (1, 2, 3))
        atLeast (2, lists) should (newContain newInOrder (1, 2, 3) and newContain newInOrder (1, 2, 3))
        atMost (2, lists) should (newContain newInOrder (1, 2, 3) and newContain newInOrder (1, 2, 3))
        no (lists) should (newContain newInOrder (3, 6, 9) and newContain newInOrder (3, 4, 5))
        no (nils) should (newContain newInOrder (1, 2, 8) and newContain newInOrder (1, 3, 4))
        no (listsNil) should (newContain newInOrder (3, 8, 5) and newContain newInOrder (3, 4, 5))
        
        val e1 = intercept[TestFailedException] {
          all (lists) should (newContain newInOrder (1, 2, 3) and newContain newInOrder (1, 2, 3))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(lists(2)) + " did not contain all of " + "(1, 2, 3)" + " in order", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (list1s) should (newContain newInOrder (1, 2, 3) and newContain newInOrder (1, 3, 4))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(list1s(0)) + " contained all of " + "(1, 2, 3)" + " in order" + ", but " + decorateToStringValue(list1s(0)) + " did not contain all of " + "(1, 3, 4)" + " in order", thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
        
        val e3 = intercept[TestFailedException] {
          all (nils) should (newContain newInOrder ("hi", "hello") and newContain newInOrder ("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e3, allErrMsg(0, decorateToStringValue(nils(0)) + " did not contain all of " + "(\"hi\", \"hello\")" + " in order", thisLineNumber - 2, nils), fileName, thisLineNumber - 2)
        
        val e4 = intercept[TestFailedException] {
          all (hiLists) should (newContain newInOrder ("hi", "hello") and newContain newInOrder ("hello", "hi"))
        }
        checkMessageStackDepth(e4, allErrMsg(0, decorateToStringValue(hiLists(0)) + " contained all of " + "(\"hi\", \"hello\")" + " in order" + ", but " + decorateToStringValue(hiLists(0)) + " did not contain all of " + "(\"hello\", \"hi\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (newContain newInOrder ("HI", "HELLO") and newContain newInOrder ("HI", "HELLO"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (newContain newInOrder ("HO", "HELLO") and newContain newInOrder ("HI", "HELLO"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(hiLists(0)) + " did not contain all of " + "(\"HO\", \"HELLO\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (hiLists) should (newContain newInOrder ("HI", "HELLO") and newContain newInOrder ("HELLO", "HI"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(hiLists(0)) + " contained all of " + "(\"HI\", \"HELLO\")" + " in order" + ", but " + decorateToStringValue(hiLists(0)) + " did not contain all of " + "(\"HELLO\", \"HI\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      def `should use an explicitly provided Equality` {
        (all (hiLists) should (newContain newInOrder ("HI", "HELLO") and newContain newInOrder ("HI", "HELLO"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (newContain newInOrder ("HO", "HELLO") and newContain newInOrder ("HI", "HELLO"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(hiLists(0)) + " did not contain all of " + "(\"HO\", \"HELLO\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (newContain newInOrder ("HI", "HELLO") and newContain newInOrder ("HELLO", "HI"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(hiLists(0)) + " contained all of " + "(\"HI\", \"HELLO\")" + " in order" + ", but " + decorateToStringValue(hiLists(0)) + " did not contain all of " + "(\"HELLO\", \"HI\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
    }

    object `when used with (be xx and contain inOrder xx)` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (be (List(0, 1, 2, 2, 3)) and newContain newInOrder (1, 2, 3))
        atLeast (2, lists) should (be (List(0, 1, 2, 3, 3)) and newContain newInOrder (1, 2, 3))
        atMost (2, lists) should (be (List(3, 2, 1)) and newContain newInOrder (1, 2, 3))
        no (lists) should (be (List(3, 6, 9)) and newContain newInOrder (3, 4, 5))
        no (nils) should (be (List(1, 6, 8)) and newContain newInOrder (1, 3, 4))
        no (listsNil) should (be (List(2, 6, 8)) and newContain newInOrder (3, 4, 5))
        
        val e1 = intercept[TestFailedException] {
          all (lists) should (be (List(0, 1, 2, 3, 3)) and newContain newInOrder (1, 2, 3))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(lists(2)) + " was not equal to " + decorateToStringValue(List(0, 1, 2, 3, 3)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (list1s) should (be (List(0, 1, 2, 2, 3)) and newContain newInOrder (2, 3, 8))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(list1s(0)) + " was equal to " + decorateToStringValue(List(0, 1, 2, 2, 3)) + ", but " + decorateToStringValue(list1s(0)) + " did not contain all of " + "(2, 3, 8)" + " in order", thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
        
        val e3 = intercept[TestFailedException] {
          all (nils) should (be (List("hey")) and newContain newInOrder ("hi", "hello"))
        }
        checkMessageStackDepth(e3, allErrMsg(0, decorateToStringValue(nils(0)) + " was not equal to " + decorateToStringValue(List("hey")), thisLineNumber - 2, nils), fileName, thisLineNumber - 2)
        
        val e4 = intercept[TestFailedException] {
          all (hiLists) should (be (List("he", "hi", "hello")) and newContain newInOrder ("hello", "hi"))
        }
        checkMessageStackDepth(e4, allErrMsg(0, decorateToStringValue(hiLists(0)) + " was equal to " + decorateToStringValue(List("he", "hi", "hello")) + ", but " + decorateToStringValue(hiLists(0)) + " did not contain all of " + "(\"hello\", \"hi\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e5 = intercept[TestFailedException] {
          all (listsNil) should (be (List(0, 1, 2, 3)) and newContain newInOrder (1, 2, 3))
        }
        checkMessageStackDepth(e5, allErrMsg(2, decorateToStringValue(listsNil(2)) + " was not equal to " + decorateToStringValue(List(0, 1, 2, 3)), thisLineNumber - 2, listsNil), fileName, thisLineNumber - 2)
        
        val e6 = intercept[TestFailedException] {
          all (list1s) should (be (List(0, 1, 2, 2, 3)) and newContain newInOrder (2, 3, 8))
        }
        checkMessageStackDepth(e6, allErrMsg(0, decorateToStringValue(list1s(0)) + " was equal to " + decorateToStringValue(List(0, 1, 2, 2, 3)) + ", but " + decorateToStringValue(list1s(0)) + " did not contain all of " + "(2, 3, 8)" + " in order", thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (be (List("he", "hi", "hello")) and newContain newInOrder ("HI", "HELLO"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (be (List("HI", "HELLO")) and newContain newInOrder ("HI", "HELLO"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(hiLists(0)) + " was not equal to " + decorateToStringValue(List("HI", "HELLO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (hiLists) should (be (List("he", "hi", "hello")) and newContain newInOrder ("HELLO", "HI"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(hiLists(0)) + " was equal to " + decorateToStringValue(List("he", "hi", "hello")) + ", but " + decorateToStringValue(hiLists(0)) + " did not contain all of " + "(\"HELLO\", \"HI\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (be (List("he", "hi", "hello")) and newContain newInOrder ("HI", "HELLO"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (be (List("HI", "HELLO")) and newContain newInOrder ("HI", "HELLO"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(hiLists(0)) + " was not equal to " + decorateToStringValue(List("HI", "HELLO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (be (List("he", "hi", "hello")) and newContain newInOrder ("HELLO", "HI"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(hiLists(0)) + " was equal to " + decorateToStringValue(List("he", "hi", "hello")) + ", but " + decorateToStringValue(hiLists(0)) + " did not contain all of " + "(\"HELLO\", \"HI\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
    }

    object `when used with (not contain inOrder xx and not contain inOrder xx)` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (not newContain newInOrder (3, 2, 8) and not newContain newInOrder (8, 3, 4))
        atLeast (2, lists) should (not newContain newInOrder (3, 8, 5) and not newContain newInOrder (8, 3, 4))
        atMost (2, lists) should (not newContain newInOrder (2, 4, 3) and newContain newInOrder (2, 3, 4))
        no (list1s) should (not newContain newInOrder (1, 2, 3) and not newContain newInOrder (1, 2, 3))
        
        val e1 = intercept[TestFailedException] {
          all (lists) should (not newContain newInOrder (2, 3, 4) and not newContain newInOrder (8, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(lists(2)) + " contained all of " + "(2, 3, 4)" + " in order", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (lists) should (not newContain newInOrder (3, 6, 8) and not newContain newInOrder (2, 3, 4))
        }
        checkMessageStackDepth(e2, allErrMsg(2, decorateToStringValue(lists(2)) + " did not contain all of " + "(3, 6, 8)" + " in order" + ", but " + decorateToStringValue(lists(2)) + " contained all of " + "(2, 3, 4)" + " in order", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
        
        val e3 = intercept[TestFailedException] {
          all (hiLists) should (not newContain newInOrder ("hi", "hello") and not newContain newInOrder ("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e3, allErrMsg(0, decorateToStringValue(hiLists(0)) + " contained all of " + "(\"hi\", \"hello\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e4 = intercept[TestFailedException] {
          all (hiLists) should (not newContain newInOrder ("ho", "hey", "howdy") and not newContain newInOrder ("hi", "hello"))
        }
        checkMessageStackDepth(e4, allErrMsg(0, decorateToStringValue(hiLists(0)) + " did not contain all of " + "(\"ho\", \"hey\", \"howdy\")" + " in order" + ", but " + decorateToStringValue(hiLists(0)) + " contained all of " + "(\"hi\", \"hello\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (not newContain newInOrder ("HELLO", "HI") and not newContain newInOrder ("HELLO", "HO"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not newContain newInOrder ("HI", "HELLO") and not newContain newInOrder ("HO"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(hiLists(0)) + " contained all of " + "(\"HI\", \"HELLO\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (hiLists) should (not newContain newInOrder ("HELLO", "HI") and not newContain newInOrder ("HI", "HELLO"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(hiLists(0)) + " did not contain all of " + "(\"HELLO\", \"HI\")" + " in order" + ", but " + decorateToStringValue(hiLists(0)) + " contained all of " + "(\"HI\", \"HELLO\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (not newContain newInOrder ("HELLO", "HI") and not newContain newInOrder ("HELLO", "HO"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not newContain newInOrder ("HI", "HELLO") and not newContain newInOrder ("HO"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(hiLists(0)) + " contained all of " + "(\"HI\", \"HELLO\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (not newContain newInOrder ("HELLO", "HI") and not newContain newInOrder ("HI", "HELLO"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(hiLists(0)) + " did not contain all of " + "(\"HELLO\", \"HI\")" + " in order" + ", but " + decorateToStringValue(hiLists(0)) + " contained all of " + "(\"HI\", \"HELLO\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
    }
    
    object `when used with (not be xx and not contain inOrder xx)` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (not be (List(2)) and not newContain newInOrder (8, 3, 4))
        atLeast (2, lists) should (not be (List(3)) and not newContain newInOrder (8, 3, 4))
        atMost (2, lists) should (not be (List(8, 2, 3, 4)) and not newContain newInOrder (2, 3, 4))
        no (list1s) should (not be (List(0, 1, 2, 2, 3)) and not newContain newInOrder (1, 2, 3))
        
        val e1 = intercept[TestFailedException] {
          all (lists) should (not be (List(8, 2, 3, 4)) and not newContain newInOrder (8, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(lists(2)) + " was equal to " + decorateToStringValue(List(8, 2, 3, 4)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (lists) should (not be (List(3)) and not newContain newInOrder (2, 3, 4))
        }
        checkMessageStackDepth(e2, allErrMsg(2, decorateToStringValue(lists(2)) + " was not equal to " + decorateToStringValue(List(3)) + ", but " + decorateToStringValue(lists(2)) + " contained all of " + "(2, 3, 4)" + " in order", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
        
        val e3 = intercept[TestFailedException] {
          all (hiLists) should (not be (List("he", "hi", "hello")) and not newContain newInOrder ("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e3, allErrMsg(0, decorateToStringValue(hiLists(0)) + " was equal to " + decorateToStringValue(List("he", "hi", "hello")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e4 = intercept[TestFailedException] {
          all (hiLists) should (not be (List("ho")) and not newContain newInOrder ("hi", "hello"))
        }
        checkMessageStackDepth(e4, allErrMsg(0, decorateToStringValue(hiLists(0)) + " was not equal to " + decorateToStringValue(List("ho")) + ", but " + decorateToStringValue(hiLists(0)) + " contained all of " + "(\"hi\", \"hello\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (not be (List("ho")) and not newContain newInOrder ("HO", "HELLO"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not be (List("he", "hi", "hello")) and not newContain newInOrder ("HELLO", "HI"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(hiLists(0)) + " was equal to " + decorateToStringValue(List("he", "hi", "hello")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (hiLists) should (not be (List("ho")) and not newContain newInOrder ("HI", "HELLO"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(hiLists(0)) + " was not equal to " + decorateToStringValue(List("ho")) + ", but " + decorateToStringValue(hiLists(0)) + " contained all of " + "(\"HI\", \"HELLO\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (not be (List("ho")) and not newContain newInOrder ("HO", "HELLO"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not be (List("he", "hi", "hello")) and not newContain newInOrder ("HELLO", "HI"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(hiLists(0)) + " was equal to " + decorateToStringValue(List("he", "hi", "hello")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (not be (List("ho")) and not newContain newInOrder ("HI", "HELLO"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(hiLists(0)) + " was not equal to " + decorateToStringValue(List("ho")) + ", but " + decorateToStringValue(hiLists(0)) + " contained all of " + "(\"HI\", \"HELLO\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
    }
  }
}
