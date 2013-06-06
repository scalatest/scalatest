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

class ListShouldContainInOrderLogicalOrSpec extends Spec with Matchers {
  
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
      case _ => value
    }
  
  val upperCaseStringEquality =
    new Equality[String] {
      def areEqual(a: String, b: Any): Boolean = upperCase(a) == upperCase(b)
    }
  
  //ADDITIONAL//
  
  val fileName: String = "ListShouldContainInOrderLogicalOrSpec.scala"
  
  object `a List` {
    
    val fumList: List[String] = List("fex", "fum", "fum", "foe", "fie", "fee", "fee", "fee")
    val toList: List[String] = List("too", "you", "to", "to", "birthday", "birthday", "happy")
    
    object `when used with (contain inOrder xx or contain inOrder xx)` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (newContain newInOrder ("fum", "foe", "fie", "fee") or newContain newInOrder ("fum", "foe", "fie", "fee"))
        fumList should (newContain newInOrder ("fee", "fie", "foe", "fum") or newContain newInOrder ("fum", "foe", "fie", "fee"))
        fumList should (newContain newInOrder ("fum", "foe", "fie", "fee") or newContain newInOrder ("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (newContain newInOrder ("fee", "fie", "foe", "fum") or newContain newInOrder ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, Resources("didNotContainAllOfElementsInOrder", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\"") + ", and " + Resources("didNotContainAllOfElementsInOrder", decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (newContain newInOrder ("FUM", "FOE", "FIE", "FEE") or newContain newInOrder ("FUM", "FOE", "FIE", "FEE"))
        fumList should (newContain newInOrder ("FEE", "FIE", "FOE", "FUM") or newContain newInOrder ("FUM", "FOE", "FIE", "FEE"))
        fumList should (newContain newInOrder ("FUM", "FOE", "FIE", "FEE") or newContain newInOrder ("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (newContain newInOrder ("FEE", "FIE", "FOE", "FUM") or (newContain newInOrder ("FIE", "FEE", "FAM", "FOE")))
        }
        checkMessageStackDepth(e1, Resources("didNotContainAllOfElementsInOrder", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\"") + ", and " + Resources("didNotContainAllOfElementsInOrder", decorateToStringValue(fumList), "\"FIE\", \"FEE\", \"FAM\", \"FOE\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (newContain newInOrder ("FUM", "FOE", "FIE", "FEE") or newContain newInOrder ("FUM", "FOE", "FIE", "FEE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (newContain newInOrder ("FEE", "FIE", "FOE", "FUM") or newContain newInOrder ("FUM", "FOE", "FIE", "FEE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (newContain newInOrder ("FUM", "FOE", "FIE", "FEE") or newContain newInOrder ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (newContain newInOrder ("FEE", "FIE", "FOE", "FUM") or newContain newInOrder ("FIE", "FEE", "FAM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotContainAllOfElementsInOrder", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\"") + ", and " + Resources("didNotContainAllOfElementsInOrder", decorateToStringValue(fumList), "\"FIE\", \"FEE\", \"FAM\", \"FOE\""), fileName, thisLineNumber - 2)
        (fumList should (newContain newInOrder (" FUM ", " FOE ", " FIE ", " FEE ") or newContain newInOrder (" FUM ", " FOE ", " FIE ", " FEE "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
    }
    
    object `when used with (equal xx and contain inOrder xx)` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (equal (fumList) or newContain newInOrder ("fum", "foe", "fie", "fee"))
        fumList should (equal (toList) or newContain newInOrder ("fum", "foe", "fie", "fee"))
        fumList should (equal (fumList) or newContain newInOrder ("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) or newContain newInOrder ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("didNotContainAllOfElementsInOrder", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (equal (fumList) or newContain newInOrder ("FUM", "FOE", "FIE", "FEE"))
        fumList should (equal (toList) or newContain newInOrder ("FUM", "FOE", "FIE", "FEE"))
        fumList should (equal (fumList) or newContain newInOrder ("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) or (newContain newInOrder ("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("didNotContainAllOfElementsInOrder", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (equal (toList) or newContain newInOrder ("FUM", "FOE", "FIE", "FEE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (equal (fumList) or newContain newInOrder ("FUM", "FOE", "FIE", "FEE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (equal (toList) or newContain newInOrder ("FEE", "FIE", "FOE", "FUM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (equal (fumList) or newContain newInOrder ("FEE", "FIE", "FOE", "FUM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("didNotContainAllOfElementsInOrder", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
        (fumList should (equal (toList) or newContain newInOrder (" FEE ", " FIE ", " FOE ", " FUM "))) (decided by invertedListOfStringEquality, after being lowerCased and trimmed)
      }
    }
    
    object `when used with (legacyEqual xx and contain inOrder xx)` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (legacyEqual (fumList) or newContain newInOrder ("fum", "foe", "fie", "fee"))
        fumList should (legacyEqual (toList) or newContain newInOrder ("fum", "foe", "fie", "fee"))
        fumList should (legacyEqual (fumList) or newContain newInOrder ("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (legacyEqual (toList) or newContain newInOrder ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("didNotContainAllOfElementsInOrder", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (legacyEqual (fumList) or newContain newInOrder ("FUM", "FOE", "FIE", "FEE"))
        fumList should (legacyEqual (toList) or newContain newInOrder ("FUM", "FOE", "FIE", "FEE"))
        fumList should (legacyEqual (fumList) or newContain newInOrder ("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (legacyEqual (toList) or (newContain newInOrder ("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("didNotContainAllOfElementsInOrder", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (legacyEqual (fumList) or newContain newInOrder ("FUM", "FOE", "FIE", "FEE"))) (decided by upperCaseStringEquality)
        (fumList should (legacyEqual (toList) or newContain newInOrder ("FUM", "FOE", "FIE", "FEE"))) (decided by upperCaseStringEquality)
        (fumList should (legacyEqual (fumList) or newContain newInOrder ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (legacyEqual (toList) or newContain newInOrder ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("didNotContainAllOfElementsInOrder", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
        (fumList should (legacyEqual (fumList) or newContain newInOrder (" FUM ", " FOE ", " FIE ", " FEE "))) (after being lowerCased and trimmed)
      }
    }

    object `when used with (contain inOrder xx and legacyEqual xx)` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (newContain newInOrder ("fum", "foe", "fie", "fee") or legacyEqual (fumList))
        fumList should (newContain newInOrder ("fee", "fie", "foe", "fum") or legacyEqual (fumList))
        fumList should (newContain newInOrder ("fum", "foe", "fie", "fee") or legacyEqual (toList))
        val e1 = intercept[TestFailedException] {
          fumList should (newContain newInOrder ("fee", "fie", "foe", "fum") or legacyEqual (toList))
        }
        checkMessageStackDepth(e1, Resources("didNotContainAllOfElementsInOrder", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\"") + ", and " + Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (newContain newInOrder ("FUM", "FOE", "FIE", "FEE") or legacyEqual (fumList))
        fumList should (newContain newInOrder ("FEE", "FIE", "FOE", "FUM") or legacyEqual (fumList))
        fumList should (newContain newInOrder ("FUM", "FOE", "FIE", "FEE") or legacyEqual (toList))
        val e1 = intercept[TestFailedException] {
          fumList should (newContain newInOrder ("FEE", "FIE", "FOE", "FUM") or legacyEqual (toList))
        }
        checkMessageStackDepth(e1, Resources("didNotContainAllOfElementsInOrder", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\"") + ", and " + Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (newContain newInOrder ("FUM", "FOE", "FIE", "FEE") or legacyEqual (fumList))) (decided by upperCaseStringEquality)
        (fumList should (newContain newInOrder ("FEE", "FIE", "FOE", "FUM") or legacyEqual (fumList))) (decided by upperCaseStringEquality)
        (fumList should (newContain newInOrder ("FUM", "FOE", "FIE", "FEE") or legacyEqual (toList))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (newContain newInOrder ("FEE", "FIE", "FOE", "FUM") or legacyEqual (toList))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotContainAllOfElementsInOrder", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\"") + ", and " + Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        (fumList should (newContain newInOrder (" FUM ", " FOE ", " FIE ", " FEE ") or legacyEqual (fumList))) (after being lowerCased and trimmed)
      }
    }
    
    object `when used with (not contain inOrder xx and not contain inOrder xx)` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (not newContain newInOrder ("fee", "fie", "foe", "fum") or not newContain newInOrder ("fee", "fie", "foe", "fum"))
        fumList should (not newContain newInOrder ("fum", "foe", "fie", "fee") or not newContain newInOrder ("fee", "fie", "foe", "fum"))
        fumList should (not newContain newInOrder ("fee", "fie", "foe", "fum") or not newContain newInOrder ("fum", "foe", "fie", "fee"))
        val e1 = intercept[TestFailedException] {
          fumList should (not newContain newInOrder ("fum", "foe", "fie", "fee") or not newContain newInOrder ("fum", "foe", "fie", "fee"))
        }
        checkMessageStackDepth(e1, Resources("containedAllOfElementsInOrder", decorateToStringValue(fumList), "\"fum\", \"foe\", \"fie\", \"fee\"") + ", and " + Resources("containedAllOfElementsInOrder", decorateToStringValue(fumList), "\"fum\", \"foe\", \"fie\", \"fee\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (not newContain newInOrder ("FEE", "FIE", "FOE", "FUM") or not newContain newInOrder ("FEE", "FIE", "FOE", "FUM"))
        fumList should (not newContain newInOrder ("FUM", "FOE", "FIE", "FEE") or not newContain newInOrder ("FEE", "FIE", "FOE", "FUM"))
        fumList should (not newContain newInOrder ("FEE", "FIE", "FOE", "FUM") or not newContain newInOrder ("FUM", "FOE", "FIE", "FEE"))
        val e1 = intercept[TestFailedException] {
          fumList should (not newContain newInOrder ("FUM", "FOE", "FIE", "FEE") or not newContain newInOrder ("FUM", "FOE", "FIE", "FEE"))
        }
        checkMessageStackDepth(e1, Resources("containedAllOfElementsInOrder", decorateToStringValue(fumList), "\"FUM\", \"FOE\", \"FIE\", \"FEE\"") + ", and " + Resources("containedAllOfElementsInOrder", decorateToStringValue(fumList), "\"FUM\", \"FOE\", \"FIE\", \"FEE\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (not newContain newInOrder ("FEE", "FIE", "FOE", "FUM") or not newContain newInOrder ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (not newContain newInOrder ("FUM", "FOE", "FIE", "FEE") or not newContain newInOrder ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (not newContain newInOrder ("FEE", "FIE", "FOE", "FUM") or not newContain newInOrder ("FUM", "FOE", "FIE", "FEE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not newContain newInOrder ("FUM", "FOE", "FIE", "FEE") or not newContain newInOrder ("FUM", "FOE", "FIE", "FEE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("containedAllOfElementsInOrder", decorateToStringValue(fumList), "\"FUM\", \"FOE\", \"FIE\", \"FEE\"") + ", and " + Resources("containedAllOfElementsInOrder", decorateToStringValue(fumList), "\"FUM\", \"FOE\", \"FIE\", \"FEE\""), fileName, thisLineNumber - 2)
      }
    }
    
    object `when used with (not equal xx and not contain inOrder xx)` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (not equal (toList) or not newContain newInOrder ("fee", "fie", "foe", "fum"))
        fumList should (not equal (fumList) or not newContain newInOrder ("fee", "fie", "foe", "fum"))
        fumList should (not equal (toList) or not newContain newInOrder ("fum", "foe", "fie", "fee"))
        val e1 = intercept[TestFailedException] {
          fumList should (not equal (fumList) or not newContain newInOrder ("fum", "foe", "fie", "fee"))
        }
        checkMessageStackDepth(e1, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("containedAllOfElementsInOrder", decorateToStringValue(fumList), "\"fum\", \"foe\", \"fie\", \"fee\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (not equal (toList) or not newContain newInOrder ("FEE", "FIE", "FOE", "FUM"))
        fumList should (not equal (fumList) or not newContain newInOrder ("FEE", "FIE", "FOE", "FUM"))
        fumList should (not equal (toList) or not newContain newInOrder ("FUM", "FOE", "FIE", "FEE"))
        val e2 = intercept[TestFailedException] {
          fumList should (not equal (fumList) or (not newContain newInOrder ("FUM", "FOE", "FIE", "FEE")))
        }
        checkMessageStackDepth(e2, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("containedAllOfElementsInOrder", decorateToStringValue(fumList), "\"FUM\", \"FOE\", \"FIE\", \"FEE\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (not equal (fumList) or not newContain newInOrder ("FEE", "FIE", "FOE", "FUM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (not equal (toList) or not newContain newInOrder ("FEE", "FIE", "FOE", "FUM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (not equal (fumList) or not newContain newInOrder ("FUM", "FOE", "FIE", "FEE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not equal (toList) or not newContain newInOrder ("FUM", "FOE", "FIE", "FEE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("containedAllOfElementsInOrder", decorateToStringValue(fumList), "\"FUM\", \"FOE\", \"FIE\", \"FEE\""), fileName, thisLineNumber - 2)
      }
    }
    
    object `when used with (not be xx and not contain inOrder xx)` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (not be (toList) or not newContain newInOrder ("fee", "fie", "foe", "fum"))
        fumList should (not be (fumList) or not newContain newInOrder ("fee", "fie", "foe", "fum"))
        fumList should (not be (toList) or not newContain newInOrder ("fum", "foe", "fie", "fee"))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) or not newContain newInOrder ("fum", "foe", "fie", "fee"))
        }
        checkMessageStackDepth(e1, Resources("wasEqualTo", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("containedAllOfElementsInOrder", decorateToStringValue(fumList), "\"fum\", \"foe\", \"fie\", \"fee\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (not be (toList) or not newContain newInOrder ("FEE", "FIE", "FOE", "FUM"))
        fumList should (not be (fumList) or not newContain newInOrder ("FEE", "FIE", "FOE", "FUM"))
        fumList should (not be (toList) or not newContain newInOrder ("FUM", "FOE", "FIE", "FEE"))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) or (not newContain newInOrder ("FUM", "FOE", "FIE", "FEE")))
        }
        checkMessageStackDepth(e1, Resources("wasEqualTo", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("containedAllOfElementsInOrder", decorateToStringValue(fumList), "\"FUM\", \"FOE\", \"FIE\", \"FEE\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (not be (toList) or not newContain newInOrder ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        (fumList should (not be (fumList) or not newContain newInOrder ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        (fumList should (not be (toList) or not newContain newInOrder ("FUM", "FOE", "FIE", "FEE"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not be (fumList) or not newContain newInOrder ("FUM", "FOE", "FIE", "FEE"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("wasEqualTo", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("containedAllOfElementsInOrder", decorateToStringValue(fumList), "\"FUM\", \"FOE\", \"FIE\", \"FEE\""), fileName, thisLineNumber - 2)
        (fumList should (not newContain newInOrder (Set(" FEE ", " FIE ", " FOE ", " FUU ")) or not newContain newInOrder (Set(" FEE ", " FIE ", " FOE ", " FUU ")))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
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
      "in " + left
    
    object `when used with (contain inOrder xx and contain inOrder xx)` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (newContain newInOrder (1, 2, 3) or newContain newInOrder (1, 2, 3))
        all (list1s) should (newContain newInOrder (3, 2, 5) or newContain newInOrder (1, 2, 3))
        all (list1s) should (newContain newInOrder (1, 2, 3) or newContain newInOrder (2, 3, 4))
        
        atLeast (2, lists) should (newContain newInOrder (1, 2, 3) or newContain newInOrder (1, 2, 3))
        atLeast (2, lists) should (newContain newInOrder (3, 6, 5) or newContain newInOrder (1, 2, 3))
        atLeast (2, lists) should (newContain newInOrder (1, 2, 3) or newContain newInOrder (8, 3, 4))
        
        val e1 = intercept[TestFailedException] {
          all (lists) should (newContain newInOrder (1, 2, 3) or newContain newInOrder (1, 2, 3))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(lists(2)) + " did not contain all of " + "(1, 2, 3)" + " in order" + ", and " + decorateToStringValue(lists(2)) + " did not contain all of " + "(1, 2, 3)" + " in order", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (newContain newInOrder ("HI", "HELLO") or newContain newInOrder ("hi", "hello"))
        all (hiLists) should (newContain newInOrder ("HELLO", "HO") or newContain newInOrder ("hi", "hello"))
        all (hiLists) should (newContain newInOrder ("HI", "HELLO") or newContain newInOrder ("hello", "ho"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (newContain newInOrder ("HELLO", "HO") or newContain newInOrder ("hello", "ho"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(hiLists(0)) + " did not contain all of " + "(\"HELLO\", \"HO\")" + " in order" + ", and " + decorateToStringValue(hiLists(0)) + " did not contain all of " + "(\"hello\", \"ho\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (newContain newInOrder ("HI", "HELLO") or newContain newInOrder ("hi", "hello"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (newContain newInOrder ("HELLO", "HO") or newContain newInOrder ("hi", "hello"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (newContain newInOrder ("HI", "HELLO") or newContain newInOrder ("hello", "ho"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)

        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (newContain newInOrder ("HELLO", "HO") or newContain newInOrder ("hello", "ho"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(hiLists(0)) + " did not contain all of " + "(\"HELLO\", \"HO\")" + " in order" + ", and " + decorateToStringValue(hiLists(0)) + " did not contain all of " + "(\"hello\", \"ho\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
    }

    object `when used with (be xx and contain inOrder xx)` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (be (List(0, 1, 2, 2, 3)) or newContain newInOrder (1, 2, 3))
        all (list1s) should (be (List(8, 2, 3, 4)) or newContain newInOrder (1, 2, 3))
        all (list1s) should (be (List(0, 1, 2, 2, 3)) or newContain newInOrder (2, 3, 4))
        
        val e1 = intercept[TestFailedException] {
          all (list1s) should (be (List(8, 2, 3, 4)) or newContain newInOrder (2, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(list1s(0)) + " was not equal to " + decorateToStringValue(List(8, 2, 3, 4)) + ", and " + decorateToStringValue(list1s(0)) + " did not contain all of " + "(2, 3, 4)" + " in order", thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (be (List("he", "hi", "hello")) or newContain newInOrder ("HI", "HELLO"))
        all (hiLists) should (be (List("ho", "hello")) or newContain newInOrder ("HI", "HELLO"))
        all (hiLists) should (be (List("he", "hi", "hello")) or newContain newInOrder ("HELLO", "HI"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (be (List("ho", "hello")) or newContain newInOrder ("HELLO", "HI"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(hiLists(0)) + " was not equal to " + decorateToStringValue(List("ho", "hello")) + ", and " + decorateToStringValue(hiLists(0)) + " did not contain all of " + "(\"HELLO\", \"HI\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (be (List("he", "hi", "hello")) or newContain newInOrder ("HI", "HELLO"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (be (List("ho", "hello")) or newContain newInOrder ("HI", "HELLO"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (be (List("he", "hi", "hello")) or newContain newInOrder ("HELLO", "HI"))) (decided by upperCaseStringEquality)
        
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (be (List("ho", "hello")) or newContain newInOrder ("HELLO", "HI"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(hiLists(0)) + " was not equal to " + decorateToStringValue(List("ho", "hello")) + ", and " + decorateToStringValue(hiLists(0)) + " did not contain all of " + "(\"HELLO\", \"HI\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
    }
    
    object `when used with (not contain inOrder xx and not contain inOrder xx)` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (not newContain newInOrder (3, 2, 8) or not newContain newInOrder (8, 3, 4))
        all (list1s) should (not newContain newInOrder (1, 2, 3) or not newContain newInOrder (8, 3, 4))
        all (list1s) should (not newContain newInOrder (3, 2, 8) or not newContain newInOrder (1, 2, 3))
        
        val e1 = intercept[TestFailedException] {
          all (lists) should (not newContain newInOrder (2, 3, 4) or not newContain newInOrder (2, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(lists(2)) + " contained all of " + "(2, 3, 4)" + " in order" + ", and " + decorateToStringValue(lists(2)) + " contained all of " + "(2, 3, 4)" + " in order", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (not newContain newInOrder ("HELLO", "HI") or not newContain newInOrder ("hello", "hi"))
        all (hiLists) should (not newContain newInOrder ("HI", "HELLO") or not newContain newInOrder ("hello", "hi"))
        all (hiLists) should (not newContain newInOrder ("HELLO", "HI") or not newContain newInOrder ("hi", "hello"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not newContain newInOrder ("HI", "HELLO") or not newContain newInOrder ("hi", "hello"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(hiLists(0)) + " contained all of " + "(\"HI\", \"HELLO\")" + " in order" + ", and " + decorateToStringValue(hiLists(0)) + " contained all of " + "(\"hi\", \"hello\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (not newContain newInOrder ("HELLO", "HI") or not newContain newInOrder ("hello", "hi"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (not newContain newInOrder ("HI", "HELLO") or not newContain newInOrder ("hello", "hi"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (not newContain newInOrder ("HELLO", "HI") or not newContain newInOrder ("hi", "hello"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not newContain newInOrder ("HI", "HELLO") or not newContain newInOrder ("hi", "hello"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(hiLists(0)) + " contained all of " + "(\"HI\", \"HELLO\")" + " in order" + ", and " + decorateToStringValue(hiLists(0)) + " contained all of " + "(\"hi\", \"hello\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
    }
    
    object `when used with (not be xx and not contain inOrder xx)` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (not be (List(2)) or not newContain newInOrder (8, 3, 4))
        all (list1s) should (not be (List(0, 1, 2, 2, 3)) or not newContain newInOrder (8, 3, 4))
        all (list1s) should (not be (List(2)) or not newContain newInOrder (1, 2, 3))
        
        val e1 = intercept[TestFailedException] {
          all (list1s) should (not be (List(0, 1, 2, 2, 3)) or not newContain newInOrder (1, 2, 3))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(list1s(0)) + " was equal to " + decorateToStringValue(List(0, 1, 2, 2, 3)) + ", and " + decorateToStringValue(list1s(0)) + " contained all of " + "(1, 2, 3)" + " in order", thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (not be (List("hello", "ho")) or not newContain newInOrder ("HELLO", "HO"))
        all (hiLists) should (not be (List("he", "hi", "hello")) or not newContain newInOrder ("HELLO", "HO"))
        all (hiLists) should (not be (List("hello", "ho")) or not newContain newInOrder ("HI", "HELLO"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not be (List("he", "hi", "hello")) or not newContain newInOrder ("HI", "HELLO"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(hiLists(0)) + " was equal to " + decorateToStringValue(List("he", "hi", "hello")) + ", and " + decorateToStringValue(hiLists(0)) + " contained all of " + "(\"HI\", \"HELLO\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (not be (List("hello", "ho")) or not newContain newInOrder ("HELLO", "HO"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (not be (List("he", "hi", "hello")) or not newContain newInOrder ("HELLO", "HO"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (not be (List("hello", "ho")) or not newContain newInOrder ("HI", "HELLO"))) (decided by upperCaseStringEquality)

        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not be (List("he", "hi", "hello")) or not newContain newInOrder ("HI", "HELLO"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(hiLists(0)) + " was equal to " + decorateToStringValue(List("he", "hi", "hello")) + ", and " + decorateToStringValue(hiLists(0)) + " contained all of " + "(\"HI\", \"HELLO\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
    }
  }
}
