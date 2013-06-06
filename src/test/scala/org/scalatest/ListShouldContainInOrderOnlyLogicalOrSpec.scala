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

class ListShouldContainInOrderOnlyLogicalOrSpec extends Spec with Matchers {
  
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
  
  val fileName: String = "ListShouldContainInOrderOnlyLogicalOrSpec.scala"
  
  object `a List` {
    
    val fumList: List[String] = List("fum", "foe", "fie", "fee")
    val toList: List[String] = List("you", "to", "birthday", "happy")
    
    object `when used with (contain inOrderOnly xx or contain inOrderOnly xx)` {

      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (newContain newInOrderOnly ("fum", "foe", "fie", "fee") or newContain newInOrderOnly ("fum", "foe", "fie", "fee"))
        fumList should (newContain newInOrderOnly ("fee", "fie", "foe", "fum") or newContain newInOrderOnly ("fum", "foe", "fie", "fee"))
        fumList should (newContain newInOrderOnly ("fum", "foe", "fie", "fee") or newContain newInOrderOnly ("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (newContain newInOrderOnly ("fee", "fie", "foe", "fum") or newContain newInOrderOnly ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, Resources("didNotContainInOrderOnlyElements", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\"") + ", and " + Resources("didNotContainInOrderOnlyElements", decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (newContain newInOrderOnly ("FUM", "FOE", "FIE", "FEE") or newContain newInOrderOnly ("FUM", "FOE", "FIE", "FEE"))
        fumList should (newContain newInOrderOnly ("FEE", "FIE", "FOE", "FUM") or newContain newInOrderOnly ("FUM", "FOE", "FIE", "FEE"))
        fumList should (newContain newInOrderOnly ("FUM", "FOE", "FIE", "FEE") or newContain newInOrderOnly ("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (newContain newInOrderOnly ("FEE", "FIE", "FOE", "FUM") or (newContain newInOrderOnly ("FIE", "FEE", "FAM", "FOE")))
        }
        checkMessageStackDepth(e1, Resources("didNotContainInOrderOnlyElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\"") + ", and " + Resources("didNotContainInOrderOnlyElements", decorateToStringValue(fumList), "\"FIE\", \"FEE\", \"FAM\", \"FOE\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (newContain newInOrderOnly ("FUM", "FOE", "FIE", "FEE") or newContain newInOrderOnly ("FUM", "FOE", "FIE", "FEE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (newContain newInOrderOnly ("FEE", "FIE", "FOE", "FUM") or newContain newInOrderOnly ("FUM", "FOE", "FIE", "FEE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (newContain newInOrderOnly ("FUM", "FOE", "FIE", "FEE") or newContain newInOrderOnly ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (newContain newInOrderOnly ("FEE", "FIE", "FOE", "FUM") or newContain newInOrderOnly ("FIE", "FEE", "FAM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotContainInOrderOnlyElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\"") + ", and " + Resources("didNotContainInOrderOnlyElements", decorateToStringValue(fumList), "\"FIE\", \"FEE\", \"FAM\", \"FOE\""), fileName, thisLineNumber - 2)
        (fumList should (newContain newInOrderOnly (" FUM ", " FOE ", " FIE ", " FEE ") or newContain newInOrderOnly (" FUM ", " FOE ", " FIE ", " FEE "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
    }
    
    object `when used with (equal xx and contain inOrderOnly xx)` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (equal (fumList) or newContain newInOrderOnly ("fum", "foe", "fie", "fee"))
        fumList should (equal (toList) or newContain newInOrderOnly ("fum", "foe", "fie", "fee"))
        fumList should (equal (fumList) or newContain newInOrderOnly ("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) or newContain newInOrderOnly ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("didNotContainInOrderOnlyElements", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (equal (fumList) or newContain newInOrderOnly ("FUM", "FOE", "FIE", "FEE"))
        fumList should (equal (toList) or newContain newInOrderOnly ("FUM", "FOE", "FIE", "FEE"))
        fumList should (equal (fumList) or newContain newInOrderOnly ("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) or (newContain newInOrderOnly ("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("didNotContainInOrderOnlyElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (equal (toList) or newContain newInOrderOnly ("FUM", "FOE", "FIE", "FEE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (equal (fumList) or newContain newInOrderOnly ("FUM", "FOE", "FIE", "FEE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (equal (toList) or newContain newInOrderOnly ("FEE", "FIE", "FOE", "FUM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (equal (fumList) or newContain newInOrderOnly ("FEE", "FIE", "FOE", "FUM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("didNotContainInOrderOnlyElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
        (fumList should (equal (toList) or newContain newInOrderOnly (" FEE ", " FIE ", " FOE ", " FUM "))) (decided by invertedListOfStringEquality, after being lowerCased and trimmed)
      }
    }
    
    object `when used with (legacyEqual xx and contain inOrderOnly xx)` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (legacyEqual (fumList) or newContain newInOrderOnly ("fum", "foe", "fie", "fee"))
        fumList should (legacyEqual (toList) or newContain newInOrderOnly ("fum", "foe", "fie", "fee"))
        fumList should (legacyEqual (fumList) or newContain newInOrderOnly ("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (legacyEqual (toList) or newContain newInOrderOnly ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("didNotContainInOrderOnlyElements", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (legacyEqual (fumList) or newContain newInOrderOnly ("FUM", "FOE", "FIE", "FEE"))
        fumList should (legacyEqual (toList) or newContain newInOrderOnly ("FUM", "FOE", "FIE", "FEE"))
        fumList should (legacyEqual (fumList) or newContain newInOrderOnly ("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (legacyEqual (toList) or (newContain newInOrderOnly ("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("didNotContainInOrderOnlyElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (legacyEqual (fumList) or newContain newInOrderOnly ("FUM", "FOE", "FIE", "FEE"))) (decided by upperCaseStringEquality)
        (fumList should (legacyEqual (toList) or newContain newInOrderOnly ("FUM", "FOE", "FIE", "FEE"))) (decided by upperCaseStringEquality)
        (fumList should (legacyEqual (fumList) or newContain newInOrderOnly ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (legacyEqual (toList) or newContain newInOrderOnly ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("didNotContainInOrderOnlyElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
        (fumList should (legacyEqual (fumList) or newContain newInOrderOnly (" FUM ", " FOE ", " FIE ", " FEE "))) (after being lowerCased and trimmed)
      }
    }

    object `when used with (contain inOrderOnly xx and legacyEqual xx)` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (newContain newInOrderOnly ("fum", "foe", "fie", "fee") or legacyEqual (fumList))
        fumList should (newContain newInOrderOnly ("fee", "fie", "foe", "fum") or legacyEqual (fumList))
        fumList should (newContain newInOrderOnly ("fum", "foe", "fie", "fee") or legacyEqual (toList))
        val e1 = intercept[TestFailedException] {
          fumList should (newContain newInOrderOnly ("fee", "fie", "foe", "fum") or legacyEqual (toList))
        }
        checkMessageStackDepth(e1, Resources("didNotContainInOrderOnlyElements", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\"") + ", and " + Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (newContain newInOrderOnly ("FUM", "FOE", "FIE", "FEE") or legacyEqual (fumList))
        fumList should (newContain newInOrderOnly ("FEE", "FIE", "FOE", "FUM") or legacyEqual (fumList))
        fumList should (newContain newInOrderOnly ("FUM", "FOE", "FIE", "FEE") or legacyEqual (toList))
        val e1 = intercept[TestFailedException] {
          fumList should (newContain newInOrderOnly ("FEE", "FIE", "FOE", "FUM") or legacyEqual (toList))
        }
        checkMessageStackDepth(e1, Resources("didNotContainInOrderOnlyElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\"") + ", and " + Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (newContain newInOrderOnly ("FUM", "FOE", "FIE", "FEE") or legacyEqual (fumList))) (decided by upperCaseStringEquality)
        (fumList should (newContain newInOrderOnly ("FEE", "FIE", "FOE", "FUM") or legacyEqual (fumList))) (decided by upperCaseStringEquality)
        (fumList should (newContain newInOrderOnly ("FUM", "FOE", "FIE", "FEE") or legacyEqual (toList))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (newContain newInOrderOnly ("FEE", "FIE", "FOE", "FUM") or legacyEqual (toList))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotContainInOrderOnlyElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\"") + ", and " + Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        (fumList should (newContain newInOrderOnly (" FUM ", " FOE ", " FIE ", " FEE ") or legacyEqual (fumList))) (after being lowerCased and trimmed)
      }
    }
    
    object `when used with (not contain inOrderOnly xx and not contain inOrderOnly xx)` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (not newContain newInOrderOnly ("fee", "fie", "foe", "fum") or not newContain newInOrderOnly ("fee", "fie", "foe", "fum"))
        fumList should (not newContain newInOrderOnly ("fum", "foe", "fie", "fee") or not newContain newInOrderOnly ("fee", "fie", "foe", "fum"))
        fumList should (not newContain newInOrderOnly ("fee", "fie", "foe", "fum") or not newContain newInOrderOnly ("fum", "foe", "fie", "fee"))
        val e1 = intercept[TestFailedException] {
          fumList should (not newContain newInOrderOnly ("fum", "foe", "fie", "fee") or not newContain newInOrderOnly ("fum", "foe", "fie", "fee"))
        }
        checkMessageStackDepth(e1, Resources("containedInOrderOnlyElements", decorateToStringValue(fumList), "\"fum\", \"foe\", \"fie\", \"fee\"") + ", and " + Resources("containedInOrderOnlyElements", decorateToStringValue(fumList), "\"fum\", \"foe\", \"fie\", \"fee\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (not newContain newInOrderOnly ("FEE", "FIE", "FOE", "FUM") or not newContain newInOrderOnly ("FEE", "FIE", "FOE", "FUM"))
        fumList should (not newContain newInOrderOnly ("FUM", "FOE", "FIE", "FEE") or not newContain newInOrderOnly ("FEE", "FIE", "FOE", "FUM"))
        fumList should (not newContain newInOrderOnly ("FEE", "FIE", "FOE", "FUM") or not newContain newInOrderOnly ("FUM", "FOE", "FIE", "FEE"))
        val e1 = intercept[TestFailedException] {
          fumList should (not newContain newInOrderOnly ("FUM", "FOE", "FIE", "FEE") or not newContain newInOrderOnly ("FUM", "FOE", "FIE", "FEE"))
        }
        checkMessageStackDepth(e1, Resources("containedInOrderOnlyElements", decorateToStringValue(fumList), "\"FUM\", \"FOE\", \"FIE\", \"FEE\"") + ", and " + Resources("containedInOrderOnlyElements", decorateToStringValue(fumList), "\"FUM\", \"FOE\", \"FIE\", \"FEE\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (not newContain newInOrderOnly ("FEE", "FIE", "FOE", "FUM") or not newContain newInOrderOnly ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (not newContain newInOrderOnly ("FUM", "FOE", "FIE", "FEE") or not newContain newInOrderOnly ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (not newContain newInOrderOnly ("FEE", "FIE", "FOE", "FUM") or not newContain newInOrderOnly ("FUM", "FOE", "FIE", "FEE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not newContain newInOrderOnly ("FUM", "FOE", "FIE", "FEE") or not newContain newInOrderOnly ("FUM", "FOE", "FIE", "FEE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("containedInOrderOnlyElements", decorateToStringValue(fumList), "\"FUM\", \"FOE\", \"FIE\", \"FEE\"") + ", and " + Resources("containedInOrderOnlyElements", decorateToStringValue(fumList), "\"FUM\", \"FOE\", \"FIE\", \"FEE\""), fileName, thisLineNumber - 2)
      }
    }
    
    object `when used with (not equal xx and not contain inOrderOnly xx)` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (not equal (toList) or not newContain newInOrderOnly ("fee", "fie", "foe", "fum"))
        fumList should (not equal (fumList) or not newContain newInOrderOnly ("fee", "fie", "foe", "fum"))
        fumList should (not equal (toList) or not newContain newInOrderOnly ("fum", "foe", "fie", "fee"))
        val e1 = intercept[TestFailedException] {
          fumList should (not equal (fumList) or not newContain newInOrderOnly ("fum", "foe", "fie", "fee"))
        }
        checkMessageStackDepth(e1, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("containedInOrderOnlyElements", decorateToStringValue(fumList), "\"fum\", \"foe\", \"fie\", \"fee\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (not equal (toList) or not newContain newInOrderOnly ("FEE", "FIE", "FOE", "FUM"))
        fumList should (not equal (fumList) or not newContain newInOrderOnly ("FEE", "FIE", "FOE", "FUM"))
        fumList should (not equal (toList) or not newContain newInOrderOnly ("FUM", "FOE", "FIE", "FEE"))
        val e2 = intercept[TestFailedException] {
          fumList should (not equal (fumList) or (not newContain newInOrderOnly ("FUM", "FOE", "FIE", "FEE")))
        }
        checkMessageStackDepth(e2, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("containedInOrderOnlyElements", decorateToStringValue(fumList), "\"FUM\", \"FOE\", \"FIE\", \"FEE\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (not equal (fumList) or not newContain newInOrderOnly ("FEE", "FIE", "FOE", "FUM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (not equal (toList) or not newContain newInOrderOnly ("FEE", "FIE", "FOE", "FUM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (not equal (fumList) or not newContain newInOrderOnly ("FUM", "FOE", "FIE", "FEE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not equal (toList) or not newContain newInOrderOnly ("FUM", "FOE", "FIE", "FEE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("containedInOrderOnlyElements", decorateToStringValue(fumList), "\"FUM\", \"FOE\", \"FIE\", \"FEE\""), fileName, thisLineNumber - 2)
      }
    }
    
    object `when used with (not be xx and not contain inOrderOnly xx)` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (not be (toList) or not newContain newInOrderOnly ("fee", "fie", "foe", "fum"))
        fumList should (not be (fumList) or not newContain newInOrderOnly ("fee", "fie", "foe", "fum"))
        fumList should (not be (toList) or not newContain newInOrderOnly ("fum", "foe", "fie", "fee"))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) or not newContain newInOrderOnly ("fum", "foe", "fie", "fee"))
        }
        checkMessageStackDepth(e1, Resources("wasEqualTo", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("containedInOrderOnlyElements", decorateToStringValue(fumList), "\"fum\", \"foe\", \"fie\", \"fee\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (not be (toList) or not newContain newInOrderOnly ("FEE", "FIE", "FOE", "FUM"))
        fumList should (not be (fumList) or not newContain newInOrderOnly ("FEE", "FIE", "FOE", "FUM"))
        fumList should (not be (toList) or not newContain newInOrderOnly ("FUM", "FOE", "FIE", "FEE"))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) or (not newContain newInOrderOnly ("FUM", "FOE", "FIE", "FEE")))
        }
        checkMessageStackDepth(e1, Resources("wasEqualTo", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("containedInOrderOnlyElements", decorateToStringValue(fumList), "\"FUM\", \"FOE\", \"FIE\", \"FEE\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (not be (toList) or not newContain newInOrderOnly ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        (fumList should (not be (fumList) or not newContain newInOrderOnly ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        (fumList should (not be (toList) or not newContain newInOrderOnly ("FUM", "FOE", "FIE", "FEE"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not be (fumList) or not newContain newInOrderOnly ("FUM", "FOE", "FIE", "FEE"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("wasEqualTo", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("containedInOrderOnlyElements", decorateToStringValue(fumList), "\"FUM\", \"FOE\", \"FIE\", \"FEE\""), fileName, thisLineNumber - 2)
        (fumList should (not newContain newInOrderOnly (Set(" FEE ", " FIE ", " FOE ", " FUU ")) or not newContain newInOrderOnly (Set(" FEE ", " FIE ", " FOE ", " FUU ")))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
    }
  }
  
  object `collection of Lists` {
    
    val list1s: Vector[List[Int]] = Vector(List(1, 2, 3), List(1, 2, 3), List(1, 2, 3))
    val lists: Vector[List[Int]] = Vector(List(1, 2, 3), List(1, 2, 3), List(2, 3, 4))
    val nils: Vector[List[Int]] = Vector(Nil, Nil, Nil)
    val listsNil: Vector[List[Int]] = Vector(List(1, 2, 3), List(1, 2, 3), Nil)
    val hiLists: Vector[List[String]] = Vector(List("hi", "hello"), List("hi", "hello"), List("hi", "hello"))
    val toLists: Vector[List[String]] = Vector(List("you", "to"), List("you", "to"), List("you", "to"))
    
    def allErrMsg(index: Int, message: String, lineNumber: Int, left: Any): String = 
      "'all' inspection failed, because: \n" +
      "  at index " + index + ", " + message + " (" + fileName + ":" + (lineNumber) + ") \n" +
      "in " + left
    
    object `when used with (contain inOrderOnly xx and contain inOrderOnly xx)` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (newContain newInOrderOnly (1, 2, 3) or newContain newInOrderOnly (1, 2, 3))
        all (list1s) should (newContain newInOrderOnly (3, 2, 5) or newContain newInOrderOnly (1, 2, 3))
        all (list1s) should (newContain newInOrderOnly (1, 2, 3) or newContain newInOrderOnly (2, 3, 4))
        
        atLeast (2, lists) should (newContain newInOrderOnly (1, 2, 3) or newContain newInOrderOnly (1, 2, 3))
        atLeast (2, lists) should (newContain newInOrderOnly (3, 6, 5) or newContain newInOrderOnly (1, 2, 3))
        atLeast (2, lists) should (newContain newInOrderOnly (1, 2, 3) or newContain newInOrderOnly (8, 3, 4))
        
        val e1 = intercept[TestFailedException] {
          all (lists) should (newContain newInOrderOnly (1, 2, 3) or newContain newInOrderOnly (1, 2, 3))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(List(2, 3, 4)) + " did not contain only " + "(1, 2, 3)" + " in order" + ", and " + decorateToStringValue(List(2, 3, 4)) + " did not contain only " + "(1, 2, 3)" + " in order", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (newContain newInOrderOnly ("HI", "HELLO") or newContain newInOrderOnly ("hi", "hello"))
        all (hiLists) should (newContain newInOrderOnly ("HELLO", "HO") or newContain newInOrderOnly ("hi", "hello"))
        all (hiLists) should (newContain newInOrderOnly ("HI", "HELLO") or newContain newInOrderOnly ("hello", "ho"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (newContain newInOrderOnly ("HELLO", "HO") or newContain newInOrderOnly ("hello", "ho"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi", "hello")) + " did not contain only " + "(\"HELLO\", \"HO\")" + " in order" + ", and " + decorateToStringValue(List("hi", "hello")) + " did not contain only " + "(\"hello\", \"ho\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (newContain newInOrderOnly ("HI", "HELLO") or newContain newInOrderOnly ("hi", "hello"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (newContain newInOrderOnly ("HELLO", "HO") or newContain newInOrderOnly ("hi", "hello"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (newContain newInOrderOnly ("HI", "HELLO") or newContain newInOrderOnly ("hello", "ho"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)

        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (newContain newInOrderOnly ("HELLO", "HO") or newContain newInOrderOnly ("hello", "ho"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi", "hello")) + " did not contain only " + "(\"HELLO\", \"HO\")" + " in order" + ", and " + decorateToStringValue(List("hi", "hello")) + " did not contain only " + "(\"hello\", \"ho\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
    }

    object `when used with (be xx and contain inOrderOnly xx)` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (be (List(1, 2, 3)) or newContain newInOrderOnly (1, 2, 3))
        all (list1s) should (be (List(2, 3, 4)) or newContain newInOrderOnly (1, 2, 3))
        all (list1s) should (be (List(1, 2, 3)) or newContain newInOrderOnly (2, 3, 4))
        
        val e1 = intercept[TestFailedException] {
          all (list1s) should (be (List(2, 3, 4)) or newContain newInOrderOnly (2, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List(1, 2, 3)) + " was not equal to " + decorateToStringValue(List(2, 3, 4)) + ", and " + decorateToStringValue(List(1, 2, 3)) + " did not contain only " + "(2, 3, 4)" + " in order", thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (be (List("hi", "hello")) or newContain newInOrderOnly ("HI", "HELLO"))
        all (hiLists) should (be (List("ho", "hello")) or newContain newInOrderOnly ("HI", "HELLO"))
        all (hiLists) should (be (List("hi", "hello")) or newContain newInOrderOnly ("HELLO", "HI"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (be (List("ho", "hello")) or newContain newInOrderOnly ("HELLO", "HI"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi", "hello")) + " was not equal to " + decorateToStringValue(List("ho", "hello")) + ", and " + decorateToStringValue(List("hi", "hello")) + " did not contain only " + "(\"HELLO\", \"HI\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (be (List("hi", "hello")) or newContain newInOrderOnly ("HI", "HELLO"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (be (List("ho", "hello")) or newContain newInOrderOnly ("HI", "HELLO"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (be (List("hi", "hello")) or newContain newInOrderOnly ("HELLO", "HI"))) (decided by upperCaseStringEquality)
        
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (be (List("ho", "hello")) or newContain newInOrderOnly ("HELLO", "HI"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi", "hello")) + " was not equal to " + decorateToStringValue(List("ho", "hello")) + ", and " + decorateToStringValue(List("hi", "hello")) + " did not contain only " + "(\"HELLO\", \"HI\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
    }
    
    object `when used with (not contain inOrderOnly xx and not contain inOrderOnly xx)` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (not newContain newInOrderOnly (3, 2, 8) or not newContain newInOrderOnly (8, 3, 4))
        all (list1s) should (not newContain newInOrderOnly (1, 2, 3) or not newContain newInOrderOnly (8, 3, 4))
        all (list1s) should (not newContain newInOrderOnly (3, 2, 8) or not newContain newInOrderOnly (1, 2, 3))
        
        val e1 = intercept[TestFailedException] {
          all (lists) should (not newContain newInOrderOnly (2, 3, 4) or not newContain newInOrderOnly (2, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(List(2, 3, 4)) + " contained only " + "(2, 3, 4)" + " in order" + ", and " + decorateToStringValue(List(2, 3, 4)) + " contained only " + "(2, 3, 4)" + " in order", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (not newContain newInOrderOnly ("HELLO", "HI") or not newContain newInOrderOnly ("hello", "hi"))
        all (hiLists) should (not newContain newInOrderOnly ("HI", "HELLO") or not newContain newInOrderOnly ("hello", "hi"))
        all (hiLists) should (not newContain newInOrderOnly ("HELLO", "HI") or not newContain newInOrderOnly ("hi", "hello"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not newContain newInOrderOnly ("HI", "HELLO") or not newContain newInOrderOnly ("hi", "hello"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi", "hello")) + " contained only " + "(\"HI\", \"HELLO\")" + " in order" + ", and " + decorateToStringValue(List("hi", "hello")) + " contained only " + "(\"hi\", \"hello\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (not newContain newInOrderOnly ("HELLO", "HI") or not newContain newInOrderOnly ("hello", "hi"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (not newContain newInOrderOnly ("HI", "HELLO") or not newContain newInOrderOnly ("hello", "hi"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (not newContain newInOrderOnly ("HELLO", "HI") or not newContain newInOrderOnly ("hi", "hello"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not newContain newInOrderOnly ("HI", "HELLO") or not newContain newInOrderOnly ("hi", "hello"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi", "hello")) + " contained only " + "(\"HI\", \"HELLO\")" + " in order" + ", and " + decorateToStringValue(List("hi", "hello")) + " contained only " + "(\"hi\", \"hello\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
    }
    
/*
    object `when used with (not be xx and not contain inOrderOnly xx)` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (not be (List(2)) or not newContain newInOrderOnly (8, 3, 4))
        all (list1s) should (not be (List(1, 2, 3)) or not newContain newInOrderOnly (8, 3, 4))
        all (list1s) should (not be (List(2)) or not newContain newInOrderOnly (1, 2, 3))
        
        val e1 = intercept[TestFailedException] {
          all (list1s) should (not be (List(1, 2, 3)) or not newContain newInOrderOnly (1, 2, 3))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List(1, 2, 3)) + " was equal to " + decorateToStringValue(List(1, 2, 3)) + ", and " + decorateToStringValue(List(1, 2, 3)) + " contained only " + "(1, 2, 3)" + " in order", thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (not be (List("hello", "ho")) or not newContain newInOrderOnly ("HELLO", "HO"))
        all (hiLists) should (not be (List("hi", "hello")) or not newContain newInOrderOnly ("HELLO", "HO"))
        all (hiLists) should (not be (List("hello", "ho")) or not newContain newInOrderOnly ("HI", "HELLO"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not be (List("hi", "hello")) or not newContain newInOrderOnly ("HI", "HELLO"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi", "hello")) + " was equal to " + decorateToStringValue(List("hi", "hello")) + ", and " + decorateToStringValue(List("hi", "hello")) + " contained only " + "(\"HI\", \"HELLO\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (not be (List("hello", "ho")) or not newContain newInOrderOnly ("HELLO", "HO"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (not be (List("hi", "hello")) or not newContain newInOrderOnly ("HELLO", "HO"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (not be (List("hello", "ho")) or not newContain newInOrderOnly ("HI", "HELLO"))) (decided by upperCaseStringEquality)

        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not be (List("hi", "hello")) or not newContain newInOrderOnly ("HI", "HELLO"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi", "hello")) + " was equal to " + decorateToStringValue(List("hi", "hello")) + ", and " + decorateToStringValue(List("hi", "hello")) + " contained only " + "(\"HI\", \"HELLO\")" + " in order", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
    }
*/
  }
}
