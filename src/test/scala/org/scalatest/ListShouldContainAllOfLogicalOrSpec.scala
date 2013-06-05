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

class ListShouldContainAllOfLogicalOrSpec extends Spec with Matchers {
  
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
  
  val fileName: String = "ListShouldContainAllOfLogicalOrSpec.scala"
  
  object `a List` {
    
    val fumList: List[String] = List("fex", "fum", "foe", "fie", "fee")
    val toList: List[String] = List("too", "you", "to", "birthday", "happy")
    
    object `when used with (contain allOf (..) or contain allOf (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (newContain newAllOf ("fee", "fie", "foe", "fum") or newContain newAllOf ("fie", "fee", "fum", "foe"))
        fumList should (newContain newAllOf ("fee", "fie", "foe", "fam") or newContain newAllOf ("fie", "fee", "fum", "foe"))
        fumList should (newContain newAllOf ("fee", "fie", "foe", "fum") or newContain newAllOf ("fie", "fee", "fam", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (newContain newAllOf ("fee", "fie", "foe", "fam") or newContain newAllOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, Resources("didNotContainAllOfElements", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fam\"") + ", and " + Resources("didNotContainAllOfElements", decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (newContain newAllOf ("FEE", "FIE", "FOE", "FUM") or newContain newAllOf ("FIE", "FEE", "FUM", "FOE"))
        fumList should (newContain newAllOf ("FEE", "FIE", "FOE", "FAM") or newContain newAllOf ("FIE", "FEE", "FUM", "FOE"))
        fumList should (newContain newAllOf ("FEE", "FIE", "FOE", "FUM") or newContain newAllOf ("FIE", "FEE", "FAM", "FOE"))
        val e1 = intercept[TestFailedException] {
          fumList should (newContain newAllOf ("FEE", "FIE", "FOE", "FAM") or (newContain newAllOf ("FIE", "FEE", "FAM", "FOE")))
        }
        checkMessageStackDepth(e1, Resources("didNotContainAllOfElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FAM\"") + ", and " + Resources("didNotContainAllOfElements", decorateToStringValue(fumList), "\"FIE\", \"FEE\", \"FAM\", \"FOE\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (newContain newAllOf ("FEE", "FIE", "FOE", "FUM") or newContain newAllOf ("FIE", "FEE", "FUM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (newContain newAllOf ("FEE", "FIE", "FOE", "FAM") or newContain newAllOf ("FIE", "FEE", "FUM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (newContain newAllOf ("FEE", "FIE", "FOE", "FUM") or newContain newAllOf ("FIE", "FEE", "FAM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (newContain newAllOf ("FEE", "FIE", "FOE", "FAM") or newContain newAllOf ("FIE", "FEE", "FAM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotContainAllOfElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FAM\"") + ", and " + Resources("didNotContainAllOfElements", decorateToStringValue(fumList), "\"FIE\", \"FEE\", \"FAM\", \"FOE\""), fileName, thisLineNumber - 2)
        (fumList should (newContain newAllOf (" FEE ", " FIE ", " FOE ", " FUM ") or newContain newAllOf (" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
    }
    
    object `when used with (equal (..) and contain allOf (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (equal (fumList) or newContain newAllOf ("fie", "fee", "fum", "foe"))
        fumList should (equal (toList) or newContain newAllOf ("fie", "fee", "fum", "foe"))
        fumList should (equal (fumList) or newContain newAllOf ("fie", "fee", "fam", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) or newContain newAllOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("didNotContainAllOfElements", decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (equal (fumList) or newContain newAllOf ("FIE", "FEE", "FUM", "FOE"))
        fumList should (equal (toList) or newContain newAllOf ("FIE", "FEE", "FUM", "FOE"))
        fumList should (equal (fumList) or newContain newAllOf ("FIE", "FEE", "FAM", "FOE"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) or (newContain newAllOf ("FIE", "FEE", "FAM", "FOE")))
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("didNotContainAllOfElements", decorateToStringValue(fumList), "\"FIE\", \"FEE\", \"FAM\", \"FOE\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (equal (toList) or newContain newAllOf ("FIE", "FEE", "FUM", "FOE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (equal (fumList) or newContain newAllOf ("FIE", "FEE", "FUM", "FOE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (equal (toList) or newContain newAllOf ("FIE", "FEE", "FAM", "FOE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (equal (fumList) or newContain newAllOf ("FIE", "FEE", "FAM", "FOE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("didNotContainAllOfElements", decorateToStringValue(fumList), "\"FIE\", \"FEE\", \"FAM\", \"FOE\""), fileName, thisLineNumber - 2)
        (fumList should (equal (toList) or newContain newAllOf Set(" FEE ", " FIE ", " FOE ", " FUM "))) (decided by invertedListOfStringEquality, after being lowerCased and trimmed)
      }
    }
    
    object `when used with (legacyEqual (..) and contain allOf (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (legacyEqual (fumList) or newContain newAllOf ("fie", "fee", "fum", "foe"))
        fumList should (legacyEqual (toList) or newContain newAllOf ("fie", "fee", "fum", "foe"))
        fumList should (legacyEqual (fumList) or newContain newAllOf ("fie", "fee", "fam", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (legacyEqual (toList) or newContain newAllOf ("fie", "fee", "fam", "foe"))
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("didNotContainAllOfElements", decorateToStringValue(fumList), "\"fie\", \"fee\", \"fam\", \"foe\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (legacyEqual (fumList) or newContain newAllOf ("FIE", "FEE", "FUM", "FOE"))
        fumList should (legacyEqual (toList) or newContain newAllOf ("FIE", "FEE", "FUM", "FOE"))
        fumList should (legacyEqual (fumList) or newContain newAllOf ("FIE", "FEE", "FAM", "FOE"))
        val e1 = intercept[TestFailedException] {
          fumList should (legacyEqual (toList) or (newContain newAllOf ("FIE", "FEE", "FAM", "FOE")))
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("didNotContainAllOfElements", decorateToStringValue(fumList), "\"FIE\", \"FEE\", \"FAM\", \"FOE\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (legacyEqual (fumList) or newContain newAllOf ("FIE", "FEE", "FUM", "FOE"))) (decided by upperCaseStringEquality)
        (fumList should (legacyEqual (toList) or newContain newAllOf ("FIE", "FEE", "FUM", "FOE"))) (decided by upperCaseStringEquality)
        (fumList should (legacyEqual (fumList) or newContain newAllOf ("FIE", "FEE", "FAM", "FOE"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (legacyEqual (toList) or newContain newAllOf ("FIE", "FEE", "FAM", "FOE"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("didNotContainAllOfElements", decorateToStringValue(fumList), "\"FIE\", \"FEE\", \"FAM\", \"FOE\""), fileName, thisLineNumber - 2)
        (fumList should (legacyEqual (fumList) or newContain newAllOf (" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed)
      }
    }

    object `when used with (contain allOf (..) and legacyEqual (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (newContain newAllOf ("fie", "fee", "fum", "foe") or legacyEqual (fumList))
        fumList should (newContain newAllOf ("fie", "fee", "fam", "foe") or legacyEqual (fumList))
        fumList should (newContain newAllOf ("fie", "fee", "fum", "foe") or legacyEqual (toList))
        val e1 = intercept[TestFailedException] {
          fumList should (newContain newAllOf ("fee", "fie", "foe", "fam") or legacyEqual (toList))
        }
        checkMessageStackDepth(e1, Resources("didNotContainAllOfElements", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fam\"") + ", and " + Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (newContain newAllOf ("FIE", "FEE", "FUM", "FOE") or legacyEqual (fumList))
        fumList should (newContain newAllOf ("FIE", "FEE", "FAM", "FOE") or legacyEqual (fumList))
        fumList should (newContain newAllOf ("FIE", "FEE", "FUM", "FOE") or legacyEqual (toList))
        val e1 = intercept[TestFailedException] {
          fumList should (newContain newAllOf ("FEE", "FIE", "FOE", "FAM") or legacyEqual (toList))
        }
        checkMessageStackDepth(e1, Resources("didNotContainAllOfElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FAM\"") + ", and " + Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (newContain newAllOf ("FIE", "FEE", "FUM", "FOE") or legacyEqual (fumList))) (decided by upperCaseStringEquality)
        (fumList should (newContain newAllOf ("FIE", "FEE", "FAM", "FOE") or legacyEqual (fumList))) (decided by upperCaseStringEquality)
        (fumList should (newContain newAllOf ("FIE", "FEE", "FUM", "FOE") or legacyEqual (toList))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (newContain newAllOf ("FEE", "FIE", "FOE", "FAM") or legacyEqual (toList))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotContainAllOfElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FAM\"") + ", and " + Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        (fumList should (newContain newAllOf (" FEE ", " FIE ", " FOE ", " FUM ") or legacyEqual (fumList))) (after being lowerCased and trimmed)
      }
    }
    
    object `when used with (not contain allOf (..) and not contain allOf (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (not newContain newAllOf ("fee", "fie", "foe", "fuu") or not newContain newAllOf ("fie", "fee", "fuu", "foe"))
        fumList should (not newContain newAllOf ("fee", "fie", "foe", "fum") or not newContain newAllOf ("fie", "fee", "fuu", "foe"))
        fumList should (not newContain newAllOf ("fee", "fie", "foe", "fuu") or not newContain newAllOf ("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (not newContain newAllOf ("fee", "fie", "foe", "fum") or not newContain newAllOf ("fie", "fee", "fum", "foe"))
        }
        checkMessageStackDepth(e1, Resources("containedAllOfElements", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\"") + ", and " + Resources("containedAllOfElements", decorateToStringValue(fumList), "\"fie\", \"fee\", \"fum\", \"foe\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (not newContain newAllOf ("FEE", "FIE", "FOE", "FUU") or not newContain newAllOf ("FIE", "FEE", "FUU", "FOE"))
        fumList should (not newContain newAllOf ("FEE", "FIE", "FOE", "FUM") or not newContain newAllOf ("FIE", "FEE", "FUU", "FOE"))
        fumList should (not newContain newAllOf ("FEE", "FIE", "FOE", "FUU") or not newContain newAllOf ("FIE", "FEE", "FUM", "FOE"))
        val e1 = intercept[TestFailedException] {
          fumList should (not newContain newAllOf ("FEE", "FIE", "FOE", "FUM") or not newContain newAllOf ("FIE", "FEE", "FUM", "FOE"))
        }
        checkMessageStackDepth(e1, Resources("containedAllOfElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\"") + ", and " + Resources("containedAllOfElements", decorateToStringValue(fumList), "\"FIE\", \"FEE\", \"FUM\", \"FOE\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (not newContain newAllOf ("FEE", "FIE", "FOE", "FUU") or not newContain newAllOf ("FIE", "FEE", "FUU", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (not newContain newAllOf ("FEE", "FIE", "FOE", "FUM") or not newContain newAllOf ("FIE", "FEE", "FUU", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (not newContain newAllOf ("FEE", "FIE", "FOE", "FUU") or not newContain newAllOf ("FIE", "FEE", "FUM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not newContain newAllOf ("FEE", "FIE", "FOE", "FUM") or not newContain newAllOf ("FIE", "FEE", "FUM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("containedAllOfElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\"") + ", and " + Resources("containedAllOfElements", decorateToStringValue(fumList), "\"FIE\", \"FEE\", \"FUM\", \"FOE\""), fileName, thisLineNumber - 2)
        (fumList should (newContain newAllOf (" FEE ", " FIE ", " FOE ", " FUM ") or newContain newAllOf (" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
    }
    
    object `when used with (not equal (..) and not contain allOf (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (not equal (toList) or not newContain newAllOf ("fie", "fee", "fuu", "foe"))
        fumList should (not equal (fumList) or not newContain newAllOf ("fie", "fee", "fuu", "foe"))
        fumList should (not equal (toList) or not newContain newAllOf ("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (not equal (fumList) or not newContain newAllOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("containedAllOfElements", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (not equal (toList) or not newContain newAllOf ("FIE", "FEE", "FUU", "FOE"))
        fumList should (not equal (fumList) or not newContain newAllOf ("FIE", "FEE", "FUU", "FOE"))
        fumList should (not equal (toList) or not newContain newAllOf ("FIE", "FEE", "FUM", "FOE"))
        val e2 = intercept[TestFailedException] {
          fumList should (not equal (fumList) or (not newContain newAllOf ("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e2, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("containedAllOfElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (not equal (fumList) or not newContain newAllOf ("FIE", "FEE", "FUU", "FOE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (not equal (toList) or not newContain newAllOf ("FIE", "FEE", "FUU", "FOE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (not equal (fumList) or not newContain newAllOf ("FIE", "FEE", "FUM", "FOE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not equal (toList) or not newContain newAllOf ("FIE", "FEE", "FUM", "FOE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("containedAllOfElements", decorateToStringValue(fumList), "\"FIE\", \"FEE\", \"FUM\", \"FOE\""), fileName, thisLineNumber - 2)
        (fumList should (not newContain newAllOf (" FEE ", " FIE ", " FOE ", " FUU ") or not newContain newAllOf (" FEE ", " FIE ", " FOE ", " FUU "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
    }
    
    object `when used with (not be (..) and not contain allOf (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (not be (toList) or not newContain newAllOf ("fie", "fee", "fuu", "foe"))
        fumList should (not be (fumList) or not newContain newAllOf ("fie", "fee", "fuu", "foe"))
        fumList should (not be (toList) or not newContain newAllOf ("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) or not newContain newAllOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources("wasEqualTo", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("containedAllOfElements", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (not be (toList) or not newContain newAllOf ("FIE", "FEE", "FUU", "FOE"))
        fumList should (not be (fumList) or not newContain newAllOf ("FIE", "FEE", "FUU", "FOE"))
        fumList should (not be (toList) or not newContain newAllOf ("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) or (not newContain newAllOf ("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e1, Resources("wasEqualTo", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("containedAllOfElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (not be (toList) or not newContain newAllOf ("FIE", "FEE", "FUU", "FOE"))) (decided by upperCaseStringEquality)
        (fumList should (not be (fumList) or not newContain newAllOf ("FIE", "FEE", "FUU", "FOE"))) (decided by upperCaseStringEquality)
        (fumList should (not be (toList) or not newContain newAllOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not be (fumList) or not newContain newAllOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("wasEqualTo", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("containedAllOfElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
        (fumList should (not newContain newAllOf (" FEE ", " FIE ", " FOE ", " FUU ") or not newContain newAllOf (" FEE ", " FIE ", " FOE ", " FUU "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
    }
    
  }
  
  object `collection of Lists` {
    
    val list1s: Vector[List[Int]] = Vector(List(3, 2, 1, 0), List(3, 2, 1, 0), List(3, 2, 1, 0))
    val lists: Vector[List[Int]] = Vector(List(3, 2, 1, 0), List(3, 2, 1, 0), List(8, 4, 3, 2))
    val nils: Vector[List[Int]] = Vector(Nil, Nil, Nil)
    val listsNil: Vector[List[Int]] = Vector(List(3, 2, 1, 0), List(3, 2, 1, 0), Nil)
    val hiLists: Vector[List[String]] = Vector(List("howdy", "hi", "hello"), List("howdy", "hi", "hello"), List("howdy", "hi", "hello"))
    val toLists: Vector[List[String]] = Vector(List("nice", "you", "to"), List("nice", "you", "to"), List("nice", "you", "to"))
    
    def allErrMsg(index: Int, message: String, lineNumber: Int, left: Any): String = 
      "'all' inspection failed, because: \n" +
      "  at index " + index + ", " + message + " (" + fileName + ":" + (lineNumber) + ") \n" +
      "in " + left
    
    object `when used with (contain allOf (..) and contain allOf (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (newContain newAllOf (3, 2, 1) or newContain newAllOf (1, 3, 2))
        all (list1s) should (newContain newAllOf (3, 2, 5) or newContain newAllOf (1, 3, 2))
        all (list1s) should (newContain newAllOf (3, 2, 1) or newContain newAllOf (2, 3, 4))
        
        atLeast (2, lists) should (newContain newAllOf (3, 1, 2) or newContain newAllOf (1, 2, 3))
        atLeast (2, lists) should (newContain newAllOf (3, 6, 5) or newContain newAllOf (1, 3, 2))
        atLeast (2, lists) should (newContain newAllOf (3, 1, 2) or newContain newAllOf (8, 3, 4))
        
        val e1 = intercept[TestFailedException] {
          all (lists) should (newContain newAllOf (3, 1, 2) or newContain newAllOf (1, 3, 2))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(List(8, 4, 3, 2)) + " did not contain all of " + "(3, 1, 2)" + ", and " + decorateToStringValue(List(8, 4, 3, 2)) + " did not contain all of " + "(1, 3, 2)", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (newContain newAllOf ("HELLO", "HI") or newContain newAllOf ("hello", "hi"))
        all (hiLists) should (newContain newAllOf ("HELLO", "HO") or newContain newAllOf ("hello", "hi"))
        all (hiLists) should (newContain newAllOf ("HELLO", "HI") or newContain newAllOf ("hello", "ho"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (newContain newAllOf ("HELLO", "HO") or newContain newAllOf ("hello", "ho"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("howdy", "hi", "hello")) + " did not contain all of " + "(\"HELLO\", \"HO\")" + ", and " + decorateToStringValue(List("howdy", "hi", "hello")) + " did not contain all of " + "(\"hello\", \"ho\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (newContain newAllOf ("HELLO", "HI") or newContain newAllOf ("hello", "hi"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (newContain newAllOf ("HELLO", "HO") or newContain newAllOf ("hello", "hi"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (newContain newAllOf ("HELLO", "HI") or newContain newAllOf ("hello", "ho"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (newContain newAllOf ("HELLO", "HO") or newContain newAllOf ("hello", "ho"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("howdy", "hi", "hello")) + " did not contain all of " + "(\"HELLO\", \"HO\")" + ", and " + decorateToStringValue(List("howdy", "hi", "hello")) + " did not contain all of " + "(\"hello\", \"ho\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
    }
    
    object `when used with (be (..) and contain allOf (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (be (List(3, 2, 1, 0)) or newContain newAllOf (1, 2, 3))
        all (list1s) should (be (List(2, 3, 4)) or newContain newAllOf (1, 2, 3))
        all (list1s) should (be (List(3, 2, 1, 0)) or newContain newAllOf (2, 3, 4))
        
        val e1 = intercept[TestFailedException] {
          all (list1s) should (be (List(2, 3, 4)) or newContain newAllOf (2, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List(3, 2, 1, 0)) + " was not equal to " + decorateToStringValue(List(2, 3, 4)) + ", and " + decorateToStringValue(List(3, 2, 1, 0)) + " did not contain all of " + "(2, 3, 4)", thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (be (List("howdy", "hi", "hello")) or newContain newAllOf ("HELLO", "HI"))
        all (hiLists) should (be (List("ho", "hello")) or newContain newAllOf ("HELLO", "HI"))
        all (hiLists) should (be (List("howdy", "hi", "hello")) or newContain newAllOf ("HELLO", "HO"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (be (List("ho", "hello")) or newContain newAllOf ("HELLO", "HO"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("howdy", "hi", "hello")) + " was not equal to " + decorateToStringValue(List("ho", "hello")) + ", and " + decorateToStringValue(List("howdy", "hi", "hello")) + " did not contain all of " + "(\"HELLO\", \"HO\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (be (List("howdy", "hi", "hello")) or newContain newAllOf ("HELLO", "HI"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (be (List("ho", "hello")) or newContain newAllOf ("HELLO", "HI"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (be (List("howdy", "hi", "hello")) or newContain newAllOf ("HELLO", "HO"))) (decided by upperCaseStringEquality)
        
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (be (List("ho", "hello")) or newContain newAllOf ("HELLO", "HO"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("howdy", "hi", "hello")) + " was not equal to " + decorateToStringValue(List("ho", "hello")) + ", and " + decorateToStringValue(List("howdy", "hi", "hello")) + " did not contain all of " + "(\"HELLO\", \"HO\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
    }
    
    object `when used with (not contain allOf xx and not contain allOf xx)` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (not newContain newAllOf (3, 2, 8) or not newContain newAllOf (8, 3, 4))
        all (list1s) should (not newContain newAllOf (1, 2, 3) or not newContain newAllOf (8, 3, 4))
        all (list1s) should (not newContain newAllOf (3, 2, 8) or not newContain newAllOf (2, 3, 1))
        
        val e1 = intercept[TestFailedException] {
          all (lists) should (not newContain newAllOf (4, 2, 3) or not newContain newAllOf (2, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(List(8, 4, 3, 2)) + " contained all of " + "(4, 2, 3)" + ", and " + decorateToStringValue(List(8, 4, 3, 2)) + " contained all of " + "(2, 3, 4)", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (not newContain newAllOf ("HELLO", "HO") or not newContain newAllOf ("hello", "ho"))
        all (hiLists) should (not newContain newAllOf ("HELLO", "HI") or not newContain newAllOf ("hello", "ho"))
        all (hiLists) should (not newContain newAllOf ("HELLO", "HO") or not newContain newAllOf ("hello", "hi"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not newContain newAllOf ("HELLO", "HI") or not newContain newAllOf ("hello", "hi"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("howdy", "hi", "hello")) + " contained all of " + "(\"HELLO\", \"HI\")" + ", and " + decorateToStringValue(List("howdy", "hi", "hello")) + " contained all of " + "(\"hello\", \"hi\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (not newContain newAllOf ("HELLO", "HO") or not newContain newAllOf ("hello", "ho"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (not newContain newAllOf ("HELLO", "HI") or not newContain newAllOf ("hello", "ho"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (not newContain newAllOf ("HELLO", "HO") or not newContain newAllOf ("hello", "hi"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not newContain newAllOf ("HELLO", "HI") or not newContain newAllOf ("hello", "hi"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("howdy", "hi", "hello")) + " contained all of " + "(\"HELLO\", \"HI\")" + ", and " + decorateToStringValue(List("howdy", "hi", "hello")) + " contained all of " + "(\"hello\", \"hi\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
    }
    
    object `when used with (not be (...) and not contain allOf (...))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (not be (List(2)) or not newContain newAllOf (8, 3, 4))
        all (list1s) should (not be (List(3, 2, 1, 0)) or not newContain newAllOf (8, 3, 4))
        all (list1s) should (not be (List(2)) or not newContain newAllOf (1, 2, 3))
        
        val e1 = intercept[TestFailedException] {
          all (list1s) should (not be (List(3, 2, 1, 0)) or not newContain newAllOf (2, 3, 1))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List(3, 2, 1, 0)) + " was equal to " + decorateToStringValue(List(3, 2, 1, 0)) + ", and " + decorateToStringValue(List(3, 2, 1, 0)) + " contained all of " + "(2, 3, 1)", thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (not be (List("hello", "ho")) or not newContain newAllOf ("HELLO", "HO"))
        all (hiLists) should (not be (List("howdy", "hello", "hi")) or not newContain newAllOf ("HELLO", "HO"))
        all (hiLists) should (not be (List("hello", "ho")) or not newContain newAllOf ("HELLO", "HI"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not be (List("howdy", "hi", "hello")) or not newContain newAllOf ("HELLO", "HI"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("howdy", "hi", "hello")) + " was equal to " + decorateToStringValue(List("howdy", "hi", "hello")) + ", and " + decorateToStringValue(List("howdy", "hi", "hello")) + " contained all of " + "(\"HELLO\", \"HI\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (not be (List("hello", "ho")) or not newContain newAllOf ("HELLO", "HO"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (not be (List("howdy", "hello", "hi")) or not newContain newAllOf ("HELLO", "HO"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (not be (List("hello", "ho")) or not newContain newAllOf ("HELLO", "HI"))) (decided by upperCaseStringEquality)
        
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not be (List("howdy", "hi", "hello")) or not newContain newAllOf ("HELLO", "HI"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("howdy", "hi", "hello")) + " was equal to " + decorateToStringValue(List("howdy", "hi", "hello")) + ", and " + decorateToStringValue(List("howdy", "hi", "hello")) + " contained all of " + "(\"HELLO\", \"HI\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
    }
  }
}
