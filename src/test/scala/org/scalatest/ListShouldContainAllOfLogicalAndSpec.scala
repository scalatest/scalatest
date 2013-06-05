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

class ListShouldContainAllOfLogicalAndSpec extends Spec with Matchers {
  
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
  
  val fileName: String = "ListShouldContainAllOfLogicalAndSpec.scala"
  
  object `a List` {
    
    val fumList: List[String] = List("fex", "fum", "foe", "fie", "fee")
    val toList: List[String] = List("too", "you", "to", "birthday", "happy")
    
    object `when used with (contain allOf (..) and contain allOf (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (newContain newAllOf ("fee", "fie", "foe", "fum") and newContain newAllOf ("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (newContain newAllOf ("happy", "birthday", "to", "you") and newContain newAllOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources("didNotContainAllOfElements", decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (newContain newAllOf ("fee", "fie", "foe", "fum") and newContain newAllOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e2, Resources("containedAllOfElements", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\"") + ", but " + Resources("didNotContainAllOfElements", decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (newContain newAllOf ("FEE", "FIE", "FOE", "FUM") and newContain newAllOf ("FEE", "FIE", "FUM", "FOE"))
        val e1 = intercept[TestFailedException] {
          fumList should (newContain newAllOf ("FEE", "FIE", "FOE", "FAM") and newContain newAllOf ("FEE", "FIE", "FUM", "FOE"))
        }
        checkMessageStackDepth(e1, Resources("didNotContainAllOfElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FAM\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (newContain newAllOf ("FEE", "FIE", "FOE", "FUM") and (newContain newAllOf ("FEE", "FIE", "FAM", "FOE")))
        }
        checkMessageStackDepth(e2, Resources("containedAllOfElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\"") + ", but " + Resources("didNotContainAllOfElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FAM\", \"FOE\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (newContain newAllOf ("FEE", "FIE", "FOE", "FUM") and newContain newAllOf ("FEE", "FIE", "FUM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (newContain newAllOf ("FEE", "FIE", "FOE", "FAM") and newContain newAllOf ("FEE", "FIE", "FUM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotContainAllOfElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FAM\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (newContain newAllOf ("FEE", "FIE", "FOE", "FUM") and newContain newAllOf ("FEE", "FIE", "FAM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources("containedAllOfElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\"") + ", but " + Resources("didNotContainAllOfElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FAM\", \"FOE\""), fileName, thisLineNumber - 2)
        (fumList should (newContain newAllOf (" FEE ", " FIE ", " FOE ", " FUM ") and newContain newAllOf (" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
    }
    
    object `when used with (equal (..) and contain allOf (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (equal (fumList) and newContain newAllOf ("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) and newContain newAllOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (equal (fumList) and newContain newAllOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e2, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", but " + Resources("didNotContainAllOfElements", decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (equal (fumList) and newContain newAllOf ("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) and newContain newAllOf ("FEE", "FIE", "FOE", "FUM"))
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (equal (fumList) and (newContain newAllOf ("FEE", "FIE", "FOE", "FAM")))
        }
        checkMessageStackDepth(e2, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", but " + Resources("didNotContainAllOfElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FAM\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (equal (toList) and newContain newAllOf ("FEE", "FIE", "FOE", "FUM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (equal (toList) and newContain newAllOf ("FEE", "FIE", "FOE", "FAM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", but " + Resources("didNotContainAllOfElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FAM\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (equal (fumList) and newContain newAllOf ("FEE", "FIE", "FOE", "FUM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(fumList)), fileName, thisLineNumber - 2)
        (fumList should (equal (toList) and newContain newAllOf (" FEE ", " FIE ", " FOE ", " FUM "))) (decided by invertedListOfStringEquality, after being lowerCased and trimmed)
      }
    }
    
    object `when used with (legacyEqual (..) and contain allOf (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (legacyEqual (fumList) and newContain newAllOf ("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (legacyEqual (toList) and newContain newAllOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (legacyEqual (fumList) and newContain newAllOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e2, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", but " + Resources("didNotContainAllOfElements", decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (legacyEqual (fumList) and newContain newAllOf ("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (legacyEqual (toList) and newContain newAllOf ("FEE", "FIE", "FOE", "FUM"))
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (legacyEqual (fumList) and (newContain newAllOf ("happy", "birthday", "to", "you")))
        }
        checkMessageStackDepth(e2, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", but " + Resources("didNotContainAllOfElements", decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (legacyEqual (fumList) and newContain newAllOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (legacyEqual (fumList) and newContain newAllOf ("happy", "birthday", "to", "you"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", but " + Resources("didNotContainAllOfElements", decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (legacyEqual (toList) and newContain newAllOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        (fumList should (legacyEqual (fumList) and newContain newAllOf (" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed)
      }
    }

    object `when used with (contain allOf (..) and legacyEqual (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (newContain newAllOf ("fie", "fee", "fum", "foe") and legacyEqual (fumList))
        val e1 = intercept[TestFailedException] {
          fumList should (newContain newAllOf ("fee", "fie", "foe", "fum") and legacyEqual (toList))
        }
        checkMessageStackDepth(e1, Resources("containedAllOfElements", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\"") + ", but " + Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (newContain newAllOf ("happy", "birthday", "to", "you") and legacyEqual (fumList))
        }
        checkMessageStackDepth(e2, Resources("didNotContainAllOfElements", decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (newContain newAllOf ("FIE", "FEE", "FUM", "FOE") and legacyEqual (fumList))
        val e1 = intercept[TestFailedException] {
          fumList should (newContain newAllOf ("FIE", "FEE", "FAM", "FOE") and legacyEqual (toList))
        }
        checkMessageStackDepth(e1, Resources("didNotContainAllOfElements", decorateToStringValue(fumList), "\"FIE\", \"FEE\", \"FAM\", \"FOE\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (newContain newAllOf ("HAPPY", "BIRTHDAY", "TO", "YOU") and (legacyEqual (fumList)))
        }
        checkMessageStackDepth(e2, Resources("didNotContainAllOfElements", decorateToStringValue(fumList), "\"HAPPY\", \"BIRTHDAY\", \"TO\", \"YOU\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (newContain newAllOf ("FIE", "FEE", "FUM", "FOE") and legacyEqual (fumList))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (newContain newAllOf ("FIE", "FEE", "FAM", "FOE") and legacyEqual (fumList))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotContainAllOfElements", decorateToStringValue(fumList), "\"FIE\", \"FEE\", \"FAM\", \"FOE\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (newContain newAllOf ("FIE", "FEE", "FUM", "FOE") and legacyEqual (toList))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources("containedAllOfElements", decorateToStringValue(fumList), "\"FIE\", \"FEE\", \"FUM\", \"FOE\"") + ", but " + Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        (fumList should (newContain newAllOf (" FEE ", " FIE ", " FOE ", " FUM ") and legacyEqual (fumList))) (after being lowerCased and trimmed)
      }
    }
    
    object `when used with (not contain allOf xx and not contain allOf xx)` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (not newContain newAllOf ("fee", "fie", "foe", "fuu") and not newContain newAllOf ("fie", "fee", "fuu", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (not newContain newAllOf ("fee", "fie", "foe", "fum") and not newContain newAllOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, Resources("containedAllOfElements", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not newContain newAllOf ("happy", "birthday", "to", "you") and not newContain newAllOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e2, Resources("didNotContainAllOfElements", decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\"") + ", but " + Resources("containedAllOfElements", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (not newContain newAllOf ("FIE", "FEE", "FAM", "FOE") and not newContain newAllOf ("FIE", "FEE", "FOE", "FAM"))
        val e1 = intercept[TestFailedException] {
          fumList should (not newContain newAllOf ("FIE", "FEE", "FUM", "FOE") and not newContain newAllOf ("FIE", "FEE", "FOE", "FAM"))
        }
        checkMessageStackDepth(e1, Resources("containedAllOfElements", decorateToStringValue(fumList), "\"FIE\", \"FEE\", \"FUM\", \"FOE\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not newContain newAllOf ("FIE", "FEE", "FAM", "FOE") and (not newContain newAllOf ("FIE", "FEE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e2, Resources("didNotContainAllOfElements", decorateToStringValue(fumList), "\"FIE\", \"FEE\", \"FAM\", \"FOE\"") + ", but " + Resources("containedAllOfElements", decorateToStringValue(fumList), "\"FIE\", \"FEE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (not newContain newAllOf ("FIE", "FEE", "FAM", "FOE") and not newContain newAllOf ("FIE", "FEE", "FOE", "FAM"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not newContain newAllOf ("FIE", "FEE", "FAM", "FOE") and not newContain newAllOf ("FIE", "FEE", "FOE", "FUM"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotContainAllOfElements", decorateToStringValue(fumList), "\"FIE\", \"FEE\", \"FAM\", \"FOE\"") + ", but " + Resources("containedAllOfElements", decorateToStringValue(fumList), "\"FIE\", \"FEE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (not newContain newAllOf ("FIE", "FEE", "FUM", "FOE") and not newContain newAllOf ("FIE", "FEE", "FOE", "FAM"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources("containedAllOfElements", decorateToStringValue(fumList), "\"FIE\", \"FEE\", \"FUM\", \"FOE\""), fileName, thisLineNumber - 2)
        (fumList should (newContain newAllOf (" FEE ", " FIE ", " FOE ", " FUM ") and newContain newAllOf (" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
    }
    
    object `when used with (not equal (..) and not contain allOf (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (not equal (toList) and not newContain newAllOf ("fie", "fee", "fuu", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (not equal (fumList) and not newContain newAllOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(fumList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not equal (toList) and not newContain newAllOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e2, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", but " + Resources("containedAllOfElements", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (not equal (toList) and not newContain newAllOf ("FIE", "FEE", "FAM", "FOE"))
        val e1 = intercept[TestFailedException] {
          fumList should (not equal (fumList) and not newContain newAllOf ("FIE", "FEE", "FAM", "FOE"))
        }
        checkMessageStackDepth(e1, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(fumList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not equal (toList) and (not newContain newAllOf ("FIE", "FEE", "FUM", "FOE")))
        }
        checkMessageStackDepth(e2, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", but " + Resources("containedAllOfElements", decorateToStringValue(fumList), "\"FIE\", \"FEE\", \"FUM\", \"FOE\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (not equal (fumList) and not newContain newAllOf ("FIE", "FEE", "FAM", "FOE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not equal (fumList) and not newContain newAllOf ("FIE", "FEE", "FUM", "FOE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", but " + Resources("containedAllOfElements", decorateToStringValue(fumList), "\"FIE\", \"FEE\", \"FUM\", \"FOE\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (not equal (toList) and not newContain newAllOf ("FIE", "FEE", "FAM", "FOE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        (fumList should (not newContain newAllOf (" FEE ", " FIE ", " FOE ", " FUU ") and not newContain newAllOf (" FEE ", " FIE ", " FOE ", " FUU "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
    }
    
    object `when used with (not be (..) and not contain allOf (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (not be (toList) and not newContain newAllOf ("fie", "fee", "fuu", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) and not newContain newAllOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, Resources("wasEqualTo", decorateToStringValue(fumList), decorateToStringValue(fumList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not be (toList) and not newContain newAllOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e2, Resources("wasNotEqualTo", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", but " + Resources("containedAllOfElements", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (not be (toList) and not newContain newAllOf ("FIE", "FEE", "FAM", "FOE"))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) and not newContain newAllOf ("FIE", "FEE", "FAM", "FOE"))
        }
        checkMessageStackDepth(e1, Resources("wasEqualTo", decorateToStringValue(fumList), decorateToStringValue(fumList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not be (toList) and (not newContain newAllOf ("FIE", "FEE", "FUM", "FOE")))
        }
        checkMessageStackDepth(e2, Resources("wasNotEqualTo", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", but " + Resources("containedAllOfElements", decorateToStringValue(fumList), "\"FIE\", \"FEE\", \"FUM\", \"FOE\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (not be (toList) and not newContain newAllOf ("FIE", "FEE", "FAM", "FOE"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not be (toList) and not newContain newAllOf ("FIE", "FEE", "FUM", "FOE"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("wasNotEqualTo", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", but " + Resources("containedAllOfElements", decorateToStringValue(fumList), "\"FIE\", \"FEE\", \"FUM\", \"FOE\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (not be (fumList) and not newContain newAllOf ("FIE", "FEE", "FAM", "FOE"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources("wasEqualTo", decorateToStringValue(fumList), decorateToStringValue(fumList)), fileName, thisLineNumber - 2)
        (fumList should (not newContain newAllOf (" FEE ", " FIE ", " FOE ", " FUU ") and not newContain newAllOf (" FEE ", " FIE ", " FOE ", " FUU "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
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
    
    object `used with contain allOf xx and contain allOf xx` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (newContain newAllOf (3, 2, 1) and newContain newAllOf (1, 3, 2))
        atLeast (2, lists) should (newContain newAllOf (3, 1, 2) and newContain newAllOf (2, 3, 1))
        atMost (2, lists) should (newContain newAllOf (3, 1, 2) and newContain newAllOf (2, 3, 1))
        no (lists) should (newContain newAllOf (3, 6, 9) and newContain newAllOf (3, 4, 5))
        
        val e1 = intercept[TestFailedException] {
          all (lists) should (newContain newAllOf (1, 2, 3) and newContain newAllOf (1, 3, 2))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(List(8, 4, 3, 2)) + " did not contain all of " + "(1, 2, 3)", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (list1s) should (newContain newAllOf (1, 2, 3) and newContain newAllOf (1, 3, 4))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(List(3, 2, 1, 0)) + " contained all of " + "(1, 2, 3)" + ", but " + decorateToStringValue(List(3, 2, 1, 0)) + " did not contain all of " + "(1, 3, 4)", thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
        
        val e4 = intercept[TestFailedException] {
          all (hiLists) should (newContain newAllOf ("hi", "hello") and newContain newAllOf ("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e4, allErrMsg(0, decorateToStringValue(List("howdy", "hi", "hello")) + " contained all of " + "(\"hi\", \"hello\")" + ", but " + decorateToStringValue(List("howdy", "hi", "hello")) + " did not contain all of " + "(\"ho\", \"hey\", \"howdy\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (newContain newAllOf ("HELLO", "HI") and newContain newAllOf ("HI", "HELLO"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (newContain newAllOf ("HO", "HELLO") and newContain newAllOf ("HI", "HELLO"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("howdy", "hi", "hello")) + " did not contain all of " + "(\"HO\", \"HELLO\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (hiLists) should (newContain newAllOf ("HELLO", "HI") and newContain newAllOf ("HO", "HELLO"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(List("howdy", "hi", "hello")) + " contained all of " + "(\"HELLO\", \"HI\")" + ", but " + decorateToStringValue(List("howdy", "hi", "hello")) + " did not contain all of " + "(\"HO\", \"HELLO\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (newContain newAllOf ("HELLO", "HI") and newContain newAllOf ("HI", "HELLO"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (newContain newAllOf ("HO", "HELLO") and newContain newAllOf ("HI", "HELLO"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("howdy", "hi", "hello")) + " did not contain all of " + "(\"HO\", \"HELLO\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (newContain newAllOf ("HELLO", "HI") and newContain newAllOf ("HO", "HELLO"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(List("howdy", "hi", "hello")) + " contained all of " + "(\"HELLO\", \"HI\")" + ", but " + decorateToStringValue(List("howdy", "hi", "hello")) + " did not contain all of " + "(\"HO\", \"HELLO\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
    }
    
    object `when used with (be (..) and contain allOf (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (be (List(3, 2, 1, 0)) and newContain newAllOf (1, 3, 2))
        atLeast (2, lists) should (be (List(3, 2, 1, 0)) and newContain newAllOf (1, 3, 2))
        atMost (2, lists) should (be (List(3, 2, 1, 0)) and newContain newAllOf (2, 3, 1))
        no (lists) should (be (List(3, 6, 9)) and newContain newAllOf (3, 4, 5))
        no (nils) should (be (List(1, 6, 8)) and newContain newAllOf (1, 3, 4))
        no (listsNil) should (be (List(2, 6, 8)) and newContain newAllOf (3, 4, 5))
        
        val e1 = intercept[TestFailedException] {
          all (lists) should (be (List(3, 2, 1, 0)) and newContain newAllOf (1, 3, 2))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(List(8, 4, 3, 2)) + " was not equal to " + decorateToStringValue(List(3, 2, 1, 0)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (list1s) should (be (List(3, 2, 1, 0)) and newContain newAllOf (2, 3, 8))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(List(3, 2, 1, 0)) + " was equal to " + decorateToStringValue(List(3, 2, 1, 0)) + ", but " + decorateToStringValue(List(3, 2, 1, 0)) + " did not contain all of " + "(2, 3, 8)", thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
        
        val e3 = intercept[TestFailedException] {
          all (nils) should (be (List("hey")) and newContain newAllOf ("hello", "hi"))
        }
        checkMessageStackDepth(e3, allErrMsg(0, decorateToStringValue(Nil) + " was not equal to " + decorateToStringValue(List("hey")), thisLineNumber - 2, nils), fileName, thisLineNumber - 2)
        
        val e4 = intercept[TestFailedException] {
          all (hiLists) should (be (List("howdy", "hi", "hello")) and newContain newAllOf ("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e4, allErrMsg(0, decorateToStringValue(List("howdy", "hi", "hello")) + " was equal to " + decorateToStringValue(List("howdy", "hi", "hello")) + ", but " + decorateToStringValue(List("howdy", "hi", "hello")) + " did not contain all of " + "(\"ho\", \"hey\", \"howdy\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e5 = intercept[TestFailedException] {
          all (listsNil) should (be (List(3, 2, 1, 0)) and newContain newAllOf (1, 3, 2))
        }
        checkMessageStackDepth(e5, allErrMsg(2, decorateToStringValue(Nil) + " was not equal to " + decorateToStringValue(List(3, 2, 1, 0)), thisLineNumber - 2, listsNil), fileName, thisLineNumber - 2)
        
        val e6 = intercept[TestFailedException] {
          all (list1s) should (be (List(3, 2, 1, 0)) and newContain newAllOf (2, 3, 8))
        }
        checkMessageStackDepth(e6, allErrMsg(0, decorateToStringValue(List(3, 2, 1, 0)) + " was equal to " + decorateToStringValue(List(3, 2, 1, 0)) + ", but " + decorateToStringValue(List(3, 2, 1, 0)) + " did not contain all of " + "(2, 3, 8)", thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (be (List("howdy", "hi", "hello")) and newContain newAllOf ("HELLO", "HI"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (be (List("HI", "HELLO")) and newContain newAllOf ("HELLO", "HI"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("howdy", "hi", "hello")) + " was not equal to " + decorateToStringValue(List("HI", "HELLO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (hiLists) should (be (List("howdy", "hi", "hello")) and newContain newAllOf ("HO", "HELLO"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(List("howdy", "hi", "hello")) + " was equal to " + decorateToStringValue(List("howdy", "hi", "hello")) + ", but " + decorateToStringValue(List("howdy", "hi", "hello")) + " did not contain all of " + "(\"HO\", \"HELLO\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (be (List("howdy", "hi", "hello")) and newContain newAllOf ("HELLO", "HI"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (be (List("HI", "HELLO")) and newContain newAllOf ("HELLO", "HI"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("howdy", "hi", "hello")) + " was not equal to " + decorateToStringValue(List("HI", "HELLO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (be (List("howdy", "hi", "hello")) and newContain newAllOf ("HO", "HELLO"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(List("howdy", "hi", "hello")) + " was equal to " + decorateToStringValue(List("howdy", "hi", "hello")) + ", but " + decorateToStringValue(List("howdy", "hi", "hello")) + " did not contain all of " + "(\"HO\", \"HELLO\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
    }
    
    object `when used with (not contain allOf xx and not contain allOf xx)` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (not newContain newAllOf (3, 2, 8) and not newContain newAllOf (8, 3, 4))
        atLeast (2, lists) should (not newContain newAllOf (3, 8, 5) and not newContain newAllOf (8, 3, 4))
        atMost (2, lists) should (not newContain newAllOf (2, 4, 3) and newContain newAllOf (4, 3, 2))
        no (list1s) should (not newContain newAllOf (1, 2, 3) and not newContain newAllOf (1, 3, 2))
        
        val e1 = intercept[TestFailedException] {
          all (lists) should (not newContain newAllOf (2, 3, 4) and not newContain newAllOf (8, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(List(8, 4, 3, 2)) + " contained all of " + "(2, 3, 4)", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (lists) should (not newContain newAllOf (3, 6, 8) and not newContain newAllOf (2, 3, 4))
        }
        checkMessageStackDepth(e2, allErrMsg(2, decorateToStringValue(List(8, 4, 3, 2)) + " did not contain all of " + "(3, 6, 8)" + ", but " + decorateToStringValue(List(8, 4, 3, 2)) + " contained all of " + "(2, 3, 4)", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
        
        val e3 = intercept[TestFailedException] {
          all (hiLists) should (not newContain newAllOf ("hello", "hi") and not newContain newAllOf ("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e3, allErrMsg(0, decorateToStringValue(List("howdy", "hi", "hello")) + " contained all of " + "(\"hello\", \"hi\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e4 = intercept[TestFailedException] {
          all (hiLists) should (not newContain newAllOf ("ho", "hey", "howdy") and not newContain newAllOf ("hello", "hi"))
        }
        checkMessageStackDepth(e4, allErrMsg(0, decorateToStringValue(List("howdy", "hi", "hello")) + " did not contain all of " + "(\"ho\", \"hey\", \"howdy\")" + ", but " + decorateToStringValue(List("howdy", "hi", "hello")) + " contained all of " + "(\"hello\", \"hi\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (not newContain newAllOf ("TO") and not newContain newAllOf ("HO"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not newContain newAllOf ("HELLO", "HI") and not newContain newAllOf ("HO"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("howdy", "hi", "hello")) + " contained all of " + "(\"HELLO\", \"HI\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (hiLists) should (not newContain newAllOf ("HO") and not newContain newAllOf ("HELLO", "HI"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(List("howdy", "hi", "hello")) + " did not contain all of " + "(\"HO\")" + ", but " + decorateToStringValue(List("howdy", "hi", "hello")) + " contained all of " + "(\"HELLO\", \"HI\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (not newContain newAllOf ("TO") and not newContain newAllOf ("HO"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not newContain newAllOf ("HELLO", "HI") and not newContain newAllOf ("HO"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("howdy", "hi", "hello")) + " contained all of " + "(\"HELLO\", \"HI\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (not newContain newAllOf ("HO") and not newContain newAllOf ("HELLO", "HI"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(List("howdy", "hi", "hello")) + " did not contain all of " + "(\"HO\")" + ", but " + decorateToStringValue(List("howdy", "hi", "hello")) + " contained all of " + "(\"HELLO\", \"HI\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
    }
    
    object `when used with (not be (..) and not contain allOf (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (not be (List(2)) and not newContain newAllOf (8, 3, 4))
        atLeast (2, lists) should (not be (List(3)) and not newContain newAllOf (8, 3, 4))
        atMost (2, lists) should (not be (List(4, 3, 2)) and not newContain newAllOf (3, 4, 2))
        no (list1s) should (not be (List(3, 2, 1)) and not newContain newAllOf (1, 2, 3))
        
        val e1 = intercept[TestFailedException] {
          all (lists) should (not be (List(8, 4, 3, 2)) and not newContain newAllOf (8, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(List(8, 4, 3, 2)) + " was equal to " + decorateToStringValue(List(8, 4, 3, 2)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (lists) should (not be (List(3)) and not newContain newAllOf (2, 3, 4))
        }
        checkMessageStackDepth(e2, allErrMsg(2, decorateToStringValue(List(8, 4, 3, 2)) + " was not equal to " + decorateToStringValue(List(3)) + ", but " + decorateToStringValue(List(8, 4, 3, 2)) + " contained all of " + "(2, 3, 4)", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
        
        val e3 = intercept[TestFailedException] {
          all (hiLists) should (not be (List("howdy", "hi", "hello")) and not newContain newAllOf ("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e3, allErrMsg(0, decorateToStringValue(List("howdy", "hi", "hello")) + " was equal to " + decorateToStringValue(List("howdy", "hi", "hello")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e4 = intercept[TestFailedException] {
          all (hiLists) should (not be (List("ho")) and not newContain newAllOf ("hello", "hi"))
        }
        checkMessageStackDepth(e4, allErrMsg(0, decorateToStringValue(List("howdy", "hi", "hello")) + " was not equal to " + decorateToStringValue(List("ho")) + ", but " + decorateToStringValue(List("howdy", "hi", "hello")) + " contained all of " + "(\"hello\", \"hi\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (not be (List("ho")) and not newContain newAllOf ("HO", "HELLO"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not be (List("howdy", "hi", "hello")) and not newContain newAllOf ("HELLO", "HI"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("howdy", "hi", "hello")) + " was equal to " + decorateToStringValue(List("howdy", "hi", "hello")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (hiLists) should (not be (List("ho")) and not newContain newAllOf ("HI", "HELLO"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(List("howdy", "hi", "hello")) + " was not equal to " + decorateToStringValue(List("ho")) + ", but " + decorateToStringValue(List("howdy", "hi", "hello")) + " contained all of " + "(\"HI\", \"HELLO\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (not be (List("ho")) and not newContain newAllOf ("HO", "HELLO"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not be (List("howdy", "hi", "hello")) and not newContain newAllOf ("HELLO", "HI"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("howdy", "hi", "hello")) + " was equal to " + decorateToStringValue(List("howdy", "hi", "hello")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (not be (List("ho")) and not newContain newAllOf ("HI", "HELLO"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(List("howdy", "hi", "hello")) + " was not equal to " + decorateToStringValue(List("ho")) + ", but " + decorateToStringValue(List("howdy", "hi", "hello")) + " contained all of " + "(\"HI\", \"HELLO\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
    }
  }
}
