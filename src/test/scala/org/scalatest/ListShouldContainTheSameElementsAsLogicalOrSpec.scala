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

class ListShouldContainTheSameElementsAsLogicalOrSpec extends Spec with Matchers {
  
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
  
  val fileName: String = "ListShouldContainTheSameElementsAsLogicalOrSpec.scala"
  
  object `a List` {
    
    val fumList: List[String] = List("fum", "foe", "fie", "fee")
    val toList: List[String] = List("you", "to", "birthday", "happy")
    
    object `when used with (contain theSameElementsAs (..) or contain theSameElementsAs (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (contain theSameElementsAs Set("fee", "fie", "foe", "fum") or contain theSameElementsAs Set("fie", "fee", "fum", "foe"))
        fumList should (contain theSameElementsAs Set("fee", "fie", "foe", "fam") or contain theSameElementsAs Set("fie", "fee", "fum", "foe"))
        fumList should (contain theSameElementsAs Set("fee", "fie", "foe", "fum") or contain theSameElementsAs Set("fie", "fee", "fam", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain theSameElementsAs Set("fee", "fie", "foe", "fam") or contain theSameElementsAs Set("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, Resources("didNotContainSameElements", decorateToStringValue(fumList), decorateToStringValue(Set("fee", "fie", "foe", "fam"))) + ", and " + Resources("didNotContainSameElements", decorateToStringValue(fumList), decorateToStringValue(Set("happy", "birthday", "to", "you"))), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (contain theSameElementsAs Set("FEE", "FIE", "FOE", "FUM") or contain theSameElementsAs Set("FIE", "FEE", "FUM", "FOE"))
        fumList should (contain theSameElementsAs Set("FEE", "FIE", "FOE", "FAM") or contain theSameElementsAs Set("FIE", "FEE", "FUM", "FOE"))
        fumList should (contain theSameElementsAs Set("FEE", "FIE", "FOE", "FUM") or contain theSameElementsAs Set("FIE", "FEE", "FAM", "FOE"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain theSameElementsAs Set("FEE", "FIE", "FOE", "FAM") or (contain theSameElementsAs Set("FIE", "FEE", "FAM", "FOE")))
        }
        checkMessageStackDepth(e1, Resources("didNotContainSameElements", decorateToStringValue(fumList), decorateToStringValue(Set("FEE", "FIE", "FOE", "FAM"))) + ", and " + Resources("didNotContainSameElements", decorateToStringValue(fumList), decorateToStringValue(Set("FIE", "FEE", "FAM", "FOE"))), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (contain theSameElementsAs Set("FEE", "FIE", "FOE", "FUM") or contain theSameElementsAs Set("FIE", "FEE", "FUM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (contain theSameElementsAs Set("FEE", "FIE", "FOE", "FAM") or contain theSameElementsAs Set("FIE", "FEE", "FUM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (contain theSameElementsAs Set("FEE", "FIE", "FOE", "FUM") or contain theSameElementsAs Set("FIE", "FEE", "FAM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain theSameElementsAs Set("FEE", "FIE", "FOE", "FAM") or contain theSameElementsAs Set("FIE", "FEE", "FAM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotContainSameElements", decorateToStringValue(fumList), decorateToStringValue(Set("FEE", "FIE", "FOE", "FAM"))) + ", and " + Resources("didNotContainSameElements", decorateToStringValue(fumList), decorateToStringValue(Set("FIE", "FEE", "FAM", "FOE"))), fileName, thisLineNumber - 2)
        (fumList should (contain theSameElementsAs Set(" FEE ", " FIE ", " FOE ", " FUM ") or contain theSameElementsAs Set(" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
    }
    
    object `when used with (equal (..) and contain theSameElementsAs (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (equal (fumList) or contain theSameElementsAs Set("fie", "fee", "fum", "foe"))
        fumList should (equal (toList) or contain theSameElementsAs Set("fie", "fee", "fum", "foe"))
        fumList should (equal (fumList) or contain theSameElementsAs Set("fie", "fee", "fam", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) or contain theSameElementsAs Set("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("didNotContainSameElements", decorateToStringValue(fumList), decorateToStringValue(Set("happy", "birthday", "to", "you"))), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (equal (fumList) or contain theSameElementsAs Set("FIE", "FEE", "FUM", "FOE"))
        fumList should (equal (toList) or contain theSameElementsAs Set("FIE", "FEE", "FUM", "FOE"))
        fumList should (equal (fumList) or contain theSameElementsAs Set("FIE", "FEE", "FAM", "FOE"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) or (contain theSameElementsAs Set("FIE", "FEE", "FAM", "FOE")))
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("didNotContainSameElements", decorateToStringValue(fumList), decorateToStringValue(Set("FIE", "FEE", "FAM", "FOE"))), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (equal (toList) or contain theSameElementsAs Set("FIE", "FEE", "FUM", "FOE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (equal (fumList) or contain theSameElementsAs Set("FIE", "FEE", "FUM", "FOE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (equal (toList) or contain theSameElementsAs (Set("FIE", "FEE", "FAM", "FOE")))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (equal (fumList) or contain theSameElementsAs (Set("FIE", "FEE", "FAM", "FOE")))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("didNotContainSameElements", decorateToStringValue(fumList), decorateToStringValue(Set("FIE", "FEE", "FAM", "FOE"))), fileName, thisLineNumber - 2)
        (fumList should (equal (toList) or contain theSameElementsAs Set(" FEE ", " FIE ", " FOE ", " FUM "))) (decided by invertedListOfStringEquality, after being lowerCased and trimmed)
      }
    }
    
    object `when used with (legacyEqual (..) and contain theSameElementsAs (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (legacyEqual (fumList) or contain theSameElementsAs Set("fie", "fee", "fum", "foe"))
        fumList should (legacyEqual (toList) or contain theSameElementsAs Set("fie", "fee", "fum", "foe"))
        fumList should (legacyEqual (fumList) or contain theSameElementsAs Set("fie", "fee", "fam", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (legacyEqual (toList) or contain theSameElementsAs Set("fie", "fee", "fam", "foe"))
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("didNotContainSameElements", decorateToStringValue(fumList), decorateToStringValue(Set("fie", "fee", "fam", "foe"))), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (legacyEqual (fumList) or contain theSameElementsAs Set("FIE", "FEE", "FUM", "FOE"))
        fumList should (legacyEqual (toList) or contain theSameElementsAs Set("FIE", "FEE", "FUM", "FOE"))
        fumList should (legacyEqual (fumList) or contain theSameElementsAs Set("FIE", "FEE", "FAM", "FOE"))
        val e1 = intercept[TestFailedException] {
          fumList should (legacyEqual (toList) or (contain theSameElementsAs Set("FIE", "FEE", "FAM", "FOE")))
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("didNotContainSameElements", decorateToStringValue(fumList), decorateToStringValue(Set("FIE", "FEE", "FAM", "FOE"))), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (legacyEqual (fumList) or contain theSameElementsAs Set("FIE", "FEE", "FUM", "FOE"))) (decided by upperCaseStringEquality)
        (fumList should (legacyEqual (toList) or contain theSameElementsAs Set("FIE", "FEE", "FUM", "FOE"))) (decided by upperCaseStringEquality)
        (fumList should (legacyEqual (fumList) or contain theSameElementsAs Set("FIE", "FEE", "FAM", "FOE"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (legacyEqual (toList) or contain theSameElementsAs Set("FIE", "FEE", "FAM", "FOE"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("didNotContainSameElements", decorateToStringValue(fumList), decorateToStringValue(Set("FIE", "FEE", "FAM", "FOE"))), fileName, thisLineNumber - 2)
        (fumList should (legacyEqual (fumList) or contain theSameElementsAs Set(" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed)
      }
    }

    object `when used with (contain theSameElementsAs (..) and legacyEqual (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (contain theSameElementsAs Set("fie", "fee", "fum", "foe") or legacyEqual (fumList))
        fumList should (contain theSameElementsAs Set("fie", "fee", "fam", "foe") or legacyEqual (fumList))
        fumList should (contain theSameElementsAs Set("fie", "fee", "fum", "foe") or legacyEqual (toList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain theSameElementsAs Set("fee", "fie", "foe", "fam") or legacyEqual (toList))
        }
        checkMessageStackDepth(e1, Resources("didNotContainSameElements", decorateToStringValue(fumList), decorateToStringValue(Set("fee", "fie", "foe", "fam"))) + ", and " + Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (contain theSameElementsAs Set("FIE", "FEE", "FUM", "FOE") or legacyEqual (fumList))
        fumList should (contain theSameElementsAs Set("FIE", "FEE", "FAM", "FOE") or legacyEqual (fumList))
        fumList should (contain theSameElementsAs Set("FIE", "FEE", "FUM", "FOE") or legacyEqual (toList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain theSameElementsAs Set("FEE", "FIE", "FOE", "FAM") or legacyEqual (toList))
        }
        checkMessageStackDepth(e1, Resources("didNotContainSameElements", decorateToStringValue(fumList), decorateToStringValue(Set("FEE", "FIE", "FOE", "FAM"))) + ", and " + Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (contain theSameElementsAs Set("FIE", "FEE", "FUM", "FOE") or legacyEqual (fumList))) (decided by upperCaseStringEquality)
        (fumList should (contain theSameElementsAs Set("FIE", "FEE", "FAM", "FOE") or legacyEqual (fumList))) (decided by upperCaseStringEquality)
        (fumList should (contain theSameElementsAs Set("FIE", "FEE", "FUM", "FOE") or legacyEqual (toList))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain theSameElementsAs Set("FEE", "FIE", "FOE", "FAM") or legacyEqual (toList))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotContainSameElements", decorateToStringValue(fumList), decorateToStringValue(Set("FEE", "FIE", "FOE", "FAM"))) + ", and " + Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        (fumList should (contain theSameElementsAs Set(" FEE ", " FIE ", " FOE ", " FUM ") or legacyEqual (fumList))) (after being lowerCased and trimmed)
      }
    }
    
    object `when used with (not contain theSameElementsAs (..) and not contain theSameElementsAs (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (not contain theSameElementsAs (Set("fee", "fie", "foe", "fuu")) or not contain theSameElementsAs (Set("fie", "fee", "fuu", "foe")))
        fumList should (not contain theSameElementsAs (Set("fee", "fie", "foe", "fum")) or not contain theSameElementsAs (Set("fie", "fee", "fuu", "foe")))
        fumList should (not contain theSameElementsAs (Set("fee", "fie", "foe", "fuu")) or not contain theSameElementsAs (Set("fie", "fee", "fum", "foe")))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain theSameElementsAs (Set("fee", "fie", "foe", "fum")) or not contain theSameElementsAs (Set("fie", "fee", "fum", "foe")))
        }
        checkMessageStackDepth(e1, Resources("containedSameElements", decorateToStringValue(fumList), decorateToStringValue(Set("fee", "fie", "foe", "fum"))) + ", and " + Resources("containedSameElements", decorateToStringValue(fumList), decorateToStringValue(Set("fie", "fee", "fum", "foe"))), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (not contain theSameElementsAs (Set("FEE", "FIE", "FOE", "FUU")) or not contain theSameElementsAs (Set("FIE", "FEE", "FUU", "FOE")))
        fumList should (not contain theSameElementsAs (Set("FEE", "FIE", "FOE", "FUM")) or not contain theSameElementsAs (Set("FIE", "FEE", "FUU", "FOE")))
        fumList should (not contain theSameElementsAs (Set("FEE", "FIE", "FOE", "FUU")) or not contain theSameElementsAs (Set("FIE", "FEE", "FUM", "FOE")))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain theSameElementsAs (Set("FEE", "FIE", "FOE", "FUM")) or not contain theSameElementsAs (Set("FIE", "FEE", "FUM", "FOE")))
        }
        checkMessageStackDepth(e1, Resources("containedSameElements", decorateToStringValue(fumList), decorateToStringValue(Set("FEE", "FIE", "FOE", "FUM"))) + ", and " + Resources("containedSameElements", decorateToStringValue(fumList), decorateToStringValue(Set("FIE", "FEE", "FUM", "FOE"))), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (not contain theSameElementsAs (Set("FEE", "FIE", "FOE", "FUU")) or not contain theSameElementsAs (Set("FIE", "FEE", "FUU", "FOE")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (not contain theSameElementsAs (Set("FEE", "FIE", "FOE", "FUM")) or not contain theSameElementsAs (Set("FIE", "FEE", "FUU", "FOE")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (not contain theSameElementsAs (Set("FEE", "FIE", "FOE", "FUU")) or not contain theSameElementsAs (Set("FIE", "FEE", "FUM", "FOE")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not contain theSameElementsAs (Set("FEE", "FIE", "FOE", "FUM")) or not contain theSameElementsAs (Set("FIE", "FEE", "FUM", "FOE")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("containedSameElements", decorateToStringValue(fumList), decorateToStringValue(Set("FEE", "FIE", "FOE", "FUM"))) + ", and " + Resources("containedSameElements", decorateToStringValue(fumList), decorateToStringValue(Set("FIE", "FEE", "FUM", "FOE"))), fileName, thisLineNumber - 2)
        (fumList should (contain theSameElementsAs Set(" FEE ", " FIE ", " FOE ", " FUM ") or contain theSameElementsAs Set(" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
    }
    
    object `when used with (not equal (..) and not contain theSameElementsAs (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (not equal (toList) or not contain theSameElementsAs (Set("fie", "fee", "fuu", "foe")))
        fumList should (not equal (fumList) or not contain theSameElementsAs (Set("fie", "fee", "fuu", "foe")))
        fumList should (not equal (toList) or not contain theSameElementsAs (Set("fie", "fee", "fum", "foe")))
        val e1 = intercept[TestFailedException] {
          fumList should (not equal (fumList) or not contain theSameElementsAs (Set("fee", "fie", "foe", "fum")))
        }
        checkMessageStackDepth(e1, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("containedSameElements", decorateToStringValue(fumList), decorateToStringValue(Set("fee", "fie", "foe", "fum"))), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (not equal (toList) or not contain theSameElementsAs (Set("FIE", "FEE", "FUU", "FOE")))
        fumList should (not equal (fumList) or not contain theSameElementsAs (Set("FIE", "FEE", "FUU", "FOE")))
        fumList should (not equal (toList) or not contain theSameElementsAs (Set("FIE", "FEE", "FUM", "FOE")))
        val e2 = intercept[TestFailedException] {
          fumList should (not equal (fumList) or (not contain theSameElementsAs (Set("FEE", "FIE", "FOE", "FUM"))))
        }
        checkMessageStackDepth(e2, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("containedSameElements", decorateToStringValue(fumList), decorateToStringValue(Set("FEE", "FIE", "FOE", "FUM"))), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (not equal (fumList) or not contain theSameElementsAs (Set("FIE", "FEE", "FUU", "FOE")))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (not equal (toList) or not contain theSameElementsAs (Set("FIE", "FEE", "FUU", "FOE")))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (not equal (fumList) or not contain theSameElementsAs (Set("FIE", "FEE", "FUM", "FOE")))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not equal (toList) or not contain theSameElementsAs (Set("FIE", "FEE", "FUM", "FOE")))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("containedSameElements", decorateToStringValue(fumList), decorateToStringValue(Set("FIE", "FEE", "FUM", "FOE"))), fileName, thisLineNumber - 2)
        (fumList should (not contain theSameElementsAs (Set(" FEE ", " FIE ", " FOE ", " FUU ")) or not contain theSameElementsAs (Set(" FEE ", " FIE ", " FOE ", " FUU ")))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
    }
    
    object `when used with (not be (..) and not contain theSameElementsAs (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (not be (toList) or not contain theSameElementsAs (Set("fie", "fee", "fuu", "foe")))
        fumList should (not be (fumList) or not contain theSameElementsAs (Set("fie", "fee", "fuu", "foe")))
        fumList should (not be (toList) or not contain theSameElementsAs (Set("fee", "fie", "foe", "fum")))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) or not contain theSameElementsAs (Set("fee", "fie", "foe", "fum")))
        }
        checkMessageStackDepth(e1, Resources("wasEqualTo", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("containedSameElements", decorateToStringValue(fumList), decorateToStringValue(Set("fee", "fie", "foe", "fum"))), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (not be (toList) or not contain theSameElementsAs (Set("FIE", "FEE", "FUU", "FOE")))
        fumList should (not be (fumList) or not contain theSameElementsAs (Set("FIE", "FEE", "FUU", "FOE")))
        fumList should (not be (toList) or not contain theSameElementsAs (Set("FEE", "FIE", "FOE", "FUM")))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) or (not contain theSameElementsAs (Set("FEE", "FIE", "FOE", "FUM"))))
        }
        checkMessageStackDepth(e1, Resources("wasEqualTo", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("containedSameElements", decorateToStringValue(fumList), decorateToStringValue(Set("FEE", "FIE", "FOE", "FUM"))), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (not be (toList) or not contain theSameElementsAs (Set("FIE", "FEE", "FUU", "FOE")))) (decided by upperCaseStringEquality)
        (fumList should (not be (fumList) or not contain theSameElementsAs (Set("FIE", "FEE", "FUU", "FOE")))) (decided by upperCaseStringEquality)
        (fumList should (not be (toList) or not contain theSameElementsAs (Set("FEE", "FIE", "FOE", "FUM")))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not be (fumList) or not contain theSameElementsAs (Set("FEE", "FIE", "FOE", "FUM")))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("wasEqualTo", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("containedSameElements", decorateToStringValue(fumList), decorateToStringValue(Set("FEE", "FIE", "FOE", "FUM"))), fileName, thisLineNumber - 2)
        (fumList should (not contain theSameElementsAs (Set(" FEE ", " FIE ", " FOE ", " FUU ")) or not contain theSameElementsAs (Set(" FEE ", " FIE ", " FOE ", " FUU ")))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
    }
    
  }
  
  object `collection of Lists` {
    
    val list1s: Vector[List[Int]] = Vector(List(3, 2, 1), List(3, 2, 1), List(3, 2, 1))
    val lists: Vector[List[Int]] = Vector(List(3, 2, 1), List(3, 2, 1), List(4, 3, 2))
    val nils: Vector[List[Int]] = Vector(Nil, Nil, Nil)
    val listsNil: Vector[List[Int]] = Vector(List(3, 2, 1), List(3, 2, 1), Nil)
    val hiLists: Vector[List[String]] = Vector(List("hi", "hello"), List("hi", "hello"), List("hi", "hello"))
    val toLists: Vector[List[String]] = Vector(List("you", "to"), List("you", "to"), List("you", "to"))
    
    def allErrMsg(index: Int, message: String, lineNumber: Int, left: Any): String = 
      "'all' inspection failed, because: \n" +
      "  at index " + index + ", " + message + " (" + fileName + ":" + (lineNumber) + ") \n" +
      "in " + left
    
    object `when used with (contain theSameElementsAs (..) and contain theSameElementsAs (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (contain theSameElementsAs Set(3, 2, 1) or contain theSameElementsAs Set(1, 3, 2))
        all (list1s) should (contain theSameElementsAs Set(3, 2, 5) or contain theSameElementsAs Set(1, 3, 2))
        all (list1s) should (contain theSameElementsAs Set(3, 2, 1) or contain theSameElementsAs Set(2, 3, 4))
        
        atLeast (2, lists) should (contain theSameElementsAs Set(3, 1, 2) or contain theSameElementsAs Set(1, 2, 3))
        atLeast (2, lists) should (contain theSameElementsAs Set(3, 6, 5) or contain theSameElementsAs Set(1, 3, 2))
        atLeast (2, lists) should (contain theSameElementsAs Set(3, 1, 2) or contain theSameElementsAs Set(8, 3, 4))
        
        val e1 = intercept[TestFailedException] {
          all (lists) should (contain theSameElementsAs Set(3, 1, 2) or contain theSameElementsAs Set(1, 3, 2))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(List(4, 3, 2)) + " did not contain the same elements as " + decorateToStringValue(Set(3, 1, 2)) + ", and " + decorateToStringValue(List(4, 3, 2)) + " did not contain the same elements as " + decorateToStringValue(Set(1, 3, 2)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (contain theSameElementsAs Set("HELLO", "HI") or contain theSameElementsAs Set("hello", "hi"))
        all (hiLists) should (contain theSameElementsAs Set("HELLO", "HO") or contain theSameElementsAs Set("hello", "hi"))
        all (hiLists) should (contain theSameElementsAs Set("HELLO", "HI") or contain theSameElementsAs Set("hello", "ho"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (contain theSameElementsAs Set("HELLO", "HO") or contain theSameElementsAs Set("hello", "ho"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi", "hello")) + " did not contain the same elements as " + decorateToStringValue(Set("HELLO", "HO")) + ", and " + decorateToStringValue(List("hi", "hello")) + " did not contain the same elements as " + decorateToStringValue(Set("hello", "ho")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (contain theSameElementsAs Set("HELLO", "HI") or contain theSameElementsAs Set("hello", "hi"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (contain theSameElementsAs Set("HELLO", "HO") or contain theSameElementsAs Set("hello", "hi"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (contain theSameElementsAs Set("HELLO", "HI") or contain theSameElementsAs Set("hello", "ho"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (contain theSameElementsAs Set("HELLO", "HO") or contain theSameElementsAs Set("hello", "ho"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi", "hello")) + " did not contain the same elements as " + decorateToStringValue(Set("HELLO", "HO")) + ", and " + decorateToStringValue(List("hi", "hello")) + " did not contain the same elements as " + decorateToStringValue(Set("hello", "ho")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
    }
    
    object `when used with (be (..) and contain theSameElementsAs (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (be (List(3, 2, 1)) or contain theSameElementsAs Set(1, 2, 3))
        all (list1s) should (be (List(2, 3, 4)) or contain theSameElementsAs Set(1, 2, 3))
        all (list1s) should (be (List(3, 2, 1)) or contain theSameElementsAs Set(2, 3, 4))
        
        val e1 = intercept[TestFailedException] {
          all (list1s) should (be (List(2, 3, 4)) or contain theSameElementsAs Set(2, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List(3, 2, 1)) + " was not equal to " + decorateToStringValue(List(2, 3, 4)) + ", and " + decorateToStringValue(List(3, 2, 1)) + " did not contain the same elements as " + decorateToStringValue(Set(2, 3, 4)), thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (be (List("hi", "hello")) or contain theSameElementsAs Set("HELLO", "HI"))
        all (hiLists) should (be (List("ho", "hello")) or contain theSameElementsAs Set("HELLO", "HI"))
        all (hiLists) should (be (List("hi", "hello")) or contain theSameElementsAs Set("HELLO", "HO"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (be (List("ho", "hello")) or contain theSameElementsAs Set("HELLO", "HO"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi", "hello")) + " was not equal to " + decorateToStringValue(List("ho", "hello")) + ", and " + decorateToStringValue(List("hi", "hello")) + " did not contain the same elements as " + decorateToStringValue(Set("HELLO", "HO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (be (List("hi", "hello")) or contain theSameElementsAs Set("HELLO", "HI"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (be (List("ho", "hello")) or contain theSameElementsAs Set("HELLO", "HI"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (be (List("hi", "hello")) or contain theSameElementsAs Set("HELLO", "HO"))) (decided by upperCaseStringEquality)
        
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (be (List("ho", "hello")) or contain theSameElementsAs Set("HELLO", "HO"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi", "hello")) + " was not equal to " + decorateToStringValue(List("ho", "hello")) + ", and " + decorateToStringValue(List("hi", "hello")) + " did not contain the same elements as " + decorateToStringValue(Set("HELLO", "HO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
    }
    
    object `when used with (not contain theSameElementsAs xx and not contain theSameElementsAs xx)` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (not contain theSameElementsAs (Set(3, 2, 8)) or not contain theSameElementsAs (Set(8, 3, 4)))
        all (list1s) should (not contain theSameElementsAs (Set(1, 2, 3)) or not contain theSameElementsAs (Set(8, 3, 4)))
        all (list1s) should (not contain theSameElementsAs (Set(3, 2, 8)) or not contain theSameElementsAs (Set(2, 3, 1)))
        
        val e1 = intercept[TestFailedException] {
          all (lists) should (not contain theSameElementsAs (Set(4, 2, 3)) or not contain theSameElementsAs (Set(2, 3, 4)))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(List(4, 3, 2)) + " contained the same elements as " + decorateToStringValue(Set(4, 2, 3)) + ", and " + decorateToStringValue(List(4, 3, 2)) + " contained the same elements as " + decorateToStringValue(Set(2, 3, 4)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (not contain theSameElementsAs (Set("HELLO", "HO")) or not contain theSameElementsAs (Set("hello", "ho")))
        all (hiLists) should (not contain theSameElementsAs (Set("HELLO", "HI")) or not contain theSameElementsAs (Set("hello", "ho")))
        all (hiLists) should (not contain theSameElementsAs (Set("HELLO", "HO")) or not contain theSameElementsAs (Set("hello", "hi")))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not contain theSameElementsAs (Set("HELLO", "HI")) or not contain theSameElementsAs (Set("hello", "hi")))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi", "hello")) + " contained the same elements as " + decorateToStringValue(Set("HELLO", "HI")) + ", and " + decorateToStringValue(List("hi", "hello")) + " contained the same elements as " + decorateToStringValue(Set("hello", "hi")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (not contain theSameElementsAs (Set("HELLO", "HO")) or not contain theSameElementsAs (Set("hello", "ho")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (not contain theSameElementsAs (Set("HELLO", "HI")) or not contain theSameElementsAs (Set("hello", "ho")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (not contain theSameElementsAs (Set("HELLO", "HO")) or not contain theSameElementsAs (Set("hello", "hi")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not contain theSameElementsAs (Set("HELLO", "HI")) or not contain theSameElementsAs (Set("hello", "hi")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi", "hello")) + " contained the same elements as " + decorateToStringValue(Set("HELLO", "HI")) + ", and " + decorateToStringValue(List("hi", "hello")) + " contained the same elements as " + decorateToStringValue(Set("hello", "hi")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
    }
    
    object `when used with (not be (...) and not contain theSameElementsAs (...))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (not be (List(2)) or not contain theSameElementsAs (Set(8, 3, 4)))
        all (list1s) should (not be (List(3, 2, 1)) or not contain theSameElementsAs (Set(8, 3, 4)))
        all (list1s) should (not be (List(2)) or not contain theSameElementsAs (Set(1, 2, 3)))
        
        val e1 = intercept[TestFailedException] {
          all (list1s) should (not be (List(3, 2, 1)) or not contain theSameElementsAs (Set(2, 3, 1)))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List(3, 2, 1)) + " was equal to " + decorateToStringValue(List(3, 2, 1)) + ", and " + decorateToStringValue(List(3, 2, 1)) + " contained the same elements as " + decorateToStringValue(Set(2, 3, 1)), thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (not be (List("hello", "ho")) or not contain theSameElementsAs (Set("HELLO", "HO")))
        all (hiLists) should (not be (List("hello", "hi")) or not contain theSameElementsAs (Set("HELLO", "HO")))
        all (hiLists) should (not be (List("hello", "ho")) or not contain theSameElementsAs (Set("HELLO", "HI")))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not be (List("hi", "hello")) or not contain theSameElementsAs (Set("HELLO", "HI")))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi", "hello")) + " was equal to " + decorateToStringValue(List("hi", "hello")) + ", and " + decorateToStringValue(List("hi", "hello")) + " contained the same elements as " + decorateToStringValue(Set("HELLO", "HI")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (not be (List("hello", "ho")) or not contain theSameElementsAs (Set("HELLO", "HO")))) (decided by upperCaseStringEquality)
        (all (hiLists) should (not be (List("hello", "hi")) or not contain theSameElementsAs (Set("HELLO", "HO")))) (decided by upperCaseStringEquality)
        (all (hiLists) should (not be (List("hello", "ho")) or not contain theSameElementsAs (Set("HELLO", "HI")))) (decided by upperCaseStringEquality)
        
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not be (List("hi", "hello")) or not contain theSameElementsAs (Set("HELLO", "HI")))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi", "hello")) + " was equal to " + decorateToStringValue(List("hi", "hello")) + ", and " + decorateToStringValue(List("hi", "hello")) + " contained the same elements as " + decorateToStringValue(Set("HELLO", "HI")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
    }
  }
}
