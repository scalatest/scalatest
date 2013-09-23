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
import org.scalautils.Uniformity
import org.scalautils.StringNormalizations._
import SharedHelpers._
import FailureMessages.decorateToStringValue

class ListShouldContainOnlyLogicalOrSpec extends Spec with Matchers {
  
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
  
  //ADDITIONAL//
  
  val fileName: String = "ListShouldContainOnlyLogicalOrSpec.scala"
  
  object `a List` {
    
    val fumList: List[String] = List("fum", "foe", "fie", "fee")
    val toList: List[String] = List("you", "to", "birthday", "happy")
    
    object `when used with (contain only (..) or contain only (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (contain only ("fee", "fie", "foe", "fum") or contain only ("fie", "fee", "fum", "foe"))
        fumList should (contain only ("fee", "fie", "foe", "fam") or contain only ("fie", "fee", "fum", "foe"))
        fumList should (contain only ("fee", "fie", "foe", "fum") or contain only ("fie", "fee", "fam", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain only ("fee", "fie", "foe", "fam") or contain only ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, Resources("didNotContainOnlyElements", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fam\"") + ", and " + Resources("didNotContainOnlyElements", decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (contain only ("FEE", "FIE", "FOE", "FUM") or contain only ("FIE", "FEE", "FUM", "FOE"))
        fumList should (contain only ("FEE", "FIE", "FOE", "FAM") or contain only ("FIE", "FEE", "FUM", "FOE"))
        fumList should (contain only ("FEE", "FIE", "FOE", "FUM") or contain only ("FIE", "FEE", "FAM", "FOE"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain only ("FEE", "FIE", "FOE", "FAM") or (contain only ("FIE", "FEE", "FAM", "FOE")))
        }
        checkMessageStackDepth(e1, Resources("didNotContainOnlyElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FAM\"") + ", and " + Resources("didNotContainOnlyElements", decorateToStringValue(fumList), "\"FIE\", \"FEE\", \"FAM\", \"FOE\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (contain only ("FEE", "FIE", "FOE", "FUM") or contain only ("FIE", "FEE", "FUM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (contain only ("FEE", "FIE", "FOE", "FAM") or contain only ("FIE", "FEE", "FUM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (contain only ("FEE", "FIE", "FOE", "FUM") or contain only ("FIE", "FEE", "FAM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain only ("FEE", "FIE", "FOE", "FAM") or contain only ("FIE", "FEE", "FAM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotContainOnlyElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FAM\"") + ", and " + Resources("didNotContainOnlyElements", decorateToStringValue(fumList), "\"FIE\", \"FEE\", \"FAM\", \"FOE\""), fileName, thisLineNumber - 2)
        (fumList should (contain only (" FEE ", " FIE ", " FOE ", " FUM ") or contain only (" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
      
      def `should throw NotAllowedException with correct stack depth and message when RHS is empty` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (contain only () or contain only ("fie", "fee", "fum", "foe"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("onlyEmpty")))
        
        val e2 = intercept[exceptions.NotAllowedException] {
          fumList should (contain only ("fie", "fee", "fum", "foe") or contain only ())
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources("onlyEmpty")))
      }
      
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (contain only ("fee", "fie", "foe", "fie", "fum") or contain only ("fie", "fee", "fum", "foe"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("onlyDuplicate")))
        
        val e2 = intercept[exceptions.NotAllowedException] {
          fumList should (contain only ("fie", "fee", "fum", "foe") or contain only ("fee", "fie", "foe", "fie", "fum"))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources("onlyDuplicate")))
      }

      def `should throw TFE with friendly reminder when single GenTraversable argument is passed and failed` {
        val e1 = intercept[TestFailedException] {
          fumList should (contain only Vector("fee", "fie", "foe", "fam") or contain only Vector("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, Resources("didNotContainOnlyElementsWithFriendlyReminder", decorateToStringValue(fumList), Vector("fee", "fie", "foe", "fam")) + ", and " + Resources("didNotContainOnlyElementsWithFriendlyReminder", decorateToStringValue(fumList), Vector("happy", "birthday", "to", "you")), fileName, thisLineNumber - 2)
      }
    }
    
    object `when used with (equal (..) and contain only (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (equal (fumList) or contain only ("fie", "fee", "fum", "foe"))
        fumList should (equal (toList) or contain only ("fie", "fee", "fum", "foe"))
        fumList should (equal (fumList) or contain only ("fie", "fee", "fam", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) or contain only ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("didNotContainOnlyElements", decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (equal (fumList) or contain only ("FIE", "FEE", "FUM", "FOE"))
        fumList should (equal (toList) or contain only ("FIE", "FEE", "FUM", "FOE"))
        fumList should (equal (fumList) or contain only ("FIE", "FEE", "FAM", "FOE"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) or (contain only ("FIE", "FEE", "FAM", "FOE")))
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("didNotContainOnlyElements", decorateToStringValue(fumList), "\"FIE\", \"FEE\", \"FAM\", \"FOE\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (equal (toList) or contain only ("FIE", "FEE", "FUM", "FOE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (equal (fumList) or contain only ("FIE", "FEE", "FUM", "FOE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (equal (toList) or contain only ("FIE", "FEE", "FAM", "FOE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (equal (fumList) or contain only ("FIE", "FEE", "FAM", "FOE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("didNotContainOnlyElements", decorateToStringValue(fumList), "\"FIE\", \"FEE\", \"FAM\", \"FOE\""), fileName, thisLineNumber - 2)
        (fumList should (equal (toList) or contain only Set(" FEE ", " FIE ", " FOE ", " FUM "))) (decided by invertedListOfStringEquality, after being lowerCased and trimmed)
      }
      
      def `should throw NotAllowedException with correct stack depth and message when RHS is empty` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (equal (fumList) or contain only ())
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("onlyEmpty")))
      }
      
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (equal (fumList) or contain only ("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("onlyDuplicate")))
      }

      def `should throw TFE with friendly reminder when single GenTraversable argument is passed and failed` {
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) or contain only Vector("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("didNotContainOnlyElementsWithFriendlyReminder", decorateToStringValue(fumList), Vector("happy", "birthday", "to", "you")), fileName, thisLineNumber - 2)
      }
    }
    
    object `when used with (be (..) and contain only (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (be (fumList) or contain only ("fie", "fee", "fum", "foe"))
        fumList should (be (toList) or contain only ("fie", "fee", "fum", "foe"))
        fumList should (be (fumList) or contain only ("fie", "fee", "fam", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (be (toList) or contain only ("fie", "fee", "fam", "foe"))
        }
        checkMessageStackDepth(e1, Resources("wasNotEqualTo", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("didNotContainOnlyElements", decorateToStringValue(fumList), "\"fie\", \"fee\", \"fam\", \"foe\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (be (fumList) or contain only ("FIE", "FEE", "FUM", "FOE"))
        fumList should (be (toList) or contain only ("FIE", "FEE", "FUM", "FOE"))
        fumList should (be (fumList) or contain only ("FIE", "FEE", "FAM", "FOE"))
        val e1 = intercept[TestFailedException] {
          fumList should (be (toList) or (contain only ("FIE", "FEE", "FAM", "FOE")))
        }
        checkMessageStackDepth(e1, Resources("wasNotEqualTo", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("didNotContainOnlyElements", decorateToStringValue(fumList), "\"FIE\", \"FEE\", \"FAM\", \"FOE\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (be (fumList) or contain only ("FIE", "FEE", "FUM", "FOE"))) (decided by upperCaseStringEquality)
        (fumList should (be (toList) or contain only ("FIE", "FEE", "FUM", "FOE"))) (decided by upperCaseStringEquality)
        (fumList should (be (fumList) or contain only ("FIE", "FEE", "FAM", "FOE"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (be (toList) or contain only ("FIE", "FEE", "FAM", "FOE"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("wasNotEqualTo", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("didNotContainOnlyElements", decorateToStringValue(fumList), "\"FIE\", \"FEE\", \"FAM\", \"FOE\""), fileName, thisLineNumber - 2)
        (fumList should (be (fumList) or contain only (" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed)
      }
      
      def `should throw NotAllowedException with correct stack depth and message when RHS is empty` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (be (fumList) or contain only ())
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("onlyEmpty")))
      }
      
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (be (fumList) or contain only ("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("onlyDuplicate")))
      }

      def `should throw TFE with friendly reminder when single GenTraversable argument is passed and failed` {
        val e1 = intercept[TestFailedException] {
          fumList should (be (toList) or contain only Vector("fie", "fee", "fam", "foe"))
        }
        checkMessageStackDepth(e1, Resources("wasNotEqualTo", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("didNotContainOnlyElementsWithFriendlyReminder", decorateToStringValue(fumList), Vector("fie", "fee", "fam", "foe")), fileName, thisLineNumber - 2)
      }
    }

    object `when used with (contain only (..) and be (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (contain only ("fie", "fee", "fum", "foe") or be (fumList))
        fumList should (contain only ("fie", "fee", "fam", "foe") or be (fumList))
        fumList should (contain only ("fie", "fee", "fum", "foe") or be (toList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain only ("fee", "fie", "foe", "fam") or be (toList))
        }
        checkMessageStackDepth(e1, Resources("didNotContainOnlyElements", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fam\"") + ", and " + Resources("wasNotEqualTo", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (contain only ("FIE", "FEE", "FUM", "FOE") or be (fumList))
        fumList should (contain only ("FIE", "FEE", "FAM", "FOE") or be (fumList))
        fumList should (contain only ("FIE", "FEE", "FUM", "FOE") or be (toList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain only ("FEE", "FIE", "FOE", "FAM") or be (toList))
        }
        checkMessageStackDepth(e1, Resources("didNotContainOnlyElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FAM\"") + ", and " + Resources("wasNotEqualTo", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (contain only ("FIE", "FEE", "FUM", "FOE") or be (fumList))) (decided by upperCaseStringEquality)
        (fumList should (contain only ("FIE", "FEE", "FAM", "FOE") or be (fumList))) (decided by upperCaseStringEquality)
        (fumList should (contain only ("FIE", "FEE", "FUM", "FOE") or be (toList))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain only ("FEE", "FIE", "FOE", "FAM") or be (toList))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotContainOnlyElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FAM\"") + ", and " + Resources("wasNotEqualTo", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        (fumList should (contain only (" FEE ", " FIE ", " FOE ", " FUM ") or be (fumList))) (after being lowerCased and trimmed)
      }
      
      def `should throw NotAllowedException with correct stack depth and message when RHS is empty` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (contain only () or be (fumList))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("onlyEmpty")))
      }
      
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (contain only ("fee", "fie", "foe", "fie", "fum") or be (fumList))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("onlyDuplicate")))
      }

      def `should throw TFE with friendly reminder when single GenTraversable argument is passed and failed` {
        val e1 = intercept[TestFailedException] {
          fumList should (contain only Vector("fee", "fie", "foe", "fam") or be (toList))
        }
        checkMessageStackDepth(e1, Resources("didNotContainOnlyElementsWithFriendlyReminder", decorateToStringValue(fumList), Vector("fee", "fie", "foe", "fam")) + ", and " + Resources("wasNotEqualTo", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
      }
    }
    
    object `when used with (not contain only (..) and not contain only (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (not contain only ("fee", "fie", "foe", "fuu") or not contain only ("fie", "fee", "fuu", "foe"))
        fumList should (not contain only ("fee", "fie", "foe", "fum") or not contain only ("fie", "fee", "fuu", "foe"))
        fumList should (not contain only ("fee", "fie", "foe", "fuu") or not contain only ("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain only ("fee", "fie", "foe", "fum") or not contain only ("fie", "fee", "fum", "foe"))
        }
        checkMessageStackDepth(e1, Resources("containedOnlyElements", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\"") + ", and " + Resources("containedOnlyElements", decorateToStringValue(fumList), "\"fie\", \"fee\", \"fum\", \"foe\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (not contain only ("FEE", "FIE", "FOE", "FUU") or not contain only ("FIE", "FEE", "FUU", "FOE"))
        fumList should (not contain only ("FEE", "FIE", "FOE", "FUM") or not contain only ("FIE", "FEE", "FUU", "FOE"))
        fumList should (not contain only ("FEE", "FIE", "FOE", "FUU") or not contain only ("FIE", "FEE", "FUM", "FOE"))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain only ("FEE", "FIE", "FOE", "FUM") or not contain only ("FIE", "FEE", "FUM", "FOE"))
        }
        checkMessageStackDepth(e1, Resources("containedOnlyElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\"") + ", and " + Resources("containedOnlyElements", decorateToStringValue(fumList), "\"FIE\", \"FEE\", \"FUM\", \"FOE\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (not contain only ("FEE", "FIE", "FOE", "FUU") or not contain only ("FIE", "FEE", "FUU", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (not contain only ("FEE", "FIE", "FOE", "FUM") or not contain only ("FIE", "FEE", "FUU", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (not contain only ("FEE", "FIE", "FOE", "FUU") or not contain only ("FIE", "FEE", "FUM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not contain only ("FEE", "FIE", "FOE", "FUM") or not contain only ("FIE", "FEE", "FUM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("containedOnlyElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\"") + ", and " + Resources("containedOnlyElements", decorateToStringValue(fumList), "\"FIE\", \"FEE\", \"FUM\", \"FOE\""), fileName, thisLineNumber - 2)
        (fumList should (contain only (" FEE ", " FIE ", " FOE ", " FUM ") or contain only (" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
      
      def `should throw NotAllowedException with correct stack depth and message when RHS is empty` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not contain only () or not contain only ("fie", "fee", "fuu", "foe"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("onlyEmpty")))
        
        val e2 = intercept[exceptions.NotAllowedException] {
          fumList should (not contain only ("fie", "fee", "fuu", "foe") or not contain only ())
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources("onlyEmpty")))
      }
      
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not contain only ("fee", "fie", "foe", "fie", "fum") or not contain only ("fie", "fee", "fuu", "foe"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("onlyDuplicate")))
        
        val e2 = intercept[exceptions.NotAllowedException] {
          fumList should (not contain only ("fie", "fee", "fuu", "foe") or not contain only ("fee", "fie", "foe", "fie", "fum"))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources("onlyDuplicate")))
      }

      def `should throw TFE with friendly reminder when single GenTraversable argument is passed and failed` {
        val e1 = intercept[TestFailedException] {
          Vector(Vector("fee", "fie", "foe", "fum")) should (not contain only (Vector("fee", "fie", "foe", "fum")) or not contain only (Vector("fee", "fie", "foe", "fum")))
        }
        checkMessageStackDepth(e1, Resources("containedOnlyElementsWithFriendlyReminder", decorateToStringValue(Vector(Vector("fee", "fie", "foe", "fum"))), Vector("fee", "fie", "foe", "fum")) + ", and " + Resources("containedOnlyElementsWithFriendlyReminder", decorateToStringValue(Vector(Vector("fee", "fie", "foe", "fum"))), Vector("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)
      }
    }
    
    object `when used with (not equal (..) and not contain only (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (not equal (toList) or not contain only ("fie", "fee", "fuu", "foe"))
        fumList should (not equal (fumList) or not contain only ("fie", "fee", "fuu", "foe"))
        fumList should (not equal (toList) or not contain only ("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (not equal (fumList) or not contain only ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("containedOnlyElements", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (not equal (toList) or not contain only ("FIE", "FEE", "FUU", "FOE"))
        fumList should (not equal (fumList) or not contain only ("FIE", "FEE", "FUU", "FOE"))
        fumList should (not equal (toList) or not contain only ("FIE", "FEE", "FUM", "FOE"))
        val e2 = intercept[TestFailedException] {
          fumList should (not equal (fumList) or (not contain only ("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e2, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("containedOnlyElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (not equal (fumList) or not contain only ("FIE", "FEE", "FUU", "FOE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (not equal (toList) or not contain only ("FIE", "FEE", "FUU", "FOE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (not equal (fumList) or not contain only ("FIE", "FEE", "FUM", "FOE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not equal (toList) or not contain only ("FIE", "FEE", "FUM", "FOE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("containedOnlyElements", decorateToStringValue(fumList), "\"FIE\", \"FEE\", \"FUM\", \"FOE\""), fileName, thisLineNumber - 2)
        (fumList should (not contain only (" FEE ", " FIE ", " FOE ", " FUU ") or not contain only (" FEE ", " FIE ", " FOE ", " FUU "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
      
      def `should throw NotAllowedException with correct stack depth and message when RHS is empty` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not equal (toList) or not contain only ())
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("onlyEmpty")))
      }
      
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not equal (toList) or not contain only ("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("onlyDuplicate")))
      }

      def `should throw TFE with friendly reminder when single GenTraversable argument is passed and failed` {
        val e1 = intercept[TestFailedException] {
          Vector(Vector("fee", "fie", "foe", "fum")) should (not equal (Vector(Vector("fee", "fie", "foe", "fum"))) or not contain only (Vector("fee", "fie", "foe", "fum")))
        }
        checkMessageStackDepth(e1, Resources("equaled", decorateToStringValue(Vector(Vector("fee", "fie", "foe", "fum"))), decorateToStringValue(Vector(Vector("fee", "fie", "foe", "fum")))) + ", and " + Resources("containedOnlyElementsWithFriendlyReminder", decorateToStringValue(Vector(Vector("fee", "fie", "foe", "fum"))), Vector("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)
      }
    }
    
    object `when used with (not be (..) and not contain only (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (not be (toList) or not contain only ("fie", "fee", "fuu", "foe"))
        fumList should (not be (fumList) or not contain only ("fie", "fee", "fuu", "foe"))
        fumList should (not be (toList) or not contain only ("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) or not contain only ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources("wasEqualTo", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("containedOnlyElements", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (not be (toList) or not contain only ("FIE", "FEE", "FUU", "FOE"))
        fumList should (not be (fumList) or not contain only ("FIE", "FEE", "FUU", "FOE"))
        fumList should (not be (toList) or not contain only ("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) or (not contain only ("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e1, Resources("wasEqualTo", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("containedOnlyElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (not be (toList) or not contain only ("FIE", "FEE", "FUU", "FOE"))) (decided by upperCaseStringEquality)
        (fumList should (not be (fumList) or not contain only ("FIE", "FEE", "FUU", "FOE"))) (decided by upperCaseStringEquality)
        (fumList should (not be (toList) or not contain only ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not be (fumList) or not contain only ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("wasEqualTo", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("containedOnlyElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
        (fumList should (not contain only (" FEE ", " FIE ", " FOE ", " FUU ") or not contain only (" FEE ", " FIE ", " FOE ", " FUU "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
      
      def `should throw NotAllowedException with correct stack depth and message when RHS is empty` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not be (toList) or not contain only ())
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("onlyEmpty")))
      }
      
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not be (toList) or not contain only ("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("onlyDuplicate")))
      }

      def `should throw TFE with friendly reminder when single GenTraversable argument is passed and failed` {
        val e1 = intercept[TestFailedException] {
          Vector(Vector("fee", "fie", "foe", "fum")) should (not be (Vector(Vector("fee", "fie", "foe", "fum"))) or not contain only (Vector("fee", "fie", "foe", "fum")))
        }
        checkMessageStackDepth(e1, Resources("wasEqualTo", decorateToStringValue(Vector(Vector("fee", "fie", "foe", "fum"))), decorateToStringValue(Vector(Vector("fee", "fie", "foe", "fum")))) + ", and " + Resources("containedOnlyElementsWithFriendlyReminder", decorateToStringValue(Vector(Vector("fee", "fie", "foe", "fum"))), Vector("fee", "fie", "foe", "fum")), fileName, thisLineNumber - 2)
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
    
    object `when used with (contain only (..) and contain only (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (contain only (3, 2, 1) or contain only (1, 3, 2))
        all (list1s) should (contain only (3, 2, 5) or contain only (1, 3, 2))
        all (list1s) should (contain only (3, 2, 1) or contain only (2, 3, 4))
        
        atLeast (2, lists) should (contain only (3, 1, 2) or contain only (1, 2, 3))
        atLeast (2, lists) should (contain only (3, 6, 5) or contain only (1, 3, 2))
        atLeast (2, lists) should (contain only (3, 1, 2) or contain only (8, 3, 4))
        
        val e1 = intercept[TestFailedException] {
          all (lists) should (contain only (3, 1, 2) or contain only (1, 3, 2))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(List(4, 3, 2)) + " did not contain only " + "(3, 1, 2)" + ", and " + decorateToStringValue(List(4, 3, 2)) + " did not contain only " + "(1, 3, 2)", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (contain only ("HELLO", "HI") or contain only ("hello", "hi"))
        all (hiLists) should (contain only ("HELLO", "HO") or contain only ("hello", "hi"))
        all (hiLists) should (contain only ("HELLO", "HI") or contain only ("hello", "ho"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (contain only ("HELLO", "HO") or contain only ("hello", "ho"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi", "hello")) + " did not contain only " + "(\"HELLO\", \"HO\")" + ", and " + decorateToStringValue(List("hi", "hello")) + " did not contain only " + "(\"hello\", \"ho\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (contain only ("HELLO", "HI") or contain only ("hello", "hi"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (contain only ("HELLO", "HO") or contain only ("hello", "hi"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (contain only ("HELLO", "HI") or contain only ("hello", "ho"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (contain only ("HELLO", "HO") or contain only ("hello", "ho"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi", "hello")) + " did not contain only " + "(\"HELLO\", \"HO\")" + ", and " + decorateToStringValue(List("hi", "hello")) + " did not contain only " + "(\"hello\", \"ho\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should throw NotAllowedException with correct stack depth and message when RHS is empty` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (contain only () or contain only (1, 3, 2))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("onlyEmpty")))
        
        val e2 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (contain only (1, 3, 2) or contain only ())
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources("onlyEmpty")))
      }
      
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (contain only (3, 2, 2, 1) or contain only (1, 3, 2))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("onlyDuplicate")))
        
        val e2 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (contain only (1, 3, 2) or contain only (3, 2, 2, 1))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources("onlyDuplicate")))
      }

      def `should throw TFE with friendly reminder when single GenTraversable argument is passed and failed` {
        val e1 = intercept[TestFailedException] {
          all (Vector(Vector(Vector("hi", "hello")))) should (contain only Vector("HELLO", "HO") or contain only Vector("hello", "ho"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(Vector(Vector("hi", "hello"))) + " did not contain only " + "(" + Vector("HELLO", "HO") + "), did you forget to say : _*" + ", and " + decorateToStringValue(Vector(Vector("hi", "hello"))) + " did not contain only " + "(" + Vector("hello", "ho") + "), did you forget to say : _*", thisLineNumber - 2, Vector(Vector(Vector("hi", "hello")))), fileName, thisLineNumber - 2)
      }
    }
    
    object `when used with (be (..) and contain only (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (be (List(3, 2, 1)) or contain only (1, 2, 3))
        all (list1s) should (be (List(2, 3, 4)) or contain only (1, 2, 3))
        all (list1s) should (be (List(3, 2, 1)) or contain only (2, 3, 4))
        
        val e1 = intercept[TestFailedException] {
          all (list1s) should (be (List(2, 3, 4)) or contain only (2, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List(3, 2, 1)) + " was not equal to " + decorateToStringValue(List(2, 3, 4)) + ", and " + decorateToStringValue(List(3, 2, 1)) + " did not contain only " + "(2, 3, 4)", thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (be (List("hi", "hello")) or contain only ("HELLO", "HI"))
        all (hiLists) should (be (List("ho", "hello")) or contain only ("HELLO", "HI"))
        all (hiLists) should (be (List("hi", "hello")) or contain only ("HELLO", "HO"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (be (List("ho", "hello")) or contain only ("HELLO", "HO"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi", "hello")) + " was not equal to " + decorateToStringValue(List("ho", "hello")) + ", and " + decorateToStringValue(List("hi", "hello")) + " did not contain only " + "(\"HELLO\", \"HO\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (be (List("hi", "hello")) or contain only ("HELLO", "HI"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (be (List("ho", "hello")) or contain only ("HELLO", "HI"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (be (List("hi", "hello")) or contain only ("HELLO", "HO"))) (decided by upperCaseStringEquality)
        
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (be (List("ho", "hello")) or contain only ("HELLO", "HO"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi", "hello")) + " was not equal to " + decorateToStringValue(List("ho", "hello")) + ", and " + decorateToStringValue(List("hi", "hello")) + " did not contain only " + "(\"HELLO\", \"HO\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should throw NotAllowedException with correct stack depth and message when RHS is empty` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (be (List(3, 2, 1)) or contain only ())
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("onlyEmpty")))
      }
      
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (be (List(3, 2, 1)) or contain only (1, 2, 2, 3))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("onlyDuplicate")))
      }

      def `should throw TFE with friendly reminder when single GenTraversable argument is passed and failed` {
        val e1 = intercept[TestFailedException] {
          all (Vector(Vector(Vector(3, 2, 1)))) should (be (List(2, 3, 4)) or contain only Vector(2, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(Vector(Vector(3, 2, 1))) + " was not equal to " + decorateToStringValue(List(2, 3, 4)) + ", and " + decorateToStringValue(Vector(Vector(3, 2, 1))) + " did not contain only (" + Vector(2, 3, 4) + "), did you forget to say : _*", thisLineNumber - 2, Vector(Vector(Vector(3, 2, 1)))), fileName, thisLineNumber - 2)
      }
    }
    
    object `when used with (not contain only xx and not contain only xx)` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (not contain only (3, 2, 8) or not contain only (8, 3, 4))
        all (list1s) should (not contain only (1, 2, 3) or not contain only (8, 3, 4))
        all (list1s) should (not contain only (3, 2, 8) or not contain only (2, 3, 1))
        
        val e1 = intercept[TestFailedException] {
          all (lists) should (not contain only (4, 2, 3) or not contain only (2, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(List(4, 3, 2)) + " contained only " + "(4, 2, 3)" + ", and " + decorateToStringValue(List(4, 3, 2)) + " contained only " + "(2, 3, 4)", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (not contain only ("HELLO", "HO") or not contain only ("hello", "ho"))
        all (hiLists) should (not contain only ("HELLO", "HI") or not contain only ("hello", "ho"))
        all (hiLists) should (not contain only ("HELLO", "HO") or not contain only ("hello", "hi"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not contain only ("HELLO", "HI") or not contain only ("hello", "hi"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi", "hello")) + " contained only " + "(\"HELLO\", \"HI\")" + ", and " + decorateToStringValue(List("hi", "hello")) + " contained only " + "(\"hello\", \"hi\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (not contain only ("HELLO", "HO") or not contain only ("hello", "ho"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (not contain only ("HELLO", "HI") or not contain only ("hello", "ho"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (not contain only ("HELLO", "HO") or not contain only ("hello", "hi"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not contain only ("HELLO", "HI") or not contain only ("hello", "hi"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi", "hello")) + " contained only " + "(\"HELLO\", \"HI\")" + ", and " + decorateToStringValue(List("hi", "hello")) + " contained only " + "(\"hello\", \"hi\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should throw NotAllowedException with correct stack depth and message when RHS is empty` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not contain only () or not contain only (8, 3, 4))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("onlyEmpty")))
        
        val e2 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not contain only (8, 3, 4) or not contain only ())
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources("onlyEmpty")))
      }
      
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not contain only (1, 2, 2, 3) or not contain only (8, 3, 4))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("onlyDuplicate")))
        
        val e2 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not contain only (8, 3, 4) or not contain only (1, 2, 2, 3))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources("onlyDuplicate")))
      }

      def `should throw TFE with friendly reminder when single GenTraversable argument is passed and failed` {
        val e1 = intercept[TestFailedException] {
          all (Vector(Vector(Vector(3, 2, 1)))) should (not contain only (Vector(3, 2, 1)) or not contain only (Vector(3, 2, 1)))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(Vector(Vector(3, 2, 1))) + " contained only (" + Vector(3, 2, 1) + "), did you forget to say : _*" + ", and " + decorateToStringValue(Vector(Vector(3, 2, 1))) + " contained only (" + Vector(3, 2, 1) + "), did you forget to say : _*", thisLineNumber - 2, Vector(Vector(Vector(3, 2, 1)))), fileName, thisLineNumber - 2)
      }
    }
    
    object `when used with (not be (...) and not contain only (...))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (not be (List(2)) or not contain only (8, 3, 4))
        all (list1s) should (not be (List(3, 2, 1)) or not contain only (8, 3, 4))
        all (list1s) should (not be (List(2)) or not contain only (1, 2, 3))
        
        val e1 = intercept[TestFailedException] {
          all (list1s) should (not be (List(3, 2, 1)) or not contain only (2, 3, 1))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List(3, 2, 1)) + " was equal to " + decorateToStringValue(List(3, 2, 1)) + ", and " + decorateToStringValue(List(3, 2, 1)) + " contained only " + "(2, 3, 1)", thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (not be (List("hello", "ho")) or not contain only ("HELLO", "HO"))
        all (hiLists) should (not be (List("hello", "hi")) or not contain only ("HELLO", "HO"))
        all (hiLists) should (not be (List("hello", "ho")) or not contain only ("HELLO", "HI"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not be (List("hi", "hello")) or not contain only ("HELLO", "HI"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi", "hello")) + " was equal to " + decorateToStringValue(List("hi", "hello")) + ", and " + decorateToStringValue(List("hi", "hello")) + " contained only " + "(\"HELLO\", \"HI\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (not be (List("hello", "ho")) or not contain only ("HELLO", "HO"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (not be (List("hello", "hi")) or not contain only ("HELLO", "HO"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (not be (List("hello", "ho")) or not contain only ("HELLO", "HI"))) (decided by upperCaseStringEquality)
        
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not be (List("hi", "hello")) or not contain only ("HELLO", "HI"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi", "hello")) + " was equal to " + decorateToStringValue(List("hi", "hello")) + ", and " + decorateToStringValue(List("hi", "hello")) + " contained only " + "(\"HELLO\", \"HI\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should throw NotAllowedException with correct stack depth and message when RHS is empty` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not be (List(2)) or not contain only ())
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("onlyEmpty")))
      }
      
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not be (List(2)) or not contain only (1, 2, 2, 3))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("onlyDuplicate")))
        
        val e2 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not contain only (8, 3, 4) or not contain only (1, 2, 2, 3))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources("onlyDuplicate")))
      }

      def `should throw TFE with friendly reminder when single GenTraversable argument is passed and failed` {
        val e1 = intercept[TestFailedException] {
          all (Vector(Vector(Vector(3, 2, 1)))) should (not be (Vector(Vector(3, 2, 1))) or not contain only (Vector(3, 2, 1)))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(Vector(Vector(3, 2, 1))) + " was equal to " + decorateToStringValue(Vector(Vector(3, 2, 1))) + ", and " + decorateToStringValue(Vector(Vector(3, 2, 1))) + " contained only (" + Vector(3, 2, 1) + "), did you forget to say : _*", thisLineNumber - 2, Vector(Vector(Vector(3, 2, 1)))), fileName, thisLineNumber - 2)
      }
    }
  }
}
